{-# OPTIONS -fno-warn-missing-local-signatures #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
module Command.Compile (run) where 
import Prelude

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, tryReadTChan, writeTChan)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Supply (evalSupplyT, runSupplyT)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath (joinPath, takeDirectory, (</>))
import System.IO.UTF8 (readUTF8FileT, writeUTF8FileT)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Go as Go
import qualified Language.PureScript as PS
import qualified Language.PureScript.CoreFn as CF


run :: [FilePath] -> IO ()
run input = do
    moduleFiles <- readInputFiles input

    -- Our codegen sends corefn modules on this channel. Why don't you 
    -- deserialize the corefn.json files, I hear you ask? Because type info is
    -- lost during serialization, and we need it.
    coreChan <- atomically newTChan

    (result, _warnings) <- PS.runMake options $ do
        ms <- PS.parseModulesFromFiles id moduleFiles
        let filePathMap = M.fromList $ fmap (\(_fp, PS.Module _ _ mn _ _) -> 
                -- FIXME: We shouldn't really be rebuilding everything...
                (mn, Left PS.RebuildAlways)) ms

        let foreigns = M.empty -- TODO
        let makeActions = PS.buildMakeActions outputDir filePathMap foreigns False
        let modules = fmap snd ms

        let codegen cf _ _ = lift . PS.makeIO undefined $ 
                atomically (writeTChan coreChan cf)
        externs <- PS.make makeActions { PS.codegen = codegen }  modules

        -- This is a bit wierd: we need to dive back into the original modules
        -- so that we can extract /all/ the type information we need for
        -- creating Go type declarations. We can't use the core because it
        -- doesn't hold onto enough info (for our purposes) and externs only 
        -- have info for exported things.

        let sorted = fromJust (alignOn PS.efModuleName PS.getModuleName externs modules)
        --  NOTE: Externs should be topologically sorted, so use their sort order
        
        desugared <- evalSupplyT 0 $ 
            PS.desugar externs (PS.importPrim <$> sorted)

        pure (desugared, externs)

    -- TODO: Print warnings, properly handle errors
    (modules, _externs) <- either (die . show) pure result

    -- Walk through all the PureScript modules and lift out all the necessary
    -- type declarations. Then stick that information in the environment.
    let (types, next) = either undefined id (runSupplyT 0 $ Go.typeDeclsForModules modules)
    let initialEnv = Go.emptyEnvironment { Go.envTypes = types }

    coreModules <- drainTChan coreChan <&> 
        fromJust . alignOn PS.getModuleName CF.moduleName modules 

    runExceptT (compileModules initialEnv next [] coreModules) >>= \case
        Left err -> die (show err)
        Right files -> do
            for_ files $ \(path, file) -> do
                let rendered = Go.render (Go.prettyFile file)
                createDirectoryIfMissing True (takeDirectory path)
                writeUTF8FileT path rendered

            -- XXX
            putStrLn (T.unpack . Go.render . Go.prettyFile . snd $ head files)
  where
  options :: PS.Options
  options = PS.defaultOptions 
      { PS.optionsCodegenTargets = S.singleton PS.CoreFn }

  outputDir :: FilePath
  outputDir = "output"

  -- TODO: This should be a flag
  gomod :: String
  gomod = "purgo"

  outputPath :: PS.ModuleName -> FilePath -> FilePath
  outputPath mn fp = joinPath
      [ outputDir 
      , T.unpack (PS.runModuleName mn)  
      , fp
      ]

  compileModules 
      :: MonadIO m 
      => Go.Environment 
      -> Integer 
      -> [(FilePath, Go.File)]  -- ^ accumulator
      -> [CF.Module CF.Ann] 
      -> ExceptT Error m [(FilePath, Go.File)]
  compileModules _ _ accum [] = pure accum
  compileModules env next accum (CF.Module{..} : rest) = do
      (decls, next') <- runSupplyT next (Go.annotateDecls env moduleName moduleDecls) ? TypeError
      let base = gomod </> outputDir
      file' <- Go.compileFile env base moduleName moduleImports moduleExports decls ? CompileError
      let file = addTypeDecls env moduleName file'
      let env' = Go.extendEnvironmentWithBinds moduleName 
                                               moduleExports
                                              (fmap Go.annType <$> decls) 
                                               env
      let path = outputPath moduleName Go.defaultFileName
      compileModules env' next' ((path, file) : accum) rest

  addTypeDecls :: Go.Environment -> PS.ModuleName -> Go.File -> Go.File
  addTypeDecls env mn file = 
        file { Go.fileDecls = extractTypeDecls mn (Go.envTypes env) <> Go.fileDecls file }

  extractTypeDecls 
      :: PS.ModuleName 
      -> M.Map (PS.Qualified Go.TypeName) Go.Type
      -> [Go.Decl Go.Type]
  extractTypeDecls mn = 
      mapMaybe (\(tn, t) -> flip Go.TypeDecl t <$> PS.disqualifyFor (Just mn) tn)
          . M.toList


data Error 
    = AesonError String
    | TypeError Go.TypeError
    | CompileError Go.CompileError
    deriving (Show)


readInputFiles :: [FilePath] -> IO [(FilePath, Text)]
readInputFiles = traverse $ \inFile -> (inFile,) <$> readUTF8FileT inFile


drainTChan :: TChan a -> IO [a]
drainTChan tc = do
    next <- atomically (tryReadTChan tc)
    case next of
      Nothing -> pure []
      Just a  -> (a:) <$> drainTChan tc


(?) :: Functor m => ExceptT e m a -> (e -> e') -> ExceptT e' m a
(?) = flip withExceptT


alignOn :: Eq c => (a -> c) -> (b -> c) -> [a] -> [b] -> Maybe [b]
alignOn f g as bs = traverse (\a -> lookup (f a) kvs) as 
    where kvs = fmap (\b -> (g b, b)) bs
