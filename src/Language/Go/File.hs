module Language.Go.File 
    ( File(..)

    -- * Lenses
    , packageName
    , imports
    , decls
    )
where

import Prelude

import Language.Go.AST (Decl, Import)
import Language.Go.Names (PackageName)
import Language.Go.Types (Type)


-- | A single Go source file. 
--
-- A collection of source files (in the same directory) forms a "package", and
-- a collection of versioned packages forms a "module".
data File = File
    { filePackageName :: PackageName                
    -- ^ package foo
    , fileImports     :: [Import]  
    -- ^ import Effect "output/Effect"
    , fileDecls       :: [Decl Type]               
    }


packageName :: Functor f => (PackageName -> f PackageName) -> File -> f File
packageName f file = 
    (\pn -> file { filePackageName = pn }) <$> f (filePackageName file) 


imports :: Functor f => ([Import] -> f [Import]) -> File -> f File
imports f file = 
    (\imps -> file { fileImports = imps }) <$> f (fileImports file) 


decls :: Functor f => ([Decl Type] -> f [Decl Type]) -> File -> f File
decls f file = 
    (\ds -> file { fileDecls = ds }) <$> f (fileDecls file) 
