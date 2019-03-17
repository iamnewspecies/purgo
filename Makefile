
HS_SOURCE_FILES := $(shell find purgo src -name '*.hs' -not -name 'Main.hs')
PATHS_MODULE := $(shell find dist-newstyle -wholename '**/build/autogen/Paths_purgo.hs')
# NOTE: File to load should come first.
GHCI_INPUT_FILES := purgo/Main.hs $(HS_SOURCE_FILES) $(PATHS_MODULE)
GHCI_FLAGS := -Weverything -fno-warn-missing-import-lists -fno-warn-all-missed-specialisations -fno-warn-safe -fno-warn-unsafe


.PHONY: ghcid
ghcid:
	@# We need to load all the modules manually, because we're not using
	@# stack and `cabal new-repl` can't load multiple components at once...
	ghcid --command "ghci $(GHCI_INPUT_FILES) $(GHCI_FLAGS)" --run
