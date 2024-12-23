all: run

GHC = ghc -no-keep-hi-files -no-keep-o-files -Wno-tabs

run:
	@cabal run all -- -d $(day) < inputs/$(day).txt

debug:
	@cabal run all -- -d $(day)

download:
	@echo "Downloading testcases for Day $(day)"
	@nix run nixpkgs#aoc-cli -- download -q -s ./.session -d $(day) -y 2024 -I -i inputs/$(day).txt
