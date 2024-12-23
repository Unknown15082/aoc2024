all: build run

FILE = src/Day$(day)
GHC = ghc -no-keep-hi-files -no-keep-o-files -Wno-tabs

build: $(FILE).hs
	@$(GHC) $(FILE).hs -o $(FILE)

run: $(FILE)
	@./$(FILE) < inputs/$(day).txt

debug: $(FILE)
	@./$(FILE)

download:
	@echo "Downloading testcases for Day $(day)"
	@nix run nixpkgs#aoc-cli -- download -q -s ./.session -d $(day) -y 2024 -I -i inputs/$(day).txt
