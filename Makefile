.PHONY: build run test

build:
	cabal build
	@echo "Build complete."

test:
	cabal test
	@echo "Tests complete."

# Example usage:
# make run FILE=./test/file.cml
run: 
	cabal run caraml-exe -- --compile-and-run $(FILE)