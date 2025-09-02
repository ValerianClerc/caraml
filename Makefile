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

install-dev:
	cabal install --overwrite-policy=always

install:
	cabal install --installdir=/usr/local/bin --overwrite-policy=always