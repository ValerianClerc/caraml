# caraml

caraML: a compiler for a sweet and simple subset of Standard ML

## Usage

To run test suite, run
```bash
cabal test
```

To compile a code file at the path `test/file.cml`, run
```bash
cabal run caraml-exe -- test/file.cml
```

To run a specific step of the compiler, specify it with a flag:
```bash
cabal run caraml-exe -- --lexer test/file.cml
```

## Repo structure

```
.github/workflows/    # Github Actions config
app/                  # executable file
design/               # grammar definition of the language
src/                  # majority of source code
test/                 # test suite
caraml.cabal          # config file for the cabal package
```

## Runtime dependencies
* clang
* llvm version 15? (or could be just build-time?)

## Very useful references/blogs
* [LLVM basics in Haskell](https://danieljharvey.github.io/posts/2023-02-08-llvm-compiler-part-1.html)

## Notes to self

* Currently I'm using GHC 8.10.7, because the llvm-hs-pretty package has some breaking components. In the future, I'd like to move back to GHC 9. Here are the things that I'd need to do:
  * Switch the llvm-hs-pretty branch to the llvm-12 branch
  * Ensure that I'm also using the llvm-12 branch of llvm-hs-pure


* Add make script: `clang -Wno-override-module -lm test.ll test.c -o a.out`

## WASM notes:

1. Compile runtime: 
```
docker run --rm -v $(pwd):/src -u $(id -u):$(id -g)   emscripten/emsdk emcc -O2 runtime/runtime.c -c -o runtime/runtime.o
```
2. Compile code:
```
docker run --rm -v $(pwd):/src -u $(id -u):$(id -g)   emscripten/emsdk emcc -O2 --emrun runtime/runtime.o test/file.ll -o fib.html
```

Unconfirmed:
Compile code to be called in React:
```
docker run --rm -v "$(pwd)":/src -u "$(id -u)":"$(id -g)" emscripten/emsdk emcc test/file.ll -O3   -s WASM=1   -s MODULARIZE=1   -s EXPORT_NAME="createExec"   -s INVOKE_RUN=0   -s EXIT_RUNTIME=1   -s ALLOW
_MEMORY_GROWTH=1 runtime/runtime.o  -o exec.js
```