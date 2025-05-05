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

## Very useful references/blogs
* [LLVM basics in Haskell](https://danieljharvey.github.io/posts/2023-02-08-llvm-compiler-part-1.html)

## Notes to self

* Currently I'm using GHC 8.10.7, because the llvm-hs-pretty package has some breaking components. In the future, I'd like to move back to GHC 9. Here are the things that I'd need to do:
  * Switch the llvm-hs-pretty branch to the llvm-12 branch
  * Ensure that I'm also using the llvm-12 branch of llvm-hs-pure


* Add make script: `clang -Wno-override-module -lm test.ll test.c -o a.out`