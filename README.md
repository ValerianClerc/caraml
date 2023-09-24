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
package.yaml          # config file for the cabal package
```