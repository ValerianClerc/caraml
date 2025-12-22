# caraml

caraml is a compiler for a sweet and simple language inspired by Standard ML, written in Haskell.

**Try it out online:** [playground.caraml.valerianclerc.com](https://playground.caraml.valerianclerc.com)
**Playground Source:** [github.com/ValerianClerc/caraml-playground](https://github.com/ValerianClerc/caraml-playground)

## Features

*   **Types**: Strong, static typing with type inference. Supports `int` and `bool`.
*   **Functions**: Recursive functions, multi-argument support.
*   **Control Flow**: `if-then-else` expressions.
*   **Bindings**: `let` expressions for variable scoping.
*   **Compilation**: Compiles to LLVM IR for efficient execution.

## Cross-Platform Support

`caraml` leverages the power of **LLVM** to achieve cross-platform compatibility. By compiling to LLVM Intermediate Representation (IR), `caraml` programs can be compiled to native machine code for virtually any architecture supported by LLVM, including:

*   **x86_64** (Linux, macOS, Windows)
*   **ARM64** (Apple Silicon, Mobile, Raspberry Pi)
*   **WebAssembly** (Run in the browser, like in the online playground)

This decoupling of the frontend language from the backend machine code generation ensures that `caraml` is both portable and highly optimized.

## Compiler Architecture

`caraml` follows a classic compiler pipeline architecture:

1.  **Lexer** (`src/Lexer.hs`): Tokenizes the source code.
2.  **Parser** (`src/Parser.hs`): Constructs an Abstract Syntax Tree (AST) from tokens.
3.  **Type Inference** (`src/TypeInfer.hs`): Annotates the AST with types, ensuring type safety.
4.  **LLVM Codegen** (`src/ToLlvm.hs`): Translates the typed AST into LLVM IR.

## Language Specification

The language grammar is defined in Extended Backus-Naur Form (EBNF). You can find the full specification in [design/mini_ebnf.txt](design/mini_ebnf.txt).

## Examples

### Fibonacci Sequence
```sml
fun fib(n: int) = 
  if n <= 1 then n 
  else fib(n-1) + fib(n-2);

let result = fib(10);
```

### Factorial
```sml
fun fact(n: int) = 
  if n = 0 then 1 
  else n * fact(n - 1);
```

### Greatest Common Divisor (GCD)
Demonstrates multiple arguments and arithmetic logic (using integer division for modulo).
```sml
fun gcd(a: int, b: int) = 
  if b = 0 then a 
  else gcd(b, a - (a / b) * b);

let result = gcd(1071, 462);
```

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

### Runtime support

The generated LLVM IR calls small runtime helpers (`printint`, `printbool`). When you install the `caraml` executable, the C runtime source file (`runtime.c`) is shipped as a data file. You can retrieve a copy at any time with:

```bash
cabal run caraml-exe -- --emit-runtime runtime.c
```

Then compile your program like so:

```bash
# Generate LLVM IR
cabal run caraml-exe -- --llvm test/file.cml  # produces test/file.ll

# Build native binary (clang will compile both IR and runtime C)
clang -Wno-override-module -lm test/file.ll runtime.c -o a.out
```

Or run directly via the built-in driver (which automatically locates the installed runtime):

```bash
cabal run caraml-exe -- --compile-and-run test/file.cml
```

## Development notes
### Repo structure

```
.github/workflows/    # Github Actions config
app/                  # executable file
design/               # grammar definition of the language
src/                  # majority of source code
test/                 # test suite
caraml.cabal          # config file for the cabal package
```

### Runtime dependencies
* clang
* llvm version 15? (or could be just build-time?)

### Very useful references/blogs
* [LLVM basics in Haskell](https://danieljharvey.github.io/posts/2023-02-08-llvm-compiler-part-1.html)

### Notes to self

* Currently I'm using GHC 8.10.7, because the llvm-hs-pretty package has some breaking components. In the future, I'd like to move back to GHC 9. Here are the things that I'd need to do:
  * Switch the llvm-hs-pretty branch to the llvm-12 branch
  * Ensure that I'm also using the llvm-12 branch of llvm-hs-pure


* Add make script: `clang -Wno-override-module -lm test.ll test.c -o a.out`

### WASM notes:

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