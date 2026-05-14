---
name: rasm
description: Helps with the RASM programming language - a functional language targeting x86. Provides guidance on syntax, macros, stdlib, building projects, and common patterns.
---

# RASM Programming Language

RASM is a native compiler for a functional language targeting x86-32/64 Linux executables. It features functions, structs, enums, pattern matching, closures, and compile-time macros.

## When to Use This Skill

Use this skill when the user:

- Asks about RASM syntax or language features
- Needs help writing RASM code (functions, structs, macros, etc.)
- Wants to build or run a RASM project
- Has questions about the standard library
- Needs to debug RASM code

## Quick Reference

### Build & Run

```bash
# Build a project
cargo run --release -- build <path> -o <output_dir>

# Run a single file (uses stdlib by default)
cargo run --release -- build myfile.rasm -t c -o .

# Run tests
SKIP_SDL_TESTS=true cargo test --release --workspace

# Run a specific test
./run_test.sh <test_name>
```

### Command Line Options

- `-t nasmi386` - 32-bit x86 assembly output (default)
- `-t c` - C output
- `-d` - Debug mode (includes comments in generated code)
- `-r` - Release mode (optimized)
- `-o <dir>` - Output directory
- `--compile` - Generate only .asm/.c files (no linking)

## Language Syntax

### Functions

```rasm
pub fn name<T>(param: T) -> T {
    param
}

fn name() -> int { 0 }
fn name() -> float { 0.0 }
fn name() -> str { "" }
```

The last statement in a function is the return value of the function.

### Structs

```rasm
@toString()
@eq()
pub struct Pair<A,B> {
    first: A,
    second: B
}
```

### Enums

```rasm
pub enum Option<T> {
    Some(value: T),
    None
}

pub enum Result<OK,ERROR> {
    Ok(value: OK),
    Error(error: ERROR)
}
```

### Variables

```rasm
let name = value
let name = value.method().anotherMethod()
```

### Match Expressions

```rasm
match(value,
    fn(pattern) { result },
    { default })

match(option, fn(v) { v }, { defaultValue })

fn compare(a: int, b: int) -> int {
    if(a.less(b), -1, { if(a.eq(b), 0, 1) }) 
}

### Closures

```rasm
fn(x) { x }
fn(x, y) { x.add(y) }
fn(accum, current) { accum.add(current) }
```

### Method Calls

```rasm
value.method()
value.method(arg)
vector.map(fn(x) { x })
list.foldLeft(zero, fn(acc, x) { acc.add(x) })
```

### Literals

```rasm
42          // int
3.14        // float
"hello"    // str
'a'         // char
true, false // bool
```

### Built-in Types

- `int`, `float`, `bool`, `str`, `char`

## Macros

Compile-time macros are invoked with `!` suffix. Examples from stdlib:

```rasm
vec!(1, 2, 3)            // create vec from values
println!("Hello {}", x)  // print with format
print!("Value: {}", v)   // print without newline
```

Format placeholders `{}` are replaced by arguments in order.

### Attribute Macros

Auto-generate methods for structs/enums:

```rasm
@toString()
@eq()
pub struct Pair<A,B> {
    first: A,
    second: B
}
```

### Defining Macros

#### Expression Macros

Call with `!` suffix, receive AST expressions:

```rasm
pub fn vec(exprs: Vec<ASTExpression>) -> MacroExpressionResult {
    exprs.first.match(
        fn(first) {
            let start = simpleASTCall("vecOf", vecOf(first))
            let result = exprs.enumerate.filter(fn(act) { act.index.greater(0) })
                .foldLeft(start, fn(prev, act) {
                    simpleASTCall("push", vecOf(prev, act.value))
                })
            MacroExpressionOk(result, Vec())
        },
        { MacroExpressionResult::MacroError("No values, use Vec()") }
    )
}

Returns `MacroExpressionOk(expr, functions)` or `MacroExpressionResult::MacroError(message)`.

#### Statement Macros

Used for statements (like `println!`):

```rasm
pub fn println(s: str, exprs: Vec<ASTExpression>) -> MacroStatementResult {
    let parameters = vecOf(stringASTValue(s)).add(exprs)
    let f = format(s, exprs)
    f.match(fn(expr, functions) {
        let statement = ASTExpressionStatement(simpleASTCall("println", vecOf(expr)))
        MacroStatementOk(vecOf(statement), functions)
    }, fn(error) {
        MacroStatementResult::MacroError(error)
    })
}
```

Returns `MacroStatementOk(statements, functions)` or `MacroError(message)`.

### AST Builder Functions

Helper functions to construct AST:

```rasm
simpleASTCall("name", vecOf(args))         // function call
stringASTValue("hello")                    // string literal
integerASTValue(42)                        // int literal
booleanASTValue(true)                      // bool literal
ASTValueRefExpression("name")              // variable reference
```

## Project Structure

```
rasm.toml
src
  main
    rasm        # source code
    resources   # resources
    nasmi386    # target-specific code
    c           # C target code
  test
    rasm
    resources
```

### rasm.toml

```toml
[package]
name=
version=
main=

[dependencies]
"name" = "version" or { path = "path to the root of the library project" }
```

## Common Patterns

### Working with Option/Result

```rasm
// Using Option
let maybeValue: Option<int> = Some(42)
match(maybeValue, fn(v) { v.mul(2) }, { 0 })

// Using Result
let result: Result<int, str> = Ok(42)
match(result, fn(v) { v }, fn(e) { println(e) 0 })
```

### Functional Operations

```rasm
// Map 
let doubled = vec.map(fn(x) { x.mul(2) })

// Filter
let evens = vec.filter(fn(x) { x.mod(2).eq(0) })

// Fold
let sum = vec.foldLeft(0, fn(acc, x) { acc.add(x) })

// Enumerate
let indexed = vec.enumerate()
```

### String Operations

```rasm
let s = "hello"
s.append(" world")
s.len()
s.eq("hello")
s.substring(0, 5)
```

## Stdlib Modules

Key modules from stdlib:

- `vec` - Vector operations (map, filter, fold, etc.)
- `list` - Linked list operations
- `str` - String utilities
- `option` - Option type helpers
- `result` - Result type helpers
- `math` - Mathematical functions
- `time` - Time/date operations
- `json` - JSON parsing
- `iter` - Iterator utilities
- `print` - Print macros
- `test` - Testing utilities

## Common Errors & Solutions

### "No such file or directory"

- Ensure dependencies are installed: `./install_libs.sh`

### SDL test failures

- Install 32-bit SDL libraries or run with `SKIP_SDL_TESTS=true`

### Linker errors

- Ensure `gcc-multilib g++-multilib libc++-dev nasm` are installed

### Macro errors

- Check that macro functions return correct type (`MacroExpressionOk`, `MacroStatementOk`, etc.)
- Use `simpleASTCall` to generate function calls
- Use `stringASTValue`, `integerASTValue`, etc. for literals

## Examples

```rasm
// Fibonacci
pub fn fib(n: int) -> int {
    if(lessOrEqual(n, 1), n, {
        fib(n.sub(1)).add(fib(n.sub(2))) 
    })
}

// Using stdlib
let nums = vec!(1, 2, 3, 4, 5)
let sum = nums.foldLeft(0, fn(acc, n) { acc.add(n) })
println!("Sum: {}", sum)
```

## Getting Help

- Check `/home/enrico/development/rust/rasm/README.md` for full documentation
- Look at `/home/enrico/development/rust/rasm/stdlib/src/main/rasm/` for stdlib examples
- Check `/home/enrico/development/rust/rasm/rasm/resources/test/` for test examples
