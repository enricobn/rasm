# RASM

A native compiler (for now it produces only 80386/x86-64, linux executables, depending on target) for a "non pure" functional language.  
It is still in experimental phase (and probably it will always be...).

**It is not secure, so try it at your own risk!!!**

## Language Syntax

### Functions

```rasm
pub fn name<T>(param: T) -> T {
    param;
}

fn name() -> int { 0 }

fn name() -> float { 0.0 }

fn name() -> str { "" }
```

### Structs

```rasm
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
let name = value;
let name = value.method().anotherMethod();
```

### Match Expressions

```rasm
match(value,
    fn(pattern) { result; },
    { default; });

match(option, fn(v) { v; }, { defaultValue; });

match(c, { lessCase; }, { equalCase; }, { greaterCase; });
```

### Closures

```rasm
fn(x) { x }
fn(x, y) { x.add(y) }
fn(accum, current) { accum.add(current); }
```

### Method Calls

```rasm
value.method()
value.method(arg)
vector.map(fn(x) { x })
list.foldLeft(zero, fn(acc, x) { acc.add(x); })
```

### Literals

```rasm
42          // int
3.14        // float
"hello"     // str
'a'         // char
true, false // bool
```

### Built-in Types

- `int`, `float`, `bool`, `str`, `char`

### Macros

Compile-time macros are invoked with `!` suffix. Examples from the stdlib:

```rasm
vec!(1, 2, 3)            // create vec from values
println!("Hello {}", x)  // print with format
print!("Value: {}", v)   // print without newline
```

Format placeholders `{}` are replaced by arguments in order.

Attribute macros auto-generate methods, from stdlib:

```rasm
@toString()
@eq()
pub struct Pair<A,B> {
    first: A,
    second: B
}
```

This generates `toString` and `eq` implementations automatically.

### Defining Macros

There are two kinds of macros:

#### Expression Macros

Call with `!` suffix, receive AST expressions:

```rasm
pub fn vec(exprs: Vec<ASTExpression>) -> MacroExpressionResult {
    exprs.first.match(
        fn(first) {
            let start = simpleASTCall("vecOf", vecOf(first));
            let result = exprs.enumerate.filter(fn(act) { act.index.greater(0);})
                .foldLeft(start, fn(prev, act) {
                    simpleASTCall("push", vecOf(prev, act.value));
                });
            MacroExpressionOk(result, Vec());
        },
        { MacroExpressionResult::MacroError("No values, use Vec()"); }
    );
}
```

Returns `MacroExpressionOk(expr, functions)` or `MacroExpressionResult::MacroError(message)`.

#### Statement Macros

Used for statements (like `println!`):

```rasm
pub fn println(s: str, exprs: Vec<ASTExpression>) -> MacroStatementResult {
    let parameters = vecOf(stringASTValue(s)).add(exprs);
    let f = format(s, exprs);
    f.match(fn(expr, functions) {
        let statement = ASTExpressionStatement(simpleASTCall("println", vecOf(expr)));
        MacroStatementOk(vecOf(statement), functions);
    }, fn(error) {
        MacroStatementResult::MacroError(error);
    });
}
```

Returns `MacroStatementOk(statements, functions)` or `MacroError(message)`.

#### Attribute Macros

Attach to structs/enums with `@`:

```rasm
@toString()
pub struct Pair<A,B> { first: A, second: B }
```

The function receives an `ASTStructDef` or `ASTEnumDef`:

```rasm
pub fn toString(s: ASTStructDef) -> MacroAttributeResult {
    // generate toString function from struct definition
    let function = ASTFunctionDef(...);
    MacroAttributeOk(vecOf(function));
}
```

Returns `MacroAttributeOk(functions)` or `MacroError(message)`.

### AST Builder Functions

Helper functions to construct AST, in stdlib:

```rasm
simpleASTCall("name", vecOf(args))         // function call
stringASTValue("hello")                    // string literal
integerASTValue(42)                        // int literal
booleanASTValue(true)                      // bool literal
ASTValueRefExpression("name")              // variable reference
```

## Compile a rasm project

### Compile prerequisites

#### On Ubuntu

```bash
sudo apt install gcc-multilib g++-multilib libc++-dev nasm
```

### Directory structure of a rasm project

```text
rasm.toml
src  
  main  
    rasm  
    resources  
    nasmi386
    c
    ...other arch  
  test  
    rasm  
    resources
```

### Structure of rasm.toml

```toml
[package]  
name=
version=
main=

[dependencies]  
"name" = "version" or { path = "path to the root of the library project" }
```

### Usage

```text
Usage: rasm [OPTIONS] <ACTION> [file]

Arguments:
  <ACTION>  the action to perform [possible values: build, install, run, buildtest, test, server, ui]
  [file]    the input directory or file

Options:
  -t <target>
          the compiler target [default: nasmi386] [possible values: nasmi386, c]
  -o <out>
          the output folder of generated artifacts, if not set, the "target" folder under the project's root
      --compile
          creates only .asm/.c and .o files
      --message-format <message-format>
          for vscode
  -d, --debug
          compiles with debug symbols and includes comments in generated code
  -D, --memorydebug
          prints memory debug informations at runtime (very verbose)
  -M, --memoryinfo
          prints memory informations
  -p, --printcode
          prints code
  -r, --release
          optimize for release
      --arguments <arguments>
          arguments to be passed to main/test when run
      --include-tests <include-tests>
          a comma separated list of test functions to be included
  -h, --help
          Print help
  -V, --version
          Print version
```

To build a project from its root:
`rasm build`

an executable will be created in the `target` directory.

To build a project from another directory:
`rasm build <directory>`

an executable will be created in the `<directory>/target` directory.

There is a limited support for building a single file, since you need some library to do something useful,
by default to a single file project is added a dependency to stdlib 0.1 compatible version,
so before compiling such a project, you have to install stdlib :  
`rasm build <name>.rasm`

an executable `<name>` will be created in the current directory.

## Examples

to successfully compile the examples you have to install some rasm libraries, run:  
`./install_libs.sh`

In the examples the -- is really not needed when running manually from the command line,
it's needed if you are running it with an IDE that parses markdown and lets
you run it from the IDE itself.

### breakout

```bash
cargo run --release -- build rasm/resources/examples/breakout/ -o .
```

a "breakout" executable file will be created in the current folder

### fibonacci

```bash
cargo run --release -- build rasm/resources/test/fibonacci.rasm -o .
```

```bash
./fibonacci 40
```

it should print the fortieth fibonacci number (102334155)

## SDL examples

### nasmi386 target

### To install SDL 32 bit libraries on Ubuntu

```bash
sudo apt-get install libsdl2-dev:i386
```

### To install SDL TTF 32 bit libraries on Ubuntu

```bash
sudo apt-get install libsdl2-ttf-dev:i386
```

### To install SDL image 32 bit libraries on Ubuntu needed for some examples and for running tests

```bash
sudo apt-get install libsdl2-image-dev:i386
```

### c target

### To install SDL libraries on Ubuntu needed for some examples and for running tests

```bash
sudo apt-get install libsdl2-dev
```

### To install SDL TTF libraries on Ubuntu needed for some examples and for running tests

```bash
sudo apt-get install libsdl2-ttf-dev
```

### To install SDL image libraries on Ubuntu needed for some examples and for running tests

```bash
sudo apt-get install libsdl2-image-dev
```

## profiling

### valgrind

Only executables produced with libc support can be run with valgrind.

```bash
sudo apt-get install libc6-dbg:i386
```

### profiling build

```bash
sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'  
cargo build
valgrind --tool=callgrind target/debug/rasm build ...
callgrind_annotate --auto=yes callgrind.out.`<pid>`
```

### profiling executable

```bash
sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'  
valgrind --tool=callgrind `<executable>`  
callgrind_annotate --auto=yes callgrind.out.`<pid>`
```
