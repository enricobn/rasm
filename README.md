# RASM

A native compiler (for now it produces only i386/x86-64 Linux executables, depending on the target) for a "non-pure" functional language.  
It is still in an experimental phase (and probably it will always be...)

**It is not secure, so try it at your own risk!!!**

This is a brief introduction, for more insides look at [docs/index.html](docs/index.html)

## Language Syntax

Every statement must end with a semicolon (`;`), including statements inside closures and statements outside a function or closure, and even the last statement in a function/closure which represents the return value.

### Main

There is no main function in the language; statements outside a function/closure are treated as the "main function".
This is a valid RASM program:

```rasm
println("Hello world");
```

### Built-in Types

`int`, `float`, `bool`, `str`, `char`

### Literals

```rasm
42          // int
3.14        // float
"hello"     // str
'a'         // char
true, false // bool
```

### Operators, predefined functions and macros

There are no operators, predefined functions, or macros in the language, except for the automatically defined functions for structs and enums (see below).

### Functions

```rasm
pub fn addTwo(n: int) -> int {
    add(n, 2); // the last statement of a function is the return value.
}

// generic function
pub fn addOne<T>(n: T) -> T {
    add(n, 1);
}

fn anInt() -> int { 0; }

// method: no return type
fn printName(name: str) {
    println("Name: ", name);
}
```

### Function Calls

```rasm
let two = add(1, 1);

// syntactic sugar...
let three = two.add(1);

// generic function call, useful when the compiler cannot determine the type automatically
let v = evaluate<int>("10"); // probably it's not possible to create such function, but as an example...
```

### Structs

```rasm
@toString() // this is an attribute macro (see below)
pub struct Pair<A,B> {
    first: A,
    second: B
}

let p = Pair("number", 1);
println(first(p));
// or with syntactic sugar...
println(p.first);

let p1 = p.second(2); // Pair("number", 2)
let p2 = p.second(fn(v) { v.add(1);}); // Pair("number", 2)
```

### Enums

```rasm
pub enum Option<T> {
    Some(value: T),
    None
}

pub enum Planet {
    Earth,
    Mars,
    Venus,
    Other
}

let v = Some(10);
let s = v.match(fn(v) { v.add(1);}, { 0;}); // Some(11)

let planet = Earth(); // or, for disambiguation... Planet::Earth();
println(planet.matchEarth({"it's the Earth";}, { "it's not the Earth";}));
```

### Variables

```rasm
let one = 1;
let two = one.add(1);
```

### Closures

```rasm
fn(x) { x; }
fn(x, y) { x.add(y); }
fn(accum, current) { accum.add(current); }
```

### Macros

Compile-time macros are invoked with `!` suffix. Examples from the stdlib:

```rasm
vec!(1, 2, 3)            // create vec from values
println!("Hello {}", x)  // print with format
print!("Value: {}", v)   // print without newline
```

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

## Compile a rasm project

The language itself does not define how a project is organized; the organization depends on the compiler.

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

to get an help of the command line tools

```bash
rasm --help

```

To build a project from its root:
`rasm build`

an executable will be created in the `target` directory.

To build a project from another directory:
`rasm build <directory>`

an executable will be created in the `<directory>/target` directory.

There is limited support for building a single file, since you need some library to do something useful.
By default, a dependency on a stdlib 0.1 compatible version is added to a single file project,
so before compiling such a project, you have to install stdlib:  
`rasm build <name>.rasm`

an executable `<name>` will be created in the current directory.

## Examples

To compile the examples successfully, install some RASM libraries by running:  
`./install_libs.sh`

In the examples, the `--` is really not needed when running manually from the command line, it's needed if you are running it with an IDE that parses markdown and lets you run the program from the IDE.

### breakout

```bash
cargo run --release -- build rasm/resources/examples/breakout/ -o .
```

a "breakout" executable will be created in the current folder

### fibonacci

```bash
cargo run --release -- build rasm/resources/test/fibonacci.rasm -o .
```

```bash
./fibonacci 40
```

it should print the 40th Fibonacci number (102334155)

## SDL examples

### nasmi386 target

To install SDL 32 bit libraries on Ubuntu

```bash
sudo apt install libsdl2-dev:i386
```

To install SDL TTF 32 bit libraries on Ubuntu

```bash
sudo apt install libsdl2-ttf-dev:i386
```

To install SDL image 32 bit libraries on Ubuntu needed for some examples and for running tests

```bash
sudo apt install libsdl2-image-dev:i386
```

### c target

To install SDL libraries on Ubuntu needed for some examples and for running tests

```bash
sudo apt install libsdl2-dev
```

To install SDL TTF libraries on Ubuntu needed for some examples and for running tests

```bash
sudo apt install libsdl2-ttf-dev
```

To install SDL image libraries on Ubuntu needed for some examples and for running tests

```bash
sudo apt install libsdl2-image-dev
```
