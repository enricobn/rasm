# RASM

A native compiler (for now it produces only 386 linux executables) for a "not pure" functional language.  
It is still in experimental phase (and probably it will always be...).   
**It is not secure, so try it at your own risk!!!**

## Build

To build the compiler you need the rust toolchain (https://www.rust-lang.org/tools/install):  
`cargo build --release`

## Compile

### Prerequisites

#### On Ubuntu

`sudo apt install gcc-multilib g++-multilib libc++-dev nasm`

### Directory structure of a rasm project

```
rasm.toml
src  
  main  
    rasm  
    resources  
  nasmi386  
  ...other arch  
  test  
    rasm  
    resources
```

### Structure of rasm.toml

[package]  
name=  
version=  
main=  
out=

[dependencies]  
"name" = { path = "path to the root of the library project" }

### Usage

```
Usage: rasm [OPTIONS] <ACTION> <file>

Arguments:
<ACTION>  the action to perform [possible values: build, test, server]
<file>    the project directory or a single source file

Options:  
-o <out>                               sets the output file to create  
--compile                              creates only .asm and .o files  
--message-format <message-format>      for vscode  
-d, --debug                            prints debug information at runtime (verbose)  
-m, --memoryinfo                       prints memory informations  
-p, --printcode                        prints code  
-h, --help                             prints help  
-V, --version                          prints version
```

To build a project from its root:
`rasm build`

an executable will be created in the `target` directory.

To build a project from another directory:
`rasm build <directory>`

an executable will be created in the `<directory>/target` directory.

There is a limited support for building a single file, since you need some library to do something useful,
you can define an environment variable called RASM_STDLIB with a path to a library to be added as a dependency:
`rasm build <name>.rasm`

an executable `<name>` will be created in the current directory.

### Examples

In the examples the -- is really not needed when running manually from the command line,
it's needed if you are running it with an IDE that parses markdown and lets
you run it from the IDE itself.

# breakout

`cargo run --release -- build rasm/resources/examples/breakout/ -o breakout`

a "breakout" executable file will be created in the current folder

# fibonacci

`cargo run --release -- build rasm/resources/test/fibonacci.rasm -o fibonacci`  
`./fibonacci 40`  
it should print the fortieth fibonacci number (102334155)

## SDL examples

### To install SDL 32 bit libraries on Ubuntu

`sudo apt-get install libsdl2-dev:i386`

### To install SDL TTF 32 bit libraries on Ubuntu

`sudo apt-get install libsdl2-ttf-dev:i386`

## valgrind

Only executables produced with libc support can be run with valgrind.

`sudo apt-get install libc6-dbg:i386`

## profiling build

sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'  
CARGO_PROFILE_RELEASE_DEBUG=true cargo flamegraph -p rasm build -f rasm/resources/examples/breakout

## profiling executable

sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'  
valgrind --tool=callgrind "executable"  
callgrind_annotate --auto=yes callgrind.out."pid"