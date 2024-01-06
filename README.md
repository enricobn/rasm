# RASM

A native compiler (for now it produces only 386 linux executables) for a "not pure" functional language.  
It is still in experimental phase (and probably it will always be..).   
**It is not secure, so try it at your own risk!!!**

## Build

To build the compiler you need the rust toolchain (https://www.rust-lang.org/tools/install):  
`cargo build --release`

## Compile

### Prerequisites

#### On Ubuntu

`sudo apt install gcc-multilib g++-multilib libc++-dev nasm`

### Directory structure of a rasm project
src  
&nbsp;&nbsp;main  
&nbsp;&nbsp;&nbsp;&nbsp;rasm  
&nbsp;&nbsp;&nbsp;&nbsp;resources  
&nbsp;&nbsp;nasmi386  
&nbsp;&nbsp;...other arch  
&nbsp;&nbsp;test  
&nbsp;&nbsp;&nbsp;&nbsp;rasm  
&nbsp;&nbsp;&nbsp;&nbsp;resources     
rasm.toml

### Structure of rasm.toml
[package]  
name=  
version=  
main=  
out=  

[dependencies]  
"name" = { path = "path to the root of the library project" }


### Compile a rasm program

Usage: rasm [OPTIONS] <ACTION>

Arguments:
<ACTION>  The action to perform, that can be: build, run, test [possible values: build, run, test]

Options:
-f <file>                              Sets the input directory or file to use
-o <out>                               Sets the output file to use
--compile                          produces .asm and .o files
--message-format <message-format>  for vscode
-h, --help                             Print help
-V, --version                          Print version


The simplest way is to build a project from its root :
`rasmexecutable build`

an executable will be created in the target folder

### Examples

# breakout
`cargo run --release -- build -f rasm/resources/examples/breakout/ -o breakout`  

a "breakout" executable file will be created in the current folder

# fibonacci
`cargo run --release -- build -f rasm/resources/test/fibonacci.rasm -o fibonacci`  
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