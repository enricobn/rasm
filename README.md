# RASM

A native compiler (for now it produces only 80386/x86-64, linux executables, depending on target) for a "non pure" functional language.  
It is still in experimental phase (and probably it will always be...).

**It is not secure, so try it at your own risk!!!**

## Build

### Build prerequisites

To build the compiler you need the rust toolchain (<https://www.rust-lang.org/tools/install>)

```bash
cargo build --release
```

## Compile

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
"name" = { path = "path to the root of the library project" }
```

### Usage

```text
Arguments:
  <ACTION>  the action to perform [possible values: build, test, server, ui]
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
          prints debug informations at runtime (very verbose)
  -m, --memoryinfo
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
