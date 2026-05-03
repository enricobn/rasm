# Compile a RASM Project

The language itself does not define how a project is organized; the organization depends on the compiler.

## Compile prerequisites

### On Ubuntu

```bash
sudo apt install gcc-multilib g++-multilib libc++-dev nasm
```

## Directory structure of a rasm project

```
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

## Structure of rasm.toml

```toml
[package]
name=
version=
main=

[dependencies]
"name" = "version" or { path = "path to the root of the library project" }
```

## Usage

```
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
          prints memory debug information at runtime (very verbose)
  -M, --memoryinfo
          prints memory information
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

To build a project from its root: `rasm build`

an executable will be created in the `target` directory.

To build a project from another directory: `rasm build <directory>`

an executable will be created in the `<directory>/target` directory.

There is limited support for building a single file, since you need some library to do something useful.

By default, a dependency on a stdlib 0.1 compatible version is added to a single file project,

so before compiling such a project, you have to install stdlib:

`rasm build <name>.rasm`

an executable `<name>` will be created in the current directory.