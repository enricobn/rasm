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

### Compile a rasm program

`target/release/rasm "name".rasm` 
it will produce a "name" executable

`target/release/rasm "name".rasm "executable name"`  
it will produce a "executable name" executable

for example:
- `target/release/rasm rasm/resources/test/fibonacci.rasm`  
  will produce the fibonacci executable file in the rasm/resources/test folder  
- `target/release/rasm rasm/resources/test/fibonacci.rasm fibonacci`  
  will produce the fibonacci executable file in the current folder, then you can run it with:  
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

## profiling
sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'
CARGO_PROFILE_RELEASE_DEBUG=true cargo flamegraph -p rasm rasm/resources/examples/breakout
