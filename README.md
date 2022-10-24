# RASM

A native compiler (for now it produces only 386 linux executables) for a "not pure" functional language.  
It is still in experimental phase (and probably it will always be..).   
**It is not secure, so try it at your own risk!!!**

## Build

To build the compiler you need the rust toolchain (https://www.rust-lang.org/tools/install):  
`cargo build --release`

## Compile

To compile a rasm program:  
`target/release/rasm "file name".rasm`    
it will produce a "file name" executable

for example:  
`target/release/rasm resources/test/fibonacci.rasm`  
will produce fibonacci file  
you can run it with:  
`./fibonacci 40`  
it should print the fortieth fibonacci number (102334155)

## LIBC examples/test

### To install 32 bit libc libraries on Ubuntu

`sudo apt install gcc-multilib g++-multilib`  
`sudo apt install libc++-dev`

## SDL examples

install libc libraries (see above)

### To install SDL 32 bit libraries on Ubuntu

`sudo apt-get install libsdl2-dev:i386`

## valgrind

Only executables produced with libc support can be run with valgrind.

`sudo apt-get install libc6-dbg:i386`

