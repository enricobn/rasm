## gdb

- Show 4 words in the stack in gdb    
  gdb> x/4xw $sp
- print registers  
  info registers
- Show 4 words in memory address  
  gdb> x/4xw 0xmemoryaddress
- print a string  
  printf "%s", x

## profile
To use valgrind you have to link with gcc, but for now there is not a way to force it, other than requiring libc:  
requires "libc"

valgrind --tool=callgrind ./list
callgrind_annotate --tree=both --inclusive=yes --auto=yes --show-percs=yes callgrind.out."pid"

