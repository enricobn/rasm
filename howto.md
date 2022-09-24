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

valgrind --tool=callgrind ./list
callgrind_annotate --tree=both --inclusive=yes --auto=yes --show-percs=yes callgrind.out."pid"

