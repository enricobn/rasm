# How to

## gdb

- Show 4 words in the stack in gdb
  gdb> x/4xw $sp
- print registers  
  info registers
- Show 4 words in memory address  
  gdb> x/4xw 0xmemoryaddress
- print a string  
  printf "%s", x

## profiling

### valgrind

Only executables produced with libc support can be run with valgrind.

```bash
sudo apt install libc6-dbg:i386
```

### profiling build

```bash
sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'  
cargo build
valgrind --tool=callgrind target/debug/rasm build ...
callgrind_annotate --auto=yes callgrind.out.`<pid>`
```

or you can use KChacheGrind

### profiling executable

```bash
sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'  
valgrind --tool=callgrind `<executable>`  
callgrind_annotate --auto=yes callgrind.out.`<pid>`
```
