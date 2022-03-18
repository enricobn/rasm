if cargo run --release resources/test/fibonacci.rasm fibonacci.asm ; then
    if nasm -f elf -g -F dwarf fibonacci.asm ; then
      #nasm -O0 -f elf fibonacci.asm
      if ld -m elf_i386 fibonacci.o -o fibonacci ; then
        ./fibonacci
      fi
    fi
fi

