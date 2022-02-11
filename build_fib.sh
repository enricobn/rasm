if cargo run --release resources/test/fibonacci.rasm fibonacci.asm ; then
    # nasm -f elf -g -F dwarf fibonacci.asm
    nasm -f elf fibonacci.asm
    ld -m elf_i386 fibonacci.o -o fibonacci
    ./fibonacci
fi

