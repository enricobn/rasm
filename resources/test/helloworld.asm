SECTION .data
_rasm_s0    db    'Hello world', 0h
SECTION .text
global  _start

_start:
    push    _rasm_s0
    call    sprintln
    mov     ebx, 01
    mov     eax, 1
    int     80h
    ret

sprintln:
push    ebp
mov     ebp,esp
pop     ebp
ret

sprint:
push    ebp
mov     ebp,esp

    ; to be generated
    ; :sprint
    ; push    ebp
    ; mov     ebp,esp
    push    eax
    push    ebx
    mov     ebx,[ebp+4+4]
    push    ebx
    call    slen
    pop     ebx

    push    edx
    push    ecx
    push    ebx

    mov     edx, eax
    pop     eax

    mov     ecx, eax
    mov     ebx, 1
    mov     eax, 4
    int     80h

    pop     ebx
    pop     ecx
    pop     edx
    pop     eax
    ; to be generated
    ; pop     ebp
    ; ret
pop     ebp
ret

slen:
push    ebp
mov     ebp,esp

    ; to be generated
    ; slen:
    ; push    ebp             ; Save the bottom pointer of the stack into the stack
    ; mov     ebp, esp        ; Set the bottom of the stack to the top of the stack (I guess so the stack seems to be empty)
    push    ebx             ; Save ebx to the stack since we use it
    mov     eax, [ebp+4+4]  ; Get the parameter from the stack (4 the PC + 4 ebp) and put it in eax
    mov     ebx, eax

nextchar:
    cmp     byte [eax], 0
    jz      finished
    inc     eax
    jmp     nextchar

finished:
    sub     eax, ebx
    pop     ebx
    ; to be generated
    ; pop     ebp             ; Restore the top of the stack
    ; ret
pop     ebp
ret
