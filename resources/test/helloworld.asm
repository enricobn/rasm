SECTION .data
_rasm_s1    db    'Hi', 0h                        ; generated
_rasm_s0    db    'Hello world', 0h               ; generated
section .bss                                      ; generated
  _rasm_buffer_10b resb 10                        ; generated
SECTION .text                                     ; generated
global  main                                      ; generated
                                                  ; generated
main:                                             ; generated
    call    helloWorld                            ; generated
    mov     ebx, 01                               ; generated
    mov     eax, 1                                ; generated
    int     80h                                   ; generated
    ret                                           ; generated
                                                  ; generated
helloWorld:                                       ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
    push    _rasm_s0                              ; generated
    call    sprintln                              ; generated
    add esp,4                                     ; generated
    push    _rasm_s1                              ; generated
    call    sprintln                              ; generated
    add esp,4                                     ; generated
    push    10                                    ; generated
    call    nprintln                              ; generated
    add esp,4                                     ; generated
    push    10                                    ; generated
    push    20                                    ; generated
    call    nadd                                  ; generated
    add esp,4                                     ; generated
    add esp,4                                     ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
                                                  ; generated
sprintln:                                         ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
    push    eax                                   ; generated
    mov     eax,[ebp+4+4]                         ; generated
    push    eax                                   ; generated
    call    sprint                                ; generated
    pop     eax                                   ; generated
    pop     eax                                   ; generated
    call    println                               ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
                                                  ; generated
nprintln:                                         ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
    push    eax                                   ; generated
    mov     eax,[ebp+4+4]                         ; generated
    push    eax                                   ; generated
    call    nprint                                ; generated
    pop     eax                                   ; generated
    pop     eax                                   ; generated
    call    println                               ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
                                                  ; generated
nprint:                                           ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

  push    esi
  push    eax
  push    ebx
  push    ecx
  push    edx
  mov     eax,[ebp+4+4] ; integer value to convert
  mov     esi,_rasm_buffer_10b
  add     esi,9
  mov     byte [esi],0    ; String terminator

  mov ebx,10
  xor ecx,ecx         ; initialize length
.next_digit:
  xor edx,edx         ; Clear edx prior to dividing edx:eax by ebx
  div ebx             ; eax /= 10
  add dl,'0'          ; Convert the remainder to ASCII
  dec esi             ; store characters in reverse order
  inc ecx             ; length

  mov [esi],dl
  test eax,eax
  jnz .next_digit     ; Repeat until eax==0

  mov    edx,ecx ; the length
  mov     ecx,esi ; the pointer to the first digit (not necessarily the start of the provided buffer)
  mov     ebx, 1  ; stdout
  mov     eax, 4  ; write
  int     80h

  pop    edx
  pop    ecx
  pop    ebx
  pop    eax
  pop    esi
    pop     ebp                                   ; generated
    ret                                           ; generated
                                                  ; generated
println:                                          ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    mov     eax, 0Ah    ; move 0Ah into eax - 0Ah is the ascii character for a linefeed
    push    eax         ; push the linefeed onto the stack so we can get the address
    push    esp         ; push the address of the current stack pointer where is the \n char for sprint
    call    sprint      ; call our sprint function
    pop     eax         ; remove the linefeed char from the stack
    pop     eax
    pop     ebp                                   ; generated
    ret                                           ; generated
                                                  ; generated
sprint:                                           ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    push    edx
    push    ecx
    push    ebx
    push    eax
    mov     eax,[ebp+4+4]
    push    eax
    call    slen

    mov     edx, eax
    pop     eax

    mov     ecx, eax
    mov     ebx, 1
    mov     eax, 4
    int     80h

    pop     eax
    pop     ebx
    pop     ecx
    pop     edx
    pop     ebp                                   ; generated
    ret                                           ; generated
                                                  ; generated
slen:                                             ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    push    ebx             ; Save ebx to the stack since we use it
    mov     eax, [ebp+4+4]  ; Get the parameter from the stack (4 the PC + 4 ebp) and put it in eax
    mov     ebx, eax

.nextchar:
    cmp     byte [eax], 0
    jz      .finished
    inc     eax
    jmp     .nextchar

.finished:
    sub     eax, ebx
    pop     ebx
    pop     ebp                                   ; generated
    ret                                           ; generated
                                                  ; generated
nadd:                                             ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    push    ebx
    mov     eax, [ebp+4+4]
    mov     ebx, [ebp+4+8]
    add     eax, ebx
    pop     ebx
    pop     ebp                                   ; generated
    ret                                           ; generated