SECTION .data
_rasm_s0    db    'Hello world', 0h               ; generated
_rasm_s1    db    'Hi', 0h                        ; generated
_rasm_s2    db    'two', 0h                       ; generated
_rasm_s3    db    'one', 0h                       ; generated
section .bss                                      ; generated
  _rasm_buffer_10b resb 10                        ; generated
SECTION .text                                     ; generated
global  main                                      ; generated
                                                  ; generated
main:                                             ; generated
; calling function helloWorld                     ; generated
    call    helloWorld                            ; generated
; end calling function helloWorld                 ; generated
    mov     ebx, 01                               ; generated
    mov     eax, 1                                ; generated
    int     80h                                   ; generated
    ret                                           ; generated
helloWorld:                                       ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
; calling function sprintln                       ; generated
    push    _rasm_s0                              ; generated
    call    sprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function sprintln                   ; generated
; calling function sprintln                       ; generated
    push    _rasm_s1                              ; generated
    call    sprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function sprintln                   ; generated
; calling function nprintln                       ; generated
    push    10                                    ; generated
    call    nprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function nprintln                   ; generated
; calling function nprintln                       ; generated
; calling function nadd                           ; generated
    push    25                                    ; generated
    push    15                                    ; generated
    call    nadd                                  ; generated
    add     esp,8                                 ; generated
; end calling function nadd                       ; generated
    push    eax                                   ; generated
    call    nprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function nprintln                   ; generated
; calling function nprintln                       ; generated
; calling function nadd                           ; generated
; calling function nadd                           ; generated
    push    20                                    ; generated
    push    10                                    ; generated
    call    nadd                                  ; generated
    add     esp,8                                 ; generated
; end calling function nadd                       ; generated
    push    eax                                   ; generated
    push    15                                    ; generated
    call    nadd                                  ; generated
    add     esp,8                                 ; generated
; end calling function nadd                       ; generated
    push    eax                                   ; generated
    call    nprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function nprintln                   ; generated
; calling function sprintln2                      ; generated
    push    _rasm_s2                              ; generated
    push    _rasm_s3                              ; generated
    call    sprintln2                             ; generated
    add     esp,8                                 ; generated
; end calling function sprintln2                  ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
itn:                                              ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    mov     eax,[ebp+4+4]
    pop     ebp                                   ; generated
    ret                                           ; generated
sprintln2:                                        ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
; calling function sprintln                       ; generated
    push     dword [ebp+4+4]                      ; generated
    call    sprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function sprintln                   ; generated
; calling function sprintln                       ; generated
    push     dword [ebp+4+8]                      ; generated
    call    sprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function sprintln                   ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
sprintln:                                         ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
; calling function sprint                         ; generated
    push     dword [ebp+4+4]                      ; generated
    call    sprint                                ; generated
    add     esp,4                                 ; generated
; end calling function sprint                     ; generated
; calling function println                        ; generated
    call    println                               ; generated
; end calling function println                    ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
nprintln:                                         ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
; calling function nprint                         ; generated
    push     dword [ebp+4+4]                      ; generated
    call    nprint                                ; generated
    add     esp,4                                 ; generated
; end calling function nprint                     ; generated
; calling function println                        ; generated
    call    println                               ; generated
; end calling function println                    ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
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
if:                                               ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    cmp     word [ebp+4+4], 0
    jz     .falseValue
    call     [ebp+4+8] ; true value
    jmp      .finished
.falseValue:
    call     [ebp+4+12] ; false value
.finished:
    pop     ebp                                   ; generated
    ret                                           ; generated
lessOrEqual:                                      ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    push    ebx
    push    ecx
    mov     eax,1 ; true
    mov     ebx,[ebp+4+4] ; a
    mov     ecx,[ebp+4+8] ; b
    cmp     ebx,ecx
    jbe     .finished ; Jump if Below or Equal (unsigned comparison)
    mov     eax,0 ; false
.finished:
    pop     ecx
    pop     ebx
    pop     ebp                                   ; generated
    ret                                           ; generated