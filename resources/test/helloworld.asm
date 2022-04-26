SECTION .data
_rasm_s0    db    'Hello world', 0h               ; generated
_rasm_s1    db    'Hi', 0h                        ; generated
_rasm_s2    db    'two', 0h                       ; generated
_rasm_s3    db    'one', 0h                       ; generated
_rasm_s6    db    'assertion failed', 0h          ; generated
section .bss                                      ; generated
  _rasm_buffer_10b resb 10                        ; generated
  _rasm_args resw 12                              ; generated
SECTION .text                                     ; generated
global  main                                      ; generated
                                                  ; generated
main:                                             ; generated
mov     eax,[esp + 0]                             ; generated
mov     [_rasm_args + 0], eax                     ; generated
mov     eax,[esp + 4]                             ; generated
mov     [_rasm_args + 4], eax                     ; generated
mov     eax,[esp + 8]                             ; generated
mov     [_rasm_args + 8], eax                     ; generated
mov     eax,[esp + 12]                            ; generated
mov     [_rasm_args + 12], eax                    ; generated
mov     eax,[esp + 16]                            ; generated
mov     [_rasm_args + 16], eax                    ; generated
mov     eax,[esp + 20]                            ; generated
mov     [_rasm_args + 20], eax                    ; generated
mov     eax,[esp + 24]                            ; generated
mov     [_rasm_args + 24], eax                    ; generated
mov     eax,[esp + 28]                            ; generated
mov     [_rasm_args + 28], eax                    ; generated
mov     eax,[esp + 32]                            ; generated
mov     [_rasm_args + 32], eax                    ; generated
mov     eax,[esp + 36]                            ; generated
mov     [_rasm_args + 36], eax                    ; generated
mov     eax,[esp + 40]                            ; generated
mov     [_rasm_args + 40], eax                    ; generated
mov     eax,[esp + 44]                            ; generated
mov     [_rasm_args + 44], eax                    ; generated
; calling function helloWorld                     ; generated
    call    helloWorld                            ; generated
; end calling function helloWorld                 ; generated
    mov     ebx, 0                                ; generated
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
; inlining function nadd                          ; generated
; To remove from stack  nadd 0                    ; generated

    mov     eax, 15
    add     eax, 25
; end inlining function nadd                      ; generated
    push    eax                                   ; generated
    call    nprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function nprintln                   ; generated
; calling function nprintln                       ; generated
; inlining function nadd                          ; generated
; inlining function nadd                          ; generated
; To remove from stack  nadd 0                    ; generated

    mov     eax, 10
    add     eax, 20
; end inlining function nadd                      ; generated
    push    eax                                   ; generated
; To remove from stack  nadd 1                    ; generated

    mov     eax, 15
    add     eax, [ebp-4]
    add     esp,4                                 ; generated
; end inlining function nadd                      ; generated
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
itn:                                              ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    mov     eax,[ebp+8]
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
  mov     eax,[ebp+8] ; integer value to convert
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
    mov     eax,[ebp+8]
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
    mov     eax, [ebp+8]  ; Get the parameter from the stack (4 the PC + 4 ebp) and put it in eax
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

    mov     eax, [ebp+8]
    add     eax, [ebp+12]
    pop     ebp                                   ; generated
    ret                                           ; generated
if:                                               ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    cmp     word [ebp+8], 0
    jz      $+7
    call    [ebp+12] ; true value
    jmp     $+5
    call    [ebp+16] ; false value
    pop     ebp                                   ; generated
    ret                                           ; generated
lessOrEqual:                                      ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    push    ebx
    mov     eax,1 ; true
    mov     ebx,[ebp+8]
    cmp     ebx,[ebp+12]
    jbe     $+7  ; Jump if Below or Equal (unsigned comparison)
    mov     eax,0 ; false
    pop     ebx
    pop     ebp                                   ; generated
    ret                                           ; generated
less:                                             ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    push    ebx
    mov     eax,1 ; true
    mov     ebx,[ebp+8]
    cmp     ebx,[ebp+12]
    jb     $+7  ; Jump if Below or Equal (unsigned comparison)
    mov     eax,0 ; false
    pop     ebx
    pop     ebp                                   ; generated
    ret                                           ; generated
greater:                                          ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    push    ebx
    mov     eax,1 ; true
    mov     ebx,[ebp+8]
    cmp     ebx,[ebp+12]
    jg      $+7  ; Jump if greater (unsigned comparison)
    mov     eax,0 ; false
    pop     ebx
    pop     ebp                                   ; generated
    ret                                           ; generated
eq:                                               ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    push    ebx
    mov     eax,1 ; true
    mov     ebx,[ebp+8]
    cmp     ebx,[ebp+12]
    je      $+7  ; Jump if equals
    mov     eax,0 ; false
    pop     ebx
    pop     ebp                                   ; generated
    ret                                           ; generated
exit:                                             ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    mov     ebx, [ebp+8]    ; Arg one: the status
    mov     eax, 1          ; Syscall number:
    int     0x80
    pop     ebp                                   ; generated
    ret                                           ; generated
assert:                                           ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
; inlining function if                            ; generated
    push    dword [ebp+4+4]                       ; generated
    push     lambda4                              ; generated
    push     lambda5                              ; generated
; To remove from stack  if 3                      ; generated

    cmp     word [ebp+4+4], 0
    jz      $+7
    call    [ebp-12] ; true value
    jmp     $+5
    call    [ebp-8] ; false value
    add     esp,12                                ; generated
; end inlining function if                        ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
argc:                                             ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    mov     eax,[_rasm_args]
    pop     ebp                                   ; generated
    ret                                           ; generated
argv:                                             ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

    push ebx
    push ecx
    mov ebx, _rasm_args
    mov ecx,4           ; i add 4*i to the base address (_rasm_args)
    mov eax,[ebp+8]
    inc eax             ; we skip the length (argc)
    mul ecx
    add ebx,eax
    mov eax,[ebx]
    pop ecx
    pop ebx
    pop     ebp                                   ; generated
    ret                                           ; generated
atoi:                                             ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated

  push    esi
  push    ebx
  push    ecx
  push    edx
    mov edx, [ebp+8] ; our string
    xor eax, eax ; zero a "result so far"
    .top:
    movzx ecx, byte [edx] ; get a character
    inc edx ; ready for next one
    cmp ecx, '0' ; valid?
    jb .done
    cmp ecx, '9'
    ja .done
    sub ecx, '0' ; "convert" character to number
    imul eax, 10 ; multiply "result so far" by ten
    add eax, ecx ; add in current digit
    jmp .top ; until done
.done:
  pop    edx
  pop    ecx
  pop    ebx
  pop    esi
    pop     ebp                                   ; generated
    ret                                           ; generated
lambda4:                                          ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
; calling function sprintln                       ; generated
    push    _rasm_s6                              ; generated
    call    sprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function sprintln                   ; generated
; calling function exit                           ; generated
    push    1                                     ; generated
    call    exit                                  ; generated
    add     esp,4                                 ; generated
; end calling function exit                       ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
lambda5:                                          ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated