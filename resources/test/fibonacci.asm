section .bss                                      ; generated
  _rasm_buffer_10b resb 10                        ; generated
SECTION .text                                     ; generated
global  main                                      ; generated
                                                  ; generated
main:                                             ; generated
; calling function nprintln                       ; generated
; calling function fib                            ; generated
    push    40                                    ; generated
    call    fib                                   ; generated
    add     esp,4                                 ; generated
; end calling function fib                        ; generated
    push    eax                                   ; generated
    call    nprintln                              ; generated
    add     esp,4                                 ; generated
; end calling function nprintln                   ; generated
    mov     ebx, 01                               ; generated
    mov     eax, 1                                ; generated
    int     80h                                   ; generated
    ret                                           ; generated
fib:                                              ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
; inlining function if                            ; generated
    push    dword[ebp+4+4]                        ; generated
    push     lambda0                              ; generated
    push     lambda1                              ; generated
; inlining function lessOrEqual                   ; generated
    push     dword [ebp+4+4]                      ; generated
; To remove from stack  lessOrEqual 4             ; generated

    push    ebx
    mov     eax,1 ; true
    mov     ebx,[ebp-16]
    cmp     ebx,1
    jbe     $+7  ; Jump if Below or Equal (unsigned comparison)
    mov     eax,0 ; false
    pop     ebx
    add     esp,4                                 ; generated
; end inlining function lessOrEqual               ; generated
    push    eax                                   ; generated
; To remove from stack  if 4                      ; generated

    cmp     word [ebp-16], 0
    jz      $+7
    call    [ebp-12] ; true value
    jmp     $+5
    call    [ebp-8] ; false value
    add     esp,16                                ; generated
; end inlining function if                        ; generated
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
lambda0:                                          ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
; inlining function nadd                          ; generated
; calling function fib                            ; generated
; inlining function nadd                          ; generated
    push     dword [ebp+4+16]                     ; generated
; To remove from stack  nadd 1                    ; generated

    mov     eax, [ebp-4]
    add     eax, -2
    add     esp,4                                 ; generated
; end inlining function nadd                      ; generated
    push    eax                                   ; generated
    call    fib                                   ; generated
    add     esp,4                                 ; generated
; end calling function fib                        ; generated
    push    eax                                   ; generated
; calling function fib                            ; generated
; inlining function nadd                          ; generated
    push     dword [ebp+4+16]                     ; generated
; To remove from stack  nadd 2                    ; generated

    mov     eax, [ebp-8]
    add     eax, -1
    add     esp,4                                 ; generated
; end inlining function nadd                      ; generated
    push    eax                                   ; generated
    call    fib                                   ; generated
    add     esp,4                                 ; generated
; end calling function fib                        ; generated
    push    eax                                   ; generated
; To remove from stack  nadd 2                    ; generated

    mov     eax, [ebp-8]
    add     eax, [ebp-4]
    add     esp,8                                 ; generated
; end inlining function nadd                      ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated
lambda1:                                          ; generated
    push    ebp                                   ; generated
    mov     ebp,esp                               ; generated
; inlining function itn                           ; generated
    push     dword [ebp+4+16]                     ; generated
; To remove from stack  itn 1                     ; generated

    mov     eax,[esp]
    add     esp,4                                 ; generated
; end inlining function itn                       ; generated
    pop     ebp                                   ; generated
    ret                                           ; generated