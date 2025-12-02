section .bss
buffer resb 64000 ; reserve 64K for input buffer
output resb 512   ; textual output

section .text
global _start
_start:
    ; read input from stdout into a big buffer.
    mov rax, 0        ; read(2)
    mov rdi, 0        ; stdin fd
    mov rsi, buffer   ; ptr to buffer
    mov rdx, 17190    ; input size
    syscall

    ; then, initialise running state
    lea  r8, [rel buffer] ; buffer start
    add  r8, rax          ; r8 = buffer end (buffer + bytes_read)
    mov  r9, 50           ; dial starts at 50
    mov r10,  0           ; alignment counter
    lea r11, [rel buffer] ; scanning pointer

    ; scan through data
  loop:
    cmp r11, r8          ; check if at end of buffer
    jge loop_done        ; finish if so
    xor   rax, rax       ; clean register
    mov    al, [r11]     ; load first byte of instruction
    mov   rbx, 0         ; multiplier for integer
    mov   rcx, -1        ; load -1 for L direction
    cmp    al, 76        ; compare to b'L'
    cmove rbx, rcx       ; if L, swap direction
    mov   rcx, 1         ; load 1 for R direction
    cmp    al, 82        ; compare to b'R'
    cmove rbx, rcx       ; if R, don't swap direction
    cmp   rbx, 0         ; if neither 1 nor -1,
    je loop_done         ; finish
    inc   r11            ; move to next byte
    mov   rax, 0         ; initialise parser
  parse:
    cmp   r11, r8        ; check if at end of buffer
    jge   parse_done     ; exit if so
    xor   rcx, rcx       ; clear rcx
    mov    cl, [r11]     ; load byte of number
    cmp    cl, 10        ; if it's a line feed,
    je parse_done        ; exit parse loop
    imul  rax, rax, 10   ; make space for digit
    sub    cl, 48        ; deäsciify
    movzx rdx, cl        ; zext to 64-bit
    add   rax, rdx       ; insert digit
    inc   r11            ; move to next byte
    jmp   parse          ; continue parsing
  parse_done:
    inc   r11            ; skip the newline
    imul  rax, rbx       ; flip number if L
  ; part 2
    mov   r12, rax       ; save rot val
    mov   r13, r9        ; a = angle
    add   r13, rbx       ; a = angle + dir
    mov   r14, r9        ; b = angle
    add   r14, rax       ; b = angle + val * dir
    ; ensure r13 <= r14
    cmp   r13, r14       ; compare a and b
    jle   no_swap        ; if a <= b, no swap needed
    xchg  r13, r14       ; otherwise swap them
  no_swap:
    ; now r9 += max(b // 100 - (a + 99) // 100 + 1, 0)
    ; r14 = floor_div(r14, 100):
    mov   rax, r14
    mov   rcx, 100
    cqo                  ; sign extend rax into rdx:rax
    idiv  rcx            ; rax = quotient, rdx = remainder
    test  rdx, rdx       ; check if remainder is negative
    jns   no_adjust1     ; if non-negative, skip adjustment
    dec   rax            ; if negative remainder, subtract 1 from quotient
  no_adjust1:
    mov   r14, rax
    ; r13 = floor_div(r13 + 99, 100)
    add   r13, 99
    mov   rax, r13
    mov   rcx, 100
    cqo                  ; sign extend rax into rdx:rax
    idiv  rcx            ; rax = quotient, rdx = remainder
    test  rdx, rdx       ; check if remainder is negative
    jns   no_adjust2     ; if non-negative, skip adjustment
    dec   rax            ; if negative remainder, subtract 1 from quotient
  no_adjust2:
    mov   r13, rax
    ; compute r14 - r13 + 1
    sub   r14, r13
    inc   r14
    mov   r13, 0
    cmp   r14, 0
    cmovl r14, r13
    add   r10, r14
    mov   rax, r12       ; restore rot
  ; part 2
    add    r9, rax       ; rotate dial
    ; <mod100>
    mov    rax, r9       ; save original value
    xor    edx, edx      ; zero out remainder
    mov    rcx, 100
    cqo                  ; sign extend rax to rdx:rax
    idiv   rcx           ; signed divide by 100
    test   rdx, rdx      ; check if remainder is negative
    jns    mod_done      ; if non-negative, we're done
    add    rdx, 100      ; adjust negative remainder
  mod_done:
    mov    r9, rdx       ; store result
    ; </mod100>
  ; deleted for part 2
    ; cmp r9 , 0 ; if zero,
    ; jne loop
    ; inc r10    ; increment alignment counter
  ; deleted for part 2
    jmp loop
  loop_done:
    ; now, we need to serialise the alignment counter.
    ; simple approach: pad with zeros to make it 16 digits
    mov  rax, r10               ; load alignment counter
    lea  rsi, [rel output + 15] ; start at end of 16-byte buffer
    mov  rcx, 10   ; divisor
    mov  r8, 16    ; counter for 16 digits
  serialise:
    xor   edx, edx  ; zero out edx
    div   rcx       ; divide rax by 10, remainder in rdx
    add    dl, 48   ; convert to ASCII
    mov [rsi], dl   ; write digit
    dec   rsi       ; move backwards
    dec    r8
    test   r8, r8
    jnz serialise

    ; print the answer (all 16 digits)
    mov rdi, 1            ; stdout fd
    lea rsi, [rel output] ; start of output buffer
    mov rdx, 16           ; 16 digits
    mov rax, 1            ; write(2)
    syscall

    ; exit
    mov rax, 60       ; sys_exit
    xor rdi, rdi      ; exit code 0
    syscall
