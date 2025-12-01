section .bss
buffer resb 8192 ; reserve 8KB for input buffer
output resb 512  ; textual output

section .text
global _start
_start:
    ; goal: read input from stdout into a big buffer.
    mov rax, 0        ; read(2)
    mov rdi, 0        ; stdin fd
    mov rsi, buffer   ; ptr to buffer
    mov rdx, 17190    ; input size
    syscall
    ; move 16b of data from buffer to output
    movups xmm0, [rel buffer]
    movups [rel output], xmm0
    ; print the answer
    mov rdi, 1        ; stdout fd
    mov rdx, 16       ; len of string
    mov rsi, output   ; ptr to string
    mov rax, 1        ; write(2)
    syscall
    mov rax, 60       ; sys_exit
    xor rdi, rdi      ; exit code 0
    syscall
