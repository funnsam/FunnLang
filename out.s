.intel_syntax
.global main
main:
    push %rbp
    mov %rbp, %rsp
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15
.main.L0:
    mov %r15, 1
    mov %r14, 0
    mov %r12, 10
    mov %r13, %r15
    mov %r15, %r14
    mov %r14, %r12
    jmp .main.L1

.main.L1:
    mov %r12, 1
    cmp %r12, 0
    jne .main.L2
    jmp .main.L3

.main.L2:
    mov %r11, %r15
    mov %r12, %r13
    mov %r8, %r11
    add %r8, %r12
    mov %r11, %r8
    mov %r12, %r15
    mov %r9, %r11
    sub %r9, %r12
    mov %r10, %r14
    mov %r11, 1
    mov %r12, %r10
    sub %r12, %r11
    mov %r13, %r8
    mov %r15, %r9
    mov %r14, %r12
    jmp .main.L1

.main.L3:
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    mov %rsp, %rbp
    pop %rbp
    ret

