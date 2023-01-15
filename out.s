.intel_syntax
.global main
out_char:
    push %rbp
    mov %rbp, %rsp
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15
.out_char.L0:
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    mov %rsp, %rbp
    pop %rbp
    ret

println:
    push %rbp
    mov %rbp, %rsp
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15
.println.L0:
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    mov %rsp, %rbp
    pop %rbp
    ret

println_u:
    push %rbp
    mov %rbp, %rsp
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15
.println_u.L0:
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    mov %rsp, %rbp
    pop %rbp
    ret

println_i:
    push %rbp
    mov %rbp, %rsp
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15
.println_i.L0:
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    mov %rsp, %rbp
    pop %rbp
    ret

main:
    push %rbp
    mov %rbp, %rsp
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15
.main.L0:
    mov %r15, 10
    mov %r12, %r15
    jmp .main.L1

.main.L1:
    mov %r15, %r12
    cmp %r15, 0
    jne .main.L2
    jmp .main.L3

.main.L2:
    mov %r13, %r12
    mov %r14, 1
    mov %r15, %r13
    sub %r15, %r14
    mov %r12, %r15
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

