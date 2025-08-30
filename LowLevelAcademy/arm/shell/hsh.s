.global _start

.text
_start:
    bl print_prompt
    b exit_ok


print_prompt:
    push {lr}
    ldr r0, =my_prompt
    ldr r1, =prompt_len
    bl printf
    pop {pc}

printf:
    push {lr}
    mov r2, r1  // count: length of prompt
    mov r1, r0  // buffer: prompt

    mov r7, #4  // syscall: write
    mov r0, #1  // fd: stdout
    svc #0
    pop {pc}

exit_ok:
    mov r7, #1
    mov r0, #0
    svc #0

.data
my_prompt:
    .asciz "> "
prompt_len = . - my_prompt
