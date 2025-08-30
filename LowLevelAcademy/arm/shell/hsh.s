.global _start

.text
_start:
    bl print_prompt
    bl read_cmd
    bl print_result
    b exit_ok


print_prompt:
    push {lr}
    ldr r0, =my_prompt
    ldr r1, =prompt_len
    bl printf
    pop {pc}

read_cmd:
    push {lr}
    mov r7, #3      // syscall: read
    mov r0, #0      // fd: stdin
    ldr r1, =input_buf 
    mov r2, #buffer_len     // count
    svc #0

    ldr r0, =input_buf 
    pop {pc}

print_result:               // prints first 2 chars + newline
    push {lr}
    mov r1, r0              // buffer
    mov r0, #'\n'           // load newline character
    strb r0, [r1, #2]       // store newline at [buffer+3]

    mov r7, #4              // syscall: write
    mov r0, #1              // fd: stdout
    mov r2, #3              // count: 3 bytes
    svc #0
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

.equ buffer_len, 16    // 15 chars + null terminator

.bss
    .align 2
input_buf:
    .skip buffer_len   // Reserve 16 bytes for input_buf, all initialized to zero
