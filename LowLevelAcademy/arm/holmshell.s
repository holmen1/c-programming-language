.global _start

.text
_start:
    ldr r0, =my_welcome_msg
    ldr r1, =welcome_msg_len
    bl printf
main_loop:
    bl print_prompt
    bl read_cmd
    bl check_exit
    bl create_cmd
    bl exec_cmd
    b main_loop

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

create_cmd:
    push {lr}
    ldr r1, =buffer         // destination buffer for full path
    ldr r2, =my_path        // source: "/usr/bin/"
    ldr r3, =path_len       // length of "/usr/bin/"
    mov r4, #0
copy_path:
    cmp r4, r3
    beq append_cmd
    ldrb r5, [r2, r4]
    strb r5, [r1, r4]
    add r4, r4, #1
    b copy_path
append_cmd:
    add r1, r1, r3          // r1 = buffer + path_len (append here)
    mov r2, r0              // r2 = original input buffer pointer
copy_cmd:
    ldrb r5, [r2], #1
    cmp r5, #10             // newline?
    beq null_term
    strb r5, [r1], #1
    b copy_cmd
null_term:
    mov r5, #0
    strb r5, [r1]           // null-terminate
    ldr r0, =buffer
    pop {pc}

exec_cmd:
    push {lr}
    bl fork
    cmp r0, #0
    beq child_process
    bl wait_for_child
child_process:
    push {lr}
    mov r7, #11         // sys_execve
    ldr r0, =buffer
    ldr r1, =argv
    mov r2, #0          // envp
    svc #0
    // If execve fails, r0 will be negative
    cmp r0, #0
    blt exit_error  // Branch to error exit if failed
    pop {pc}        // This won't run if execve succeeds
wait_for_child:
    mov r7, #0x72        // sys_wait4
    mov r0, #-1          // Wait for any child
    mov r1, #0           // No options
    mov r2, #0           // No status
    svc #0
    pop {pc}

fork:
    push {lr}
    mov r7, #2              // sys_fork
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

exit_error:
    mov r7, #1      // sys_exit
    mov r0, #1      // Exit code 1 (error)
    svc #0

check_exit:
    push {lr}
    ldrb r2, [r0]        // first char
    cmp r2, #'q'
    bne return
    ldrb r2, [r0, #1]
    cmp r2, #10          // newline (ASCII 10)
    bne return
    b exit_ok            // input is "q\n", so exit shell

return:
    pop {pc}

.data
my_prompt:
    .asciz "> "
prompt_len = . - my_prompt
// my_prompt    address of the prompt string
// .            is the location counter (current address/position in the program)
// . - my_prompt calculates the length of data by subtracting startaddress from current address

my_welcome_msg:
    .asciz "Welcome to holmshell!\nExit with 'q'\n"
welcome_msg_len = . - my_welcome_msg

my_path:
    .ascii "/usr/bin/"
path_len = . - my_path  //

argv:
    .word buffer  // Pointer to the command path
    .word 0       // NULL terminator

.equ buffer_len, 16    // 15 chars + null terminator

// uninitialized data
.bss
    .align 2            // Align to 4-byte boundary (2^2 = 4)
input_buf:
    .skip buffer_len    // Reserve 16 bytes (uninitialized, zero-filled by OS at runtime)
buffer:
    .skip buffer_len    // Reserve 16 bytes (uninitialized, zero-filled by OS at runtime)



@ $ ./hsh 
@ Welcome to holmshell!
@ Exit with 'q'
@ > ls
@ hsh  hsh.s
@ > ps
@     PID TTY          TIME CMD
@   15297 pts/4    00:00:00 bash
@   28795 pts/4    00:00:00 hsh
@   28832 pts/4    00:00:00 ps
@ > whoami
@ holmen1
@ > q
@ $ echo $?
@ 0

@ $ qemu-arm -strace ./hsh 
@ 29036 write(1,0x20247,37)Welcome to holmshell!
@ Exit with 'q'
@  = 37
@ 29036 write(1,0x20244,3)>  = 3
@ 29036 read(0,0x20280,16)ls
@  = 3
@ 29036 fork() = 29071
@ 29036 wait4(-1,(nil),0,0x9) = 0
@ 29071 execve("/usr/bin/ls",{"/usr/bin/ls",NULL})hsh  hsh.s
@  = -1 errno=14 (Bad address)
@ 29036 write(1,0x20244,3)>  = 3
@ 29036 read(0,0x20280,16)
