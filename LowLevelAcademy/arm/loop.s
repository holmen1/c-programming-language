.global _start

.text
_start:
    mov r0, #0          // Initialize counter to 0
    mov r1, #0x10       // Load value 16 into r1 (loop limit)
    mov r2, #0          // Initialize sum to 0

loop:
    cmp r0, r1          // Compare counter with limit
    bge end             // If counter >= limit, exit loop
    add r2, r2, r0      // sum += counter
    add r0, r0, #1      // counter++
    b loop              // Repeat loop

end:
    // exit(sum) - Exit with sum as exit code
    mov r7, #1          // syscall: exit
    mov r0, r2          // exit code: sum (120)
    svc #0              // make syscall

// $ arm-none-eabi-gcc -o loop loop.s -nostdlib -static
// $ qemu-arm ./loop
// $ echo $?
// 120

