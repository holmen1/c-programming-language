.global _start

.text
_start:
    mov r7, #1      // syscall: exit
    mov r0, #0      // exit code: 0
    svc #0          // make syscall


// arm-none-eabi-gcc -g -o simple simple.s -nostdlib -static
// qemu-arm ./simple
// echo $?  # Shows exit code