# arm

## ARM Assembly Development Environment
This guide provides instructions for setting up a development environment for ARMv7 (32-bit) assembly on both Linux and macOS

## Setup Instructions
Install the necessary tools: the ARM cross-compiler toolchain and the QEMU emulator.

Linux
```bash
yay -Sy arm-none-eabi-gcc arm-none-eabi-gdb qemu-full
```

macOS
```bash
brew install arm-none-eabi-gcc arm-none-eabi-gdb qemu
```

## Workflow

### Compile
```bash
arm-none-eabi-gcc -g -mcpu=generic-armv7-a -o hello hello.s -nostdlib -static
```
- -g: Includes debugging symbols, which is essential for GDB
- -mcpu=generic-armv7-a: Specifies the target ARM architecture
- -nostdlib: Prevents linking against a standard C library, as we are writing bare-metal code
- -static: Creates a statically linked executable

Alternative: Assembling and Linking Manually
```bash
arm-none-eabi-as -g -mcpu=generic-armv7-a -o hello.o hello.s
arm-none-eabi-ld -o hello hello.o
```
This two-step process gives you the same executable file as the gcc command but offers more control over the intermediate object files

### Run
```bash
qemu-system-arm -M versatilepb -m 128M -nographic -kernel ./hello -semihosting
```
- -M versatilepb: Emulates a specific ARM-based board (Versatile Platform Baseboard)
- -nographic: Disables video output and redirects I/O to the console
- -kernel ./hello: Specifies the program to run as the kernel

### Debug
```bash
qemu-system-arm -M versatilepb -m 128M -nographic -kernel ./hello -gdb tcp::1234 -S -semihosting
```
- -gdb tcp::1234: Starts a GDB server on TCP port 1234
- -S: Freezes the CPU at startup, waiting for a debugger to connect before executing any code

In a second terminal, start GDB and connect
```bash
arm-none-eabi-gdb hello
```

Inside GDB, connect to QEMU
```bash
(gdb) target remote localhost:1234
(gdb) layout regs
```

Common GDB Commands
- `b _start`   Set breakpoint at program start
- `b n`        Set breakpoint at line n
- `n`          Go to next instruction
- `s`          Go to next instruction, diving into function
- `c`          Run until next breakpoint
- `x/10i $pc`  Show next 10 instructions from program counter
- `q`          Exit GDB
- `fs next`    Focus on next window


## Lessons Learned: The Challenge of Syscalls and I/O

A common point of confusion when starting with `qemu-system-arm` is how to perform basic input/output, like printing text to the console.

### The Problem: "Syscalls Don't Work"

If you write ARM assembly with standard Linux syscalls (e.g., using `svc #0` to make a system call), you will find that it works with `qemu-arm` but fails with `qemu-system-arm`.

- **`qemu-arm`** is a *user-mode emulator*. It runs a Linux binary on a host Linux kernel, so it understands Linux syscalls.
- **`qemu-system-arm`** is a *full system emulator*. It emulates a bare-metal hardware board (like `versatilepb`). It has no operating system and no knowledge of Linux syscalls.

### The Solution: Semihosting

The correct way to perform I/O in a bare-metal environment like `qemu-system-arm` is **semihosting**. This is a mechanism where the emulated ARM processor traps a special instruction and passes the I/O request to the host computer running QEMU.

To make it work, two things are crucial:

**1. Use Semihosting Instructions in Your Code:**
You must use the specific semihosting conventions. For printing a string, the operation is `SYS_WRITE0` (code `0x04`). The call is triggered by `svc 0x123456`.

```armasm
.global _start

_start:
    // Semihosting call to write the string "Hello, World!\n"
    mov r0, #0x04       // SYS_WRITE0 operation
    ldr r1, =msg        // Pointer to the string
    svc 0x123456        // Semihosting trap

    // Semihosting call to exit QEMU
    mov r0, #0x18       // AngelSWI_ReportException (exit)
    ldr r1, =0x20026    // ADP_Stopped_ApplicationExit
    svc 0x123456        // Semihosting trap

.data
msg:
    .asciz "Hello, World!\n"
```

**2. Enable Semihosting in QEMU and Compile Correctly:**
Your build and run commands must be configured to support this.

- **Compiler:** Specifying the CPU with `-mcpu=generic-armv7-a` ensures the assembler generates compatible instructions.
- **QEMU:** You must explicitly enable the semihosting feature with the `-semihosting` flag.

```bash
# Compile
arm-none-eabi-gcc -g -mcpu=generic-armv7-a -o hello hello.s -nostdlib -static

# Run
qemu-system-arm -M versatilepb -m 128M -nographic -kernel ./hello -semihosting
```

Without both the correct assembly code and the correct flags, semihosting will fail, and you won't see any output. This was a key discovery in setting up a reliable development environment.


## Further Resources

- [Semihosting operations](https://developer.arm.com/documentation/dui0471/i/semihosting/semihosting-operations)

- [GNU ARM Embedded Toolchain Documentation](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain)

