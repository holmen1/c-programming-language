# arm

## ARM Assembly Development Environment
This guide provides instructions for setting up a development environment for ARMv7 (32-bit)

## Setup Instructions
Install the necessary tools: the ARM cross-compiler toolchain and the QEMU emulator.

Debian
```bash
sudo apt install gcc-arm-linux-gnueabihf qemu-user gdb-multiarch
```

## Workflow

### Compile
```bash
arm-linux-gnueabihf-gcc -g -o hello hello.s -nostdlib -static
```
- -nostdlib: Prevents linking against a standard C library, as we are writing bare-metal code
- -static: Creates a statically linked executable

Alternative: Assembling and Linking Manually
```bash
arm-linux-gnueabihf-as -o hello.o hello.s
arm-linux-gnueabihf-ld -o hello hello.o
```
This two-step process gives you the same executable file as the gcc command but offers more control over the intermediate object files

### Run
```bash
qemu-arm ./hello
```
```bash
qemu-arm -strace ./hello
```

### Debug
For Linux binaries (user-mode)
```bash
qemu-arm -g 1234 ./hello
```
- -g 1234: Starts a GDB server on TCP port 1234

In a second terminal, start GDB and connect
```bash
gdb-multiarch ./hello
```

Inside GDB, connect to QEMU
```bash
(gdb) target remote localhost:1234
(gdb) layout regs
```

Common GDB Commands
- `b _start`        Set breakpoint at program start
- `b n`             Set breakpoint at line n
- `n`               Go to next instruction
- `s`               Go to next instruction, diving into function
- `c`               Run until next breakpoint
- `x/10i $pc`       Show next 10 instructions from program counter
- `x/4xb 0xaddress` Show 4 bytes in hexadecimal format
- `q`               Exit GDB
- `fs next`         Focus on next window


## Further Resources

- [ARM32 Syscall Table](https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md#arm-32_bit_EABI)


