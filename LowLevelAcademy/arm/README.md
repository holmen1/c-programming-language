# arm

Learning ARMv7 (32-bit) assembly programming with Linux syscalls


## Programs

### simple.s
The simplest runnable ARM program - exits with status code 0 using the `exit` syscall. Demonstrates the minimal structure needed for an executable.

### hello.s
Classic "Hello, World!" program that writes a message to stdout using the `write` syscall, then exits cleanly.

### loop.s
Calculates the sum of numbers 0 to 15 using a loop with conditional branching. Exits with the sum (120) as the exit code.

### toupper.s
Interactive program that reads one character from stdin, converts it to uppercase if it's a lowercase letter, and writes it back to stdout with a newline. Demonstrates stack allocation and function calls.

### holmshell

A minimal command shell implementation in ARM assembly that demonstrates process management through Linux syscalls. The shell reads user commands, constructs full paths by prepending `/usr/bin/`, forks a child process, and executes commands using `execve`. It uses `fork`, `wait4`, `read`, and `write` syscalls directly without any C library dependencies.

**Key features:**
- Interactive prompt with command input
- Process forking and execution
- Parent process waits for child completion
- Direct syscall interface (no libc)
- Simple exit handling

**Example usage:**
```bash
$ arm-none-eabi-gcc -o hsh holmshell.s -nostdlib -static
$ qemu-arm ./hsh
Welcome to holmshell!
Exit with 'q'
> ls
README.md  hello.s  holmshell.s  hsh  loop.s  simple.s  toupper.s
> whoami
holmen1
> ps
    PID TTY          TIME CMD
   1682 pts/0    00:00:00 bash
 548639 pts/0    00:00:00 qemu-arm
 548647 pts/0    00:00:00 ps
> q
$ echo $?
0
```



## ARM Assembly Development Environment
This guide provides instructions for setting up a development environment for ARMv7 (32-bit)

## Setup Instructions
Install the necessary tools: the ARM cross-compiler toolchain and the QEMU emulator.

### Arch Linux (AMD64)

**Bare-metal style toolchain (for assembly with Linux syscalls):**
```bash
sudo pacman -S arm-none-eabi-gcc arm-none-eabi-binutils qemu-user qemu-user-static gdb
```
This compiles assembly programs that make direct Linux syscalls (not truly bare-metal, but no C library).

## Workflow

### Compile
```bash
arm-none-eabi-gcc -g -o hello hello.s -nostdlib -static
```

Options:
- `-nostdlib`: Prevents linking against a standard C library
- `-static`: Creates a statically linked executable

**Alternative: Assembling and Linking Manually**
```bash
arm-none-eabi-as -o hello.o hello.s
arm-none-eabi-ld -o hello hello.o
```

### Run
```bash
qemu-arm ./hello
```

Trace system calls and signals
```bash
% qemu-arm -strace ./hello
548733 write(1,0x9024,14)Hello, World!
 = 14
548733 exit(0)
```

### Debug
Start GDB server:
```bash
qemu-arm -g 1234 ./hello
```

In a second terminal, connect with GDB:
```bash
gdb ./hello
```

Inside GDB:
```bash
(gdb) target remote localhost:1234
(gdb) layout regs or next
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
- `<Enter>`         Repeats previous command


## Further Resources

- [ARM32 Syscall Table](https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md#arm-32_bit_EABI)


## Alternative Setups

### Debian/Ubuntu
```bash
sudo apt install gcc-arm-linux-gnueabihf qemu-user gdb-multiarch
```

**Note:** Use `gdb-multiarch` on Debian/Ubuntu (Arch users use `gdb`).

### Full ARM Linux Toolchain (Arch - for C programs with libc)

For C programs that use standard library functions like `printf()`, `malloc()`, etc.:

```bash
yay -S arm-linux-gnueabihf-gcc arm-linux-gnueabihf-binutils qemu-arch-extra
```

Or install from AUR manually:
```bash
git clone https://aur.archlinux.org/arm-linux-gnueabihf-gcc.git
cd arm-linux-gnueabihf-gcc
makepkg -si
```

Compile with:
```bash
arm-linux-gnueabihf-gcc -g -o hello hello.s -nostdlib -static
# Or for two-step process:
arm-linux-gnueabihf-as -o hello.o hello.s
arm-linux-gnueabihf-ld -o hello hello.o
```


## Troubleshooting

### Installing Full Linux Toolchain (arm-linux-gnueabihf)

Common issues when installing the full ARM Linux toolchain from AUR:

**GPG Key Import Failures**

If you get `gpg: keyserver receive failed: No data`:

```bash
# Try different keyservers:
gpg --keyserver keyserver.ubuntu.com --recv-keys ABAF11C65A2970B130ABE3C479BE3E4300411886
# Or:
gpg --keyserver keys.openpgp.org --recv-keys ABAF11C65A2970B130ABE3C479BE3E4300411886
```

Then retry the installation. Alternatively, skip PGP verification (less secure):
```bash
yay -S arm-linux-gnueabihf-gcc arm-linux-gnueabihf-binutils qemu-arch-extra --skippgpcheck
```

**Package Download 404 Errors**

Update your system first:
```bash
sudo pacman -Syu
```

If issues persist, refresh mirrorlist:
```bash
sudo pacman -S reflector
sudo reflector --latest 20 --protocol https --sort rate --save /etc/pacman.d/mirrorlist
```

**Circular Dependency (gcc-stage2 ↔ glibc)**

Install in this order to break the cycle:
```bash
yay -S arm-linux-gnueabihf-binutils
yay -S arm-linux-gnueabihf-linux-api-headers
yay -S arm-linux-gnueabihf-glibc-headers  # Breaks the cycle
yay -S arm-linux-gnueabihf-gcc
```

Or build manually from AUR:
```bash
cd /tmp
for pkg in arm-linux-gnueabihf-{binutils,linux-api-headers,glibc,gcc}; do
  git clone https://aur.archlinux.org/$pkg.git
  cd $pkg && makepkg -si && cd ..
done
```