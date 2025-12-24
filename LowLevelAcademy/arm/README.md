# arm

## ARM Assembly Development Environment
This guide provides instructions for setting up a development environment for ARMv7 (32-bit)

## Setup Instructions
Install the necessary tools: the ARM cross-compiler toolchain and the QEMU emulator.

### Arch Linux (AMD64)
```bash
sudo pacman -S arm-none-eabi-gcc arm-none-eabi-binutils qemu-user qemu-user-static gdb
```

Alternatively, for full ARM Linux toolchain (recommended for Linux binaries):
```bash
yay -S arm-linux-gnueabihf-gcc arm-linux-gnueabihf-binutils qemu-arch-extra
```
Or install from AUR manually:
```bash
git clone https://aur.archlinux.org/arm-linux-gnueabihf-gcc.git
cd arm-linux-gnueabihf-gcc
makepkg -si
```

### Debian/Ubuntu
```bash
sudo apt install gcc-arm-linux-gnueabihf qemu-user gdb-multiarch
```

**Note for Arch users:** Use `gdb` instead of `gdb-multiarch` in the debugging commands below.

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
# Debian/Ubuntu
gdb-multiarch ./hello

# Arch Linux
gdb ./hello
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




## Troubleshooting

### GPG Key Import Issues

If you encounter `gpg: keyserver receive failed: No data` during installation:

**1. Import the key manually from a different keyserver:**
```bash
gpg --keyserver keyserver.ubuntu.com --recv-keys ABAF11C65A2970B130ABE3C479BE3E4300411886
```

If that fails, try another keyserver:
```bash
gpg --keyserver keys.openpgp.org --recv-keys ABAF11C65A2970B130ABE3C479BE3E4300411886
```

Or:
```bash
gpg --keyserver pgp.mit.edu --recv-keys ABAF11C65A2970B130ABE3C479BE3E4300411886
```

After importing, retry the yay installation.

**2. If all keyservers fail, skip PGP check (less secure):**
```bash
yay -S arm-linux-gnueabihf-gcc arm-linux-gnueabihf-binutils qemu-arch-extra --skippgpcheck
```

**3. Or if building manually from AUR:**
```bash
cd arm-linux-gnueabihf-gcc
makepkg -si --skippgpcheck
```

The keyserver issues are often temporary, so the first approach (trying different keyservers) is preferable and usually works.

### Package Download Failures (404 errors)

If you encounter `failed retrieving file` or `404` errors during installation:

**1. Update your package database and system:**
```bash
sudo pacman -Syu
```

**2. If the issue persists, refresh your mirrorlist:**
```bash
# Update mirrors automatically (recommended)
sudo pacman -S reflector
sudo reflector --latest 20 --protocol https --sort rate --save /etc/pacman.d/mirrorlist
```

Or manually edit `/etc/pacman.d/mirrorlist` to prioritize different mirrors.

**3. Clear package cache and retry:**
```bash
yay -Sc
yay -S arm-linux-gnueabihf-gcc arm-linux-gnueabihf-binutils qemu-arch-extra
```

These errors typically occur when your local package database is out of sync with the mirrors or when specific mirrors are outdated.

### Build Failures (exit status 8)

If you encounter `Failed to install the following packages` with exit status 8 for `arm-linux-gnueabihf-glibc` or `arm-linux-gnueabihf-gcc`:

**1. Clean the build cache and rebuild:**
```bash
yay -Scc  # Clean all cache
rm -rf ~/.cache/yay/arm-linux-gnueabihf-*
```

**2. Install dependencies manually in order:**
```bash
yay -S arm-linux-gnueabihf-binutils
yay -S arm-linux-gnueabihf-linux-api-headers
yay -S arm-linux-gnueabihf-glibc (try option 2)
yay -S arm-linux-gnueabihf-gcc
yay -S qemu-arch-extra
```

**3. Alternative: Build from AUR with clean state:**
```bash
cd /tmp
git clone https://aur.archlinux.org/arm-linux-gnueabihf-binutils.git
cd arm-linux-gnueabihf-binutils && makepkg -si && cd ..

git clone https://aur.archlinux.org/arm-linux-gnueabihf-linux-api-headers.git
cd arm-linux-gnueabihf-linux-api-headers && makepkg -si && cd ..

git clone https://aur.archlinux.org/arm-linux-gnueabihf-glibc.git
cd arm-linux-gnueabihf-glibc && makepkg -si && cd ..

git clone https://aur.archlinux.org/arm-linux-gnueabihf-gcc.git
cd arm-linux-gnueabihf-gcc && makepkg -si && cd ..
```

Exit status 8 typically indicates dependency conflicts or incomplete builds. Installing packages in the correct order resolves most issues.

### Circular Dependency (gcc-stage2 and glibc)

If you encounter circular dependency errors where `arm-linux-gnueabihf-gcc-stage2` needs `arm-linux-gnueabihf-glibc-headers` and vice versa:

**1. Install glibc-headers first (breaks the cycle):**
```bash
yay -S arm-linux-gnueabihf-glibc-headers
yay -S arm-linux-gnueabihf-gcc
```

**2. Alternative: Use arm-none-eabi toolchain (simpler, for bare-metal only):**
```bash
sudo pacman -S arm-none-eabi-gcc arm-none-eabi-binutils qemu-user-static
```
Note: This toolchain is for bare-metal/nostdlib programs. For full Linux binaries with libc support, you need the arm-linux-gnueabihf toolchain.

**3. If all else fails, try using a prebuilt cross-compiler:**
Download ARM GCC from [ARM Developer site](https://developer.arm.com/downloads/-/gnu-a) and extract to a local directory, then add to PATH:
```bash
export PATH=$PATH:/path/to/arm-gnu-toolchain/bin
```