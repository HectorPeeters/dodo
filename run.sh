cargo r $1 &&
nasm -f elf64 output.asm -o /tmp/output.o -g &&
gcc -o /tmp/output /tmp/output.o -nostartfiles -no-pie -g &&
/tmp/output
