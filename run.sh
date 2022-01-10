cargo r &&
nasm -f elf64 output.asm -o output.o -g &&
gcc -o output output.o -nostartfiles -no-pie -g &&
./output
