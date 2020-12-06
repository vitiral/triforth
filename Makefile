
triforth: triforth.S
	@as triforth.S --32 -o out/triforth.o
	@ld out/triforth.o -m elf_i386 -o out/triforth

tfc: triforth.c
	@$(CC) -m32  triforth.c -o out/triforth -Wall -Wextra -std=c99
