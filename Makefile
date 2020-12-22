triforth: triforth.S
	@as triforth.S --32 -o out/triforth.o
	@ld out/triforth.o -m elf_i386 -o out/triforth

twoforth: twoforth.S
	@as twoforth.S --32 -o out/twoforth.o
	@ld out/twoforth.o -m elf_i386 -o out/twoforth
