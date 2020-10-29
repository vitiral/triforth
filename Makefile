
triforth: triforth.S
	@as triforth.S --32 -o out/triforth.o
	@ld out/triforth.o -m elf_i386 -o out/triforth

sas: sas.S
	@as sas.S --32 -o out/sas.o
	@ld out/sas.o -m elf_i386 -o out/sas
