
triforth: triforth.S
	@as triforth.S -o out/triforth.o
	@ld out/triforth.o -o out/triforth
