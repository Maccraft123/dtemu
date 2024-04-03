AS8085 := as8085
AS8085FLAGS = -o
ASLINK := aslink
ASLINKFLAGS = -n -i

all: bios.rom

%.rom: %.ihx
	hex2bin $<

%.ihx: %.rel
	$(ASLINK) $(ASLINKFLAGS) $<

%.rel: %.asm
	$(AS8085) $(AS8085FLAGS) $<
