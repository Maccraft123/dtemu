DTC := dtc
CPP := cpp
DTSPATH := $(CURDIR)

default: all

dtbs: apple1.dtb \
	pet2001.dtb \
	nes.dtb \
	generic-cpm.dtb

all: dtbs

clean:
	@rm -f -- $(DTSPATH)/*.dts.p
	@rm -f -- $(DTSPATH)/*.dtb

%.dtb: $(CURDIR)/%.dts
	$(CPP) -E -x assembler-with-cpp -I$(DTSPATH)/../dt-bindings/c $< $<.p
	$(DTC) -I dts -O dtb $<.p > $(CURDIR)/$@
	@rm $<.p
