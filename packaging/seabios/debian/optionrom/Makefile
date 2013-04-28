all: build-all

CFLAGS := -Wall -Wstrict-prototypes -Werror -fomit-frame-pointer -fno-builtin -fno-stack-protector
CFLAGS += -I$(SRC_PATH)
LD=ld
OBJCOPY=objcopy

build-all: $(subst .S,.bin,$(wildcard *.S))

%.img: %.o
	$(LD) -Ttext 0 -e _start -s -o $@ $<

%.raw: %.img
	$(OBJCOPY) -O binary -j .text $< $@

%.bin: %.raw
	python ./signrom.py $< $@

clean:
	rm -f *.o *.d *.raw *.img *.bin *~

.PHONY : all clean build-all
