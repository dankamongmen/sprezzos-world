.DELETE_ON_ERROR:
.PHONY: world all clean

world: growlight libbluray omphalos

all: world

.PHONY: growlight
growlight:
	git clone https://github.com/dankamongmen/growlight.git

.PHONY: libbluray
libbluray:
	git clone git://git.videolan.org/libbluray.git

.PHONY: omphalos
omphalos:
	wget http://dank.qemfd.net/pub/omphalos-0.0.99-pre.tar.bz2

clean:
