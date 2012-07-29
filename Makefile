.DELETE_ON_ERROR:
.PHONY: world all clean

world: libbluray omphalos

all: world

.PHONY: libbluray
libbluray:
	git clone git clone git://git.videolan.org/libbluray.git

.PHONY: omphalos
omphalos:
	wget http://dank.qemfd.net/pub/growlight-0.0.99-pre.tar.bz2

clean:
