.DELETE_ON_ERROR:
.PHONY: world all clean

world: omphalos

all: world

.PHONY: omphalos
omphalos:
	wget http://dank.qemfd.net/pub/growlight-0.0.99-pre.tar.bz2

clean:
