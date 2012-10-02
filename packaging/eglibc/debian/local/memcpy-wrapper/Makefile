all: memcpy-preload.so memcpy-syslog-preload.so

clean:
	rm -f memcpy-preload.so memcpy-syslog-preload.so

memcpy-preload.so: memcpy-preload.c 
	$(CC) -D_GNU_SOURCE -DNOLOG -O2 -Wall -fPIC -shared -o memcpy-preload.so memcpy-preload.c

memcpy-syslog-preload.so: memcpy-preload.c 
	$(CC) -D_GNU_SOURCE -O2 -Wall -fPIC -shared -o memcpy-syslog-preload.so memcpy-preload.c

