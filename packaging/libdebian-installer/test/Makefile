CC = gcc
CFLAGS = -Wall -I/usr/include/glib-1.2 -I/usr/lib/glib/include
LDFLAGS = -ldebian-installer -lau-c-unit
test: test_system_packages.o test_hash.o main.o
	${CC} ${LDFLAGS} -o test $^

%.o:%.c
	$(CC) $(CFLAGS) -c $<
