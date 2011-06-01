CFLAGS=-g -O2 -Wall -Wextra
IPMSLIB=/home/martin/mma-essentials-0209/
# note that the option -Wl,-rpath forces the linker to store the
# directory containing the libIPMS_SLM.so library. However, normally
# one installs the .so file into /usr/lib and doesn't have to bother
# with rpath.
LDFLAGS=-L$(IPMSLIB) -lIPMS_SLM -Wl,-rpath=$(IPMSLIB)
all: libmma.so mma reset
reset: reset.c libmma.so
	gcc -L. -lmma -Wl,-rpath=./ reset.c -o reset
mma: mma.c libmma.so
	gcc -L. -lmma -Wl,-rpath=./ mma.c -o mma
libmma.so: libmma.c
	gcc -fPIC -c libmma.c && gcc -shared -Wl,-soname,libmma.so $(LDFLAGS) -o libmma.so libmma.o
clean:
	rm mma libmma.so reset