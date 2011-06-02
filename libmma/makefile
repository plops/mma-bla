CFLAGS=-ggdb3 -O0 -Wall -Wextra 
CC=gcc-4.3
IPMSLIB=/home/martin/mma-essentials-0209/
# note that the option -Wl,-rpath forces the linker to store the
# directory containing the libIPMS_SLM.so library. However, normally
# one installs the .so file into /usr/lib and doesn't have to bother
# with rpath.
LDFLAGS=-L$(IPMSLIB) -lIPMS_SLM -Wl,-rpath=$(IPMSLIB)
all: libmma.so mma reset burn burn-big
libmma.so: libmma.c
	gcc $(CFLAGS) -fPIC -c libmma.c && gcc -shared -Wl,-soname,libmma.so $(LDFLAGS) -o libmma.so libmma.o

burn-big: burn-big.c libmma.so
	gcc $(CFLAGS) $(LDFLAGS) $< -o $@


%: %.c libmma.so
	gcc $(CFLAGS) -L. -lmma -Wl,-rpath=./ $< -o $@


clean:
	rm mma libmma.so reset burn mma.c.* reset.c.* burn.c.* libmma.c.*