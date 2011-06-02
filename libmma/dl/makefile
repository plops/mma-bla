CFLAGS=-ggdb3 -O0 -Wall -Wextra 
CC=gcc-4.3
# note that the option -Wl,-rpath forces the linker to store the
# directory containing the libIPMS_SLM.so library. However, normally
# one installs the .so file into /usr/lib and doesn't have to bother
# with rpath.
all: libmma.so mma
libmma.so: libmma.c SLMFunctions.o
	$(CC) $(CFLAGS) -fPIC -c libmma.c && gcc SLMFunctions.o -shared -Wl,-soname,libmma.so -ldl -o libmma.so libmma.o

%: %.c libmma.so
	$(CC) $(CFLAGS) -L. -lmma -Wl,-rpath=./ $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -fPIC $< -c -o $@ 

clean:
	rm mma libmma.so SLMFunctions.o