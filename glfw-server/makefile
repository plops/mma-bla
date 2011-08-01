CFLAGS=-Wall -g -Wextra
LDFLAGS=-lglfw -lm
all: glfw 
queue.o: queue.c
test-queue: queue.c
	gcc queue.c -I. -DTEST -o test-queue
glfw: glfw.c 

