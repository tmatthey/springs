# DEC
# OSF1 V4.0 564 alpha

CC = cc
FC = f77

CFLAGS =	-I/usr/include/X11 -L/usr/lib/X11

OB =	main.o\
	springs.o\
	display.o

HDRS =	global.h\
	display.h

LIBS =	-lX11 -lm -lc -lfor -lots

springs:	$(OB) $(HDRS) 
	$(CC) $(CFLAGS) -o springs $(OB)  $(LIBS)

main.o:	main.c
	$(CC) $(CFLAGS) -c main.c

display.o:	display.c
	$(CC) $(CFLAGS) -c display.c

springs.o:	springs.f
	$(FC) -c -0 springs.f

clean:
	rm $(OB) fsprings


nogfx:
	$(FC) springs.f -o fsprings

all:
	make clean; make springs; make nogfx
