CC = cc 
CF = f77

CFLAGS =	-Aa -I/usr/include/X11R4 -L/usr/lib -L/usr/lib/X11R4 -L/lib
FFLAGS =
OB =	main.o\
	springs.o\
	display.o

HDRS =	global.h\
	display.h

LIBS =	-lX11 -lm -lc -lf

springs:	$(OB) $(HDRS) 
	$(CC) $(CFLAGS) -o springs $(OB)  $(LIBS)

main.o:	main.c
	$(CC) $(CFLAGS) -c main.c

display.o:	display.c
	$(CC) $(CFLAGS) -c display.c

springs.o:	springs.f
	$(FC) -c  springs.f

clean:
	rm $(OB) fsprings

nogfx:
	$(FC) $(FFLAGS) springs.f -o fsprings
 












