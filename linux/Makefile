#
#

CC	= cc  
CPCP	= CC  
FC	= gfortran 

CFLAGS	= -I/usr/X11R6/include -I/usr/X11R6/include/X11

FFLAGS	= -O3

OB =	main.o\
	display.o\
	springs.o

HDRS =	global.h\
	display.h

LIBS =	 -L/usr/X11R6/lib -L/usr/X11R6/lib/X11 -lX11


springs:	$(OB) $(HDRS)
	$(FC) -o springs $(OB)  $(LIBS)

main.o:	main.c
	$(CC) $(CFLAGS) -c main.c

display.o:	display.c
	$(CC) $(CFLAGS) -c display.c

springs.o:	springs.f
	$(FC) $(FFLAGS) -c springs.f

clean:
	rm -f $(OB) fsprings springs

nogfx:
	$(FC) springs.f -o fsprings

all:
	make clean; make springs; make nogfx












