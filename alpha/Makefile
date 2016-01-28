#
#

CC = cc
FC = f77

CFLAGS =	-I/usr/X11R6/include -L/usr/X11R6/lib -L/usr/lib -L/lib
# -I/usr/openwin/include -L/usr/openwin/lib -I/opt/SUNWSpro/include -L/opt/SUNWspro/lib -L/opt/SUNWspro/SC3.0.1/lib
OB =	main.o\
	springs.o\
	display.o

HDRS =	global.h\
	display.h

LIBS =	-lX11 -lm -lc -lfor -lots
#-lF77 -lM77

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












