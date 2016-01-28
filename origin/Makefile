#
#

CC	= cc  
CPCP	= CC  
FC	= f77 

CFLAGS	= -L/usr/lib -fullwarn -O3 -mips4 

FFLAGS	= -vms_cc -i4 -static -O3 -mips4 -mp

OB =	main.o\
	display.o\
	springs.o

HDRS =	global.h\
	display.h

LIBS =	-lX11 -lfastm -mp


springs:	$(OB) $(HDRS)
	$(FC) -mp -o springs $(OB)  $(LIBS)

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












