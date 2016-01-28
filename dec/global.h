/* Defines, definitions of physical constants, data structures, and external functions */

#ifndef _GLOBAL_H
#define _GLOBAL_H

#define REAL double
#define BOOL char

/* Physical data */
#define TWOTO31 0x7FFFFFFF
#define EPS 0.001
#define FROM_ATOMIC_UNIT_TO_KG 1.660565586E-27
#define FROM_METER_TO_ANGSTROM 1E10
#define FROM_ANGSTROM_TO_METER 1E-10
#define FROM_ANGSTROM_TO_METER 1E-10
#define K_BOLTZMANN 1.38066244E-23

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/* Fortran */
#define NMAX 100000

/* To store the particles */
typedef struct {
  int tag;			/* Tag */
  REAL x, y;			/* Position */
  REAL previousX, previousY;	/* Previous position */
  REAL attachX, attachY;	/* Attachment position */
  REAL forceX, forceY;		/* Force acting on particle */
  REAL momentX, momentY;	/* Linear momentum */
} Particle;

typedef struct 
{
  int  nlayer1;
  REAL dx1;
  int  nlayer2;
  REAL dx2;
  REAL dx12;

  REAL mass1;
  REAL mass2;
  REAL rcut;
  REAL vfr;
  REAL temp;
  int  iseed;
  REAL q;
  int  nvf;
  REAL *vfarr;
  int  nt1;
  REAL *t1arr;  
  int  nt2;
  REAL *t2arr;

  REAL tstop;
  REAL nstop;
  REAL dt;
  REAL djitt;
  
  int  nut;
  REAL *utarr;

  REAL fx;
  REAL fy;

  REAL alale,alals;
  int  mal,nal;
  REAL alfee,alfes;
  int  malfe,nalfe;
  REAL fefee,fefes;
  int  mfe,nfe;

  REAL gammaN;
  REAL gammaS;
  REAL my1;
  REAL my12;
  REAL r1;
  REAL r2;
  REAL r0;

} Parameter;

#endif
















































