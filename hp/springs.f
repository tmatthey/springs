c-------------------------------------------------------------:
c---- Fortran main
c-------------------------------------------------------------:

	program springs
 
c This a simple main to do the calculation without any gfx 

	parameter (nmax=100000)
	integer nstop,n,i
	double precision t,xi,xf,yi,yf
      	double precision rx(nmax),ry(nmax)
      	double precision th(nmax)
      	double precision tag(nmax)
        integer col(nmax)

	character*80 infile
	nstop = 0
	i = 0
	
        write(*,*)'Inputfile ?'
        read(*,"(A)")infile 	

1	call fmain(nstop,n,t,xi,xf,yi,yf,rx,ry,th,tag,col,infile)
	i = i+1

	write(*,*)'Step:',i

	if (i .le. nstop) goto 1
	
	write(*,*)'Done.'
	close(10)
	stop
	end


c-------------------------------------------------------------:
c---- main call
c-------------------------------------------------------------:

      subroutine fmain(nstop,n,t1,xi,xf,yi,yf,rx,ry,th,tag,col,infil)

c
c This subroutine  propagates newtons equations in time from t1 to t2.
c
c First time the routine is called all data are initiated.
c 
c At certain  timesteps the total phase space is dumped. 
c plots initial/final configuration of spheres moving on a 
c Phase space is a collection of ns1 spherical grains moving
c on a bed of 'ns2' surface particles connected by springs.
c 
c ______________________________________________________________________
c Units:
c        SI
c
c Present version: 
c  BED: V_i,j = (1/2) k (x_i - x_j)**2
c  Moving Spheres: Equations from Torstein Poshel et al
c
c JPH/97/
c _______________________________________________________________________
      external bforce
      parameter (nmax=100000,nbmax=4,ngmax=16,nmaxut=100)

      double precision t1,t2,tstop,tstep
      integer nstop	
      character*80 infil
      double precision xi,xf,yi,yf
      integer nut,n,i,jj
      double precision tut(nmaxut),dt,tut1
      logical first
      data first/.true./

      double precision rx(nmax),ry(nmax)
      double precision vx(nmax),vy(nmax)
      double precision th(nmax), w(nmax)
      double precision tag(nmax)
      integer col(nmax)

c common:
c Nabour map (for bed particles)
      integer nbours(nmax,nbmax),nbnr(nmax)
      common /nbhood/nbours,nbnr

c Grid
c
c ngrid      : boxs with the particles inside
c ngridnr    : number of particle inside a box
c ngridpath  : neighbour-boxs
c ngridindex : box-index for the particle
c ngridw     : number of boxs horizontal
c ngridh     : number of boxs vertical
c ngridax, ngridbx, ngriday,ngridby
c            : transformation-coeff. for getgridindex()  
c
      integer ngrid(nmax*2,ngmax),ngridnr(nmax*2),ngridpath(nmax*2,9)
      integer ngridindex(nmax*2),ngridw,ngridh
      double precision gridax,gridbx,griday,gridby
      common /grid/gridax,gridbx,griday,gridby,ngrid,ngridnr,
     $     ngridpath,ngridindex,ngridw,ngridh

c Simulation boxes of each layer (used for periodic boundary cond.)
      double precision L1,H1,L2,H2,d12
      common /box/L1,H1,L2,H2

      double precision vfr,tmp, Q
      integer nvf,vfarr(1000), nt1,t1arr(1000),nt2,t2arr(1000)
      common /dynam/ Q, vfr,tmp, nvf,vfarr,nt1,t1arr,nt2,t2arr

c energie:
      double precision frik,load,epot,ekin1,ekin2,erot,mrx,mry,dL
      common /energie/ frik,load,epot,ekin1,ekin2,erot,mrx,mry,dL

      double precision mass1,mass2,rcut,dx1,dx2
      integer nlay1,ns1,nlay2,ns2
      common /lattice/ mass1,mass2,rcut,nlay1,ns1,dx1,nlay2,ns2,dx2

      if (first) then
        open(29,file=infil,STATUS= 'UNKNOWN')
        call init(tstop,nstop,dt,nut,tut,rx,ry,th,vx,vy,w,n,d12,
     $     tag,col)
        close(29)
        xi = -L2/2
        xf =  L2/2
        yi =  -(H1/2+H2+d12)
        yf =  H1/2

        t2=0
        t1=0
333	tstep=tstop/nstop
	if (tstep .le. dt ) then
	   write(*,*)'tstep is less than dt - I decrease nstop'
	   nstop=nstop/2
	   goto 333
	endif
       jj=1
        i= 0
      endif

      t1= tstep * i
      i= i+1
      t2= tstep * i
      
c      write(*,*)'dL, Load:',dL,load/nlay1
c      write(*,*)w(nlay1*2+1),w(nlay1+1),w(nlay1*nlay1)
c      write(*,*)w(1),w(2)
c      write(*,*)sqrt(vx(1)**2+vy(1)**2),sqrt(vx(2)**2+vy(2)**2)
c      write(*,*)w(ns1),w(ns1/nlay1+1)	

c Dump atoms:
      if ((jj .le. nut).and.(t2 .ge. tut(jj)))then
         if (jj .gt. 1) then
	   tut1 = tut(jj)-tut(jj-1)
         else 
           tut1 =0
         endif
         jj = jj+1
         call dump(rx,ry,vx,vy,w,n,t2,tut1)
      endif
      call verlet_vel(bforce,rx,ry,th,vx,vy,w,n,t1,t2,dt,first)

      first=.false.
      return
      end


c-------------------------------------------------------------:
c---- Init
c-------------------------------------------------------------:

      subroutine init(tstop,nstop,dt,nut,tut,rx,ry,th,vx,vy,w,n,
     $     d12,tag,col) 

c
c Initiate 2 cubic slabs of Al atoms and a rectangular
c layer of Fe atoms.                
c
c The slabs may be realxed/and rotated (to study forces
c between various crystal planes) before the surface slab is translated
c to give a required distance between the planes!
c This may be done by a call to the not yet written routines
c relaxal and relaxfe
c
c PROGRAM Units:
c 1 AMU        = 1.6605655e-27kg
c 1 eV         = 1.60219e-19J
c 1 AAngstroem = 1.0e-10m
c
c 1 TIME UNIT      = 10e-7 [s]
c 1 LENGTH UNIT    = 10e-7 [m]
c 1 MASS UNIT      = 4/3 * Pi * Length**3 * RHO-Al = 11.3e-18 [Kg]
c 1 VELOCITY UNIT  = Length/Time = 1/1 [m/s]
c 1 ENERGY UNIT    = 1/2 * Mass * Velocity**2 = 5.65e-18 [J]
c 1 FORCE UNIT     = Energy/Length = 5.65e-11 [N]
c 1 PRESSURE UNIT  = Energy/V = 5.65e+3 [Pa]
c
c Input Variables: 
c nlay1     : number of layers  = number of atom in a layer (2D, cubic slab)
c nlay2     : number of  layers in 2 (surface layer)
c dx1       : distance between each atom in slab 1 
c dx2       : distance between each atom in slab 2 (surace slab)
c ns1(2)    : total number of atoms in first, second layer
c d12       : initial distance between bottom layer in phase 1 and top layer
c           : in phase 2.
c mass1(2)  : atomic masses of atoms in 1(2) layer
c rcut      : max distance for force calculation between to atoms 
c vfr       : fixed velocity of given layers
c nvf,vfarr : number of layers, array of which layers which are explicitely
c           : moving (or frozen) with velocity vfr.
c nt1,t1arr : number of layers, and array of numbers indicating which arrays
c           : in upper slab which are subject to temperature control 
c           : (Langv. dynamics)
c nt2,t2arr : number of layers, and array of numbers indicating which arrays
c           : in surface slab  which are subject to temperature control 
c           : (Langv. dynamics)
c           : tstop simulation length
c nstop     : Number of times to stop verlet dyn. and update neibbour list
c dt        : numerical time step
c djitt     : time interval between writing out energy, friction etc
c           : from verlet routine
c

      parameter (rhoal=2.7e+3,rhofe=7.9e+3)

      integer getgridindex
      parameter (nmax=100000,nbmax=4,ngmax=16,nmaxut=100)
      double precision vvx,vvy,rk2,rk1,pi,x,y,d12
      integer iseed,nlay3,i,ii,j,jj,k,kk,l,ll,m,mm
      logical same
      integer nut,nstop,ihi
      double precision tut(nmaxut)
      double precision rx(nmax),ry(nmax)
      double precision vx(nmax),vy(nmax)
      double precision th(nmax), w(nmax)
      double precision rx1(nmax),ry1(nmax)
      double precision rx2(nmax),ry2(nmax)
      double precision tstop,dt,massi
      double precision tag(nmax)
      integer col(nmax)
c common:
      real djitt
      common /timut/djitt

      character*80 outf,temp
      common /fil/outf

ction/sphere parameters:
c gamman: Normal friction koefficient
c gammas: Shear friction koefficient
c my     Kinetic friction coefficient
c my12               "               between particles and bed
c r1    :Radius of active particles
c r2    :Radius of bed particles
c r0    :Equilibrium distance between bed particls (elastic spring)
      double precision gamman,gammas, my, my12, r1, r2, r0
      common /frikpar/  gamman,gammas, my, my12, r1, r2, r0

c nabourmap:
      integer nbours(nmax,nbmax),nbnr(nmax)
      common /nbhood/nbours,nbnr
c Grid
      integer ngrid(nmax*2,ngmax),ngridnr(nmax*2),ngridpath(nmax*2,9)
      integer ngridindex(nmax*2),ngridw,ngridh
      double precision gridax,gridbx,griday,gridby
      common /grid/gridax,gridbx,griday,gridby,ngrid,ngridnr,
     $     ngridpath,ngridindex,ngridw,ngridh

c Units (good to ahve stored)
      double precision lenu,massu,eu,kbolz
      common /units/lenu,massu,eu,kbolz

c Simulation boxes of each layer (used for periodic boundary cond.)
      double precision L1,H1,L2,H2
      common /box/L1,H1,L2,H2
 
c external permanent forces + Lennard Jones interaction parameters
c between al-al, al-fe, fe-fe
      double precision fx,fy
      integer mal,nal,malfe,nalfe,mfe,nfe
      double precision alale,alals,alfee,alfes,fefee,fefes
      common /force/fx,fy,alale,alals,alfee,alfes,fefee,fefes,
     $              mal,nal,malfe,nalfe,mfe,nfe

      double precision mass1,mass2,rcut,dx1,dx2
      integer nlay1,ns1,nlay2,ns2
      common /lattice/ mass1,mass2,rcut,nlay1,ns1,dx1,nlay2,ns2,dx2
      
      double precision vfr,tmp, Q
      integer nvf,vfarr(1000), nt1,t1arr(1000),nt2,t2arr(1000)
      common /dynam/ Q, vfr,tmp, nvf,vfarr,nt1,t1arr,nt2,t2arr

      lenu=1.0e-10
      massu=1.66057e-27
      eu=1.60219e-19
      kbolz=1.38066e-23/eu
      kbolz=1.38066e-23/5.65e-18             
      pi = acos(-1.0)

      read(29,'(a)')outf
      open(10,file=outf,STATUS = 'UNKNOWN')
      do i = 1,80
        if (outf(i:i) .eq. ' ') goto 100
      enddo
 100  ihi=i-1
      temp = outf(1:ihi)//'tRNbot'                              
      open(42,file=temp,STATUS= 'UNKNOWN')
      temp = outf(1:ihi)//'tV'                              
      open(43,file=temp,STATUS= 'UNKNOWN')
      temp = outf(1:ihi)//'tRtop'                              
      open(44,file=temp,STATUS= 'UNKNOWN')

      read(29,*)tstop,nstop,dt,djitt
      read(29,*)nut, (tut(i),i=1,nut)
      read(29,*)nlay1,dx1,nlay2,dx2,d12
      read(29,*)mass1,mass2,rcut
      read(29,*)vfr,tmp,iseed,Q                 
      read(29,*)nvf, (vfarr(i), i = 1,nvf)
      read(29,*)nt1, (t1arr(i), i = 1,nt1)
      read(29,*)nt2, (t2arr(i), i = 1,nt2)
      read(29,*) fx,fy
      read(29,*) alale,alals,mal,nal
      read(29,*) alfee,alfes,malfe,nalfe
      read(29,*) fefee,fefes,mfe,nfe
      read(29,*) gamman,gammas, my, my12, r1, r2, r0

c Calculate the mass's if <= 0:
      if (mass1 .le. 0) then
        mass1 = r1**3
        write(*,*)'Default: mass1 =',mass1
      endif
      if(mass2 .le. 0) then
        mass2 = r2**3*rhofe/rhoal
        write(*,*)'Default: mass2 =',mass2
      endif
      
c consistency test:
      if (nvf .gt. nlay2) then
        write(*,*)'nvf, nlay2=',nvf,nlay2  
        write(*,*)' I stop '
        stop  
      endif
      if (nt1 .gt. nlay1) then
        write(*,*)'nt1, nlay1=',nt1,nlay1  
        write(*,*)' I stop '
        stop 
      endif
      if (nt2 .gt. nlay2) then
        write(*,*)'nt2, nlay2=',nt2,nlay2  
        write(*,*)' I stop '
        stop 
      endif
      
      L1 = nlay1*dx1
c      L1 = 11*dx1  
      ns1=nlay1*int(L1/dx1+0.5)

      H1 = dx1*nlay1
c      call fastslab(nlay1,dx1,rx1,ry1,ns1)
      H1 = H1 * sqrt(0.75)
      call fastslabcompact(nlay1,dx1,rx1,ry1,ns1)

c STandard (but slow cubic setup)...
c      call creslab(nlay1,dx1,rx1,ry1,ns1)
      write(*,*)'Al-slab created.'
c      call sort(rx1,ry1,ns1)
c      write(*,*)'Al-slab sorted.'

Cc
Cc translate each atom a distance +/- dx1/4 (bcc)       
Cc
C      do i = 1,nlay1
C        do j = 1,nlay1
C          jj = (i-1)*nlay1+j
C          rx1(jj) = rx1(jj)+(-1)**(i+1)*dx1/4.
C        enddo
C      enddo
c     

c set number of layers for Fe initialy big enough to make large slab
c for Al to slide on:
      nlay3 = nint((1.5*L1)/dx2) + 2
      if (mod(nlay3,2) .eq. 0) then
        nlay3 = nlay3 + 1
      endif

      ns2=nlay3*nlay2
      call fastslab(nlay2,dx2,rx2,ry2,ns2)

c Standard (but slow cubic setup)...
c      call creslab(nlay3,dx2,rx2,ry2,ns2)
      write(*,*)'Fe-slab created.'
c      call sort(rx2,ry2,ns2)
c      write(*,*)'Fe-slab sorted.'

Cc
Cc translate each atom a distance +/- dx2/4 (bcc)       
Cc
C      do i = 1,nlay3
C        do j = 1,nlay3
C          jj = (i-1)*nlay3+j
C          rx2(jj) = rx2(jj)+(-1)**(i+1)*dx2/4.
C        enddo
C      enddo
      L2 = nlay3*dx2
      H2 = nlay2*dx2  
      X=0 
      Y= -((H1-dx1)+(H2-dx2))/2 - d12
      ns2=nlay3*nlay2
      call translate(rx2,ry2,ns2,X,Y) 
      H2=nlay2*dx2
c
c Define initial array of position and velocities:
c
      n = ns1+ns2
      WRITE(*,*) 'ns1,ns2,n = ',ns1,ns2,n
      if (n .gt. nmax) then 
        write(*,*)'ABORT: n .gt. nmax in init '
        write(*,*)'n,nmax = ',n,nmax
        stop
      endif
      do i = 1,ns1
        rx(i) = rx1(i)
        ry(i) = ry1(i)
        if (mod(i-1,ns1/nlay1) .eq. 0) then
          col(i) = 1
        else
          col(i) = 0
        endif
        if (i .le. ns1/nlay1 .and. col(i) .eq. 0) then
          col(i) = 3
	endif
        tag(i) = r1
      enddo
      do i = 1,ns2
        rx(ns1+i) = rx2(i)
        ry(ns1+i) = ry2(i)
        tag(ns1+i) = r2
        col(ns1+i) = 0
      enddo
c
c Assign initial neighbourhood map to surface particles:
c
      if (nlay2 .eq. 1) then
        nbnr(ns1+1) = 1
        nbours(ns1+1,1)=ns1+2
        nbnr(ns1+nlay3)=1
        nbours(ns1+nlay3,1)=ns1+nlay3-1
        do  i = 2,nlay3-1
         ii = i+ns1
          nbnr(ii) = 2
          nbours(ii,1) = ii+1
          nbours(ii,2) = ii-1
        enddo
      else
c first:
        nbnr(ns1+1)=2
        nbours(ns1+1,1)=ns1+2
        nbours(ns1+1,2)=ns1+nlay3+1
     
        nbnr(ns1+nlay3)=2
        nbours(ns1+nlay3,1)=ns1+nlay3-1
        nbours(ns1+nlay3,2)=ns1+2*nlay3
c last:
        nbnr(n)=2
        nbours(n,1)=n-1
        nbours(n,2)=n-nlay3

        nbnr(n-nlay3+1) = 2
        nbours(n-nlay3+1,1) = n-nlay3+2 
        nbours(n-nlay3+1,2) = n-2*nlay3+1
c interiour surface:
        do i = 2,nlay3-1 
         ii = i+ns1
         nbnr(ii) = 3
         nbours(ii,1) = ii+1 
         nbours(ii,2) = ii-1
         nbours(ii,3) = ii+nlay3
        
         ii = ns1+(nlay2-1)*nlay3+i
         nbnr(ii) = 3
         nbours(ii,1) = ii+1
         nbours(ii,2) = ii-1
         nbours(ii,3) = ii-nlay3
        enddo  
  
        do i = 2,nlay2-1
         ii = ns1+(i-1)*nlay3+1
         nbnr(ii) = 3
         nbours(ii,1) = ii+1
         nbours(ii,2) = ii-nlay3
         nbours(ii,3) = ii+nlay3
  
         ii = ns1+i*nlay3         
         nbnr(ii) = 3
         nbours(ii,1) = ii+nlay3
         nbours(ii,2) = ii-1
         nbours(ii,3) = ii-nlay3
        enddo
c interiour:
        do i = 2,nlay2-1
         do j = 2,nlay3-1
           jj = ns1+(i-1)*nlay3+j
           nbnr(jj) = 4
           nbours(jj,1) = jj-1
           nbours(jj,2) = jj+1
           nbours(jj,3) = jj-nlay3
           nbours(jj,4) = jj+nlay3
         enddo
        enddo
      endif

c
c Initialize the grid
c
c The grid has ngridw*ngridw boxes and covers
c the area (-L1/2,L1/2)*(-H1,H1).
c
      ngridw = int(L1 / rcut + 0.5)
      gridbx = L1/2
      gridax = ngridw / L1
      ngridh = int(2 * H1 / rcut + 0.5)
      ngridh = ngridh/0.9
      gridby = H1
      griday = ngridh / H1 / 2
      if ((ngridw * ngridh) .gt. nmax*2) then
         write(*,*) 'ngmax (',ngmax,') or rcut is too small.'
         stop
      endif
c
c Finde the particles for a given box
c
      do i = 1,ns1
         j = getgridindex(rx(i),ry(i))
         ngridindex(i) = j
         ngridnr(j) = ngridnr(j) +1
         if (ngridnr(j) .gt. ngmax) then
            write(*,*)'To many particles for a box.'
            stop
         endif
         ngrid(j,ngridnr(j))=i
      enddo

c
c Calculate the neighbourboxes for a given box
c
      do i = 1,ngridh
         do j = 1,ngridw
            jj = 1
            ii = (i-1)*ngridw +j
            do k =-1,1
               do l = -1,1
                  kk = k
                  ll = l
                  if (l+j .le. 0) then
                     kk = kk +1
                  endif
                  if (l+j .gt. ngridw) then
                     kk = kk -1
                  endif 
                  if (k+i .le. 0) then
                     ll = 0
                     kk = 0
                  endif
                  if (k+i .gt. ngridh) then
                     ll = 0
                     kk = 0
                  endif
                  same = .false.
                  mm = (i+kk-1)*ngridw+j+ll
                  do m = 1,jj-1
                     if (mm .eq. ngridpath(ii,m))then
                        same = .true.
                     end if
                  enddo
c
c Check if the box has already been added to the path
c 
                  if (same) then
                     ngridpath(ii,jj)=nmax*2
                  else
                     ngridpath(ii,jj)= mm
                  endif
                  jj = jj +1
               enddo
            enddo
         enddo
      enddo
      write(*,*)'Grid initialized.'
c
c  Assign Velocities based on temperature:
c  CRAY:
c      call ranset(iseed,0)
        do i = 1,ns1
         vx(i) = 0
         vy(i) = vfr
         th(i) = 0
c         if (i .le. int(ns1/nlay1+0.5)) then
           w(i) = 0
c         else
c           w(i) = ((-1)**(int(1+(i-1+0.5)/nlay1)))*vfr
c         endif
         massi = mass1
         if (i .gt. ns1) massi=mass2
        if (iseed .eq. 0) then
          vx(i) = 0
          vy(i) = 0
        else        
c CRAY:
c         rk1 = ranf()
c         rk2 = ranf()
c: HP:
c         rk1 = rand(iseed)
c         rk2 = rand(iseed)
c DEC/SIL. GRAPH.
          rk1 = ran(iseed)
          rk2 = ran(iseed)
	  vvx  = (-2*log(rk1))**0.5 * cos(2*pi*rk2)
	  vvy  = (-2*log(rk1))**0.5 * sin(2*pi*rk2)
	  vx(i) = vvx*sqrt(kbolz*Tmp/massi)
	  vy(i) = vvy*sqrt(kbolz*Tmp/massi)
        endif
      enddo
c      vx(1) = 0.0
c      vx(2) = 0.1
c      vy(1) = 0
c      vy(2) = 0
c      rx(1) = rx(1)
c      rx(2) = rx(1)+r1*2
c      ry(1) = ry(1) + r1*0
c      ry(2) = ry(2) -r1*0
c      w(1) = 0
c      w(2) = 1 
      end


c-------------------------------------------------------------:
c---- Fast Slab
c-------------------------------------------------------------:

      subroutine fastslab(nlayer,dx,rx,ry,n)

c
c number of atoms in each layer = nlayer (2D)
c Create a cubic slab of natomic layers
c each layer containing n/nlayer  atoms
c with distance dx between each other
c
c The positions of each atom are returned in rx,ry arrays
c n is the number of atomic positions (input!)        
c
      parameter (nmax=100000)
      integer n,nlayer,nx,ij,i,j  
      double precision rx(nmax),ry(nmax),dx,L,H

      if (mod(nlayer,2) .eq. 0) then
        write(*,*)'This qubic packing algorithm requires odd nr. layers'
        nlayer=nlayer+1
        write(*,*)'I increased nlayers to:',nlayer
      endif
 
      write(*,*)' Entered fastslab, nlayer=',nlayer
      write(*,*)' Entered fastslab, n=',n
      if (mod(n,nlayer) .ne. 0) then
        write(*,*)'Error in fastslab, nx*nlayer .ne. n '                  
        stop
      endif
 
      nx = int(n/nlayer+0.5)
      L = nx*dx
      H = nlayer*dx
      ij = 0
      do i = 1,nlayer
        do j = 1,nx    
          ij = ij+1
          rx(ij) = (-L+dx)/2 + (j-1)*dx
          ry(ij) = (H-dx)/2 - (i-1)*dx 
        enddo 
      enddo
      return
      end
      

c-------------------------------------------------------------:
c---- Fast Slab Compact
c-------------------------------------------------------------:

      subroutine fastslabcompact(nlayer,dx,rx,ry,n)

c
c number of atoms in each layer = nlayer (2D)
c Create a compact slab of natomic layers
c each layer containing n/nlayer  atoms
c with distance dx between each other
c
c The positions of each atom are returned in rx,ry arrays
c n is the number of atomic positions (input!)        
c
      parameter (nmax=100000)
      integer n,nlayer,nx,ij,i,j  
      double precision rx(nmax),ry(nmax),dx,L,H

      if (mod(nlayer,2) .eq. 0) then
        write(*,*)'This qubic packing algorithm requires odd nr. layers'
        nlayer=nlayer+1
        write(*,*)'I increased nlayers to:',nlayer
      endif
 
      write(*,*)' Entered fastslab, nlayer=',nlayer
      write(*,*)' Entered fastslab, n=',n
      if (mod(n,nlayer) .ne. 0) then
        write(*,*)'Error in fastslab, nx*nlayer .ne. n '                  
        stop
      endif
 
      nx = int(n/nlayer+0.5)
      L = nx*dx
      H = nlayer*dx
      ij = 0
      do i = 1,nlayer
        do j = 1,nx    
          ij = ij+1
          rx(ij) = (-L+dx)/2 + (-0.25*((-1)**i)+0.25+j-1)*dx
          ry(ij) = sqrt(0.75)*(H-dx)/2 - (i-1)*dx*sqrt(0.75) 
        enddo 
      enddo
      return
      end
      

c-------------------------------------------------------------:
c---- Create Slab
c-------------------------------------------------------------:

      subroutine creslab(nlayer,dx,rx,ry,n)

c
c number of atoms in each layer = nlayer (2D)
c Create a cubic slab of natomic layers 
c each layer containing natl atoms
c with distance dx between each other
c
c The positions of each atom are returned in rx,ry arrays
c n is the number of atomic positions (=nlayer*nlayer)
c nlayer=1 is stored at origin.
c
      parameter (nmax=100000)
      integer n,nlayer,nld2
      double precision rx(nmax),ry(nmax),dx  

      if (mod(nlayer,2) .eq. 0) then
        write(*,*)'This qubic packing algorithm requires odd nr. layers'
        nlayer=nlayer+1
        write(*,*)'I increased nlayers to:',nlayer
      endif
      rx(1)=0
      ry(1)=0
      n=1
      nld2=(nlayer-1)/2
      do i = 1,nld2      
        do j = 0,i
          if (j .eq. 0) then
             n=n+1
             rx(n)=i*dx
             ry(n)=0
             n=n+1
             rx(n)=-i*dx
             ry(n)=0
             n=n+1
             ry(n)=i*dx
             rx(n)=0
             n=n+1
             ry(n)=-i*dx
             rx(n)=0
          elseif (j.eq.i) then
             n=n+1
             rx(n)=i*dx
             ry(n)=i*dx
             n=n+1
             rx(n)=-i*dx
             ry(n)=i*dx
             n=n+1
             rx(n)=i*dx
             ry(n)=-i*dx
             n=n+1
             rx(n)=-i*dx
             ry(n)=-i*dx
          else  
             n=n+1
             rx(n)=i*dx
             ry(n)=j*dx
             n=n+1
             rx(n)=-i*dx
             ry(n)=j*dx
             n=n+1
             rx(n)=i*dx
             ry(n)=-j*dx
             n=n+1
             rx(n)=-i*dx
             ry(n)=-j*dx
             n=n+1
             rx(n)=j*dx
             ry(n)=i*dx
             n=n+1
             rx(n)=-j*dx
             ry(n)=i*dx
             n=n+1
             rx(n)=j*dx
             ry(n)=-i*dx
             n=n+1
             rx(n)=-j*dx
             ry(n)=-i*dx
          endif
        enddo
      enddo
      end
      

c-------------------------------------------------------------:
c---- Sort
c-------------------------------------------------------------:

      subroutine sort(rx,ry,n)

c
c Sort the number of layers with decreasing 
c RY coordinate, and incraesing RX.
c (first layer is top of slab)
c
      parameter (nmax=100000)
      integer n,nlay,i,j,index
      double precision rx(nmax),ry(nmax), ymax,xch,ych 

c sort with respect to decreasing y.
      do i = 1,n
        ymax=ry(i)       
        index=i
        do j = i+1,n  
          if (ry(j) .gt. ymax) then
            ymax=ry(j)
            index=j
          endif
        enddo   
        ych=ry(i)
        xch=rx(i)
        rx(i)=rx(index)
        ry(i)=ry(index)
        rx(index)=xch        
        ry(index)=ych           
      enddo
c sort with respect to increasing  (not realy necessary for dynamics)
      nlay=nint(n**0.5)
      do i = 1,nlay 
        do j = (i-1)*nlay+1,i*nlay
          ymax=rx(j)          
          index=j
          do k=j+1,i*nlay
            if (rx(k) .lt.  ymax) then
              ymax=rx(k)
              index=k
            endif
          enddo
          ych=ry(j)
          xch=rx(j)
          rx(j)=rx(index)
          ry(j)=ry(index)
          rx(index)=xch
          ry(index)=ych
        enddo
      enddo
      end
      

c-------------------------------------------------------------:
c---- Translate
c-------------------------------------------------------------:

      subroutine translate(rx,ry,n,DX,DY)

c 
c translate each atom a distance DX,DY in the X,Y diection.
c
      parameter (nmax=100000)
      integer i,n
      double precision rx(nmax),ry(nmax), DX,DY           
      do i = 1,n
        rx(i) = rx(i)+DX
        ry(i) = ry(i)+DY
      enddo
      end


c-------------------------------------------------------------:
c---- Dump
c-------------------------------------------------------------:

      subroutine dump(rx,ry,vx,vy,w,n,t2,tut1)

c
c Dump active atoms to new data-file.
c
      parameter (nmax=100000)
      parameter (nvmax=10000)
      double precision t2,tut1
      double precision rx(nmax),ry(nmax),rxold(nmax)
      double precision vx(nmax),vy(nmax),w(nmax)
      double precision v(nvmax),vn(nvmax),omega(nvmax)
      integer ih,ik,i,n,index,mindex
      data ik,ih/0,0/
      
c energie:
      double precision frik,load,epot,ekin1,ekin2,erot,mrx,mry,dL
      common /energie/ frik,load,epot,ekin1,ekin2,erot,mrx,mry,dL

      character*80 outf
      common /fil/outf

      double precision L1,H1,L2,H2
      common /box/L1,H1,L2,H2
 
      double precision gamman,gammas, my, my12, r1, r2, r0
      common /frikpar/  gamman,gammas, my, my12, r1, r2, r0

      double precision mass1,mass2,rcut,dx1,dx2
      integer nlay1,ns1,nlay2,ns2
      common /lattice/ mass1,mass2,rcut,nlay1,ns1,dx1,nlay2,ns2,dx2
c
      character*80 temp
      double precision L,H
      
c Do an assert
      if(nlay1*10 .gt. nvmax) then
        write(*,*)"dump(): nlay1 is to big, increase nvmax"
        stop
      endif

      write(*,*)"Dump at t=",t2

c
      do i = 1,80
        if (outf(i:i) .eq. ' ') goto 100
      enddo
 100  ihi=i-1


c Calculate the velocity and rotation profile and rel. position

      mrx = 0
      mry = 0
      L = L1*(nlay1-1)/nlay1
      H = H1*(nlay1-1)/nlay1
      if (H .gt. 0 ) then
        dL = (ry(1)-ry(ns1))/H
      else
        dL = 0
      endif
      mindex = nlay1*0.5
      do i = 1,mindex
        v(i)  = 0.0
        omega(i)  = 0.0
        vn(i) = 0
      enddo
      do i = 1,ns1
        mrx = mrx + rx(i)
        mry = mry + ry(i)
        index = ((ry(i) + H1)*mindex)/(H1*2)
        if (index .le. 0) then 
          index = 1
        endif
        if (index .gt. mindex) then
          index = mindex
        endif
        if (tut1 .eq. 0) then
          v(index) = v(index) + vx(i)
        else
          if(rx(i).le.rxold(i).and.abs(rx(i)-rxold(i)).gt.r1*2)then
            v(index) = v(index) + (rx(i)-rxold(i)+L1)/tut1        
          else  
            v(index) = v(index) + (rx(i)-rxold(i))/tut1        
          endif
        endif
        rxold(i) = rx(i)
        omega(index) = omega(index) + w(i) 
        vn(index) = vn(index) + 1 
      enddo
      if (H .gt. 0 .and. L .gt. 0) then
        mrx = (mrx/ns1 + (L)/2)/L
        mry = (mry/ns1 + (H)/2)/H	
      else
        mrx = 0.5
        mry = 0.5
      endif
      do i = 1,mindex
        if (vn(i) .gt. 0) then
          v(i) = v(i) / vn(i)
          omega(i) = omega(i) / vn(i)
        endif
      enddo

c write the velocity profile
      temp = outf(1:ihi)//char(86)//char(48+ih)//char(48+ik)
      open(29,file=temp,STATUS= 'UNKNOWN')
      write(29,*)'# ',t2,tut1
      do i = 1,mindex
        if (vn(i) .gt. 0) then
          write(29,'(7F20.10)')v(i),-H1/2 + i*H1/mindex, vn(i)
        endif
      enddo
      close(29)

c write the rotation profile
      temp = outf(1:ihi)//char(87)//char(48+ih)//char(48+ik)
      open(29,file=temp,STATUS= 'UNKNOWN')
      do i = 1,mindex
        if (vn(i) .gt. 0) then
          write(29,'(7F20.10)')-H1/2 + i*H1/mindex, vn(i)
        endif
      enddo
      close(29)

	
c write the coords, v, theta
c      temp = outf(1:ihi)//char(67)//char(48+ih)//char(48+ik)
c      open(29,file=temp,STATUS= 'UNKNOWN')
cC      write(29,'(5F9.4,2I5,F10.5)')time, L1, dx1, dx2, rcut, ns1, ns2
c      do i = 1,n
c        write(29,'(6e20.10)')rx(i),ry(i),vx(i),vy(i)
c      enddo
c      close(29)

c write the energie      
      temp = outf(1:ihi)//char(69)//char(48+ih)//char(48+ik)
      open(29,file=temp,STATUS= 'UNKNOWN')
      write(29,*)'# Time,Ekin1,Erot1,Load1,Fric1,Ekin2,Epot2,mrx,mry,dL'
      write(29,'(7F20.10)')t2,ekin1/ns1,erot/ns1,load/nlay1,frik/nlay1,
     $ ekin2/ns2,epot/ns2,mrx,mry,dL      
      close(29)

            
      ik=ik+1
      ik= mod(ik,10)
      if (ik.eq.0) ih=ih+1

      end


c-------------------------------------------------------------:
c---- Update
c-------------------------------------------------------------:

      subroutine update(rx,ry,n)         

c
c Updating neighboorhood map around each atom      
c XXX: THIS VERSION: Only map between surface particles:
c
c nbmax(i,j) contains the 'nbmax'  nearest neigbours of sphere nr. i
c
      double precision skin
      parameter (skin=1.5)
      parameter (nmax=100000,nbmax=4)
      integer n
      double precision rx(nmax),ry(nmax)
      double precision rdist(nmax),rmin
      integer kulnr(nmax),index,i,j,k
      double precision rxi,ryi,rxij,ryij

c nabourmap:
      integer nbours(nmax,nbmax),nbnr(nmax)
      common /nbhood/nbours,nbnr

c Simulation boxes of each layer (used for periodic boundary cond.)
      double precision L1,H1,L2,H2
      common /box/L1,H1,L2,H2

      double precision mass1,mass2,rcut,dx1,dx2
      integer nlay1,ns1,nlay2,ns2
      common /lattice/ mass1,mass2,rcut,nlay1,ns1,dx1,nlay2,ns2,dx2

C XXX: 
c let ns1-> n to work on both phases
      ns11=ns1+1
      do i = ns11,n
        rxi=rx(i)          
        ryi=ry(i)         
        do j = ns11,n 
            kulnr(j)=j
            rxij  =  rxi-rx(j)                
            ryij  =  ryi-ry(j)          
            rdist(j) = (rxij**2 + ryij**2)**0.5  
        enddo
c
c Stuff atoms within skin*rcut from nr. i into neighbour list:
c
        do j = 1,nbmax 
          rmin = 1e+30
C XXX: The loop goes only over Al phase:
c let ns1-> n to work on both phases
          do k = j,n
           if ((rdist(k).lt.rmin).and.(rdist(k).gt.0.0001)) then     
             index=k
             rmin=rdist(k)
            endif
          enddo
          if (rmin .gt. skin*rcut) goto 99
          if (j .eq. nbmax) then
            write(*,*)'XXXX OVERFLOW ERROR in UPDATE'
            write(*,*)'atom nr. ',i,'have to many neighbours'
            write(*,*)'increase nbmax- recompile'
            stop
          endif
          nbours(i,j)=kulnr(index)
          kulnr(index)=kulnr(j)
          rdist(index)=rdist(j)
        enddo
 99     continue
        nbnr(i) = j-1
      enddo
      end


c-------------------------------------------------------------:
c------------ UPDATE THE GRID --------------------------------:
c-------------------------------------------------------------:

      subroutine updategrid(index,x,y)

c
c This routine updates a particle.  
c
c Input variables:
c index : index of the particle
c x,y   : new position of the particle 
c
      integer index
      double precision x,y
      parameter (nmax=100000,nbmax=4,ngmax=16)

c Grid
      integer ngrid(nmax*2,ngmax),ngridnr(nmax*2),ngridpath(nmax*2,9)
      integer ngridindex(nmax*2),ngridw,ngridh
      double precision gridax,gridbx,griday,gridby
      common /grid/gridax,gridbx,griday,gridby,ngrid,ngridnr,
     $     ngridpath,ngridindex,ngridw,ngridh

      integer old,new,i
      integer getgridindex
      logical found

      found = .false.
      new = getgridindex(x,y)
      old = ngridindex(index)
      ngridindex(index) = new
      if (new .ne. old) then
        do i=1,ngridnr(old)
           if (found) then
              ngrid(old,i-1) = ngrid(old,i)
           else
              if (ngrid(old,i) .eq. index) then
                 found =.true.
              endif
           endif    
        enddo
        if (found) then
           ngridnr(old) = ngridnr(old)-1
        else
           write(*,*)'Indextable souspius.'
        endif
        ngridnr(new) = ngridnr(new)+1
        if (ngridnr(new) .gt. ngmax) then
           write(*,*)'Too many particles for a box. Maybe choas.'
           stop
        endif
        ngrid(new,ngridnr(new)) = index
      endif
      
      return
      end


c-------------------------------------------------------------:
c------------ GET THE INDEX OF THE GRID ----------------------:
c-------------------------------------------------------------:

      integer function getgridindex(x,y)

c
c This routine calculates the index of the grid
c for a given x,y. 
c
c Input variables:
c x,y : position of the particle
c
c Output vraibles:
c getgridindex : index of the box
c
      double precision x,y
      parameter (nmax=100000,nbmax=4,ngmax=16)

c Grid
      integer ngrid(nmax*2,ngmax),ngridnr(nmax*2),ngridpath(nmax*2,9)
      integer ngridindex(nmax*2),ngridw,ngridh
      double precision gridax,gridbx,griday,gridby
      common /grid/gridax,gridbx,griday,gridby,ngrid,ngridnr,
     $     ngridpath,ngridindex,ngridw,ngridh

      integer w,h
      w = (x+gridbx)*gridax
      if (w .lt. 0) then
         w = 0
      endif
      if (w .ge. ngridw) then
         w = ngridw-1
      endif
      h = (y+gridby)*griday
      if (h .lt. 0) then
         h = 0
      endif
      if (h .ge. ngridh) then
         h = ngridh-1
      endif
      getgridindex = h * ngridw + w +1
      return
      end


c-------------------------------------------------------------:
c------------ FORCE CALCULATORS nabfrc, nabrel, bforce -------:
c-------------------------------------------------------------:

      subroutine bforce(rx,ry,vx,vy,th,w,n,first)  

c
c This routine calculates the force between the 
c THIS VERSION: Al particles
c To include Active Fe/slab see XXX
c
c Force is calculated between all paricles closer than cutoff
c distance rcut between each other.                 
c
      implicit none
      integer nmax,nbmax,ngmax
      parameter (nmax=100000,nbmax=4,ngmax=16)          
      logical first
      double precision fxi,fyi,rxij,ryij,rijsq,fij
      double precision sr1,fsx,fsy,fs,fnx,fny,fc,mred,vs
      double precision rxi,ryi,fxij,fyij
      integer i,ii,j,jj,k,l,ns11,n,index
      double precision rx(nmax),ry(nmax),vx(nmax),vy(nmax)
      double precision th(nmax), w(nmax)
      double precision taui,tauij
c common:
c Simulation boxes of each layer (used for periodic boundary cond.)
      double precision L1,H1,L2,H2
      common /box/L1,H1,L2,H2

      double precision fxa(nmax),fya(nmax),tau(nmax)
      common /config/fxa,fya,tau

c energie:
      double precision frik,load,epot,ekin1,ekin2,erot,mrx,mry,dL
      common /energie/ frik,load,epot,ekin1,ekin2,erot,mrx,mry,dL

c external permanent forces                                              
      double precision fx,fy
      integer mal,nal,malfe,nalfe,mfe,nfe
      double precision alale,alals,alfee,alfes,fefee,fefes
      common /force/fx,fy,alale,alals,alfee,alfes,fefee,fefes,
     $              mal,nal,malfe,nalfe,mfe,nfe

ction/sphere parameters:
c gamman: Normal friction koefficient
c gammas: Shear friction koefficient
c my     Kinetic friction coefficient
c my12               "               between particles and bed
c r1    :Radius of active particles
c r2    :Radius of bed particles
c r0    :Equilibrium distance between bed particls (elastic spring)
      double precision gamman,gammas, my, my12, r1, r2, r0
      common /frikpar/  gamman,gammas, my, my12, r1, r2, r0

      double precision r1r1,r1r2
      double precision mass1,mass2,rcut,dx1,dx2
      integer nlay1,ns1,nlay2,ns2
      common /lattice/ mass1,mass2,rcut,nlay1,ns1,dx1,nlay2,ns2,dx2

c Grid
      integer ngrid(nmax*2,ngmax),ngridnr(nmax*2),ngridpath(nmax*2,9)
      integer ngridindex(nmax*2),ngridw,ngridh
      double precision gridax,gridbx,griday,gridby
      common /grid/gridax,gridbx,griday,gridby,ngrid,ngridnr,
     $     ngridpath,ngridindex,ngridw,ngridh

      integer getgridindex
c nabourmap
      integer nbours(nmax,nbmax),nbnr(nmax)
      common /nbhood/nbours,nbnr
      ns11=ns1+1
c
c START FORCE CALCULATION:
c Take care of periodic boundary at x=L and hard wall
c on top as well:
c
      load=0
      frik=0
      mred=0.5*mass1
      r1r1=2*r1
      r1r2=r1+r2
       
      do i=1,n
         fxa(i) = 0 
         fya(i) = 0 
         tau(i) = 0
      enddo

c FORCES AND TORQUES BETWEEN ACTIVE PARTICLES:
      do i = 1,ns1
        rxi=rx(i)            
        ryi=ry(i)           
        fxi=0
        fyi=0
        taui=0

        ii = ngridindex(i)
        do k=1,9
         jj = ngridpath(ii,k)
         do l=1,ngridnr(jj)
          j = ngrid(jj,l)
c          if (mod(i-1,ns1/nlay1) .eq. mod(j-1,ns1/nlay1)) then
c            j = 0
c          end
          if (j .gt. i) then
           ryij  =  ryi-ry(j)          
           if ((ryij .ge. (-rcut)).and.(ryij .le. rcut)) then
             rxij  =  rxi-rx(j)

             if (rxij .lt. -L1/2) then
                rxij = rxij + L1
             endif
             if (rxij .gt. L1/2) then
                rxij = rxij - L1
             endif

             if ((rxij .ge. (-rcut)).and.(rxij .le. rcut)) then
                rijsq = sqrt(rxij**2+ryij**2)
                if (rijsq .le. rcut) then
                   sr1 = alale*(abs(r1r1-rijsq))**alals 
		   if ((r1r1-rijsq) .le. 0) then
		      sr1 = -sr1
		   endif
                   fnx = sr1 * rxij/rijsq 
     $                  -  mred * gamman * (vx(i)-vx(j))
                   fny = sr1 * ryij /rijsq
     $                  -  mred * gamman * (vy(i)-vy(j))
        vs = -(vx(i)-vx(j))*ryij/rijsq 
     $                  + (vy(i)-vy(j))*rxij/rijsq
     $                  - w(i)*r1 - w(j)*r1
                   fs  = gammas * mred * vs
                   fsx = -fs * ryij/rijsq
                   fsy =  fs * rxij/rijsq 
c Coloumb friction:
                   fc = my*sqrt(fnx**2+fny**2)
		   if (abs(fs) .gt. fc) then
                     fsx = -fc * ryij/rijsq*fs/abs(fs)
                     fsy =  fc * rxij/rijsq*fs/abs(fs) 
                   endif
		   
                   fxij = fnx+fsx 
                   fyij = fny+fsy
                   tauij = 0.5*(rxij*fsy-ryij*fsx)

                   fxi=fxi+fxij
                   fyi=fyi+fyij
                   taui=taui+tauij

                   fxa(j) = fxa(j) - fxij
                   fya(j) = fya(j) - fyij
                   tau(j) = tau(j) + tauij
                endif
             endif
           endif  
          endif
         enddo
        enddo
        fxa(i) = fxa(i)+fxi
        fya(i) = fya(i)+fyi
        tau(i) = tau(i)+taui
      enddo
      
c
c FORCES AND TORQUES BETWEEN ACTIVE AND SURFACE PARTICLES: 
c

      mred=mass1*mass2/(mass1+mass2)
      do i = ns11,n 
        rxi=rx(i)
        ryi=ry(i)
        fxi=0
        fyi=0
        taui=0

        ii = getgridindex(rxi,ryi)
        do k=1,9
         jj = ngridpath(ii,k)
         do l=1,ngridnr(jj)
          j = ngrid(jj,l)
          rxij  =  rxi-rx(j)
          if ((rxij .ge. (-rcut)).and.(rxij .le. rcut)) then
          ryij  =  ryi-ry(j)
          if ((ryij .ge. (-rcut)).and.(ryij .le. rcut)) then
          rijsq = sqrt(rxij**2+ryij**2)

          if (rijsq .lt. rcut) then
             sr1 = alale*(abs(r1r2-rijsq))**alals
	     if ((r1r2-rijsq) .le. 0) then
		sr1 = -sr1
	     endif

             fnx = sr1 * rxij/rijsq 
     $            -  mred * gamman * (vx(i)-vx(j))
             fny = sr1 * ryij/rijsq 
     $            -  mred * gamman * (vy(i)-vy(j))
            
            vs =-(vx(i)-vx(j))*ryij/rijsq+(vy(i)+vy(j))*rxij/rijsq
     $            -  w(j)*r1 
            fs  = gammas * mred * vs
            fsx = -fs * ryij/rijsq
            fsy =  fs * rxij/rijsq 

c Coloumb friction:
            fc = my12*sqrt(fnx**2+fny**2)
	    if (abs(fs) .gt. fc) then
              fsx = -fc * ryij/rijsq*fs/abs(fs)
              fsy =  fc * rxij/rijsq*fs/abs(fs) 
            endif

            fxij = fnx+fsx
            fyij = fny+fsy
            tauij = (r1/r1r2)*(rxij*fsy-ryij*fsx)
            fxi=fxi+fxij
            fyi=fyi+fyij
            taui=taui+tauij

            fxa(j) = fxa(j) - fxij
            fya(j) = fya(j) - fyij
            tau(j) = tau(j) + tauij
            frik=frik+fxij        
            load=load+fyij           
           endif
          endif  
         endif
        enddo
       enddo
       fxa(i) = fxa(i)+fxi
       fya(i) = fya(i)+fyi
       tau(i) = 0
      enddo
	

c  FORCES BETWEEN SURFACE PARTICLES:
c (nonrotating;nearest neigbour strings)
c
      epot=0
      do i = ns11,n
        rxi=rx(i)
        ryi=ry(i)
        fxi=0
        fyi=0
        do j=1,nbnr(i)        
          index=nbours(i,j) 
          rxij  =  rxi-rx(index)                 
          ryij  =  ryi-ry(index)          
          rijsq = sqrt(rxij**2+ryij**2)
          fij   = fefee*(r0-rijsq)
          fxij  = fij*rxij/rijsq
          fyij  = fij*ryij/rijsq   
          fxi=fxi+fxij
          fyi=fyi+fyij
          epot = epot+0.5*fefee*(r0-rijsq)**2
        enddo
        fxa(i)=fxa(i)+fxi
        fya(i)=fya(i)+fyi
      enddo
      epot=epot/2
      return
      end


c-------------------------------------------------------------:
c---- Verlet Vel
c-------------------------------------------------------------:

      subroutine verlet_vel(frc,rx,ry,th,vx,vy,w,n,t1,t2,dt,first)

c
c Propagation by Verlet: r(t+dt) = 2r(t) - r(t-dt) * dt^2 a(t)
c THIS VERSION: Al particles
c To include Active Fe/slab see XXX
c     
      implicit none
      
      external frc,updategrid
      integer nmax
      parameter (nmax=100000)
      logical first,zig
      double precision massi,vxav,vyav
      double precision I1,L,H,vm,wm
      double precision t1,t2,dt,dt2,dtsq,dthlf
      double precision vx(nmax),vy(nmax)
      integer ns11,nt,i,j,n,nn,nc,k1,k2 
      double precision rx(nmax),ry(nmax),rxold(nmax)
      double precision th(nmax), w(nmax)

c Simulation boxes of each layer (used for periodic boundary cond.)
      double precision L1,H1,L2,H2
      common /box/L1,H1,L2,H2

c energie:
      double precision frik,load,epot,ekin1,ekin2,erot,mrx,mry,dL
      common /energie/ frik,load,epot,ekin1,ekin2,erot,mrx,mry,dL

      double precision fx,fy
      integer mal,nal,malfe,nalfe,mfe,nfe
      double precision alale,alals,alfee,alfes,fefee,fefes
      common /force/fx,fy,alale,alals,alfee,alfes,fefee,fefes,
     $              mal,nal,malfe,nalfe,mfe,nfe

ction/sphere parameters:
c gamman: Normal friction koefficient
c gammas: Shear friction koefficient
c my     Kinetic friction coefficient
c my12               "               between particles and bed
c r1    :Radius of active particles
c r2    :Radius of bed particles
c r0    :Equilibrium distance between bed particls (elastic spring)
      double precision gamman,gammas, my, my12, r1, r2, r0
      common /frikpar/  gamman,gammas, my, my12, r1, r2, r0

      double precision fxa(nmax),fya(nmax),tau(nmax)
      common /config/fxa,fya,tau

      double precision mass1,mass2,rcut,dx1,dx2
      integer nlay1,ns1,nlay2,ns2
      common /lattice/ mass1,mass2,rcut,nlay1,ns1,dx1,nlay2,ns2,dx2

      double precision vfr,tmp, Q
      integer nvf,vfarr(1000), nt1,t1arr(1000),nt2,t2arr(1000)
      common /dynam/ Q, vfr,tmp, nvf,vfarr,nt1,t1arr,nt2,t2arr

c Units (good to ahve stored)
      double precision lenu,massu,eu,kbolz
      common /units/lenu,massu,eu,kbolz

      double precision maxw,sumfx

      real djitt,jitt
      data jitt/0.0/
      common /timut/djitt

      if (t2 .le. t1) then 
        write(*,*)' Error in verlet, tstop < tstart'
        write(*,*)' t1,t2,dt = ',t1,t2,dt
        stop
      endif

      if (first. eqv. .true.) then
c nn is number of active (atoms free to move)
        I1 = 0.4*mass1*r1**2
        dtsq=dt**2/2
        dt2=2*dt
        dthlf=dt/2
        ns11 = ns1+1
        nc = ns2/nlay2
        nn = nlay2-1
        call frc(rx,ry,vx,vy,th,w,n,first)
        first=.false.

      endif

      nt=(t2-t1)/dt     
      do j = 1,nt

        t1=t1+dt
        massi=mass1

c update r(t+dt) = r(t) + v(t)*dt + F(t)/m * dt**2/2
c update v(t+dt/2) = v(t+dt/2) + F(t+dt)/m * dt/2:

        ekin1=0
        erot=0
        vxav=0
        vyav=0

        do i = 1,ns1
c
c Only top level feels fy
c
          if (i. le. ns1/nlay1 .and. vfr .ge. 0) then
            w(i) = 0
            ry(i) = ry(i) + vy(i)*dt + dtsq*(fya(i)+fy)/mass1
            rx(i) = rx(i) + vx(i)*dt
            th(i) = th(i) + w(i)*dt
            vy(i) = vy(i) + dthlf *(fya(i)+fy)/mass1
          else
            rx(i) = rx(i) + vx(i)*dt + dtsq*(fxa(i)+fx)/mass1
            ry(i) = ry(i) + vy(i)*dt + dtsq*(fya(i)   )/mass1
            th(i) = th(i) + w(i)*dt  + dtsq*tau(i)/I1

            vx(i) = vx(i) + dthlf *(fxa(i)+fx)/mass1
            vy(i) = vy(i) + dthlf *(fya(i)   )/mass1
            w(i)  =  w(i) + dthlf * tau(i)/I1
          endif
          
c x-BOUNDARIES:
          if ((rx(i) .gt.  L1/2).and.(vx(i) .gt. 0)) then
c Cycling (->):
            rx(i) = rx(i) - L1
c Wall:
c            rx(i) = L1/2
c            vx(i) = 0
          endif
          if ((rx(i) .lt. -L1/2).and.(vx(i) .lt. 0)) then
c Cycling (<-):
            rx(i) = rx(i) + L1
c Wall:
c            rx(i) = -L1/2
c            vx(i) = 0
          endif

c y-BOUNDARIES:
          if (ry(i) .gt. (H1/8*3-r1) .and. vfr .gt. 0
     $            .and. ry(i) .gt. -r1) then
c          if (i .le. ns1/nlay1 .and. vfr .ge. 0) then
            vx(i) = vfr
            w(i)  = 0
            if ((abs(vy(i)).eq.vy(i)).and.(ry(i).gt.(H1/2-r1))) then
              vy(i) = -vy(i)
            endif
          endif
          
          ekin1=ekin1+0.5*mass1*(vx(i)**2+vy(i)**2)
          erot=erot+0.5*I1*w(i)**2
          vxav=vxav+vx(i)
          vyav=vyav+vy(i)

          call updategrid(i,rx(i),ry(i))
        enddo

C boundary for w
C	maxw = sqrt(abs(5/2*kbolz*tmp/mass1))/r1
	maxw = vfr
	do i = 1,ns1
	   if (abs(w(i)) .gt. maxw) then
	      w(i) = w(i) / abs(w(i)) * maxw
	   endif
	enddo


C  Update positions for SURFACE:
        ekin2 = 0
        do k2 = 1,nn
        do k1 = 2,nc-1
          i = ns1+nc*(k2-1)+k1
          rx(i) = rx(i) + vx(i)*dt + dtsq*(fxa(i)+fx)/mass2
          ry(i) = ry(i) + vy(i)*dt + dtsq*(fya(i)+fy)/mass2
          vx(i) = vx(i) + dthlf *(fxa(i)+fx)/mass2
          vy(i) = vy(i) + dthlf *(fya(i)+fy)/mass2

          ekin2=ekin2+0.5*mass2*(vx(i)**2+vy(i)**2)
        enddo
        enddo

c update Force(t+dt) = F(r(t+dt), v(t+dt/2)):
        call frc(rx,ry,vx,vy,th,w,n,first)

c update v(t+dt) = v(t+dt/2) + F(t+dt)/m * dt/2:


        do i = 1,ns1
          if (i .le. ns1/nlay1 .and. vfr .ge. 0) then
            vy(i) = vy(i) + dthlf *(fya(i)+fy)/mass1
            w(i) = 0
          else
            vy(i) = vy(i) + dthlf * (fya(i))/mass1
            w(i) =  w(i) +  dthlf * tau(i)/I1          
            vx(i) = vx(i) + dthlf *(fxa(i)+fx)/mass1
          endif
        enddo

c update the wall
        do k2 = 1,nn        
        do k1 = 2,nc-1 
          i = ns1+nc*(k2-1)+k1 
          vx(i) = vx(i) + dthlf *(fxa(i)+fx)/mass2
          vy(i) = vy(i) + dthlf *(fya(i)+fy)/mass2
        enddo
        enddo

c do some statistic
      if (t1 .gt. jitt) then
        jitt = jitt + djitt
        write(41,'(6E20.10)')t1, ekin1/ns1, erot/ns1, ekin2/ns2
        write(42,'(6E20.10)')t1, load/nlay1, frik/nlay1
        vm = 0
        k1 = 0
        k2 = 0
        sumfx = 0
        do i= 1,ns1
          if (ry(i) .gt. (H1/8*3-r1) .and. vfr .gt. 0
     $            .and. ry(i) .gt. -r1) then
	    sumfx = sumfx + fxa(i)
            k2 = k2+1
	  else
            if (rx(i) .le. rxold(i) .and. 
     $            abs(rx(i)-rxold(i)) .gt. r1*2) then
              vm = vm + (rx(i)-rxold(i)+L1)/djitt
            else
              vm = vm +(rx(i)-rxold(i))/djitt
            endif
            k1 = k1+1
          endif        
          rxold(i) = rx(i)
        enddo
        if (k1 .gt. 0) then
          vm = vm / k1
        endif
        if (k2 .gt. 0) then
	  sumfx = sumfx / k2
        endif
        if (zig .eq. .true.) then 
c          write(*,*)vm
          write(43,"(6E20.10)")t1, vm
        endif
        write(44,'(6E20.10)')t1, sumfx
      endif

      enddo
      


c Numeric corrections 
c
c Epsilon for the velocity:

c      do i = 1,ns1
c        if (vx(i)*vx(i)+vy(i)*vy(i) .le. 1e-5) then
c          vx(i) = 0
c          vy(i) = 0
c        endif
c        vx(i) = 0
c      enddo

c
c Caculate midium position and compression
c
      L = L1*(nlay1-1)/nlay1
      H = H1*(nlay1-1)/nlay1
      if (H .gt. 0) then
        dL = (ry(1)-ry(ns1))/H
      else
        dL = 0
      endif
c      mrx = 0
c      mry = 0
c      do i = 1,ns1
c        mrx = mrx + rx(i)
c        mry = mry + ry(i)
c      enddo
c      if (H .gt. 0 .and. L .gt. 0) then 
c        mrx = (mrx/ns1 + (L)/2)/L
c        mry = (mry/ns1 + (H)/2)/H	
c      else
c        mrx = 0.5
c        mry = 0.5
c      endif
      zig = .true.
      return
      end
