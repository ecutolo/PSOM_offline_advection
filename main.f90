PROGRAM main
!----------------------------------------------------
USE header
!implicit none
!INTEGER,PARAMETER :: NI=24,  NJ=24, NK=24,ngrid=3,maxout=20832,   maxint=15768,   int1=13824
!  use relaxation
INTEGER step,i,j,k !nsteps,n,initime
REAL(kind=rc_kind) ::  dtim

nsteps = 10
initime = 0
nbegin = initime

! 1. Initialize the tracers
CALL tracerinit(0)    !initializes tracer
! 2. advection routine
do step = initime,(initime+nsteps)
    ! 2a. load the velocity fields and interpolate in time
    if (mod(step,out3d_int).eq.0) then
        nbegin = step
    endif
    CALL readcdfcn(Tr,uf,vf,wf,xc,yc,zc,xf,yf,zf,nbegin)
    ! 2b. advect the tracer using the velocity fields
    do ivb=1,3
        if(ivb==1) then; dtim=dtf/3.d0; ivs=0;ivf=1;endif;
        if(ivb==2) then; dtim=0.5d0*dtf;ivs=1;ivf=1;endif;
        if(ivb==3) then; dtim=dtf      ;ivs=1;ivf=0;endif;
        tsp = dtim*1.d05
        CALL advection_and_mixing(ivs,ivf,dtim,step)
    enddo ! ivb
    ! 2c. react tracer
    CALL tracersource(step,dtim)
    ! 3. Save the tracer in a netcdf file
    if (mod(step,out3d_int).eq.0) then
        CALL write_cdf_3D(step,n)
    endif
enddo ! steps

END PROGRAM main
