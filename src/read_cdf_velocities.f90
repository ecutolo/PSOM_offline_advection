subroutine read_cdf_velocities(nstp,dudx,dudy,dudz,dvdx,dvdy,dvdz,drdx,drdy)
  !subroutine readcdfcn(Tr,uf,vf,wf,xc,yc,zc,xf,yf,zf,nbegin)
  !     ------------------------------------------------------
  !     reads in the netCDF files written by the routine outcdf.f

  !use header, only : NI,NJ,NK,ntr,rc_kind
  use header

  implicit none

#include "netcdf.inc"
  ! include 'dims.f90'

  integer :: nstp
  integer :: idInFile

  REAL(kind=4) ::  dudx(NI,NJ,NK),dudy(NI,NJ,NK),dudz(NI,NJ,NK), &
                         dvdx(NI,NJ,NK),dvdy(NI,NJ,NK),dvdz(NI,NJ,NK), &
                         drdx(NI,NJ,NK),drdy(NI,NJ,NK)

  integer :: idvdy,idrdy,idrdx
  REAL(kind=rc_kind) ::  rcode
  !
  !double precision Tr(0:NI+1,0:NJ+1,0:NK+1,ntr), &
  !     uf(0:NI,NJ,NK),vf(NI,0:NJ,NK),wf(NI,NJ,0:NK)

  character (len = 550) :: inname_data

  integer start(3), count(3)), &
       countuf(3), countvf(3), countwf(3)

  DATA start /1, 1, 1/
  DATA count /NI2, NJ2, NK2/
  DATA start2d /1, 1/
  DATA count2d /NI, NJ/

  countuf(1)= NI+1
  countuf(2)= NJ
  countuf(3)= NK

  countvf(1)= NI
  countvf(2)= NJ+1
  countvf(3)= NK

  countwf(1)= NI
  countwf(2)= NJ
  countwf(3)= NK+1

  WRITE(zslice_data,'("zslice_",I5.5,".cdf")') NK
  idzSliceFile = ncopn(TRIM(dirout)//inname_data, NCNOWRIT,rcode)

  idxc = ncvid(idzSliceFile,'xc',rcode)
  idyc = ncvid(idzSliceFile,'yc',rcode)
  idzc = ncvid(idzSliceFile,'zc',rcode)
  call ncvgt( idzSliceFile, idxc, start(1), count(1), xc, rcode )
  call ncvgt( idzSliceFile, idyc, start(2), count(2), yc, rcode )
  call ncvgt( idzSliceFile, idzc, start2d, count2d, zc, rcode )

  call ncclos(idInFile, rcode)

  WRITE(inname_data,'("face_",I5.5,".cdf")') mod(nstp,10)
  idInFile = ncopn(TRIM(dirout)//inname_data, NCNOWRIT,rcode)

  iduf = ncvid(idInFile,'uf',rcode)
  idvf = ncvid(idInFile,'vf',rcode)
  idwf = ncvid(idInFile,'wf',rcode)

  call ncvgt( idInFile, iduf, start, countuf, uf, rcode )
  call ncvgt( idInFile, idvf, start, countvf, vf, rcode )
  call ncvgt( idInFile, idwf, start, countwf, wf, rcode )

  call ncclos(idInFile, rcode)

  return
end
