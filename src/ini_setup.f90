SUBROUTINE ini_setup

  !     subroutine init(p,vfent,ufex)                                    
  !     ------------------------------------------------                  
  !     For the curvilinear grid.                                         
  !     sigma levels are evenly spaced.  Hence wz is a function of        
  !     time but not of z.   Here u,v,w refer to the xi,eta,sigma direcito
  !     Those metric quantities that do not change with time are evaluated
  !     The rest are evaluated in "sigma.f" which will be called at every 
  !     step.                                                             
  !     Also it is absolutely essential that the integral of the flux     
  !     over the entrance section is equal to that over the exit section. 
  !     -No longer necessary with the free surface.                       
  !     This may require some special attention when the free surface is  
  !     moving at the exit. At the entrance vf is held fixed.             
  !                                                                       
 USE header 

 REAL(kind=rc_kind) ::   xdu(0:NI+1,0:NJ+1),ydu(0:NI+1,0:NJ+1),xdv(0:NI+1,0:NJ+1),ydv(0:NI+1,0:NJ+1)
 REAL(kind=rc_kind) :: fconst,phi0,cosphi0,sinphi0,dthet,dtheta,dphi,dep,y
 
 INTEGER i,j,k,jmid,iseed 

 DLinv= 1.d0/DL 
 phi0= phi0deg*PI/180.d0  !     phi0 is the central lat, dphi,dtheta grid spacing(angle)          
 dphi = dy/(apr*AL) 

 !--------------------------------------------------
 ! INITIALIZATION OF THE GRID
 !

 ! HORIZONTAL GRID VARIABLES

 DO  j=0,NJ+1 
   DO  i=0,NI+1 
     xdu(i,j)= dx/LEN 
     ydu(i,j)= 0.d0 
     xdv(i,j)= 0.d0 
     ydv(i,j)= dy/LEN !- used for constant dy
     ! ydv(i,j)= dyM(j)/LEN 
   ENDDO
 ENDDO

 DO j=0,NJ+1 
   DO i=0,NI+1 
     J2d(i,j)=  xdu(i,j)*ydv(i,j) - xdv(i,j)*ydu(i,j) 
     ux(i,j) =  ydv(i,j)/J2d(i,j) 
     vx(i,j) = -ydu(i,j)/J2d(i,j) 
     uy(i,j) = -xdv(i,j)/J2d(i,j) 
     vy(i,j) =  xdu(i,j)/J2d(i,j) 
     g11(i,j)=  ux(i,j)*ux(i,j) + uy(i,j)*uy(i,j) 
     g12(i,j)=  ux(i,j)*vx(i,j) + uy(i,j)*vy(i,j) 
     g22(i,j)=  vx(i,j)*vx(i,j) + vy(i,j)*vy(i,j) 
   ENDDO
 ENDDO

 !--------------------------------------------------
 ! INITIALIZATION OF THE VARIABLES
 !
 Tr(:,:,:,:,0) = 0d0
 uf = 0d0
 vf = 0d0
 wf = 0d0

 ufbce(:,:)= uf(NI,:,:) 
 ufbcw(:,:)= uf(0,:,:) 

 vfbcn(:,:)= vf(:,NJ,:) 
 vfbcs(:,:)= vf(:,0,:) 

 wfbcb(:,:) = 0.d0    ! at the sea bed, wf=0                                              

 !---------------------

 RETURN 
END SUBROUTINE ini_setup
