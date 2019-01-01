!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!  
MODULE monter_carlo
IMPLICIT NONE
      INTEGER (KIND=8),PARAMETER :: nx=10,ny=10,nz=10,nT=21,n_average=5000
      REAL (KIND=8),PARAMETER :: Tmin=2.0,Tmax=6.0
      INTEGER (KIND=8) :: iT,i,j,k,i_loop,im,ip,jm,jp,km,kp
      REAL    (KIND=8) :: T,delT,rdn_mtp,real_na
      REAL    (KIND=8) :: E_tmp1,E_tmp2,energy,energy_2,E_av,E_2_av,Cv,S_tmp,M,M_av

      REAL    (KIND=8),DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: S

      CONTAINS

!!!=======================================================================================
!!!=======================================================================================
      SUBROUTINE average_thermal()
      IMPLICIT NONE

      CALL ini_rdm_number()

      real_na=real(nx*ny*nz)
                  
      S(:,:,:)=0.
      S(1:nx,1:ny,1:nz)=1.        
  
      E_av=0.  ; E_2_av=0. ; M_av=0.
      
      DO i_loop=1,n_average
            CALL value_thermal()        
            E_av=E_av+energy
            E_2_av=E_2_av+energy_2
            M_av=M_av+M                  
      END DO

      E_av=E_av/real(n_average)
      E_2_av=E_2_av/real(n_average)
      M_av=M_av/real(n_average)
      Cv = real_na*(E_2_av-E_av**2.)/(T**2.)

      END SUBROUTINE average_thermal

!!!=======================================================================================
!!!=======================================================================================
      SUBROUTINE ini_rdm_number()
      IMPLICIT NONE
      
      INTEGER (KIND=8),DIMENSION(8) :: time
      INTEGER (KIND=8),DIMENSION(50) :: seed

      CALL DATE_AND_TIME(values=time)     ! Get the current time
      seed(1) = time(4)*(360000*time(5) + 6000*time(6) + 100*time(7) + time(8))
      CALL RANDOM_SEED(PUT=seed)

      END SUBROUTINE ini_rdm_number

!!!=======================================================================================
!!!=======================================================================================
      SUBROUTINE value_thermal()
      IMPLICIT NONE
          
      energy=0.

      DO i=1,nx
      ip=i+1
      im=i-1
      DO j=1,ny
      jp=j+1
      jm=j-1
      DO k=1,nz
      kp=k+1
      km=k-1

      E_tmp1=-S(i,j,k)*(S(ip,j,k)+S(im,j,k)+S(i,jp,k)+S(i,jm,k)+S(i,j,kp)+S(i,j,km))                  
      E_tmp2=-E_tmp1
      S_tmp=S(i,j,k)

            CALL random_number(rdn_mtp)
            IF (exp(-(E_tmp2-E_tmp1)/T) > rdn_mtp) THEN
                  S(i,j,k)=-S_tmp
                  E_tmp1=E_tmp2
            END IF
            energy=energy+E_tmp1
            M=M+S(i,j,k)
      END DO
      END DO
      END DO

      energy=energy/(2.*real_na)
      energy_2=energy**2.
      M=abs(M)/real_na

      END SUBROUTINE value_thermal 

END MODULE monter_carlo

!!!=======================================================================================
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%! 
      PROGRAM main_ising
      USE monter_carlo
      IMPLICIT NONE

      CALL system('rm CPU_result.dat')
!!!---------------------------------------------------------------------

      IF (nT==1) THEN
            delT=0.
      ELSE
            delT=(Tmax-Tmin)/real(nT-1)
      END IF

      OPEN(unit=20,file='CPU_result.dat')

      DO iT=1,nT
            WRITE(*,*)'iT = ', iT                        
            T=Tmin+delT*real(iT-1)
            CALL average_thermal()
            WRITE(20,*) T,E_av,M_av,Cv
            !WRITE(*,*) T,E_av,M_av,Cv
      END DO 

      CLOSE(20)

!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      END PROGRAM main_ising
      
