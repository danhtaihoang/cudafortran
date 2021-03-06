MODULE ex3_mod
CONTAINS
ATTRIBUTES(GLOBAL) SUBROUTINE sumation(a,b,d)
IMPLICIT NONE
INTEGER,INTENT(in) :: a(:,:)
INTEGER,INTENT(in) :: b(:,:)
INTEGER,INTENT(out):: d(:,:)
INTEGER :: i,j,nx,ny

i = blockDim%x*(blockIdx%x-1)+threadIdx%x
j = blockDim%y*(blockIdx%y-1)+threadIdx%y
nx=size(a,1) ; ny=size(a,2)
IF ((i<=nx).and.(j<=ny)) THEN
	d(i,j) = a(i,j)+ b(i,j)
END IF
END SUBROUTINE sumation
END MODULE ex3_mod 

!!==========================================
PROGRAM ex3_presentation
USE cudafor
USE ex3_mod
IMPLICIT NONE
INTEGER, PARAMETER :: nx=30000000,ny=4
INTEGER :: a(nx,ny),b(nx,ny),d(nx,ny)
INTEGER, device :: a_d(nx,ny),b_d(nx,ny),d_d(nx,ny)
TYPE(dim3) :: gridDim,blockDim
a=10
b=20
blockDim=dim3(512,2,1)
gridDim=dim3(ceiling(real(nx)/blockDim%x),ceiling(real(ny)/blockDim%y),1)

a_d=a ; b_d=b
CALL sumation <<<gridDim,blockDim>>>(a_d,b_d,d_d)
d=d_d
IF(all(d==30)) THEN
      WRITE(* ,*)'**** OK ****'
ELSE
      WRITE(* ,*)'Program Failed'
END IF
END PROGRAM ex3_presentation


