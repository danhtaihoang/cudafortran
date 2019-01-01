MODULE ex4_mod
CONTAINS
SUBROUTINE sumation(a,b,d)
IMPLICIT NONE
INTEGER,INTENT(in) :: a(:,:)
INTEGER,INTENT(in) :: b(:,:)
INTEGER,INTENT(out):: d(:,:)
INTEGER :: i,j,nx,ny

nx = size(a,1) ; ny = size(a,2)
DO i=1,nx
DO j=1,ny
d(i,j) = a(i,j)+b(i,j)
END DO
END DO

END SUBROUTINE sumation      
END MODULE ex4_mod      

!!==========================================
PROGRAM ex4_presentation
USE ex4_mod
IMPLICIT NONE
INTEGER, PARAMETER :: nx=30000000, ny=4
INTEGER :: a(nx,ny),b(nx,ny),d(nx,ny)
a = 10
b = 20

call sumation(a,b,d)

IF(all(d==30)) THEN
      WRITE(* ,*)'**** OK ****'
ELSE
      WRITE(* ,*)'Program Failed'
END IF
END PROGRAM ex4_presentation


