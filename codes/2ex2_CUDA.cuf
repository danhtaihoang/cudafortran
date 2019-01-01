MODULE ex2_mod
CONTAINS
ATTRIBUTES(GLOBAL) SUBROUTINE sumation(a,b,d)
IMPLICIT NONE
INTEGER,INTENT(in) :: a(:)
INTEGER,INTENT(in) :: b(:)
INTEGER,INTENT(out):: d(:)
INTEGER :: i,n

i = blockDim%x*(blockIdx%x-1)+threadIdx%x
n=size(a)
IF (i<=n) THEN
	d(i) = a(i)+ b(i)
END IF
END SUBROUTINE sumation
END MODULE ex2_mod 

!!==========================================
PROGRAM ex2_presentation
USE cudafor
USE ex2_mod
IMPLICIT NONE
INTEGER, PARAMETER :: n = 5002
INTEGER :: a(n),b(n),d(n)
INTEGER, device :: a_d(n),b_d(n),d_d(n)
INTEGER :: tpb=1000
a = 10
b = 20
a_d=a ; b_d=b
CALL sumation <<<ceiling(real(n)/tpb),tpb>>>(a_d,b_d,d_d)
d=d_d
IF(all(d==30)) THEN
      WRITE(* ,*)'**** OK ****'
ELSE
      WRITE(* ,*)'Program Failed'
END IF
END PROGRAM ex2_presentation


