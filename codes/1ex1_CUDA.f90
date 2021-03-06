MODULE ex1_mod
CONTAINS
ATTRIBUTES(GLOBAL) SUBROUTINE sumation(a,b,d)
IMPLICIT NONE
INTEGER,INTENT(in) :: a(:)
INTEGER,INTENT(in) :: b(:)
INTEGER,INTENT(out):: d(:)
INTEGER :: i

i = threadIdx%x
d(i) = a(i)+ b(i)
END SUBROUTINE sumation
END MODULE 

!!==========================================
PROGRAM ex1_presentation
USE cudafor
USE ex1_mod
IMPLICIT NONE
INTEGER, PARAMETER :: n = 1000
INTEGER :: a(n),b(n),d(n)
INTEGER, device :: a_d(n),b_d(n),d_d(n)
a = 10
b = 20
a_d=a ; b_d=b
CALL sumation <<<1,n>>>(a_d,b_d,d_d)
d=d_d
IF(all(d==30)) THEN
      WRITE(* ,*)'**** OK ****'
ELSE
      WRITE(* ,*)'Program Failed'
END IF
END PROGRAM ex1_presentation



