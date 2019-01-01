MODULE ex1_mod
CONTAINS
SUBROUTINE sumation(a,b,d)
IMPLICIT NONE
INTEGER,INTENT(in) :: a(:)
INTEGER,INTENT(in) :: b(:)
INTEGER,INTENT(out):: d(:)
INTEGER :: i,n

n = size(a)
DO i = 1, n
d(i) = a(i)+b(i)
END DO
END SUBROUTINE sumation      
END MODULE ex1_mod      

!!==========================================
PROGRAM ex1_presentation
USE ex1_mod
IMPLICIT NONE
INTEGER, PARAMETER :: n=1000
INTEGER :: a(n),b(n),d(n)
a = 10
b = 20

call sumation(a,b,d)

IF(all(d==30)) THEN
      WRITE(* ,*)'**** OK ****'
ELSE
      WRITE(* ,*)'Program Failed'
END IF
END PROGRAM ex1_presentation


