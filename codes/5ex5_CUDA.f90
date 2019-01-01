MODULE ex_share
IMPLICIT NONE
CONTAINS
ATTRIBUTES(GLOBAL) SUBROUTINE sumation(a,b,e)
IMPLICIT NONE
INTEGER, PARAMETER :: n=1024
INTEGER,INTENT(in) :: a(n)
INTEGER,INTENT(in) :: b(n)
INTEGER,INTENT(out):: e(n)
INTEGER :: i,j
INTEGER,shared :: d(n)
!INTEGER :: d(n)

i = threadIdx%x
d(i) = a(i)+ b(i)
CALL syncthreads()

IF (i/=n) THEN
    j=i+1
ELSE
    j=1
END IF

e(i) = d(i)+d(j)

END SUBROUTINE sumation
END MODULE ex_share 
!!==========================================
PROGRAM ex_presentation
USE cudafor
USE ex_share
IMPLICIT NONE
INTEGER, PARAMETER :: n = 1024
INTEGER :: a(n),b(n),e(n)
INTEGER, device :: a_d(n),b_d(n),e_d(n)

a = 10 ; b = 20
a_d=a ; b_d=b
CALL sumation <<<1,n>>>(a_d,b_d,e_d)
e=e_d
IF(all(e==60)) THEN
      WRITE(* ,*)'**** OK ****'
ELSE
      WRITE(* ,*)'Program Failed'
END IF
END PROGRAM ex_presentation




