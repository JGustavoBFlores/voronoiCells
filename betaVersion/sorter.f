      SUBROUTINE ARRAYMAKER(N,walls,ouArr)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL, DIMENSION(N,2) :: ouArr
      DO J=1,2
      DO I=1,N
       iy=1
       CALL RANDOM_NUMBER(x)
       IF(x.LT.0.5)iy=-1
       CALL RANDOM_NUMBER(x)
       ouArr(I,J)=walls*iy*x
      END DO
      END DO
      END SUBROUTINE
