      PROGRAM SORTERER
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (N=100) 
      REAL, DIMENSION(N,2) :: inArr
      REAL, DIMENSION(N,2) :: ouArr
      
      CALL ARRAYMAKER(N,inArr,ouArr)
      print*, inArr(1,1) 
      
      END PROGRAM
      SUBROUTINE ARRAYMAKER(N,inArr,ouArr)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL, DIMENSION(N,2) :: inArr
      REAL, DIMENSION(N,2) :: ouArr

C Generate a random seed
      CALL init_random_seed()
C Random initial state:
      DO J=1,2
      DO I=1,N
       iy=1
       CALL RANDOM_NUMBER(x)
       IF(x.LT.0.5)iy=-1
       CALL RANDOM_NUMBER(x)
       inArr(I,J)=10*iy*x
      END DO
      END DO

      END SUBROUTINE

      SUBROUTINE init_random_seed()
      INTEGER :: i, n, clock
      INTEGER, DIMENSION(:), ALLOCATABLE :: seed
      
      CALL RANDOM_SEED(size = n)
      ALLOCATE(seed(n))
      
      CALL SYSTEM_CLOCK(COUNT=clock)
      
      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      CALL RANDOM_SEED(PUT = seed)

      DEALLOCATE(seed)
      END SUBROUTINE
