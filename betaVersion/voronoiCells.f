      PROGRAM VORONOICELLS
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (iter=100, N=100) !iterations, #cells
      PARAMETER (dT=0.2D0, amxmn=1.0d1)
      REAL, DIMENSION(N,2) :: posit
      CHARACTER(12) frmt
      CHARACTER(16) files(iter)

C Create data files to save the iterations
      CALL SYSTEM('mkdir library')
      CALL dataFiles(iter,files)

C Generate a random seed
      CALL init_random_seed()
C Generate a random starting state
      CALL ARRAYMAKER(N,amxmn,posit)
      PRINT*, posit(1,1)
C Our first particle will always be on (-mxmn,-mxmn)
      posit(1,1)=-amxmn
      posit(1,2)=-amxmn
C Store the initial state
      OPEN(UNIT=1,FILE='library/data0000')
      DO I=1,N
       WRITE(1,*) (posit(I,J),J=1,2)
      END DO
      CLOSE(1)
      
C Now we need a subroutine to grab the positions
C and output an array with some very important info
C about all the possible segments

C Segment Information
C Starting point, Ending point, Size, Slope
C :
      
      

      END PROGRAM

C This subroutine creates a randomized N*2 array  this is directed towards
C randomized positions in between a space that spans rectangularly from
C (-mxmn,-mxmn) to (mxmn,mxmn)
      SUBROUTINE ARRAYMAKER(N,amxmn,ouArr)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL, DIMENSION(N,2) :: ouArr
      DO J=1,2
      DO I=1,N
       iy=1
       CALL RANDOM_NUMBER(x)
       IF(x.LT.0.5)iy=-1
       CALL RANDOM_NUMBER(x)
       ouArr(I,J)=amxmn*iy*x
      END DO
      END DO
      END SUBROUTINE

C This subroutine creates N data files named data####
C  and moves them to a directory called library
      SUBROUTINE dataFiles(N,files)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER(16) files(N)
      OPEN(UNIT=2,FILE='listOfFiles')
      DO I=1,N
       WRITE(files(i),'(A,I4.4)') 'library/data',I
       WRITE(2,'(A)') files(I)
      END DO
      END SUBROUTINE

C This subroutine initializes a seed for the random_number
C statement calls, based on the local clock
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
