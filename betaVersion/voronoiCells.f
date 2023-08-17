      PROGRAM VORONOICELLS
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (iter=100, N=1000000) !iterations, #cells
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
C Our first particle will always be on (-mxmn,-mxmn)
      posit(1,1)=-amxmn
      posit(1,2)=-amxmn
      
C Sort our data before storing it
      CALL SORTERER(N,amxmn,posit)
      
C Store the initial state
      OPEN(UNIT=1,FILE='library/data0000')
      DO I=1,N
       WRITE(1,*) (posit(I,J),J=1,2)
      END DO
      CLOSE(1)

      END PROGRAM
C Now we need a subroutine to grab the positions
C and output an array with some very important info
C about all the possible segments

C Segment Information
C Starting point, Ending point, Size, Slope

      SUBROUTINE DELAUNAYFINDR(N,inArr,slpsz)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL, DIMENSION(N,2) :: inArr
      REAL, DIMENSION(N,N,2) :: slpsz
      STOP
      END SUBROUTINE

C This subroutine sorts matrix's rows according
C to the first element of each row
      
      SUBROUTINE SORTERER(N,amxmn,inArr)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL, DIMENSION(N,2) :: inArr, ouArr
      REAL, ALLOCATABLE :: column(:)
C This sorting algorithm is extremely slow one 
C reason is that we are not using intrinsic functions
C For this reason we will try a different approach,
      GOTO 3030 !Comment if you want to be slow
      L=0
      ouArr=2*amxmn
 2020 CONTINUE
      L=L+1
      hold1=2*amxmn
      DO I=1,N
      IF(inArr(L,1).EQ.amxmn)CYCLE
      IF(inArr(I,1).LT.hold1)THEN
      hold1=inArr(I,1)
      K=I
      END IF 
      END DO
      ouArr(L,1)=inArr(K,1)
      ouArr(L,2)=inArr(K,2)
      inArr(K,1)=2*amxmn
      IF(L.EQ.N)GOTO 2021
      GOTO 2020
 2021 CONTINUE
      inArr=ouArr
      IF(ANY(inArr == 2*amxmn))PRINT*, 'Theres a particle outside!'
 3030 CONTINUE

C Lets try a new sorting methodology
      L=0
      ouArr=2*amxmn
 3031 CONTINUE
      L=L+1
      ALLOCATE(column(N-2L+2))
      column=inArr(L:N+1-L,1)
      ouArr(L,1)=MAXVAL(column)
      ouArr(N+1-L,1)=MINVAL(column)
      DO I=1,N
      IF(inArr(I,1).EQ.ouArr(L,1))THEN
       ouArr(L,2)=inArr(I,2)
       EXIT
      ELSE IF(inArr(I,1).EQ.ouArr(N+1-L,1))THEN
       ouArr(N+1-L,2)=inArr(I,2)
       EXIT
       
      END IF
      
      END DO
      DEALLOCATE(column)
      GOTO 3031
      END SUBROUTINE
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
