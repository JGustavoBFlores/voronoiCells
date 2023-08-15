      PROGRAM VORONOICELLS
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (iter=100, N=100, dT=0.20) !iterations and grid size
      REAL, DIMENSION(N,2) :: posit
      REAL, DIMENSION(N,2) :: speed
      CHARACTER(12) frmt
      CHARACTER(16) files(iter)

C Create data files to save the iterations
      CALL SYSTEM('mkdir library')
      CALL dataFiles(iter,files)

C Generate a random seed
      CALL init_random_seed()
C Random initial state:
      DO J=1,2
      DO I=1,N
       iy=1
       CALL RANDOM_NUMBER(x)
       IF(x.LT.0.5)iy=-1
       CALL RANDOM_NUMBER(x)
       posit(I,J)=10*iy*x
      END DO
      END DO
C Our first particle will always be on (-10,-10)
      posit(1,1)=-10.0D0
      posit(1,2)=-10.0D0
C Store the initial state
      OPEN(UNIT=1,FILE='library/data0000')
      DO I=1,N
       WRITE(1,*) (posit(I,J),J=1,2)
      END DO
      CLOSE(1)
C Move the cells iter-times
      
      DO K=1,iter
      OPEN(UNIT=1,FILE=files(K))

C This block moves the particles over on a random direction
C     DO J=1,2
C     DO I=1,N
C      iy=1
C      CALL RANDOM_NUMBER(x)
C      IF(X.lt.0.5)iy=-1
C      CALL RANDOM_NUMBER(x)
C      posit(I,J)=posit(I,J) + iy*x*dT    !Random movement
C     END DO
C     END DO

C Lets now try moving a particle from quad3 towards the quad1
C     CALL RANDOM_NUMBER(x)
C     IF(X.lt.0.5)iy=-1
C     CALL RANDOM_NUMBER(x)
C     posit(1,1)=posit(1,1)+(Cos(x*0.175)-Sin(iy*x*0.175))*dT
C     posit(1,2)=posit(1,2)+(Cos(x*0.175)+Sin(iy*x*0.175))*dT
C     DO I=1,N
C      WRITE(1,*) (posit(I,J),J=1,2)
C     END DO

      CLOSE(1)
      END DO
      


      END PROGRAM

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
