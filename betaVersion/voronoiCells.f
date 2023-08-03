      PROGRAM VORONOICELLS
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (iter=100, N=100, dT=0.35) !iterations and grid size
      REAL, DIMENSION(N,2) :: posit
      REAL, DIMENSION(N,2) :: speed
      CHARACTER(12) frmt
      CHARACTER(16) files(iter)

C Create data files to save the iterations
      CALL SYSTEM('mkdir library')
      CALL dataFiles(iter,files)

C Random initial state:
      DO J=1,2
      DO I=1,N
       CALL RANDOM_NUMBER(x)
       posit(I,J)=20*x
      END DO
      END DO
C Store the initial state
      OPEN(UNIT=1,FILE='library/data0000')
      DO I=1,N
       WRITE(1,*) (posit(I,J),J=1,2)
      END DO
      CLOSE(1)
 
C Move the cells iter-times
      DO K=1,iter
      OPEN(UNIT=1,FILE=files(K))

      DO J=1,2
      DO I=1,N
       iy=1
       CALL RANDOM_NUMBER(x)
       IF(X.lt.0.5)iy=-1
       CALL RANDOM_NUMBER(x)
       posit(I,J)=posit(I,J) + iy*x*dT
      END DO
      END DO
      
      DO I=1,N
       WRITE(1,*) (posit(I,J),J=1,2)
      END DO



      END DO
      CLOSE(1)
      


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
