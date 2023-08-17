      PROGRAM wver                          
      real, dimension(3, 4) :: matrix 
      real, dimension(4) :: min_values
      integer :: dim

      DO J=1,4
      DO I=1,3
       matrix(I,J)=I*J
      END DO
      END DO

      DO I=1,3
       PRINT*, (matrix(I,J),J=1,4)
      END DO

      dim = 2  ! Along the first dimension

      min_values = MINVAL(matrix, dim)
      PRINT*, min_values

      END program      
