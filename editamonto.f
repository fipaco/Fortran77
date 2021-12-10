C*******************************************************************************
C*    Rutina para editar un monto separandolo en unidades de tres en tres antes
C*    de imprimirse.
C*******************************************************************************

      character*16 function editamonto (monto)  
      implicit none

      CHARACTER A*16, E*16
      real*4    monto
      WRITE (A,'(I16.3)') int (monto*100.0)
c*      write (a,'(f16.2)') monto
c*         12 345 678 901 234 56      
c*    A = '99 999 999 999 999 99'
c*         1234567890123456      
c*    E = '9.999.999.999,99'
      E = '                '
      E (11:16) = A (12:14) // ',' // A (15:16)
      IF (A(9:11) .NE. '   ' ) E (7:10) = A (9:11) // '.'
      IF (A(6:08) .NE. '   ' ) E (3:06) = A (6:08) // '.'
      IF (A(3:05) .NE. '   ' ) E (1:02) = A (5:05) // '.'
      editamonto = e
      RETURN
      END
      
