************************************************************************
*     Esta rutina averigua si el caracter dado en "C1" coincide con
*     alguno de los caracteres suministrados en "C2".
************************************************************************

      logical*4 function esalguno (c1, c2)
      implicit none
      
      character c1*1, c2*(*)
      integer*4 i, n2
      
      esalguno = .true.
      i = index (c2,c1)
      if (i .gt. 0) return
      esalguno = .false.
      return
      end