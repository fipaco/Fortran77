************************************************************************
*     Esta rutina averigua si alguno de los caracteres de "C1" coincide
*     con alguno de los caracteres de "C2", es decir, si esta contenido
*     en la segunda cadena.
************************************************************************

      logical*4 function tienealguno (c1, c2)
      implicit none
      
      character c1*(*), c2*(*)
      integer*4 i, j, n1
      
      tienealguno = .true.
      n1 = len(c1)
      do i = 1, n1
         j = index (c2,c1(i:i))
         if (j .gt. 0) return
      enddo
      tienealguno = .false.
      return
      end
      