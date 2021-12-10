c********************************************************************************
c*     Rutina para verificar si un campo alfabetico tiene solo letras y espacios
c*     en blanco.
c********************************************************************************

      logical*4 function esalfabetico (c)
      implicit none
      
      character c*(*), letras*27
      integer*4 n, i, j
*                   123456789012345678901234567
      DATA letras /' ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

*      n = lonstrin (c)
      n = len_trim (c) ! long.de caracteres hasta primer blanco
      esalfabetico = .true.
      i = 1
      do while (i .le. n .and. index(letras,c(i:i)) .gt. 0)
         i = i + 1
      enddo
      if (i .le. n) esalfabetico = .false.
      return
      end
      
