***********************************************************************
*     Rutina para eliminar todos los caracteres en blanco a la izquierda
*     de una cadena de caracteres. Se desplaza hacia la izquierda los
*     caracteres diferentes de blanco. Se devuelve una cadena de longitud
*     igual a la cadena dada. En la variable "n" se devuelve la cantidad
*     de caracteres validos.
***********************************************************************

      character*(*) function ltrim (a, n)
      implicit none
      character a*(*)
      integer*4 n, i
      if (n .eq. 0) return
      i = 0
      do while (i .le. n .and. a(i:i) .eq. ' ')
         i = i + 1
      enddo
      ltrim (1:n) = a(i:n) // a(1:i-1)
      n = n - i + 1
      return
      end
