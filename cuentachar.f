c********************************************************************************
c*     Rutina para contar cuantas veces se repite en la cadena "a", el caracter
c*     indicado en la variable "c".
*      n = nro de caracteres validos de la cadena "a" a ser examinada
c********************************************************************************

      integer*4 function cuentachar (a, c, n)
      implicit none
      character a*(*), c*1
      integer*4 n, i, n1
      cuentachar = 0
      n1 = n
      if (n1 .eq. 0) n1 = len_trim (a) ! lonstrin (a)
      do i = 1, n1
         if (a(i:i) .eq. c) cuentachar = cuentachar + 1
      enddo
      return
      end
