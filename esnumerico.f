c*******************************************************************************
c*    Rutina para determinar si un campo caracter es numerico es decir, si solo
c*    contiene numeros (digitos)
c*******************************************************************************

      logical*4 function esnumerico (a)
      implicit none

      character a*(*), fmt*6
      integer*4 n
   	  real*4    num

      esnumerico = .true.
*      n = len (a)
*      write (fmt,'("(i",i2.2,") ")') n
      read (a,*,err=10) num
      return

10    continue
      esnumerico = .false.
      return
      end
      
