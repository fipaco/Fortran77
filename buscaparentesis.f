c********************************************************************************
c*     Esta rutina busca en una cadena de caracteres el par de parentesis (abre y
c*     cierra parentesis) y devuelve las posiciones en i1, i2 del par indicado
c*     en la variable "k" (primer par, o segundo par, etc). "k" = indica el nivel
c*     y/o grupo de abre/cierra parentesis. Ejemplo:
c*     12345678901234567890123456789012345678901234567890
c*     ((...(.)..)..(...))
c*     primer-par = 1-19   segundo-par = 2-11/14-18    tercer-par = 6-8 
c*     "a" = cadena en la cual se realiza la busqueda
c*     "n" = nro. de caracteres de la cadena "a"
c*     "i1" e "i2" = Indices del par de parentesis en "a"
c*     "k" = nro. de nivel y/o grupo de parentesis solicitado
c********************************************************************************

      logical*4 function buscaparentesis (a, n, i1, i2, k)
      implicit none
      
      character a*(*)
      integer*4 n, i1, i2, i, j, k, l
      
      buscaparentesis = .false.
      i1 = 0
      i2 = 0
      i  = 1
      j  = 0
      l  = 0
      do while (i .le. n)
         if (a(i:i) .eq. '(') then
            l = l + 1
            if (l .eq. k) i1 = i
         else
         if (a(i:i) .eq. ')') then
            if (l .eq. k) then
               i2 = i
               i = n + 1
            else
               l = l - 1
            endif
         endif
         endif
         i = i + 1
      enddo
      if (i2-i1 .gt. 0) buscaparentesis = .true.
      return
      end
      
