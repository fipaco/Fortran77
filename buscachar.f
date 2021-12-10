c********************************************************************************
c*     Rutina para buscar en "a", de longitud "n", el caracter dado en "c".
c*     Se devuelve la posicion en la variable "i". Se busca la enesima ocurrencia
c*     del caracter indicado por la variable "j". Si viene en cero, se asume 1.
c*     Si no se consigue el enesimo caracter, se devuelve .false. y la variable
c*     "i" tendrá el numero de caracteres (indicado en "c") encontrados en "a".
c*     Normalmente se busca hacia adelante (de izquierda a derecha) con "m" mayor
c*     o igual a cero. Si "m" es menor que cero (negativo) entonces la busqueda
c*     es inversa de derecha a izquierda
c********************************************************************************

      logical*4 function buscachar (a, c, n, i, j, m)
      implicit none
      character a*(*), c*1
      integer*4 n, i, j, m, k, i1, i2, i3
      buscachar = .true.
      if (m .ge. 0) then ! orden de busqueda. 1 = izq. -> derecha
         i1 = 1          !                   -1 = der. -> izquierda
         i2 = n
         i3 = 1
      else
         i1 = n
         i2 = 1
         i3 = -1
      endif
      k = 0 ! contador de caracteres encontrados
      if (j .lt. 1) j = 1
      do i = i1, i2, i3
         if (a(i:i) .eq. c) then
            k = k + 1
            if (j .eq. k) return
         endif
      enddo
      i = k
      buscachar = .false.
      return
      end
      
