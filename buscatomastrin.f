************************************************************************
*     Rutina para averiguar si existe una cadena de caracteres delimitada
*     por comillas y/o apostrofes y si es asi devuelve las posiciones de
*     inicio-fin (desde-hasta) en las variables i1,i2
*     c1 = cadena de caracteres de entrada sobre la cual se realiza la
*          busqueda
*     i1,i2 = posicion desde,hasta de la cadena localizada en "c1".
************************************************************************

      logical*4 function buscatomastrin (c1, i1, i2)
      implicit none
      
      character c1*(*), comi*1, apos*1, c*1
      integer*4 i1, i2
      
      buscatomastrin = .false.
      comi = '"'
      apos = "'"
      i1 = index (c1,apos) ! busca el apostrofe
      i2 = index (c1,comi) ! busca la comilla
      if (i1 .gt. 0 .and. i2 .gt. 0) then ! se encontraron los dos caracteres (comillas y apostrofes)
         if (i1 .lt. i2) then             ! se averigua cual caracter esta primero en la cadena
            c = apos                      ! esta primero el apostrofe
         else
            c = comi                      ! esta primero la comilla
            i1 = i2
         endif
      else                                ! solo se encontro un caracter
         if (i1 .gt. 0) then              ! se encontro el apostrofe
            c = apos
         else                             ! se encontro la comilla
            c = comi
            i1 = i2
         endif
      endif
      c1 (i1:i1) = ' '  ! lo quita para que no lo encuentre en esa posicion
      i2 = index (c1,c) ! busca la pareja del primer caracter encontrado
      c1(i1:i1) = c     ! lo vuelve a colocar para dejar todo igual
      if (i1 .gt. 0 .and. i2 .gt. i1) buscatomastrin = .true.
      return
      end
      