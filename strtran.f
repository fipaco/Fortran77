c********************************************************************************
c*     Rutina para buscar y reemplazar una cadena de caracteres (c2) en una
c*     cadena (c1) por otra cadena (c3)
c*     Se busca "c2" en "c1". Si se encuentra, se cambia por "c3".
c*     El cambio se realiza en todas las ocurrencias que se encuentren de 
c*     "c2" en "c1"
c*     La longitud de "strtran" debe ser mayor o igual a la longitud de "c1".
c*     c1,c2,c3 = cadenas de caracteres. en la que se busca, lo que se busca y por
c*                lo que se reemplaza.
c*     n1,n2,n3 = nro de caracteres de las cadenas c1,c2,c3
c*                si n3=0, se eliminan los caracteres "c2" conseguidos. se reduce
c*                la cadena "c1"
c*     n1 = siempre se actualiza y se devuelve con su valor.
c********************************************************************************

      character*(*) function strtran (c1,c2,c3,n11,n22,n33)
      implicit none
      
      character c1*(*), c2*(*), c3*(*)
      integer*4 i, j, n1, n2, n3, n11, n22, n33, i1, i2, i3 !, lonstrin
      logical*4 sw, buscastrin

      strtran = c1
      n1 = n11
      n2 = n22
      n3 = n33
      if (n1 .eq. 0) n1 = len_trim (c1)  !  lonstrin (c1)
      if (n1 .eq. 0) return ! cadena vacia no hay nada donde buscar
      if (n2 .eq. 0) n2 = len_trim (c2)  !  lonstrin (c2)
      if (n2 .eq. 0) return ! no hay nada que buscar para reemplazar
      if (n3 .eq. 0) n3 = len_trim (c3)  !  lonstrin (c3)
      if (n2 .eq. n3  .and.  c2(1:n2) .eq. c3(1:n3)) return ! no hay cambio
      i1 = 0
      i2 = 0
      i3 = 1  ! busca la primera ocurrencia desde i2
      do while (buscastrin(strtran,c2,n1,n2,i1,i2,i3,1))
         if (i1 .eq. 1) then ! se consiguio al principio
            if (n3 .eq. 0) then
               strtran = strtran(i2+1:n1)
            else
               strtran = c3(1:n3)//strtran(i2+1:n1)
            endif
         else
            if (n3 .eq. 0) then
               strtran = strtran(1:i1-1)//strtran(i2+1:n1)
            else
               strtran = strtran(1:i1-1)//c3(1:n3)//strtran(i2+1:n1)
            endif
         endif
         n1 = n1 - n2 + n3
         i2 = i1 + n3 - 1
      enddo
      n11 = n1
      return
      end
      
