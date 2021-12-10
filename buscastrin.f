c********************************************************************************
c*     Rutina para buscar una cadena de caracteres (c2) en otra cadena de
c*     caracteres (c1).
c*     Si se encuentra se devuelven los numeros de las columnas de inicio y
c*     fin en las variables "i1" e "i2" (desde-hasta).
c*     "c1" = cadena de caracteres en la cual se efectua la busqueda.
c*     "c2" = cadena de caracteres que se busca en "c1".
c*     "n1" y "n2" = nro. de caracteres de las cadenas "c1" y "c2". Si vienen en
c*     cero, seran determinados haciendo uso de la funcion len_trim.
c*     "i1" e "i2" = nros. de las columnas de inicio y fin de la cadena "c2" 
c*     encontrada en "c1". A la entrada, deben venir en cero para que la busqueda
c*     comience desde el primer caracter de "c1", de lo contrario la busqueda
c*     comenzara desde el caracter "i2+1".
c*     "i" = nro. de la ocurrencia de "c2" en "c1" a buscar. Si no se encuentra
c*     se devuelve con el nro. de ocurrencias encontradas. Si esta variable
c*     viene en cero, entonces se cuentan todas las ocurrencias de "c2" en "c1".
c*     "j" = orden de busqueda. 1 = izquierda a derecha. -1 = derecha a izquierda
c********************************************************************************

      logical*4 function buscastrin (c1,c2,n1,n2,i1,i2,i,j)
      implicit none
      
      character c1*(*), c2*(*)
      integer*4 n1, n2, n11, n22, i1, i2, i, j, k
      integer*4 j1, j2, j3, ii
      
      buscastrin = .true.
      ii = i
      if (ii .eq. 0) ii = 99999
      n11 = n1 ! nro. de caracteres de "c1"
      n22 = n2 ! nro. de caracteres de "c2"
      if (n11 .eq. 0) n11 = len_trim (c1) ! cadena donde se busca la cadena c2
      if (n22 .eq. 0) n22 = len_trim (c2) ! cadena a buscar en la cadena c1
      if (j .eq. 1) then ! orden de busqueda. 1 = izquierda a derecha
         j1 = i2 + 1     !                   -1 = derecha a izquierda
         j2 = n11 - n22 + 1
         j3 = 1
      else
         j1 = n11 - n22 - i1 ! desde donde empezar a buscar
         j2 = 1              ! hasta donde buscar
         j3 = -1             ! incremento y/o decremento
      endif
      k = 0 ! contador de ocurrencias de "c2" en "c1"
      do i1 = j1, j2, j3
         i2 = i1 + n22 - 1
         if (c1(i1:i2) .eq. c2(1:n22)) then
            k = k + 1
            if (k .eq. ii) return  ! se encontro
         endif
      enddo
      i = k ! no se encontro. se actualiza con nro.de ocurrencias de "c2" en "c1"
      i1 = 0
      i2 = 0
      buscastrin = .false.
      return
      end
      
