c********************************************************************************
c*     Esta rutina ajusta la cadena dada en funcion del ancho dado moviendo las
c*     palabras a la izquierda un caracter (o mas) hacia la derecha para que
c*     queden ajustadas a la derecha.
c*     "b" = cadena de caracteres a ser ajustados
c*     "n" = ancho al que hay que ajustar la cadena
c*     "k" = indice del ultimo caracter en "b" y/o nro. de caracteres en "b"
c********************************************************************************
      
      character*(*) function ajustalinea (b,n,k)
      implicit none
      character b*(*), bb*4
      integer*4 n, k, nc, i, i1, i2
      logical*4 sw, buscachar
      
      ajustalinea = b
      bb = '    '
      i2 = 1     ! espacios a insertar al mover las ultimas palabras de la cadena (asume 1 espacio)
      nc = n - k ! nro. de caracteres a ajustar
      if (nc .eq. 0) return
      sw = buscachar (b,' ',k,i1,99,-1) ! cuenta los caract.blancos entre palabras
      do while (nc .gt. i1*i2)          ! i1=cantidad de blancos en la cadena
         i2 = i2 + 1                    ! esto calcula la cantidad de espacios entre palabras que hay que insertar
      enddo
      if (k+i2 .gt. n) i2 = i2 - 1      ! si se pasa de la longitud especificada (k)
      if (i2 .eq. 0 ) return            ! no hay nada que hacer (espacios a insertar se ha reducido a cero)
      do i = nc, 1, -1
         if (buscachar (b,' ',k,i1,i,-1)) then ! busca el espacio "i" hacia atras
            b (i1:k+i2) = bb(1:i2) // b (i1:k) ! cambia 1 espacio por dos o tal vez mas
            k = k + i2
            if (k .ge. n) then
               ajustalinea = b
               return
            endif
            if (k+i2 .gt. n) i2 = i2 - 1
         endif
      enddo
      ajustalinea = b
      return
      end
      
