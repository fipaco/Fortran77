*!      include "/F/G77/Rutinas/itrim.f"
*!      include "/F/G77/Rutinas/lonstrin.f"
*!      program prueba
*!      implicit none
*!      character a*16, separacantidad*16
*!      integer*4 i, n
*!      real*4    x
*!      
*!      n = 34567654
*!      x = 123324.45
*!      write (a,'(i10)') n
*!      i = len_trim(a)
*!      a = separacantidad (a,i,1)
*!      print*,n,'    ', a(1:i)
*!      write (a,'(f12.2)') x
*!      i = len_trim(a)
*!      a = separacantidad (a,i,1)
*!      print*,x,'    ', a(1:i)
*!      stop
*!      end

************************************************************************
*     Rutina para separar una cantidad dada en cifras de tres en tres
*     con puntos y/o comas respetando los decimales si los hubiera
*     a = cadena (cantidad) de entrada a separar. debe venir por lo menos
*         con un espacio en blanco al principio de la cadena.
*     n = nro de caracteres de la cadena de entrada "a"
*     l = indica cual es el caracter a  utilizar como separador
*         1 = usa el punto ("."); 2 = usa la coma (",")
************************************************************************

      character*(*) function separacantidad (a,n,l)
      implicit none
      character a*(*), b*16, c*1, itrim*16
      integer*4 n, l, i, j, k

*      print*,'Sub.Separa.Cantidad: ',n,' ',a
      n = len_trim (a)
      if (n .eq. 0) return
      do i = 1, 16
         b (i:i) = ' ' ! inicializa el area auxiliar
      enddo
      j = 17                ! fija el apuntador del area auxiliar
      i = index (a,'.')     ! busca si hay punto decimal
      if (i .gt. 0) then
         j = 16 - (n-i)     ! calcula posicion area uxiliar
         b (j:16) = a (i:n) ! mueve los decimales (con el punto incluido)
         n = i - 1          ! actualiza el nro de caracteres que quedan en "a"
         if (b(j:j) .eq. '.' .and. l .eq. 1) b (j:j) = ',' ! cambia el separador de decimales
*         print*,j,' ',b
      endif
      c = '.' ! separador de cifras miles, millones, etc
      if (l .eq. 2) c = ',' ! se cambia el separador
      k = 0
      do i = n, 1, -1
         if (a(i:i) .eq. ' ') exit
         if (k .eq. 3) then
            k = 0
            j = j - 1
            b (j:j) = c
         endif
         j = j - 1
         k = k + 1
         b (j:j) = a (i:i)
*         print*,j,' ',b
      enddo
      n = 16
      separacantidad = itrim (b,n,0)
      return
      end
