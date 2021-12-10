c********************************************************************************
c*     Rutina para cambiar (reemplazar) caracteres por otros en una cadena dada
c*     en la cadena "c1", se cambian los caracteres "c2" encontrados en "c1" por
c*     los caracteres indicados en "c3".
c*     Ejemplos        1234567890123456789012345678901234567890123
c*     1. Entrada..: ("Francisco es una persona jubilada de la UNA","abc","*$#",43,3,3)
c*        Resultado: ("Fr*n#is#o es un* person* ju$il*d* de l* UNA")
c*
c*     "c1", "c2" y "c3" = cadenas de caracteres. La primera es la cadena 
c*     en la cual se efectuaran los cambios. La segunda es la cadena que indica
c*     los caracteres a buscar en la cadena "c1" para ser reemplazados por
c*     los caracteres indicados en "c3". La longitud de "c2" y "c3"
c*     debe ser igual de lo contrario se toma la menor.
c*     "n1" = Longitud de la cadena "c1". Si viene en cero, se determina, de 
c*     lo contrario se usa el valor dado para reemplazar solo la cantidad de
c*     caracteres indicados por esta variable.
c*     "n2" y "n3" son las longitudes de las cadenas "c2" y "c3"
c*     Si "n3" = 1, se reemplazan todos los caracteres indicados en "c2" por uno
c*                  solo indicado en "c3".
c*     Si "n3" = 0, se eliminan todos los caracteres indicados en "c2".
c********************************************************************************

      character*(*) function chartran (c1,c2,c3,n1,n2,n3)
      implicit none
      
      character c1*(*), c2*(*), c3*(*)
      integer*4 n1, n2, n3, i, j, k
      logical*4 sw
      
      chartran = c1
      if (n1 .eq. 0) n1 = len_trim(c1)
      if (n1 .eq. 0) return
      if (n2 .eq. 0) n2 = len_trim(c2)
      if (n2 .eq. 0  .and.  c2(1:1) .eq. ' ') n2 = 1 ! esto con "n3=0" elimina todos los caracteres en blanco
      if (n3 .eq. 0) go to 10 ! va a eliminar los caracteres indicados en "c2"
      sw = .false.
      if (n3 .eq. 1) sw = .true. ! los caracteres indicados en "c2" seran cambiados por uno solo indicado en "c3"
      do i = 1, n2
         k = i
         if (sw) k = 1
         do j = 1, n1
            if (c2(i:i) .eq. c1(j:j)) chartran(j:j) = c3(k:k)
         enddo
      enddo
      return

c*    La cadena "c3" esta vacia y/o solo tiene un caracter (en blanco).
c*    Se quiere eliminar los caracteres indicados en "c2"

10    continue
      k = 0
      do 20 j = 1, n1
         do i = 1, n2
            if (c2(i:i) .eq. c1(j:j)) goto 20
         enddo
         k = k + 1 ! solo se copian los caracteres no encontrados en "c2"
         chartran (k:k) = c1 (j:j)
20    continue
      n1 = k  ! se actualiza el nro de caracteres de "n1"
      return
      end
