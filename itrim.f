c********************************************************************************
c*     Rutina para eliminar espacios en blanco intermedios entre palabras en
c*     una cadena de caracteres. Tambien se eliminan a la izquierda y derecha.
c*     El tamaño total de la cadena no se puede reducir. Siempre sera igual al
c*     tamaño definido en la instruccion CHARACTER.
c*     El proceso consiste en suprimir los blancos a la izquierda y a la derecha
c*     de la cadena de caracteres y suprimir todos aquellos espacios en blanco
c*     innecesarios (mas de uno) que esten intermedios entre las palabras.
c*     Se devuelve una cadena libre de espacios en blanco a la izquierda y a la
c*     derecha y el numero de caracteres validos en la variable "n".
c*     La variable "n" siempre tendra el numero de caracteres de la cadena "a",
c*     tanto a la entrada como a la salida (se actualiza su valor).
c*     Si la variable "k" tiene el valor cero (0), entonces se eliminan todos los
c*     espacios en blanco. De lo contrario, se sustituyen los espacios en blanco
c*     intermedios por la cantidad de espacios indicados en "k". La cadena devuelta
c*     puede resultar mayor en longitud a la cadena de entrada. El usuario debe
c*     asegurarse que la funcion tenga el espacio suficiente para recibir el resul-
c*     tado de la cadena final.
c********************************************************************************

      character*(*) function itrim (a,n,k)
      implicit none
      character a*(*)
      integer*4 n, i, j, k, l
      logical*4 sw
      
*      itrim = a ! asume que la salida es igual a la entrada (en principio)
      if (n .eq. 0) n = len_trim (a) ! nro de caracteres validos (no considera espacios al final)
      if (n .eq. 0) return
      j = 1
      do while (j .le. n  .and.  a(j:j) .eq. ' ') ! busca el primer caracter no blanco al principio de la cadena
         j = j + 1
      enddo
      sw = .false.
      i  = 0 ! contador de caracteres en la salida
      do j = j, n  ! recorre todos los caracteres validos en la cadena de entrada
         if (a(j:j) .eq. ' ') then
            sw = .true. ! consiguio espacios intermedios
         else
            if (sw .and. k .gt. 0) then ! se consiguio un espacio y se desea insertar "k" espacios
               do l = 1, k ! inserta y/o cambia un espacio por "k" espacios
                  i = i + 1
                  itrim (i:i) = ' '
               enddo
               sw = .false. ! ya no hay espacios intermedios
            endif
            i = i + 1 ! cuenta los caracteres que va moviendo a la salida
            itrim (i:i) = a (j:j) ! mueve el caracter de la entrada a la salida
         endif
      enddo
*      if (i .lt. n) then
*         do j = i+1, n
*            itrim (j:j) = ' ' ! blanquea los caracteres que quedaron al final
*         enddo
*      endif
      n = i ! actualiza el valor de la cantidad de caracteres validos en la cadena (los que quedaron)
      return
      end
      
