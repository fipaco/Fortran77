c********************************************************************************
c*     Esta rutina convierte un numero entero a caracter.
c*     "k" = Indica la cantidad de caracteres deseados en la salida. Si esta en
c*     cero, entonces se eliminan todos los espacios en blanco y solo se devuelve
c*     los digitos (caracteres) que tenga el numero y "k" se actualiza con el
c*     numero de caracteres que ocupe el numero convertido en caracter.
c********************************************************************************
      
      character*(*) function numtochar (n,k)
      implicit none
      integer*4 n, k, i
      character a*10
      write (a,'(i10)') n
c*      numtochar = a
      if (k .gt. 0) then
         numtochar(1:k) = a (10-k+1:)
      else
         i = 1
         do while (a(i:i) .eq. ' ')
            i = i + 1
         enddo
         numtochar = a(i:)
         k = 10 - i + 1
      endif
      return
      end
