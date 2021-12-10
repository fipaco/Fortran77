c********************************************************************************
c*     Esta rutina devuelve "i" caracteres a la izquierda de la cadena "c".
c*     "c" = cadena de la cual se van a extraer los
c*     "i" = caracteres a devolver desde el primero hasta el "i"esimo
c*     "n" = longitud de caracteres validos de la cadena 
c********************************************************************************

      character*(*) function izquierda (c,n,i)
      implicit none
      character c*(*)
      integer*4 n, i
      izquierda = c (1:i)
      return
      end
      
c********************************************************************************
c*     Esta rutina devuelve "i" caracteres a la derecha de la cadena "c".
c*     A la derecha de los caracteres validos. La dimension de la cadena "c"
c*     pude ser mayor a la cantidad de caracteres validos (distintos de blanco).
c*     "c" = cadena de la cual se van a extraer los
c*     "i" = caracteres a devolver mas a la derecha
c*     "n" = longitud de caracteres validos de la cadena 
c********************************************************************************

      character*(*) function derecha (c,n,i)
      implicit none
      character c*(*)
      integer*4 n, n1, i
      n1 = n
      if (n1 .eq. 0) n1 = len_trim (c)
      derecha = c (n1-i+1:n1)
      return
      end
      
c********************************************************************************
c*     Esta rutina devuelve "n" caracteres a partir de la posicion "i" de la
c*     cadena "c" contados desde la izquierda de la cadena.
c*     "c" = cadena de la cual se van a extraer los "n" caracteres a devolver
c*     "i" = indice del caracter desde el cual se inicia la extraccion
c*     "n" = nro. de caracteres a devolver
c********************************************************************************

      character*(*) function substrin (c,i,n)
      implicit none
      character c*(*)
      integer*4 n, i
      substrin = c (i:i+n-1)
      return
      end
      
