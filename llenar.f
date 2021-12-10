*******************************************************************************
*     Rutina para llenar (inicializar) una variable caracter con un caracter dado
*     v = variable que se desea llenar (inicializar)
*     c = caracter con el que se desea llenar (inicializar) la variable
*******************************************************************************

      subroutine llenar (v,c)
      implicit none
      character v*(*), c*1
      integer*4 i
      do i = 1, len (v)
         v (i:i) = c
      enddo
      return
      end
      
*******************************************************************************
*     Rutina para repetir una cadena de caracteres "c" tantas veces como se
*     indique en la variable "n" formando asi una cadena mas grande.
*     c = cadena de caracteres a ser repetidos.
*     n = nro de veces que se desea repetir la cadena "c".
*     la longitud declarada en el programa que llama (invoca) a esta funcion debe
*     estar de acuerdo con la longitud esperada en la cadena que se va a formar.
*     esto debe ser asi: (minimo) long.de.repetir = n*nc  ::  nc = longitud de la
*     cadena de entrada "c"
*******************************************************************************

      character*(*) function repetir (c,n)
      implicit none
      character c*(*)
      integer*4 n, i, nc, j1, j2
      nc = len(c)
      j2 = 0
      do i = 1, n
         j1 = j2 + 1
         j2 = j2 + nc
         repetir (j1:j2) = c(1:nc)
      enddo
      return
      end
      