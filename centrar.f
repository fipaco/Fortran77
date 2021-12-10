c********************************************************************************
c*     Esta rutina centra una cadena de caracteres en si misma insertando
c*     caracteres en blanco a la izquierda y a la derecha.
c*     "k" = indica la cantidad de caracteres que debe tener la salida y la 
c*     cadena "c" debe estar centrada en esa cantidad de caracteres.
c*     "n" = es la cantidad de caracteres de "c".
c*     Si "j" < 0  = alinea la cadena a la izquierda
c*     Si "j" = 0  = centra la cadena de acuerdo con la longitud dada en "k"
c*     si "j" > 0  = alinea la cadena a la derecha de acuerdo con la longitud "k"
c********************************************************************************
      
      character*(*) function centrar (c,n,k,j)
      implicit none
      character c*(*), blanco*128, b1(128)*1
      integer*4 n, i, j, k
      equivalence (blanco,b1(1))
      data (b1(i),i=1,128)/128*' '/
      centrar = c
      if (n .ge. k) return ! no hay nada que centrar
      if (j) 10,20,30
10    continue
      centrar = c(1:n)//blanco(1:k-n) ! alinea a la izquierda
      return      
20    continue      
      i = (k-n)/2
      centrar = blanco(1:i)//c(1:n)//blanco(1:i)
      return
30    continue
      centrar = blanco(1:k-n)//c(1:n) ! alinea a la derecha
      return
      end
      
