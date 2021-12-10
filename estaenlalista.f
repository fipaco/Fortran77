************************************************************************
*     Rutina para determinar si una palabra dada (como strin) esta en una
*     lista de palabras dadas (como strin) en otra cadena de caracteres
*     c1 = contiene una palabra para buscar en "C2".
*     c2 = contiene una lista de palabras separadas por un caracter en blanco
*     n1,n2 = nro de caracteres de "c1" y "c2" respectivamente
*     Para que esta rutina funcione correctamente es necesario que las
*     palabras contenidas en la lista, esten separadas por un espacio en
*     blanco. La lista debe empezar y terminar con un caracter en blanco.
*     Cuando la palabra a buscar es de una longitud muy pequeña, existe la
*     posibilidad que haga coincidencia parcial con el contenido de alguna 
*     palabra en la lista. Para evitar que esto pueda pasar, se le agrega
*     un caracter en blanco al principio y al final a la palabra a buscar.
*     De esta forma se asegura una coincidencia exacta con alguna de las 
*     palabras en la lista.
************************************************************************

      logical*4 function estaenlalista (c1, c2, n1, n2)
      implicit none
      
      character c1*(*), c2*(*), c3*32
      integer*4 n1, n2, n3, i
      
      estaenlalista = .true.
*     se le agrega un espacio en blanco al principio y otro al final para asegurar
*     que la busqueda coincida exactamente con lo que se desea buscar
      c3 = ' '//c1(1:n1)// ' '
      n3 = n1 + 2
      i = index (c2(1:n2),c3(1:n3))
      if (i .gt. 0) return
      estaenlalista = .false.
      return
      end
      