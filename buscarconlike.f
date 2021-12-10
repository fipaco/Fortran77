c***********************************************************************
c*    Esta rutina busca una coincidencia al principio, en el medio o al
c*    final de una cadena "c1" lo que se indica en la cadena a buscar
c*    "c2". La cadena "c2" puede venir con el caracter comodin "%" el
c*    cual indica cualquier cantidad de caracteres.
c*    Ejemplos: c2 = "%hola" : Cierto si "hola" esta al final de la 
c*                   cadena en la cual se busca ("c2" en "c1").
c*              c2 = "hola%" : Cierto si "hola esta al principio de la
c*                   cadena en la cual se busca ("c2" en "c1").
c*              c2 = "%hola%" : Cierto si hola esta en la cadena en
c*                   cualquier parte. Tambien se puede indicar sin el
c*                   comodin ("%").
c***********************************************************************

      logical*4 function buscarconlike (c1, c2, n1, n2)
      implicit none
      character c1*(*), c2*(*), c3*20, chrtran*20
      integer*4 n1, n2, n3, i1, i2, i3, i
      logical*4 buscastrin, swinicio, swfinal

      buscarconlike = .false.
      swinicio = .false.
      swfinal  = .false.
      c3 = c2
      n3 = n2
      i = index (c3,'%') ! busca el comodin "%"
      if (i .gt. 1 .and. i .lt. n3) then ! el comodin esta en el medio
         i1 = i - 1  ! debe empezar con los primeros caract. de "c2" y 
         i2 = n3-i-1 ! terminar con los ultimos caract. de "c2"
         if (c1(1:i1)     .eq. c3(1:i1)  .and.
     *       c1(n1-i2:n1) .eq. c3(n3-i2:n3)) buscarconlike = .true.
         return
      endif
      if (i .eq. 1) then ! el comodin esta al inicio, debe terminar con "c2"
         swfinal = .true.
         c3 = c3 (i+1:n3)
         n3 = n3 - 1
      endif
      i = index (c3,'%')
      if (i .eq. n3) then ! el comodin esta al final, debe empezar con "c2"
         swinicio = .true.
         c3 = c3 (1:i-1)
         n3 = n3 - 1
      endif
      if (swinicio .and. swfinal) then
         swinicio = .false.
         swfinal  = .false.
      endif
      i1 = 0
      i2 = 0
      i3 = 1 ! buscar la primera ocurrencia de "c3" en "c1"
      if (buscastrin(c1,c3,n1,n3,i1,i2,i3,1)) then
         if ((swinicio .and. i1 .gt. 1 ) .or.
     *       (swfinal  .and. i2 .lt. n1)) return
         buscarconlike = .true.
      endif
      return
      end

