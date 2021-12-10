c********************************************************************************
c*     Rutina para buscar una palabra cualquiera (cadena/strin) de caracteres en
c*     la cadena "a". Se localiza columna de inicio y columna de terminacion. Se
c*     devuelve col.Inicio, col.Fin (desde-hasta) y nro. de caracteres (longitud).
c*     La busqueda se hace en el strin "a" desde la columna "i2+1".
c*     Si se consigue algo, se devuelve en "i1" la columna de inicio y en "i2"
c*     la columna de terminacion y en "nc" la cantidad de caracteres.
c*     A la entrada, "i1" e "i2" tienen los nros. de columna de la ultima palabra
c*     encontrada en el strin "a". Para que la busqueda comience desde la primera
c*     columna del strin "a", "i2" debe tener el valor cero a la entrada.
c*     La variable "nc" devuelve el nro. de caracteres de la palabra encontrada.
c*     La variable "na" contiene el nro.de caracteres de informacion en "a"
c*     Si la cadena (string) conseguida comienza con una comilla (") o un apostrofe
c*     ('), entonces se busca el final de la cadena hasta conseguir la pareja de
c*     ese caracter. Toda esa cadena se considera una palabra.
c********************************************************************************

      logical*4 function buscapal (a,na,i1,i2,nc)
      implicit none
      character a*(*), c*1
      integer*4 i1, i2, nc, na
         
      c = ' '
      buscapal = .false.
      if (i2 .lt. 0) i2 = 0
      if (i2 .ge. na) return
      i1 = i2 + 1
      do while (i1 .le. na)
         if (a(i1:i1) .ne. c) goto 10
         i1 = i1 + 1
      enddo
      i1 = 0
      i2 = 0
      nc = 0
      return
      
10    continue
      if (a(i1:i1) .eq. '"' .or. a(i1:i1) .eq. "'") c = a(i1:i1)
      i2 = i1
      do while (i2 .le. na)
         i2 = i2 + 1
         if (a(i2:i2) .eq. c) go to 20
      enddo
        
20    continue
      if (c .eq. ' ') i2 = i2 - 1
      nc = i2 - i1 + 1
      buscapal = .true.
      return
      end
      
