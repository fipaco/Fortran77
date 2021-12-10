      program px2
      implicit none
      integer*4 lrd, lc1, lrx, determinalreg
      lrd = 330
      lc1 = 14
      lrx = determinalreg (lrd, lc1)
      write (6,*) lrx
      stop
      end

c********************************************************************************
c*     Esta rutina calcula la longitud del registro fisico de la estructura de
c*     datos en funcion de la longitud del registro de datos y la constante 1024.
c*     El objetivo es obtener una longitud cercana a 1K (1024) por encima de 
c*     tal manera que se puedan almacenar un numero fijo y entero de registros de
c*     datos en el registro fisico de la estructura. Se toma en cuenta que los
c*     primeros 10 bytes del registro se reservan para hacer la cadena de
c*     registros en la estructura. (nro.reg.siguiente y anterior).
c********************************************************************************

      integer*4 function determinalreg (lrd, lc1)
      implicit none
      integer*4 i1, i2, k1, k2, lrd, lc1
c*      nrs = nro.reg.sig. (integer*4)
c*      nra = nro.reg.ant. (integer*4)
c*      nr  = nro.registros (integer*2) 
      i1 = 1024 - 10  ! 10 = nrs+nra+nr
      i2 = i1 / lrd
      if (i2*lrd .lt. i1) i2 = i2 + 1 ! asegura 1024 y algo mas 
10    continue
      k1 = i2*lrd
      k2 = k1 / lc1  ! nro. de claves por reg.   72
      if (k2*lc1 .lt. k1) i2 = i2 + 1
      k1 = i2*lrd
      k2 = i2*lrd/lc1
      if (k2*lc1 .lt. k1) then
         i2 = i2 + 1
         if (i2*lrd .lt. 2038) go to 10
      endif
      determinalreg = i2*lrd + 10
      return
      end
      
