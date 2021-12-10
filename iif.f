***********************************************************************
*     Rutina para devolver un valor ENTERO en funcion del resultado de
*     evaluar una expresion logica (booleana).
*     sw = puede ser una expresion logica. Ejemplo: "3.eq.4" (sin comillas)
*          o tambien puede ser un valor logico (.true. o .false.)
*     v1,v2 = valores enteros a ser devueltos de acuerdo al resultado de
*          evaluar la expresion logica.
***********************************************************************
      
      integer*4 function iif (sw, v1, v2)
      implicit none
      logical*4 sw
      integer*4 v1, v2
      iif = v2
      if (sw) iif = v1
      return
      end

***********************************************************************
*     Rutina para devolver un valor REAL en funcion del resultado de
*     evaluar una expresion logica (booleana).
*     sw = puede ser una expresion logica. Ejemplo: "3.eq.4" (sin comillas)
*          o tambien puede ser un valor logico (.true. o .false.)
*     v1,v2 = valores reales a ser devueltos de acuerdo al resultado de
*          evaluar la expresion logica.
***********************************************************************
      
      real*4 function rif (sw, v1, v2)
      implicit none
      logical*4 sw
      real*4    v1, v2
      rif = v2
      if (sw) rif = v1
      return
      end

***********************************************************************
*     Rutina para devolver un valor LOGICO en funcion del resultado de
*     evaluar una expresion logica (booleana).
*     sw = puede ser una expresion logica. Ejemplo: "3.eq.4" (sin comillas)
*          o tambien puede ser un valor logico (.true. o .false.)
*     v1,v2 = valores logicos a ser devueltos de acuerdo al resultado de
*          evaluar la expresion logica.
***********************************************************************
      
      logical*4 function lif (sw, v1, v2)
      implicit none
      logical*4 sw
      logical*4 v1, v2
      lif = v2
      if (sw) lif = v1
      return
      end
      
***********************************************************************
*     Rutina para devolver un valor CHARACTER en funcion del resultado de
*     evaluar una expresion logica (booleana).
*     sw = puede ser una expresion logica. Ejemplo: "3.eq.4" (sin comillas)
*          o tambien puede ser un valor logico (.true. o .false.)
*     v1,v2 = cadenas de caracteres a ser devueltos de acuerdo al resultado
*          de evaluar la expresion logica.
***********************************************************************
      
      character*(*) function cif (sw, v1, v2)
      implicit none
      logical*4 sw
      character v1*(*), v2*(*)
      cif = v2
      if (sw) cif = v1
      return
      end
