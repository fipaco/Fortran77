c********************************************************************************
c*     Esta rutina convierte una cadena caracter que solo contiene numeros y/o
c*     digitos a un numero real (numerico) en memoria.
c*     se devolvera cero (0) si la cadena esta vacia y/o tiene cualquier caracter
c*     distinto de digitos (numeros)
c********************************************************************************
      
      real*4 function chartonum (a)
      implicit none
      character a*(*)
      chartonum = 0.
      read (a,*,err=88) chartonum
88    return
      end
      
