********************************************************************************      
*     Esta rutina redondea un valor a una cifra superior o igual en terminos de
*     5 centesimas. Ejemplos: 1.83 --> 1.85     1.85 --> 1.85     1.86 --> 1.90
********************************************************************************      
      real*4 function redondea (valor)
      implicit none
      real*4 valor, dif
      redondea = real(int(valor*10.0))*0.1
      dif = valor-redondea
      if (dif .eq. 0.0) return
      if (dif .lt. 0.05) then
         redondea = redondea + 0.05
      else 
         redondea = redondea + 0.10
      endif
      return
      end
