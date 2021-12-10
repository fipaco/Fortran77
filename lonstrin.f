c********************************************************************************
c*     Rutina para determinar la longitud de una cadena de caracteres (strin)
c*     Se recorre hacia atras la cadena de caracteres hasta conseguir el primer
c*     caracter diferente de blanco (" ") y diferente del caracter nulo (char(0))
c*
c*     Esta rutina ha quedado en desuso por haber conseguido que el compilador
c*     provee de esta funcion (LEN_STRIM) que hace lo mismo. (06/04/2020).
c*
c********************************************************************************

      integer*4 function lonstrin (a)
      implicit none
      character a*(*)
c*      integer*4 i
c* 
c*      i = len (a) ! esta funcion devuelve la longitud definida en "CHARACTER"
c*      do while (i.gt.0 .and. (a(i:i) .eq. ' ' .or. a(i:i) .eq. char(0)))
c*         i = i - 1
c*      enddo
c*      lonstrin = i
c*
c*     Durante mucho tiempo, hice uso de esta funcion para determinar el nro de caracteres
c*     validos a utilizar en las variables de tipo CHARACTER.
c*     Recientemente (marzo-2020) por cosas del azar consultando algunas documentaciones a
c*     traves de iternet, descubri la funcion "LEN_TRIM()" (que no esta en alguno de los manuales
c*     de fortran) y decidi probarla. Con satisfaccion pude observar que me entrega exactamente
c*     el mismo resultado de mi funcion "LONSTRIN". Decidi hacer el cambio aqui y asi conservar
c*     el funcionamiento y compatibilidad de mis programas que hacen uso de la funcion "LONSTRIN"
c*     y hago el cambio porque asumo que usar una funcion provista por el compilador FORTRAN
c*     siempre sera mejor y mas rapida que cualquiera que yo pueda hacer.
c*     Francisco Iglesias P. (04/04/2020).

      lonstrin = len_trim (a)  ! entrega el mismo valor del codigo anterior (en comentarios)
      
c*      print *, 'Lonstrin: ', lonstrin,' *',a(1:lonstrin),'*'
      return
      end
      
