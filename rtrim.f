c********************************************************************************
c*     Rutina para eliminar (???) todos los espacios a la derecha en una cadena
c*     de caracteres.
c*     En fortran no es posible generar una cadena de caracteres de longitud
c*     igual a la cantidad de caracteres con informacion. La longitud de la 
c*     cadena siempre sera igual a la longitud definida en la instruccion
c*     CHARACTER. Por lo tanto, se devuelve la cadena pasada a la funcion tal 
c*     como se paso y se indica la cantidad total de caracteres de la cadena
c*     DESDE la posicion uno (1) HASTA la posicion de la cadena mas a la derecha
c*     con informacion diferente de blanco. Todo esto equivale exactamente igual
c*     a la informacion devuelta por la FUNCION lonstrin
c*     Se devuelve en "n" la cantidad de caracteres validos (distintos de blanco)
c*     En este caso, esta rutina equivale exactamente a la rutina "LONSTRIN"
c********************************************************************************

      character*(*) function rtrim (a,n)
      implicit none
      character a*(*)
      integer*4 n
      n = len_trim (a) ! lonstrin()
      rtrim = a
      return
      end
      
