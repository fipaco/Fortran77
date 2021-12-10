c**********************************************************************
c*    Rutina para validar que una fecha sea valida
c*    "f" es una fecha (caracter de 6) con formato "ddmmaa"
c**********************************************************************

      logical*4 function fechavalida (f)
      implicit none

      character f*6
      logical*4 esnumerico
      integer*4 dia, mes, ano

      fechavalida = .false.
      if (.not. esnumerico(f)) return
      read (f,'(3i2)') dia, mes, ano
      if (dia .lt. 01  .or.
     *    dia .gt. 31  .or.
     *    mes .lt. 01  .or.
     *    mes .gt. 12) return
      if (dia .gt. 28) then
         if (mes .eq. 02) then
            if (ano/4*4 .ne. ano) return
            if (dia .gt. 29) return
         else
         if (dia .gt. 30) then
            if (mes .eq. 04  .or.
     *          mes .eq. 06  .or.
     *          mes .eq. 09  .or.
     *          mes .eq. 11) return
            end if
         end if
      end if

      fechavalida = .true.
      return
      end
      
