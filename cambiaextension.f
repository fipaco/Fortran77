c********************************************************************************
c*     Esta rutina obtiene el nombre del archivo de entrada, le quita la ruta o
c*     directorio si lo tiene y le cambia la extension por la indicada para asi
c*     formar el nombre del archivo de salida.
c********************************************************************************

      character*(*) function cambiaextension (fent, ext)
      implicit none

      character fent*(*), ext*4, fsal*60
      integer*4 i, k

      k = len_trim (fent)
      i = k
      do while (i .gt. 0 .and. fent(i:i) .ne. '/')
         i = i - 1
      enddo 
      fsal = fent (i+1:k)
      k = k - i
      i = k
      do while (i .gt. 0 .and. fsal(i:i) .ne. '.')
         i = i - 1
      enddo
      if (i .eq. 0) then
         fsal = fsal(1:k) // ext
      else
         fsal = fsal(1:i-1) // ext
      endif
      cambiaextension = fsal
      return
      end
      
