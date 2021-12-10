c********************************************************************************
c*     Esta rutina presenta un mensaje en pantalla y solicita el nombre para un
c*     archivo.
c*     La variable "sw" tiene un doble uso. A la entrada viene con .true. para 
c*     indicar que el archivo a solicitar debe existir. Si viene con .false.
c*     entonces el archivo a solicitar no debe existir (archivo nuevo a crear).
c*     A la salida, tendra .true. para indicar que la accion solicitada se ha
c*     cumplido (es cierta) y tendra .false. para indicar que no se ha cumplido.
c********************************************************************************

      subroutine indicararchivo (file, msg, sw)
      implicit none
      character file*(*), msg*(*), resp*1
      integer*4 n
      logical*4 sw, swn

      swn = sw
1     continue
      write (6,*)
      write (6,*) msg
      read  (5,'(a)') file
      if (file(1:3) .eq. 'fin'  .or.
     *    file(1:3) .eq. 'FIN'  .or.
     *    file(1:3) .eq. '   ') then
         sw = .false.
         return
      endif
      n = len_trim (file)
      call mayusculas (file,n)
      inquire (file=file, exist=sw)
      if (swn) then     ! el archivo debe existir
         if (sw) return ! y existe
         write (6,*) 'Archivo no existe.'
      else                     ! el archivo no debe existir
         if (.not. sw) return  ! y no existe
         write (6,*) 'Archivo ya existe.'
         write (6,*) 'Desea sobre-escribirlo ? (s/n)'
         read  (5,'(a)') resp
         if (resp .eq. 's' .or. resp .eq. 'S') then
            sw = .not. sw
            return
         endif
      endif
      go to 1
      end
      
