c********************************************************************************
c*     Rutina para determinar la longitud de registro de un archivo secuencial.
c*     Se examinan los primeros 20 registros para seleccionar como longitud
c*     la mayor debido a que hay archivos de datos secuenciales que tienen
c*     uno o mas registros de cabecera con longitud menor a los registros de
c*     datos o detalle.
c*     Se optimiza considerando que si se consiguen 5 registros consecutivos de
c*     igual longitud, se da por terminada la busqueda.
c*     Si la longitud de registro fuera diferente entre si de la mayoria de los
c*     registros de un archivo (caso de un programa fuente), entonces la rutina
c*     devuelve la longitud de registro mayor encontrada en los primeros veinte
c*     (20) registros del archivo.
c********************************************************************************

      integer*4 function lonreg (filea)
      implicit none
      
      character filea*(*), a*1
      integer*4 cont, nreg, i, nc, lonant, ios
      
      open (1, file=filea, status='old', access='direct', recl=1,
     *         form='unformatted')
     
      lonreg = 0
      lonant = 0
      cont   = 0
      nreg   = 0
      nc     = 0
      i      = 0
      do while (cont .lt. 20) ! nro.reg.max.a examinar
1        continue
         nreg = nreg + 1
         read (1,err=20,iostat=ios,rec=nreg) a
c*10       format (a1)
         if (ios .ne. 0) go to 20
         i = i + 1
         if (i .gt. 500) go to 25   
         if (ichar(a) .ne. 13) go to 1  ! CR (Car-Return)
         if (lonreg .lt. i) lonreg = i - 1
         if (lonant .eq. lonreg) nc = nc + 1
         lonant = lonreg
         i = -1
         cont = cont + 1
c*         write (6,*) ' Char=',ichar(a), ' cont=',cont, ' nreg=',nreg,
c*     *               ' lonreg=',lonreg, ' nc=',nc
         if (nc .eq. 5) exit !go to 20  ! 5 reg.consecutivos de igual longitud
      enddo

20    continue
      close (1)
      return
25    continue
      close (1)
      print *, 'Long.reg.arch.ent. >= 500. Falta "CR". Revise!'
      stop      
      end
      
