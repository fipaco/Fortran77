c********************************************************************************
c*     Esta rutina tiene por objetivo presentar un mensaje en la pantalla.
c*     Se indica el ancho maximo de la linea de mensaje.
c********************************************************************************

      subroutine mtramensaje2 (msg, n)
      implicit none
      character msg*(*), aa*1024, b*80, itrim*1024, ajustalinea*80
      integer*4 n, nn, i1, i2, i, k, k1, npa
      logical*4 buscachar
      
60    format (90a1)         
      nn = len_trim (msg)   ! longitud de la cadena
      aa = itrim (msg,nn,1) ! elimina espacios innecesarios (mas de 1)
      k = 0
      i1 = 1
      do while (i1 .le. nn)
         i2 = i1 + n - 1 ! n = ancho a presentar
         b = aa (i1:i2)  ! toma una porcion del mensaje
         npa = 0 ! nro. de puntos y aparte
         if (buscachar(b,'#',n,k,1,1)) then ! punto y aparte
            k1 = k
            do while (k .gt. 0 .and. b(k:k) .eq. '#')
               npa = npa + 1
               k = k + 1
            enddo
            do i = k1, n
               b (i:i) = ' '
            enddo
            k = k1 - 1
         else
            if (aa(i2+1:i2+1) .eq. ' ') then ! termina con palabra completa
               k = n
            else  ! palabra cortada. hay que buscar final palabra completa
               if (buscachar(b,' ',n,k,1,-1)) then
                  k = k - 1
               else
                  k = 0
               endif
            endif
         endif
         i1 = i1 + k + 1 + npa ! fija indice proxima extraccion
         if (k .lt. n .and. b(k:k) .ne. '.') b = ajustalinea (b,n,k)
         write (6,60) ' ',(b(i:i),i=1,n)
c         write (6,60) ' ',(b(i:i),i=1,k)
         if (npa .gt. 1) then
            do k = 1, npa
               write (6,60)
            enddo
         endif
      enddo
      return
      end
      