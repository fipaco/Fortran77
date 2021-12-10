c********************************************************************************
c*     Esta rutina tiene por objetivo presentar un mensaje en la pantalla ence-
c*     rrandolo en un marco de asteriscos ("*").
c*     Se indica el ancho del cuadro en la variable "n" y la unidad y/o archivo
c*     de salida en la variable "na". si esta no se indica o viene en cero (0)
c*     se usa la unidad 6.
c********************************************************************************

      subroutine mtramensaje (msg, n, na)
      implicit none
      character msg*(*), aa*1024, b*80, blanco*80, b1(80)*1
      character repetir*80, itrim*1024, ajustalinea*80
      integer*4 n, na, nn, i1, i2, i, k, k1, npa, cuentachar
      logical*4 buscachar, sw
      equivalence (blanco,b1(1))
      data (b1(i),i=1,80) /80*' '/
      
      if (na .le. 0) na = 6 ! archivo de salida
      if (n .gt. 80) n = 80 ! limite del ancho a ajustar
60    format (90a1)         
      write (na,60) (' ',i=1,(80-n)/2),('*',i=1,n+6) ! centra el mensaje
      write (na,60) (' ',i=1,(80-n)/2),'*',(' ',i=1,n+4),'*'
      nn = len_trim (msg)   ! longitud de la cadena
      aa = itrim (msg,nn,1) ! elimina espacios innecesarios (mas de 1)

      i1 = 1
      do while (i1 .le. nn)
         i2 = i1 + n - 1     ! n = ancho a presentar
         b = aa (i1:i2)      ! toma una porcion del mensaje
         k = i2-i1+1         ! nro de caracteres de la porcion tomada
         b = itrim (b,k,1)   ! elimino espacios a la izquierda y derecha (e intermedios mas de uno si los hay)
*         npa = 0             ! nro. de puntos y aparte
!        averiguo si hay puntos y aparte en la porcion de texto tomada ("#")         
         npa = cuentachar (b,'#',k)        ! cuento los punto y aparte que hay
         if (npa .gt. 0) then              ! hay puntos y aparte
            sw = buscachar (b,'#',n,k,1,1) ! esto para que "k" apunte al primer "#" (si es que lo hay)
            i1 = i1 + k + (npa-1)          ! fijo el indice de la proxima extraccion
         else
            if (aa(i2+1:i2+1) .ne. ' ') then ! termina con palabra cortada
               if (buscachar(b,' ',n,k,1,-1)) then ! busco el primer espacio hacia atras
                  k = k - 1  ! fijo el apuntador al caracter anterior al espacio encontrado
               else
                  k = n      ! si no hay entonces es una sola palabra de ancho igual a "n"
               endif
            endif
            i1 = i1 + k + 1  ! fija indice proxima extraccion del area del mensaje de entrada (parametro "aa")
         endif
         if (k .le. n .and. b(k:k) .ne. '#') then ! si no termina con "#", hay que ajustar
            b = ajustalinea (b,n,k)
         else
            do i = k, n
               b (i:i) = ' '    ! aqui limpio los "#" que indican salto de linea (punto y aparte)
            enddo
            k = k - 1           ! descuento el caracter "#"
            if (b(1:1) .eq. '$' .and. b(k:k) .eq. '$') then
               b = b(2:k-1)
               k = k - 2
               i = (n-k)/2
               b = blanco(1:i) // b(1:k) // blanco(1:i) ! aqui centro el mensaje
            endif
         endif
         write (na,60) (' ',i=1,(80-n)/2),              ! aqui imprimo el emnsaje (lo muestro) enmarcado
     *                  '*',' ',' ',(b(i:i),i=1,n),' ',' ','*'
         if (npa .gt. 1) then                           ! salto lineas segun los punto y aparte
            do k = 1, npa-1
               write (na,60) (' ',i=1,(80-n)/2),'*',(' ',i=1,n+4),'*'
            enddo
         endif
      enddo
      write (na,60) (' ',i=1,(80-n)/2),'*',(' ',i=1,n+4),'*'
      write (na,60) (' ',i=1,(80-n)/2),('*',i=1,n+6)
      return
      end
      