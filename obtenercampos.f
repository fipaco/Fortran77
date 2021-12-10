c********************************************************************************
c*     Esta rutina examina la cadena de caracteres dada (strin) y obtiene cada
c*     una de sus partes utilizando el caracater (",") o ("-") como separador de
c*     campos. La coma denota campo a campo (unicos). El guion denota un rango de
c*     campos (varios en secuencia).
c*     "c" = cadena de caracteres dada
c*     "v" = vector de campos. max.100. La cantidad y/o numero de campos se
c*           guarda en la posicion cero del vector (v(0)=nro.de campos en "v")
c*     "n" = nro. de caracteres en la cadena "c"
c*     Ejemplo: c = "4,1,8-10,12-15,34,20-15,10,8"  (puede haber repetidos)
c*     El archivo de salida tendra todos estos campos en el orden indicado.
c*     La coma (",") separa un campo de otro.
c*     El guion ("-") denota un rango de campos (desde-hasta).
c*     "r" Esta variable es usada para indicar si el rango debe expandirse gene-
c*     rando una secuencia o por el contrario no debe hacerlo. 1 = si, 0 = no.
c*     Cuando "r" = 0 hace el efecto de no tomar en cuenta el guion como rango. 
c*     En este caso se cambia el guion por una coma.
c*     "t" = nro. maximo de campos hasta el cual se puede expandir un rango.
c*     Si "c" viene con "ALL" o "TODOS", entonces se desarrolla una secuencia
c*     desde 1 hasta el nro.max.de campos (variable "t") siempre que "t" > 0
c*     Si el rango en "c" viene asi. "1-END" o "4-FIN" 0 "2-FINAL" 0 "5-ULTIMO"
c*     Entonces se desarrola la secuencia desde el primer nro. del rango hasta
c*     el nro.max.de campos (variable "t") siempre que "t" > 0 de lo contrario,
c*     el primer nro.del rango se multiplica por (-1) para que el programa que
c*     llama desarrolle esta secuencia ya que en esta rutina no se conoce el nro.
c*     maximo de campos en este momento.
c*     swerror = .true. para indicar que hubo un error en los campos
c*     swerror = .false. no hubo error en el proceso de obtener los campos
c********************************************************************************

      subroutine obtenercampos (c,v,n,r,t,swerror)
      implicit none
      
      character c*(*), chartran*80
      integer*4 v(0:100), i, j, k, m, n, r, t
      integer*4 nrocampo, incremento
      logical*4 swerror, swrango

      swerror = .true.
      if (n .eq. 0) n = len_trim (c)
      if (n .eq. 0) return
      swrango = .false.
      if (r .eq. 0) c = chartran (c,'-',',',n,1,1)
      j = 1 ! apuntador inicial de un campo
      k = 0 ! contador de campos
      v (0) = 0
      if (c(1:n) .eq. 'TODOS'  .or.  c(1:n) .eq. 'ALL') then
         if (t .le. 0) return
         j = t
         if (j .gt. 100) j = 100
         do i = 1, j
            v (i) = i
         enddo
         v (0) = j
         swerror = .false.
         return
      endif
      if (c(n:n) .eq. ','  .or.  c(n:n) .eq. '-') then
         c(n:n) = ' '
         n = n - 1
      endif
      do i = 1, n
         if (c(i:i) .eq. ','  .or.  c(i:i) .eq. '-') then
            read (c(j:i-1),*,err=88) nrocampo
            if (swrango) then
               swrango = .false.
               incremento = 1
               if (nrocampo .lt. v(k)) incremento = -1
               do m = v(k)+incremento, nrocampo, incremento
                  k = k + 1
                  v (k) = m
                  if (k .eq. 100) go to 77
               enddo
            else
               k = k + 1
               v (k) = nrocampo
               if (k .eq. 100) go to 77
            endif
            j = i + 1
            if (c(i:i) .eq. '-') swrango = .true.
         endif
      enddo
      if (j .gt. n) go to 77
      if (swrango .and. (c(j:i-1) .eq. 'FIN'   .or.
     *                   c(j:i-1) .eq. 'END'   .or.
     *                   c(j:i-1) .eq. 'FINAL' .or.
     *                   c(j:i-1) .eq. 'ULTIMO')) then
         if (t .le. 0) then
            v (k) = v (k) * (-1)
            go to 77
         endif
         if (t .le. v(k)) go to 77
         j = t
         if (j .gt. 100) j = 100
         do i = v(k)+1, t
            k = k + 1
            v (k) = i
         enddo
         go to 77
      endif
      read (c(j:i-1),*,err=88) nrocampo
      if (swrango) then
         incremento = 1
         if (nrocampo .lt. v(k)) incremento = -1
         do m = v(k)+incremento, nrocampo, incremento
            k = k + 1
            v (k) = m
            if (k .eq. 100) go to 77
         enddo
      else
         k = k + 1
         v (k) = nrocampo
      endif
77    continue
      v (0) = k
      swerror = .false.
88    return
      end
      
