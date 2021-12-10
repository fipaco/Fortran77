c********************************************************************************
c*     Esta rutina y/o funcion expande y/o convierte a caracter un campo dado en
c*     forma de numero (binario). El campo "CC" viene en binario (formato nume-
c*     rico) y puede ser: INTEGER*2, INTEGER*4, REAL*4, REAL*8 o LOGICAL*4. Debe
c*     venir en una variable CHARACTER a la cual previamente se le asigno el
c*     numero en una instruccion EQUIVALENCE. Esto se da generalmente en cadenas
c*     tipo caracter las cuales contienen campos numericos (en binario) por medio
c*     de hacer redefiniciones de campos (variables) mediante un EQUIVALENCE. Es
c*     el caso de las declaraciones de registro de archivos.
c*     "cc"  = campo caracter con contenido binario de un numero
c*     "tpo" = tipo de numero y/o variable (I*2,I*4,R*4,R*8,L*4) para numeros
c*             Integer's, Real's y Logical's
c*     "lc"  = longitud de expansion (salida) en caracteres ascii (visibles).
c*     "n"   = Si la variable a convertir es INTEGER, entonces sera el numero de
c*             digitos deseados en la salida. (Ej. valor 12 , lc=6 y n=4, a la
c*             salida devuleve "  0012".
c*             Si la variable a convertir es REAL, entonces "n" representa el
c*             numero de decimales deseados en la salida.
c*             Forma parte de "lc" mas un lugar para el punto decimal. Si se
c*             especifica "lc=10" y "n=2", la salida sera de 7 lugares para los
c*             enteros, un punto decimal y dos lugares para los decimales (F10.2)
c*             Con "n=0" se obtendra el numero sin decimales y con un punto
c*             decimal al final (a la derecha).
c*     "sw1" = 1 Indica que debe eliminarse el punto decimal. Para lograr
c*             esto, se desplaza hacia la derecha un espacio la cantidad conver-
c*             tida a caracter, previo de eliminar el punto decimal. Los decimales
c*             quedan en su lugar. La cantidad se entrega alineada a la derecha.
c*     "sw2" = 1 Indica que si la cantidad convertida da como resultado un
c*             cero (si es entera y/o 0.00 si es real (decimal)), entonces se
c*             debe devolver solo blancos, es decir, se eliminan los ceros de las
c*             cantidades que estan en cero (0 y/o 0.00). Esto es para efectos
c*             de visualizacion en los reportes.
c*            
c*             Se devuelve el numero convertido a caracter ascii (visible).
c********************************************************************************
 
      character*(*) function expandecampo (cc, tpo, lc, n, sw1, sw2)
      implicit none
      character cc*(*), tpo*4
      integer*4 lc, n, i, sw1, sw2
      
      character c2*2, c4*4, c8*8, aux*20, fmt*8, b*20
c*      character eliminapunto*20, ceroporblanco*20
      integer*2 i2
      integer*4 i4
      real*4    r4
      real*8    r8
      logical*4 l4
      equivalence (c2,i2), (c4,i4,r4,l4), (c8,r8)
c*             12345678901234567890      
      data b /'                    '/      
      
c*      Debido a que cuando se graba con formato Im.n, si el campo da como
c*      resultado cero (0) y "n" es cero (0), el campo queda en blanco, pues
c*      automaticamente elimna el cero y el usuario especificó que no se eliminen
c*      campos ceros por blanco ("CAMPO-CERO-POR-BLANCO: no") o no lo especifico
c*      y el defecto es "NO", entonces hay que cambiar el valor de la variable 
c*      "n" por "1" para evitar que sea cambiado el "0" por un blanco.

      aux = b
      if (tpo(1:1) .eq. 'I'  .or.  tpo(1:1) .eq. 'L') then
         if (sw2 .ne. 1  .and.  n .eq. 0) n = 1
         if      (tpo .eq. 'I*2 ') then    ! INTEGER*2
            c2 = cc
            write (fmt,'("(I",i2.2,".",i2,")")') lc, n
            write (aux(1:lc),fmt) i2
         else if (tpo .eq. 'I*4 ') then    ! INTEGER*4
            c4 = cc
            write (fmt,'("(I",i2.2,".",i2,")")') lc, n
            write (aux(1:lc),fmt) i4
         else if (tpo .eq. 'L*4 ') then    ! LOGICAL*4
            c4 = cc
            write (fmt,'("(L",i2.2,")   ")') lc
            write (aux(1:lc),fmt) l4
         endif
c*         chequea y cambia cero por blanco
         if (sw2.eq.1.and.aux(1:lc).eq.b(1:lc-1)//'0')aux(lc:lc)=' '
         expandecampo = aux
         return
      endif
      
      write (fmt,'("(f",i2.2,".",i1,") ")') lc, n
      if (tpo .eq. 'R*4 ') then    ! REAL*4
         c4 = cc
         write (aux(1:lc),fmt) r4
      else if (tpo .eq. 'R*8 ') then    ! REAL*8
         c8 = cc
         write (aux(1:lc),fmt) r8
      endif

      if (sw1 .eq. 1) then  ! Elimina el pto. decimal
         i = index (aux,'.')
         if (i .eq. lc) then
            aux (1:lc) = ' ' // aux(1:i-1)
         else
            aux (1:lc) = ' ' // aux(1:i-1) // aux(i+1:lc)
         endif
      endif
c*      chequea y cambia cero por blanco
      if (sw2.eq.1.and.aux(1:lc).eq.b(1:lc-1)//'0')aux(lc:lc)=' '
      expandecampo = aux
c*      write (6,*) 'fmt:',fmt, ' Exp.',aux,' ',lc,' ',n
      return
      end

