************************************************************************
*     Rutina para hacer inicializacion de valores en arreglos enteros de
*     una sola dimension (vectores).
*     var = arreglo y/o vector de una dimension y de tipo INTEGER*4
*     c   = cadena de caracteres con los valores a ser cargados en el
*           arreglo. (Ver mas detalle mas abajo).
************************************************************************
*      include '/F/G77/Rutinas/lonstrin.f'
*      include '/F/G77/Rutinas/buscachar.f'
*      include '/F/G77/Rutinas/cuentachar.f'
*
*      program prueba
*      implicit none
*      integer*4 i, j, vc(100) /100*5/ 
*      integer*4 m(10,10) /100*100/, m1(100) ! /100*45/
*      character blanco*1024,blanko(1024)*1/1024*' '/
*      equivalence (blanco,blanko(1))
*      equivalence (m,m1)
*      real pi /3.1416/
*
*      print *,'vc=',(vc(i),i=1,20)
*      print *,'m1=',(m1(i),i=1,20)
*      call inicvar (m1,'61:70/10*999/')
*      do j = 1, 10
*         print *,'j=',j,' m=',(m(i,j),i=1,10)
*      enddo
*      call inicvar (m1,'1:10:1')
*      call inicvar (m1,'1:10:1//')
*      call inicvar(m1,'51:60:1/541,123,432,444,56,7,1111,123,765,1/')
*      print *, (m1(i),i=51,60)
*      print *,(vc(i),i=1,20)
*      call inicvar (vc,'1:10:1/10,9,8,7,6,5,4,3,2,1/')
*      print *, (vc(i),i=1,20)
*      call inicvar (vc,'/66,55,44,33,22,11/')
*      print *, (vc(i),i=1,20)
*      call inicvar (vc,'5:10/66,55,44,33,22,11/')
*      print *, (vc(i),i=1,20)
*      call inicvar (vc,'::2/222,333,444,1111/')
*      print *, (vc(i),i=1,20)
*      call inicvar (vc,'1:10:2/100,200,300,400,500,600/')
*      print *, (vc(i),i=1,20)
*      call inicvar (vc,'/10*1234/')
*      print *, (vc(i),i=1,20)
*      call inicvar (vc,'21:30/10*4532/')
*      print *, (vc(i),i=21,30)
*      call inicvar (m1,'/100*0/')
*      print *,(m1(i),i=1,20)
*      stop
*      end
*      
************************************************************************
*     Rutina para hacer inicializacion de valores en arreglos enteros de
*     una sola dimension (vectores).
*     var = arreglo y/o vector de una dimension y de tipo INTEGER*4
*     c   = cadena de caracteres con los valores a ser cargados en el
*           arreglo y/o vector. Los valores deben estar encerrados entre
*           dos caracteres "/" y separados por comas.
*           Adicinalmente se puede especificar un ciclo implicito de esta
*           forma: "desde:hasta:incremento/valores/". El caracter
*           ":" es obligatorio como separador del ciclo implicito y el
*           caracter "/" es obligatorio como separador entre el ciclo y
*           y los valores. Los valores deben estar de acuerdo con el ciclo
*           en cantidad y deben estar separados por comas. No es necesario
*           indicar el ciclo implicito pero si se hace, debe ser el primero
*           en la cadena. Se puede omitir cualquiera de las partes del
*           ciclo (desde:hasta:incremento), ejemplos "1:10:1;", ":10:1;",
*           "::;" = 1,nc,1 nc=nro de caracteres de la cadena. El "hasta"
*           siempre estara determinado por la cantidad de valores en la 
*           cadena. Los "/" determinan el rango desde-hasta donde se buscan
*           los valores para inicializar el arreglo (vector).
*           Tambien se pueden indicar los valores de esta forma "/n*valor/"
*           en donde "n"= es la cantidad de valores iguales y "valor" = es
*           el valor con el cual sera inicializado el arreglo y/o vector.
************************************************************************      

      subroutine inicvar (var, c)
      implicit none

      character c*(*)
      integer var(*), i, j, i1, i2, i3, n, nc, desde, hasta, incre
      integer*4 cuentachar, canti, valor
      logical*4 sw1, sw2, sw3, buscachar
      
      nc = len_trim(c) ! nro de caracteres validos en la cadena
      print *
      print *,c(1:nc)
      sw1 = buscachar (c,'/',nc,i1,1,1)   ! busca el primer "/" hacia adelante
      sw2 = buscachar (c,'/',nc,i2,1,-1)  ! busca el primer "/" hacia atras
      sw3 = .false. ! se asume varios valores separados por comas
      if (.not.(sw1.and.sw2) .or.  ! faltan los signos "/"
     *    (i2-i1) .le. 1) then     ! faltan los valores entre los "/"
         print *,'Error...(INICVAR) Faltan los valores'
         return
      endif
      i1 = i1 + 1 ! marca el inicio de los posibles valores
      i2 = i2 - 1 ! marca el final de los posibles valores
      i3 = i2 - i1 + 1 ! nro de caracteres entre i1 y i2 (incluidos estos)
*     hasta aqui tenemos la certeza de que hay valores encerrados entre los caracteres "/"
*     averiguamos si los valores vienen separados por comas o si fueron indicados asi: /10*123/
      canti = 0
      valor = 0
      n = cuentachar (c(i1:i2),',',i3) + 1 ! averigua si hay comas (",")
      if (n .le. 1) then ! no hay ninguna coma, entonces debe haber algo asi: /10*123/
         sw3 = .true. ! los valores estan expresados en cantidad y un solo valor
         sw1 = buscachar (c(1:i2),'*',i2,i,1,1) ! busca el asterisco "*"
*         print *,' i1=',i1,' i2=',i2,' i=',i
         read (c(i1:i-1),*) canti ! cantidad de valores iguales
         read (c(i+1:i2),*) valor ! valor con el que se va a inicializar
*         print *, ' canti=',canti,' valor=',valor
      endif
*     ahora falta determinar desde donde hasta donde se inicializa. tambien el incremento
*     se busca a ver si hay un ciclo implicito. se asume por defecto
      desde = 1 ! limite inferior del arreglo
      hasta = 1 ! limite superior del arreglo
      incre = 1 ! incremento
      if (canti .gt. 1) hasta = canti                   !   1234567890
      if (i1 .le. 5) go to 100 ! no hay ciclo implicito !  "1:9/..."  (minimo para un ciclo implicito) 
      n = cuentachar (c(1:i1-2),':',i1-2) ! busco a ver cuantos signos ":" hay
      if (n .eq. 0) go to 100 ! no hay ciclo implicito
      if (n .eq. 1) then ! "1:9/...." ! hay un solo signo ":" ciclo implicito sin incremento
         sw1 = buscachar(c(1:i1-2),':',i1-2,i,1,1) ! busco la posicion del primer signo ":"
         read (c(1:i-1),*) desde
         read (c(i+1:i1-2),*) hasta
         if (sw3) go to 100
         read (c(i1:i2),*) (var(i),i=desde,hasta)
         return
      endif
      if (n .eq. 2) then ! "1:9:1/...." ! hay dos signos ":" ciclo implicito con incremento
         sw1 = buscachar(c(1:i1-2),':',i1-2,i,1,1) ! busco la posicion del primer signo ":"
         read (c(1:i-1),*) desde
         sw1 = buscachar(c(1:i1-2),':',i1-2,j,2,1) ! busco la posicion del segundo signo ":"
         read (c(i+1:j-1),*) hasta
         read (c(j+1:i1-2),*) incre
         if (sw3) go to 100
         read (c(i1:i2),*) (var(i),i=desde,hasta,incre)
         return
      endif

100   continue
      do i = desde, hasta, incre
         var (i) = valor
      enddo
      return
      end
