      include '/F/G77/Rutinas/chartran.f'
      include '/F/G77/Rutinas/lonstrin.f'
*      program f
*      implicit none
*      character a*72, b*72, inviertefecha*8
*      integer*4 l
*      logical*4 fechavalida
*
*      a = 'help'
*      call fecha (a)
*      a = 'ddmmaaaa'
*      call fecha (a)
*60    format (a8,1x,a8)      
*      b = inviertefecha (a(1:8))
*      write (6,60) a(1:8), b(1:8)
*      a = 'aaaammdd'
*      call fecha (a)
*      b = inviertefecha (a(1:8))
*      write (6,60) a(1:8), b(1:8) ! inviertefecha (a(1:8))
*      a = 'aammdd'
*      call fecha (a)
*      b = inviertefecha (a(1:8))
*      write (6,60) a(1:6), b(1:8) ! inviertefecha (a(1:6))
*      a = 'ddmmaa'
*      call fecha (a)
*      b = inviertefecha (a(1:8))
*      write (6,60) a(1:6), b(1:8) ! inviertefecha (a(1:6))
*      a = 'francisco iglesias'
*      call cminusmayus (a,1)
*      call cminusmayus (a,2)
*      a (1:8) = '310299  '
*      if (.not. fechavalida(a(1:8))) then
*         write (6,*) 'La fecha '//a(1:8)//' es incorrecta'
*      endif
*      a (1:8) = '311299  '
*      if (fechavalida(a(1:8))) then
*         write (6,*) 'La fecha '//a(1:8)//' es correcta'
*      endif
*      stop
*      end
*
c**********************************************************************
c*    Rutina FECHA para pedir la fecha y hora del dÕa al sistema opera-
c*    tivo en distintos formatos.
c*    Actualizada por: Francisco Iglesias    Dic-94
c**********************************************************************

      subroutine fecha (b)
      implicit none
      character b*(*), a*80
*      integer*4 l

*      l = len_trim(b)
      call cminusmayus (b,2) ! convierte a mayusculas
      if (b(1:4) .ne. 'HELP'  .and.  b(1:5) .ne. 'AYUDA') then
         call f0 (b)
         return
      end if

      write (6,*)
      WRITE (6,600)
600   FORMAT ('PARAMETROS DE LA RUTINA FECHA')

      a = 'DDMMAA'
      call f0 (a)
      write (6,601) a(1:6)
601   FORMAT ('DDMMAA      : ',a6)  ! DIAMESANO'/

      a = 'DDMMAAAA'
      call f0 (a)
      write (6,602) a(1:8)
602   FORMAT ('DDMMAAAA    : ',a8)  ! DIAMESANO'/

      a = 'AAMMDD'
      call f0 (a)
      write (6,607) a(1:6)
607   FORMAT ('AAMMDD      : ',a6)  ! ANOMESDIA'/

      a = 'AAAAMMDD'
      call f0 (a)
      write (6,608) a(1:8)
608   FORMAT ('AAAAMMDD    : ',a8)  ! ANOMESDIA'/

      a = 'DD-MM-AA'
      call f0 (a)
      write (6,603) a(1:8)
603   FORMAT ('DD-MM-AA    : ',a8)  ! DIA-MES-ANO'/

      a = 'DD/MM/AA'
      call f0 (a)
      write (6,604) a(1:8)
604   FORMAT ('DD/MM/AA    : ',a8)  ! DIA/MES/ANO'/

      a = 'DD-MM-AAAA'
      call f0 (a)
      write (6,620) a(1:10)
620   FORMAT ('DD-MM-AAAA  : ',a10)  ! DIA/MES/ANO'/

      a = 'DD/MM/AAAA'
      call f0 (a)
      write (6,621) a(1:10)
621   FORMAT ('DD/MM/AAAA  : ',a10)  ! DIA/MES/ANO'/

      a = 'DD-MMM-AA'
      call f0 (a)
      write (6,605) a(1:9)
605   FORMAT ('DD-MMM-AA   : ',a9)  ! DIA-TRES PRIMERAS LETRAS DEL MES-ANO'/
      a = 'DD-MMM-AAAA'
      call f0 (a)
      write (6,6051) a(1:11)
6051  FORMAT ('DD-MMM-AAAA : ',a11)  ! DIA-TRES PRIMERAS LETRAS DEL MES-ANO'/

      a = 'DD/MMM/AA'
      call f0 (a)
      write (6,606) a(1:9)
606   FORMAT ('DD/MMM/AA   : ',a9)  ! DIA/TRES PRIMERAS LETRAS DEL MES/ANO'/
      a = 'DD/MMM/AAAA'
      call f0 (a)
      write (6,6061) a(1:11)
6061  FORMAT ('DD/MMM/AAAA : ',a11)  ! DIA/TRES PRIMERAS LETRAS DEL MES/ANO'/

      a = 'HH:MM:SS'
      call f0 (a)
      write (6,609) a(1:8)
609   FORMAT ('HH:MM:SS    : ',a8)  ! HORA:MINUTOS:SEGUNDOS'/

      a = 'DIA'
      call f0 (a)
      write (6,611) a(1:10)
611   FORMAT ('DIA         : ',a10) ! NOMBRE DEL DIA'/

      a = 'MES'
      call f0 (a)
      write (6,610) a(1:10)
610   FORMAT ('MES         : ',a10) ! NOMBRE DEL MES'/

      a = 'N-DIA'
      call f0 (a)
      write (6,613) a(1:2)
613   FORMAT ('N-DIA       : ',a2)  ! NUMERO DEL DIA (1-7)'/

      a = 'N-MES'
      call f0 (a)
      write (6,612) a(1:2)
612   FORMAT ('N-MES       : ',a2)  ! NUMERO DEL MES (1-12)'/

      a = 'N-ANO'
      call f0 (a)
      write (6,614) a(1:4)
614   FORMAT ('N-ANO       : ',a4)  ! NUMERO DEL ANO'/

      a = 'AA'
      call f0 (a)
      write (6,622) a(1:2)
622   format ('AA          : ',a2) ! numero del a·o

      a = 'AAAA'
      call f0 (a)
      write (6,623) a(1:4)
623   format ('AAAA        : ',a4) ! numero del a·o

      a = 'DDDAA'
      call f0 (a)
      write (6,615) a(1:5)
615   FORMAT ('DDDAA       : ',a5)  ! FECHA JULIANA'/

      a = 'FECHA'
      call f0 (a)
      write (6,616) a(1:80)
616   FORMAT ('FECHA       : ',a80) ! FECHA EN LETRAS (MAYUSCULAS)'/

      a = 'FECHA1'
      call f0 (a)
      write (6,617) a(1:80)
617   FORMAT ('FECHA1      : ',a80) ! FECHA EN LETRAS (MAYUSCULAS)'/

      a = 'FECHA2'
      call f0 (a)
      write (6,618) a(1:80)
618   FORMAT ('FECHA2      : ',a80) ! FECHA EN LETRAS (MINUSCULAS)'/

      write (6,619)
619   FORMAT ('HELP        : MUESTRA ESTA AYUDA')

      write (6,624)
624   FORMAT ('AYUDA       : MUESTRA ESTA AYUDA')

      return
      end

C**********************************************************************
C*    RUTINA PARA PEDIR LA FECHA Y HORA AL SISTEMA OPERATIVO Y DEVOL-
C*    VERLA AL USUARIO DE ACUERDO AL FORMATO ESPECIFICADO EN EL PARA-
C*    METRO DE ENTRADA.
C*    Actualizada: Francisco Iglesias   Dic-94
C**********************************************************************
  
      SUBROUTINE F0 (A)
      implicit none
  
      CHARACTER A*(*)
      CHARACTER B*6, DIAS(0:6)*10, MESES(12)*10, E*20, D*80, ff*8,
     *          hora*8
      INTEGER*4 NDIA, NMES, I, N, mile, nano, I1, I2, I3, L, I0, J
      integer*4 diasem
  
      DATA DIAS  /'DOMINGO   ', ! 0
     *            'LUNES     ', ! 1
     *            'MARTES    ', ! 2
     *            'MIERCOLES ', ! 3
     *            'JUEVES    ', ! 4
     *            'VIERNES   ', ! 5
     *            'SABADO    '/ ! 6
  
      DATA MESES /'ENERO     ',
     *            'FEBRERO   ',
     *            'MARZO     ',
     *            'ABRIL     ',
     *            'MAYO      ',
     *            'JUNIO     ',
     *            'JULIO     ',
     *            'AGOSTO    ',
     *            'SEPTIEMBRE',
     *            'OCTUBRE   ',
     *            'NOVIEMBRE ',
     *            'DICIEMBRE '/
C*
c*      call idate (nmes, ndia, nano)
*      if (nano .gt. 99) then
*         nano = nano - 100
*         mile = 20
*      else
*         mile = 20
*      endif
*      call time  (hora)
      character fechax*8, horax*10 ! fecha: AAAAMMDD     HORA: HHMMSS.000
      call date_and_time (fechax, horax)
      read (fechax,'(4i2)') mile, nano, nmes, ndia
      hora = horax(1:2)//':'//horax(3:4)//':'//horax(5:6)
C*
*      l = len_trim (a)
*      write (6,*)'Sub.F0: ',l,' ',a(1:l)
      if (a(1:8) .eq. 'DDMMAAAA') then
         write (a(1:8),'(4i2.2)') ndia, nmes, mile, nano
         return
      end if

      if (a(1:8) .eq. 'AAAAMMDD') then
         write (a(1:8),'(4i2.2)') mile, nano, nmes, ndia
         return
      end if

      IF (A(1:6) .EQ. 'DDMMAA') THEN
         write (a(1:6),'(3i2.2)') ndia, nmes, nano
         RETURN
      END IF
  
      IF (A(1:6) .EQ. 'AAMMDD') THEN
         write (a(1:6),'(3i2.2)') nano, nmes, ndia
         RETURN
      END IF
 
      IF (A(1:10) .EQ. 'DD-MM-AAAA') THEN
         write (a(1:10),100) ndia, '-', nmes, '-', mile, nano
         RETURN
      END IF
  
      IF (A(1:8) .EQ. 'DD-MM-AA') THEN
         write (a(1:8),100) ndia, '-', nmes, '-', nano
100      format (i2.2,a1,i2.2,a1,2i2.2)
         RETURN
      END IF
  
      IF (A(1:10) .EQ. 'DD/MM/AAAA') THEN
         write (a(1:10),100) ndia, '/', nmes, '/', mile, nano
         RETURN
      END IF
  
      IF (A(1:8) .EQ. 'DD/MM/AA') THEN
         write (a(1:8),100) ndia, '/',nmes, '/',nano
         RETURN
      END IF
  
      IF (A(1:11) .EQ. 'DD-MMM-AAAA') then
         write (a(1:11),102) ndia, '-', meses(nmes)(1:3), '-', mile,nano
102      format (i2.2,a1,a3,a1,2i2.2)
         RETURN
      END IF
  
      IF (A(1:9) .EQ. 'DD-MMM-AA') then
         write (a(1:9),101) ndia, '-', meses(nmes)(1:3), '-', nano
101      format (i2.2,a1,a3,a1,i2.2)
         RETURN
      END IF
  
      IF (A(1:11) .EQ. 'DD/MMM/AAAA') THEN
         write (a(1:9),102) ndia, '/', meses(nmes)(1:3), '/', mile,nano
         RETURN
      END IF
  
      IF (A(1:9) .EQ. 'DD/MMM/AA') THEN
         write (a(1:9),101) ndia, '/', meses(nmes)(1:3), '/', nano
         RETURN
      END IF
  
      IF (A(1:5) .EQ. 'DDDAA') THEN
         IF (NMES .GT. 01) NDIA = NDIA + 31
         IF (NMES .GT. 02) NDIA = NDIA + 28
         IF (NANO/4*4 .EQ. NANO) NDIA = NDIA + 1
         IF (NMES .GT. 03) NDIA = NDIA + 31
         IF (NMES .GT. 04) NDIA = NDIA + 30
         IF (NMES .GT. 05) NDIA = NDIA + 31
         IF (NMES .GT. 06) NDIA = NDIA + 30
         IF (NMES .GT. 07) NDIA = NDIA + 31
         IF (NMES .GT. 08) NDIA = NDIA + 31
         IF (NMES .GT. 09) NDIA = NDIA + 30
         IF (NMES .GT. 10) NDIA = NDIA + 31
         IF (NMES .GT. 11) NDIA = NDIA + 30
         WRITE (A(1:5), '(I3.3,i2.2)') NDIA, nano
         RETURN
      END IF
  
      IF (A(1:8) .EQ. 'HH:MM:SS') THEN
         a (1:8) = hora
         RETURN
      END IF
  
      IF (A(1:3) .EQ. 'DIA') THEN
         write (ff,'(4i2.2)') ndia, nmes, mile, nano
         n = diasem (ff)
         A (1:10) = DIAS (N)
         RETURN
      END IF
  
      IF (A(1:3) .EQ. 'MES') THEN
         A (1:10) = MESES (NMES)
         RETURN
      END IF
  
      IF (A(1:5) .EQ. 'N-DIA') THEN
         WRITE (A(1:5), '(I2.2,3X)') NDIA
         RETURN
      END IF
  
      IF (A(1:5) .EQ. 'N-MES') THEN
         WRITE (A(1:5), '(I2.2,3X)') NMES
         RETURN
      END IF
 
      IF (A(1:5) .EQ. 'N-ANO') THEN
         write (a(1:5),'(2i2.2,1x)') mile, nano
         RETURN
      END IF

      if (a(1:4) .eq. 'AAAA') then
         write (a(1:4),'(2i2.2)') mile, nano
         return
      end if
  
      if (a(1:2) .eq. 'AA') then
         write (a(1:2),'(i2.2)') nano
         return
      end if

      IF (A(1:5) .EQ. 'FECHA'  .OR.
     *    A(1:6) .EQ. 'FECHA1' .OR.
     *    A(1:6) .EQ. 'FECHA2') THEN
         D (01:40) = '                                        '
         D (41:80) = '                                        '
         write (b(1:6),'(2i2.2)') ndia, nano
         CALL F1 (B(1:2),E,L) 
         I1 = 1
         I2 = L
         D (I1:I2) = E (1:L)
         I1 = I2 + 1
         IF (B(1:2) .EQ. '01') THEN
            I2 = I2 + 16
            D (I1:I2) = ' DIA DEL MES DE '
         ELSE
            I2 = I2 + 17
            D (I1:I2) = ' DIAS DEL MES DE '
         END IF
         i0 = i1 + 1
         DO J = 10, 1, -1
            IF (MESES(NMES)(J:J) .NE. ' ') GO TO 25
         END DO
25       CONTINUE
         I1 = I2 + 1
         if (mile .eq. 19) then
            I2 = I2 + J + 20
            D (I1:I2) = MESES (NMES) (1:J) // ' DE MIL NOVECIENTOS '
            I3 = I1
            CALL F1 (B(3:4),E,L)
            I1 = I2 + 1
            I2 = I2 + L + 1
            D (I1:I2) = E (1:L) // '.'
            IF (D(I2-2:I2) .EQ. 'UN.') THEN
               I2 = I2 + 1
               D (I2-1:I2) = 'O.'
            END IF
         else
            I2 = I2 + J + 17
*            D (I1:I2) = MESES (NMES) (1:J) // ' DEL A¶O DOS MIL '
            D (I1:I2) = MESES (NMES) (1:J) // ' DEL AÑO DOS MIL '
            I3 = I1
            if (nano .eq. 00) then
               d (i2:i2) = '.'
            else
               CALL F1 (B(3:4),E,L)
            if (e(1:l) .eq. 'PRIMER') then
               l = 3
               e = 'UNO'
            endif
            I1 = I2 + 1
            I2 = I2 + L + 1
            D (I1:I2) = E (1:L) // '.'
         endif
         endif
         IF (A(1:6) .EQ. 'FECHA2') THEN
*            write (6,*)len_trim(d),d(1:len_trim(d))
            CALL cminusmayus (D,1)
            D (I3:I3) = MESES (NMES) (1:1)
            if (d(i0:i0+2) .eq. 'dia') d(i0:i0+2) = 'día'
         END IF
         L = LEN (A)
         IF (L .GT. I2) L = I2
         A (1:L) = D (1:L)
         RETURN
      END IF
  
      N = LEN (A)
      DO I = 1, N
         A (I:I) = '*'
      END DO

      RETURN
      END
  
      SUBROUTINE F1 (D,S,I2)
      implicit none
      CHARACTER D*2, S*20
      CHARACTER M(25)*10
      INTEGER*4 L(25), I, J, I1, I2, I3
      DATA M /'UN        ',  ! 01
     *        'DOS       ',  ! 02
     *        'TRES      ',  ! 03
     *        'CUATRO    ',  ! 04
     *        'CINCO     ',  ! 05
     *        'SEIS      ',  ! 06
     *        'SIETE     ',  ! 07
     *        'OCHO      ',  ! 08
     *        'NUEVE     ',  ! 09
     *        'DIEZ      ',  ! 10
     *        'ONCE      ',  ! 11
     *        'DOCE      ',  ! 12
     *        'TRECE     ',  ! 13
     *        'CATORCE   ',  ! 14
     *        'QUINCE    ',  ! 15
     *        'DIECI     ',  ! 16
     *        'VEINTI    ',  ! 17
     *        'TREINTA   ',  ! 18
     *        'CUARENTA  ',  ! 19
     *        'CINCUENTA ',  ! 20
     *        'SESENTA   ',  ! 21
     *        'SETENTA   ',  ! 22
     *        'OCHENTA   ',  ! 23
     *        'NOVENTA   ',  ! 24
     *        'MIL       '/  ! 25
  
      DATA L /2, 3, 4, 6, 5, 4, 5, 4, 5, 4, 4, 4, 5, 7, 6, 5, 6, 7,
     *        8, 9, 7, 7, 7, 7, 3/
 
      S(1:20) = '                    '
      I2 = 0
      READ (D(1:2),'(2I1)') I, J
      IF (D(1:1) .GT. '0') THEN
         IF (D(1:2) .GT. '15') THEN
            I = I + 15
         ELSE
            I = I*10 + J
         END IF
         I3 = L(I)
         I1 = 1
         I2 = I1 + I3 - 1
         S (I1:I2) = M (I) (1:I3)
         IF (D(1:2) .LT. '16') RETURN
         IF (D(1:2) .EQ. '20') THEN
            S(I2:I2) = 'E'
            RETURN
         END IF
         IF (D(1:2) .EQ. '30') RETURN
         IF (D(1:2) .EQ. '31') THEN
            S(I2+1:I2+5) = ' Y UN'
            I2 = I2 + 5
            RETURN
         END IF
         IF (D(1:2) .GT. '31') THEN
            S(I2+1:I2+5) = ' Y '
            I2 = I2 + 3
         END IF
      END IF
  
      IF (D(1:2) .EQ. '01') THEN
         S(1:6) = 'PRIMER'
         I2 = 6
         RETURN
      END IF
  
      I3 = L(J)
      I1 = I2 + 1
      I2 = I1 + I3 - 1
      S (I1:I2) = M (J) (1:I3)
      RETURN
      END
  
c**********************************************************************
c*    Convierte minusculas a mayusculas y viceversa
c**********************************************************************

      SUBROUTINE cminusmayus (a,i)
      implicit none
      character a*(*), letras1*32, letras2*32, chartran*128
      integer*4 n, i, nc
*                    12345678901234567890123456789012
      DATA letras1 /'ABCDEFGHIJKLMNÑOPQRSTUVWXYZ'/!AEIOU'/
      DATA letras2 /'abcdefghijklmnñopqrstuvwxyz'/!ÄÅÕÆÇ'/
      DATA NC      /27/
      n = len_trim (a)
      if (i .eq. 1) a = chartran (a,letras1,letras2,n,nc,nc)
      if (i .eq. 2) a = chartran (a,letras2,letras1,n,nc,nc)
      return
      end

c**********************************************************************
c*    Rutina para determinar el dia de la semana a que corresponde una
c*    fecha dada.
c*    diasem = (0,1,2,3,4,5,6) (Dom,lun,mar,mie,jue,vie,sab)
c**********************************************************************

      integer*4 function diasem (fecha1)
      implicit none

      character fecha1*8
      integer*4 d1, m1, a1, a2, cte(12)
      real*8    factor1, factor, dif

      data factor / 694346.0 / ! 06-01-1901 (Domingo)
      data cte /0,31,59,90,120,151,181,212,243,273,304,334/

      read (fecha1,'(2i2,i4)') d1, m1, a1
      a2 = a1
      if (m1 .lt. 3) a2 = a2 - 1
      factor1 = 365 * a1 + cte(m1) + d1 + int(a2/4)
      dif = factor1 - factor
      diasem = dif - int(dif/7)*7
      return
      end

c**********************************************************************
c*    Rutina para invertir una fecha 
c*    (ddmmaaaa --> aaaammdd)  (ddmmaa --> aammdd)
c*    (aaaammdd --> ddmmaaaa)  (aammdd --> ddmmaa)
c**********************************************************************

      character*(*) function inviertefecha (f)
      implicit none

      character f*(*)
      integer*4 l

      l = len_trim (f)
*      write (6,*)'Func.Inviertefecha: ',l,' ',f(1:l)
      if (l .eq. 8) then
         read (f,'(4x,i4)') l
         if (l .lt. 1500) then ! viene aaaa-mm-dd
            inviertefecha = f (7:8) // f (5:6) // f (1:4)
         else                  ! viene dd-mm-aaaa
            inviertefecha = f (5:8) // f (3:4) // f (1:2)
         endif
      else
         inviertefecha = f (5:6) // f (3:4) // f (1:2) // '  '
      endif

      return
      end

C**********************************************************************
C*    RUTINA PARA VALIDAR LA FECHA DADA
c*    La fecha debe venir en el formato 'ddmmaaaa' o 'ddmmaa  '
c*    Si la fecha es valida se devuelve siempre 'DDMMAAAA'
c*    El a·o sera transformado a 4 digitos si el mismo es menor a 1000
C**********************************************************************
    
      LOGICAL*4 FUNCTION fechavalida (FE)
      implicit none
  
      CHARACTER fe*8
      INTEGER*4 dia, mes, ano
  
      fechavalida = .FALSE.
      read (fe,'(2i2,i4)',err=99) dia, mes, ano
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

      if (ano .lt. 1000) then
         ano = ano + 1900
         WRITE (FE,'(2I2.2,i4.4)') dia, mes, ano
      endif

99    continue
      return
      end

c**********************************************************************
c*    rutina para calcular el nro. de dias transcurriodos entre dos 
c*    fechas dadas.
c*    La fecha debe venir "ddmmaaaa"
c**********************************************************************

      integer*4 function nrodias1 (fecha1, fecha2)
      implicit none

      character fecha1*8, fecha2*8, inviertefecha*8
      integer*4 factor1, factor2, factordia

      factor1 = factordia (inviertefecha(fecha1))
      factor2 = factordia (inviertefecha(fecha2))
      nrodias1 = factor2 - factor1 + 1
      return
      end

c**********************************************************************
c*    rutina para calcular el nro. de dias transcurriodos entre dos 
c*    fechas dadas.
c*    La fecha debe venir "aaaammdd"
c**********************************************************************

      integer*4 function nrodias2 (fecha1, fecha2)
      implicit none

      character fecha1*8, fecha2*8
      integer*4 factor1, factor2, factordia

      factor1 = factordia (fecha1)
      factor2 = factordia (fecha2)
      nrodias2 = factor2 - factor1 + 1
      return
      end

c**********************************************************************
c*    Rutina para convertir una fecha en dias trasncurridos.
c*    La fecha debe venir "aaaammdd"
c**********************************************************************

      integer*4 function factordia (fecha)
      implicit none

      character fecha*8

      integer*4 dia, mes, ano, aaa , cte(12)

      data cte /0,31,59,90,120,151,181,212,243,273,304,334/

      read (fecha,'(i4,2i2)') ano, mes, dia
      aaa = ano
      if (mes .lt. 3) aaa = aaa - 1
      factordia = 365 * ano + cte(mes) + dia + int(aaa/4)
      return
      end

