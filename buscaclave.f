C**********************************************************************
C*    RUTINA PARA LOCALIZAR UNA CLAVE DADA EN EL ARCHIVO INDICADO.
C*    SE USA EL METODO DE BUSQUEDA BINARIA.
C*    SE REQUIERE QUE EL ARCHIVO ESTE CLASIFICADO ASCENDENTEMENTE POR
C*    LA CLAVE.
C*    EL PRIMER REGISTRO DEL ARCHIVO ES REGISTRO DE CONTROL Y CONTIENE
C*    EL NRO. TOTAL DE REGISTROS VALIDOS EN EL ARCHIVO en los primeros
c*    4 bytes del registro y deben estar grabados en binario (integer*4)
C**********************************************************************
  
      LOGICAL*4 FUNCTION BUSCA_CLAVE (NA, CI, CF, CLAVE, NREG)
      implicit none
  
      CHARACTER CLAVE*(*)
      INTEGER*4 NA, CI, CF, NREG
C*
C*    DESCRIPCION DE LOS PARAMETROS
C*
C*    NA    = NRO. DEL ARCHIVO EN EL CUAL SE REALIZARA LA BUSQUEDA.
C*    CI    = COLUMNA O POSICION INICIAL DE LA CLAVE A BUSCAR.
C*    CF    = COLUMNA O POSICION FINAL DE LA CLAVE A BUSCAR.
C*    CLAVE = CONTIENE LA INFORMACION A BUSCAR.
C*    NREG  = CONTENDRA EL NRO. DEL REGISTRO EN DONDE SE ENCUENTRA LA
C*            CLAVE A BUSCAR.
C*
C**********************************************************************
C*
C*    VARIABLES LOCALES DE TRABAJO
C*
      CHARACTER REGISTRO*512, FMT1*6
      INTEGER*4 TOT_REG, LI, LS, NC, I, LR
C*
C*    PROCESO
C*
      INQUIRE (NA, RECL=LR)             ! averigua la longitud del registro
      write (fmt1,'("(A",i3.3,")")') lr ! prepara el formato de lectura de los registros
      read (na,'(a4)',rec=1) tot_reg    ! averigua el nro de registros validos en el archivo
      NC   = CF - CI + 1
      NREG = 0
      BUSCA_CLAVE = .FALSE.
C*
C*    CHEQUEO DE CONDICIONES DE BORDE
C*
      READ (NA, FMT1, REC=2) REGISTRO (1:LR)
  
      IF (CLAVE(1:NC) .EQ. REGISTRO(CI:CF)) THEN
          NREG        = 2
          BUSCA_CLAVE = .TRUE.
          RETURN
      END IF
  
      IF (CLAVE(1:NC) .LT. REGISTRO(CI:CF)) RETURN
  
      READ (NA, FMT1, REC=TOT_REG) REGISTRO (1:LR)
  
      IF (CLAVE(1:NC) .EQ. REGISTRO(CI:CF)) THEN
          NREG = TOT_REG
          BUSCA_CLAVE = .TRUE.
          RETURN
      END IF
  
      IF (CLAVE(1:NC) .GT. REGISTRO(CI:CF)) RETURN
C*
C*    BUSQUEDA BINARIA
C*
      LI = 3            !! LIMITE INFERIOR
      LS = TOT_REG - 1  !! LIMITE SUPERIOR
  
10    CONTINUE
      IF (LI .GT. LS) RETURN
  
      I = (LS - LI) / 2 + LI
      READ (NA, FMT1, REC=I) REGISTRO (1:LR)
  
      IF (CLAVE(1:NC) .LT. REGISTRO(CI:CF)) THEN
          LS = I - 1
          GO TO 10
      END IF
  
      IF (CLAVE(1:NC) .GT. REGISTRO(CI:CF)) THEN
          LI = I + 1
          GO TO 10
      END IF
  
C*    CLAVES IGUALES
  
      BUSCA_CLAVE = .TRUE.
      NREG = I
      RETURN
      END

