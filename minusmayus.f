c********************************************************************************
c*     Rutina para convertir una cadena de caracteres en letras minusculas a
c*     letras mayusculas y viceversa
c********************************************************************************

      SUBROUTINE minusmayus
      implicit none
      logical*4 sw
      INTEGER*4 I, J, N, nc  !, lonstrin
      CHARACTER A*(*), B*33, B1(33)*1, C*33, C1(33)*1, cc*1
      EQUIVALENCE (B,B1(1)), (C,C1(1))
c*              123456789012345678901234567890123
      DATA B  /' ABCDEFGHIJKLMNÑOPQRSTUVWXYZAEIOU'/
      DATA C  /' abcdefghijklmnñopqrstuvwxyzÄÅÕÆÇ'/
      DATA NC /33/
      entry minusculas (a,n)
      if (n .le. 0) n = len_trim (a)  !  lonstrin(a)
      sw = .true.
      DO I = 1, n
         if (a(i:i) .eq. '"' .or. a(i:i) .eq. "'") then
            if (sw) then
               sw = .false.
               cc = a(i:i)
            else
               if (a(i:i) .eq. cc) sw = .true.
            endif
            cycle
         endif
         if (sw) then
            DO J = 1, nc
               IF (A(I:I) .EQ. B1(J)) THEN
                  A (I:I) = C1 (J)
                  exit
               ENDIF
            enddo
         endif
      enddo
      RETURN

      entry mayusculas (a,n)
      if (n .le. 0) n = len_trim (a)  ! lonstrin (a)
      sw = .true.
      DO I = 1, n
         if (a(i:i) .eq. '"' .or. a(i:i) .eq. "'") then
            if (sw) then
               sw = .false.
               cc = a(i:i)
            else
               if (a(i:i) .eq. cc) sw = .true.
            endif
            cycle
         endif
         if (sw) then
            DO J = 1, nc
               IF (A(I:I) .EQ. C1(J)) THEN
                  A (I:I) = B1 (J)
                  exit
               ENDIF
            enddo
         endif
      enddo
      RETURN
      END
