SUBROUTINE PPLDX (X,INSTR,IRC)
IMPLICIT NONE
!----------
! METRIC-PPBASE $Id$
!----------
!
!     CALLED FROM ALGEVL
!
!     LOADS X VALUES OWNED BY PPE AS OPPOSED TO THE PROGNOSIS MODEL.
!
!     N.L.CROOKSTON - APR 87 - FORESTRY SCIENCES LAB - MOSCOW, ID
!
!     X     = THE VALUE REQUESTED.
!     INSTR = THE CODE THAT SAYS WHICH VALUE IS REQUESTED.
!     IRC   = RETURN CODE, 0=OK, 1=VARIABLE IS CURRENTLY UNDEFINED,
!             2=INSTRUCTION CODE COULD NOT BE DECIPHERED.
!
!OMMONS
!
!
INCLUDE 'PPEXCM.f90'
INCLUDE 'METRIC.f90'
!
!
!OMMONS
!
!
INTEGER IRC,INSTR,I
REAL X
!
!     DECODE INSTRUCTION AND EXECUTE.
!
IF (INSTR.GT.7000) THEN
   IF (INSTR.LT.7200) THEN
      I=INSTR-7000
      IF (I.LE.NPTST1) THEN
         IF (LPTST1(I)) THEN
           J=INSTR/100
           GOTO (1101,1102,1102,1100,1103, &
                    1102,1102,1100,1100,1100) J
1100            CONTINUE
             X = PTSTV1(I)
               GOTO 1000
1101            CONTINUE
             X = PTSTV1(I) / ACRtoHA
             GOTO 1000
1102            CONTINUE
             X = PTSTV1(I) * FT3pACRtoM3pHA
             GOTO 1000
1103            CONTINUE
             X = PTSTV1(I) * FT2pACRtoM2pHA
             GOTO 1000
1000            CONTINUE
           IRC=0
           RETURN
         ELSE
           IRC=1
           RETURN
         ENDIF
      ELSE
         IRC=2
         RETURN
      ENDIF
   ELSEIF (INSTR.LT.8000) THEN
      I=INSTR-7200
      IF (I.LT.NPTST2) THEN
         IF (LPTST2(I)) THEN
            X=PTSTV2(I)
            IRC=0
            RETURN
         ELSE
            IRC=1
            RETURN
         ENDIF
      ELSE
         IRC=2
         RETURN
      ENDIF
   ELSE
      I=INSTR-8000
      IF (I.LT.NPTST3) THEN
         X=PTSTV3(I)
         IRC=0
         RETURN
      ELSE
         IRC=2
         RETURN
      ENDIF
   ENDIF
ELSE
   IRC=2
   RETURN
ENDIF
END
