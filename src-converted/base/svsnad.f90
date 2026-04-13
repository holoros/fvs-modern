SUBROUTINE SVSNAD(IYEAR,ISNADD,NSNGS,ISWTCH)
IMPLICIT NONE
!----------
! BASE $Id$
!----------
!
!     STAND VISUALIZATION GENERATION
!     J.J.MARCHINEK -- RMRS MOSCOW -- MAY 1999
!     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
!     L.R. DAVID    -- FHTET FORT COLLINS -- JULY 2005
!     S.N.SCHAROSCH -- ABACUS -- APRIL 2008
!
!     USED FOR PROCESSING SNAG ADDING INFORMATION
!
!     ISNADD = VECTOR OF SNAGS THAT NEED TO BE ADDED
!     NSNGS  = NUMBER OF SNAGS THAT NEED TO BE ADDED
!     IYEAR  = CURRENT YEAR
!     IYOFTD = YEAR OF TREE DEATH
!     ISWTCH = 0 IF SVESTB CALLED SVSNAD DIRECTLY
!            = 1 IF SVMORT CALLED SVRMOV, FIRE-CAUSED MORTALITY
!            = 2 IF SVMORT CALLED SVRMOV, NORMAL MORTALITY
!            = 3 IF SVMORT CALLED SVRMOV, WESTWIDE PINE BEETLE MORT
!            = 4 IF SVCUTS CALLED SVRMOV
!
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.F77'
INCLUDE 'FMPARM.F77'
!
!
INCLUDE 'FMCOM.F77'
!
!
INCLUDE 'SVDEAD.F77'
!
!
INCLUDE 'ARRAYS.F77'
!
!
INCLUDE 'PLOT.F77'
!
!
INCLUDE 'SVDATA.F77'
!

INCLUDE 'CONTRL.F77'
!
!
!OMMONS
!
!
!     ISNADD - VECTOR OF SNAGS THAT NEED TO BE ADDED
!     SNASCO - SNAG SCORE
!     ADDSCO - SCORE FOR ISNADD
!     INDEX  - INDEX OF SNAG LIST
!     IBACK  - INDEX SNAGS REMOVED, POINTING BACK TO THE OBJECT LIST
!
INTEGER ISNADD(MXSVOB)
REAL ADDSCO(MXSVOB)

REAL SNASCO(MXDEAD)
INTEGER INDEX(MXDEAD)
INTEGER IBACK(MXDEAD)

INTEGER ISWTCH,NSNGS,IYEAR,ITCYC,IYOFTD,IX,ISVOBJ,ISNAG,I,IXX,
     & IDEAD,HRATE2,ITIDIF,IIX,IPUT,J,K
REAL    X,CHPOS,XHMOD,Y,FACTOR,TEMP,SNDI,SNHT,SNCRTO,
     & SNCRDI,XPROB

LOGICAL DEBUG
CALL DBCHK (DEBUG,'SVSNAD',6,ICYC)
!
!     SINCE
!
IF (ICYC.EQ.0) THEN
   ITCYC=1
ELSE
   ITCYC=ICYC
ENDIF
!
!     TO DETERMINE IF SVCUTS(1) CALLED IT, OR SVMORTS(0)
!
!     INITIALIZE THE ILYEAR
!
IF (ICYC.EQ.0 .AND. ISWTCH.EQ.0) ILYEAR=IYEAR

!
!     Initialize the year-of-tree-death variable.
!     This variable was originally designed to be incremented with each
!     new snag record, so that the tree deaths were spread evenly over
!     the current growth cycle.
!     However, that logic conflicts with the FFE snag logic that adds
!     snags in the final year of the growth period. The end result
!     was significantly different snagfall rates being predicted between
!     the FFE and SVS logic. Therefore, the SVS logic is now set to
!     match the FFE logic, with all mortality trees added at the end of
!     the current growth cycle.
!        IY -- IY(1)=INVENTORY DATE, IY(2)=ENDPOINT OF FIRST CYCLE,
!              IY(3)=ENDPOINT OF SECOND CYCLE,...,IY(MAXCY1)=ENDPOINT
!              OF FORTIETH CYCLE.
!

!>>>  IYOFTD = IYEAR
IF ( ICYC.EQ.0 .OR. ISWTCH.EQ.1 .OR. ISWTCH.EQ.4 ) THEN
  IYOFTD = IYEAR
ELSE
  IYOFTD = IY(ITCYC+1) - 1
ENDIF

!
!     DEBUG
!
IF (DEBUG) THEN
   IX=0
   DO ISVOBJ=1,NSVOBJ
      IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
   ENDDO
   WRITE (JOSTND,1010) ICYC, IYEAR, ISWTCH,
     & NDEAD, NSVOBJ, IX
1010    FORMAT (/ ' ','ENTERING SVSNAD, ICYC=',I2,', IYEAR=',I4,
     & ', ISWTCH=',I1,':', / ,
     & ' ',T5,'NDEAD=',I4,', SVS TOTAL OBJECTS=',I6,
     & ', SVS SNAG OBJECTS=',I5,':',/)
ENDIF
!
!     RETURN IF NO OBJECTS TO ADD
!
IF (NSNGS .EQ. 0) RETURN
!
IF (DEBUG) THEN
   DO ISVOBJ=1,NSVOBJ
      IF (IOBJTP(ISVOBJ).EQ.2)
     & WRITE (JOSTND,*) '   IS2F(',ISVOBJ,')=',IS2F(ISVOBJ)
   ENDDO
   WRITE(JOSTND,1020) NSNGS
1020    FORMAT(/, '    SNAG RECORDS TO ADD (NSNGS)=',I4)
ENDIF
!
!     CALCULATE THE SCORES FOR THE SNAGS BEING ADDED.
!
DO ISNAG=1,NSNGS
   I=ISNADD(ISNAG)
   ADDSCO(I) = DBH(IS2F(I))*DBH(IS2F(I))*HT(IS2F(I))
ENDDO
!
!     IF THERE IS ENOUGH ROOM, ADD THE SNAGS
!
IF (MXDEAD - NDEAD .GE. NSNGS) THEN
   IXX=0
   ISNAG=0
   DO IDEAD=1,MXDEAD

!           LOOPS THROUGH THE LIST OF SNAGS TO FIND AN OPEN SPOT

      IF (ISTATUS(IDEAD).EQ.0 .AND. NSNGS.GT.ISNAG) THEN

!              WE FOUND AN OPEN SPOT, AND STILL HAVE SNAGS TO ADD

         ISNSP(IDEAD)=ISP(IS2F(ISNADD(NSNGS-ISNAG)))
         IF (IOBJTP(ISNADD(NSNGS-ISNAG)) .EQ. -3) THEN

            ISTATUS(IDEAD)=1
            CALL SVRANN(X)
            FALLDIR(IDEAD) = IFIX(360. *X +.5)
            CRNRTO(IDEAD)=99.
            OLEN(IDEAD)=HT(IS2F(ISNADD(NSNGS-ISNAG)))*
     & ICR(IS2F(ISNADD(NSNGS-ISNAG)))*.01
            SNGLEN(IDEAD)=OLEN(IDEAD)
            CHPOS=HT(IS2F(ISNADD(NSNGS-ISNAG)))-
     & OLEN(IDEAD)

!                 0.0174532778 IS APPROXIMATELY PI/180.

            XSLOC(ISNADD(NSNGS-ISNAG))=XSLOC(ISNADD(NSNGS-ISNAG))
     & +CHPOS*SIN(FALLDIR(IDEAD)*0.0174532778)
            YSLOC(ISNADD(NSNGS-ISNAG))=YSLOC(ISNADD(NSNGS-ISNAG))
     & +CHPOS*COS(FALLDIR(IDEAD)*0.0174532778)
            ODIA(IDEAD)=DBH(IS2F(ISNADD(NSNGS-ISNAG)))*
     & OLEN(IDEAD)/HT(IS2F(ISNADD(NSNGS-ISNAG)))
            SNGDIA(IDEAD)=ODIA(IDEAD)
            CRNDIA(IDEAD)=CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))
            SNGCNWT(IDEAD,0:3)=
     & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
            OIDTRE(IDEAD)=IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

         ELSE
            OLEN(IDEAD)=HT(IS2F(ISNADD(NSNGS-ISNAG)))
            SNGLEN(IDEAD)=OLEN(IDEAD)
            ODIA(IDEAD)=DBH(IS2F(ISNADD(NSNGS-ISNAG)))
            SNGDIA(IDEAD)=ODIA(IDEAD)
            CRNRTO(IDEAD)=ICR(IS2F(ISNADD(NSNGS-ISNAG)))
            CRNDIA(IDEAD)=CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))
            SNGCNWT(IDEAD,0:3)=
     & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
             OIDTRE(IDEAD)=IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

            IF (IOBJTP(ISNADD(NSNGS-ISNAG)) .EQ. -2) THEN
               CALL SVRANN(X)
               FALLDIR(IDEAD) = IFIX(360. *X +.5)
            ELSE
               FALLDIR(IDEAD)=-1
            ENDIF
            IF (ISWTCH .EQ. 1) THEN
!                    THIS IS A FIRE MORTALITY, SET ISTATUS TO 5.
               ISTATUS(IDEAD) = 5
            ELSEIF (ISWTCH .EQ. 3) THEN
!                    THIS IS A WWPB MORTALITY, SET ISTATUS TO 90.
               ISTATUS(IDEAD) = 90
            ELSE
!                    CALCULATE ISTATUS NORMALLY.
               IF (IOBJTP(ISNADD(NSNGS-ISNAG)) .EQ. -1) THEN
                  ISTATUS(IDEAD) = 2
               ELSE
                  ISTATUS(IDEAD)=1
               ENDIF
            ENDIF
         ENDIF
         IYRCOD(IDEAD)=IYOFTD
!
!              IF SNAG IS ADDED AT THE BEGINNING, THE YEAR OF DEATH
!              OF THE SNAG MUST BE DETERMINED, AS WELL AS THE
!              ORIGINAL INFORMATION
!
         IF (ICYC .EQ. 0) THEN
            IF (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .EQ. 7) THEN
               IYRCOD(IDEAD)=IYRCOD(IDEAD)-MAX(IFIX(FINTM*.7),1)
               ISTATUS(IDEAD)=3
            ELSEIF (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .EQ. 9) THEN
               IYRCOD(IDEAD)=IYRCOD(IDEAD)-IFIX(FINTM+2)
               ISTATUS(IDEAD)=4
            ENDIF
            CALL SVHABT(XHMOD)
            IF (ISTATUS(IDEAD) .EQ. 4) THEN
               HRATE2=2
               Y=YHFHTS(ISNSP(IDEAD))
            ELSE
               HRATE2=1
               Y=YHFHTH(ISNSP(IDEAD))
            ENDIF
            ITIDIF=IYEAR-IYRCOD(IDEAD)
!
            IF ( DEBUG ) THEN
               WRITE(JOSTND,1025) IDEAD, IYRCOD(IDEAD), ITIDIF,
     & ODIA(IDEAD), OLEN(IDEAD)
1025                FORMAT (/ ' ',T5,'ADDING REC FOR ',
     & 'INITIAL TREELIST SNAG:',/,
     & ' ',T8,'IDEAD=',I4,', IYRCOD=',I4,
     & ', ITIDIF=',I2,/,
     & ' ',T8,'BEFORE BACKDATING, ODIA=',F4.1,
     & ', OLEN=',F5.1)
            ENDIF
!
            SNGLEN(IDEAD)=OLEN(IDEAD)
            IF (ITIDIF .GT. 0) THEN
               FACTOR=(1-0.0228*XHMOD*HRATE(ISNSP(IDEAD))*HRATE2)
               IF (FACTOR**(ITIDIF) .GT. 0.5) THEN
                  OLEN(IDEAD)=OLEN(IDEAD)/(FACTOR**(ITIDIF))
               ELSE
                  OLEN(IDEAD)=OLEN(IDEAD) /
     & (0.5*FACTOR**(MAX(0.0,(ITIDIF-Y))))
               ENDIF
            ENDIF
!
            IF ( DEBUG ) THEN
               WRITE (JOSTND,1026) OLEN(IDEAD), SNGLEN(IDEAD)
1026                FORMAT (' ',T8,'AFTER BACKDATING, OLEN=',F5.1,
     & ',SNGLEN=',F5.1)
            ENDIF
            CRNDIA(IDEAD)=CRNDIA(IDEAD)/(.90**ITIDIF)
!>>>           ELSE
!
!                 The following logic increments IYOFTD (yr of tree death),
!                 by one for each consecutively-added snag record, so that
!                 snag additions are spread equally across the years
!                 comprising the current growth period.
!
!                 Override IYOFTD assignment if SVSNAD is being called for
!                 fire mortality. In this case, assign all snags a year-of-death
!                 equal to the burn year.
!
!>>>              NOTE: logic to spread mortality across years in current growth
!>>>                    period is disabled, in order to match FFE logic.
!>>>
!>>>              IF (IYOFTD+1 .LT. IY(ITCYC+1) .AND. ISWTCH.NE.4) THEN
!>>>                 IYOFTD=IYOFTD+1
!>>>              ELSE
!>>>                 IYOFTD=IYEAR
!>>>              ENDIF
!>>>              IF ( ISWTCH .EQ. 1 ) THEN
!>>>                 IYOFTD=IYEAR
!>>>              ENDIF
         ENDIF
         IS2F(ISNADD(NSNGS-ISNAG))=IDEAD
         IOBJTP(ISNADD(NSNGS-ISNAG))=2
         NDEAD=NDEAD+1
         ISNAG=ISNAG+1
      ENDIF
   ENDDO

ELSE
!        THERE IS NOT ENOUGH ROOM.  ONLY ADD THE LARGEST ONES
   IF (DEBUG) THEN
      IX=0
      DO ISVOBJ=1,NSVOBJ
         IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
      ENDDO
      WRITE (JOSTND,*) 'SVSNAD1NDEAD=', NDEAD,
     & 'DEADOBJECTS=',IX
   ENDIF
!
!        SORT ADDSCO: ORIGINAL CODE USED IAPSRT, WHICH RETURNS INDEX TO AN
!        INTEGER VECTOR SORTED IN ASCENDING VECTOR; SINCE RDPSRT GIVES AN
!        INDEX TO A REAL VECTOR SORTED IN DESCENDING ORDER, THE RETURNED
!        INDICES (ISNADD) TO THE REAL VECTOR ARE REVERSED IN PLACE TO GIVE AN
!        ASCENDING REAL VECTOR. THIS IS MAKES THE SMALLEST POSSIBLE CHANGE
!        TO THE CODE. - DR/ESSA
!
   CALL RDPSRT(NSNGS,ADDSCO,ISNADD,.FALSE.)
   IF (NSNGS .GT. 1) THEN
     K = NSNGS
     J = ISNADD(K)
     DO I = 1, (NSNGS/2)
       ISNADD(K) = ISNADD(I)
       ISNADD(I) = J
       K = K-1
       J = ISNADD(K)
     ENDDO
   ENDIF
!
   ISNAG=0
   IF(MXDEAD .GT. NDEAD) THEN
      DO IDEAD=1,MXDEAD
         IF (ISTATUS(IDEAD) .EQ. 0 .AND. NSNGS.GT.ISNAG) THEN
            ISNSP(IDEAD)=ISP(IS2F(ISNADD(NSNGS-ISNAG)))
            IF (IOBJTP(ISNADD(NSNGS-ISNAG)) .EQ. -3) THEN
               PRINT *, 'IOBJTP IS -3'
               ISTATUS(IDEAD)=1
               CALL SVRANN(X)
               FALLDIR(IDEAD) = IFIX(360. *X +.5)
               CRNRTO(IDEAD)=99.
               OLEN(IDEAD)=HT(IS2F(ISNADD(NSNGS-ISNAG)))*
     & ICR(IS2F(ISNADD(NSNGS-ISNAG)))*.01
               SNGLEN(IDEAD)=OLEN(IDEAD)
               CHPOS=HT(IS2F(ISNADD(NSNGS-ISNAG)))-
     & OLEN(IDEAD)
!                    0.0174532778 IS APPROXIMATELY PI/180.
               XSLOC(ISNADD(NSNGS-ISNAG))=
     & XSLOC(ISNADD(NSNGS-ISNAG))
     & +CHPOS*SIN(FALLDIR(IDEAD)*0.0174532778)
               YSLOC(ISNADD(NSNGS-ISNAG))=
     & YSLOC(ISNADD(NSNGS-ISNAG))
     & +CHPOS*COS(FALLDIR(IDEAD)*0.0174532778)
               ODIA(IDEAD)=DBH(IS2F(ISNADD(NSNGS-ISNAG)))*
     & OLEN(IDEAD)/HT(IS2F(ISNADD(NSNGS-ISNAG)))
               SNGCNWT(IDEAD,0:3)=
     & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
               SNGDIA(IDEAD)=ODIA(IDEAD)
               CRNDIA(IDEAD)=CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))

               OIDTRE(IDEAD)=IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

            ELSE
               OLEN(IDEAD)=HT(IS2F(ISNADD(NSNGS-ISNAG)))
               SNGLEN(IDEAD)=OLEN(IDEAD)
               ODIA(IDEAD)=DBH(IS2F(ISNADD(NSNGS-ISNAG)))
               SNGDIA(IDEAD)=ODIA(IDEAD)
               CRNRTO(IDEAD)=ICR(IS2F(ISNADD(NSNGS-ISNAG)))
               CRNDIA(IDEAD)=CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))
               SNGCNWT(IDEAD,0:3)=
     & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
               OIDTRE(IDEAD)=IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

               IF (IOBJTP(ISNADD(NSNGS-ISNAG)) .EQ. -2) THEN
                  CALL SVRANN(X)
                  FALLDIR(IDEAD) = IFIX(360. *X +.5)
               ELSE
                  FALLDIR(IDEAD)=-1
               ENDIF
               IF (ISWTCH .EQ. 1) THEN
!                       THIS IS A FIRE MORTALITY, SET ISTATUS TO 5.
                  ISTATUS(IDEAD) = 5
                  TEMP = IOBJTP(ISNADD(NSNGS-ISNAG))
               ELSEIF (ISWTCH .EQ. 3) THEN
!                       THIS IS A WWPB MORTALITY, SET ISTATUS TO 90.
                  ISTATUS(IDEAD) = 90
               ELSE
!                       THIS IS NOT A FIRE MORTALITY, CALCULATE ISTATUS NORMALLY.
                  IF (IOBJTP(ISNADD(NSNGS-ISNAG)) .EQ. -1) THEN
                     ISTATUS(IDEAD) = 2
                  ELSE
                     ISTATUS(IDEAD)=1
                  ENDIF
               ENDIF
            ENDIF
            IYRCOD(IDEAD)=IYOFTD
!
!              IF SNAG IS ADDED AT THE BEGINNING, THE YEAR OF DEATH
!              OF THE SNAG MUST BE DETERMINED, AS WELL AS THE
!              ORIGINAL INFORMATION
!
            IF (ICYC .EQ. 0) THEN
               IF (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .EQ. 7) THEN
                  IYRCOD(IDEAD)=
     & IYRCOD(IDEAD)-MAX(IFIX(FINTM*.7),1)
                  ISTATUS(IDEAD)=3
               ELSEIF (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .EQ. 9) THEN
                  IYRCOD(IDEAD)=
     & IYRCOD(IDEAD)-IFIX(FINTM+2)
                  ISTATUS(IDEAD)=4
               ENDIF
               CALL SVHABT(XHMOD)
               IF (ISTATUS(IDEAD) .EQ. 4) THEN
                  HRATE2=2
                  Y=YHFHTS(ISNSP(IDEAD))
               ELSE
                  HRATE2=1
                  Y=YHFHTH(ISNSP(IDEAD))
               ENDIF
               ITIDIF=IYEAR-IYRCOD(IDEAD)
!
               IF ( DEBUG ) THEN
                  WRITE(JOSTND,1025) IDEAD, IYRCOD(IDEAD), ITIDIF,
     & ODIA(IDEAD), OLEN(IDEAD)
               ENDIF
!
               SNGLEN(IDEAD)=OLEN(IDEAD)
               IF (ITIDIF .GT. 0) THEN
                  FACTOR=
     & (1-0.0228*XHMOD*HRATE(ISNSP(IDEAD))*HRATE2)
                  IF (FACTOR**(ITIDIF) .GT. 0.5) THEN
                     OLEN(IDEAD) = OLEN(IDEAD)/(FACTOR**(ITIDIF))
                  ELSE
                     OLEN(IDEAD) = OLEN(IDEAD) /
     & (0.5*FACTOR**(MAX(0.0,(ITIDIF-Y))))
                  ENDIF
               ENDIF
!
               IF ( DEBUG ) THEN
                  WRITE (JOSTND,1026) OLEN(IDEAD), SNGLEN(IDEAD)
               ENDIF
               CRNDIA(IDEAD)=CRNDIA(IDEAD)/(.90**ITIDIF)
!>>>              ELSE
!
!                    The following logic increments IYOFTD (yr of tree death),
!                    by one for each consecutively-added snag record, so that
!                    snag additions are spread equally across the years
!                    comprising the current growth period.
!
!                    Override IYOFTD assignment if SVSNAD is being called for
!                    fire mortality. In this case, assign all snags a year-of-death
!                    equal to the burn year.
!
!>>>              NOTE: logic to spread mortality across years in current growth
!>>>                    period is disabled, in order to match FFE logic.
!>>>
!>>>                 IF (IYOFTD+1.LT.IY(ITCYC+1).AND. ISWTCH.NE.4) THEN
!>>>                    IYOFTD=IYOFTD+1
!>>>                 ELSE
!>>>                    IYOFTD=IYEAR
!>>>                 ENDIF
!>>>                 IF ( ISWTCH .EQ. 1 ) THEN
!>>>                    IYOFTD=IYEAR
!>>>                 ENDIF
            ENDIF
            IS2F(ISNADD(NSNGS-ISNAG))=IDEAD
            IOBJTP(ISNADD(NSNGS-ISNAG))=2
            NDEAD=NDEAD+1
            ISNAG=ISNAG+1
         ENDIF
      ENDDO
   ENDIF
!
!        CREATE INDEX AND THE SCORE VECTOR FOR THE SNAG LIST
!
   IX=0
   DO IDEAD=1,MXDEAD
      IF (ISTATUS(IDEAD).GT.0) THEN

!
!              AGES THE SNAG LIST
!
         CALL SVSNAGE(IYEAR,IDEAD,SNCRDI,SNCRTO,SNHT,SNDI)
         IX=IX+1
         INDEX(IX)=IDEAD
         SNASCO(IDEAD)=SNDI*SNDI*SNHT
      ENDIF
   ENDDO
!
!        MODIFY ILYEAR TO SHOW THAT THESE SNAGS HAVE BEEN AGED.
!
   ILYEAR = IYEAR
!
!        SORT SNASCO: ORIGINAL CODE USE IAPSRT, WHICH RETURNS INDEX TO AN
!        INTEGER VECTOR SORTED IN ASCENDING VECTOR; SINCE RDPSRT GIVES AN
!        INDEX TO A REAL VECTOR SORTED IN DESCENDING ORDER, THE RETURNED
!        INDICES (INDEX) TO THE REAL VECTOR ARE REVERSED IN PLACE TO GIVE AN
!        ASCENDING REAL VECTOR. THIS IS MAKES THE SMALLEST POSSIBLE CHANGE
!        TO THE CODE. - DR/ESSA
!
   CALL RDPSRT(NDEAD,SNASCO,INDEX,.FALSE.)
   IF (NDEAD .GT. 1) THEN
     K = NDEAD
     J = INDEX(K)
     DO I = 1, (NDEAD/2)
       INDEX(K) = INDEX(I)
       INDEX(I) = J
       K = K-1
       J = INDEX(K)
     ENDDO
   ENDIF
!
   IF (DEBUG) THEN
      IX=0
      DO ISVOBJ=1,NSVOBJ
         IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
      ENDDO
      WRITE (JOSTND,*) 'BETWEENNDEAD=', NDEAD,
     & 'DEADOBJECTS=',IX
   ENDIF

   DO ISVOBJ=1,NSVOBJ
      IF (IOBJTP(ISVOBJ) .EQ. 2) THEN
         IBACK(IS2F(ISVOBJ))=ISVOBJ
      ENDIF
   ENDDO

   DO IDEAD=1,MXDEAD
      IF (NSNGS.GT.ISNAG .AND. INDEX(IDEAD).NE.0) THEN
         IF (ADDSCO(ISNADD(NSNGS-ISNAG)) .GT.
     & SNASCO(INDEX(IDEAD))) THEN

            IF (DEBUG) WRITE (JOSTND,*) IX,NDEAD,
     & 'ISNADD(',NSNGS-ISNAG,')=',
     & ISNADD(NSNGS-ISNAG),'INDEX(',IDEAD,
     & ')=', INDEX(IDEAD),
     & 'IS2F(',ISNADD(NSNGS-ISNAG),')=',
     & IS2F(ISNADD(NSNGS-ISNAG)),
     & 'ADDSCO=',ADDSCO(ISNADD(NSNGS-ISNAG)),
     & 'SNASCO=',SNASCO(INDEX(IDEAD))

            ISNSP(INDEX(IDEAD))=ISP(IS2F(ISNADD(NSNGS-ISNAG)))
            IF (IOBJTP(ISNADD(NSNGS-ISNAG)) .EQ. -3) THEN

!**                     PRINT *, 'IOBJTP IS -3'

               IF (ISTATUS(INDEX(IDEAD)).EQ.0) THEN
                  NDEAD=NDEAD+1
                  IF (DEBUG) WRITE (JOSTND,*) 'ADDEDTO', NDEAD
               ENDIF
               ISTATUS(INDEX(IDEAD))=1
               CALL SVRANN(X)
               FALLDIR(INDEX(IDEAD)) = IFIX(360. *X +.5)
               CRNRTO(INDEX(IDEAD))=99.
               OLEN(INDEX(IDEAD))=HT(IS2F(ISNADD(NSNGS-ISNAG)))*
     & ICR(IS2F(ISNADD(NSNGS-ISNAG)))*.01
               SNGLEN(INDEX(IDEAD))=OLEN(INDEX(IDEAD))
               CHPOS=HT(IS2F(ISNADD(NSNGS-ISNAG)))-
     & OLEN(INDEX(IDEAD))

!                    0.0174532778 IS APPROXIMATELY PI/180.

               XSLOC(ISNADD(NSNGS-ISNAG))=
     & XSLOC(ISNADD(NSNGS-ISNAG))
     & +CHPOS*SIN(FALLDIR(INDEX(IDEAD))*0.0174532778)
               YSLOC(ISNADD(NSNGS-ISNAG))=
     & YSLOC(ISNADD(NSNGS-ISNAG))
     & +CHPOS*COS(FALLDIR(INDEX(IDEAD))*0.0174532778)
               ODIA(INDEX(IDEAD))=
     & DBH(IS2F(ISNADD(NSNGS-ISNAG)))*
     & OLEN(INDEX(IDEAD))
     & /HT(IS2F(ISNADD(NSNGS-ISNAG)))
               SNGCNWT(INDEX(IDEAD),0:3)=
     & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
                SNGDIA(INDEX(IDEAD))=ODIA(INDEX(IDEAD))
               CRNDIA(INDEX(IDEAD))=
     & CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))

               OIDTRE(INDEX(IDEAD))=
     & IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

            ELSE
               OLEN(INDEX(IDEAD))=HT(IS2F(ISNADD(NSNGS-ISNAG)))
               SNGLEN(INDEX(IDEAD))=OLEN(INDEX(IDEAD))
               ODIA(INDEX(IDEAD))=DBH(IS2F(ISNADD(NSNGS-ISNAG)))
               SNGCNWT(INDEX(IDEAD),0:3)=
     & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
               SNGDIA(INDEX(IDEAD))=ODIA(INDEX(IDEAD))
               CRNRTO(INDEX(IDEAD))=ICR(IS2F(ISNADD(NSNGS-ISNAG)))

               CRNDIA(INDEX(IDEAD))=
     & CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))

               OIDTRE(INDEX(IDEAD))=
     & IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

               IF (IOBJTP(ISNADD(NSNGS-ISNAG)) .EQ. -2) THEN
                  CALL SVRANN(X)
                  FALLDIR(INDEX(IDEAD)) = IFIX(360. *X +.5)
               ELSE
                  FALLDIR(INDEX(IDEAD))=-1
               ENDIF
               IF (ISTATUS(INDEX(IDEAD)).EQ.0) THEN
                  NDEAD=NDEAD+1
                  IF (DEBUG) WRITE (JOSTND,*) 'ADDEDTO', NDEAD
               ENDIF
               IF (IOBJTP(ISNADD(NSNGS-ISNAG)) .EQ. -1) THEN
                  ISTATUS(INDEX(IDEAD)) = 2
               ELSE
                  ISTATUS(INDEX(IDEAD))=1
               ENDIF
               IIX=0
               IF (DEBUG) THEN
                  DO ISVOBJ=1,NSVOBJ
                     IF (IOBJTP(ISVOBJ).EQ.2) IIX=IIX+1
                  ENDDO
                  IF (DEBUG) WRITE (JOSTND,*) 'DEADOBJECTS=',IIX
               ENDIF
            ENDIF
            IS2F(ISNADD(NSNGS-ISNAG))=INDEX(IDEAD)
            IOBJTP(ISNADD(NSNGS-ISNAG))=2
            IF (IBACK(INDEX(IDEAD)).GE.1 .AND.
     & IBACK(INDEX(IDEAD)).LE.MXSVOB)
     & IOBJTP(IBACK(INDEX(IDEAD)))=0
            ISNAG=ISNAG+1
            IYRCOD(INDEX(IDEAD))=IYOFTD
!
!              IF SNAG IS ADDED AT THE BEGINNING, THE YEAR OF DEATH
!              OF THE SNAG MUST BE DETERMINED, AS WELL AS THE
!              ORIGINAL INFORMATION
!
            IF (ICYC .EQ. 0) THEN
               IF (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .EQ. 7) THEN
                  IYRCOD(INDEX(IDEAD))=
     & IYRCOD(INDEX(IDEAD))-MAX(IFIX(FINTM*.7),1)
                  ISTATUS(INDEX(IDEAD))=3
               ELSEIF (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .EQ. 9) THEN
                  IYRCOD(INDEX(IDEAD))=
     & IYRCOD(INDEX(IDEAD))-IFIX(FINTM+2)
                  ISTATUS(INDEX(IDEAD))=4
               ENDIF
               CALL SVHABT(XHMOD)
               IF (ISTATUS(INDEX(IDEAD)) .EQ. 4) THEN
                  HRATE2=2
                  Y=YHFHTS(ISNSP(INDEX(IDEAD)))
               ELSE
                  HRATE2=1
                  Y=YHFHTH(ISNSP(INDEX(IDEAD)))
               ENDIF
               ITIDIF=IYEAR-IYRCOD(INDEX(IDEAD))
!
               IF ( DEBUG ) THEN
                  WRITE(JOSTND,1025) IDEAD, IYRCOD(INDEX(IDEAD)),
     & ITIDIF, ODIA(INDEX(IDEAD)),
     & OLEN(INDEX(IDEAD))
               ENDIF
!
               SNGLEN(INDEX(IDEAD)) = OLEN(INDEX(IDEAD))
               IF (ITIDIF .GT. 0) THEN
                  FACTOR=(1-0.0228*XHMOD*
     & HRATE(ISNSP(INDEX(IDEAD)))*HRATE2)
                  IF (FACTOR**ITIDIF .GT. 0.5) THEN
                     OLEN(INDEX(IDEAD)) =
     & OLEN(INDEX(IDEAD))/(FACTOR**(ITIDIF))
                  ELSE
                     OLEN(INDEX(IDEAD)) = OLEN(INDEX(IDEAD))/
     & (0.5*FACTOR**(MAX(0.0,(ITIDIF-Y))))
                  ENDIF
               ENDIF
               IF ( DEBUG ) THEN
                  WRITE (JOSTND,1026) OLEN(INDEX(IDEAD)),
     & SNGLEN(INDEX(IDEAD))
               ENDIF
               CRNDIA(INDEX(IDEAD))=
     & CRNDIA(INDEX(IDEAD))/(.90**ITIDIF)
!>>>              ELSE
!
!                    The following logic increments IYOFTD (yr of tree death),
!                    by one for each consecutively-added snag record, so that
!                    snag additions are spread equally across the years
!                    comprising the current growth period.
!
!                    Override IYOFTD assignment if SVSNAD is being called for
!                    fire mortality. In this case, assign all snags a year-of-death
!                    equal to the burn year.
!
!>>>              NOTE: logic to spread mortality across years in current growth
!>>>                    period is disabled, in order to match FFE logic.
!>>>
!>>>                 IF (IYOFTD+1.LT.IY(ITCYC+1) .AND. ISWTCH.NE.4) THEN
!>>>                    IYOFTD=IYOFTD+1
!>>>                 ELSE
!>>>                    IYOFTD=IYEAR
!>>>                 ENDIF
!>>>                 IF ( ISWTCH .EQ. 1 ) THEN
!>>>                    IYOFTD=IYEAR
!>>>                 ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
ENDIF
!
!
!
IF (DEBUG) THEN
   IX=0
   DO ISVOBJ=1,NSVOBJ
      IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
   ENDDO
   WRITE (JOSTND,1030) NDEAD, NSVOBJ, IX
1030    FORMAT (/ ' ',T5,'AFTER SNAG ADDITIONS:', / ,
     & ' ',T5,'   NDEAD=',I4,', SVS TOTAL OBJECTS=',I6,
     & ', SVS SNAG OBJECTS=',I5,':',/)
   DO ISVOBJ=1,NSVOBJ
      IF (IOBJTP(ISVOBJ).EQ.2)
     & WRITE (JOSTND,*) '   IS2F(',ISVOBJ,')=',IS2F(ISVOBJ)
   ENDDO
ENDIF
!
!     CLEAN UP OBJECTS IF NOT CALLED BY FIRE MODEL.
!
IF ( ISWTCH.NE.1 ) THEN
   IPUT=0
   DO ISVOBJ=1,NSVOBJ
      IF (IOBJTP(ISVOBJ).LE.0 .OR. IS2F(ISVOBJ).EQ.0) THEN
         IF (IPUT.EQ.0) IPUT=ISVOBJ
      ELSE
         IF (IPUT.GT.0 .AND. IPUT.LT.ISVOBJ) THEN
            IS2F(IPUT)=IS2F(ISVOBJ)
            XSLOC(IPUT)=XSLOC(ISVOBJ)
            YSLOC(IPUT)=YSLOC(ISVOBJ)
            IOBJTP(IPUT)=IOBJTP(ISVOBJ)
            IPUT=IPUT+1
         ENDIF
      ENDIF
   ENDDO
   IF (IPUT.GT.0) NSVOBJ=IPUT-1

   IF (DEBUG) THEN
      IX=0
      DO ISVOBJ=1,NSVOBJ
         IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
      ENDDO
      WRITE (JOSTND,*) 'SVSNADOUTNDEAD=', NDEAD,
     & 'DEADOBJECTS=',IX

      DO ISVOBJ=1,NSVOBJ
         IF (IOBJTP(ISVOBJ).EQ.2)
     & WRITE (JOSTND,*) 'IS2F(',ISVOBJ,')=',IS2F(ISVOBJ)
      ENDDO
   ENDIF
ENDIF
!
!     Compute, by source tree record and year of death:
!        1) Initial snag records created.
!        2) Snag records still standing.
!

IF (ICYC .EQ. 0) THEN
  DO IDEAD=1,NDEAD
    IF ( IYRCOD(IDEAD) .LE. IYEAR ) THEN
      XPROB = 0.0
      DO J=1,NDEAD
        IF ( OIDTRE(J) .EQ. OIDTRE(IDEAD) .AND.
     & IYRCOD(J) .EQ. IYRCOD(IDEAD) ) XPROB=XPROB+1
      ENDDO
      SPROBS(IDEAD,1) = XPROB
      SPROBS(IDEAD,3) = 1.0
    ENDIF
  ENDDO
ELSE
  DO IDEAD=1,NDEAD
    IF ( IYRCOD(IDEAD) .GE. IYEAR ) THEN
      XPROB = 0.0
      DO J=1,NDEAD
        IF ( OIDTRE(J) .EQ. OIDTRE(IDEAD) .AND.
     & IYRCOD(J) .EQ. IYRCOD(IDEAD) ) XPROB=XPROB+1
      ENDDO
      SPROBS(IDEAD,1) = XPROB
      SPROBS(IDEAD,3) = 1.0
    ENDIF
  ENDDO
ENDIF

DO IDEAD=1,NDEAD
  XPROB = 0.0
  DO J=1,NDEAD
    IF ( OIDTRE(J)  .EQ. OIDTRE(IDEAD) .AND.
     & IYRCOD(J)  .EQ. IYRCOD(IDEAD) .AND.
     & FALLDIR(J) .EQ. -1 ) XPROB=XPROB+1
  ENDDO
  SPROBS(IDEAD,2) = XPROB
ENDDO

IF (DEBUG) THEN
  WRITE (JOSTND,1040) ICYC, IYEAR,
     & NSNGS, NDEAD, NSVOBJ
1040   FORMAT (' ','LEAVING SVSNAD, ICYC=',I2,
     & ', IYEAR=',I4,':', / ,
     & ' ',T5,'NSNGS(add)=',I4,', NDEAD=',I4,', NSVOBJ=',I5,//,
     & ' ',T5,'SNAG LIST AFTER ADDING CUT/MORTALITY:',//,
     & ' ',T5,'  I   IDTREE  SPP ODIA OLEN IYRCOD STATUS ',
     & 'FALLDIR OPROB STDNG',/,
     & ' ',T5,'---- -------- --- ---- ---- ------ ------ ',
     & '------- ----- -----' )
!                       XXXX XXXXXXXX XXX XX.X XXX. XXXXXX XXXXXX
!                       XXXXXX. XXXX. XXXX.
  DO 300 I=1,NDEAD
    WRITE(JOSTND,1050) I, OIDTRE(I), ISNSP(I), ODIA(I), OLEN(I),
     & IYRCOD(I), ISTATUS(I),
     & FALLDIR(I), SPROBS(I,1), SPROBS(I,2)
1050     FORMAT(' ',T5,I4,1X,I8,1X,I3,1X,F4.1,1X,F4.0,1X,
     & I6,1X,I6,1X,F7.0,1X,F5.0,1X,F5.0)
300   CONTINUE
ENDIF
!
RETURN
END

