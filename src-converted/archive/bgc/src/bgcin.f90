SUBROUTINE BGCIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
!----------
!  **BGCIN  BGC--DATE OF LAST REVISION: 05/21/13 AJM. ADDED 10TH ARG TO KEYRDR CALL
!                10/15/99
!                Revised 11/12/02.  Removing index ISTND, and removing PPE
!                                   common "includes" (PPEPRM, PPCNTL, &
!                                   PRGPRM).  AJM
!                These changes--also made in BGCFVS, BGCGROW, BGCINT, BGCGO,
!                BINITIAL, and BGCCOM.f77--remove all PPE funtionality.
!                The FVS-BGC code is now, once again, a single stand model.
!----------
!
!     OPTION PROCESSOR FOR THE BGC EXTENSION
!
!     CALLED FROM: INITRE
!
!OMMONS
!
!
INCLUDE 'BGCCOM.f90'
INCLUDE 'ENTITY.f90'
INCLUDE 'PRGPRM.f90'
!      INCLUDE 'PPCNTL.F77'  ! removed 11/02 ajm
INCLUDE 'CONTRL.f90'
!
!
!OMMONS
!
PARAMETER (KWCNT = 12)
CHARACTER*8 TABLE, KEYWRD, PASKEY
CHARACTER*10 KARD(7)
LOGICAL LNOTBK,LKECHO
DIMENSION PRMS(8),KNDX1(5),KNDX2(33)
DIMENSION ARRAY(7),TABLE(KWCNT),LNOTBK(7)
CHARACTER*1 ATTRIB1
CHARACTER*2 ATTRIB2
CHARACTER*7 ENDFLAG
DATA TABLE / &
        'END     ','UNDERVEG','VEGOPT  ','BGCGROW ','        ', &
        '        ','        ','        ','        ','        ', &
        '        ','        '/
!
!     **********          EXECUTION BEGINS          **********
!----------
!----------
10 CONTINUE
CALL KEYRDR (IREAD,JOSTND,.FALSE.,KEYWRD,LNOTBK, &
                ARRAY,IRECNT,KODE,KARD,.FALSE.,LKECHO)
PRINT *,'in bgcin, after Keyrdr, keywrd= ', keywrd
PRINT *,'in bgcin, after Keyrdr, Kode= ', kode
!
!     RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK,2=EOF
!
IF (KODE .EQ. 0) GOTO 30
IF (KODE .EQ. 2) CALL ERRGRO(.FALSE.,2)
CALL ERRGRO (.TRUE.,6)
GOTO 10
30 CONTINUE
CALL FNDKEY (NUMBER,KEYWRD,TABLE,KWCNT,KODE,.FALSE.,JOSTND)
PRINT *,'in bgcin, after FNDKEY, Number= ', number
PRINT *,'in bgcin, after FNDKEY, keywrd= ', keywrd
!
!     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.
!
IF (KODE .EQ. 0) GOTO 90
IF (KODE .EQ. 1) THEN
   CALL ERRGRO (.TRUE.,1)
   GOTO 10
ENDIF
GOTO 90
!
!     SPECIAL END-OF-FILE TARGET
!
80 CONTINUE
CALL ERRGRO (.FALSE.,2)
90 CONTINUE
!
!     SIGNAL THAT THE BGC EXTENSION IS NOW ACTIVE.
!
PRINT *,'in bgcin, lbgcon=',lbgcon
!      IF (.NOT.LBGCON(ISTND)) THEN ! removed 11/02 ajm
!         LBGCON(ISTND) = .TRUE.    ! ditto
IF (.NOT.LBGCON) THEN
   LBGCON = .TRUE.
   CALL BGCINITALL
ENDIF
!
!     PROCESS OPTIONS
!
GO TO( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000, &
         1100,1200), NUMBER

100 CONTINUE
PRINT *,'in bgcin, option=1, keyword= ', keywrd
!========== OPTION NUMBER 1: END ==========================
IF(LKECHO)WRITE(JOSTND,110) KEYWRD
110 FORMAT (/A8,'   END OF BGC OPTIONS.')
RETURN
200 CONTINUE
PRINT *,'in bgcin, option=2, keyword= ', keywrd
!========== OPTION NUMBER 2: UNDERVEG =====================
IF(LKECHO)WRITE(JOSTND,205) KEYWRD
205 FORMAT (/A8,'   STAND-BGC UNDERSTORY VEGETATION INPUT')
206 READ(IREAD,*)ENDFLAG
DO 210 I=1,7
   CALL UPCASE(ENDFLAG(I:I))
210 CONTINUE
IF(ENDFLAG.NE.'ENDENT') THEN
   BACKSPACE IREAD
   READ(IREAD,*) ATTRIB1, ATTRIB2, ATTRIB3, ATTRIB4
   NVE=NVE+1
   VHT(NVE) = ATTRIB3
   VCOV(NVE) = ATTRIB4
   VSP(NVE) = ATTRIB2
   VID(NVE) = ATTRIB1
   GOTO 206
END IF
PRINT *,'in bgcin, option=2, end of veg data'
PRINT *,'in bgcin, option=2, no. of veg entities= ', NVE
GOTO 10
300 CONTINUE
PRINT *,'in bgcin, option=3, keyword= ', keywrd
!=================OPTION NUMBER 3: VEGOPT =============
ISOURCE=IFIX(ARRAY(1))
PRINT *,'in bgcin, option=3, isource= ', ISOURCE
IF(LKECHO)WRITE(JOSTND,305) KEYWRD,ISOURCE
305 FORMAT (/A8,'   SOURCE OF VEG DATA= ', I1)
GOTO 10
400 CONTINUE
!=================OPTION NUMBER 4: BGCGROW ============
!      IBGC(ISTND)=1  ! removed 11/02 ajm
IBGC=1
PRINT *,'in bgcin, option=4'
IF(LKECHO)WRITE(JOSTND,405) KEYWRD
405 FORMAT (/A8,'   INCREMENTS WILL BE FROM STAND-BGC')
GOTO 10
500 CONTINUE
!=================OPTION NUMBER 5: BGCIN ==============
GOTO 10
600 CONTINUE
!                        OPTION NUMBER 6 --
GOTO 10
700 CONTINUE
!                        OPTION NUMBER 7 --
GOTO 10
800 CONTINUE
!                        OPTION NUMBER 8 --
GOTO 10
900 CONTINUE
!                        OPTION NUMBER 9 --
GOTO 10
1000 CONTINUE
!                        OPTION NUMBER 10 --
GOTO 10
1100 CONTINUE
!                        OPTION NUMBER 11 --
GOTO 10
1200 CONTINUE
!                        OPTION NUMBER 12 --
GOTO 10
!
!     Special entry to retrieve keywords.
!
ENTRY BGCKEY (KEY,PASKEY)
PASKEY= TABLE(KEY)
RETURN
END


