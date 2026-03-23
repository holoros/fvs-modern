SUBROUTINE BMSANI (IYR)
!----------
! WWPB $Id$
!----------
!     CALLED FROM BMDRV
!     CALLS:  BMSLSH
!**********************************************************************
! **BMSANI    Date of last revision:  09/28/05
!**********************************************************************
!             revised 7/28/05: added new var: REMBKP(ISTD)to bookkeep the BKP removed
!             via sanitation cutting, by stand. In common.  For output.  AJM
!             REVISED 6/30/05.  TREE, BAH, and AREMS arrays should only be adjusted
!             when removing unattacked, low-RV trees.  COmmented out the adjustments
!             happening to these arrays in the block where recently attacked trees
!             are being sanitized. AJM
!             Date of last revision 8/99(AJM).  Commented out block
!             referring to Target Volume removal method of calculating
!             amount to sanitize.  Target volume no longer a user-input
!             option.  The referred-to PRMS(6) does not exist.
! **BMSANI    Date of last revision:  October 28, 1998 (RNH, OCT98)
!             >>>>>Remove low RV trees from highst Size to lowest based on<<<<<
!             linearly varying efficiency
! **BMSANI    Date of last revision:  October 16, 1998 (RNH, OCT98)
!             Added target volume removal method for sanitize management
! **BMSANI    Date of last revision:  September 16, 1998 (RNH, May98)
!             Added target basal area method for sanitize management
! **BMSANI    Date of last revision:  August 5, 1998 (RNH, May98)
!             Passed MYLSTS, MAXSCS and MINSCS to BMKILL to track
!             Sanitize removals in BMKILL
! **BMSANI    Date of last revision:  June 21, 1994
! **BMSANI    Date of last revision:  May 6, 1998 (RNH, May98)
!            Major revision to run Sanitize as stand level activity,
!            similar to SALVAGE keyword.  Also modified BMPPIN to
!            pass user specified stand list, from keyword file or
!            from user specified file, to BMSANI like SALVAGE keyword
!
!  Sanitation cuts are a landscape-level action. If conditions are right,
!  the cut could occur anywhere in the landscape. All beetle-killed trees
!  and low RV trees within the given size range will be removed.
!
!  NOTE THAT WE MAY WANT TO CHANGE THIS TO MAKE IT STAND-ACTIVATED (LIKE PHEROMONES)
!            Done May98 by RNH
!
!  Definitions:
!     IPC:    Loop counter over pool types (fast, med, slow)
!     ISIZ:   Loop counter over size classes
!     MINSC:  Minimum size class for cut
!     MAXSC:  Maximum size class for cut
!     RVMAX:  Maximum RV that will be removed (of unattacked trees)
!     REMOVE: Amount of TPA removed from one size class of recent kills
!     SUM:    Total amount of standing dead volume (all classes)
!     VREMOV: Amount of standing dead volume removed over size classes
!
!  Common block variables and parameters:
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'PPCNTL.f90'
INCLUDE 'BMPRM.f90'
INCLUDE 'BMCOM.f90'
INCLUDE 'BMPCOM.f90'
!.... Variable declarations.

INTEGER ISIZ, IPC
INTEGER MINSC, MAXSC
INTEGER DUM(1), INDEX(NSCL)
!
!     Add required Variables for stand level simulation (RNH May98)
!
INTEGER MYLST(MXSTND), SCNT
!
LOGICAL LOK
REAL    REMPB, REMIPS
REAL    REMOVE, RVMAX
REAL    SUM
REAL    VREMOV, TARVR
REAL    PRMS(7), AA(NSCL)

REAL    THRVOL, EFFC

SAVE
!
!
IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
10 FORMAT(' Begin BMSANI: Year= ',I5, 'Stand= ', I6)

!     Initialization
!
!      Move to loop 500
!
!      IPC = IQPTYP(ISTD,1)
!      SUM = 0.0
!      VREMOV = 0.0

!     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.
!
!      IF (ICNT .GE. BMEND) ICNT = 0
!      ICNT = ICNT + 1
!
!      IF (ICNT .EQ. 1) THEN
!
!
!     Zero out MYLST array from last year (RNH 10Aug98)
!
DO 11 IJK= 1, MXSTND
MYLST(IJK)= 0.0
VOLREM(IJK,1)=0.0 !NEW AJM 9/05
   DO 12 ISIZ= 1, NSCL
      AAREMS(IJK,ISIZ)= 0.0  !NEW AJM 9/05  THIS WILL BE JUST LIKE AREMS ONLY ZEROED ANNUALLY FOR USE IN BMOUT
12    CONTINUE
11 CONTINUE
!
IYR1= IYR
NPRMS= 6
MAXPRM = 7       !   From SALVAGE (RNH)
!
CALL GPGET2 (308,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
!       CALL GPGET2 (308, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)
!
!     Set LOKS to control Sanitize accounting in BMKILL (RNH Aug98)
!     LOKS should remain TRUE thoughout the cycle to trigger calculations
!     in BMKILL.  LOKS1 is set to FALSE in BMKILL to signal end of SANI action

  LOKS = LOK
  IF (LOKS) LOKS1 = .TRUE.
  IF (LOKS1) LOKS = .TRUE.

  IF (LOK) THEN

    MINSC = INT(PRMS(1))
    MAXSC = INT(PRMS(2))
    RVMAX = PRMS(3)
    THRVOL = PRMS(4)
    EFFC = PRMS(5)
    TARVR= PRMS(6)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     The following variables are set here to be passed to BMKILL to
!     track Sanitization removals in the PPE (MAXSCS, MINSCS,
!     MYLSTS) (RNH Aug98)
!
MINSCS = MINSC
MAXSCS = MAXSC
!
!     Append the MYLSTS array with the current years sanitiation
!     removals from MYLST
!
IJ0= 1
DO 19 IJ= 1, MXSTND
IF(MYLSTS(IJ) .NE. 0) GO TO 19
MYLSTS(IJ) = MYLST(IJ0)
IJ0= IJ0 + 1
19 CONTINUE
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
    IF (LBMDEB) WRITE (JBMBPR,71) MICYC, IYR1, MINSC, MAXSC, &
                                     RVMAX, THRVOL, EFFC
71     FORMAT (' BMSANI: MICYC=', I5,' IYR1=',I5,' MINSC=',I4, &
         ' MAXSC=',I4,' RVMAX=',F5.2,' THRVOL=',F8.0,' EFFC=',F5.3)

   ELSE
    EFFC = 0.0
  ENDIF
!      ENDIF

IF (EFFC .LE. 0.0) RETURN

!     Main loop to calculate stands +++++++++++++++++++++++++++++
!
!     The folling loop over SCNT was suggested from BMSALV
!
DO 500 I = 1, SCNT
!
    ISTD = MYLST(I)
    IPC = IQPTYP(ISTD,1)
    IF (.NOT.STOCK(ISTD).OR.ISTD.LE.0) GOTO 500
!
!     Total recently killed volume or low RV volume available for cut
!
VREMOV = 0.0
SUM = 0.0
SUMBA = 0.0
!
!  Calc. total basal area of stand (SBAT10)
!
!      SBAT10 = 0.0
!      DO 14 ISBA1 = 1, NSCL
!      BAHPNH = BAH(ISTD,ISBA1) + BANH(ISTD,ISBA1)
!      SBAT10 = SBAT10 + BAHPNH
!   14 CONTINUE
!
DO 20 ISIZ= MINSC,MAXSC
   SUM = SUM + (PBKILL(ISTD,ISIZ) + ALLKLL(ISTD,ISIZ)) &
                            * TVOL(ISTD,ISIZ,1)*0.90
   IF (GRF(ISTD,ISIZ) .LE. RVMAX) &
          SUM = SUM + TREE(ISTD,ISIZ,1) * TVOL(ISTD,ISIZ,1)*EFFC
!
!
20 CONTINUE
!
!     If not enough volume to warrent a sanitation cut then
IF (SUM .LT. THRVOL) GO TO 500
!
!     If the target basal area method of sanitiation is being used
!     TARBA < 999. then start calculations below.  Else, brance to
!     statement 35 to begin sanitation operations based on maximum
!     rating value method TARBA >= 999. (RNH sep98)
!
!      IF (TARVR .GE. 9999.) GO TO 35
!
!      WRITE (*,*) SUM,THRVOL
GO TO 35
!                                                                                    !
!...  Above change made 8/99 (AJM)                                                   !
!                                                                                    !
                                                                               !
!     New variables                                                                  !
!     NN        = number of size classes to be included in sanitization              !
!     AA(MAXSC) = scatch array to pass rating values of size classes                 !
!                 (MINSC TO MAXSC) to sort routine                                   !
!     TARVR     = stand target removal volume specified by user in input             !
!     INDEX(NN) = passed fromn sort routine this array holds the subscrips           !
!                 of AA in order of accending RV                                     !
!     INDRV     = subscript values from INDEX transformed to range                   !
!                 MINSC - MAXSC for use in other arrays                              !
!     SRVOL1    = temporary variable holding removal volumes, used to compare        !
!                 with TARVR                                                         !
!                                                                                    !
!     The following routine removes infected trees first.  Then the                  !
!     tree size classes are removed based on rating value, with the                  !
!     lowest rating value trees cut first, until the target removal volume           !
!     (TARVR) for the stand is reached.                                              !
!                                                                                    !
!     First take out all infected trees                                              !
!                                                                                    !
REMLRV= 0.0                                                                    !
SRVOL1= 0.0                                                                    !
                                                                               !
DO 22 ISIZ= MINSC,MAXSC                                                        !
!                                                                                    !
   REMPB = PBKILL(ISTD,ISIZ) * 0.90                                            !
   PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - REMPB                               !
!                                                                                    !
   REMIPS = ALLKLL(ISTD,ISIZ) * 0.90                                           !
   ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) - REMIPS                              !
!                                                                                    !
!     Changed removal efficiency of beetle killed trees to 0.90 obove                !
!     Accounted for these removal in tree record before removing low                 !
!     RV trees.  calculated reduction in basal area properly (RNH19Aug)              !
!                                                                                    !
TREE(ISTD,ISIZ,1) = TREE(ISTD,ISIZ,1) - REMPB - REMIPS                         !
BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) - BAH(ISTD,ISIZ)*(REMPB + REMIPS)               !
!                                                                                    !
SRVOL1= SRVOL1 +(REMPB + REMIPS)*TVOL(ISTD, ISIZ, 1)                           !
                                                                               !
   REMOVE = REMPB + REMIPS + REMLRV                                            !
   VREMOV = VREMOV + REMOVE * TVOL(ISTD,ISIZ,1)                                !
                                                                               !
!        Also remove some wood from the current standing dead pools because          !
!        the killed trees were added to these pools last year (so use subscript      !
!        2 which means age 1). Multiply REMOVE by the decay factor to account        !
!        for one year's decay. Note that SDWP is measured in volume.                 !
                                                                               !
   JSC = L2D(ISIZ) + 1                                                         !
   JJ = MIN0(JSC,MXDWSZ)                                                       !
   SDWP(ISTD,IPC,JSC,2) = SDWP(ISTD,IPC,JSC,2) - (REMOVE - REMLRV) &  !
                                     * TVOL(ISTD,ISIZ,1) * SDECRT(JJ)             !
                                                                               !
   SREMOV(ISTD) = SREMOV(ISTD) + REMOVE                                        !
AREMS(ISTD,ISIZ)= AREMS(ISTD,ISIZ) + REMPB + REMIPS                            !
!                                                                                    !
22 CONTINUE                                                                       !
!                                                                                    !
!*************                                                                       !
!     Take out rating value ranking code and remove trees from largest               !
!     size class to lowest based on linearly varying efficiencyN (EFFCLN)            !
!     RNH October 28, 1998                                                           !
!*************                                                                       !
!                                                                                    !
27 CONTINUE             ! Branch from end of loop if TARBA not met                !
!                                                                                    !
DO 28 IJK= MAXSC, MINSC, -1                                                    !
!                                                                                    !
!     calculate efficiency based on size class.  Want maximum size class             !
!     cut at input efficiency and size class 3 cut at efficiency= 0.0                !
!     The efficiency for cutting size classes (EFFCLN) in between                    !
!     should vary linearly                                                           !
!                                                                                    !
IF (IJK .GT. 3) THEN                                                           !
EFFCLN= EFFC*(IJK-3)/(MAXSC-3)                                                 !
ELSE                                                                           !
EFFCLN= 1.0E-6                                                                 !
ENDIF                                                                          !
!      WRITE(29,*) ' SIZCLASS= ',IJK,' EFFCLN= ',EFFCLN,' ISTD= ',ISTD               !
!      WRITE(29,*)' MINSC= ',MINSC,' MAXSC= ',MAXSC                                  !
!                                                                                    !
IF (GRF(ISTD,IJK) .LE. RVMAX) THEN                                             !
!                                                                                    !
 IF (SRVOL1 .LT. TARVR) THEN                                                   !
!                                                                                    !
!     SRVOL2 is temporary storage for SRVOL1 in refiinement of removals              !
!     to meet TARVR                                                                  !
!                                                                                    !
 SRVOL2 = SRVOL1                                                               !
!                                                                                    !
 REMLRV= TREE(ISTD,IJK,1)*EFFCLN                                               !
!                                                                                    !
IF(REMLRV .LE. 1.0E-6) REMLRV= 0.0                                             !
!                                                                                    !
!     Accumulate removals in SRVOL1                                                  !
!                                                                                    !
SRVOL1= SRVOL1 + REMLRV*TVOL(ISTD, IJK, 1)                                     !
!                                                                                    !
!      If removal accumulates above TARVR then use efficiency to refine              !
!      removal volume to approximate TARVR                                           !
!                                                                                    !
  IF (SRVOL1 .GT. TARVR) THEN                                                  !
                                                                               !
  EFFCLN1= (TARVR - SRVOL2)/(TREE(ISTD,IJK,1)*TVOL(ISTD,IJK,1))                !
!                                                                                    !
!       Recalculate REMLRV based on refined efficiency estimate                      !
!                                                                                    !
  REMLRV= TREE(ISTD,IJK,1)*EFFCLN1                                             !
  SRVOL1 = SRVOL2 + REMLRV*TVOL(ISTD,IJK,1)                                    !
  BAH(ISTD,IJK)= BAH(ISTD,IJK)*(1-EFFCLN1)                                     !
  IJK1= IJK                                                                    !
!                                                                                    !
!                                                                                    !
  GO TO 1010                                                                   !
!                                                                                    !
  END IF                                                                       !
!                                                                                    !
IF(BAH(ISTD,IJK) .LE. 1.0E-6) BAH(ISTD,IJK) = 0.0                              !
!                                                                                    !
                                                                               !
 BAH(ISTD,IJK)= BAH(ISTD,IJK) * (1 - EFFCLN)                                   !
!                                                                                    !
1010 CONTINUE                                                                       !
!                                                                                    !
!+++++++++++++                                         ++++++++++++++                !
!      Accumulate removal data to pass to BMKILL for Sanitize accounting             !
!      in PPE (RNH Aug98)                                                            !
!                                                                                    !
 AREMS(ISTD,IJK)= AREMS(ISTD,IJK) + REMLRV                                     !
!                                                                                    !
!++++++++++++                                          ++++++++++++++                !
!                                                                                    !
 TREE(ISTD,IJK,1) = TREE(ISTD,IJK,1) - REMLRV                                  !
!                                                                                    !
 REMOVE = REMLRV                                                               !
 VREMOV = VREMOV + REMOVE * TVOL(ISTD,IJK,1)                                   !
!                                                                                    !
!        Also remove some wood from the current standing dead pools because          !
!        the killed trees were added to these pools last year (so use subscript      !
!        2 which means age 1). Multiply REMOVE by the decay factor to account        !
!        for one year's decay. Note that SDWP is measured in volume.                 !
!                                                                                    !
!         JSC = L2D(IJK) + 1                                                         !
!         JJ = MIN0(JSC,MXDWSZ)                                                      !
!         SDWP(ISTD,IPC,JSC,2) = SDWP(ISTD,IPC,JSC,2) - (REMOVE - REMLRV)            !
!     &                                  * TVOL(ISTD,IJK,1)*SDECRT(JJ)               !
!                                                                                    !
   SREMOV(ISTD) = SREMOV(ISTD) + REMOVE                                        !
!                                                                                    !
ELSE                                                                           !
!                                                                                    !
!      Met TARVR criterium, finished with sanitize                                   !
!                                                                                    !
!      WRITE(29,*) ' SRVOL1= ',SRVOL1,' SREMOV= ', SREMOV(ISTD)                      !
!      WRITE(29,*) ' VREMOV= ',VREMOV                                                !
!                                                                                    !
 GO TO 50                                                                      !
 END IF                                                                        !
END IF                                                                         !
28 CONTINUE                                                                       !
!                                                                                    !
!     If loop through stand did not meet TARVR criterium then loop through again     !
!                                                                                    !
!                                                                                    !
IF (SRVOL1 .LT. TARVR) GO TO 27                                                !
!                                                                                    !
!                                                                                    !
GO TO 50                 !End of TARVR sanitize method loop                    !
!                                                                                    !
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     !
!                                                                                    !
!     Branch from TARBA test (ie method of sanitization)                             !
!C     Target stand removal volume method of Sanitization
35 CONTINUE
!
!     Insert section of code from above defining EFFCLN (8/10/99, AJM).
!
!      IF (IJK .GT. 3) THEN
!       EFFCLN= EFFC*(IJK-3)/(MAXSC-3)
!       ELSE
!       EFFCLN= 1.0E-6
!      ENDIF
!
!     Maximum rating value mehtod of Sanitation
!     Remove trees from each appropriate size class based on the
!     desired efficiency
!
DO 40 ISIZ= MINSC,MAXSC
!         REMPB = PBKILL(ISTD,ISIZ) * EFFC
   REMPB = PBKILL(ISTD,ISIZ) * 0.90
   PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - REMPB                   ! REMOVE ATTACKED TREES

!         REMIPS = ALLKLL(ISTD,ISIZ) * EFFC
   REMIPS = ALLKLL(ISTD,ISIZ) * 0.90
   ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) - REMIPS                   !DITTO
!
!     New var for bookkeeping bkp removal (FOR OUTPUT). ajm 7/05
!     Note: this assumes that each dead beetle-killed tree removed via sanitation
!     (i.e. the decrements from the PBKILL array) is fully occupied with BKP.  \

   REMBKP(ISTD) = REMBKP(ISTD) + REMPB * MSBA(ISIZ)                !WHERE IS THIS ZEROED?AJM

!     Changed removal efficiency of beetle killed trees to 0.90 obove
!     Accounted for these removal in tree record before removing low
!     RV trees.  calculated reduction in basal area properly (RNH19Aug)
!
! COMMENTING OUT THE NEXT THREE LINES AJM 6/30/05
! TREE AND BAH ARRAYS WERE ALREADY DECREMENTED IN BMISTD IF THE TREES WERE BEETLE-KILLED,
! WHICH THIS SECTION IS REMOVING.  LIKEWISE, AREMS IS BEING BOOKEPT *ONLY* FOR
! POSTING TO BASE-FVS WK2 ARRAY IN BMKILL.  SINCE THESE TREES REMOVED VIA SANIT
! ARE ALREADY DEAD, THEY'VE ALREADY BEEN ADDED TO THE TPBK ARRAY, SO ARE ALREADY
! ACCOUNTED FOR.  REMOVALS POSTED TO WK2 ARRAY FROM SANI SHOULD ONLY REFLECT THOSE
! LIVE UNATTACKED TREES REMOVED VIA SANIT.  SEE NEXT IF...THEN BLOCK.
!
!      TREE(ISTD,ISIZ,1) = TREE(ISTD,ISIZ,1) - REMPB - REMIPS
!      BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) - MSBA(ISIZ)*(REMPB + REMIPS)
!      AREMS(ISTD,ISIZ)= AREMS(ISTD,ISIZ) + REMPB + REMIPS
!
!
   IF (GRF(ISTD,ISIZ) .LE. RVMAX) THEN                               !NOW REMOVE LOW RV TREES
!           REMLRV = TREE(ISTD,ISIZ,1) * EFFCLN
      REMLRV = TREE(ISTD,ISIZ,1) * EFFC
!
!... Changed above formula [and commented out block above(ca lines 388-92)]
!     so that now efficiency is NOT modified by size class, but remains
!     constant at the user-specified level from field 7 in SANITIZE
!     keyword; default = 0.95 (set in BMPPIN).  AJM 8/11/99
!+++++++++++++                                         ++++++++++++++
!     Accumulate removal data to pass to BMKILL for Sanitize accounting
!     in PPE (RNH Aug98)
!
      AREMS(ISTD,ISIZ)= AREMS(ISTD,ISIZ) + REMLRV
      AAREMS(ISTD,ISIZ)= REMLRV                    ! NEW AJM 9/05 ANNUAL ACCUMULATOR REZEROED ANNUALLY (ABOVE) FOR OUTPUT
!            BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) * (1 - EFFCLN) ! COMMENT OUT AJM 6/30/05
!                           ! THIS SHOULD'VE BEEN CHANGED 8/99 WHEN EFFCLN WAS NIXED
      BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) * (1 - EFFC) ! ADD 6/30/05 AJM
!
!      BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) - MSBA(ISIZ)*EFFC
      TREE(ISTD,ISIZ,1) = TREE(ISTD,ISIZ,1) - REMLRV
   ELSE
      REMLRV = 0.0
   ENDIF
!
   REMOVE = REMLRV + REMPB + REMIPS
   VREMOV = VREMOV + REMOVE * TVOL(ISTD,ISIZ,1)

!        Also remove some wood from the current standing dead pools because
!        the killed trees were added to these pools last year (so use subscript
!        2 which means age 1). Multiply REMOVE by the decay factor to account
!        for one year's decay. Note that SDWP is measured in volume.

   JSC = L2D(ISIZ) + 1
   JJ = MIN0(JSC,MXDWSZ)
   SDWP(ISTD,IPC,JSC,2) = SDWP(ISTD,IPC,JSC,2) - (REMOVE - REMLRV) &
                                     * TVOL(ISTD,ISIZ,1) * SDECRT(JJ)

   SREMOV(ISTD) = SREMOV(ISTD) + REMOVE

40 CONTINUE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     Branch from TARBA method
!
50 CONTINUE
!
VOLREM(ISTD,1) = VOLREM(ISTD,1) + VREMOV ! NEW ANNUAL ACCUMULATOR AJM 6/05
CVOLREM(ISTD,1) = CVOLREM(ISTD,1) + VREMOV
!
!     Calculate amount of slash produced from sanitation

CALL BMSLSH (IPC,1.0,VREMOV,ISTD)

!     Re-calculate GRF in case any low RV trees were removed.
!                                        !note 7/28/05 ajm
                                   ! this step may not be necessary, because
TOTAL = 0.0                        ! GRFs are recalculated after this sanitation (call to BMCGRF from BMDRV)
GRFSTD(ISTD)= 0.0                  ! but before all of the BKP dynamics that needs the GRFs
DO 200 ICLS=1,NSCL                 ! i.e. we don't need to derive this here and now
  X = BAH(ISTD,ICLS)
  TOTAL= TOTAL + X
  GRFSTD(ISTD)= GRFSTD(ISTD) + GRF(ISTD,ICLS) * X
200 CONTINUE

IF (TOTAL .GT. 1.0E-9) THEN
  GRFSTD(ISTD)= GRFSTD(ISTD) / TOTAL
ELSE
  GRFSTD(ISTD)= 1.0
ENDIF

IF(LBMDEB) WRITE(JBMBPR,99) IYR, ISTD
99 FORMAT(' End BMSANI: Year= ',I5, 'Stand= ', I6)
!
500 CONTINUE
!
!     End of main stand loop +++++++++++++++++++++++++
!
RETURN
END
