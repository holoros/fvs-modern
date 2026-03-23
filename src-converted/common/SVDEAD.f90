!ODE SEGMENT SVDEAD
!----------
! COMMON $Id$
!----------
!     COMMON AREA FOR SNAGS
!----------
!
INTEGER    MXDEAD
PARAMETER (MXDEAD=1000)
!----------
!     MXCWD is set to half of MXSVOB (see SVDATA)
!----------
!
INTEGER    MXCWD
PARAMETER (MXCWD=5000)
!
INTEGER    ILYEAR,NCWD,NDEAD
!
INTEGER    ISNSP(MXDEAD),ISTATUS(MXDEAD),IYRCOD(MXDEAD), &
              OIDTRE(MXDEAD)
!
REAL       CRNDIA(MXDEAD),CRNRTO(MXDEAD),CWDDIA(MXCWD), &
              CWDDIR(MXCWD),CWDLEN(MXCWD),CWDPIL(MXCWD), &
              CWDWT(MXCWD),FALLDIR(MXDEAD),HRATE(MAXSP), &
              ODIA(MXDEAD),OLEN(MXDEAD),PBFALL(MXDEAD), &
              SNGCNWT(MXDEAD,0:3),SNGDIA(MXDEAD),SNGLEN(MXDEAD), &
              SPROBS(MXDEAD,3),YHFHTH(MAXSP),YHFHTS(MAXSP)
!
COMMON /SVDEAD/ CRNDIA,CRNRTO,CWDDIA,CWDDIR,CWDLEN,CWDPIL,CWDWT, &
                   FALLDIR,HRATE,ILYEAR,ISNSP,ISTATUS,IYRCOD,NCWD, &
                   NDEAD,ODIA,OIDTRE,OLEN,PBFALL,SNGCNWT,SNGDIA, &
                   SNGLEN,SPROBS,YHFHTH,YHFHTS
!
!----------
!  VARIABLE DEFINITIONS:
!----------
!     CRNDIA = CROWN DIAMETER OF SNAG AT TIME OF DEATH
!     CRNRTO = CROWN RATIO OF SNAG AT TIME OF DEATH
!     CWDDIA = DIAMETER OF THE CWD OBJECT (ZERO IF UNUSED)
!     CWDDIR = ORIENTATION OF THE CWD OBJECT
!     CWDLEN = LENGTH OF THE CWD OBJECT
!     CWDPIL = PILED(1)/UNPILED(0) STATUS OF THE CWD OBJECT
!     CWDWT  = WEIGHT (TONS) OF CWD OBJECT
!    FALLDIR = A VECTOR TO DETERMINE IF A SNAG IS STANDING
!              0-360 = DIRECTION OF FALL
!              -1    = THE SNAG IS STANDING
!     HRATE  = THE RATE MODIFIER FOR HEIGHT LOSS
!     ILYEAR = THE LAST YEAR ALL SNAGS WAS CALLED
!     ISNSP  = SPECIES CODE FOR SNAG
!    ISTATUS = STATUS OF THE SNAG
!              (IF NEGATIVE, INDICATES THAT SNAG IS TO BE SALVAGED,
!               AND IS REMOVED IN THE NEXT CALL TO SVOUT)
!              0 = OPEN SLOT (NO SNAG)
!              1 = GREEN, HARD SNAG
!              2 = RED, HARD SNAG
!              3 = GREY, HARD SNAG
!              4 = GREY, SOFT SNAG
!              5 = BLACK, RECENTLY BURNED, STILL HAS CROWN
!              6 = BLACK, OLDER BURNED, NO CROWN
!             90 = WWPB MORTALITY, ONE OR LESS YEARS OLD (RED)
!             91 = WWPB MORTALITY, TWO YEARS OLD (DARK)
!             92 = WWPB MORTALITY, 3-4 YEARS OLD (GREY)
!     IYRCOD = THE YEAR OF DEATH
!     NCWD   = THE TOTAL NUMBER OF CWD RECORDS, FOR SVS
!     NDEAD  = THE TOTAL NUMBER OF SNAGS
!     ODIA   = ORIGINAL TREE DIAMETER
!     OIDTRE = Original IDTREE identifier that generated snag record
!     OLEN   = ORIGINAL TREE LENGTH
!     PBFALL = Post-burn fall rate
!    SNGCNWT = SNAG CROWN WEIGHT BY SIZE CLASS (NOT ALL
!              CLASSES ARE STORED): 0 : Foliage, 1 : <0.25
!              2 :  0.25 - 1, AND 3 : 1 - 3. THESE VALUES
!              ARE AS OF THE YEAR THE SNAG WAS BORN UNLESS
!              THAT YEAR IS PRIOR TO THE INVENTORY YEAR.
!     SNGDIA = Current snag diameter.
!     SNGLEN = Current snag heights (lengths).
!     SPROBS = Trees/ac snag stockings:
!              SPROBS(I,1): snag recs, for original source tree rec and
!                           year of death, created at time of mortality.
!              SPROBS(I,2): snag recs, for original source tree rec and
!                           year of death, that are still standing.
!              SPROBS(I,3): expected snag/acre count for current snag
!                           record, if fractional fall probabilities
!                           were applied each year since death (as opposed
!                           to actual stochastic fall prediction)
!     YHFHTH = NUMBER OF YEARS AT WHICH HALF THE INITIAL
!              HEIGHT HAS BEEN LOST, FOR HARD SNAGS
!     YHFHTS = NUMBER OF YEARS AT WHICH HALF THE INITIAL
!              HEIGHT HAS BEEN LOST, FOR SOFT SNAGS
!
!-----END SEGMENT

