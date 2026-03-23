SUBROUTINE BRCANK
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!   BRCANK reads the canker data provided in the keyword list or
!   canker data file.
!   This routine is called from the subroutine BRIN.
!----------------------------------------------------------------------
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!
!  14-APR-1999 Lance R. David
!     Added code to process comments marked with * or ! in
!     the canker data file. * = echoed. ! = skipped.
!  03-APR-2001 Lance R. David (FHTET)
!     Changed order of variables and added stock type to canker file
!     read.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'HTCAL.f90'
INCLUDE 'ESTREE.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations.

CHARACTER*80 CREC
LOGICAL DEBUG
INTEGER IBRID,ISTK,JJ,NN
REAL    CAGE,CNKCNT,COUT,CUP,GGIRD

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRCANK',6,ICYC)
IF(DEBUG) WRITE(JOSTND,10) ICYC,ICIN,ICFMT
10 FORMAT('Entering subroutine BRCANK: cycle = ',I2,/, &
          ' ICIN=',I3,' ICFMT=',A)

!.... Read canker records. Note that all canker records will be read but
!.... a maximum of 10 cankers/tree will be tracked in the lethal canker
!.... array.  Also, a total canker count record is required and must be
!.... the last record listed for the tree.  This count will be set in
!.... the total canker count array for the tree but not loaded into the
!.... Blister Rust canker arrays.
!.... Tree's age is set to Prognosis stand age in subroutine BRSOR if
!.... tree age is not supplied on the canker record.

!.... Top of the canker record loop.
!.... If reading from an external file, read until the end of the file
!.... is reached.  If reading from supplemental records following the
!.... CANKDATA keyword, read until a "-999" is reached.
!.... Comments may exist in the canker data file. Records with a "*" or "!"
!.... in column 1 are comments. Records with a "*" are written to FVS
!.... standard output file. Records with "!" are not, just skipped.

90 CONTINUE

READ (ICIN,91,END=400) CREC
91 FORMAT (A)
IF (CREC(1:1).EQ.'!') GOTO 90
IF (CREC(1:1).EQ.'*') THEN
   WRITE (JOSTND,'(/,T12,A)') CREC
   GOTO 90
ENDIF

IBRID = 0
ISTK  = 0
CAGE  = 0.0
CUP   = 0.0
COUT  = 0.0
GGIRD = 0.0
CNKCNT= 0.0

!     READ(CREC,ICFMT) IBRID,UP,OUT,GGIRD,AGE,CNKCNT
IF(DEBUG) WRITE(JOSTND,*) CREC
READ(CREC,ICFMT) IBRID,ISTK,CAGE,CUP,COUT,GGIRD,CNKCNT
IF(IBRID.EQ.-999) GO TO 400

!.... If necessary, convert distance up from feet to centimeters
!.... and distance out from inches to centimeters.

IF(.NOT.LMETRIC) THEN
   CUP=CUP*30.48
   COUT=COUT*2.54
ENDIF

!.... Keep track of tree records encountered. Canker data will be
!.... loaded into the blister rust arrays according to unique
!.... tree IDs and then sorted back into the same arrays according
!.... to the internal tree numbers generated in the FVS model.

IF(INCAN.GT.0) THEN
   DO 110 JJ=1,INCAN
      IF(IBRID.EQ.IBRTID(JJ)) GO TO 120
110    CONTINUE
ENDIF
INCAN=INCAN+1
IF(INCAN.GT.MAXTRE) CALL ERRGRO(.FALSE.,13)
IBRTID(INCAN)=IBRID
JJ=INCAN
120 CONTINUE

!.... If UP, OUT, and % girdle are all zeros, this record was
!.... used to provide the, stock type, age, and total canker count
!.... for this tree.

IF(CUP.LE.0.0.AND.COUT.LE.0.0.AND.GGIRD.LE.0.0) THEN

!....    Set tree age if not 0.

   IF(CAGE.NE.0.0) BRAGE(JJ)=CAGE

!....    Set Stock type if provided.
!....    Default stock type is 5 which will be reassigned based on
!....    proportion of stock values in BRSTYP subroutine.

   IF(ISTK.GE.1 .AND. ISTK.LE.5) THEN
     ISTOTY(JJ)=ISTK
   ELSE
     ISTOTY(JJ)=5
   ENDIF

!....    ITCAN keeps a total count of cankers on the tree (lethal and
!....    non-lethal) no matter how many canker records are entered.
!....    Set ITCAN to the canker count number read from the canker
!....    list.  This canker count record is required to be the last one
!....    listed for the tree - that way we can make sure the count is
!....    at least at large as the number of lethal cankers read in.

   IF(CNKCNT.GT.0) ITCAN(JJ)=NINT(CNKCNT)
   IF(ITCAN(JJ).LT.ILCAN(JJ)) ITCAN(JJ)=ILCAN(JJ)
   IF(DEBUG) WRITE(JOSTND,*) ' ID=',IBRTID(JJ),' AGE=',BRAGE(JJ), &
        ' STOCK=',ISTOTY(JJ),' ITCAN=',ITCAN(JJ),' ILCAN=',ILCAN(JJ)
   GO TO 90
ENDIF

!.... Increment the counter for "lethal cankers".

ILCAN(JJ)=ILCAN(JJ)+1
NN=ILCAN(JJ)
IF(NN.GT.10) THEN

!....    A maximum of 10 cankers per tree will be tracked by the model
!....    in the "lethal cankers" array. If we've passed that maximum
!....    then subtract one.

   ILCAN(JJ)=ILCAN(JJ)-1
   GO TO 90
ENDIF

!.... Load the canker arrays. This data will be sorted in subroutine
!.... BRSOR to keep the canker data in synch with FVS tree data.
!.... Percent girdle for branch cankers is set to zero and cankers
!.... cannot have a negative distance out.

DUP(NN,JJ)=CUP

IF(COUT.LT.0.0) THEN
   DOUT(NN,JJ)=0.0
ELSE
   DOUT(NN,JJ)=COUT
ENDIF

IF(COUT.GT.0.0) THEN
   GIRDL(NN,JJ)=0.0
ELSE
   GIRDL(NN,JJ)=GGIRD
ENDIF

!.... Go read another canker record.

GO TO 90

!.... Common return.

400 CONTINUE
IF(DEBUG) WRITE(JOSTND,300) ICYC
300 FORMAT('Leaving subroutine BRCANK: cycle = ',I2)
RETURN
END
