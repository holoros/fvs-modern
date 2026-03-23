SUBROUTINE BRINIT
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  Initializes the Blister Rust model variables.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  27-MAY-1999 Lance David
!     Added initialization of RIAF (rust index adjustment factor).
!  03-JUN-1999 Lance David
!     Added logical variables BRTHDR and BRCHDR for canker and tree
!     list main header printing control.
!  13-DEC-2000 Lance R. David (FHTET)
!     Changed stock type resistance factors.
!  06-MAR-2001 Lance R. David (FHTET)
!     Added initialization of pathological pruning variable, LPATPR.
!     Added initialization for branch and bole canker growth rates.
!  14-MAR-2001 Lance R. David (FHTET)
!     DFACT variable expanded to array by species and stock type.
!  24-APR-2001 Lance R. David (FHTET)
!     Added species dimension to PRPSTK and RESIST arrays.
!  01-APR-2001 Lance R. David (FHTET)
!     Expanded tree category scalar variables to arrays.
!  03-MAY-2001 Lance R. David (FHTET)
!     Initialization loops for variables expanded to species arrays.
!  09-MAY-2001 Lance R. David (FHTET)
!     Changed use of FVS parameter MAXSP to WPBR parameter NBRSP for
!     BR species-specific arrays.
!  11-MAY-2001 Lance R. David (FHTET)
!     Added accumulator for total historical mortality (TBRHMR).
!  16-MAY-2001 Lance R. David (FHTET)
!     Canker growth rate arrays (BOGRTH,BRGRTH) initialization.
!  06-NOV-2002 Lance R. David (FHTET)
!     Initial values for RI based on exposure time Gaussian function
!     (variables: MINRI, MAXRI, PKAGE, RISHP).
!  11-NOV-2006 Lance R. David (FHTET)
!     Added call to reset random number generator.
!  15-MAY-2006 Lance R. David (FHTET)
!     Added BROUT keyword variables LBRDBH and LBRSUM.
!  12-JUN-2006 Lance R. David (FHTET)
!     Moved RIBUS initialization from brblkd.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'

INTEGER I4, I, J, K, L

!.... Reset the random number seed generator. WK6(1) is just used
!.... as a dummy variable as it is not utilized in the process.

CALL BRNSED(.FALSE., WK6(1))

!.... Initializations of logical variables from keyword options.

LBRSUM  = .TRUE.
LBRDBH  = .FALSE.
LMETRIC = .FALSE.
LDFACT  = .FALSE.
LPATPR  = .FALSE.
LPRUN   = .FALSE.
LCLEN   = .FALSE.
LPRGO   = .FALSE.
LEXGO   = .FALSE.
BRYES   = .FALSE.
BRTHDR  = .TRUE.
BRTL    = .FALSE.
BRCHDR  = .TRUE.
BRCL    = .FALSE.
BRCMP   = .FALSE.
CKINIT  = .FALSE.

!.... Pruning and excising guidelines.

SRATE(1) = 0.9
SRATE(2) = 0.5
HTPRPR   = 0.50
EXDMIN   = 3.0*2.54
HTMAX(1) = 8.0*30.48
HTMAX(2) = 6.0*30.48
GIRMAX   = 50.0
GIRMRT   = 100.0       ! Changed from 90.0 7-nov-02
HTMIN    = 3.0*2.54
OUTDST   = 6.0*2.54
OUTNLD   = 24.0*2.54

!.... Stand deviation factor and Rust Index Adjustment Factor.

DO I4 = 1,NBRSP
  DFACT(I4,1)=0.33
  DFACT(I4,2)=0.33
  DFACT(I4,3)=0.33
  DFACT(I4,4)=0.33
  RIAF(I4)   =1.0
  LBROUT(I4) =.FALSE.
END DO

!.... Inactivation rates.

RATINV(1)=0.05
RATINV(2)=0.01

!.... Default growth index, rust index, ribes population variables, etc.

GIDEF    = 15.0
RIBPRP(1)= 0.0
RIBPRP(2)= 0.5
RIBPRP(3)= 0.5
RIDEF    = 0.015
RIMETH   = 0
RIBUS(1,1) = 0.0
RIBUS(1,2) = 200.0
RIBUS(1,3) = 200.0
RIBUS(2,1) = 0.0
RIBUS(2,2) = 75.0
RIBUS(2,3) = 75.0

!.... These variables are for exposure time (age) based RI calculations
!.... and are initialized to zero. Equation-specific defaults are set
!.... during rustindx keyword processing.
MINRI = 0.0
MAXRI = 0.0
PKAGE = 0.0
PKSHP = 0.0


!.... Branch canker radial growth and Bole canker diameter growth rates (cm)
!.... First array dimension is BR species, second dimension is stock type.
!.... White Pine branch
BRGRTH(1,1) = 5.0
BRGRTH(1,2) = 5.0
BRGRTH(1,3) = 5.0
BRGRTH(1,4) = 5.0
!.... White Pine bole
BOGRTH(1,1) = 4.5
BOGRTH(1,2) = 4.5
BOGRTH(1,3) = 4.5
BOGRTH(1,4) = 4.5
!.... Sugar Pine branch
BRGRTH(2,1) = 5.0
BRGRTH(2,2) = 5.0
BRGRTH(2,3) = 5.0
BRGRTH(2,4) = 5.0
!.... Sugar Pine bole
BOGRTH(2,1) = 4.5
BOGRTH(2,2) = 4.5
BOGRTH(2,3) = 4.5
BOGRTH(2,4) = 4.5

!.... Canker count.
INCAN  = 0

DO I4 = 1,NBRSP

!....    Tree count.
   BRNTRECS(I4) = 0

!....    Stock type resistance factors.

   RESIST(I4,1)=1.00
   RESIST(I4,2)=0.33
   RESIST(I4,3)=0.17
   RESIST(I4,4)=0.11

!....    Default proportions of white pine stock types which
!....    make up the total white pine population.

   PRPSTK(I4,1)=1.0
   PRPSTK(I4,2)=0.0
   PRPSTK(I4,3)=0.0
   PRPSTK(I4,4)=0.0

!....    Stand average summary variables which are used in subroutine
!....    BRSTAT.  Also summary variables for the 2" DBH class table.

   AVTCPT(I4)=0.0
   AVLCPT(I4)=0.0
   AVECPT(I4)=0.0
   PITCA(I4)=0.0
   PILCA(I4)=0.0
   STSUM(I4)=0.0
END DO

DO I=1,10
   D2AVT(I)=0.0
   D2AVL(I)=0.0
   D2AVE(I)=0.0
   D2PIT(I)=0.0
   D2PIL(I)=0.0
END DO

!.... Initialize tree category accumulation variables which are
!.... used in subroutine BRTSTA.  Also summary variables for the
!.... 2" DBH class table.

DO I4 = 1,NBRSP
   AVGGI(I4) =0.0
   AVGRI(I4) =0.0
   AVGSTS(I4)=0.0
   TBRHST(I4)=0.0
   TBRCLN(I4)=0.0
   TBRNOL(I4)=0.0
   TBRPRN(I4)=0.0
   TBREXC(I4)=0.0
   TBRNOS(I4)=0.0
   TBRGIR(I4)=0.0
   TBRMRT(I4)=0.0
   TBRHMR(I4)=0.0
   THPROB(I4)=0.0
   TRETN(I4) =0.0
END DO

DO I=1,10
   D2CLN(I)=0.0
   D2NOL(I)=0.0
   D2PRN(I)=0.0
   D2EXC(I)=0.0
   D2NOS(I)=0.0
   D2GIR(I)=0.0
   D2DED(I)=0.0
   D2WP(I)=0.0
END DO

!.... Initialize tree-specific variables for maximum number of trees.

DO 30 J=1,MAXTRE
   UPMARK(J)=10000.0
   BRHTBC(J)=0.0
   GI(J)=0.0
   RI(J)=0.0
   BRGD(J)=0.0
   BRPB(J)=0.0
   ESTCAN(J)=0.0
   TSTARG(J)=0.0
   ITCAN(J)=0
   ILCAN(J)=0
   BRAGE(J)=0.0
   ISTOTY(J)=0
   IBRTID(J)=0
   ICRED(J)=0
   IBRSTAT(J)=0
   LEXMLT(J)=.FALSE.

!....    Initialize canker-specific variables for the maximum number
!....    of cankers.

   DO 25 K=1,10
      ISTCAN(K,J)=0
      GIRDL(K,J)=0.0
      DUP(K,J)=0.0
      DOUT(K,J)=0.0
25    CONTINUE
30 CONTINUE

!.... Initialize blister rust summary output variables loaded in
!.... subroutine BRSUM (each WPBR species, 18 values every cycle).

DO 46 J=1,NBRSP
  DO 45 K=1,18
     DO 40 L=1,41
        BROUT(J,K,L)=0.0
40      CONTINUE
45   CONTINUE
46 CONTINUE

!.... Initialize blister rust summary output variables for the 2" DBH
!.... class table, loaded in BRSUM (10 2" DBH classes, 13 categories of
!.... information, for up to 41 cycles).

DO 60 I=1,10
   DO 55 J=1,13
      DO 50 K=1,41
         BRDBHO(I,J,K)=0.0
50       CONTINUE
55    CONTINUE
60 CONTINUE

!.... Common return.

RETURN
END
