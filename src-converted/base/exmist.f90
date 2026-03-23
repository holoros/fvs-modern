SUBROUTINE EXMIST
IMPLICIT NONE
!----------
! BASE $Id$
!----------
!  Purpose:
!     Dummy external references for dwarf mistletoe model routines. A
!  dummy version of function MISDGF is at the end of this file.
!----------
!
!  Call list definitions (for entry point routines):
!     ARRAY:  (I)  Array containing keyword entries.
!     DMFLAG: (IO) Logical flag; TRUE if theres any DM in the stand.
!     IARRAY: (IO) Array containing random integers between 1 and ISIZE.
!     ICODE:  (I)  Array of mistletoe damage codes.
!     IDMR:   (IO) Current tree dwarf mistletoe rating.
!     IPNT:   (IO) Pointer to current element in print buffer WK3.
!     ILIMIT: (I)  Size of print buffer WK3.
!     ISIZE:  (I)  Range of integers in IARRAY.
!     ITREE:  (I)  Current tree record number.
!     KEY:    (I)  Keyword number used in MISKEY call list.
!     KEYWRD: (IO) Character buffer containing keyword name.
!     LNOTBK: (O)  Logical array depicting blank keyword fields.
!     MFLAG:  (I)  Logical flag to update WK2 with mistletoe mortality.
!     MSPCNT: (O)  Array of number of DM infected records by species.
!     NFLAG:  (O)  Logical flag indicating presence of mistletoe code.
!     PRFCUT: (O)  Array of mistletoe cutting preferences by species.
!     WK3:    (IO) Work array used as a print buffer.
!
!  Local variable definitions:
!     NOMIS:  Character buffer used to tell MISKEY this is a no DM run.
!
!  Common block variables and parameters:
!     MAXSP:  From PRGPRM; maximum number species.
!     MAXTRE: From PRGPRM; maximum number tree records.
!
!----------
!----------
!  PARAMETER INCLUDE FILES.
!----------
INCLUDE 'PRGPRM.f90'
!----------
!  VARIABLE DECLARATIONS.
!----------
INTEGER I,ITREE,IDMR,KEY,IDMR1,ISIZE,ILIMIT,IPNT
LOGICAL LNOTBK(12),DMFLAG,MFLAG,NFLAG,LKECHO
REAL ARRAY(12),PRFCUT(MAXSP),WK3(MAXTRE)
INTEGER ICODE(6),MSPCNT(MAXSP),IARRAY(MAXTRE)
CHARACTER*8 KEYWRD,NOMIS
CHARACTER*1 CBUFF
REAL RDANUW
INTEGER IDANUW
CHARACTER*8 CDANUW
LOGICAL LDANUW
!----------
!  DATA STATEMENTS.
!----------
DATA NOMIS/'*NO MIST'/
!----------
!  ENTRY MISCNT
!----------
ENTRY MISCNT(MSPCNT)
  DO 100 I=1,MAXSP
    MSPCNT(I)=0
100   CONTINUE
RETURN
!----------
!  ENTRY MISCPF
!----------
ENTRY MISCPF(PRFCUT)
  DO 150 I=1,MAXSP
    PRFCUT(I)=0.0
150   CONTINUE
RETURN
!----------
!  ENTRY MISDAM
!----------
ENTRY MISDAM(ITREE,ICODE)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  IDANUW = ITREE
  RDANUW = ICODE(1)
RETURN
!----------
!  ENTRY MISGET
!----------
ENTRY MISGET(ITREE,IDMR)
  IDMR=0
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  IDANUW = ITREE
RETURN
!----------
!  ENTRY MISIN
!----------
ENTRY MISIN(KEYWRD,ARRAY,LNOTBK,LKECHO)
  CALL ERRGRO(.TRUE.,11)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  CDANUW = KEYWRD
  RDANUW = ARRAY(1)
  LDANUW = LNOTBK(1)
  LDANUW = LKECHO
RETURN
!----------
!  ENTRY MISINF
!----------
ENTRY MISINF(DMFLAG)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  LDANUW = DMFLAG
RETURN
!----------
!  ENTRY MISIN0
!----------
ENTRY MISIN0
RETURN
!----------
!  ENTRY MISINT
!----------
ENTRY MISINT
RETURN
!----------
!  ENTRY MISKEY (ENTRY POINT IN MISIN)
!----------
ENTRY MISKEY(KEY,KEYWRD)
  KEYWRD=NOMIS
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  IDANUW = KEY
RETURN
!----------
!  ENTRY MISMRT
!----------
ENTRY MISMRT(MFLAG)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  LDANUW = MFLAG
RETURN
!----------
!  ENTRY MISPRT
!----------
ENTRY MISPRT
RETURN
!----------
!  ENTRY MISPUT
!----------
ENTRY MISPUT(ITREE,IDMR)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  IDANUW = ITREE
  IDANUW = IDMR
RETURN
!----------
!  ENTRY MISPUTZ
!----------
ENTRY MISPUTZ(ITREE,IDMR1)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  IDANUW = ITREE
  IDANUW = IDMR1
RETURN
!----------
!  ENTRY MISRAN
!----------
ENTRY MISRAN(IARRAY,ISIZE)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  IDANUW = ISIZE
  IDANUW = IARRAY(1)
RETURN
!----------
!  ENTRY MISTOE
!----------
ENTRY MISTOE
RETURN
!----------
!  ENTRY MSPPPT - PPE -
!----------
ENTRY MSPPPT (WK3,IPNT,ILIMIT)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  RDANUW = WK3(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
RETURN
!----------
!  ENTRY MSCHPUT
!----------
ENTRY MSCHPUT (CBUFF, IPNT, ILIMIT)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  CDANUW(1:1) = CBUFF(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
RETURN
!----------
!  ENTRY MSPPGT - PPE -
!----------
ENTRY MSPPGT (WK3,IPNT,ILIMIT)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  RDANUW = WK3(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
RETURN
!----------
!  ENTRY MSCHGET
!----------
ENTRY MSCHGET (CBUFF, IPNT, ILIMIT)
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
  IF(.TRUE.)RETURN
  CDANUW(1:1) = CBUFF(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
RETURN
!----------
!  ENTRY MISACT
!----------
ENTRY MISACT (NFLAG)
  NFLAG=.FALSE.
RETURN
!
END

REAL FUNCTION MISDGF(ITREE,ISPC)
!----------
!  **MISDGF--MS  Date of last revision:  04/09/91
!----------
!  Purpose:
!     This is a dummy version of the mistletoe infection diameter
!  growth function. It returns MISDGF as 1.0 always (i.e. 100 percent
!  potential diameter growth).
!----------
!
!  Call list definitions:
!     ISPC:   (I) Current tree species.
!     ITREE:  (I) Current tree record number.
!     MISDGF: (O) Returns the 10 year proportion of potential diameter
!                growth due to mistletoe infection.
!
!----------
MISDGF=1.0
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
IF(.TRUE.)RETURN
IDANUW = ITREE
IDANUW = ISPC
RETURN
!
END

REAL FUNCTION MISHGF(ITREE,ISPC)
!----------
!  **MISHGF--MS  Date of last revision:  04/01/11
!----------
!  Purpose:
!     This is a dummy version of the mistletoe infection height
!  growth function. It returns MISHGF as 1.0 always (i.e. 100 percent
!  potential height growth).
!----------
!
!  Call list definitions:
!     ISPC:   (I) Current tree species.
!     ITREE:  (I) Current tree record number.
!     MISHGF: (O) Returns the 10 year proportion of potential height
!                growth due to mistletoe infection.
!
!----------
MISHGF=1.0
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
IF(.TRUE.)RETURN
IDANUW = ITREE
IDANUW = ISPC
RETURN
!
END
