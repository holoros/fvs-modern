!ODE SEGMENT ESCOMN
!----------
! COMMON $Id$
!----------
!
! *********************************************************************
! *** This version is specific to AK variant for development period,
! *** but may be placed back into base level common folder in future
! *** with appropriate modifications made to other variant subroutines.
! *********************************************************************
!
!     COMMON AREA TO USE WITH ALL REGENERATION MODEL VARIANTS.
!     BE SURE TO INCLUDE ESPARM BEFORE ESCOMN.
!     USED IN ESADVH, ESBLKD, ESDLAY, ESGENT, ESNSPE, ESPADV, ESPREP, ESPSUB,
!     ESPXCS, ESSUBH, ESXCSH, ESTAB, ESTIME, ESTOCK, ESTPP, ESTUMP, & ESUCKR.
!----------
!
INTEGER IHAB,IPHY,IPREP,NNID,IFT0
!
INTEGER IFORCD(MXFRCDS),IFORST(MXFRCDS),ISPSPE(NSPSPE)
!
REAL    BAA,BAALN,BAASQ,BWAF,BWB4,ELEVSQ,REGT,SLO,SQBWAF,SQREGT
REAL    TIME,XCOS,XCOSAS,XSIN,XSINAS
!
REAL    BNORML(20),DBHMID(NDBHCL),HHTMAX(MAXSP),OCURHT(16,MAXSP)
REAL    OCURNF(MXFRCDS,MAXSP),SUMPI(MAXSP),SUMPX(MAXSP)
REAL    XMIN(MAXSP),OCURFT(MAXSP,14)
!
COMMON /ESCOMN/ BAA,BAALN,BAASQ,BNORML,BWAF,BWB4,DBHMID,ELEVSQ, &
                   HHTMAX,IFORCD,IFORST,IHAB,IPHY,IPREP,ISPSPE,NNID, &
                   OCURHT,OCURNF,REGT,SLO,SQBWAF,SQREGT,SUMPI,SUMPX, &
                   TIME,XCOS,XCOSAS,XMIN,XSIN,XSINAS,OCURFT,IFT0
!
!----------
!  VARIABLE DEFINITIONS:
!----------
!     BAA --
!   BAALN --
!   BAASQ --
!  BNORML --
!    BWAF --
!    BWB4 --
!  DBHMID --
!  ELEVSQ --
!  HHTMAX --
!  IFORCD --
!  IFORST --
!    IHAB --
!    IPHY --
!   IPREP --
!  ISPSPE --
!    NNID --
!  OCURHT --
!  OCURNF --
!  OCURFT --
!    REGT --
!     SLO --
!  SQBWAF --
!  SQREGT --
!   SUMPI --
!   SUMPX --
!    TIME --
!    XCOS --
!  XCOSAS --
!    XMIN --
!    XSIN --
!  XSINAS --
!
!-----END SEGMENT
