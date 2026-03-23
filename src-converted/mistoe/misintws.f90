SUBROUTINE MISINT
IMPLICIT NONE
!**********************************************************************
! MISTOE $Id$
!----------------------------------------------------------------------
!  Purpose:
!     Mistletoe parameter initialization routine. This routine is
!  variant dependent and sets the variant dependent variables for other
!  mistletoe routines. This is the WESSIN version.
!----------------------------------------------------------------------
!
!  Call list definitions:
!
!  Local variable definitions:
!     DEBUG:  Logical flag to turn debug on and off.
!     AFIT:   Array of MISFIT data.
!     ACSP:   Array of CSPARR data.
!     ADGP:   Array of DGPMDR data.
!     APMC:   Array of PMCSP data.
!
!  Common block variables and parameters:
!     CSPARR: From MISCOM; 2-char. representations of all species.
!     DGPDMR: From MISCOM; diameter growth potentials based on species
!                and DMR (0-6).
!     ICYC:   From CONTRL; cycle index number.
!     JOSTND: From CONTRL; logical unit number for stand output.
!     MISFIT: From MISCOM; tells which species are affected by DM.
!     PMCSP:  From MISCOM; percent mortality coefficients by species.
!
!  Revisions:
!  01-MAR-2011 Lance R. David (FMSC)
!    Expanded in conjunction with the FVS WS variant 43 species update.
!    (Actually created by Gary Dixon. Lance set junipers to non-host.)
!  12-JUL-2011 Lance R. David (FMSC)
!    Added arrays for height growth impacts.
!    Impact values must be supplied by MistHMod keyword.
!
!**********************************************************************
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'MISCOM.f90'
!
!
!OMMONS
!----------
!  DIMENSIONS FOR INTERNAL VARIABLES.
!----------
LOGICAL DEBUG
CHARACTER*2 ACSP(MAXSP)
INTEGER I,J,AFIT(MAXSP)
REAL ADGP(MAXSP,7),AHGP(MAXSP,7),APMC(MAXSP,3)
!----------
!     SPECIES LIST FOR WESTERN SIERRAS VARIANT.
!
!     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
!     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
!     3 = WHITE FIR (WF)                    ABIES CONCOLOR
!     4 = GIANT SEQUOIA (GS)                SEQUOIA GIGANTEA
!     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
!     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
!     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
!     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
!     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
!    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
!    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
!    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
!    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
!    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
!    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
!    16 = COULTER PINE (CP)                 PINUS COULTERI
!    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
!    18 = MONTEREY PINE (MP)                PINUS RADIATA
!    19 = GRAY PINE (GP)                    PINUS SABINIANA
!         (OR CALIFORNIA FOOTHILL PINE)
!    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
!    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
!    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
!    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
!    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
!    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
!    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
!    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
!    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
!    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
!    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
!    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
!    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
!         (OR CALIFORNIA WHITE OAK)
!    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
!    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
!    35 = GIANT CHINKAPIN (GC)              CASTANOPSIS CHRYSOPHYLLA
!    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
!    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
!    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
!    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
!    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
!    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
!    42 = OTHER SOFTWOODS (OS)
!    43 = OTHER HARDWOODS (OH)
!
!  BASE FVS SURROGATE EQUATION ASSIGNMENT:
!
!    FROM EXISTING WS EQUATIONS --
!      USE 1(SP) FOR 11(WP) AND 24(MH)
!      USE 2(DF) FOR 22(BD)
!      USE 3(WF) FOR 13(SF)
!      USE 4(GS) FOR 23(RW)
!      USE 6(JP) FOR 12(PM)
!      USE 8(PP) FOR 18(MP)
!      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
!      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
!                     43(OH)
!
!    FROM CA VARIANT --
!      USE CA11(KP) FOR 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE), 25(WJ),
!                     26(WJ), AND 27(CJ)
!      USE CA12(LP) FOR 9(LP) AND 10(WB)
!
!    FROM SO VARIANT --
!      USE SO30(MC) FOR 41(MC)
!
!    FROM UT VARIANT --
!      USE UT17(GB) FOR 21(GB)
!----------
!  DATA STATEMENTS
!
!  Species character representations
!----------
DATA (ACSP(I),I=1,MAXSP)/ &
    'SP', 'DF', 'WF', 'GS', 'IC', 'JP', 'RF', 'PP', 'LP', 'WB', &
    'WP', 'PM', 'SF', 'KP', 'FP', 'CP', 'LM', 'MP', 'GP', 'WE', &
    'GB', 'BD', 'RW', 'MH', 'WJ', 'UJ', 'CJ', 'LO', 'CY', 'BL', &
    'BO', 'VO', 'IO', 'TO', 'GC', 'AS', 'CL', 'MA', 'DG', 'BM', &
    'MC', 'OS', 'OH'/
!----------
!  Species affected by mistletoe
!----------
DATA (AFIT(I),I=1,MAXSP)/ &
       1,    1,    1,    0,    0,    1,    1,    1,    1,    1, &
       1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
       1,    1,    0,    1,    0,    0,    0,    0,    0,    0, &
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0, &
       0,    0,    0/
!----------
!  Diameter growth rates                            DM Model specie
!----------                                         impact assigned
DATA ((ADGP(I,J),J=1,7),I=1,10)/ &
    1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  !  1 (SP) (    LP  )
    1.0, .98, .97, .85, .80, .52, .44, &  !  2 (DF) (  DF    )
    1.0, 1.0, 1.0, .98, .95, .70, .50, &  !  3 (WF) (GF      )
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !  4 (GS)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !  5 (IC)
    1.0, 1.0, 1.0, .98, .86, .73, .50, &  !  6 (JP) (      PP)
    1.0, 1.0, 1.0, .98, .95, .70, .50, &  !  7 (RF) (GF      )
    1.0, 1.0, 1.0, .98, .86, .73, .50, &  !  8 (PP) (      PP)
    1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  !  9 (LP) (    LP  )
    1.0, 1.0, 1.0, 1.0, .94, .80, .59/ ! 10 (WB) (    LP  )
!
DATA ((ADGP(I,J),J=1,7),I=11,20)/ &
    1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  ! 11 (WP) (    LP  )
    1.0, 1.0, 1.0, .98, .86, .73, .50, &  ! 12 (PM) (      PP)
    1.0, 1.0, 1.0, .98, .95, .70, .50, &  ! 13 (SF) (GF      )
    1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  ! 14 (KP) (    LP  )
    1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  ! 15 (FP) (    LP  )
    1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  ! 16 (CP) (    LP  )
    1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  ! 17 (LM) (    LP  )
    1.0, 1.0, 1.0, .98, .86, .73, .50, &  ! 18 (MP) (      PP)
    1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  ! 19 (GP) (    LP  )
    1.0, 1.0, 1.0, 1.0, .94, .80, .59/ ! 20 (WE) (    LP  )
!
DATA ((ADGP(I,J),J=1,7),I=21,30)/ &
    1.0, 1.0, 1.0, .98, .86, .73, .50, &  ! 21 (GB  (      PP)
    1.0, .98, .97, .85, .80, .52, .44, &  ! 22 (BD) (  DF    )
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 23 (RW)
    1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  ! 24 (MH) (    LP  )
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 25 (WJ)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 26 (UJ)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 27 (CJ)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 28 (LO)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 29 (CY)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/ ! 30 (BL)
!
DATA ((ADGP(I,J),J=1,7),I=31,40)/ &
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 31 (BO)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 32 (VO)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 33 (IO)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 34 (TO)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 35 (GC)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 36 (AS)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 37 (CL)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 38 (MA)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 39 (DG)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/ ! 40 (BM)
!
DATA ((ADGP(I,J),J=1,7),I=41,MAXSP)/ &
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 41 (MC)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  ! 42 (OS)
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/ ! 43 (OH)

!----------
!  Height growth potential rates
!----------
!....
!.... Using Douglas-fir height growth impact values described in:
!....
!.... Marshall, Katy 2007. Permanent plots for measuring spread and
!.... impact of Douglas-fir dwarf mistletoe in the Southern Oregon
!.... Cascades, Pacific Northwest Region: Results of the ten year
!.... remeasurement. USDA Forest Service, Pacific Northwest Region,
!.... Southwest Oregon Forest Insect and Disease Service Center,
!.... Central Point, Oregon. SWOFIDSC-07-04. 34 pp.
!....
!.... Default values for DF in this table would be:
!.... &   1.0,1.0,1.0,.95,.65,.50,.10,
!.... So that impacts are not unknowingly applied to projections,
!.... the values must be supplied with the MistHMod keyword.
!.... when appropriat default values are developed, they will be
!.... set here.

DATA ((AHGP(I,J),J=1,7),I=1,10)/ &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  !  1 (SP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  !  2 (DF)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  !  3 (WF)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  !  4 (GS)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  !  5 (IC)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  !  6 (JP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  !  7 (RF)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  !  8 (PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  !  9 (LP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/     ! 10 (WB)
!
DATA ((ADGP(I,J),J=1,7),I=11,20)/ &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 11 (WP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 12 (PM)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 13 (SF)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 14 (KP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 15 (FP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 16 (CP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 17 (LM)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 18 (MP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 19 (GP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/     ! 20 (WE)
!
DATA ((ADGP(I,J),J=1,7),I=21,30)/ &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 21 (GB
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 22 (BD)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 23 (RW)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 24 (MH)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 25 (WJ)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 26 (UJ)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 27 (CJ)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 28 (LO)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 29 (CY)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/     ! 30 (BL)
!
DATA ((ADGP(I,J),J=1,7),I=31,40)/ &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 31 (BO)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 32 (VO)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 33 (IO)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 34 (TO)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 35 (GC)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 36 (AS)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 37 (CL)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 38 (MA)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 39 (DG)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/     ! 40 (BM)
!
DATA ((ADGP(I,J),J=1,7),I=41,MAXSP)/ &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 41 (MC)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 42 (OS)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/     ! 43 (OH)

!----------
!  Mortality coefficients                         DM Model specie
!----------                                       coeffs assigned
DATA ((APMC(I,J),J=1,3),I=1,10)/ &
      0.00112,   0.02170,  -0.00171, &  !  1 (SP) (    LP  )
      0.01319,  -0.01627,   0.00822, &  !  2 (DF) (  DF    )
          0.0,   0.00159,   0.00508, &  !  3 (WF) (GF      )
          0.0,       0.0,       0.0, &  !  4 (GS)
          0.0,       0.0,       0.0, &  !  5 (IC)
      0.00681,  -0.00580,   0.00935, &  !  6 (JP) (      PP)
          0.0,   0.00159,   0.00508, &  !  7 (RF) (GF      )
      0.00681,  -0.00580,   0.00935, &  !  8 (PP) (      PP)
      0.00112,   0.02170,  -0.00171, &  !  9 (LP) (    LP  )
      0.00112,   0.02170,  -0.00171/ ! 10 (WB) (    LP  )
!
DATA ((APMC(I,J),J=1,3),I=11,20)/ &
      0.00112,   0.02170,  -0.00171, &  ! 11 (WP) (    LP  )
      0.00681,  -0.00580,   0.00935, &  ! 12 (PM) (      PP)
          0.0,   0.00159,   0.00508, &  ! 13 (SF) (GF      )
      0.00112,   0.02170,  -0.00171, &  ! 14 (KP) (    LP  )
      0.00112,   0.02170,  -0.00171, &  ! 15 (FP) (    LP  )
      0.00112,   0.02170,  -0.00171, &  ! 16 (CP) (    LP  )
      0.00112,   0.02170,  -0.00171, &  ! 17 (LM) (    LP  )
      0.00681,  -0.00580,   0.00935, &  ! 18 (MP) (      PP)
      0.00112,   0.02170,  -0.00171, &  ! 19 (GP) (    LP  )
      0.00112,   0.02170,  -0.00171/ ! 20 (WE) (    LP  )
!
DATA ((APMC(I,J),J=1,3),I=21,30)/ &
      0.00681,  -0.00580,   0.00935, &  ! 21 (GB  (      PP)
      0.01319,  -0.01627,   0.00822, &  ! 22 (BD) (  DF    )
          0.0,       0.0,       0.0, &  ! 23 (RW)
      0.00112,   0.02170,  -0.00171, &  ! 24 (MH) (    LP  )
          0.0,       0.0,       0.0, &  ! 25 (WJ)
          0.0,       0.0,       0.0, &  ! 26 (UJ)
          0.0,       0.0,       0.0, &  ! 27 (CJ)
          0.0,       0.0,       0.0, &  ! 28 (LO)
          0.0,       0.0,       0.0, &  ! 29 (CY)
          0.0,       0.0,       0.0/ ! 30 (BL)
!
DATA ((APMC(I,J),J=1,3),I=31,40)/ &
          0.0,       0.0,       0.0, &  ! 31 (BO)
          0.0,       0.0,       0.0, &  ! 32 (VO)
          0.0,       0.0,       0.0, &  ! 33 (IO)
          0.0,       0.0,       0.0, &  ! 34 (TO)
          0.0,       0.0,       0.0, &  ! 35 (GC)
          0.0,       0.0,       0.0, &  ! 36 (AS)
          0.0,       0.0,       0.0, &  ! 37 (CL)
          0.0,       0.0,       0.0, &  ! 38 (MA)
          0.0,       0.0,       0.0, &  ! 39 (DG)
          0.0,       0.0,       0.0/ ! 40 (BM)
!
DATA ((APMC(I,J),J=1,3),I=41,MAXSP)/ &
          0.0,       0.0,       0.0, &  ! 41 (MC)
          0.0,       0.0,       0.0, &  ! 42 (OS)
          0.0,       0.0,       0.0/ ! 43 (OH)
!----------
!  Check for debug.
!----------
CALL DBCHK(DEBUG,'MISINT',6,ICYC)
IF(DEBUG) WRITE(JOSTND,10)ICYC
10 FORMAT(' Begin/end MISINTWS: Cycle = ',I5)
!----------
!  Mistletoe model initializations.
!----------
DO 200 I=1,MAXSP
   MISFIT(I)=AFIT(I)
   CSPARR(I)=ACSP(I)
   DO 100 J=1,7
      DGPDMR(I,J)=ADGP(I,J)
      HGPDMR(I,J)=AHGP(I,J)
100    CONTINUE
   DO 150 J=1,3
      PMCSP(I,J)=APMC(I,J)
150    CONTINUE
200 CONTINUE
!
RETURN
END
