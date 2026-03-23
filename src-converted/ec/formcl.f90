SUBROUTINE FORMCL(ISPC,IFOR,D,FC)
IMPLICIT NONE
!----------
! EC $Id$
!----------
!
! THIS PROGRAM CALCULATES FORM FACTORS FOR CALCULATING CUBIC AND
! BOARD FOOT VOLUMES.
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
!OMMONS
!
!----------
REAL GIFPFC(MAXSP,5),MTHDFC(MAXSP,5),OKANFC(MAXSP,5)
REAL WENAFC(MAXSP,5),FC,D
INTEGER IFOR,ISPC,IFCDBH
!----------
!  FOREST ORDER: (IFOR)
!  1=MOUNT HOOD(606)  2=OKANOGAN(608)  3=WENATCHEE(617)
!  4=OKANOGAN (TONASKET RD) (699) 5=GIFFORD PINCHOT(603)
!
!  SPECIES LIST FOR EAST CASCADES VARIANT.
!
!   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
!   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
!   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
!   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
!   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
!   6 = GRAND FIR               (GF)    ABIES GRANDIS
!   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
!   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
!   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
!  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
!  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
!  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
!  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
!  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
!  15 = NOBLE FIR               (NF)    ABIES PROCERA
!  16 = WHITE FIR               (WF)    ABIES CONCOLOR
!  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
!  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
!  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
!  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
!  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
!  22 = RED ALDER               (RA)    ALNUS RUBRA
!  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
!  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
!  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
!  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
!  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
!  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
!  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
!  30 = WILLOW SPECIES          (WI)    SALIX sp.
!  31 = OTHER SOFTWOODS         (OS)
!  32 = OTHER HARDWOODS         (OH)
!
!  SURROGATE EQUATION ASSIGNMENT:
!
!  FROM THE EC VARIANT:
!      USE 6(GF) FOR 16(WF)
!      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)
!
!
!  FROM THE WC VARIANT:
!      USE 19(WH) FOR 11(WH)
!      USE 33(PY) FOR 13(PY)
!      USE 31(WB) FOR 14(WB)
!      USE  7(NF) FOR 15(NF)
!      USE 30(LL) FOR 17(LL)
!      USE  8(YC) FOR 18(YC)
!      USE 29(WJ) FOR 19(WJ)
!      USE 21(BM) FOR 20(BM) AND 21(VN)
!      USE 22(RA) FOR 22(RA)
!      USE 24(PB) FOR 23(PB)
!      USE 25(GC) FOR 24(GC)
!      USE 34(DG) FOR 25(DG)
!      USE 26(AS) FOR 26(AS) AND 32(OH)
!      USE 27(CW) FOR 27(CW)
!      USE 28(WO) FOR 28(WO)
!      USE 36(CH) FOR 29(PL)
!      USE 37(WI) FOR 30(WI)
!----------
!  GIFFORD PINCHOT FORM CLASS VALUES
!----------
DATA GIFPFC/ &
    84., 76., 82., 87., 70., 84., 82., 80., 80., 76., &
    86., 82., 60., 82., 84., 84., 75., 76., 60., 74., &
    74., 74., 70., 75., 74., 75., 74., 70., 74., 75., &
    82., 75., &
!
    84., 76., 82., 87., 70., 84., 82., 80., 80., 76., &
    86., 82., 60., 82., 84., 84., 75., 76., 60., 74., &
    74., 74., 70., 75., 74., 75., 74., 70., 74., 75., &
    82., 75., &
!
    84., 74., 80., 86., 68., 84., 82., 80., 80., 78., &
    84., 82., 60., 82., 84., 84., 75., 74., 60., 74., &
    74., 74., 70., 75., 74., 75., 74., 70., 74., 75., &
    82., 75., &
!
    82., 74., 79., 84., 68., 84., 82., 78., 80., 80., &
    84., 80., 60., 82., 82., 84., 74., 72., 60., 74., &
    74., 74., 70., 75., 74., 75., 74., 70., 74., 75., &
    80., 75., &
!
    82., 74., 78., 84., 68., 84., 82., 78., 80., 82., &
    82., 80., 60., 82., 82., 84., 74., 70., 60., 74., &
    74., 74., 70., 75., 74., 75., 74., 70., 74., 75., &
    80., 75./
!----------
!  MOUNT HOOD FORM CLASS VALUES
!----------
DATA MTHDFC/ &
    84., 86., 76., 87., 75., 76., 76., 77., 84., 79., &
    78., 72., 60., 82., 78., 76., 75., 75., 60., 74., &
    74., 74., 70., 75., 70., 75., 74., 70., 74., 75., &
    72., 75., &
!
    76., 86., 82., 87., 82., 72., 68., 77., 84., 79., &
    74., 72., 60., 82., 78., 72., 75., 75., 60., 74., &
    74., 74., 70., 75., 70., 75., 74., 70., 74., 75., &
    72., 75., &
!
    76., 86., 82., 86., 82., 72., 68., 77., 82., 82., &
    74., 72., 60., 82., 78., 72., 75., 73., 60., 74., &
    74., 75., 70., 75., 70., 75., 74., 70., 75., 75., &
    72., 75., &
!
    76., 86., 82., 84., 82., 72., 68., 77., 80., 83., &
    74., 72., 60., 82., 78., 72., 74., 70., 60., 74., &
    74., 75., 70., 75., 70., 75., 74., 70., 75., 75., &
    72., 75., &
!
    76., 87., 82., 80., 82., 72., 68., 77., 75., 82., &
    74., 72., 60., 82., 78., 72., 74., 70., 60., 74., &
    74., 75., 70., 75., 70., 75., 74., 70., 75., 75., &
    72., 75./
!----------
!  OKANOGAN FORM CLASS VALUES
!----------
DATA OKANFC/ &
    78., 78., 72., 82., 75., 76., 85., 82., 84., 78., &
    87., 75., 56., 82., 78., 76., 75., 75., 60., 74., &
    74., 74., 70., 75., 70., 75., 74., 70., 74., 75., &
    75., 75., &
!
    80., 78., 72., 82., 75., 78., 85., 82., 84., 80., &
    87., 78., 60., 82., 78., 78., 75., 75., 60., 74., &
    74., 74., 70., 75., 70., 75., 74., 70., 74., 75., &
    78., 75., &
!
    80., 78., 73., 82., 72., 77., 85., 83., 85., 81., &
    85., 79., 60., 82., 78., 77., 75., 73., 60., 74., &
    74., 75., 70., 75., 70., 75., 74., 70., 75., 75., &
    79., 75., &
!
    82., 73., 75., 84., 68., 76., 85., 86., 85., 82., &
    85., 79., 60., 82., 78., 76., 74., 70., 60., 74., &
    74., 75., 70., 75., 70., 75., 74., 70., 75., 75., &
    79., 75., &
!
    80., 73., 75., 84., 61., 76., 85., 86., 85., 84., &
    84., 78., 60., 82., 78., 76., 74., 70., 60., 74., &
    74., 75., 70., 75., 70., 75., 74., 70., 75., 75., &
    78., 75./
!----------
!  WENATCHEE FORM CLASS VALUES
!----------
DATA WENAFC/ &
    83., 77., 75., 85., 69., 78., 82., 79., 76., 77., &
    87., 82., 56., 82., 78., 78., 75., 75., 60., 74., &
    74., 74., 70., 75., 70., 75., 74., 70., 74., 75., &
    82., 75., &
!
    84., 78., 76., 86., 70., 79., 82., 80., 77., 78., &
    87., 82., 60., 82., 78., 79., 75., 75., 60., 74., &
    74., 74., 70., 75., 70., 75., 74., 70., 74., 75., &
    82., 75., &
!
    84., 79., 75., 84., 70., 79., 82., 80., 78., 81., &
    85., 82., 60., 82., 78., 79., 75., 73., 60., 74., &
    74., 75., 70., 75., 70., 75., 74., 70., 75., 75., &
    82., 75., &
!
    85., 80., 76., 86., 68., 79., 82., 82., 76., 81., &
    85., 80., 60., 82., 78., 79., 74., 70., 60., 74., &
    74., 75., 70., 75., 70., 75., 74., 70., 75., 75., &
    80., 75., &
!
    84., 80., 73., 86., 70., 80., 82., 82., 77., 80., &
    84., 80., 60., 82., 78., 80., 74., 70., 60., 74., &
    74., 75., 70., 75., 70., 75., 74., 70., 75., 75., &
    80., 75./
!----------
!  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
!  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
!----------
IF(FRMCLS(ISPC).LE.0.) THEN
  IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
  IF(IFCDBH .LT. 1) IFCDBH=1
  IF(D.GT.40.9) IFCDBH=5
!
  SELECT CASE (IFOR)
!
  CASE(1)
    FC = MTHDFC(ISPC,IFCDBH)
!
  CASE(2,4)
    FC = OKANFC(ISPC,IFCDBH)
!
  CASE(5)
    FC = GIFPFC(ISPC,IFCDBH)
!
  CASE DEFAULT
    FC = WENAFC(ISPC,IFCDBH)
!
  END SELECT
!
ELSE
  FC=FRMCLS(ISPC)
ENDIF
!
RETURN
END
