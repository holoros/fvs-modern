FUNCTION BRATIO(IS,D,H)
IMPLICIT NONE
!----------
! AK $Id$
!----------
!  FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER
!
! SPECIES LIST FOR ALASKA VARIANT.
!
! Number Code  Common Name         FIA  PLANTS Scientific Name
!   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
!   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
!   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
!   4     TA   tamarack            071  LALA   Larix laricina
!   5     WS   white spruce        094  PIGL   Picea glauca
!   6     LS   Lutz�s spruce            PILU   Picea lutzii
!   7     BE   black spruce        095  PIMA   Picea mariana
!   8     SS   Sitka spruce        098  PISI   Picea sitchensis
!   9     LP   lodgepole pine      108  PICO   Pinus contorta
!  10     RC   western redcedar    242  THPL   Thuja plicata
!  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
!  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
!  13     OS   other softwoods     298  2TN
!  14     AD   alder species       350  ALNUS  Alnus species
!  15     RA   red alder           351  ALRU2  Alnus rubra
!  16     PB   paper birch         375  BEPA   Betula papyrifera
!  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
!  18     BA   balsam poplar       741  POBA2  Populus balsamifera
!  19     AS   quaking aspen       746  POTR5  Populus tremuloides
!  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
!  21     WI   willow species      920  SALIX  Salix species
!  22     SU   Scouler�s willow    928  SASC   Salix scouleriana
!  23     OH   other hardwoods     998  2TB
!
!----------
!  VARIABLE DECLARATIONS:
!----------
REAL BARKB(3,23),H,D,BRATIO,DIB,DBT
INTEGER IS
REAL RDANUW
!----------
DATA BARKB/ &
    1.,  0.186   ,  0.45417 , &
    1.,  0.186   ,  0.45417 , &
    1.,  0.186   ,  0.45417 , &
    3.,  0.796645,  1.060823, &
    3.,  0.839825,  1.039951, &
    3.,  0.839825,  1.039951, &
    3.,  0.796645,  1.060823, &
    3.,  0.843047,  1.030798, &
    1.,  0.186   ,  0.45417 , &
    1.,  0.186   ,  0.45417 , &
    1.,  0.186   ,  0.45417 , &
    3.,  0.743935,  1.048186, &
    3.,  0.839825,  1.039951, &
    2.,  0.075256,  0.94373 , &
    2.,  0.075256,  0.94373 , &
    3.,  0.899145,  1.011965, &
    3.,  0.899145,  1.011965, &
    3.,  0.784424,  1.031326, &
    3.,  0.921204,  0.996669, &
    3.,  0.784424,  1.031326, &
    3.,  0.784424,  1.031326, &
    3.,  0.784424,  1.031326, &
    3.,  0.784424,  1.031326/
!----------
!  EQUATION TYPES AND COEFS (COLUMN 4 OF BARKB)
!  1  DBT = a * DOB ** b
!     BRATIO = (D - DBT)/D
!         TAKEN FROM EQ 4.2.1 OF AK VARIANT GUIDE
!  2  BRATIO = (a + (b * DOB))/DOB
!         TAKEN FROM EQ 4.2.2 OF AK VARIANT GUIDE
!  3  DIB = a * DOB ** b
!     BRATIO = DIB/D
!         DEVELOPED DURING FITTING BY MARK CASTLE
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
RDANUW = H
!
IF (D .GT. 0) THEN
  IF(BARKB(1,IS) .EQ. 1.)THEN
    DBT=BARKB(2,IS)*D**BARKB(3,IS)
    BRATIO=(D-DBT)/D
  ELSEIF (BARKB(1,IS) .EQ. 2.)THEN
    BRATIO=(BARKB(2,IS) + (BARKB(3,IS)*D))/D
  ELSEIF (BARKB(1,IS) .EQ. 3.)THEN
    DIB=BARKB(2,IS)*D**BARKB(3,IS)
    BRATIO=DIB/D
  ELSE
    BRATIO= 0.9
  ENDIF
ELSE
  BRATIO = 0.99
ENDIF
!
IF(BRATIO .GT. 0.99) BRATIO= 0.99
IF(BRATIO .LT. 0.80) BRATIO= 0.80
!
RETURN
END

