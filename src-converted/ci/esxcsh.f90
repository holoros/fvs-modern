SUBROUTINE ESXCSH (HTMAX,HTMIN,TIME,II,DRAW,HHT)
IMPLICIT NONE
!----------
! CI $Id$
!----------
!   SUBROUTINE TO ASSIGN HEIGHTS TO EXCESS TREES
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
!OMMONS
!----------
INTEGER II,ITIME
REAL BB(3,MAXSP),CC(3,MAXSP),SHIFT(MAXSP)
REAL HHT,DRAW,TIME,HTMIN,HTMAX,CLASS,XUPPR,XX
!----------
!     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
!
!     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
!     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
!     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
!     4 = GRAND FIR (GF)                   ABIES GRANDIS
!     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
!     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
!     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
!     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
!     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
!    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
!    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
!    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
!    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
!    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
!    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
!    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
!    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
!    18 = OTHER SOFTWOODS (OS)
!    19 = OTHER HARDWOODS (OH)
!----------
!  DATA STATEMENTS
!----------
DATA SHIFT/ &
    4.0, 4.0, 2.0, 2.0, 2.0, 2.0, 4.0, 2.0, 2.0, 4.0, &
     0.,  0.,  0.,  0.,  0.,  0.,  0., 2.0,  0./
!----------
!     COEFFICIENTS FOR ARRAYS BB AND CC PREDICT TREE HEIGHT 'CLASS'
!     RATHER THAN ACTUAL HEIGHT; E.G.
!     CLASS  DF,GF,C,H,S,AF  WP,WL,LP,PP
!        1         .6            1.0
!        2         .8            1.2
!        3        1.0            1.4    ETC.
!     PLOT AGE(YRS): 3 THRU 7  8 THRU 12  13 THRU 20
!----------
DATA BB/ &
          2.121455,  5.060402,   5.979549, &
          6.643726, 11.422982,  19.618871, &
          3.816083,  8.161474,  10.987699, &
          3.089571,  5.830185,  10.105748, &
          3.347712,  6.806825,  13.553455, &
          3.169513,  4.506403,   8.940539, &
          7.360424, 10.928846,  25.214411, &
          1.466152,  5.159270,   9.272780, &
          2.921356,  4.581383,  10.333282, &
          2.779221,  9.033310,  14.131212, &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
          3.347712,  6.806825,  13.553455, &
                0.,        0.,         0./
!
DATA CC/ &
           .745850,   .782170,    .842171, &
           .902909,  1.166155,   1.306380, &
           .996732,   .845413,    .948037, &
           .800681,   .832278,    .954081, &
           .567768,   .894628,   1.214044, &
           .640554,   .813543,    .943493, &
          1.148084,  1.232333,   1.117025, &
           .722527,   .739031,   1.125510, &
           .885137,   .871559,   1.043759, &
           .899325,  1.074932,    .930698, &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
                0.,        0.,         0., &
           .567768,   .894628,   1.214044, &
                0.,        0.,         0./
!
ITIME=1
IF(TIME.GT.7.5.AND.TIME.LT.12.5) ITIME=2
IF(TIME.GT.12.5) ITIME=3
!
SELECT CASE (II)
CASE(11,12,14,15,16)
  HHT = 0.5
!
CASE(13,17,19)
  HHT = 5.0
!
CASE DEFAULT
  CLASS=(HTMAX/0.2) -SHIFT(II)
  XUPPR=1.0-EXP(-(((CLASS-HTMIN)/BB(ITIME,II))**CC(ITIME,II)))
  XX=XUPPR*DRAW
  HHT=((-(ALOG(1.00-XX)))**(1.0/CC(ITIME,II)))*BB(ITIME,II)+HTMIN
  HHT=0.2*(HHT+SHIFT(II))
!
END SELECT
!
RETURN
END
