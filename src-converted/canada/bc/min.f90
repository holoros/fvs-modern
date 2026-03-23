 SUBROUTINE MIN(ISC,AGE,FIZ,DBH,HT,SH,TD,HTRUNC,GROSIMP,TVOLIMP)
!----------
! CANADA-BC $Id$
!----------
!
!C  -by Tim Hammond.
!C
!C    This routine and its subroutine LOG were substituted for the
!C  standard volume calculations in FVS in JUNE of 1990.  This
!C  routine converts species codes and inventory zone codes from
!C  FVS format to Kozak's format.  It determines the maturity
!C  of a tree based on its height, DBH and species, and finally it
!C  converts the output of Kozak's volume function (LOG) to imperial
!C  units.
!C
!C  SPECIES CODES
!C  Prog-code    FVS       Name             Kozak   Kozak-code
!C      1        WP        White Pine       Pw      7
!C      2        WL        Western Larch    L       10
!C      3        DF        Douglas Fir      F       1
!C      4        GF        Grand Fir        B       4
!C      5        WH        Western Hemlock  H       3
!C      6        C         Cedar            C       2
!C      7        LP        Lodgepole Pine   Pl      8
!C      8        S         Spruce           S       5
!C      9        AF        Subalpine Fir    B       4
!C     10        PP        Ponderosa Pine   Py      9
!C     11        EP        paper birch      Bi     14
!C     12        AT        trembling aspen  A      15
!C     13        AC        cottonwood       CT     11
!C     14        OC        other conifer    F       1
!C     15        OH        other hardwood   Bi     14
!
!C    *** IM represents the maturity of the tree
!C    *** it is equal to 1 if the tree is immature and 2 otherwise
!C    *** AGE is the age of the tree passed as an argument
!
!
include 'PRGPRM.f90'
!
!
include 'CONTRL.f90'
!
!
include 'METRIC.f90'
!
!
INTEGER*2 IS, IM, IFZ
INTEGER   ISC, AGE, FIZ
!
!C    *** IS represents the Kozak species code
!C    *** ISC is another argument from FVS representing the FVS
!C    *** species code
!
!     ***  DBHMET = diameter outside bark in cm at breast height.
!C    ***  DBH    = diameter outside bark in inches at breast height.
!C    ***  HTMET  = total height in m.
!C    ***  HT     = total height in feet.
!C    ***  SHMET  = stump height in m.
!C    ***  SH     = stump height in feet.
!C    ***  TD     = top diameter for use in inches.
!C    ***  TDMET  = top diameter for utilization in cm.
!C    ***  GOL    = log length in m.
!C    ***  HTRUNC = Height to point of top kill in feet
!C    ***  HTM    = Height to point of top kill in metres
!C    ***  HMERC  = Merchantable height in metres    ! not used
!C    ***  HMRIMP = Merchantable height in feet      ! not used
!
REAL DBH, DBHMET, HTMET, HT, SHMET, SH, TDMET, TD, GOL
REAL HTRUNC, HTM, HMERC
!
!C    *** volumes and top diameters are in metric
!C    *** VLOG     = volumes for each log (40)
!C    *** TDL      = top diameters for each log (40)
!C    *** HLL      = length of top log
!C    *** DBT      = butt diameter of first log
!C    *** TVOL     = volume between stump ht and top diameter for use
!C    *** NL       = Number of logs.
!C    *** GROS     = total volume from ground to top
!C    *** GROSIMP  = total volume from ground to top in imperial
!C    *** BAR      = bark thickness (not available in this program )
!
REAL VLOG(50),TDL(50)
REAL GROS, GROSIMP, TVOL, TVOLIMP, HLL, DBT, BAR
INTEGER*2 NL, SPTR(MAXSP)
LOGICAL DEBUG
COMMON /VOLMET/ GROS,VLOG,TDL,HLL,DBT,TVOL,BAR,HMERC

DATA GOL  / 3.0 /
DATA SPTR /  7,10, 1, 4, 3, &
                2, 8, 5, 4, 9, &
               14,15,11, 1,14 /
!
!     TRANSLATE SPECIES CODE TO KOZAK CODE
!
IS  = SPTR(ISC)

call dbchk( debug, 'MIN', 3, icyc )
if (debug) then
    write (jostnd,'('' Entering MIN'')')
    write (jostnd,'('' Parameters:'')')
    write (jostnd,'(t8,''Input Species:'',i10)')   isc
    write (jostnd,'(t8,''Age:          '',i10)')   age
    write (jostnd,'(t8,''FIZ:          '',i10)')   fiz
    write (jostnd,'(t8,''DBH (cm):     '',f10.4)') dbh
    write (jostnd,'(t8,''Height (m):   '',f10.4)') ht
    write (jostnd,'(t8,''Stump Ht (m): '',f10.4)') sh
    write (jostnd,'(t8,''Top Diam (cm):'',f10.4)') td
    write (jostnd,'(t8,''Trunc Ht (m): '',f10.4)') htrunc
endif
!
!C    Convert the arguments to metric for use in LOG
!
DBHMET =  DBH    * INtoCM
HTMET  =  HT     * FTtoM
SHMET  =  SH     * FTtoM
TDMET  =  TD     * INtoCM
HTM    =  HTRUNC * FTtoM

GROSIMP = 0.0
!
!C    Set the maturity code to immature if AGE is less than 121
!C    or less than 81 for lodgepole pine.
!
!C    Note that IM isn't used in any way. Could it be a place-
!C    holder?
!
IF (ISC .EQ. 7 .AND. AGE .LE. 80) THEN
  IM = 1
ELSEIF (AGE .GT. 120) THEN
  IM = 2
ELSEIF (ISC .EQ. 7) THEN
  IM = 2
ELSE
  IM = 1
ENDIF

IFZ = int(FIZ,2)

!     Initialize log length

if (debug) write (jostnd,1000) IS,IM,FIZ,DBHMET,HTMET,SHMET, &
                                  TDMET,GOL,HTM
1000 FORMAT (' Calling LOG with:', &
           /T5,'IS     = ',I10,  '  MATURE  = ',I10, &
           /T5,'FIZ    = ',I10,  '  DBHMET  = ',F10.5, &
           /T5,'HT     = ',F10.5,'  STUMPHT = ',F10.5, &
           /T5,'TDMET  = ',F10.5,'  LOGLEN  = ',F10.5, &
           /T5,'HTRUNC = ',F10.5)
CALL LOG(IS,IFZ,IM,DBHMET,HTMET,SHMET,TDMET,GOL,NL, &
            VLOG,TDL,HLL,DBT,TVOL,GROS,BAR,HTM)
if (debug) write (jostnd,'('' Called LOG...'')' )
GROSIMP = GROS * M3toFT3
TVOLIMP = TVOL * M3toFT3
!     HMRIMP  = HMERC * MtoFT
if (debug) write (jostnd,'('' Leaving MIN'')' )
RETURN
END
