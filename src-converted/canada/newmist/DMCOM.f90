!----------
! CANADA-NEWMIST $Id$
!----------
!  **DMCOM  Date of last revision:  12/20/03
!--------------------------------------------------------------------
! Purpose:
!   Common block variables related to the NISI dwarf mistletoe
! routines.
!--------------------------------------------------------------------
!********************************************************************
! Symbolic names for array indices:
!
!     RADIUS  1st element of 3rd index of 'DMRDMX()' array.
!     VOLUME  2nd element of 3rd index of 'DMRDMX()' array.
!     XX      1st element of 3rd index of 'CShd()' array.
!     ZZ      2nd element of 3rd index of 'CShd()' array.
!     FST     1st element of 3rd index of 'DMSPtr()' [DMTREG]
!              array.
!     LST     2nd element of 3rd index of 'DMSPtr()' [DMTREG]
!              array.
!     CRTHRD  Number of parts in crown thirds.
!     BPCNT   Number of breakpoints in crown thirds.

!     IMMAT     1st element of 3rd index of 'DMINF()' array.
!     LATENT    2nd element of 3rd index of 'DMINF()' array.
!     SUPRSD    3rd element of 3rd index of 'DMINF()' array.
!     ACTIVE    4th element of 3rd index of 'DMINF()' array.
!     DEAD_BC   5th element of 3rd index of 'DMINF()' array.

!     KNT     1st element of 2nd index of 'SrcLst()' [DMTREG]
!              array.
!     INDX    2nd element of 2nd index of 'SrcLst()' [DMTREG]
!              array.
!********************************************************************

INTEGER RADIUS, VOLUME
INTEGER XX, ZZ
INTEGER FST, LST
INTEGER CRTHRD
INTEGER BPCNT
INTEGER IMMAT, LATENT, SUPRSD, ACTIVE, DEAD_BC
INTEGER KNT, INDX

PARAMETER(KNT=1,    INDX=2)
PARAMETER(RADIUS=1, VOLUME=2)
PARAMETER(XX=1,     ZZ=2)
PARAMETER(FST=1,    LST=2)

PARAMETER(CRTHRD = 3)
PARAMETER(BPCNT = CRTHRD + 1)

!     LIFE HISTORY POOLS

PARAMETER(IMMAT     = 1)
PARAMETER(LATENT    = 2)
PARAMETER(SUPRSD    = 3)
PARAMETER(ACTIVE    = 4)
PARAMETER(DEAD_BC   = 5)

!********************************************************************
! Fixed parameters and common array boundaries:
!
!     MESH    The size (MESH) of spatial grid cells.
!     MXTRAJ  The maximum number of trajectories that will *ever*
!              arrive at any grid cell. This value must be reviewed
!              whenever a new trajectory data set is put in DMBLKD.
!     ORIGIN  The cell (MESH) from which spread field trajectories
!              emmanate.
!     MXHT    The maximum height (MESH) of the stand.
!     MXTHRX  The maximum lateral distance of seed x-travel (MESH)
!               given a 20 m origin.
!     MXTHRZ  The maximum vertical distance of seed z-travel (MESH)
!               given a 20 m origin.
!     TOP1    The length of the *black box* array Shd1 that
!              holds all the encoded trajectory information.
!     DSTLEN  The length of the cumulative binomial-family
!              distribution.
!
! ORIGIN,...,MXTHRZ must all be evenly divisible by MESH.
!********************************************************************

INTEGER MESH
INTEGER MXTRAJ
INTEGER ORIGIN
INTEGER MXHT
INTEGER MXTHRX
INTEGER MXTHRZ
INTEGER TOP1
INTEGER DSTLEN

PARAMETER(MESH=2)
PARAMETER(MXTRAJ=1)
PARAMETER(MXTHRX=14/MESH)
PARAMETER(MXTHRZ=26/MESH)
PARAMETER(ORIGIN=20/MESH)
PARAMETER(MXHT=50/MESH)
PARAMETER(TOP1=1496)
PARAMETER(DSTLEN=1000)

!********************************************************************
! Some numerical constants.
!
!     TRAJWT  The weight of each observation in the shading array;
!              this is based on the number of paths used in the
!              trajectory simulation: 1000.
!     SQM2AC  The conversion factor for acres to square meters.
!     FPM     The number of feet in one meter.
!     PIE     The usual meaning of PI (NB: "PI" is a different
!              variable used elsewhere in the base model.)
!     HLFPIE  The usual meaning of PI / 2.
!     TWOPIE  PI * 2.
!     DMTINY  An arbitrarily small number that is near enough to
!              zero (in single precision) to mark the stop of some
!              calculations.
!********************************************************************

REAL    TRAJWT
REAL    SQM2AC
REAL    FPM
REAL    PIE
REAL    HLFPIE
REAL    TWOPIE
REAL    DMTINY

INTEGER MAXBC

PARAMETER(TRAJWT=1./1000.)
PARAMETER(SQM2AC=1./4046.8564)
PARAMETER(FPM=3.2808)
PARAMETER(PIE=3.14159)
PARAMETER(HLFPIE=1.57080)
PARAMETER(TWOPIE=6.283185)
PARAMETER(DMTINY=1.0E-10)

PARAMETER(MAXBC=5) ! max biological control systems

!     BIOCONTROL PARAMETERS FROM MISBCI KEYWORD

TYPE BIOCONTROL_DESCRIPTION
  SEQUENCE
  INTEGER     Spp
  REAL        Mort(ACTIVE)
  REAL        Suprs(ACTIVE)
  REAL        Yr(ACTIVE)
  REAL        HfLf(ACTIVE)
END TYPE

!********************************************************************
!
! Common block variables.
!
!  >> LOGICAL VARIABLES <<
!
!     LOGICAL  LDETAIL    .TRUE. if detailed outputs are requested
!     LOGICAL  NTDn       .TRUE. after the crownthirds have been
!                          initialized.
!     LOGICAL  DCDn       .TRUE. after the inventory damage codes
!                          have been read.
!     LOGICAL  NEWMOD     .TRUE. if the NISI model is in effect.
!     LOGICAL  ZPdn       .TRUE. if the lifehistory pools have been
!                          initialized.
!
!  >> DM RATING <<
!
!     INTEGER DMDMR       Default crownthird DM assignments: bottom
!                          up assignment of DM.
!                          Index 1: The six DM categories.
!                          Index 2: Upper (=1), Mid (=2) and Lower
!                                   (=3) crown third.
!     INTEGER DMRATE      The NISI DM category assigned to each
!                          treelist record. The range is from zero
!                          to six (Hawksworth's DMR).
!
!  >> TRAJECTORY INFORMATION <<
!
!     INTEGER*2 Shd1      The encoded trajectory information. More
!                          details on the coding are given in
!                          DMBSHD.
!     INTEGER*4 ShdPtr    Matrix of pointers to the 'Shd1()' array.
!                          There is a pointer for each of the X-
!                          and Z- positions of the trajectory
!                          reference frame. Each grid cell is a
!                          square of length MESH (normally 2
!                          meters).
!                          Index 1: Grid cells on the Z- (vertical)
!                                   axis. Ordering is from the
!                                   ground (=1) up.
!                          Index 2: Grid cells on the X-
!                                   (horizontal) axis. Ordering is
!                                   from the stem (=1) outward.
!
!  >> CROWN GEOMETRY, OPACITY & LIGHT <<
!
!     REAL    DMRDMX      Array holding crown geometry information
!                          for each MESH thickness slice of each
!                          treelist record.
!                          Index 1: The treelist record.
!                          Index 2: The height of the piece of
!                                   crown, measured in bands of
!                                   MESH thickness, from the ground
!                                   (=1) upward.
!                          Index 3: The estimated radius (=RADIUS)
!                                   and volume (=VOLUME) of the
!                                   relevant part of the crown.
!                                   Units are MESH, normally 2
!                                   meters.
!     REAL    DMOPQM      Multiplier that is applied to all the
!                          species-specific opacity estimates.
!                          This factor can be used to tie relative
!                          estimates of opacity to empirical
!                          measurement.
!     REAL    DMOPAQ      The relative opacity (PP = LP = 1.0 for
!                          the NI variant) of each species. Units
!                          measure the proportion of seeds that
!                          are *not* intercepted during a 1 meter
!                          flight path through the canopy of the
!                          relevant tree species.
!     REAL    DMOPQ2      Like 'DMOPAQ()', but expressed in MESH
!                           units.
!     REAL    DMLtRx      The coefficients for the light-driven
!                          forward and backward maturation
!                          effects. Up to four points can be used
!                          to describe the shape of each response
!                          surface. The x- (proportion of light)
!                          and y- (proportion changing state;
!                          either by going forward from latent to
!                          flowering, or backward from flowering
!                          to latent) are {0,1}. Each sequence of
!                          points must be strictly increasing on
!                          the x-axis.
!                          Index 1: The tree species.
!                          Index 2: Foward (=1) and backward (=2).
!                          Index 3: X- (=1) and Y- (=2) points.
!                          Index 4: The number of each point.
!     INTEGER DMLtnp      The number of points that have been
!                          chosen to represent the forward and
!                          backward reactions in 'DMLtRx()'.
!                          Index 1: The tree species.
!                          Index 2: The number of points for the
!                                   forward (=1) and the backward
!                                   (=2) reactions.
!
!  >> LIFE HISTORY <<
!
!     INTEGER DMKTUN      A species-specific multiplier that can
!                          be used to remove treelist records of
!                          a given DM category from contributing
!                          to spread.
!     REAL    DMETUN      A species-specific multiplier that can
!                          be used to increase or decrease the
!                          magnitude of spread and intensification
!                          simultaneously. It works *in addition
!                          to* the effects of 'DMSTUN()' and
!                          'DMITUN()'.
!     REAL    DMSTUN      A species-specific multiplier that can
!                          be used to increase or decrease the
!                          magnitude of spread.
!     REAL    DMITUN      A species-specific multiplier that can
!                          be used to increase or decrease the
!                          magnitude of intensification.
!     INTEGER DMFLWR      The species-specific average time
!                          (years) between the establishment of a
!                          successful infection and the earliest
!                          possible flowering under optimal
!                          mistletoe conditions.
!     REAL    DMCAP       The species-specific maximum density of
!                          infections that are permitted to occupy
!                          a crown-third. Units are "DM equivalent", which
!                          means that they are measured the same
!                          way as DMR: the number of infections
!                          required per MESH**3, to give an
!                          observed DMR of 'x'. In other words,
!                          a way to skirt the issue of how many
!                          plants it takes to give a DMR.
!     REAL    DMDETH      The species-specific annual death rate
!                          of DM infections. The units are:
!                          'proportion dying per year',
!                          independent of tree or branch death;
!                          units are dimensionless {0,1}.
!     REAL    DMSURV      The species-specific complement of
!                          'DMDETH()'. Units are dimensionless:
!                          '1.0 - {0,1}'.
!
! >> SPATIAL EFFECTS <<
!
!     REAL    DMCLMP      The amount of clumping found in the
!                          stand. Units are the variance/mean
!                          ratio for *all* species combined,
!                          and may be fixed or dynamic, depending
!                          on keyword control.
!     REAL    DMALPH      The DM-dependent autocorrelation term,
!                          independent of species. Implementation
!                          details can be found in DMOPTS.
!     REAL    DMBETA      The distance-dependent autocorrelation
!                          term, independent of species.
!                          Implementation details can be found in
!                          DMOPTS.
!     INTEGER DMRDFF      Matrix of differences between DM
!                          categories. This is a simple efficient
!                          way to record the absolute difference
!                          between any two DM categories.
!                          Index 1: The 0:6 DM categories.
!                          Index 2: The 0:6 DM categories.
!     REAL    SF          Scaling factor required to adjust
!                          neighborhood densities of each DM
!                          category in each sampling ring.
!                          Implementation details can be found
!                          in DMAUTO.
!                          Index 1: The 0:6 DM categories.
!                          Index 2: The sampling rings.
!
!  >> INFECTIONS <<
!
!     REAL    NewSpr      The amount of new infection originating
!                          from spread. Units are "DM equivalents"
!                          as described above for 'DMCAP()'. This
!                          is required in a common block only so
!                          that it can be printed in the detailed
!                          output file.
!                          Index 1: The treelist record.
!                          Index 2: The crown third.
!     REAL    NewInt      The amount of new infection originating
!                          from intensification. Units are "DM
!                          equivalents"as described above for
!                          'DMCAP()'. This is required in a common
!                          block only so that it can be printed in
!                          the detailed output file.
!                          Index 1: The treelist record.
!                          Index 2: The crown third.
!     REAL    DMINF       The infection density (DMR/MESH**3) of
!                          each life history stage, in each crown
!                          third of each treelist record's canopy.
!                          Index 1: The treelist record.
!                          Index 2: The crown third.
!                          Index 3: The life history class:
!                                   immature (=IMMAT), latent
!                                   (=LATENT), suppressed
!                                   (=QACTV) and flowering
!                                   (=ACTIVE). plus the biocontrol
!                                   equivalents.
!
!  >> MORE CROWN GEOMETRY, OPACITY & LIGHT <<
!
!     REAL    Shade       Opacity per layer
!     REAL    Light       Heightwise light extinction array; {0,1}
!                          where 0 means no light and 1 means full
!                          sky illumination.
!     REAL    BrkPnt      The four breakpoints that divide each
!                          tree's canopy into three equal pieces:
!                          crown thirds. Units are MESH (normally
!                          2 meters), and apply to the current
!                          time step.
!     REAL    PBrkPt      The four breakpoints that divide each
!                          tree's canopy into three equal pieces:
!                          crown thirds. Units are MESH (normally
!                          2 meters), and apply to the previous
!                          time step.
!     REAL    CrArea      The area (acres) of each sampling
!                          *disc*. This value is used to predicted
!                          the mean number of trees expected in a
!                          sampling disc, and is subsequently
!                          modified to estimate a sampling *ring*.
!     REAL    Dstnce      The distance (meters) between the center
!                          (target tree) and the mid-point of each
!                          sampling ring.
!
!  >> KLUDGE <<
!
!  Temporary (93 Oct 4) variable for MISGET and MISPUT
!
!     REAL    DMKLDG      A quick fix to allow a number of
!                          variables to be passed quickly to the
!                          MISGET and MISPUT routines: each
!                          treelist records breakpoints, previous
!                          breakpoints, and the infection levels
!                          of each life history stage in each
!                          crown third of the canopy.
!
!  >> RANDOM NUMBERS <<
!
!     DOUBLE  DMS0        Random number modulus 0
!     DOUBLE  DMS1        Random number modulus 1
!     REAL    DMSS        Starting seed.
!
!********************************************************************

LOGICAL LDETAIL
LOGICAL NTDn
LOGICAL DCDn
LOGICAL NEWMOD
LOGICAL ZPdn

INTEGER DMDMR(0:6,CRTHRD)
INTEGER DMRATE(MAXTRE)
INTEGER Shd1(TOP1) !
INTEGER ShdPtr(MXTHRZ, MXTHRX) !

REAL    DMRDMX(MAXTRE, MXHT, VOLUME)
REAL    DMOPQM
REAL    DMOPAQ(MAXSP)
REAL    DMOPQ2(MAXSP)

REAL    DMLtRx(MAXSP,2,2,4)
INTEGER DMLtnp(MAXSP,2)

INTEGER DMKTUN(MAXSP)
REAL    DMETUN(MAXSP)
REAL    DMSTUN(MAXSP)
REAL    DMITUN(MAXSP)

INTEGER DMFLWR(MAXSP)
REAL    DMCAP(MAXSP)
REAL    DMDETH(MAXSP)
REAL    DMSURV(MAXSP)

REAL    DMCLMP
REAL    DMALPH
REAL    DMBETA
INTEGER DMRDFF(0:6, 0:6)
REAL    SF(0:6, MXTHRX)

REAL    NewSpr(MAXTRE, CRTHRD)
REAL    NewInt(MAXTRE, CRTHRD)
REAL    DMINF(MAXTRE, CRTHRD, DEAD_BC)
REAL    DMINF_BC(MAXTRE, CRTHRD, ACTIVE, MAXBC)
REAL    Shade(MXHT), Light(MXHT)
REAL    BrkPnt(MAXTRE, BPCNT)
REAL    PBrkPt(MAXTRE, BPCNT)
REAL    CrArea(MXTHRX)
REAL    Dstnce(MXTHRX)

REAL    DMKLDG(2*BPCNT + CRTHRD*DEAD_BC + CRTHRD*ACTIVE*MAXBC)

COMMON /DMMIST/ LDETAIL,NTDn, DCDn
COMMON /DMMIST/ NEWMOD, ZPdn
COMMON /DMMIST/ DMDMR, DMRATE, DMRDMX
COMMON /DMMIST/ DMOPAQ, DMOPQ2, DMOPQM
COMMON /DMMIST/ DMLtRx,DMLtnp
COMMON /DMMIST/ DMKTUN, DMETUN, DMSTUN, DMITUN
COMMON /DMMIST/ DMFLWR, DMCAP
COMMON /DMMIST/ DMDETH, DMSURV
COMMON /DMMIST/ DMCLMP, DMALPH, DMBETA, DMRDFF, SF
COMMON /DMMIST/ NewSpr, NewInt, DMINF, DMINF_BC
COMMON /DMMIST/ Shade, Light
COMMON /DMMIST/ BrkPnt, PBrkPt, CrArea, Dstnce
COMMON /DMMIST/ Shd1, ShdPtr
COMMON /DMMIST/ DMKLDG

! Common block for random number generator.

REAL*8  DMS0, DMS1
REAL    DMSS

COMMON /DMRNCM/ DMS0, DMS1, DMSS

! Common block for biocontrol parameters

TYPE (BIOCONTROL_DESCRIPTION) BC(MAXBC)

COMMON /DMBCCM/ BC

! End segment.
