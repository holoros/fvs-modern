!ODE SEGMENT RDCOM
!----------
! RD $Id$
!----------
! REVISION HISTORY:
!   27-JUNE-2002 Lance David
!     Added variable descriptions found in the original western root disease
!     model.
!-------------------------
INTEGER HOST(3*MAXSP)
INTEGER IBBOUT, IDOBB, IDITYP(ITOTSP), IFRRC, IIEND, ILEND, &
           IMCOUT, INFLAG, IOUNIT, IPCFLG(ITOTRR), IPUSH, IRCOMP, &
           IRDOUT, IRFLAG, IRGEN(10), IRHAB, IRINIT, IRIPTY, &
           IROOT, IRRSP, IRSNYR, IRSPTY, IRSTYP, IRTSPC(MAXSP), &
           IRUNIT, ISPS(ITOTSP), ISTEP, ISTFLG, IYEAR, &
           JRAGED(ITOTRR,2,5,41), JRSIT, MCTREE(ITOTRR,50), &
           NCENTS(ITOTRR), NINSIM, NMONT, NNCENT, NNDEC, NNDMX, &
           NNINF, NRSTEP, NSCEN(ITOTRR), NSTUMP, NTREES, NUMBB, &
           NUMTRE, IDRDOUT(3)

REAL    IURATE(3*MAXSP), IIRATE(3*MAXSP), &
           MCRATE(ITOTRR,50), MINDBH(3*MAXSP), MININF(3*MAXSP)

REAL   AGECUR, AREANU(ITOTRR), CORINF(ITOTRR,2), CURAGE, &
          DBHD(ITOTRR,2,5), DBHDA(ITOTRR,2,5,41), DBIFAC(ITOTSP), &
          DECRAT(ITOTRR,2,5,41), DGTOT, DICLAS(4), DIMEN, DSEED, &
          DSFAC(2), EXPINF(ITOTRR,2), FRINGE(ITOTRR), &
          HABFAC(ITOTSP,ITOTRR,2), HTIFAC(ITOTSP), HTIMP, HTTOT, &
          OFRATE(3*MAXSP), OOAREA(ITOTRR), ORATE(3*MAXSP), &
          PAREA(ITOTRR), PCENTS(ITOTRR,100,3), &
          PCOLO(ITOTSP,ITOTRR), PINSET, PISIZE(ITOTRR), &
          PKILLS(ITOTSP,ITOTRR), PNINF(ITOTSP,ITOTRR), PPUSH, &
          PRANKL(IRRTRE), PRINF(ITOTRR+MAXSP), PRKILL(ITOTRR), &
          PROBD(ITOTRR,2,5), PROBDA(ITOTRR,2,5,41), PROOT(ITOTSP), &
          PRPTOT(ITOTRR+MAXSP), PRREM, PRUN(ITOTRR), PTRE, &
          RADINF(50,2), RDKILL(IRRTRE), REINEK(4), ROCRI(MAXSP), &
          ROCRL(MAXSP), ROOT4(4), ROOTD(ITOTRR,2,5), &
          ROOTDA(ITOTRR,2,5,41), ROOTH(4,IRRTRE), ROTSIT, &
          ROWDOM(ITOTSP), ROWIBP(ITOTSP,2), ROWIND, ROWMIN, &
          RRATES(ITOTRR,100), RRGEN(ITOTRR,10), RRIARE, RRIDIM, &
          RRIMEN, RRINCS(ITOTRR), RRIRAD(IRRTRE), RRJINC(ITOTRR), &
          RROBMR(ITOTRR), RROBNK(MAXSP), RROBOL(4), RROBRD(4), &
          RROBSC(4), RROBTS(4), RROOTT(IRRTRE), RRPSWT(ITOTSP), &
          RRRATE(ITOTRR), RRRSET(ITOTRR), RRSARE, RRSDBH(50), &
          RRSFRN, RRSRAD(50), RSLOP(ITOTSP), SAREA, SDRATE(IRRTRE), &
          SHCENT(ITOTRR,100,3), SPRQMD(ITOTRR,50), &
          SSSFAC(ITOTSP,ITOTRR), STCUT(5), STEMSI(MAXSP), &
          STEMSL(MAXSP), TNJUMP(ITOTRR), TSTEMS, TXP12(4), WINDN, &
          WINDSP(MAXSP,4), WK22(IRRTRE), XDBH(2), XHT(2), &
          XMINKL(ITOTRR), XMINLF(ITOTRR), XMTH(4,IRRTRE), &
          XRRI(IRRTRE), XRRS(50), XXINF(5), YDBH(2), YHT(2), &
          YRRI(IRRTRE), YRRS(50), YTKILL, YYINF(5)

REAL   OLDPRP                 ! From common that was directly coded in rdranp.f

REAL   CDF(1001)              ! From common that was directly coded in rdranp.f

LOGICAL LBBON, LRTYPE, LXNOTE

COMMON /RDCOM/ AGECUR, AREANU, CORINF, CURAGE, DBHD, DBHDA, &
                  DBIFAC, DECRAT, DGTOT,DICLAS, DIMEN, DSEED, &
                  DSFAC, EXPINF, FRINGE, HABFAC, HOST, HTIFAC, &
                  HTIMP, HTTOT, IBBOUT, IDITYP, IDOBB, IFRRC, &
                  IIEND, IIRATE, ILEND, IMCOUT, INFLAG, IOUNIT, &
                  IPCFLG, IPUSH, IRCOMP, IRDOUT, IRFLAG, IRGEN, &
                  IRHAB, IRINIT, IRIPTY, IROOT, IRRSP,IRSNYR, &
                  IRSPTY, IRSTYP, IRTSPC, IRUNIT, ISPS, ISTEP, &
                  ISTFLG, IURATE, IYEAR, JRAGED, JRSIT, LBBON, &
                  LRTYPE, LXNOTE, MCRATE, MCTREE, MINDBH, MININF, &
                  NCENTS, NINSIM, NMONT, NNCENT, NNDEC, NNDMX, &
                  NNINF, NRSTEP, NSCEN, NSTUMP, NTREES, NUMBB, &
                  NUMTRE, OFRATE, OOAREA, ORATE, PAREA, PCENTS, &
                  PCOLO, PINSET, PISIZE, PKILLS, PNINF, PPUSH, &
                  PRANKL, PRINF, PRKILL, PROBD, PROBDA, PROOT, &
                  PRPTOT, PRREM, PRUN, PTRE, RADINF, RDKILL, &
                  REINEK, ROCRI, ROCRL, ROOT4, ROOTD, ROOTDA, &
                  ROOTH, ROTSIT, ROWDOM, ROWIBP, ROWIND, ROWMIN, &
                  RRATES, RRGEN, RRIARE, RRIDIM, RRIMEN, RRINCS, &
                  RRIRAD, RRJINC, RROBMR, RROBNK, RROBOL, &
                  RROBRD, RROBSC, RROBTS, RROOTT, RRPSWT, &
                  RRRATE, RRRSET, RRSARE, RRSDBH, RRSFRN, &
                  RRSRAD, RSLOP, SAREA, SDRATE, SHCENT, SPRQMD, &
                  SSSFAC, STCUT, STEMSI, STEMSL, TNJUMP, TSTEMS, &
                  TXP12, WINDN, WINDSP, WK22, XDBH, XHT, XMINKL, &
                  XMINLF, XMTH, XRRI, XRRS, XXINF, YDBH, YHT, &
                  YRRI, YRRS, YTKILL, YYINF, IDRDOUT

COMMON  /PRPDAT/ OLDPRP       ! From common that was directly coded in rdranp.f
COMMON  /PRPDATD/ CDF         ! From common that was directly coded in rdranp.f

!     AGECUR  -- YEAR IN WHICH CARRYOVER IS TO OCCUR
!                (** CURRENTLY NOT USED)
!                USED)
!     AREANU  -- THE NUMBER OF ACRES OF NEW AREA INFECTED BY ROOT
!                DISEASE IN THE CURRENT CYCLE.
!     CORINF  -- (,1) NUMBER OF TREES INFECTED THIS CYCLE INSIDE THE
!                'CORE' OF THE CENTER
!             -- (,2) NUMBER OF TREES INSIDE 'CORE' BEOFRE INFECTION
!                OCCURS
!     CURAGE  --
!     DBIFAC  --
!     DECRAT  --  DECAY RATE OF ROOTS
!     DGTOT   --
!     DICLAS  --
!     DIMEN   --
!     DSEED   --
!     DSFAC   --
!     EXPINF  -- (,1) NUMBER OF TREES INFECTED THIS CYCLE FROM
!                EXPANDING THE CENTER
!             -- (,2) NUMBER OF TREES IN THE NEWLY EXPANDED AREA
!     FRINGE  -- the area in the stand within one average root radius
!                of a center
!     HABFAC  -- (TREE SP,RR,1-2) RELATIVE TIME TO DEATH.
!                (,,2) USED IF WANT TO CHANGE TIME TO DEATH AFTER SOME
!                POINT IN TIME
!     HTIFAC  --
!     HTIMP   --
!     HTTOT   --
!     IBBOUT  -- OUTPUT FILE NUMBER FOR BARK BEETLE MORTALITY TABLE
!     IDITYP  -- DISEASE TYPE: 1 = P-TYPE ANNOSUS
!                              2 = S-TYPE ANNOSUS
!                              3 = ARMILLARIA
!                              4 = PHELLINUS
!     IDOBB  - COUNTER THAT IS USED TO KEEP TRACK OF THE NUMBER OF
!              BARK BEETLE EVENTS THAT ARE SCHEDULED.
!     IDRDOUT -- ARRAY HOLDS REPORT ID'S FOR GENERAL REPORT WRITER.
!     IMCOUT  -- OUTPUT FILE NUMBER FOR SPREADRATE MONTE CARLO RESULTS
!     INFLAG  --
!     IPCFLG  -- FLAG FOR INITIALIZATION OF ROOT DISEASE.  0 = PATCHES
!                PLACED RANDOMLY.  1 = PATCHES PLACED BY THE USER.
!     IPUSH   --
!     IRFLAG  --
!     IRGEN(1) -- THE NUMBER OF CYCLES AFTER STAND ENTRY IN WHICH THE
!                CARRYOVER MODEL IS CALLED.  SET BY THE KEYWORD CARRY
!                DEFAULT = 4.  (*** NOT BEING USED IN COMBINED ROOT
!                DISEASE MODEL)
!     IRGEN(2) -- THE UNIT NUMBER WHERE THE "MACHINE READABLE" OUTPUT
!                IS WRITTEN.
!     IRGEN(3) -- NOT USED.
!     IRGEN(4) -- THE YEAR IN WHICH THE TREE SPATIAL DISTRIBUTION IN
!                THE STAND CHANGES FROM RANDOM TO LATTICE OR LATTICE TO
!                RANDOM, DEPENDING ON THE INITIAL DISTRIBUTION.  SET
!                BY THE KEYWORD TDISTN.
!     IRGEN(5..6) -- NOT USED.
!     IRGEN(7) -- FLAG THAT IS SET TO 1 IF THE "MACHINE READABLE"
!                OUTPUT IS WANTED.
!     IRGEN(8) -- THE YEAR IN WHICH THE TIME TO DEATH MULTIPLIERS ARE
!                CHANGED.  SET BY KEYWORD TTDMULT.
!     IRGEN(9) -- THE YEAR IN WHICH THE PROBABILITIES OF INFECTION
!                ARE CHANGED.  SET BY KEYWORD INFMULT.
!     IRHAB   -- USED WITH THE ARRAY HABFAC THAT HOLDS THE TIME TO DEATH
!                MULTIPLIERS.  SET TO 1 FOR THE DEFAULT VALUES AND SET
!                TO 2 WHEN THE USER ENTERED VALUES ARE TO REPLACE THE
!                DEFAULT VALUES.
!     IRINIT  -- SET TO 13500 (10 * MAX TREE RECORDS IN PROGNOSIS).
!                USED TO CALCULATE NUMTRE IN RRINSD.
!     IRIPTY  --
!     IROOT   -- SIMPLY INDICATES THAT A BLOCK OF RD KEYWORDS HAS BEEN
!                ENCOUNTERED AND THE RD MODEL IS ACTIVE.
!     IRSNYR  --
!     IRSPTY  --
!     IRSTYP  --
!     IRTSPC  --
!     ISPS    -- WOOD TYPE (1=RESINOUS, 2=NON-RESINOUS)
!     ISTEP   -- COUNTER FOR WHICH SLOT TO PUT STUMPS AND INFECTED
!                TREES IN: IT IS ALWAYS EQUAL TO ONE MORE THAN ICYC
!                SINCE IN THE INITIALIZATION PHASE (ICYC=0), INFECTED
!                STUFF MUST BE PUT INTO THE FIRST SLOT (ISTEP=1).
!                IN THE FIRST CYCLE (ICYC=1), STUFF IS PUT INTO THE
!                SECOND SLOT (ISTEP=2)
!     IYEAR   --
!     JRSIT   -- NUMBER OF YEARS TO 'SIT' WITHOUT DECREASING ROOT RADIUS
!     LBBON  - LOGICAL FLAG TO DETERMINE IF THE BARK BEETLES WERE
!              CLEARED OR NOT
!     LRTYPE - Logical flag to track implementation of the the RRTYPE
!              keyword in a model run.  A warning is printed to the
!              output file that the default, annosus disease, is used
!              in the simulations.
!     MCRATE  -- MONTE CARLO SIMULATION RESULTS (EACH RUN)
!     MCTREE  -- NUMBER OF TREES IN EACH MONTE CARLO SIMULATION
!     NCENTS  -- NUMBER OF CENTERS IN EACH DISEASE TYPE
!     NINSIM  -- NUMBER OF TIMES TO SIMULATE THE INSIDE INFECTION CALCULATION
!     NMONT   -- NUMBER OF TIMES TO SIMULATE THE EXTERNAL SPREADRATE CALCULATION
!     NNCENT  --
!     NNDEC   --
!     NNDMX   --
!     NNINF   --
!     NRSTEP  --
!     NSCEN   --  NUMBER OF SHRINKING CENTERS
!     NSTUMP  --
!     NTREES  -- actual number of trees used in the simulation spread rate
!     NUMTRE  --
!     OOAREA  --
!     PAREA   --
!     PCENTS  --
!     PCOLO   -- (TREE SP,RR) PROPN ROOT RADIUS COLONIZED AFTER DEATH
!     PINSET  --
!     PISIZE  --
!     PKILLS  -- (TREE SP,RR) PROPN ROOT RADIUS COLONIZED CAUSING DEATH
!     PNINF   -- (TREE SP,RR) PROB. OF INFECTION GIVEN INSTANCE OF ROOT OVERLAP
!     PPUSH   --
!     PRINF   --
!     PRKILL  --
!     PROOT   --
!     PRPTOT  --
!     PRREM   --
!     PRUN    --
!     PTRE    --
!     RADINF  --
!     RDKILL  --
!     ROTSIT  --  ROOT RADIUS AT WHICH TO BEGIN 'SITTING' WITHOUT DECREASING
!     RRATES  --  SPREAD RATES FOR EACH CENTER
!     RRGEN   --
!     RRIARE  --
!     RRIDIM  --
!     RRIMEN  --
!     RRINCS  --
!     RRIRAD  --
!     RRRATE  --
!     RRRSET  --
!     RRSARE  --
!     RRSDBH  --
!     RRSFRN  --
!     RRSRAD  --
!     RSLOP   --
!     SAREA   --
!     SDRATE  --  STANDARD DEVIATION OF THE MONTE CARLO SIMULATION RESULTS
!     SHCENT  --  SHRINKING CENTER CHARACTERISTICS: 1=CENTER RADIUS WHICH
!                 SHRINKS IMMEDIATELY; 2=ANNUAL RATE OF THIS SHRINKAGE;
!                 3=YEARS AFTERWARDS OF NO SHRINKAGE (BUT WHEN TIME IS UP,
!                 THE WHOLE CENTER DISAPPEARS)
!     SPRQMD  --  QUADRATIC MEAN DIAM. OF TREES IN MONTE CARLO SIMULATION
!     SSSFAC  --
!     XDBH    --
!     XHT     --
!     XRRI    --
!     XRRS    --
!     XXINF   --
!     YDBH    --
!     YHT     --
!     YRRI    --
!     YRRS    --
!     YTKILL  --
!     YYINF   --
!
! --------------------------- WINDTHROW ---------------------------
!
!     ROWIBP  --
!     ROWDOM  --
!     ROWIND  --
!     ROWMIN  --
!     STEMSL  --
!     STEMSI  --
!     ROCRL   --
!     ROCRI   --
!     ILEND   --
!     IIEND   --
!     WINDSP  --
!     TSTEMS  --
!     WINDN   --
!
! --------------------------- BARK BEETLES ------------------------
!     NUMBB   -- NUMber of Bark Beetles eligible to act in this time
!                period, which cannot exceed 3*MAXSP
!     FRINGE(ITOTRR) -- the area in the stand that is within an average
!                root radius of a root rot infection center, for each
!                root rot type.  This area may experience greater
!                mortality due to bark beetles.
!     HOST(3*MAXSP) -- host tree species on which each beetle acts
!     MINDBH(3*MAXSP) -- MINimum host DBH at which trees become
!                susceptible to each BB
!     MININF(3*MAXSP) -- MINnimum proportion of roots INFected at which
!                IIRATE applies
!     ORATE(3*MAXSP) -- mortality rate Outside infection centers (and
!                any associated 'fringe') caused by each BB
!     OFRATE(3*MAXSP) -- mort. rate Outside infection centers in the
!                'Fringe' area that may experience greater mortality
!     IURATE(3*MAXSP) -- mort. rate Inside infection centers of
!                Uninfected trees, each BB
!     IIRATE(3*MAXSP) -- mort. rate Inside infection centers of Infected
!                trees, each BB
!     RROBBO --
!     RROBMR (BEETLE TYPE)  -- ARRAY THAT HOLDS THE MORTALITY RATE
!                THAT IS APPLIED TO ALL ELIGIBLE TREES IF AN OUTBREAK
!                OCCURS FOR EACH BEETLE TYPE.  THESE VALUES CAN BE
!                CHANGED BY THE FOUR BARK BEETLE KEYWORDS
!                (BBTYPE1, BBTYPE2, BBTYPE3, BBTYPE4)
!     RROBSC (BEETLE TYPE)  -- ARRAY THAT HOLDS THE MINIMUM DBH (INCHES)
!                FOR TREES WHICH WILL BE CONSIDERED ELIGIBLE FOR ATTACK
!                BY EACH BEETLE TYPE.  THESE VALUES CAN BE CHANGED BY
!                THE FOUR BARK BEETLE KEYWORDS
!                (BBTYPE1, BBTYPE2, BBTYPE3, BBTYPE4)
!     RROBRD  --
!     RROBNK  --
!     RROBTS (BEETLE TYPE)  -- ARRAY THAT HOLDS THE TREE SPECIES TO
!                INFEST FOR EACH BARK BEETLE TYPE.  THESE VALUES CAN BE
!                CHANGED BY THE FOUR BARK BEETLE KEYWORDS
!                (BBTYPE1, BBTYPE2, BBTYPE3, BBTYPE4)
!     RROBTY  --
!     RROBOL (BEETLE TYPE)  -- ARRAY THAT HOLDS THE MINIMUM DENSITY
!                (TREES/ACRE) OF ELIGIBLE TREES WHICH MUST BE PRESENT
!                FOR AN OUTBREAK TO OCCUR FOR EACH BARK BEETLE TYPE.
!                FOR BARK BEETLE TYPE 2 THIS VALUE IS THE MINIMUM NUMBER
!                OF TREES THAT MUST BE WINDTHROWN FOR THE OUTBREAK TO
!                OCCUR. THESE VALUES CAN BE CHANGED BY THE FOUR BARK
!                BEETLE KEYWORDS (BBTYPE1, BBTYPE2, BBTYPE3, BBTYPE4)
!     DBHD (root disease species, tree type, stump class) -- (The tree
!                types are heartwood and non-heartwood.  The stumps are
!                broken into 5 size classes).  This array holds the mean
!                stump diameter (inches) for the stumps in each tree
!                type for each stump class.
!     STCUT   --  BREAKPOINTS FOR THE STUMP SIZE CLASSES
!     ISPS    --
!     IRCOMP  --
!     ROOTH   --
!     XMTH    --
!     WK22    --
!     RROOTT  --
!     TXP12   --
!     ROOT4   --
!     IRRSP   -- FLAG THAT HOLDS THE ROOT DISEASE TYPE THAT BEING
!                SIMULATED:
!                1 = P-TYPE ANNOSUS
!                2 = S-TYPE ANNOSUS
!                3 = ARMILLARIA
!                4 = PHELLINUS
!     IFRRC   --
!     PNINF   --
!     TNJUMP  --
!     XMINKL  --
!     XMINLF  --
!     RRPSWT  --
!     PROBD   -- (DISEASE TYPE, TREE TYPE, STUMP CLASS)
!                THE TREE TYPES ARE HEARTWOOD AND NON-HEARTWOOD.
!                THE STUMPS ARE BROKEN INTO 5 DIAMETER CLASSES.
!                THIS ARRAY HOLDS THE NUMBER OF STUMPS IN THE ROOT
!                DISEASE AREA FOR EACH TREE TYPE AND STUMP CLASS FOR
!                THE CURRENT CYCLE.
!     PROBDA  -- (DISEASE TYPE, TREE TYPE, STUMP CLASS, TIME STEP)
!                THE TREE TYPES ARE HEARTWOOD AND NON-HEARTWOOD.  THE
!                STUMPS ARE STORED IN 5 DIAMETER CLASSES. THIS ARRAY HOLDS
!                THE NUMBER OF STUMPS IN THE ROOT DISEASE AREA FOR EACH
!                TREE TYPE, STUMP CLASS AND THE CYCLE (TIME STEP) IN WHICH
!                THE STUMPS ARE CREATED.  ARRAY IS UPDATED IN RDCNTL,
!                RDPUSH, RDSPOR, RDSTP, AND RDINOC.
!     ROOTD   -- (DISEASE TYPE, TREE TYPE, STUMP CLASS) THE TREE TYPES ARE
!                HEARTWOOD AND NON-HEARTWOOD.  THE STUMPS ARE STORED IN
!                5 SIZE CLASSES.  THIS ARRAY HOLDS THE ROOT RADIUS (FEET)
!                FOR THE STUMPS IN EACH TREE TYPE FOR EACH STUMP CLASS.
!     ROOTDA  -- (DISEASE TYPE, TREE TYPE, STUMP CLASS, TIME STEP)
!                4 DIMENSIONAL ARRAY THAT HOLDS THE STUMP ROOT RADIUS (FEET)
!                FOR THE STUMPS CREATED IN EACH TIME STEP.  THE STUMPS ARE
!                CATEGORIZED BY TREE TYPE (HEARTWOOD AND NON-HEARTWOOD) AND BY
!                STUMP CLASS (THERE ARE 5 STUMP DIAMETER CLASSES).  THE
!                ARRAY IS UPDATED IN RDSTP AND RDSPOR, DECAYED IN RDINOC.
!     DBHDA   -- (root disease species, tree type, stump size class,
!                time step) -- 4 dimensional array that holds the
!                stump diameter (inches) for the stumps created in
!                each time step.  The stumps are catergorized by root
!                disease species, tree type (heartwood and non-heartwod)
!                and by stump class (there are 5 stump diameter classes).
!                The array is updated in RDSTP and RDSPOR and decayed in
!                RDINOC.
!     JRAGED  --
!     RRJINC  --
!     IOUNIT  --
!     IRDOUT  --
!     IRUNIT  --
!
!-----END SEGMENT
