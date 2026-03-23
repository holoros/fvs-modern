!ODE SEGMENT BRCOM
!----------
! WPBR $Id$
!----------
!  Common block variable definitions for the Blister Rust model.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  03-JUN-1999 Lance R. David (FHTET)
!     Added logical variables BRCHDR and BRTHDR to control main header
!     printing on canker and tree list files.
!  13-SEP-2000 Lance R. David (FHTET)
!     Transfered Glen Brink's July, 2000 modifications from older version
!     of blister rust source code:
!       Added ISPBR array and IBRDAM variable.
!     Also changed declaration of IBRTID from INTEGER*4 to INTEGER.
!  06-MAR-2001 Lance R. David (FHTET)
!     Added pathological pruning variable, LPATPR.
!     Added branch and bole canker growth rate variables (BRGRTH, BOGRTH).
!  14-MAR-2001 Lance R. David (FHTET)
!     Expanded variable DFACT to array by species and stock type.
!  21-MAR-2001 Lance R. David (FHTET)
!     added variables THPROB and TRETN to common block BRCOM.
!  24-APR-2001 Lance R. David (FHTET)
!     Added species dimension to PRPSTK and RESIST arrays.
!  01-MAY-2001 Lance R. David (FHTET)
!     Expanded tree category scalar variables to arrays.
!  08-MAY-2001 Lance R. David (FHTET)
!     Changed ISPBR to BRSPM and changed the manner in which the array
!     is utilized.
!     WPBR model arrays that were dimensioned by FVS parameter MAXSP
!     is now dimensioned using WPBR parameter NBRSP.
!  11-MAY-2001 Lance R. David (FHTET)
!     Added accumulator for total BR historical mortality (TBRHMR).
!  16-MAY-2001 Lance R. David (FHTET)
!     Expanded canker growth rate variables (BRGRTH, BOGRTH) for
!     species and stock type.
!  06-NOV-2002 Lance R. David (FHTET)
!     Added variables for new RI calculations: MINRI, MAXRI, PKAGE, RISHP
!  08-MAY-2006 Lance R. David (FHTET)
!     Changed random number seed variable names to unique variables
!     BRS0, BRS1, BRSS. Changed variable NTREES to BRNTRECS.
!     Changed array ISTAT to IBRSTAT.
!  15-MAY-2006 Lance R. David (FHTET)
!     Added variables LBRDBH and LBRSUM.
!**********************************************************************
INTEGER NBRSP
PARAMETER (NBRSP=2)

!.... Variable declarations.

DOUBLE PRECISION BRS0,BRS1,BRSS

LOGICAL BRCHDR,BRCL,BRCMP,BRTHDR,BRTL,BRYES,CKINIT, &
     LCLEN,LDFACT,LEXGO,LEXMLT(MAXTRE),LMETRIC,LPATPR, &
     LPRGO,LPRUN,LBRDBH,LBRSUM,LBROUT(NBRSP)

CHARACTER*4  BRSPC(NBRSP)
CHARACTER*80 ICFMT

INTEGER BRSPM(MAXSP),IBRDAM,IBRTID(MAXTRE),ICIN,ICRED(MAXTRE), &
     IDCOUT,IDTOUT,ILCAN(MAXTRE),INCAN,IBRSTAT(MAXTRE), &
     ISTCAN(10,MAXTRE),ISTOTY(MAXTRE),ITCAN(MAXTRE), &
     BRNTRECS(NBRSP),RIMETH,LOTCA

REAL &
     AVECPT(NBRSP),AVGGI(NBRSP),AVGRI(NBRSP),AVGSTS(NBRSP), &
     AVLCPT(NBRSP),AVTCPT(NBRSP),BOGRTH(NBRSP,4),BRAGE(MAXTRE), &
     BRDBHO(10,13,41),BRGD(MAXTRE),BRGRTH(NBRSP,4),BRHTBC(MAXTRE), &
     BROUT(NBRSP,18,41),BRPB(MAXTRE),BRPI, &
     D2AVE(10),D2AVL(10),D2AVT(10), &
     D2CLN(10),D2DED(10),D2EXC(10),D2GIR(10),D2NOL(10),D2NOS(10), &
     D2PIL(10),D2PIT(10),D2PRN(10),D2WP(10),DFACT(NBRSP,4), &
     DOUT(10,MAXTRE),DUP(10,MAXTRE),ESTCAN(MAXTRE),EXDMIN,GI(MAXTRE), &
     GIDEF,GIRDL(10,MAXTRE),GIRMAX,GIRMRT,HTMAX(2),HTMIN,HTPRPR, &
     MAXRI,MINRI,OUTDST,OUTNLD,PILCA(NBRSP),PITCA(NBRSP),PKAGE, &
     PKSHP,PRPSTK(NBRSP,4),RATINV(2),RESIST(NBRSP,4),RI(MAXTRE), &
     RIAF(NBRSP),RIBPRP(3),RIDEF,RSF(3),SRATE(2),STSUM(NBRSP), &
     TBRCLN(NBRSP),TBREXC(NBRSP),TBRGIR(NBRSP),TBRHMR(NBRSP), &
     TBRHST(NBRSP),TBRMRT(NBRSP),TBRNOL(NBRSP),TBRNOS(NBRSP), &
     TBRPRN(NBRSP),THPROB(NBRSP),TRETN(NBRSP),TSTARG(MAXTRE), &
     UPMARK(MAXTRE),TOTCA

REAL FACTOR(3),RIBSUM(2),RIBUS(2,3)
!.... Common blocks.

COMMON/DPCOM/BRS0,BRS1,BRSS
COMMON/BRCHCM/ICFMT,BRSPC
COMMON/RIBES/FACTOR,RIBUS,RIBSUM,RSF
COMMON/BRCOM/ &
     AVECPT,AVGGI,AVGRI,AVGSTS,AVLCPT,AVTCPT,BOGRTH,BRAGE,BRCHDR, &
     BRCL,BRCMP,BRDBHO,BRGD,BRGRTH,BRHTBC,BROUT,BRPB,BRPI,BRSPM, &
     BRTHDR,BRTL,BRYES,CKINIT,D2AVE,D2AVL,D2AVT,D2CLN,D2DED,D2EXC, &
     D2GIR,D2NOL,D2NOS,D2PIL,D2PIT,D2PRN,D2WP,DFACT,DOUT,DUP, &
     ESTCAN,EXDMIN,GI,GIDEF,GIRDL,GIRMAX,GIRMRT,HTMAX,HTMIN, &
     HTPRPR,IBRDAM,IBRTID,ICIN,ICRED,IDCOUT,IDTOUT,ILCAN,INCAN, &
     IBRSTAT,ISTCAN,ISTOTY,ITCAN,LCLEN,LDFACT,LEXGO,LEXMLT,LMETRIC, &
     LOTCA,LPATPR,LPRGO,LPRUN,MAXRI,MINRI,BRNTRECS,OUTDST,OUTNLD, &
     PILCA,PITCA,PKAGE,PKSHP,PRPSTK,RATINV,RESIST,RI,RIAF,RIBPRP, &
     RIDEF,RIMETH,SRATE,STSUM,TBRCLN,TBREXC,TBRGIR,TBRHMR,TBRHST, &
     TBRMRT,TBRNOL,TBRNOS,TBRPRN,THPROB,TOTCA,TRETN,TSTARG,UPMARK, &
     LBRDBH,LBRSUM,LBROUT

!.... Definitions of variables in BRCOM common blocks.

!....   BRS0 -- Used in the random number generator routine BRANN.
!....   BRS1 -- Used in the random number generator routine BRANN.
!....   BRSS -- Used in the random number generator routine BRANN.
!.... UPMARK -- Distance up from ground level in centimeters of highest
!....           canker in the tree. This is initialized in BRCANK when
!....           canker data is entered. It is changed in BRCGRO.
!....     GI -- Growth index for the individual tree. Calculated in
!....           BRGI.
!....     RI -- Rust index for individual tree. Calculated in BRTARG
!....           and BRTREG.
!....   BRGD -- Tree ground diameter in centimeters. Calculated in
!....           BRSETP and updated in BRUPDT.
!.... BRHTBC -- Height to base of crown for the tree.  This value is
!....           initialized using the Prognosis crown ratio and is
!....           changed by pruning.
!....   BRPB -- Trees/acre dead due to blister rust. Calculated in
!....           BRCGRO.
!.... ESTCAN -- Estimated number of cankers expected in this year.
!....           Calculated in BRECAN.
!.... TSTARG -- Cumulative target area for the individual tree.
!....           Calculated in BRGI.
!....  ITCAN -- Total number of cankers for the individual tree. This
!....           number includes both potentially lethal and non-lethal
!....           cankers. This number can be larger than 10 for the tree.
!....           These cankers are just not tracked by the model.
!....           Calculated in BRCANK. Modified in BRCDEL, BRECAN.
!....  ILCAN -- Number of potentially lethal cankers for the tree.
!....           Actually this number is the cankers that will be tracked
!....           by the model (and therefore have the potential to become
!....           lethal - as opposed to the number stored in ITCAN which
!....           is used to determine spore production but which are
!....           never grown in the model). Maximum number of "lethal"
!....           cankers is 10. Calculated in BRCANK.  Modified in
!....           BRECAN and BRCDEL.
!.... ISTOTY -- Stock type of planted trees. Set in BRESTB. Modified in
!....           BRSTYP.
!.... IBRTID -- Unique tree identification number. Set in BRCANK.
!...  BRSPC  -- BR Host tree species alpha codes. Index values in BRSPM
!...            point to the appropriate position of the species in this
!...            array. Set in BRBLKD.F.
!...  BRSPM  -- A non-zero value in this array is the corresponding tree
!...            species index in the blister rust arrays. This array
!...            index/species order is that of FVS. Species that are
!...            blister rust hosts and represented by this model will
!...            have the appropriate index value in the host species
!...            position. See definition of BRSPC for list of BR host
!...            species and index values.
!...            BRSPM is set in BRBLKD.F and is variant-specific.
!...            0=Species not a host to Blister Rust
!...  IBRDAM -- Blister Rust damage code (initialized in brklkd.f)
!.... BRAGE  -- Tree age loaded from CANKDATA and is incremented on a
!....           yearly basis. If tree age is not supplied with canker
!....           data, Prognosis stand age is used to initialize it in
!....           subroutine BRSOR.
!....  ICRED -- Flag used to determine if crown ratio of the tree is to
!....           be reduced in this cycle. Set in BRCGRO and BRCSTA.
!....IBRSTAT -- Current status of tree based on the worst canker on the
!....           tree. Computed in BRTSTA.
!.... ISTCAN -- Status of individual cankers on the tree. Canker
!....           Classifications are: 0=clean tree, 1=non-lethal canker,
!....           2=prunable canker, 3=excisable canker, 4=non-salvable
!....           canker, 5=tree topkilled by canker and 7= tree killed by
!....           canker.
!....  GIRDL -- Percentage of tree bole circumference which canker has
!....           girdled. Range: 0 to 100 percent.
!.... GIRMRT -- Canker girdling percent which causes death.  This value
!....           is used when determining the canker status to define
!....           when a bole canker's girdling topkills or kills the
!....           tree depending on the cankers height relative to the
!....           height to base of crown.
!....  HTMIN -- Mininmum height in centimeters a canker must be above
!....           ground to qualify for excising.  Bole cankers below
!....           this height are assigned a status code 4 (non-salvable).
!....           Initialized in BRINIT.  changed through EXSPECS keyword
!....           field 6.
!....    DUP -- Distance up from ground level (in centimeters) of canker
!....           for either bole or branch canker. Initialized IN BRCANK.
!....           Assigned for new cankers in BRECAN.
!....   DOUT -- Distance out (in centimeters) of canker on the tree
!....           branch (0 for bole cankers). Initialized in BRCANK.
!....           Assigned for new cankers in BRECAN.
!....  TOTCA -- Total number of cankers.
!....  LOTCA -- Total number of lethal cankers.
!.... LBRDBH -- Logical variable controls writing of DBH class statistics.
!.... LBRSUM -- Logical variable controls writing of summary statistics.
!.... LBROUT -- Logical variable array set true if output info has been
!....           generated for a host to avoid writing tables full of zeros.
!....
!.... The following arrays are "tree category" variables
!....
!.... TBRHST -- Total blister rust host TPA by tree species.
!....           This is a running total for the simulation and not just
!....           current cycle live trees and mortality.
!....           Accumulated in BRSTYP and ???
!....           Should be the sum of the following 7 categories:
!....           (TBRCLN, TBRNOL, TBRPRN, TBREXC, TBRNOS, TBRGIR, TBRMRT)
!.... TBRCLN -- Total host TPA without cankers (i.e. clean).
!....           Calculated in BRTSTA.
!.... TBRNOL -- Total host TPA with non-lethal cankers. Calculated
!....           in BRTSTA.
!.... TBRPRN -- Total host TPA with prunable cankers. Calculated
!....           in BRTSTA.
!.... TBREXC -- Total host TPA with excisable cankers. Calculated
!....           in BRTSTA.
!.... TBRNOS -- Total host TPA with non-salvable cankers.
!....           Calculated in BRTSTA.
!.... TBRGIR -- Total host TPA topkilled by blister rust (i.e. a canker
!....           has girdled a tree above its "mortal" height).
!....           Calculated in BRTSTA.
!.... TBRMRT -- Total blister rust mortality TPA by tree species.
!....           This is a running total for the simulation and not just
!....           current cycle mortality.
!....           Calculated in BRTSTA ???.
!.... TBRHMR -- Total blister rust historical mortality TPA by species.
!....           This is a running total for the simulation. Set in BRINIT
!....           and accumulated in BRCGRO. Used BRSTAT to calculate
!....           proportion of trees infected (PITCA and PILCA).
!....
!.... BRNTRECS-- Number of host species tree records.  Note this is not the
!....           same as TBRHS which is total trees/acre of host species.
!.... AVTCPT -- Average total number of cankers (lethal and non-lethal)
!....           per tree per acre. Calculated in BRSTAT.
!.... AVLCPT -- Average number of lethal cankers per tree per acre.
!....           Calculated in BRSTAT.
!....  BROUT -- Array for storing the blister rust summary output data.
!....           Loaded in BRSUM.
!....   BRPI -- The value of pi, i.e. 3.1417. Set in BRBLKD.
!.... HTPRPR -- Value used to determine the threshold for pruning.
!....           Expressed as a proportion of total tree height below
!....           which cankers will be considered for pruning.
!....           Entered on PRNSPECS keyword. Initialized in BRINIT.
!.... EXDMIN -- Value used to determine the threshold for excising.
!....           This is the minimum DBH of the tree above which bole
!....           cankers will be considered for excising.  Entered on the
!....           EXSPECS keyword.  Initialized in BRINIT.
!....  HTMAX -- Values used to determine thresholds for pruning and
!....           excising. Expressed as maximum absolute tree height in
!....           centimeters below which cankers will be considered for
!....           removal. Initialized in BRINIT.  changed through
!....           PRNSPECS and EXSPECS keywords.
!.... GIRMAX -- Value used to determine whether tree is excisable or
!....           non-salvable. This is the maximum percent of the tree
!....           bole circumference that can be girdled after which a
!....           canker is no longer excisable. Entered in EXSPECS
!....           keyword. Initialized in BRINIT.
!....  SRATE -- Used to set the 'success rate' of pruning or excising.
!....           Entered on the PRUNE and EXCISE keywords. Initialized
!....           in BRINIT.
!....  AVGRI -- Average value of rust index for the stand. Calculated in
!....           BRTREG.
!....  AVGGI -- Average value of growth index for the stand. Calculated
!....           in BRTREG.
!....  DFACT -- Stand deviation factor array by species and stock type.
!....           Calculated in BRTARG.
!....           Initialized in BRINIT. Can be set with RUSTINDX keyword.
!....           When set by keyword, the supplied value WILL be used.
!....           Stand deviation factor cannot be less than 0.01.
!.... LDFACT -- Logical variable set to true when the stand deviation
!....           factor is provided by keyword RUSTINDX.
!....  STSUM -- Sum of target areas for the entire stand in the current
!....           cycle.
!.... AVGSTS -- Average sum of target area for the stand in the current
!....           cycle.
!.... AVECPT -- Average number of expected cankers per tree per acre.
!....           Calculated in BRSTAT.
!....  PILCA -- Proportion of trees per acre infected with lethal
!....           cankers. Calculated in BRSTAT.
!....  PITCA -- Proportion of trees per acre infected with lethal and
!....           non-lethal cankers. Calculated in BRSTAT.
!.... THPROB -- Total host trees per acre used in calculating PITCA
!....           and monitoring the increase of proportion of host trees
!....           with infections.
!....  TRETN -- Total trees per acre of host with infections (lethal or
!....           non-lethal).
!....LMETRIC -- Logical variable set to TRUE if canker data is provided
!....           in centimeters; FALSE if data is in feet and inches.
!....           Initialized in BRINIT. Changed by CANKDATA keyword.
!....   ICIN -- Logical unit number for where to read the canker data
!....           from.  Initialized in BRBLKD.  Also entered in the
!....           CANKDATA keyword.
!.... IDTOUT -- Logical unit number for where detailed tree output is
!....           to be written. Initialized in BRBLKD. Also entered on
!....           BRTLST keyword.
!.... IDCOUT -- Logical unit number for where detailed canker output is
!....           to be written. Initialized in BRBLKD. Also entered on
!....           BRCLST keyword.
!....  ICFMT -- Format used to read the input canker data file.
!....           Initialized in BRBLKD. Reset using the CANFMT keyword.
!....  RIBPRP - Real array of 3. Each ribes species proportion of
!....           total ribes population. Used for weighted Average
!....           method of determining stand rust index from basal area.
!....  RIDEF -- Default Rust Index value. Initialized in BRINIT.
!....           Entered directly or calculated as specified by RUSTINDX
!....           keyword.  Also may be calculated directly from ribes
!....           populations which is controlled by RIBES keyword.
!.... RIMETH -- Rust Index assignment method.  Initialized in BRINIT.
!....           Controls how the stand rust index (RIDEF) will be
!....           calculated and assigned to individual trees.
!....           Value changed only through RUSTINDX keyword.
!....  GIDEF -- Default growth index value. Initialized in BRINIT.
!.... OUTDST -- Distance out in the TREE branch in centimeters which
!....           defines a prunable canker. If a canker is at a distance
!....           out greater than the value of OUTDST then the canker is
!....           comsidered to be prunable. If the canker is at a
!....           distance out less than the value of OUTDST then the
!....           canker is considered non-prunable. Initialized in BRINIT
!....           changed through PRNSPECS keyword.
!.... OUTNLD -- Distance out on the tree branch (in centimeters) which
!....           defines a non-lethal canker. If a canker is at a
!....           distance out greater than the value of OUTNLD then the
!....           canker is considered to be non-lethal. Initialized in
!....           BRINIT. Change through the PRNSPECS keyword.
!.... RESIST -- Resistance factor to blister rust for the four types of
!....           planting stock for each host species.
!....           The types are: 1=WILD stock, 2=F1 stock, 3=F2 stock and
!....           4=F3 stock. Factors are set in BRINIT and changed using
!....           the STOCK keyword.
!.... RATINV -- Canker inactivation rates. Initialized in BRINIT.  Also
!....           set using the INACT keyword.  RATINV(1) holds the canker
!....           inactivation rate for branch cankers; RATINV(2) holds
!....           the same for bole cankers.
!....  INCAN -- Number of unique tree ID's encountered in the blister
!....           rust canker data file. Calculated in BRCANK.
!.... PRPSTK -- Proportions of each host species stock type making up the
!....           population. Initialized in BRINIT and changed using the
!....           STOCK keyword.
!....  BRYES -- Logical variable whcih is TRUE if the blister rust model
!....           is being used in the current simulation. Initialized in
!....           BRINIT. Set in BRIN.
!.... BRCHDR -- Logical variable controlling the printing of initial header
!....           information in the detailed canker list output file.
!....           Set to TRUE in BRINIT.
!....   BRCL -- Logical variable which triggers the detailed canker
!....           output. Initialized in BRINIT to FALSE. Set to TRUE with
!....           the BRCLST keyword.
!.... BRTHDR -- Logical variable controlling the printing of initial header
!....           information in the detailed tree list output file.
!....           Set to TRUE in BRINIT.
!....   BRTL -- Logical variable which triggers the detailed tree list
!....           output. Initialized in BRINIT to FALSE. Set to TRUE with
!....           the BRTLST keyword.
!....  BRCMP -- Logical variable used to trigger assignment of canker
!....           statuses when a compression has been performed.
!....           subroutine BRCSTA will be called from subroutine BRPR
!....           when BRCMP is true.
!.... BOGRTH -- Bole canker diameter growth rate (cm)-specie & stock type.
!.... BRGRTH -- Branch canker radial growth rate (cm)-specie & stock type.
!.... LPATPR -- Logical variable for pathological pruning (do not change
!....           crown base heihgts) when true. Default is false.
!....  LPRUN -- Logical variable which indicated whether prunable trees
!....           will be pruned when a pruning is scheduled. Initialized
!....           to FALSE in BRINIT. Set to TRUE in BRTREG.
!....  LCLEN -- Logical variable which indicated whether clean trees
!....           will be pruned when a pruning is scheduled. Initialized
!....           to FALSE in BRINIT. Set to TRUE in BRTREG.
!....  LPRGO -- Logical variable which triggers a scheduled pruning. Set
!....           to FALSE in BRINIT. Reset to TRUE in BRTREG in the cycle
!....           in which pruning has been scheduled.
!....  LEXGO -- Logical variable which triggers a scheduled excising.
!....           Set to FALSE in BRINIT. Reset to TRUE in BRTREG in the
!....           cycle in which excising has been scheduled.
!....  RIBUS -- Array holding the number of ribes bushes per acre of
!....           the three different species of ribes.
!....           Initialized in BRBLKD. Changed with the RIBES keyword.
!.... FACTOR -- Real array of 3.  Holds calculated RI values for the
!....           three ribes species populations.  Calculated in BRIBES.
!....           After call to BRIBES, the values remaining in array are
!....           from the new population values stored in RIBUS(2,X).
!.... RIBSUM -- Array used to hold summed variables for use in
!....           calculating reduction factor.
!....    RSF -- Real array of 3. Ribes Species Factors (Hud, Lac, Vis).
!....           Calibration values for RI base line function in
!....           subroutines BRIBES and BRIBA.
!.... LEXMLT -- Logical array of size MAXTRE.  LEXMLT will be set to
!....           TRUE for a tree that has more than one (i.e. multiple)
!....           excisable cankers in any given cycle.  This is used
!....           for when excising is scheduled; in the field trees
!....           with more than 1 excisable canker will not be excised
!....           at all.  Initialized in BRINIT, updated in BRCGRO and
!....           BRCSTA, used in BRCREM.
!.... BRDBHO -- Real, 3-dimensional storage array for holding summary
!....           information for the 2-inch DBH class blister rust table.
!....           Loaded in BRSUM. BRDBHO(10,13,41) holds 10 2-inch DBH
!....           classes, 13 categories of information (see D2 variables
!....           below) and up to 41 cycles worth of data for the table.
!.... D2AVT  -- Real array to store the average total cankers/tree/acre
!....           data by 2-inch DBH class. Calculated in BRSTAT.
!.... D2AVL  -- Real array to store the average lethal cankers/tree/acre
!....           data by 2-inch DBH class. Calculated in BRSTAT.
!.... D2AVE  -- Real array to store the average expected canks/tree/acre
!....           data by 2-inch DBH class. Calculated in BRSTAT.
!.... D2PIT  -- Real array to store the proportion of infected
!....           trees/acre total (lethal and non-lethal) by 2-inch DBH
!....           class.  Calculated in BRSTAT.
!.... D2PIL  -- Real array to store the proportion of infected
!....           trees/acre (lethal only) by 2-inch DBH class.
!....           Calculated in BRSTAT.
!.... D2CLN  -- Real array to store total clean TPA by 2-inch DBH class.
!....           Calculated in BRTSTA.
!.... D2NOL  -- Real array to store total non-lethal TPA by 2-inch DBH
!....           class. Calculated in BRTSTA.
!.... D2PRN  -- Real array to store total prunable TPA by 2-inch DBH
!....           class. Calculated in BRTSTA.
!.... D2EXC  -- Real array to store total excisable TPA by 2-inch DBH
!....           class. Calculated in BRTSTA.
!.... D2NOS  -- Real array to store total non-salvable TPA by 2-inch DBH
!....           class. Calculated in BRTSTA.
!.... D2GIR  -- Real array to store total girdled/topkilled TPA by
!....           2-inch DBH class. Calculated in BRTSTA.
!.... D2DED  -- Real array to store total dead (from WPBR) TPA by
!....           2-inch DBH class. Calculated in BRTSTA.
!.... D2WP   -- Real array to store total TPA that are white pine by
!....           2-inch DBH class. Should be the totals in each of the
!....           DBH classes of each of the statuses (D2CLN, D2NOL,
!....           D2PRN, D2EXC, D2NOS, D2GIR, D2DED). Calculated in
!....           BRTSTA.
!.... CKINIT -- Logical flag set with the CANKDATA keyword to signal
!....           that canker up, out, and girdle measurements will be
!....           randomly generated according to the canker counts read
!....           from the canker data (required last canker record for
!....           each tree); cankers are generated in BRCINI if this
!....           option is requested, and if the count dictates.
!.... RIAF   -- Rust index adjustment factor (real).  Calculated once
!....           from initial tree conditions in BRTARG and used to
!....           adjust the default rust index. Default value is set to
!....           1.0 in BRINIT to allow for simulations that do not have
!....           initial canker information (tree conditions) which
!....           includes bare ground runs where all trees are PLANTed.
!.... MINRI  -- minumum Rust Index (in subroutine BRICAL)
!.... MAXRI  -- maximum Rust Index (in subroutine BRICAL)
!.... PKAGE  -- stand age (exposure time) when Rust Index peaks
!....           (in subroutine BRICAL)
!.... RISHP  -- defines shape of curve about the peak (in subroutine BRICAL)
