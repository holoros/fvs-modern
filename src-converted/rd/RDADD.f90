!ODE SEGMENT RDADD
!----------
! RD $Id$
!----------
LOGICAL BB1GO, BB2GO, LPLINF, REINGO, RRMAN, RRTINV, WINGO, &
           LBORAX(2), LPAREA(ITOTRR), LSPFLG(3)

INTEGER DEDAGE(5), IANPLT(ITOTRR,50), ICENSP(ITOTRR,100), &
           IDPLOT(MAXTRE), INFISD(ITOTRR,50), IPRFL(MAXTRE), &
           IRDPLT(ITOTRR,50), ISDATE(3), ISDOUT, &
           JCENSP(ITOTRR,100), LONECT(ITOTRR), MINRR, MAXRR, PINT

REAL    ANUINF(ITOTRR,50), BBKILL(4,IRRTRE), BODBH, BOTRT, &
           DBHOUT(ITOTRR,2,5,3), DBHUIN(ITOTRR,2,5,3), &
           DECFN(ITOTRR,2,2), DPROB(IRRTRE,4,2), FFPROB(IRRTRE,2), &
           OAKL(3,IRRTRE), PLPROP(ITOTRR,50), PROAKL(3,IRRTRE), &
           RINNF(ITOTRR), RINUF(ITOTRR), RISTU(ITOTRR), &
           RRNEW(ITOTRR), RSITFN(ITOTRR,2), &
           RTOUT(ITOTRR,2,5,3), RTUIN(ITOTRR,2,5,3), SDISLP, &
           SDNORM, SPDBH(ITOTRR), SPINF(ITOTRR), SPPROP(ITOTRR), &
           SPTRAN(ITOTRR), SPYTK(ITOTRR), SS, STOUT(ITOTRR,2,5,3), &
           STUIN(ITOTRR,2,5,3), YINCPT, YRSITF(ITOTRR,2,2)

DOUBLE PRECISION S0, S1

COMMON /RDADD/ ANUINF, BB1GO, BB2GO, BBKILL, BODBH, BOTRT, &
                  DBHOUT, DBHUIN, DECFN, DEDAGE, DPROB, FFPROB, &
                  IANPLT, ICENSP, IDPLOT, INFISD, IPRFL, IRDPLT, &
                  ISDATE, ISDOUT, JCENSP, LBORAX, LONECT, LPAREA, &
                  LPLINF, LSPFLG, MINRR, MAXRR, OAKL, PINT, &
                  PLPROP, PROAKL, REINGO, RINNF, RINUF, RISTU, &
                  RRMAN, RRNEW, RRTINV, RSITFN, RTOUT, RTUIN, &
                  SDISLP, SDNORM, SPDBH, SPINF, SPPROP, SPTRAN, &
                  SPYTK, STOUT, STUIN, WINGO, YINCPT, YRSITF

COMMON /RRANN/ S0, S1, SS
!
!     ANUINF  -- ALL NEW INFECTIONS OCCURING IN ONE INSIDE INFECTION
!                SIMULATION
!     BB1GO   --
!     BB2GO   --
!     BBKILL  -- TREES KILLED BY BARK BEETLES, BY TREE RECORD,TYPE
!     BODBH   -- MIN. DBH FOR BORAXING
!     BOTRT   -- PROPORTION BORAXED
!     DBHOUT(# root disease species, wood type: [resinous,
!            non-resionous], size class, 1) --
!                DBH of spore infected newly created stumps outside
!                centers.  The fourth element being set to 1 indicates
!                that the stumps were created in a short cycle (length
!                of cycle is less then 5 years) and the stumps need to
!                wait one more cycle before they become centers.
!     DBHOUT(# root disease species, wood type: [resinous,
!            non-resionous], size class, 2) --
!                DBH of spore infected newly created stumps outside
!                centers.  The fourth element being set to 2 indicates
!                that the cycle length was greater then 5 and that the
!                spore created stumps should become centers in the
!                following cycle.
!     DBHUIN(# root disease species, wood type: [resinous,
!            non-resionous], size class, 1) --
!                DBH of spore infected newly created stumps inside
!                centers.  The fourth element being set to 1 indicates
!                that the stumps were created in a short cycle (length
!                of cycle is less then 5 years) and the stumps need to
!                wait one more cycle before they are added to the
!                center stump list.
!     DBHUIN(# root disease species, wood type: [resinous,
!            non-resionous], size class, 2) --
!                DBH of spore infected newly created stumps inside
!                centers.  The fourth element being set to 2 indicates
!                that the cycle length was greater then 5 and that the
!                spore created stumps should be added to the center
!                stump list in the following cycle.
!     DECFN   -- SLOPE AND INTERCEPT OF THE ROOT DECAY FUNCTION
!                (FUNCT. OF INFECTED ROOT RADIUS)
!             -- (,,1) FOR DBH <= 12, (,,2) FOR DBH>12 (ONLY DIFFERENT
!                 FOR ARMIL.& PHELL.
!     DEDAGE  -- TIME SINCE DEATH OF INFECTED TREES AND STUMPS IN THE
!                INVENTORY (BY SIZE CLASS)
!     DPROB   -- DEAD TREE LIST:(i,,) = TREE RECORD, (,,1) = UNINFECTED,
!                (,,2) = INFECTED
!                (,1,) = ROOT DISEASE, (,2,) = BARK BEETLES,
!                (,3,) = WINDTHROW, (,4,) = NATURAL
!     FFPROB  -- The purpose of FFPROB for each tree record is to
!                provide the spread-rate model with a density of live
!                trees outside of root rot disease areas ("FPROB").
!                This takes into account the greater mortality caused
!                by BB4 in the "Fringe" area that is immediately
!                adjacent to disease areas (within a distance 'D' of
!                the root rot disease areas of the type to which the
!                tree species is susceptible.  The distance 'D'
!                is the average root diameter of all the trees outside
!                of those disease areas that are susceptible to that
!                root disease type).
!                FFPROB (RECORD,1) is the lower of a) the density of
!                live trees in the fringe area after the effects of
!                bark beetle activity and windthrow during the previous
!                growth cycle are removed, or b) the average density of
!                live trees outside of disease areas when the effects
!                of all sources of mortality in the last growth cycle
!                are removed.
!                FFPROB (RECORD,2) is the density of live trees in the
!                fringe area when the effects of the bark beetle
!                activity and windthrow that will occur in the current
!                growth cycle are removed.
!     IANPLT  -- PLOT ID FOR INFECTED PLOTS, BY DISEASE TYPE
!     ICENSP  --
!     IDPLOT  -- PLOT ID FOR EACH TREE RECORD THAT IS INSIDE AN
!                INFECTED PLOT
!     INFISD  -- # INFECTION SOURCES PLACED IN THE INSIDE CENTER SIM
!                OF INFECTION
!     IPRFL   --
!     IRDPLT  --
!     ISDATE(3) -- Array that holds the date on which stand entries
!                occur.  It only keeps track of the stand entries
!                for 3 cycles.  (1=current cycle, 2=previous cycle,
!                3=2 cycles before).  This is used along with the array
!                LSPFLG to test when stumps should move from the
!                spore infected stump list to the standard infected
!                stump list or the new centers list.
!     ISDOUT  -- FILE NUMBER FOR INSIDE INFECTION SIMULATION OUTPUT
!     JCENSP  -- YEAR THAT A SPORE CREATED CENTER WAS CREATED
!     LBORAX  -- (,1) WAS THERE A KEYWORD?, (,2) HAS OPTION PROCESSOR
!                BEEN CHECKED YET THIS YEAR?
!     LONECT  -- 0=unassigned, 1=stand as one plot, 2=multiplots
!     LPAREA  --
!     LPLINF  -- INITIALIZE INFECTION USING PROP OF PLOTS INFECTED
!                (PLOTINF KEYWORD)
!     LSPFLG(3) -- Flag that specifies whether a stand entry occurred
!                in the current cycle or one of the previous 2 cycles.
!                (1=current cycle, 2=previous cycle, 3=2 cycles before).
!                This is used along with the array ISDATE to test when
!                stumps should move from the spore infected stump list
!                to the standard infected stump list or the new
!                centers list.
!     MAXRR   -- HIGHEST NUMBER DISEASE USED IN CURRENT SIMULATION
!     MINRR   -- LOWEST NUMBER DISEASE USED IN CURRENT SIMULATION
!     OAKL    -- OTHER AGENT KILL TREELIST (BARK BEETLES AND WINDTHROW)
!     PINT    -- NUMBER OF YEARS THE PROBABILITY OF INFECTION IS
!                DEFINED FOR (10 YEARS)
!     PROAKL  -- Previous iteration OTHER AGENT killed treelist
!     REINGO  --
!     RINNF   -- NUMBER OF INFECTED TREE RECORDS READ FROM TREELIST
!     RINUF   -- NUMBER OF UNINFECTED TREE RECORDS READ FROM TREELIST
!     RISTU   -- NUMBER OF INFECTED STUMP RECORDS READ FROM TREELIST
!     RRMAN   -- FLAG THAT IS SET TO .TRUE. IF THE ROOT DISEASE MODEL
!                IS BEING USED.
!     RRNEW   -- MEAN LEVEL OF INFECTION FOR NEWLY INFECTED TREES
!     RRTINV  --
!     RSITFN  -- SLOPE AND INTERCEPT OF THE FUNCTION DEFINING ROOT
!                RADIUS AT WHICH TO STOP DECAYING FOR SOME TIME
!                (DEFINED BY YRSITF) (FUNCTION OF DBH)
!     RTOUT(# root disease species, wood type: [resinous,
!           non-resionous], size class, 1) --
!                Root radius of spore infected stumps outside centers.
!                The fourth element being set to 1 indicates
!                that the stumps were created in a short cycle (length
!                of cycle is less then 5 years) and the stumps need to
!                wait one more cycle before they can become centers.
!     RTOUT(# root disease species, wood type: [resinous,
!           non-resionous], size class, 2) --
!                Root radius of spore infected stumps outside centers.
!                The fourth element being set to 2 indicates
!                that the cycle length was greater then 5 and that the
!                spore created stumps should become centers in the
!                following cycle.
!     RTUIN(# root disease species, wood type: [resinous,
!           non-resionous], size class, 1) --
!                Root radius of spore infected stumps inside centers.
!                The fourth element being set to 1 indicates
!                that the stumps were created in a short cycle (length
!                of cycle is less then 5 years) and the stumps need to
!                wait one more cycle before they are added to the
!                center stump list.
!     RTUIN(# root disease species, wood type: [resinous,
!           non-resionous], size class, 2) --
!                Root radius of spore infected stumps inside centers.
!                The fourth element being set to 2 indicates
!                that the cycle length was greater then 5 and that the
!                spore created stumps should be added to the center
!                stump list in the following cycle.
!     SDISLP  -- SLOPE OF THE RELATIONSHIP BETWEEN SDI AND A ROOT
!                MULTIPLIER
!     SDNORM  -- "NORMAL" SDI: SDI FOR WHICH THE MULTIPLIER IS 1
!     SPDBH   -- Minimum dbh for creating a center through spore
!                infection
!     SPINF   --
!     SPPROP  --
!     SPTRAN  --
!     SPYTK   --
!     STOUT(# root disease species, wood type: [resinous,
!           non-resionous], size class, 1) --
!                Number of new spore infected stumps outside centers.
!                The fourth element being set to 1 indicates
!                that the stumps were created in a short cycle (length
!                of cycle is less then 5 years) and the stumps need to
!                wait one more cycle before they become a center.
!     STOUT(# root disease species, wood type: [resinous,
!           non-resionous], size class, 2) --
!                Number of new spore infected stumps outside centers.
!                The fourth element being set to 2 indicates
!                that the cycle length was greater then 5 and that the
!                spore created stumps should become centers in the
!                following cycle.
!     STUIN(# root disease species, wood type: [resinous,
!           non-resionous], size class, 1) --
!                Number of new spore infected stumps inside centers.
!                The fourth element being set to 1 indicates
!                that the stumps were created in a short cycle (length
!                of cycle is less then 5 years) and the stumps need to
!                wait one more cycle before they are added to the
!                center stump list.
!     STUIN(# root disease species, wood type: [resinous,
!           non-resionous], size class, 2) --
!                Number of new spore infected stumps inside centers.
!                The fourth element being set to 2 indicates
!                that the cycle length was greater then 5 and that the
!                spore created stumps should be added to the center
!                stump list in the following cycle.
!     WINGO   --
!     YINCPT  -- Y INTERCEPT OF THE LINE RELATING SDI AND ROOT
!                MULTIPLIER
!     YRSITF  -- SLOPE AND INTERCEPT OF THE FUNCTION DEFINING HOW LONG
!                TO STOP THE DECAY OF THE ROOT RADIUS BEFORE THE DEAD
!                THING DISAPPEARS (FUNCTION OF DBH)
!             -- (,,1) FOR DBH <= 12, (,,2) FOR DBH>12 (ONLY DIFFERENT
!                FOR ARMIL.& PHELL.
!
!     S0      --
!     S1      --
!     SS      --
!
!-----END SEGMENT
