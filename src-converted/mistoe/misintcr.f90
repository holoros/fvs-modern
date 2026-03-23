SUBROUTINE MISINT
!**********************************************************************
! MISTOE $Id$
!----------------------------------------------------------------------
!  Purpose:
!     Mistletoe parameter initialization routine. This routine is
!  variant dependent and sets the variant dependent variables for other
!  mistletoe routines. This is the Central Rockies version.
!  ********
!  **NOTE** Growth and mortality coefficients are from the Utah variant
!  ********
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
!     IMODTY: From PLOT; input model type code.
!     JOSTND: From CONTRL; logical unit number for stand output.
!     MAXSP:  From PRGPRM; maximum # species.
!     MISFIT: From MISCOM; tells which species are affected by DM.
!     PMCSP:  From MISCOM; percent mortality coefficients by species.
!     USEMRT: From MISCOM; true if using this model's mortality calcs.
!
!
!  24-FEB-98 Update Lance David
!     Changed use of ITYPE (PLOT common variable) to
!     IMODTY (PLOT common variable) which is set by FVS MODTYPE keyword.
!     The removal of embedded mistletoe effects in the FVS Central Rockies
!     variant prompted the following:
!       Activation of mortality (USEMRT=.TRUE.).
!       Activation of diameter growth potential values.
!
!  16-DEC-98 Update Matt Oberle (MJO DEC98)
!     In response to Bulletin #363 (CR species list expanded to include
!       24 species -- realigned the surrogate species assignments for
!       consistency between model types for a given species).
!     Expanded the data statements to include the new, global species
!       list, changed dimension of ACSP(5,MAXSP) to (MAXSP).
!     Blank data statements for the future Aspen model type (6) were
!       added and commented out.
!     Although parmameters are currently the same for each species
!       across model types, I chose to keep this model type-
!       specific structure for flixibiltiy -- we may find the need for
!       species x model type specific parameters in the future. The
!       only data block that was collapsed to 1 dimension was the
!       'species character representations'.
!     Blank data statements for the future Aspen model type (6) were
!       added and commented out.
!
!  15-MAR-00 Update Lance David
!     The species identified as dwarf mistletoe host had not been
!       addressed when the tree species represented by the Central Rockies
!       variant was expanded to 24 and made consistant across all Model
!       types. As a result, only 5 tree species were recognized as host.
!       Parameters assigned for growth and mortality are the same across
!       all model types. Having mistletoe available for the Black Hills
!       model type is not a concern, because the model won't be
!       automatically activated unless there are mistletoe damage codes
!       in the data set, in which case the data may not be Black Hills
!       data and inclusion of mistletoe may be appropriate.
!  21-APR-09 Lance R. David (FMSC)
!     Changed species code WP to SW (Southwestern White Pine).
!  17-JUN-09 Gary Dixon (FMSC)
!     Expanded from 24 species to 38 species
!  12-JUL-2011 Lance R. David (FMSC)
!    Added arrays for height growth impacts.
!    Impact values must be supplied by MistHMod keyword.
!**********************************************************************
IMPLICIT NONE

!.... Parameter statements.
!.... Parameter include files.
INCLUDE 'PRGPRM.f90'
!.... Common include files.
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'MISCOM.f90'
!.... Variable declarations.
LOGICAL DEBUG
INTEGER I,J,K,LTYPE,AFIT(5,MAXSP)
REAL ADGP(5,MAXSP,7),AHGP(MAXSP,7),APMC(5,MAXSP,3)
CHARACTER*2 ACSP(MAXSP)
!.... Data statements.
!.... All the data for the 5 subvariants is stored in these data
!.... statements.  We learn at run-time which subvariant the user
!.... selected, and copy these values to the working arrays.
!....   (1) Southwest Mixed Conifer.
!....   (2) Southwest PP and PJ.
!....   (3) Black Hills.
!....   (4) Spruce-Fir.
!....   (5) Lodgepole Pine.
!....   (6) Aspen (to be completed)
!
!.... Species affected or unaffected by dwarf mistletoe.
DATA ((AFIT(I,J),J=1,MAXSP),I=1,1)/ &
     1, 1, 1, 1, 1, 1, 0, 1, 1, 1, &
     1, 1, 1, 1, 1, 0, 1, 1, 0, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     0, 0, 1, 1, 1, 1, 0, 0/
DATA ((AFIT(I,J),J=1,MAXSP),I=2,2)/ &
     1, 1, 1, 1, 1, 1, 0, 1, 1, 1, &
     1, 1, 1, 1, 1, 0, 1, 1, 0, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     0, 0, 1, 1, 1, 1, 0, 0/
DATA ((AFIT(I,J),J=1,MAXSP),I=3,3)/ &
     1, 1, 1, 1, 1, 1, 0, 1, 1, 1, &
     1, 1, 1, 1, 1, 0, 1, 1, 0, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     0, 0, 1, 1, 1, 1, 0, 0/
DATA ((AFIT(I,J),J=1,MAXSP),I=4,4)/ &
     1, 1, 1, 1, 1, 1, 0, 1, 1, 1, &
     1, 1, 1, 1, 1, 0, 1, 1, 0, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     0, 0, 1, 1, 1, 1, 0, 0/
DATA ((AFIT(I,J),J=1,MAXSP),I=5,5)/ &
     1, 1, 1, 1, 1, 1, 0, 1, 1, 1, &
     1, 1, 1, 1, 1, 0, 1, 1, 0, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     0, 0, 1, 1, 1, 1, 0, 0/
!      DATA ((AFIT(I,J),J=1,MAXSP),I=6,6)/
!     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
!     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
!     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
!     & 0, 0, 0, 0, 0, 0, 0, 0/
!
!.... Species character representations.
DATA (ACSP(J),J=1,MAXSP)/ &
      'AF','CB','DF','GF','WF','MH','RC','WL','BC','LM', &
      'LP','PI','PP','WB','SW','UJ','BS','ES','WS','AS', &
      'NC','PW','GO','AW','EM','BK','SO','PB','AJ','RM', &
      'OJ','ER','PM','PD','AZ','CI','OS','OH'/
!
!.... Diameter growth rates.
!.... Southwest Mixed Conifer.
DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=1,1)/ &
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 1  AF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 2  CB (GF      )
      1.0,.98,.97,.85,.80,.52,.44, &  ! 3  DF (  DF    )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 4  GF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 5  WF (GF      )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 6  MH (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 7  RC not host
      1.0,.94,.92,.88,.84,.58,.54, &  ! 8  WL
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 9  BC (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 10 LM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 11 LP (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 12 PI (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 13 PP (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 14 WB (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 15 SW (    LP  )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 16 UJ not host
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 17 BS (      PP)
      1.0,.98,.97,.85,.80,.52,.44, &  ! 18 ES (  DF    )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 19 WS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 20 AS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 21 NC not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 22 PW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 23 GO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 24 AW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 25 EM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 26 BK not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 27 SO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 28 PB not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 29 AJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 30 RM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 31 OJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 32 ER not host
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 33 PM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 34 PD (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 35 AZ (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 36 CI (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 37 OS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host

!.... Southwest PP and PJ.
DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=2,2)/ &
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 1  AF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 2  CB (GF      )
      1.0,.98,.97,.85,.80,.52,.44, &  ! 3  DF (  DF    )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 4  GF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 5  WF (GF      )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 6  MH (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 7  RC not host
      1.0,.94,.92,.88,.84,.58,.54, &  ! 8  WL
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 9  BC (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 10 LM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 11 LP (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 12 PI (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 13 PP (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 14 WB (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 15 SW (    LP  )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 16 UJ not host
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 17 BS (      PP)
      1.0,.98,.97,.85,.80,.52,.44, &  ! 18 ES (  DF    )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 19 WS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 20 AS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 21 NC not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 22 PW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 23 GO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 24 AW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 25 EM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 26 BK not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 27 SO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 28 PB not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 29 AJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 30 RM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 31 OJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 32 ER not host
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 33 PM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 34 PD (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 35 AZ (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 36 CI (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 37 OS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host

!.... Black Hills.
DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=3,3)/ &
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 1  AF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 2  CB (GF      )
      1.0,.98,.97,.85,.80,.52,.44, &  ! 3  DF (  DF    )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 4  GF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 5  WF (GF      )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 6  MH (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 7  RC not host
      1.0,.94,.92,.88,.84,.58,.54, &  ! 8  WL
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 9  BC (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 10 LM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 11 LP (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 12 PI (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 13 PP (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 14 WB (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 15 SW (    LP  )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 16 UJ not host
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 17 BS (      PP)
      1.0,.98,.97,.85,.80,.52,.44, &  ! 18 ES (  DF    )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 19 WS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 20 AS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 21 NC not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 22 PW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 23 GO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 24 AW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 25 EM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 26 BK not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 27 SO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 28 PB not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 29 AJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 30 RM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 31 OJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 32 ER not host
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 33 PM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 34 PD (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 35 AZ (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 36 CI (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 37 OS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host

!.... Spruce-Fir.
DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=4,4)/ &
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 1  AF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 2  CB (GF      )
      1.0,.98,.97,.85,.80,.52,.44, &  ! 3  DF (  DF    )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 4  GF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 5  WF (GF      )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 6  MH (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 7  RC not host
      1.0,.94,.92,.88,.84,.58,.54, &  ! 8  WL
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 9  BC (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 10 LM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 11 LP (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 12 PI (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 13 PP (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 14 WB (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 15 SW (    LP  )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 16 UJ not host
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 17 BS (      PP)
      1.0,.98,.97,.85,.80,.52,.44, &  ! 18 ES (  DF    )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 19 WS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 20 AS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 21 NC not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 22 PW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 23 GO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 24 AW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 25 EM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 26 BK not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 27 SO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 28 PB not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 29 AJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 30 RM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 31 OJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 32 ER not host
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 33 PM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 34 PD (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 35 AZ (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 36 CI (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 37 OS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host
!.... Lodgepole Pine.
DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=5,5)/ &
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 1  AF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 2  CB (GF      )
      1.0,.98,.97,.85,.80,.52,.44, &  ! 3  DF (  DF    )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 4  GF (GF      )
      1.0,1.0,1.0,.98,.95,.70,.50, &  ! 5  WF (GF      )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 6  MH (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 7  RC not host
      1.0,.94,.92,.88,.84,.58,.54, &  ! 8  WL
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 9  BC (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 10 LM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 11 LP (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 12 PI (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 13 PP (      PP)
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 14 WB (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 15 SW (    LP  )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 16 UJ not host
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 17 BS (      PP)
      1.0,.98,.97,.85,.80,.52,.44, &  ! 18 ES (  DF    )
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 19 WS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 20 AS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 21 NC not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 22 PW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 23 GO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 24 AW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 25 EM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 26 BK not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 27 SO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 28 PB not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 29 AJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 30 RM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 31 OJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 32 ER not host
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 33 PM (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 34 PD (    LP  )
      1.0,1.0,1.0,1.0,.94,.80,.59, &  ! 35 AZ (    LP  )
      1.0,1.0,1.0,.98,.86,.73,.50, &  ! 36 CI (      PP)
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 37 OS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host
!.... Aspen.
!      DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=6,6)/
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  1
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  2
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  3
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  4
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  5
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  6
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  7
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  8
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  9
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 10
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 11
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 12
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 13
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 14
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 15
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 16
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 17
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 18
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 19
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 20
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 21
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 22
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 23
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 24
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 25
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 26
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 27
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 28
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 29
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 30
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 31
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 32
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 33
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 34
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 35
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 36
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 37
!     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38

!.... Height growth potential rates
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
!....
!.... Special note: Height growth potential values are not set
!.... based on Model type (subvariant) like diameter growth and
!.... mortaility.
!....
DATA ((AHGP(I,J),J=1,7),I=1,MAXSP) &
     /1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 1  AF
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 2  CB
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 3  DF
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 4  GF
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 5  WF
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 6  MH
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 7  RC not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 8  WL
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 9  BC
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 10 LM
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 11 LP
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 12 PI
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 13 PP
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 14 WB
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 15 SW
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 16 UJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 17 BS
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 18 ES
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 19 WS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 20 AS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 21 NC not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 22 PW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 23 GO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 24 AW not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 25 EM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 26 BK not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 27 SO not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 28 PB not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 29 AJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 30 RM not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 31 OJ not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 32 ER not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 33 PM
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 34 PD
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 35 AZ
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 36 CI
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &  ! 37 OS not host
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host

!.... Mortality coefficients.
!.... Southwest Mixed Conifer.
DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=1,1)/ &
      0.0,     0.00159, 0.00508, &  ! 1  AF (GF      )
      0.0,     0.00159, 0.00508, &  ! 2  CB (GF      )
      0.01319,-0.01627, 0.00822, &  ! 3  DF (  DF    )
      0.0,     0.00159, 0.00508, &  ! 4  GF (GF      )
      0.0,     0.00159, 0.00508, &  ! 5  WF (GF      )
      0.00681,-0.00580, 0.00935, &  ! 6  MH (      PP)
      0.0,0.0,0.0, &  ! 7  RC not host
      0.01319,-0.01627, 0.00822, &  ! 8  WL (  DF    )
      0.00681,-0.00580, 0.00935, &  ! 9  BC (      PP)
      0.00112, 0.02170,-0.00171, &  ! 10 LM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 11 LP (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 12 PI (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 13 PP (      PP)
      0.00112, 0.02170,-0.00171, &  ! 14 WB (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 15 SW (    LP  )
      0.0,0.0,0.0, &  ! 16 UJ not host
      0.00681,-0.00580, 0.00935, &  ! 17 BS (      PP)
      0.01319,-0.01627, 0.00822, &  ! 18 ES (  DF    )
      0.0,0.0,0.0, &  ! 19 WS not host
      0.0,0.0,0.0, &  ! 20 AS not host
      0.0,0.0,0.0, &  ! 21 NC not host
      0.0,0.0,0.0, &  ! 22 PW not host
      0.0,0.0,0.0, &  ! 23 GO not host
      0.0,0.0,0.0, &  ! 24 AW not host
      0.0,0.0,0.0, &  ! 25 EM not host
      0.0,0.0,0.0, &  ! 26 BK not host
      0.0,0.0,0.0, &  ! 27 SO not host
      0.0,0.0,0.0, &  ! 28 PB not host
      0.0,0.0,0.0, &  ! 29 AJ not host
      0.0,0.0,0.0, &  ! 30 RM not host
      0.0,0.0,0.0, &  ! 31 OJ not host
      0.0,0.0,0.0, &  ! 32 ER not host
      0.00112, 0.02170,-0.00171, &  ! 33 PM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 34 PD (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 35 AZ (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 36 CI (      PP)
      0.0,0.0,0.0, &  ! 37 OS not host
      0.0,0.0,0.0/               ! 38 OH not host

!.... Southwest PP and PJ.
DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=2,2)/ &
      0.0,     0.00159, 0.00508, &  ! 1  AF (GF      )
      0.0,     0.00159, 0.00508, &  ! 2  CB (GF      )
      0.01319,-0.01627, 0.00822, &  ! 3  DF (  DF    )
      0.0,     0.00159, 0.00508, &  ! 4  GF (GF      )
      0.0,     0.00159, 0.00508, &  ! 5  WF (GF      )
      0.00681,-0.00580, 0.00935, &  ! 6  MH (      PP)
      0.0,0.0,0.0, &  ! 7  RC not host
      0.01319,-0.01627, 0.00822, &  ! 8  WL (  DF    )
      0.00681,-0.00580, 0.00935, &  ! 9  BC (      PP)
      0.00112, 0.02170,-0.00171, &  ! 10 LM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 11 LP (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 12 PI (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 13 PP (      PP)
      0.00112, 0.02170,-0.00171, &  ! 14 WB (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 15 SW (    LP  )
      0.0,0.0,0.0, &  ! 16 UJ not host
      0.00681,-0.00580, 0.00935, &  ! 17 BS (      PP)
      0.01319,-0.01627, 0.00822, &  ! 18 ES (  DF    )
      0.0,0.0,0.0, &  ! 19 WS not host
      0.0,0.0,0.0, &  ! 20 AS not host
      0.0,0.0,0.0, &  ! 21 NC not host
      0.0,0.0,0.0, &  ! 22 PW not host
      0.0,0.0,0.0, &  ! 23 GO not host
      0.0,0.0,0.0, &  ! 24 AW not host
      0.0,0.0,0.0, &  ! 25 EM not host
      0.0,0.0,0.0, &  ! 26 BK not host
      0.0,0.0,0.0, &  ! 27 SO not host
      0.0,0.0,0.0, &  ! 28 PB not host
      0.0,0.0,0.0, &  ! 29 AJ not host
      0.0,0.0,0.0, &  ! 30 RM not host
      0.0,0.0,0.0, &  ! 31 OJ not host
      0.0,0.0,0.0, &  ! 32 ER not host
      0.00112, 0.02170,-0.00171, &  ! 33 PM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 34 PD (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 35 AZ (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 36 CI (      PP)
      0.0,0.0,0.0, &  ! 37 OS not host
      0.0,0.0,0.0/               ! 38 OH not host

!.... Black Hills.
DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=3,3)/ &
      0.0,     0.00159, 0.00508, &  ! 1  AF (GF      )
      0.0,     0.00159, 0.00508, &  ! 2  CB (GF      )
      0.01319,-0.01627, 0.00822, &  ! 3  DF (  DF    )
      0.0,     0.00159, 0.00508, &  ! 4  GF (GF      )
      0.0,     0.00159, 0.00508, &  ! 5  WF (GF      )
      0.00681,-0.00580, 0.00935, &  ! 6  MH (      PP)
      0.0,0.0,0.0, &  ! 7  RC not host
      0.01319,-0.01627, 0.00822, &  ! 8  WL (  DF    )
      0.00681,-0.00580, 0.00935, &  ! 9  BC (      PP)
      0.00112, 0.02170,-0.00171, &  ! 10 LM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 11 LP (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 12 PI (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 13 PP (      PP)
      0.00112, 0.02170,-0.00171, &  ! 14 WB (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 15 SW (    LP  )
      0.0,0.0,0.0, &  ! 16 UJ not host
      0.00681,-0.00580, 0.00935, &  ! 17 BS (      PP)
      0.01319,-0.01627, 0.00822, &  ! 18 ES (  DF    )
      0.0,0.0,0.0, &  ! 19 WS not host
      0.0,0.0,0.0, &  ! 20 AS not host
      0.0,0.0,0.0, &  ! 21 NC not host
      0.0,0.0,0.0, &  ! 22 PW not host
      0.0,0.0,0.0, &  ! 23 GO not host
      0.0,0.0,0.0, &  ! 24 AW not host
      0.0,0.0,0.0, &  ! 25 EM not host
      0.0,0.0,0.0, &  ! 26 BK not host
      0.0,0.0,0.0, &  ! 27 SO not host
      0.0,0.0,0.0, &  ! 28 PB not host
      0.0,0.0,0.0, &  ! 29 AJ not host
      0.0,0.0,0.0, &  ! 30 RM not host
      0.0,0.0,0.0, &  ! 31 OJ not host
      0.0,0.0,0.0, &  ! 32 ER not host
      0.00112, 0.02170,-0.00171, &  ! 33 PM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 34 PD (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 35 AZ (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 36 CI (      PP)
      0.0,0.0,0.0, &  ! 37 OS not host
      0.0,0.0,0.0/               ! 38 OH not host

!.... Spruce-Fir.
DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=4,4)/ &
      0.0,     0.00159, 0.00508, &  ! 1  AF (GF      )
      0.0,     0.00159, 0.00508, &  ! 2  CB (GF      )
      0.01319,-0.01627, 0.00822, &  ! 3  DF (  DF    )
      0.0,     0.00159, 0.00508, &  ! 4  GF (GF      )
      0.0,     0.00159, 0.00508, &  ! 5  WF (GF      )
      0.00681,-0.00580, 0.00935, &  ! 6  MH (      PP)
      0.0,0.0,0.0, &  ! 7  RC not host
      0.01319,-0.01627, 0.00822, &  ! 8  WL (  DF    )
      0.00681,-0.00580, 0.00935, &  ! 9  BC (      PP)
      0.00112, 0.02170,-0.00171, &  ! 10 LM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 11 LP (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 12 PI (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 13 PP (      PP)
      0.00112, 0.02170,-0.00171, &  ! 14 WB (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 15 SW (    LP  )
      0.0,0.0,0.0, &  ! 16 UJ not host
      0.00681,-0.00580, 0.00935, &  ! 17 BS (      PP)
      0.01319,-0.01627, 0.00822, &  ! 18 ES (  DF    )
      0.0,0.0,0.0, &  ! 19 WS not host
      0.0,0.0,0.0, &  ! 20 AS not host
      0.0,0.0,0.0, &  ! 21 NC not host
      0.0,0.0,0.0, &  ! 22 PW not host
      0.0,0.0,0.0, &  ! 23 GO not host
      0.0,0.0,0.0, &  ! 24 AW not host
      0.0,0.0,0.0, &  ! 25 EM not host
      0.0,0.0,0.0, &  ! 26 BK not host
      0.0,0.0,0.0, &  ! 27 SO not host
      0.0,0.0,0.0, &  ! 28 PB not host
      0.0,0.0,0.0, &  ! 29 AJ not host
      0.0,0.0,0.0, &  ! 30 RM not host
      0.0,0.0,0.0, &  ! 31 OJ not host
      0.0,0.0,0.0, &  ! 32 ER not host
      0.00112, 0.02170,-0.00171, &  ! 33 PM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 34 PD (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 35 AZ (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 36 CI (      PP)
      0.0,0.0,0.0, &  ! 37 OS not host
      0.0,0.0,0.0/               ! 38 OH not host

!.... Lodgepole Pine.
DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=5,5)/ &
      0.0,     0.00159, 0.00508, &  ! 1  AF (GF      )
      0.0,     0.00159, 0.00508, &  ! 2  CB (GF      )
      0.01319,-0.01627, 0.00822, &  ! 3  DF (  DF    )
      0.0,     0.00159, 0.00508, &  ! 4  GF (GF      )
      0.0,     0.00159, 0.00508, &  ! 5  WF (GF      )
      0.00681,-0.00580, 0.00935, &  ! 6  MH (      PP)
      0.0,0.0,0.0, &  ! 7  RC not host
      0.01319,-0.01627, 0.00822, &  ! 8  WL (  DF    )
      0.00681,-0.00580, 0.00935, &  ! 9  BC (      PP)
      0.00112, 0.02170,-0.00171, &  ! 10 LM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 11 LP (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 12 PI (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 13 PP (      PP)
      0.00112, 0.02170,-0.00171, &  ! 14 WB (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 15 SW (    LP  )
      0.0,0.0,0.0, &  ! 16 UJ not host
      0.00681,-0.00580, 0.00935, &  ! 17 BS (      PP)
      0.01319,-0.01627, 0.00822, &  ! 18 ES (  DF    )
      0.0,0.0,0.0, &  ! 19 WS not host
      0.0,0.0,0.0, &  ! 20 AS not host
      0.0,0.0,0.0, &  ! 21 NC not host
      0.0,0.0,0.0, &  ! 22 PW not host
      0.0,0.0,0.0, &  ! 23 GO not host
      0.0,0.0,0.0, &  ! 24 AW not host
      0.0,0.0,0.0, &  ! 25 EM not host
      0.0,0.0,0.0, &  ! 26 BK not host
      0.0,0.0,0.0, &  ! 27 SO not host
      0.0,0.0,0.0, &  ! 28 PB not host
      0.0,0.0,0.0, &  ! 29 AJ not host
      0.0,0.0,0.0, &  ! 30 RM not host
      0.0,0.0,0.0, &  ! 31 OJ not host
      0.0,0.0,0.0, &  ! 32 ER not host
      0.00112, 0.02170,-0.00171, &  ! 33 PM (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 34 PD (    LP  )
      0.00112, 0.02170,-0.00171, &  ! 35 AZ (    LP  )
      0.00681,-0.00580, 0.00935, &  ! 36 CI (      PP)
      0.0,0.0,0.0, &  ! 37 OS not host
      0.0,0.0,0.0/               ! 38 OH not host

!.... Aspen.
!      DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=6,6)/
!     &   0.0,0.0,0.0, !  1
!     &   0.0,0.0,0.0, !  2
!     &   0.0,0.0,0.0, !  3
!     &   0.0,0.0,0.0, !  4
!     &   0.0,0.0,0.0, !  5
!     &   0.0,0.0,0.0, !  6
!     &   0.0,0.0,0.0, !  7
!     &   0.0,0.0,0.0, !  8
!     &   0.0,0.0,0.0, !  9
!     &   0.0,0.0,0.0, ! 10
!     &   0.0,0.0,0.0, ! 11
!     &   0.0,0.0,0.0, ! 12
!     &   0.0,0.0,0.0, ! 13
!     &   0.0,0.0,0.0, ! 14
!     &   0.0,0.0,0.0, ! 15
!     &   0.0,0.0,0.0, ! 16
!     &   0.0,0.0,0.0, ! 17
!     &   0.0,0.0,0.0, ! 18
!     &   0.0,0.0,0.0, ! 19
!     &   0.0,0.0,0.0, ! 20
!     &   0.0,0.0,0.0, ! 21
!     &   0.0,0.0,0.0, ! 22
!     &   0.0,0.0,0.0, ! 23
!     &   0.0,0.0,0.0, ! 24
!     &   0.0,0.0,0.0, ! 25
!     &   0.0,0.0,0.0, ! 26
!     &   0.0,0.0,0.0, ! 27
!     &   0.0,0.0,0.0, ! 28
!     &   0.0,0.0,0.0, ! 29
!     &   0.0,0.0,0.0, ! 30
!     &   0.0,0.0,0.0, ! 31
!     &   0.0,0.0,0.0, ! 32
!     &   0.0,0.0,0.0, ! 33
!     &   0.0,0.0,0.0, ! 34
!     &   0.0,0.0,0.0, ! 35
!     &   0.0,0.0,0.0, ! 36
!     &   0.0,0.0,0.0, ! 37
!     &   0.0,0.0,0.0/ ! 38
!.... Check for debug.
CALL DBCHK(DEBUG,'MISINT',6,ICYC)

IF(DEBUG) WRITE(JOSTND,10)ICYC,IMODTY
10 FORMAT(' Begin MISINTCR: Cycle = ',I5,' IMODTY = ',I5)
!.... Mistletoe model initializations.
!.... 24-feb-98 Lance David
!.... Commented out so that mortality is used, now that mistletoe effects
!.... have been removed from the base FVS Central Rockies by Gary Dixon.
!.... USEMRT=.FALSE.
!
IF (IMODTY .LT. 1 .OR. IMODTY .GT. 5) THEN
   LTYPE=1
ELSE
   LTYPE=IMODTY
ENDIF
DO 200 I=1,MAXSP
   MISFIT(I)=AFIT(LTYPE,I)
!
!.... 17-DEC-98 Matt Oberle (MJO DEC98)
!.... Changed dimension of ACSP(LTYPE,I) to ACSP(I)
   CSPARR(I)=ACSP(I)
!
!.... Temporary code to set default growth potentials.
!.... Eventually, we would like correct coefficients in the
!.... data tables so we can use the following statement:
!....
!.... 24-feb-98 Lance David
!.... The values in ADGP array are used now that mistletoe effects
!.... have been removed from the base FVS Central Rockies by Gary Dixon.
!.... Prior to this time, DGPDMR(I,J)=1.0 was used.

   DO 100 J=1,7
      DGPDMR(I,J)=ADGP(LTYPE,I,J)
      HGPDMR(I,J)=AHGP(I,J)
100    CONTINUE
   DO 150 J=1,3
      PMCSP(I,J)=APMC(LTYPE,I,J)
150    CONTINUE
200 CONTINUE

!.... Common return.
IF(DEBUG) WRITE(JOSTND,9010)ICYC,LTYPE
9010 FORMAT(' End MISINTCR: Cycle = ',I5,' LTYPE = ',I5)
RETURN
END
