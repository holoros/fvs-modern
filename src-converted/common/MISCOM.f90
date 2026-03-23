!ODE SEGMENT MISCOM
!
!----------
! COMMON $Id$
!----------
!  Purpose:
!     Common block variables related to the dwarf mistletoe routines.
!----------
!
!  Revision History :
!     03/01/95 - Lance R. David (MAG)
!                Declaration of real variable array DMGMLT (size MAXSP)
!                removed from common area MISTL due to change of
!                MISTGMOD keyword.
!
!     04/10/97 - Julie C. Wil-Cip (FHTET)
!                Added array DMTALL to common area MISTL due
!                to modification of MISTOE subroutine to speed up the
!                spread of DMR from large to small trees on a plot.
!
!     06/15/97 - Lance R. David (FHTET)
!                Added arrays SPPDMI and SPPTPA to common area MISTL as
!                continued the modification Julie began to speed spread
!                and intensification on small trees when infected overstory
!                exists on the plot (aka point).
!
!     03/07/08 - Lance R. David (FHTET)
!                Added variables that were local to misprt subroutine for
!                output control that need to be maintained for each stand in
!                PPE processing mode.
!                Variables Added are: DMFLAG, PRTTBL, LSORT4, MISTBL and ISVSP4
!
!     04/21/09 - Lance R. David (FMSC)
!                Added variables IMOUT_ (thanks to Don Robinson)
!
!     09/30/10 - Lance R. David (FMSC)
!                Added HGPDMR array for DM height growth impact proportions.
!
!     04/01/11 - Lance R. David (FMSC)
!                Added dimension to MISCYC array (1,x) used by MISDGF function
!                and (2,x) used by MISHGF (diameter and height growth impact).
!----------
!  VARIABLE DECLARATIONS:
!----------
!
LOGICAL DMFLAG,FSTMIS,FSTTBL,LSORT4,MISFLG,PRTMIS,PRTTBL,USEMRT
!
LOGICAL MISCYC(2,MAXCYC),MISTBL(MAXSP)
!
CHARACTER*2  CSPARR(MAXSP)
!
INTEGER IMOUT1,IMOUT2,IMOUT3,IMOUT4,JRAN
!
INTEGER DMPLT(MAXPLT),IDMSOUT(4),IMIST(MAXTRE),ISVSP4(4), &
           MISFIT(MAXSP)
!
REAL DMRMIN
!
REAL DGPDMR(MAXSP,7),DMMMLT(MAXSP),DMMTPA(MAXTRE), &
        DMTALL(MAXSP,MAXPLT),HGPDMR(MAXSP,7),PMCSP(MAXSP,3), &
        PRFMST(MAXSP,6),SPPDMI(MAXSP,MAXPLT),SPPTPA(MAXSP,MAXPLT), &
        YNGMLT(MAXSP),YPLMLT(MAXSP)
!
COMMON /MISTL/ DGPDMR,DMFLAG,DMMMLT,DMMTPA,DMPLT,DMRMIN,DMTALL, &
                  FSTMIS,FSTTBL,HGPDMR,IMIST,ISVSP4,JRAN,LSORT4, &
                  MISCYC,MISFIT,MISFLG,MISTBL,PMCSP,PRFMST,PRTMIS, &
                  PRTTBL,SPPDMI,SPPTPA,USEMRT,YNGMLT,YPLMLT
!
COMMON /MISTL2/ CSPARR
!
COMMON /MISTL3/ IDMSOUT,IMOUT1,IMOUT2,IMOUT3,IMOUT4
!
!----------
!  VARIABLE DEFINITIONS:
!----------
!     CSPARR: Array to hold 2 character representation of top 4
!                infected species; set in MISINT.
!     DGPDMR: Array containing diameter growth potential proportions
!                due to DM infection based on DMR; set in MISINT; set
!                from MISTGMOD keyword in MISDGF; saved for subsequent
!                cycles.
!     DMFLAG: Logical flag, true if mistletoe is present in the stand.
!     DMMMLT: Array containing DM mortality multipliers; set from
!                keyword in MISMRT, saved for subsequent cycles.
!     DMMTPA: Array containing TPA killed by mistletoe; set in MISMRT
!                for most variants, set in MISDIF for SW Mixed Conifer
!                and SW Ponderosa Pine variants.
!      DMPLT:
!     DMRMIN: Minimum tree DBH for DMR/DMI statistics (this size or
!                larger to be counted); set from keyword in MISIN.
!     DMTALL: Array used to keep track of the tallest DMR-infected tree
!                by species, by plot; contains that tree's height, set
!                in MISTOE each cycle.
!     FSTMIS: Logical flag for printing mistletoe summary output table
!                headings; set first time through subroutine MISPRT.
!     FSTTBL: Logical flag for printing mistletoe detail table headings;
!                set first time printed in subroutine MISPRT.
!     HGPDMR: Array containing height growth potential proportions
!                due to DM infection based on DMR; set in MISINT; set
!                from MISTHMOD keyword in MISHGF. Set values remain in
!                effect until changed by a later scheduled MISTHMOD.
!   IDMSOUT:
!     IMIST:  Array containing individual tree record dwarf mistletoe
!                (Hawksworth) ratings.
!     IMOUT1: Mistletoe output unit 1 (Top 4 species summary).
!     IMOUT2: Mistletoe output unit 2 (stand summary).
!     IMOUT3: Mistletoe output unit 3 (2" DBH class summary).
!     IMOUT4: Mistletoe output unit 4 (species/DBH class detail).
!     ISVSP4: Array of top 4 most DM infected species by number.
!     JRAN:   Status value of mistletoe random number generator.
!     LSORT4: Logical flag to set species by infection.
!     MISCYC: Logical array used for reading MISTGMOD keyword only once
!                per cycle (since MISDGF is called once per tree).
!     MISFIT: Array used to keep track of which species in the current
!                variant are mistletoe susceptible; variant specific;
!                (1=affected, 0=not affected by DM); set in MISINT.
!     MISFLG: Logical flag to turn mistletoe processing on/off; set from
!                keyword in MISIN, valid throughout a run.
!     MISTBL: Logical array - what species desired in detailed output.
!     PMCSP:  Array containing percent mortality equation coefficients
!                by species; set in MISINT.
!     PRFMST: Array containing species mistletoe removal preference
!                priorities; set from keyword in MISCPF.
!     PRTMIS: Logical flag to turn DM output table printing on/off; set
!                from keyword in MISIN.
!     PRTTBL: Logical flag to print DM detailed output tables.
!     SPPDMI: Array used to calculate DMI (ave DMR of infected trees only)
!                by species, by plot; set in MISTOE each cycle.
!     SPPTPA: Array used to accumulate trees per acre of infected trees
!                used in the calculation of point (plot) DMI.
!                by species, by plot; set in MISTOE each cycle.
!     USEMRT: Logical flag, true if using this model's mortality
!                calculations; set in MISIN0 or MISINT.
!     YNGMLT: Array containing DM spread probability(-) multipliers; set
!                from keyword in MISTOE, saved for subsequent cycles.
!     YPLMLT: Array containing DM spread probability(+) multipliers; set
!                from keyword in MISTOE, saved for subsequent cycles.
!
!-----END SEGMENT
