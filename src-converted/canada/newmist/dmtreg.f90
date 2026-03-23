SUBROUTINE DMTREG
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMTREG -- NISI  Date of last revision: 12/20/03
!--------------------------------------------------------------------
! Purpose:
!   This routine is the main processing point for the NISI (New &
! Improved Spread & Intensification) Model. It carries out the
! following operations:
!--------------------------------------------------------------------
!
! Called by:
!
!     MISTOE
!
! Other routines called:
!
!     DMOPTS
!     DMMTRX
!     DMFBRK
!     DMFSHD
!     DMNTRD
!     DMFINF
!     DMNB
!     DMFDNS
!     DMSRC
!     DMTLST
!     DMAUTO
!     DMSAMP
!     DMSLST
!     DMSLOP
!     DMADLV
!     DMOTHR
!     DMCYCL
!     DMNDMR
!     MISDGF
!
! Argument list definitions:
!
!     [none]
!
! Local variable definitions:
!
!     INTEGER i         Loop counter for treelist records and for
!                        species codes.
!     INTEGER j         Loop counter for crown thirds and for species
!                        codes.
!     INTEGER k         Loop counter for DM categories.
!     INTEGER m         Loop counter for sampling rings.
!     INTEGER n         Loop counter for *source* DM categories.
!     INTEGER r         Loop councter for crown thirds and heights.
!     INTEGER s         Loop counter for MESH bands within crown
!                        thirds.
!     INTEGER u         Loop counter for treelist records, and the
!                        index to treelist recrords found in the
!                        list of *targets*.
!     INTEGER v         Loop counter for crown thirds, and the index
!                        to treelist records found in the list of
!                        *sources*.
!     INTEGER jj        Loop counter for treelist records that may
!                        have been left out of the S&I process.
!     INTEGER uu        Loop counter for the list of *targets*, and
!                        a loop counter for treelist records.
!     INTEGER vv        Loop counter for the list of *sources*, and
!                        a loop counter for treelist records.
!     INTEGER Scnt      The number of occurrences of a particular
!                        treelist record in a sample ring.
!     INTEGER Offset    MESH offset due to slope effect.
!     INTEGER LHt       Lower mesh band containing a breakpoint.
!     INTEGER UHt       Upper mesh band containing a breakpoint.
!     INTEGER DMSPtr    Matrix of pointers to treelist, sorted by
!                        species and DM class:
!                        Index 1: species code.
!                        Index 2: DM rating.
!                        Index 3: FST is first position in
!                                 'DMSInd()' array; LST is last
!                                 position in array. This mapping is
!                                 analagous to the 'IND1()' array of
!                                 of the base model, but with two
!                                 levels of indirection.
!     INTEGER DMSInd    Array containing pointers to the treelist.
!                        It is sorted by species and DM rating and
!                        is an analogue of the 'IND1()' array of
!                        the base model.
!     INTEGER Sample    The number of trees of a given DM category
!                        that will be placed in a sampling ring.
!     INTEGER SrcInd    Pointers to the treelist index occupying
!                        corresponding to the cumulative
!                        probability within the 'SrcCD()' array.
!                        These records are sorted by DM category
!                        into groups marked by the 'SrcPtr()' array.
!     INTEGER SrcPtr    Breakpoints demarcating the DM categories
!                        ordered within the 'SrcI() and 'SrcCD()'
!                        arrays. Each value marks the *last* entry
!                        in that category: e.g.: 'Sptr(3)' contains
!                        the position of the last position with DM
!                        rating 3; 'Sptr(2)+1' contains the first.
!     INTEGER SrcLst    List of trees selected to occupy the sampling
!                        sampling ring being processed.
!                        Index 1: The index value to the treelist
!                                 record.
!                        Index 2: The number of occurrences of the
!                                 record.
!     INTEGER TrgLst    The trees to be used as targets; the zero'th
!                        entry contains the number of trees in the
!                        list.
!     REAL    TotalD    The total density (trees/acre) of all trees
!                        of all species in the stand.
!     REAL    dgfx      Proportion of realized diameter growth. This
!                        is used to modify "seed" output.
!     REAL    MISDGF    Dummy parameter for MISDGF function call.
!     REAL    SrcCD     The cumulative probability of each DM group
!                        is computed by taking the relative
!                        density (trees/acre) of each treelist
!                        record and forming a cumulative
!                        distribution.
!     REAL    Dnsty     Trees/acres of each DM class; computed within
!                        a species loop and not accounting for the
!                        presence of any autocorrelation.
!     REAL    SDens     Like 'Dnsty', but after computing
!                        autocorrelation effects that may include
!                        differential density for different sampling
!                        rings.
!     REAL    NBC       Cumulative distribution for the sampling
!                        properties of each sampling ring without
!                        regard to species or DM category.
!     REAL    HtWt      The weight given to the infection within a
!                        slice of canopy. This is required only if
!                        slice does not occupy a full MESH band.
!     REAL    Level     DMR in crown third
!     REAL    x         Intermediate calculation.
!     REAL    Rad       The Radius (MESH) of a slice of canopy.
!     REAL    SprFld    Infection density field; produced in the
!                        *target* record.
!     REAL    IntFld    Infection density field; produced in the
!                        *source* record.
!
! Common block variables and parameters:
!
!     MAXTRE  PRGPRM
!     MAXSP   PRGPRM
!     LST     DMCOM
!     DSTLEN  DMCOM
!     INDX    DMCOM
!     MXTHRX  DMCOM
!     MXHT    DMCOM
!     DCDn    DMCOM
!     ITRN    CONTRL
!     DMRATE  DMCOM
!     CRTHRD  DMCOM
!     DMINF   DMCOM
!     ACTIVE  DMCOM
!     DMDMR   DMCOM
!     DMOPAQ  DMCOM
!     DMOPQM  DMCOM
!     DMOPQ2  DMCOM
!     MESH    DMCOM
!     DMSURV  DMCOM
!     DMDETH  DMCOM
!     NTDn    DMCOM
!     PROB    ARRAYS
!     ISCT    CONTRL
!     DMKTUN  DMCOM
!     KNT     DMCOM
!     BPCNT   DMCOM
!     BrkPnt  DMCOM
!     DMRDMX  DMCOM
!     VOLUME  DMCOM
!     RADIUS  DMCOM
!     TWOPIE  DMCOM
!     IND1    ARRAYS
!     PBrkPt  DMCOM
!
!********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'MISCOM.f90'
INCLUDE 'DMCOM.f90'

! Local variables.

INTEGER i,j,k,m,n,r,s,u,v,jj,uu,vv
INTEGER Scnt
INTEGER Offset
INTEGER LHt,UHt
INTEGER DMSPtr(MAXSP,0:6,LST)
INTEGER DMSInd(MAXTRE)
INTEGER Sample
INTEGER SrcInd(MAXTRE)
INTEGER SrcPtr(0:6)
INTEGER SrcLst(0:DSTLEN,INDX)
INTEGER TrgLst(0:MAXTRE)
REAL    TotalD
REAL    dgfx
REAL    MISDGF
REAL    SrcCD(MAXTRE)
REAL    Dnsty(0:6)
REAL    SDens(0:6)
REAL    NBC(0:DSTLEN,MXTHRX)
REAL    HtWt,Level,x,Rad
REAL    SprFld(MXHT)
REAL    IntFld(MXHT)

REAL    DMHtWt

! On the first pass through, initialize the DMINF() with values from
! DMRATE(). For various reasons it is clumsy to attempt this anywhere
! else. Initial values for DMINF() are based on the user's (or
! default) values of DMDMR() and DMRATE().

IF (.NOT. DCDn) THEN

  DO i = 1,ITRN
    DO j = 1,CRTHRD
      DMINF(i,j,ACTIVE) = FLOAT(DMDMR(DMRATE(i),j))
    ENDDO
  ENDDO

! Create values for DMOPQ2() based on the user's (or default) value
! for DMOPAQ(). This gives a MESH-based value that parallels the
! usage of the cubic meter-based DMOPAQ().

  DO i = 1,MAXSP
    DMOPAQ(i) = DMOPAQ(i) * DMOPQM
    DMOPQ2(i) = 1.0 - ((1.0 - DMOPAQ(i)) ** MESH)
    DMSURV(i) = 1.0 - DMDETH(i)
  ENDDO

  DCDn = .TRUE.

ENDIF

! See if there are any option processor activities in this cycle.

CALL DMOPTS

! Determine the radius and volume (MESH units) of each tree in the
! treelist at each height. 'DMRDMX()' is filled as a result.

CALL DMMTRX

! Find the 4 breakpoints for each tree (1= top; 4= bottom of crown).
! The results are place in 'BrkPnt()'.

CALL DMFBRK

! Compute the likelihood of shading at any height in the stand. This
! could be modified to reflect the sampling distribution; in which
! case it might be called numerous times to create grids of
! concentric disks.

CALL DMFSHD

! Call DMNTRD on the cycle following the first entry, to map
! infection values to the new locations of each crown third.

IF (NTDn) THEN
    CALL DMNTRD
  ELSE
    NTDn = .TRUE.
ENDIF

! Zero arrays to hold new spread and intensification.

DO u = 1,ITRN
  DO v = 1,CRTHRD
    NewSpr(u,v) = 0.0
    NewInt(u,v) = 0.0
  ENDDO
ENDDO

! Scan the treelist and generate a matrix of pointers for the first
! and last records of each DMR class of each species. The indexes
! themselves are stored in 'DMSInd()', the pointer matrix is
! 'DMSPtr()'.

CALL DMFINF(DMSPtr,DMSInd)

! Create the probability distribution for all stand trees around
! potential targets in sampling ring 'm'. The answer is returned in
! 'NBC()', a matrix representing the cumulative distribution for
! trees within ring 'm'. Usually the length of this vector (DSTLEN)
! will be long enough (for densities up to about 10,000/acre). If
! densities are higher than this, the compile-time length will have
! to be enlarged or the method changed.

TotalD = 0.0
DO i = 1,ITRN
  TotalD = TotalD + PROB(i)
ENDDO

DO m = 1,MXTHRX
  CALL DMNB(m,TotalD,NBC(0,m))
ENDDO

! Loop over tree species.

DO 100 j = 1,MAXSP

  IF (MISFIT(j) .EQ. 0 .OR. ISCT(j,1) .EQ. 0) GOTO 100

! Find the trees/acre density of each Target DMR class 'k' of species
! 'j'. The answer is returned in 'Dnsty()'. A vector is computed
! rather than one-at-a-time, because the SUM of DMR is needed to
! carry out DMAUTO.

  CALL DMFDNS(j,DMSPtr,DMSInd,PROB,Dnsty)

! Set up vector of Source trees and weights, so that random Source
! trees can be selected. The answer is returned as the paired vectors
! 'SrcInd()' and 'SrcCD()', which hold the treelist record and
! cumulative proportional probability associated with the record
! number, respectively. The offsets within these vectors are held by
! 'SrcPtr()'. This is all used by DMSLST.

  CALL DMSRC(j,Dnsty,DMSPtr,DMSInd,PROB,SrcInd,SrcCD,SrcPtr)

! Loop over the Target DMR classes.

  DO 200 k = 0,6

    IF (Dnsty(k) .LE. 0.0) GOTO 200

! A few loops from now we will walk through a list of Targets of
! species 'j' having DMR 'k'. If no Targets exist, fall through to
! the bottom of the Target loop. Otherwise, choose the players.

    CALL DMTLST(j,k,DMSPtr,DMSInd,PROB,TrgLst)

    IF (TrgLst(0) .EQ. 0) GOTO 200

    DO m = 1,MXTHRX  ! Loop over sampling rings

! Find the density for each Source DMR class in each ring,
! conditioned on the Target DMR. 'SDens()' is returned with the
! density of each Source DMR class computed using the alpha and beta
! autocorrelation terms.

      CALL DMAUTO(k,m,Dnsty,SDens)

      DO 400 n = DMKTUN(j),6 ! Loop over Source DMR classes

        IF (DMSPtr(j,n,FST) .EQ. 0) GOTO 400

        DO 500 uu = 1,TrgLst(0)

          u = TrgLst(uu)

! Find out how many Source trees of DMR 'n' to place in sampling
! ring 'm' surrounding the 'TrgLst()' record 'u' having DMR 'k'.
! The answer is returned in 'Sample'. If the sample size is zero,
! fall through to the bottom of the loop.

          CALL DMSAMP(TotalD,SDens(n),NBC(0,m),1.0,Sample)
          IF (Sample .EQ. 0) GOTO 500

! Select 'Sample' random infections with 'n' Source DMR. Treelist
! record indices are returned in 'SrcLst()'; 'SRcLst(0,INDX)' holds
! the number of unique indices.

          CALL DMSLST(n,Sample,SrcInd,SrcCD,SrcPtr,SrcLst)

          DO vv = 1,SrcLst(0,INDX) ! Loop over the Source trees.

! Zero the spread fields about to be experienced by
! Source 'v' and Target 'u'.

            DO r = 1,MXHT
              SprFld(r) = 0.0
            ENDDO

! Get the index to the source tree and the number of occurrences.

            v    = SrcLst(vv,INDX)
            Scnt = SrcLst(vv,KNT)

! Get factor modifying periodic diameter growth. This affects
! 'Level', the effective seed production of the infection.

            dgfx = MISDGF(v,j)

! See if the site slope will shift the source-target relationship.
! The answer returned in 'Offset' is the difference (+ is above; - is
! below) in MESH height of the Source Tree relative to the Target. If
! Offset is > 0 and Scnt > 1 then this will compell all target trees
! to have only 1 Offset, and variation will be underestimated in some
! situations. It is also relevenat ONLY if the density of the
! particular source tree is extremely high; such that it will appear
! more than once in a sample ring.

            CALL DMSLOP(m,Offset)

! Loop over crown thirds of Source trees. The 'r' index is to LOWER
! breakpoint in each third.
!
! Find the upper and lower MESH categories into which each of the
! crown thirds is mapped. For each category define a weight
! defining how to apportion the upper and lower infection level.
! Then walk through the crown third in MESH meter bands from
! 'LHt' to 'UHt'.
!
! Note that IntFld is still present in the DMADLV arg list but not
! assigned until the very end of the subroutine

            DO r = 2,BPCNT
              Level = DMINF(v,r-1,ACTIVE)
              IF (Level .GT. 0.0) THEN
                LHt = INT(BrkPnt(v,r)) + 1
                UHt = MIN(MXHT,INT(BrkPnt(v,r-1)) + 1)
                DO s = LHt,UHt
                  HtWt = DMHtWt(v,r,s,Lht,Uht)
                  x = Level * HtWt * DMRDMX(v,s,VOLUME) * dgfx
                  CALL DMADLV(v,Scnt,SprFld,IntFld, &
                       (s+Offset),m,x,Shade,Shade,0,0)
                ENDDO
              ENDIF
            ENDDO

! Spread assumes a simple centre-of-source to centre-of-target
! geometry. In reality the source is an infected disk, so the
! subtended angle varies with position within the disk. The effect
! is more pronounced when Source-Target distance is small. The bias
! from this assumption is usually very small.  Final interception
! depends on apparent size of target from source, and is further
! modified by the radius of the disk (actually should be something
! like the mean extinction along the integrated path length through
! the disk). Thicker trees will catch more seed this way. Note that
! interception is the Target 'u'.

            DO r = 2,BPCNT
              LHt = INT(BrkPnt(u,r)) + 1
              UHt = MIN(MXHT,INT(BrkPnt(u,r-1)) + 1)
              DO s = LHt,UHt
                HtWt = DMHtWt(v,r,s,Lht,Uht)
                Rad = DMRDMX(u,s,RADIUS)
                x = ATAN(Rad / (FLOAT(m) - 0.5)) * TWOPIE
                NewSpr(u,r-1) = NewSpr(u,r-1) + &
                     (HtWt * x * SprFld(s) * Rad)
              ENDDO
            ENDDO

          ENDDO   ! end looping over Source trees
500         CONTINUE ! end looping over Target trees
400       CONTINUE  ! end looping over Source DMR classes
    ENDDO      ! end looping over ring samples
200   CONTINUE    ! end looping over Target DMR classes

!     Intensification needs to be added separately, so that a common
!     distance (MXTHRX) is used and all trajectories are included

  DO jj = ISCT(j,1),ISCT(j,2)

    v = IND1(jj)

    DO r = 1,MXHT
      IntFld(r) = 0.0
    ENDDO

    dgfx = MISDGF(v,j)

! Note that SprFld is still in the Arg list, but not used because
! it has already been computed in the main loop

    DO r = 2,BPCNT
      Level = DMINF(v,r-1,ACTIVE)
      IF (Level .GT. 0.0) THEN
        LHt = INT(BrkPnt(v,r)) + 1
        UHt = MIN(MXHT,INT(BrkPnt(v,r-1)) + 1)
        DO s = LHt,UHt
          HtWt = DMHtWt(v,r,s,Lht,Uht)
          x = Level * HtWt * DMRDMX(v,s,VOLUME) * dgfx
          CALL DMADLV(v,1,SprFld,IntFld,s,MXTHRX,x, &
                  Shade,Shade,0,0)
        ENDDO
      ENDIF
    ENDDO

! 'IntFld()' is implicitly a volume-based amount. It is scaled by
! 'HtWt' to account for the variable height across breakpoints.

    DO r = 2,BPCNT
      LHt = INT(BrkPnt(v,r))   + 1
      UHt = MIN(MXHT,INT(BrkPnt(v,r-1)) + 1)
      DO s = LHt,UHt
        IF (IntFld(s) .GT. 0.0) THEN
          HtWt = DMHtWt(v,r,s,Lht,Uht)
          NewInt(v,r-1) = NewInt(v,r-1) + &
               (IntFld(s) * HtWt * DMRDMX(v,s,VOLUME))
        ENDIF
      ENDDO
    ENDDO
  ENDDO

! End of un-intensified patch.

  CALL DMOTHR(j)

! End of looping over species.

100 CONTINUE

! Iterate each year within model timestep, then compute new DMR index
! after growth cycle.

CALL DMCYCL
CALL DMNDMR

! Cycle complete. Map breakpoints to PBrkPt() before growth occurs.

DO u = 1,ITRN
  DO v = 1,BPCNT
    PBrkPt(u,v) = BrkPnt(u,v)
  ENDDO
ENDDO

RETURN
END

!------------------------------------------------------------------

!     PRIVATE FUNCTION TO COMPUTE WEIGHT OF SLICE WITHIN CROWNTHIRD
!
!     THE UNUSUAL IF/ELSE IF/ LOGIC IS THE RESULT OF THE FACT THAT
!     SPECIAL WEIGHTING IS ONLY NEEDED FOR BREAKPOINTS 2 AND 3 (THE INNER
!     BREAKPOINTS). THE CALCULATION OF THE VOLUME AND RADIUS OF THE TOP
!     AND BOTTOM PIECES ALREADY ACCOUNTS FOR THE FACT THAT A TRUNCATED
!     SECTION MAY BE INVOLVED.

REAL FUNCTION DMHtWt(v,r,s,UHt,LHt)

INCLUDE 'PRGPRM.f90'
INCLUDE 'DMCOM.f90'

INTEGER v,r,s
INTEGER UHt,LHt
REAL    HtWt

IF ((LHt .EQ. s) .AND. (r .LT. BPCNT)) THEN
  HtWt = 1.0 - (BrkPnt(v,r) - INT(BrkPnt(v,r)))
ELSE IF ((UHt .EQ. s) .AND. (r .GT. 2)) THEN
  HtWt = BrkPnt(v,r-1) - INT(BrkPnt(v,r-1))
ELSE
  HtWt = 1.0
END IF

DMHtWt = HtWt

RETURN
END
