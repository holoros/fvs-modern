SUBROUTINE BWEPPPT (WK3, IPNT, ILIMIT, IB)
IMPLICIT NONE
!----------
! WSBWE $Id$
!----------
!  Purpose:
!     Put (store) the GenDefol/Budworm model data for a given stand
!     into buffer.
!     This is part of the Parallel Processing Extension.
!----------------------------------------------------------------------
!
!  Call list definitions:
!     ILIMIT: (I)  Size of buffer WK3.
!     IPNT:   (IO) Pointer to curent element in print buffer WK3.
!     WK3:    (IO) Work array used as a buffer.
!     IB:     (I)  Identifies variable set: 1=numeric, 2=character
!
!  Local variable definitions:
!     IIA:    Loop counter.
!     IIB:    Loop counter.
!     IIC:    Loop counter.
!     IID:    Loop counter.
!     IIX:    Array index.
!     J:      Array index.
!     K:      Series indicator for reading of DA file.
!     INTS:   Array of length MXI to hold integer values.
!     LOGICS: Array of length MXL to hold logical values.
!     REALS:  Array of length XMR to hold real values.
!
!  Common block variables and parameters:
!     See comments in common block files.
!
!  Revision History :
!   11-SEP-2006 - Lance R. David (FHTET)
!     This subroutine was written.
!   14-JUL-2010 Lance R. David (FMSC)
!     Added IMPLICIT NONE and declared variables as needed.
!   23-JUN-2011 Lance R. David (FMSC)
!     Added BWPRMS array for RAWS daily weather processing to BLCOM3.
!   20-SEP-2013 Lance R. David (FMSC)
!     Added RAWS weather year range (IYRNG array), expanded IEVENT array.
!   15-OCT-2014 Lance R. David (FMSC)
!     Complete rebuild of routine to include all integer and real variables
!     and arrays in common blocks. There are a few character array noted at
!     the end of this routine that are static and not stored or retrieved.
!----------


!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'METRIC.f90'
INCLUDE 'PPEPRM.f90'

!.... Common include files.

INCLUDE 'PPCNTL.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'BWEBOX.f90'
INCLUDE 'BWECM2.f90'
INCLUDE 'BWECOM.f90'
INCLUDE 'BWESTD.f90'

!
!     WRITE ALL INTEGER VARIABLES WITH IFWRIT, LOGICAL VARIABLES
!     WITH LFWRIT, AND REAL VARIABLES WITH BFWRIT.
!     LONG INTEGERS ARE EQIVALENCED TO REAL VARIABLES AND WRITTEN
!     WITH BFWRIT.
!     DOUBLE PRECISION VARIABLES (RANDOM NUMBER SEEDS) ARE EQUIVALENCED
!     TO REAL ARRAYS OF LENGTH 2 AND WRITTEN WITH BFWRIT.
!
!.... Variable declarations.
!.... Parameter statements.
INTEGER LNCBUF, MXI, MXL, MXR

PARAMETER (MXL=21,MXI=86,MXR=918)
PARAMETER (LNCBUF=IRECLN*4)

CHARACTER CBUFF(LNCBUF)

LOGICAL LOGICS(MXL)
INTEGER I, IID, INTS(MXI), IB, IIA, IIB, IIC, IIX, J, K, &
           ILIMIT, IPNT
REAL    REALS(MXR), WK3(*)
REAL    SEEDA(2), SEEDB(2), SEEDC(2)
EQUIVALENCE (SEEDA,DSEEDD), (SEEDB,OBSEED), (SEEDC,WSEED)

IF(IB .EQ. 2) GOTO 150

K = 2
DO J=1,MXL
   LOGICS(J) = .FALSE.
END DO
DO J=1,MXI
   INTS(J) = 0
END DO
DO J=1,MXR
   REALS(J) = 0.0
END DO

IF (PDEBUG) &
      WRITE (JOPPRT,'(/'' IN BWEPPPT: ISTND,ICYC='',I7,I4)') &
            ISTND,ICYC

!.... Load logical scalars into LOGICS array.
!
!---- from common BWEBOX ------------
LOGICS(1)  = LTEMP1(1)
LOGICS(2)  = LTEMP1(2)
LOGICS(3)  = LTEMP1(3)
LOGICS(4)  = LTEMP1(4)
LOGICS(5)  = LP1
LOGICS(6)  = LP2
LOGICS(7)  = LP3
LOGICS(8)  = LP4
LOGICS(9)  = LP5
LOGICS(10) = LP6
LOGICS(11) = LP7
LOGICS(12) = LP8
!---- from common BWECM2 ------------
LOGICS(13) = LBUDL
LOGICS(14) = LFIRST
LOGICS(15) = LREGO
LOGICS(16) = LSPRAY
!---- from common BWECOM ------------
LOGICS(17) = LBWDAM
LOGICS(18) = LBWPDM
LOGICS(19) = LCALBW
LOGICS(20) = LDEFOL
LOGICS(21) = LTOPK

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: LOGICS=', &
               LOGICS

!     Write logical scalars to buffer.
!
CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, K)

!.... Load integer scalars into INTS array.
!
!---- from common BWEBOX ------------
INTS(1)  = IBUDYR
INTS(2)  = IBWOPT
INTS(3)  = IDEFPR
INTS(4)  = IDEFSP(1)
INTS(5)  = IDEFSP(2)
INTS(6)  = IDEFSP(3)
INTS(7)  = IDEFSP(4)
INTS(8)  = IDEFSP(5)
INTS(9)  = IFLAG
INTS(10) = IOBACT
INTS(11) = IOBSCH(1,1)
INTS(12) = IOBSCH(1,2)
INTS(13) = IOBSCH(2,1)
INTS(14) = IOBSCH(2,2)
INTS(15) = IOBSCH(3,1)
INTS(16) = IOBSCH(3,2)
INTS(17) = IPARA
INTS(18) = IQUALD
INTS(19) = IQUALW
INTS(20) = ISTN
INTS(21) = ISTNUM
INTS(22) = ITEMP(1)
INTS(23) = ITEMP(2)
INTS(24) = ITEMP(3)
INTS(25) = ITEMP(4)
INTS(26) = ITEMP(5)
INTS(27) = IWSRC
INTS(28) = JOWE
INTS(29) = NEVENT
INTS(30) = NOBDON
INTS(31) = NOBSCH
INTS(32) = NUMCOL
!---- from common BWECM2 ------------
INTS(33) = ILOBYR
INTS(34) = INSTSP
INTS(35) = IOBDUR
INTS(36) = IOBLOC
INTS(37) = IOBOPT
INTS(38) = ISPRAY
INTS(39) = ISPVAR
INTS(40) = ISPYR(1)
INTS(41) = ISPYR(2)
INTS(42) = ISPYR(3)
INTS(43) = ISPYR(4)
INTS(44) = ISPYR(5)
INTS(45) = IWOPT
INTS(46) = IWYR
INTS(47) = IYRCNT
INTS(48) = IYRECV
INTS(49) = IYREND
INTS(50) = IYRNG(1)
INTS(51) = IYRNG(2)
INTS(52) = IYROBL
INTS(53) = IYRSRC
INTS(54) = IYRST
INTS(55) = JOBWP1
INTS(56) = JOBWP2
INTS(57) = JOBWP3
INTS(58) = JOBWP4
INTS(59) = JOBWP5
INTS(60) = JOBWP6
INTS(61) = JOBWP7
INTS(62) = JOBWP8
INTS(63) = KRECVR
INTS(64) = LIMITS
INTS(65) = LOWYRS
INTS(66) = NSPRAY
INTS(67) = NUMAPP
!---- from common BWECOM ------------
INTS(68) = IBWYR1
INTS(69) = IBWYR2
INTS(70) = ICUMYR
INTS(71) = IPRBYR
INTS(72) = ITODO
INTS(73) = IYRCUR
INTS(74) = JOWSBW
INTS(75) = NCROWN
INTS(76) = NCUMYR
INTS(77) = NHOSTS
INTS(78) = NTODO
INTS(79) = TABSZ
!---- from common BWESTD ------------
INTS(80) = IFHOST(1)
INTS(81) = IFHOST(2)
INTS(82) = IFHOST(3)
INTS(83) = IFHOST(4)
INTS(84) = IFHOST(5)
INTS(85) = IFHOST(6)
INTS(86) = IFHOST(7)

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: INTS=', &
               INTS

!.... Write integer scalars to buffer.
!
CALL IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, K)

!.... Write larger integer arrays from buffer
!
!     Note on processing arrays:
!     When handling a 2-dimensional array, consider a string of values a
!     complete column of the array and the length of the array as the
!     number of rows. So, a call to IFREAD or BFREAD will process 1 column
!     of the specified number of rows (length) in a 2-dimensional array.
!
!     CALL IFWRIT (WK3,IPNT,ILIMIT, array, length, K)
!     .....................................................................

DO 20 I = 1, 5
CALL IFWRIT (WK3, IPNT, ILIMIT, IDEF(1,I),        100, K) !(100,5)
20 CONTINUE

DO 22 I = 1, 5
CALL IFWRIT (WK3, IPNT, ILIMIT, IEVENT(1,I),      250, K) !(250,5)
22 CONTINUE

!.... Load real scalars then arrays into the REALS array.
!     Scalars and small one-dimensional arrays are handled first
!     for ease of indexing.
!
! BWEBOX.F77 - BWBOX
REALS(1)  = RAINDM
REALS(2)  = RAINDS
REALS(3)  = RAINM(1)
REALS(4)  = RAINM(2)
REALS(5)  = RAINM(3)
REALS(6)  = RAINS(1)
REALS(7)  = RAINS(2)
REALS(8)  = RAINS(3)
REALS(9)  = WHOTM
REALS(10) = WHOTSD
! BWECM2.F77 - BLCOMN
REALS(11) = DEFYRS(1)
REALS(12) = DEFYRS(2)
REALS(13) = DEFYRS(3)
REALS(14) = DEFYRS(4)
REALS(15) = DEFYRS(5)
REALS(16) = DEFYRS(6)
REALS(17) = DISPMR
REALS(18) = EGG1(1)
REALS(19) = EGG1(2)
REALS(20) = EGG1(3)
REALS(21) = EGG1(4)
REALS(22) = EGG1(5)
REALS(23) = EGG1(6)
REALS(24) = EGG2(1)
REALS(25) = EGG2(2)
REALS(26) = EGG2(3)
REALS(27) = EGG2(4)
REALS(28) = EGG2(5)
REALS(29) = EGG2(6)
REALS(30) = EGGDEN
REALS(31) = EGGS
REALS(32) = FRESHC(1)
REALS(33) = FRESHC(2)
REALS(34) = FRESHC(3)
REALS(35) = FRESHC(4)
REALS(36) = FRESHC(5)
REALS(37) = FRESHC(6)
REALS(38) = FWSURV
REALS(39) = GPERM2
REALS(40) = OLDMAX
REALS(41) = TREEDD
REALS(42) = WASTED(1)
REALS(43) = WASTED(2)
REALS(44) = WCOLDW
REALS(45) = WHOTF
REALS(46) = WRAIND
! BWECM2.F77 - BLCOM2
REALS(47) = AVEAMT(1)
REALS(48) = AVEAMT(2)
REALS(49) = AVEAMT(3)
REALS(50) = AVEAMT(4)
REALS(51) = AVEAMT(5)
REALS(52) = AVEAMT(6)
REALS(53) = DAYS(1)
REALS(54) = DAYS(2)
REALS(55) = DAYS(3)
REALS(56) = DISPX(1)
REALS(57) = DISPX(2)
REALS(58) = DISPX(3)
REALS(59) = DISPX(4)
REALS(60) = DISPY(1)
REALS(61) = DISPY(2)
REALS(62) = DISPY(3)
REALS(63) = DISPY(4)
REALS(64) = EWTX(1)
REALS(65) = EWTX(2)
REALS(66) = EWTX(3)
REALS(67) = EWTX(4)
REALS(68) = EWTY(1)
REALS(69) = EWTY(2)
REALS(70) = EWTY(3)
REALS(71) = EWTY(4)
REALS(72) = OLDX(1)
REALS(73) = OLDX(2)
REALS(74) = OLDX(3)
REALS(75) = OLDX(4)
REALS(76) = OLDY(1)
REALS(77) = OLDY(2)
REALS(78) = OLDY(3)
REALS(79) = OLDY(4)
REALS(80) = PMATED
REALS(81) = SRATIO
REALS(82) = WASTO
! BWECM2.F77 - BLCOM3
REALS(83) = ANT_BD(1)
REALS(84) = ANT_BD(2)
REALS(85) = ANT_BD(3)
REALS(86) = ANT_BD(4)
REALS(87) = ANT_BD(5)
REALS(88) = ANT_BD(6)
REALS(89) = ANT_BD(7)
REALS(90) = ANT_BD(8)
REALS(91) = ANT_BD(9)
REALS(92) = APRED(1)
REALS(93) = APRED(2)
REALS(94) = APRED(3)
REALS(95) = BPRED(1)
REALS(96) = BPRED(2)
REALS(97) = BPRED(3)
REALS(98) = DEFLYR
REALS(99) = DEVEL
REALS(100) = DEVELS(1)
REALS(101) = DEVELS(2)
REALS(102) = DEVELS(3)
REALS(103) = DFLUSH
REALS(104) = FOLDVX(1)
REALS(105) = FOLDVX(2)
REALS(106) = FOLDVX(3)
REALS(107) = FOLDVX(4)
REALS(108) = FOLDVY(1)
REALS(109) = FOLDVY(2)
REALS(110) = FOLDVY(3)
REALS(111) = FOLDVY(4)
REALS(112) = FOLWTX(1)
REALS(113) = FOLWTX(2)
REALS(114) = FOLWTX(3)
REALS(115) = FOLWTX(4)
REALS(116) = FOLWTY(1)
REALS(117) = FOLWTY(2)
REALS(118) = FOLWTY(3)
REALS(119) = FOLWTY(4)
REALS(120) = M1PRED(1)
REALS(121) = M1PRED(2)
REALS(122) = M1PRED(3)
REALS(123) = M2PRED(1)
REALS(124) = M2PRED(2)
REALS(125) = M2PRED(3)
REALS(126) = M3PRED(1)
REALS(127) = M3PRED(2)
REALS(128) = M3PRED(3)
REALS(129) = OBPHX(1)
REALS(130) = OBPHX(2)
REALS(131) = OBPHX(3)
REALS(132) = OBPHX(4)
REALS(133) = OBPHY(1)
REALS(134) = OBPHY(2)
REALS(135) = OBPHY(3)
REALS(136) = OBPHY(4)
REALS(137) = PRATE(1)
REALS(138) = PRATE(2)
REALS(139) = PRATE(3)
REALS(140) = PRATE(4)
REALS(141) = PRATE(5)
REALS(142) = PRATE(6)
REALS(143) = PRATE(7)
REALS(144) = PRATE(8)
REALS(145) = PRATE(9)
REALS(146) = RPHEN(1)
REALS(147) = RPHEN(2)
REALS(148) = RPHEN(3)
REALS(149) = RPHEN(4)
REALS(150) = RPHEN(5)
REALS(151) = RPHEN(6)
REALS(152) = SPEFF
REALS(153) = SYNCHX(1)
REALS(154) = SYNCHX(2)
REALS(155) = SYNCHX(3)
REALS(156) = SYNCHX(4)
REALS(157) = SYNCHX(5)
REALS(158) = SYNCHX(6)
REALS(159) = SYNCHY(1)
REALS(160) = SYNCHY(2)
REALS(161) = SYNCHY(3)
REALS(162) = SYNCHY(4)
REALS(163) = SYNCHY(5)
REALS(164) = SYNCHY(6)
REALS(165) = TRIGGR
REALS(166) = WRAIN1(1)
REALS(167) = WRAIN1(2)
REALS(168) = WRAIN1(3)
REALS(169) = WRAIN2(1)
REALS(170) = WRAIN2(2)
REALS(171) = WRAIN2(3)
REALS(172) = WRAIN3(1)
REALS(173) = WRAIN3(2)
REALS(174) = WRAIN3(3)
REALS(175) = WRAINA(1)
REALS(176) = WRAINA(2)
REALS(177) = WRAINA(3)
REALS(178) = WRAINB(1)
REALS(179) = WRAINB(2)
REALS(180) = WRAINB(3)
! BWECM2.F77 - BLCOM4
REALS(181) = ADMORT
REALS(182) = DISPDR(1)
REALS(183) = DISPDR(2)
REALS(184) = DISPDR(3)
REALS(185) = DISPDR(4)
REALS(186) = DISPDR(5)
REALS(187) = DISPDR(6)
REALS(188) = DISPDR(7)
REALS(189) = DISPDR(8)
REALS(190) = DISPDR(9)
REALS(191) = DISPMX(1)
REALS(192) = DISPMX(2)
REALS(193) = DISPMX(3)
REALS(194) = DISPMX(4)
REALS(195) = DISPMY(1)
REALS(196) = DISPMY(2)
REALS(197) = DISPMY(3)
REALS(198) = DISPMY(4)
REALS(199) = EGDISP
REALS(200) = EPMASS
REALS(201) = HOSTST
REALS(202) = OBSEER
REALS(203) = WSEEDR
! BWECOM.F77
REALS(204) = BWFINT
REALS(205) = DSEEDR
! BWESTD.F77
REALS(206) = FOLNH(1)
REALS(207) = FOLNH(2)
REALS(208) = FOLNH(3)
REALS(209) = FOLNH(4)
REALS(210) = FOLNH(5)
REALS(211) = FOLNH(6)
REALS(212) = FOLNH(7)
REALS(213) = FOLNH(8)
REALS(214) = FOLNH(9)
REALS(215) = PRCRN3(1)
REALS(216) = PRCRN3(2)
REALS(217) = PRCRN3(3)
REALS(218) = PRCRN3(4)
REALS(219) = PRCRN3(5)
REALS(220) = PRCRN3(6)
REALS(221) = PRCRN3(7)
REALS(222) = PRCRN3(8)
REALS(223) = PRCRN3(9)
!
!     Load REAL arrays larger than single dimension of 9
!     set index based on preceding assignment in scalar section.
!
IIX = 223
DO IIA = 1,4
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = NEMULT(IIA,IIB)   ! NEMULT(4,3) BWEBOX.F77
  END DO
END DO
!.... IIX at 235
DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = BW(IIA,IIB)       ! BW(9,6) BWECM2.FYY
  END DO
END DO
!.... IIX at 289
DO IIA = 1,2
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = EATEN(IIA,IIB)    ! EATEN(2,6) BWECM2.F77
  END DO
END DO
!.... IIX ends at 301


IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 1 - REALS=', &
               REALS

!.... Write real varaibles (scalars and arrays) to buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

!.... Load next real array(s) into REALS.

IIX = 0

DO IIA = 1,3
  DO IIB = 1,2
    IIX = IIX + 1
    REALS(IIX) = OBPHAS(IIA,IIB)    ! OBPHAS(3,2) BWECM2.F77
  END DO
END DO
!.... IIX at 6

DO IIA = 1,8
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = OBTABL(IIA,IIB)    ! OBTABL(8,3) BWECM2.F77
  END DO
END DO
!.... IIX at 30

DO IIA = 1,6
  DO IIB = 1,3
    DO IIC = 1,8
      IIX = IIX + 1
      REALS(IIX) = OUT2(IIA,IIB,IIC)  ! OUT2(6,3,8) BWECM2.F77
    END DO
  END DO
END DO
!.... IIX at 174

DO IIA = 1,3
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = OBTABL(IIA,IIB)    ! PHENOL(3,6) BWECM2.F77
  END DO
END DO
!.... IIX ends at 192

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 2 - REALS=', &
               REALS

!.... Write next real array(s) to buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

!.... Load next real array(s) into REALS.

IIX = 0
DO IIA = 1,9
  DO IIB = 1,6
    DO IIC = 1,17
      IIX = IIX + 1
      REALS(IIX) = OUT1(IIA,IIB,IIC)  ! OUT1(9,6,17) BWECM2.F77
    END DO
  END DO
END DO
!.... IIX ends at 918

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 3 - REALS=', &
               REALS

!.... Write next real array(s) to buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

!.... Load next real array(s) to REALS.
!.... Due to the size of this array, it is done in 2 parts,
!.... Third dimension (IIC) 1-10, part 1.
IIX = 0
DO IIA = 1,9
  DO IIB = 1,6
    DO IIC = 1,10
      IIX = IIX + 1
      REALS(IIX) = OUT3(IIA,IIB,IIC)  ! OUT3(9,6,20) BWECM2.F77
    END DO
  END DO
END DO
!.... IIX ends at 540

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 4A - REALS=', &
               REALS

!.... Write next real array(s) to buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)


!.... Load next real array(s) to REALS.
!.... Due to the size of this array, it is done in 2 parts,
!.... Third dimension (IIC) 11-20, part 2.
IIX = 0
DO IIA = 1,9
  DO IIB = 1,6
    DO IIC = 11,20
      IIX = IIX + 1
      REALS(IIX) = OUT3(IIA,IIB,IIC) ! OUT3(9,6,20) BWECM2.F77
    END DO
  END DO
END DO
!.... IIX ends at 540

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 4B - REALS=', &
               REALS

!.... Write next real array(s) to buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

!.... Load next real array(s) into REALS.

IIX = 0
DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = A1(IIA,IIB)    ! A1(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 54

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = A2(IIA,IIB)    ! A2(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 108

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = A3(IIA,IIB)    ! A3(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 162

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = A4(IIA,IIB)    ! A4(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 216

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = ACTNEW(IIA,IIB) ! ACTNEW(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 270

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = ANTDEN(IIA,IIB)    ! ANTDEN(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 324

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = BIRDEN(IIA,IIB)    ! BIRDEN(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 378

DO IIA = 1,2
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = ECI(IIA,IIB)    ! ECI(2,6) BWECM2.F77
  END DO
END DO
!.... IIX at 390

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = GMAX(IIA,IIB)    ! GMAX(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 444

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = GMIN(IIA,IIB)    ! GMIN(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 498

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = MYS1DN(IIA,IIB)    ! MYS1DN(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 552

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = MYS2DN(IIA,IIB)    ! MYS2DN(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 606

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = MYS3DN(IIA,IIB)    ! MYS3DN(9,6) BWECM2.F77
  END DO
END DO
!.... IIX at 660

DO IIA = 1,2
  DO IIB = 1,4
    IIX = IIX + 1
    REALS(IIX) = STARVX(IIA,IIB)    ! STARVX(2,4) BWECM2.F77
  END DO
END DO
!.... IIX at 714

DO IIA = 1,2
  DO IIB = 1,4
    IIX = IIX + 1
    REALS(IIX) = STARVY(IIA,IIB)    ! STARVY(2,4) BWECM2.F77
  END DO
END DO
!.... IIX ends at 768

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 5 - REALS=', &
               REALS

!.... Write next real array(s) to buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

!.... Load next real array(s) into REALS.

IIX = 0
DO IIA = 1,10
  DO IIB = 1,4
    IIX = IIX + 1
    REALS(IIX) = BWEATH(IIA,IIB)    ! BWEATH(10,4) BWECM2.F77
  END DO
END DO
!.... IIX at 40

DO IIA = 1,10
  IIX = IIX + 1
  REALS(IIX) = SPEFFS(IIA)         ! SPEFFS(10) BWECM2.F77
END DO
!.... IIX at 50

DO IIA = 1,10
  IIX = IIX + 1
  REALS(IIX) = SPINST(IIA)         ! SPINST(10) BWECM2.F77
END DO
!.... IIX at 60

DO IIA = 1,11
  DO IIB = 1,50
    IIX = IIX + 1
    REALS(IIX) = BWPRMS(IIA,IIB)   ! BWPRMS(11,50) BWECM2.F77
  END DO
END DO
!.... IIX ends at 610

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 6 - REALS=', &
               REALS

!.... Write next real array(s) to buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

!.... Load next real array(s) into REALS.
IIX = 0
DO IIA = 1,6
  DO IIB = 1,3
    DO IIC = 1,2
      DO IID = 1,5
        IIX = IIX + 1
        REALS(IIX) = APRBYR(IIA,IIB,IIC,IID)   ! APRBYR(6,3,2,5) BWESTD.F77
      END DO
    END DO
  END DO
END DO
!.... IIX at 180

DO IIA = 1,6
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = AVYRMX(IIA,IIB)   ! AVYRMX(6,3) BWESTD.F77
  END DO
END DO
!.... IIX at 198

DO IIA = 1,6
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = BWMXCD(IIA,IIB)   ! BWMXCD(6,3) BWESTD.F77
  END DO
END DO
!.... IIX at 216

DO IIA = 1,7
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = BWTPHA(IIA,IIB)   ! BWTPHA(7,3) BWESTD.F77
  END DO
END DO
!.... IIX at 237

DO IIA = 1,6
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = CDEF(IIA,IIB)     ! CDEF(6,3) BWESTD.F77
  END DO
END DO
!.... IIX at 255

DO IIA = 1,6
  DO IIB = 1,3
    DO IIC = 1,5
      IIX = IIX + 1
      REALS(IIX) = CUMDEF(IIA,IIB,IIC)   ! CUMDEF(6,3,5) BWESTD.F77
    END DO
  END DO
END DO
!.... IIX at 345

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = FNEW(IIA,IIB)   ! FNEW(9,6) BWESTD.F77
  END DO
END DO
!.... IIX ends at 399
!.... Write next real array(s) to buffer.

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 7 - REALS=', &
               REALS

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

!.... Load next real array(s) into REALS.

IIX = 0
DO IIA = 1,6
  DO IIB = 1,9
    DO IIC = 1,4
      IIX = IIX + 1
      REALS(IIX) = FOLADJ(IIA,IIB,IIC)   ! FOLADJ(6,9,4) BWESTD.F77
    END DO
  END DO
END DO
!.... IIX at 216

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = FOLD1(IIA,IIB)   ! FOLD1(9,6) BWESTD.F77
  END DO
END DO
!.... IIX at 270

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = FOLD2(IIA,IIB)   ! FOLD2(9,6) BWESTD.F77
  END DO
END DO
!.... IIX ends at 324

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 8 - REALS=', &
               REALS

!.... Write next real array(s) from buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

!.... Load next real array(s) into REALS.

IIX = 0
DO IIA = 1,6
  DO IIB = 1,9
    DO IIC = 1,4
      IIX = IIX + 1
      REALS(IIX) = FOLPOT(IIA,IIB,IIC)   ! FOLPOT(6,9,4) BWESTD.F77
    END DO
  END DO
END DO
!.... IIX at 216

DO IIA = 1,9
  DO IIB = 1,6
    IIX = IIX + 1
    REALS(IIX) = FREM(IIA,IIB)   ! FREM(9,6) BWESTD.F77
  END DO
END DO
!.... IIX at 270

DO IIA = 1,6
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = PEDDS(IIA,IIB)   ! PEDDS(6,3) BWESTD.F77
  END DO
END DO
!.... IIX at 288

DO IIA = 1,6
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = PEHTG(IIA,IIB)   ! PEHTG(6,3) BWESTD.F77
  END DO
END DO
!.... IIX at 306

DO IIA = 1,6
  DO IIB = 1,9
    DO IIC = 1,4
      IIX = IIX + 1
      REALS(IIX) = POFPOT(IIA,IIB,IIC)   ! POFPOT(6,9,4) BWESTD.F77
    END DO
  END DO
END DO
!.... IIX ends at 522

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 9 - REALS=', &
               REALS

!.... Write next real array(s) to buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)


!.... Load next real array(s) into REALS.

IIX = 0
DO IIA = 1,6
  DO IIB = 1,9
    DO IIC = 1,4
      IIX = IIX + 1
      REALS(IIX) = PRBIO(IIA,IIB,IIC)   ! PRBIO(6,9,4) BWESTD.F77
    END DO
  END DO
END DO
!.... IIX at 216

DO IIA = 1,6
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = RDDSM1(IIA,IIB)   ! RDDSM1(6,3) BWESTD.F77
  END DO
END DO
!.... IIX at 234

DO IIA = 1,4
  DO IIB = 1,9
    IIX = IIX + 1
    REALS(IIX) = RELFX(IIA,IIB)   ! RELFX(4,9) BWESTD.F77
  END DO
END DO
!.... IIX at 270

DO IIA = 1,4
  DO IIB = 1,9
    IIX = IIX + 1
    REALS(IIX) = RELFY(IIA,IIB)    ! RELFY(4,9) BWESTD.F77
  END DO
END DO
!.... IIX at 306

DO IIA = 1,6
  DO IIB = 1,3
    IIX = IIX + 1
    REALS(IIX) = RHTGM1(IIA,IIB)   ! RHTGM1(6,3) BWESTD.F77
  END DO
END DO
!.... IIX at 324

DO IIA = 1,4
  DO IIB = 1,9
    IIX = IIX + 1
    REALS(IIX) = THEOFL(IIA,IIB)   ! THEOFL(4,9) BWESTD.F77
  END DO
END DO
!.... IIX ends at 360

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 10 - REALS=', &
               REALS

!.... Write next real array(s) to buffer.

CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

!
!.... Write double precision scalars (random number seeds) to buffer.
!     SEEDA is DSEEDD, SEEDB is OBSEED, SEEDC is WSEED

IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 11 - SEEDA=',SEEDA, &
               ' SEEDB=',SEEDB,' SEEDC=',SEEDC

CALL BFWRIT (WK3, IPNT, ILIMIT, SEEDA, 2, K)
CALL BFWRIT (WK3, IPNT, ILIMIT, SEEDB, 2, K)
CALL BFWRIT (WK3, IPNT, ILIMIT, SEEDC, 2, K)

GOTO 210
!     These exist for control of numeric or character processing
!     specified by subroutine parameter IB.
150 CONTINUE

!
!.... Write character variables to buffer.
!
!     Static data items are not stored and retrieved between stands.
!
!     --- from BWEBOX ---
!     CHARACTER*4 MGMIDB
!     CHARACTER*8 DEFLAB,DLABS(5)
!     CHARACTER*12 STATES(10,2)    -- Static
!     CHARACTER*16 WSLOOK(100,10)  -- Static
!     CHARACTER*20 TEMPS2(3)       -- Static
!     CHARACTER*40 OUTNAM(8)       -- Static
!                  STNAME          -- Static
!                  WFNAME          -- Static
!     CHARACTER*50 EVENT(20)       -- Static
!     CHARACTER*72 ITITLB
!     --- from BWECM2 ---
!     CHARACTER*3 IOUT6A(3)
!     --- from BWECOM ---
!     CHARACTER*8 TABLE(25)  -- Static

K=1
DO 200 I=1,4
  CALL CHWRIT(CBUFF,IPNT,LNCBUF,MGMIDB(I:I), K)
  K=2
200 CONTINUE

DO 202 I=1,8
  CALL CHWRIT(CBUFF,IPNT,LNCBUF,DEFLAB(I:I), K)
202 CONTINUE

DO 204 J=1,5
  DO 204 I=1,8
    CALL CHWRIT(CBUFF,IPNT,LNCBUF,DLABS(J)(I:I), K)
204 CONTINUE

DO 206 I=1,72
  CALL CHWRIT(CBUFF,IPNT,LNCBUF,ITITLB(I:I), K)
206 CONTINUE

DO 208 J=1,3
  DO 208 I=1,3
    IF (I .EQ. 3) K=3
    CALL CHWRIT(CBUFF,IPNT,LNCBUF,IOUT6A(J)(I:I), K)
208 CONTINUE

210 CONTINUE
IF (PDEBUG) &
       WRITE (JOPPRT,'(/'' IN BWEPPPT: END OF ISTND,ICYC='',I7,I4)') &
             ISTND,ICYC

RETURN
END
