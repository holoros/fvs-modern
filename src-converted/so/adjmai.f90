FUNCTION ADJMAI(INSPEC,SINDEX,POINTS,IERROR)
IMPLICIT NONE
!----------
! SO $Id$
!----------
!
!         THIS FUNCTION COMPUTES THE ADJUSTED MAI FOR AN NFI PLOT USING
!         THREE DATA ITEMS FROM THE NFI PLOT SUMMARY FILE...
!         THE INPUT ITEMS ARE INSPEC, THE SITE SPECIES CODE,
!                             SINDEX, THE SITE INDEX,
!                             POINTS, THE NUMBER OF STOCKABLE POINTS.
!         THE VARIABLE IERROR RETURNS A VALUE OF 1 IF A SPECIES
!         CODE IS UNACCEPTABLE. OTHERWISE, IERROR IS SET TO ZERO.
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'

INCLUDE 'CONTRL.f90'
!
!OMMONS

INTEGER IMAP(36),ISP(36),IERROR,INSPEC,NCALLS,I,INUM
REAL POINTS,SINDEX,ADJMAI

!     FIA SPECIES CODES ORGANIZED IN SITE INDEX EQUATION GROUPS
!
DATA ISP/ &
    019,041,042,071,081,094,095,242,263,264,098,298, &
!     EQ GROUP 2
!      WB  KP  LP  CP  BP  MP
    101,103,108,109,120,124, &
!     EQ GROUP 3
!      WP
    119, &
!     EQ GROUP 4
!      JP  SP  PP
    116,117,122, &
!     EQ GROUP 5
!      BD  DF
    201,202, &
!     EQ GROUP 6
!       WL
    073, &
!     EQ GROUP 7
!      BR  ES
    092,093, &
!     EQ GROUP 8 IS THE HARDWOOD GROUP WITHOUT SPECIFIC SPECIES ASSGNED
!                FIA CODES OF 300 AND GREATER ARE ASSIGNED GROUP 8.
!     EQ GROUP 9
!      SF  WF  GF  AF  RF  SH  NF
    011,015,017,019,020,021,022, &
!     EQ GROUP 10
!      RW  GS  ??
    211,212/

!     EQUATION GROUP ASSIGNMENT MAP
DATA IMAP/12*1,6*2,3,3*4,2*5,6,2*7,7*9,2*10/
!
!
NCALLS=1
IERROR = 0
ADJMAI = 0.0
!
!   IF SPECIES CODE IS > 300 THEN SPECIES IS HARDWOOD. GO TO GROUP 8.
!
IF(INSPEC.GE.300) GO TO 80
!
!      CHECK FOR A LEGAL SPECIES CODE...
!
DO 2 I=1,32
   IF(INSPEC.EQ.ISP(I))GO TO 9
2 CONTINUE
!
WRITE (JOSTND,3) INSPEC,NCALLS
3 FORMAT(/,' ******** WARNING:  ILLEGAL SITE INDEX SPECIES (',I3, &
          ') WAS DETECTED FOR RECORD NUMBER ',I5,'.'/T21, &
          'NO ADJUSTED MAI WAS CALCULATED.')
IERROR = 1
RETURN
!
!        LEGAL SPECIES FOUND - FIND EQUATION GROUP USING COMPUTED GOTO
!        STATEMENT
!
9 CONTINUE
INUM=IMAP(I)
!
GO TO (10,20,30,40,50,60,70,80,90,100),INUM
!
!================================================================
!                   GROUP 1...
10 CONTINUE
IF(SINDEX.LT.33.0) RETURN
ADJMAI = -63.689706 + (1.9402941 * SINDEX)
GO TO 200
!================================================================
!                   GROUP 2...
20 CONTINUE
IF(SINDEX.LT.11.0) RETURN
ADJMAI = -12.0388 + (1.18672 * SINDEX)
GO TO 200
!================================================================
!                   GROUP 3...
30 CONTINUE
ADJMAI = 5.972615 + (1.857675 * SINDEX)
GO TO 200
!================================================================
!                   GROUP 4...
40 CONTINUE
ADJMAI=2.305357  + 0.033890056 * SINDEX + 0.0090108543 * SINDEX**2
GO TO 200
!================================================================
!                   GROUP 5...
50 CONTINUE
IF(SINDEX.LT.29.0) RETURN
ADJMAI = -10.303313 + .032929911 * SINDEX + .012207163 * SINDEX**2 &
                  + (-.00003543129 * SINDEX**3)
GO TO 200
!================================================================
!                   GROUP 6...
60 CONTINUE
IF(SINDEX.LT.11.0) RETURN
ADJMAI = -6.0892857 + .45178571 * SINDEX + .014464286 * SINDEX**2
GO TO 200
!================================================================
!                   GROUP 7...
70 CONTINUE
IF(SINDEX.LT.10.0) RETURN
ADJMAI = -18.4 + 1.92 * SINDEX
GO TO 200
!================================================================
!                   GROUP 8 ...
80 CONTINUE
IF(SINDEX.LT.32.0) RETURN
ADJMAI = -53.892857 + 1.7178571 * SINDEX
GO TO 200
!================================================================
!                   GROUP 9...
90 CONTINUE
ADJMAI = -4.89001 + 311.29546 * ((EXP( (SINDEX / 170.0 - 1.0)**3 &
                                       / 0.343) - 0.055) / 0.95)
GO TO 200
!================================================================
!                    GROUP 10 ...
100 CONTINUE
IF(SINDEX.LT.62.0) RETURN
ADJMAI = 157.94643 - 1.78125 * SINDEX + .014330357 * SINDEX**2
200 CONTINUE
ADJMAI = ADJMAI * POINTS/10.0
IF(ADJMAI.LT.0.0) ADJMAI = 0.0
RETURN
END
