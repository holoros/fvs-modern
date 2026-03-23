SUBROUTINE CWHR(ISP, DBH, FVS_HT, FMICR, FMPROB, CWID, FMITRN, &
              CWXPTS,CCBP,DBHBP,SZDN,CWHR_MOD,CWHR_WT)
IMPLICIT NONE
!----------
! FIRE-WS $Id$
!----------

INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

INCLUDE 'FMFCOM.f90'
INCLUDE 'CONTRL.f90'
!
!      Public Function CWHRSizeDensity(rst As DAO.Recordset) As String
!'  rst is a DAO recordset, field names must be trees, Dbh, Ht, CrA
!
!      Dim cc As Single, cc1 As Single, cc2 As Single, cc3 As Single,
!          cc4 As Single, cc5 As Single
!      Dim ht3 As Single, ht4 As Single, ht5 As Single
!      Dim tpa3 As Single, tpa4 As Single, tpa5 As Single
!      Dim basal As Single, dbh As Single, bapct As Single, ltrees
!          As Single, sumdbhsq As Single, qmd As Single
!      Dim sng1 As Single
!      Dim ftpa As Field, fdbh As Field, fht As Field, fcra As Field
!      Dim fba As Field
!      Dim size As String, dens As String
!      Dim ccadj As Single, cc3adj As Single
!      Dim bDone As Boolean, QmdPctile As Single, targetbasal
!          As Single, treebasal As Single, sumbasal As Single
!      Dim ccnetavg As Single

CHARACTER*3 SZDN
INTEGER     FMITRN, ISP, FMICR, IDUM
REAL        DBH, FVS_HT, FMPROB, CCBP, DBHBP, CWXPTS, XSUM, CWID

DIMENSION   ISP(FMITRN), FMICR(FMITRN), DBH(FMITRN), CWID(FMITRN)
DIMENSION   FVS_HT(FMITRN), FMPROB(FMITRN)
DIMENSION   CWXPTS(4,2), CCBP(3), DBHBP(4)

INTEGER     CWHR_MOD(4)
REAL        CWHR_WT(4), XV(4), YV(4)

CHARACTER*1 SZ, DN
INTEGER I,J,K
INTEGER IXS(MAXTRE)
REAL CWIDTH, CAREA, XX
REAL QMDPCTILE, BASAL, QMD, TARGETBASAL, TREEBASAL
REAL SUMBASAL, SUMDBHSQ, LTREES, SNG1
REAL CCNETAVG, CCADJ, PCNETAVG
REAL HT(0:5), CC(0:5), TPA(0:5)
!
!     VARIABLES FOR CALL TO FMDYN
!
INTEGER   IPTR(4), ITYP(4)
REAL      EQWT(4)
INTEGER IDANUW
!
!     THE INTEGER TAGS ASSOCIATED WITH EACH MODEL
!     CLASS. THEY ARE RETURNED WITH THE WEIGHT
!     10=S; 20=P; 30=M; 40=D
!
DATA IPTR / 10,20,30,40 /
DATA ITYP /  0, 0, 0, 0 /
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
IDANUW = FMICR(1)
IDANUW = ISP(1)
!
!
!     INITIALIZE WEIGHTS OF DYNAMIC MODELS
!
DO I = 1, 4
  EQWT(I)     = 0.0
  CWHR_MOD(I) = 0
  CWHR_WT(I)  = 0.0
ENDDO
!
!      With rst
!        Set ftpa = !trees
!        Set fdbh = !dbh
!        Set fht = !ht
!        Set fcra = !cra ! must be crown radius OR AREA??
!        .MoveFirst
!        Do Until .EOF
!          basal = basal + ftpa * fdbh ^ 2 * 0.005454
!          cc = cc + ftpa * fcra
!          trees = trees + ftpa
!          Select Case fdbh
!            Case Is < 1
!              cc1 = cc1 + ftpa * fcra
!            Case Is < 6
!              cc2 = cc2 + ftpa * fcra
!            Case Is < 11
!              cc3 = cc3 + ftpa * fcra
!              ht3 = ht3 + fht * ftpa
!              tpa3 = tpa3 + ftpa
!            Case Is < 24
!              cc4 = cc4 + ftpa * fcra
!              ht4 = ht4 + fht * ftpa
!              tpa4 = tpa4 + ftpa
!            Case Is >= 24
!              cc5 = cc5 + ftpa * fcra
!              ht5 = ht5 + fht * ftpa
!              tpa5 = tpa5 + ftpa
!          End Select
!          MoveNext
!        Loop
!      End With

XX = 0.0
DO I = 0,5
  HT(I)  = 0.0
  CC(I)  = 0.0
  TPA(I) = 0.0
ENDDO
BASAL = 0.0
DO I = 1,FMITRN
  IF (FMPROB(I) .GT. 0.0) THEN

    BASAL = BASAL + FMPROB(I) * DBH(I) * DBH(I) * 0.0054542

    CWIDTH=CWID(I)
    CAREA = 3.1415927*CWIDTH*CWIDTH/4.0

    IF     (DBH(I) .LT. DBHBP(1)) THEN
      K = 1
    ELSEIF (DBH(I) .LT. DBHBP(2)) THEN
      K = 2
    ELSEIF (DBH(I) .LT. DBHBP(3)) THEN
      K = 3
    ELSEIF (DBH(I) .LT. DBHBP(4)) THEN
      K = 4
    ELSE
      K = 5
    ENDIF

    HT(K)  = HT(K) + (FVS_HT(I) * FMPROB(I))
    CC(K)  = CC(K) + (CAREA * FMPROB(I))
    TPA(K) = TPA(K) + FMPROB(I)

  ENDIF
ENDDO

DO I = 1,5
  HT(0)  = HT(0)  + HT(I)
  CC(0)  = CC(0)  + CC(I)
  TPA(0) = TPA(0) + TPA(I)
ENDDO
!'  convert crown cover to percent
!
!      cc = cc / 435.6
!      cc1 = cc1 / 435.6
!      cc2 = cc2 / 435.6
!      cc3 = cc3 / 435.6
!      cc4 = cc4 / 435.6
!      cc5 = cc5 / 435.6
!      ccnetavg = pcNetAvg(cc)

DO I = 0,5
  CC(I) = CC(I)/435.60
ENDDO
CCNETAVG = PCNETAVG(CC(0))

!'  divide accumulated ht * tpa by tpa to get avg hts for size 3, 4, and 5
!
!      If tpa3 > 0 Then ht3 = ht3 / tpa3
!      If tpa4 > 0 Then ht4 = ht4 / tpa4
!      If tpa5 > 0 Then ht5 = ht5 / tpa5

DO I = 3,5
  IF (TPA(I) .GT. 0.0) HT(I) = HT(I) / TPA(I)
ENDDO

!'  Check for tree dominated habitat
!'  NOTE: pcNetAvg produces a weighted average bewteen uncorrected
!'  percent cover and percent cover assuming random spacing
!
!      ccadj = pcNetAvg(cc)
!      If ccnetavg < 10 Then
!        If trees >= 150 Then
!          CWHRSizeDensity = "1"
!        Else
!          CWHRSizeDensity = "XX"     '  designates as non-forest
!        End If
!        Exit Function
!      End If

CCADJ = PCNETAVG(CC(0))
IF (CCNETAVG .LT. 10) THEN
  IF (TPA(0) .GE. 150.) THEN
    SZDN = "1,-"
  ELSE
    SZDN = "X,-"
  ENDIF
  CWHR_MOD(1) = 10
  CWHR_WT(1)  = 1.0
  RETURN
ENDIF

!      trees = 0      '  reset to use for qmd calculation
!
!'  Check for multi-story
!
!      If ccnetavg >= 60 And ht5 > 0 And ht4 > 0 And ht3 > 0 Then
!        If cc5 >= 20 And cc5 <= 80 Then
!          If cc3 >= 20 And (ht3 / ht5) <= 0.6667 Then
!            CWHRSizeDensity = "6"
!            Exit Function
!          ElseIf cc4 >= 20 And (ht4 / ht5) <= 0.6667 Then
!            CWHRSizeDensity = "6"
!            Exit Function
!          ElseIf cc3 + cc4 >= 30 And (ht3 * tpa3 + ht4 * tpa4)
!                  / (tpa3 + tpa4) / ht5 <= 0.6667 Then
!            CWHRSizeDensity = "6"
!            Exit Function
!          End If
!        End If
!      End If

IF (CCNETAVG .GE. CCBP(3) .AND. &
      HT(5) .GT. 0.0 .AND. HT(4) .GT. 0.0 .AND. HT(3) .GT. 0.0) THEN
  IF (CC(5) .GE. 20.0 .AND. CC(5) .LE. 80.0) THEN
    IF (CC(3) .GE. 20.0 .AND. (HT(3)/HT(5)) .LE. (2./3.)) THEN
      SZDN        = "6,-"
      CWHR_MOD(1) = 40
      CWHR_WT(1)  = 1.0
      RETURN
    ELSEIF (CC(4) .GE. 20.0 .AND. &
           (HT(4)/HT(5)) .LE. (2./3.)) THEN
      SZDN        = "6,-"
      CWHR_MOD(1) = 40
      CWHR_WT(1)  = 1.0
      RETURN
    ELSEIF ( (CC(3) + CC(4)) .GE. 30.0 .AND. &
           ( ( (HT(3) * TPA(3) + HT(4) * TPA(4)) / &
           (TPA(3) + TPA(4)) ) / HT(5)) .LE. (2./3.)) THEN
      SZDN        = "6,-"
      CWHR_MOD(1) = 40
      CWHR_WT(1)  = 1.0
      RETURN
    ENDIF
  ENDIF
ENDIF

!'  Not multi-story
!
!      If ccadj < 25 Then   '  do the sparse stands
!                                 by predominance of pct cover

IF (CCADJ .LT. CCBP(1)) THEN

!        If cc3 + cc4 + cc5 < cc1 + cc2 Then
!          If cc2 >= cc1 Then
!            CWHRSizeDensity = "2S"
!          Else
!            CWHRSizeDensity = "1S"
!          End If
!        Else
!          If cc3 > cc4 + cc5 Then
!            CWHRSizeDensity = "3S"
!          ElseIf cc5 >= cc4 + cc3 Then
!            CWHRSizeDensity = "3S"
!          Else
!            CWHRSizeDensity = "4S"
!          End If
!        End If
!        Exit Function

  IF ((CC(3) + CC(4) + CC(5)) .LT. &
         (CC(1) + CC(2))) THEN
    IF (CC(2) .GE. CC(1)) THEN
      SZDN = "2,S"
    ELSE
      SZDN = "1,S"
    ENDIF
  ELSE
    IF (CC(3) .GT. (CC(4) + CC(5))) THEN
      SZDN = "3,S"
    ELSEIF (CC(5) .GE. (CC(4) + CC(3))) THEN
      SZDN = "5,S"
    ELSE
      SZDN = "4,S"
    ENDIF
  ENDIF
  CWHR_MOD(1) = 10
  CWHR_WT(1)  = 1.0
  RETURN

!      Else

ELSE

!        If cc4 + cc5 < 10 Then  '  use whole stand qmd for small-tree stands
!          QmdPctile = 100
!        End If
!        targetbasal = basal * QmdPctile
!        sumbasal = 0: ltrees = 0
!        bDone = False

  QMDPCTILE = 0.75
  IF (CC(4) + CC(5) .LT. 10.0) THEN
    QMDPCTILE = 1.00
  ENDIF
  TARGETBASAL = BASAL * QMDPCTILE
  SUMBASAL   = 0.0
  LTREES     = 0.0
  SUMDBHSQ   = 0.0
!        bDone = False
!        With rst
!          .Sort = "dbh DESC"
!         .Requery
!         .MoveFirst
!          Do Until bDone
!            treebasal = ftpa * fdbh ^ 2 * 0.005454
!            If sumbasal + treebasal < targetbasal Then
!              sumbasal = sumbasal + treebasal
!              sumdbhsq = sumdbhsq + ftpa * fdbh ^ 2
!              ltrees = ltrees + ftpa
!            Else
!'  how much of this tree record do I need?
!              sng1 = (targetbasal - sumbasal) / treebasal
!              ltrees = ltrees + ftpa * sng1
!              sumdbhsq = sumdbhsq + fdbh ^ 2 * ftpa * sng1
!              bDone = True
!            End If
!            MoveNext
!            If .EOF Then bDone = True
!         Loop
!        End With
!
  IF(FMITRN .GT. 0) CALL RDPSRT(FMITRN,DBH,IXS,.TRUE.)
  DO J = 1,FMITRN
    I = IXS(J)
    TREEBASAL = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
    IF ((SUMBASAL + TREEBASAL) .LT. TARGETBASAL) THEN
      SUMBASAL = SUMBASAL + TREEBASAL
      SUMDBHSQ = SUMDBHSQ + (FMPROB(I) * DBH(I) * DBH(I))
      LTREES   = LTREES + FMPROB(I)
    ELSE
      SNG1 = (TARGETBASAL - SUMBASAL) / TREEBASAL
      LTREES = LTREES + (FMPROB(I) * SNG1)
      SUMDBHSQ = SUMDBHSQ + (DBH(I) * DBH(I) * FMPROB(I) * SNG1)
      GOTO 100
    ENDIF
  ENDDO
100   CONTINUE


!        If ltrees > 0 Then
!          qmd = Sqr(sumdbhsq / ltrees)
!        Else
!          qmd = 0
!        End If

  QMD = 0.0
  IF (LTREES .GT. 0.0) QMD = SQRT(SUMDBHSQ / LTREES)

!        Select Case qmd
!          Case Is < 1
!            CWHRSizeDensity = "1"
!            Exit Function
!          Case Is < 6
!            size = "2"
!            ccadj = pcNetAvg(cc)
!          Case Is < 11
!            size = "3"
!            cc = cc2 + cc3 + cc4 + cc5
!            ccadj = pcNetAvg(cc)
!'  don't do percent cover correction for medium and large-tree stands
!          Case Is < 24
!            size = "4"
!            cc = cc3 + cc4 + cc5
!            ccadj = cc
!          Case Else
!            size = "5"
!            cc = cc3 + cc4 + cc5
!            ccadj = cc
!        End Select

  IF (QMD .LT. DBHBP(1)) THEN
    SZDN = "1,-"
    CWHR_MOD(1) = 10
    CWHR_WT(1)  = 1.0
    RETURN
  ELSEIF (QMD .LT. DBHBP(2)) THEN
    SZ = "2"
    CCADJ = PCNETAVG(CC(0))
  ELSEIF (QMD .LT. DBHBP(3)) THEN
    SZ = "3"
    CC(0) = CC(2) + CC(3) + CC(4) + CC(5)
    CCADJ = PCNETAVG(CC(0))
  ELSEIF (QMD .LT. DBHBP(4)) THEN
    SZ = "4"
    CC(0) = CC(3) + CC(4) + CC(5)
    CCADJ = CC(0)
  ELSE
    SZ = "5"
    CC(0) = CC(3) + CC(4) + CC(5)
    CCADJ = CC(0)
  ENDIF

!      End If

ENDIF

!      Select Case ccadj
!        Case Is < 25
!          dens = "S"
!        Case Is < 40
!          dens = "P"
!          If ccnetavg < 25 Then dens = "S"
!        Case Is < 60
!          dens = "M"
!          If ccnetavg < 40 Then dens = "P"
!        Case Else
!          dens = "D"
!          If ccnetavg < 60 Then dens = "M"
!      End Select

IF (CCADJ .LT. CCBP(1)) THEN
  DN = "S"
ELSEIF (CCADJ .LT. CCBP(2)) THEN
  DN = "P"
  IF (CCNETAVG .LT. CCBP(1)) DN = "S"
ELSEIF (CCADJ .LT. CCBP(3)) THEN
  DN = "M"
  IF (CCNETAVG .LT. CCBP(2)) DN = "P"
ELSE
  DN = "D"
  IF (CCNETAVG .LT. CCBP(3)) DN = "M"
ENDIF
!
!     CALL FMDYN TO FIND WEIGHTS FOR THE S,M,P,D MODELS
!     FMDYN PLACES RESULTS IN FWT,FMOD (IN **FMFCOM**) AND IN
!     THE RETURNED VARIABLE FMD. THESE VALUES ARE OVERWRITTEN
!     IN A LATER CALL THE FMDYN, SO TEMPORARILY SETTING THEM
!     HERE IS BENIGN
!
DO I = 1,4
  EQWT(I) = 1.0
ENDDO
CALL FMDYN(CCADJ,CCNETAVG,ITYP,CWXPTS,EQWT,IPTR,4,.TRUE.,IDUM)
DO I = 1,4
  CWHR_MOD(I) = FMOD(I)
  CWHR_WT(I)  = FWT(I)
ENDDO
!
!     IN SOME CIRCUMSTANCES 3 MODELS MAY BE CANDIDATES. MODEL
!     40 HAS A LOW SLOPE AND IN THE UP/DN/LF/RT SEARCH CAN BE
!     FOUND 'ABOVE' THE POINT BUT OUTSIDE THE DOMAIN. THE
!     THIRD MODEL TO BE REMOVED IS ALWAYS OF TYPE 40.
!     THE REMAINING 2 MODELS ARE PUT IN POSITIONS 1 AND 2
!
J = 0
DO I = 1,4
  IF (CWHR_MOD(I) .GT. 0) J = J + 1
ENDDO

IF (J .GT. 2) THEN
  DO I = 1,4
    IF (CWHR_MOD(I) .EQ. 40) THEN
      CWHR_MOD(I) = 0
      CWHR_WT(I)  = 0.0
    ENDIF
    XV(I) = REAL(CWHR_MOD(I))
    YV(I) = CWHR_WT(I)
    CWHR_MOD(I) = 0
    CWHR_WT(I)  = 0.0
  ENDDO
  CALL RDPSRT(4,XV,IXS,.TRUE.)
  DO I = 1,2
    J = IXS(I)
    CWHR_MOD(I) = INT(XV(J))
    CWHR_WT(I) = YV(J)
  ENDDO
  XSUM = 0.0
  DO I = 1,2
    XSUM = XSUM + CWHR_WT(I)
  ENDDO
  DO I = 1,2
    CWHR_WT(I) = CWHR_WT(I) / XSUM
  ENDDO
ENDIF

!      CWHRSizeDensity = size & dens

SZDN(1:1) = SZ(1:1)
SZDN(2:2) = ','
SZDN(3:3) = DN(1:1)
RETURN

END

!      End Function

!
!     LOOKS LIKE THIS DOES A CORRECTED % CANOPY COVER
!
FUNCTION PCNETAVG(grosspercentcover)
!
real pcRndPack, avg1

pcRndPack = (1. - 1. / Exp(grosspercentcover / 100.)) * 100.
avg1 = pcRndPack / 100. * pcRndPack + (100. - pcRndPack) / 100 * &
           grosspercentcover
!      If (avg1 > 100.) Then avg1 = 100.
IF(AVG1 .GT. 100.)AVG1=100.
pcNetAvg = (pcRndPack + avg1) / 2.
return
end
