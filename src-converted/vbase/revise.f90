SUBROUTINE REVISE (VAR,REV)
IMPLICIT NONE
!----------
! VBASE
!----------
!  **REVISE--BASE  DATE OF LAST REVISION:  06/01/18
!  (DON NOT CHANGE THIS DATE UNLESS THE SUBROUTINE LOGIC CHANGES.)
!----------
!  THIS ROUTINE PROVIDES THE LATEST REVISION DATE FOR EACH VARIANT
!  WHICH GETS PRINTED IN THE MAIN HEADER ON THE OUTPUT.
!  CALLED FROM GROHED, FILOPN, SUMHED, SUMOUT, ECVOLS, PRTRLS,
!  AND DGDRIV.
!----------
CHARACTER VAR*2,REV*10
!
SELECT CASE (VAR)
!----------
! SOUTHEAST ALASKA / COASTAL BRITISH COLUMBIA
!----------
  CASE('AK')
  REV = '20260401'
!----------
! BLUE MOUNTAINS
!----------
  CASE('BM')
  REV = '20260401'
!----------
! INLAND CALIFORNIA / SOUTHERN CASCADES
!----------
  CASE('CA')
  REV = '20260401'
!----------
! CENTRAL IDAHO
!----------
  CASE('CI')
  REV = '20260401'
!----------
! CENTRAL ROCKIES
!----------
  CASE('CR')
  REV = '20260401'
!----------
! CENTRAL STATES
!----------
  CASE('CS')
  REV = '20260401'
!----------
! EAST CASCADES
!----------
  CASE('EC')
  REV = '20260401'
!----------
! EASTERN MONTANA
!----------
  CASE('EM')
  REV = '20260401'
!----------
! INLAND EMPIRE (AKA NI23)
!----------
  CASE('IE')
  REV = '20260401'
!----------
! KOOTENAI / KANIKSU / TALLY LAKE
!----------
  CASE('KT')
  REV = '20260401'
!----------
! LAKE STATES
!----------
  CASE('LS')
  REV = '20260401'
!----------
! KLAMATH MOUNTAINS
!----------
  CASE('NC')
  REV = '20260401'
!----------
! NORTHEAST STATES
!----------
  CASE('NE')
  REV = '20260401'
!----------
! SOUTHWEST OREGON ORGANON
!----------
  CASE('OC')
  REV = '20260401'
!----------
! NORTHWEST OREGON ORGANON
!----------
  CASE('OP')
  REV = '20260401'
!----------
! PACIFIC NORTHWEST
!----------
  CASE('PN')
  REV = '20260401'
!----------
! SOUTHERN
!----------
  CASE('SN')
  REV = '20260401'
!----------
! SOUTH CENTRAL OREGON / NORTHEAST CALIFORNIA
!----------
  CASE('SO')
  REV = '20260401'
!----------
! TETONS
!----------
  CASE('TT')
  REV = '20260401'
!----------
! UTAH
!----------
  CASE('UT')
  REV = '20260401'
!----------
! WEST CASCADES
!----------
  CASE('WC')
  REV = '20260401'
!----------
! WESTERN SIERRA NEVADA
!----------
  CASE('WS')
  REV = '20260401'
!----------
! ANY OTHER VARIANT
!----------
  CASE DEFAULT
  REV = '20260401'
!
END SELECT
!
RETURN
END

