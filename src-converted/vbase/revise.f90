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
  REV = '20250930'
!----------
! BLUE MOUNTAINS
!----------
  CASE('BM')
  REV = '20250930'
!----------
! INLAND CALIFORNIA / SOUTHERN CASCADES
!----------
  CASE('CA')
  REV = '20250930'
!----------
! CENTRAL IDAHO
!----------
  CASE('CI')
  REV = '20250930'
!----------
! CENTRAL ROCKIES
!----------
  CASE('CR')
  REV = '20250930'
!----------
! CENTRAL STATES
!----------
  CASE('CS')
  REV = '20250930'
!----------
! EAST CASCADES
!----------
  CASE('EC')
  REV = '20250930'
!----------
! EASTERN MONTANA
!----------
  CASE('EM')
  REV = '20250930'
!----------
! INLAND EMPIRE (AKA NI23)
!----------
  CASE('IE')
  REV = '20250930'
!----------
! KOOTENAI / KANIKSU / TALLY LAKE
!----------
  CASE('KT')
  REV = '20250930'
!----------
! LAKE STATES
!----------
  CASE('LS')
  REV = '20250930'
!----------
! KLAMATH MOUNTAINS
!----------
  CASE('NC')
  REV = '20250930'
!----------
! NORTHEAST STATES
!----------
  CASE('NE')
  REV = '20250930'
!----------
! SOUTHWEST OREGON ORGANON
!----------
  CASE('OC')
  REV = '20250930'
!----------
! NORTHWEST OREGON ORGANON
!----------
  CASE('OP')
  REV = '20250930'
!----------
! PACIFIC NORTHWEST
!----------
  CASE('PN')
  REV = '20250930'
!----------
! SOUTHERN
!----------
  CASE('SN')
  REV = '20250930'
!----------
! SOUTH CENTRAL OREGON / NORTHEAST CALIFORNIA
!----------
  CASE('SO')
  REV = '20250930'
!----------
! TETONS
!----------
  CASE('TT')
  REV = '20250930'
!----------
! UTAH
!----------
  CASE('UT')
  REV = '20250930'
!----------
! WEST CASCADES
!----------
  CASE('WC')
  REV = '20250930'
!----------
! WESTERN SIERRA NEVADA
!----------
  CASE('WS')
  REV = '20250930'
!----------
! ANY OTHER VARIANT
!----------
  CASE DEFAULT
  REV = '20250930'
!
END SELECT
!
RETURN
END
