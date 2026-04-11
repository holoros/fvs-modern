! Stub implementations for ECON module and other unresolved symbols.
! These satisfy linker requirements without the full implementations.
! IMPORTANT: Stub signatures must match real subroutine signatures
! to avoid argument mismatch corruption of common block variables.

subroutine ECINIT
  return
end subroutine ECINIT

subroutine ECIN(PASESSION, KESSION)
  integer PASESSION, KESSION
  return
end subroutine ECIN

subroutine ECCALC(IY, ICYC, JSP, MGMID, NPLT, ITITLE)
  implicit none
  integer, intent(in) :: IY(*), ICYC, JSP(*)
  character(*), intent(in) :: MGMID, NPLT, ITITLE
  return
end subroutine ECCALC

subroutine ECHARV
  return
end subroutine ECHARV

subroutine ECSETP(IY)
  implicit none
  integer, intent(in) :: IY(*)
  return
end subroutine ECSETP

subroutine ECSTATUS(ICYC, NCYC, IY, beforeCuts)
  implicit none
  integer, intent(in) :: ICYC, NCYC, beforeCuts
  integer, intent(in) :: IY(*)
  return
end subroutine ECSTATUS

subroutine ECVOL
  return
end subroutine ECVOL

subroutine ECKEY(PASSESSION, LESSION)
  integer PASSESSION, LESSION
  return
end subroutine ECKEY

integer function GETISPRETENDACTIVE()
  GETISPRETENDACTIVE = 0
  return
end function GETISPRETENDACTIVE

subroutine VARVER(ESSION)
  integer ESSION
  return
end subroutine VARVER
