MODULE COMMONDATA



  IMPLICIT NONE

! ====================== Precision Options =========================== !
  INTEGER, PARAMETER :: sp   = SELECTED_REAL_KIND(6, 37)     ! 32-bit
  INTEGER, PARAMETER :: dp   = SELECTED_REAL_KIND(15, 307)   ! 64-bit
  INTEGER, PARAMETER :: qp   = SELECTED_REAL_KIND(33, 4931)  ! 128-bit
  INTEGER, PARAMETER :: prec = sp                            ! SpecIFy the precision here
! ==================================================================== !

END MODULE COMMONDATA
