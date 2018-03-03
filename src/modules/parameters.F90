!> \file: parameters.F90
!> \author: Marc Salvadori
!> \brief: This module provides some useful parameters and constants

MODULE parameters_m

      IMPLICIT NONE

      INTEGER,PARAMETER :: wp = SELECTED_REAL_KIND(8)
      REAL(KIND=wp),PARAMETER :: PI = 3.14159265358979323846264338_wp

END MODULE parameters_m