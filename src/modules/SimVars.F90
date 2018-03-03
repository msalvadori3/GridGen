!> \file: SimVars.F90
!> \author: Marc Salvadori

MODULE SimVars_m

      USE parameters_m,ONLY :wp
      IMPLICIT NONE
      
      !Obtain data from XML input file
      REAL(KIND=wp), DIMENSION(2,2) ::Geopts !Geometrypoints([x,y],[start,end])

      ! Size of bottom portion of domain as provided in the project description
      INTEGER :: FEsize,DCsize,Geosize

      ! Control factor for evaluating elliptic grid
      INTEGER :: iControl

      ! IO parameters
      INTEGER, PARAMETER :: IOunit = 10, fileLength = 64
      CHARACTER(LEN=64) :: prjTitle


      ! imax: number of grid points in i-direction
      ! jmax: number of grid points in j-direction
      INTEGER :: imax,jmax

      ! Number of maximum iterations 
      INTEGER :: nmax


      ! Stretching factor
      REAL(KIND=wp) :: Cy
      
      ! xp(2,imax,jmax) = Coordinates in physical space
      REAL(KIND=wp),ALLOCATABLE,DIMENSION(:,:,:) :: xp

      ! Bottom portion of the domain where the airfoil surface lies
      REAL(KIND=wp),ALLOCATABLE,DIMENSION(:,:) :: Bottom

      ! Inverse of Jacobian matrix
      REAL(KIND=wp),ALLOCATABLE,DIMENSION(:,:) :: Jinverse

      ! x,y points at 4 vertices of domain
      REAL(KIND=wp),DIMENSION(2,4) :: xblk   

      ! RMS Residula variables
      REAL(KIND=wp) :: rms

      ! Time Variables
      INTEGER :: c1,c2,cr
      REAL(KIND=wp) :: elapse_time
      REAL(KIND=wp) :: rate


END MODULE SimVars_m