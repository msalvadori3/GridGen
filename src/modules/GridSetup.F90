!> \file: GridSetup.F90
!> \author: Marc Salvadori

MODULE GridSetup_m

      USE parameters_m,ONLY: wp
      
      IMPLICIT NONE

CONTAINS

      SUBROUTINE InitGrid(inputData)

              USE xml_data_input_file
              USE io_m, ONLY: ReadInput

              IMPLICIT NONE

              TYPE(input_type_t) :: inputData

              CALL ReadInput(inputData)
              CALL InitVars()
              CALL BottomEdge()
              CALL SetBCs()
              CALL GridInternal()

      END SUBROUTINE InitGrid

      SUBROUTINE InitVars()

              USE SimVars_m, ONLY: imax,jmax,xp,Jinverse

              IMPLICIT NONE

              !WRITE(*,'(A)') "INITIALIZING ARRAYS FOR SIMULATION"
              ALLOCATE(xp(2,imax,jmax))
              ALLOCATE(Jinverse(imax,jmax))

              xp = 0.0_wp
              Jinverse = 0.0_wp

      END SUBROUTINE InitVars

      SUBROUTINE EndVars()

              USE SimVars_m,ONLY:xp,Jinverse

              IMPLICIT NONE

              DEALLOCATE(xp)
              DEALLOCATE(Jinverse)

      END SUBROUTINE EndVars

      SUBROUTINE BottomEdge()

              USE SimVars_m, ONLY: FEsize,DCsize,Geosize,Geopts
              USE SimVars_m,ONLY: imax,jmax,xblk,Cy,Bottom
              
              IMPLICIT NONE

              INTEGER :: i

              ALLOCATE(Bottom(2,imax))

              !WRITE(*,'(A)') "Creating bottom portion of domain using &
              !                Airfoil geometry"

              ! FE SEGMENT
              DO i = 2,FEsize

                    ! New method for Grid#5
                    Bottom(1,i) = Stretching(xblk(1,1),Geopts(1,1),i,FEsize,-1.0_wp)

                    !Initial method of interpolation for Grids#1-4
                    !!Bottom(1,i) = Uniform(xblk(1,1),Geopts(1,1),i,FEsize)
                    Bottom(2,i) = Stretching(xblk(2,1),Geopts(2,1),i,FEsize,Cy)
              ENDDO

              ! ED SEGMENT

              DO i = FEsize+1,FEsize+Geosize-1

                    ! New method for Grid#5
                    Bottom(1,i) = Stretching(Geopts(1,1),Geopts(1,2),i-FEsize+1,Geosize,1.0_wp)

                    !Initial method of interpolation for Grids#1-4
                    !!Bottom(1,i) = Uniform(Geopts(1,1),Geopts(1,2),i-FEsize+1,Geosize)
                    Bottom(2,i) = naca(Bottom(1,i))
              ENDDO

              ! DC SEGMENT

              DO i = FEsize + Geosize,imax-1

                    !New method for GRid#5
                    Bottom(1,i) = Stretching(Geopts(1,2),xblk(1,2),i-FEsize-Geosize+2,&
                                    DCsize,0.001_wp)

                    !Initial method of interpolation for Grids#1-4
                    !!Bottom(1,i) = Uniform(Geopts(1,2),xblk(1,2),i-FEsize-Geosize+2,DCsize)
                    Bottom(2,i) = Stretching(Geopts(2,2),xblk(2,2),i-FEsize-Geosize+2,&
                                    DCsize,Cy)
              ENDDO

      END SUBROUTINE BottomEdge

      SUBROUTINE SetBCs()

              USE SimVars_m,ONLY:imax,jmax,xp,xblk,Bottom,Cy
              
              IMPLICIT NONE

              INTEGER :: i,j


              !WRITE(*,'(A)') "Setting Boundary Conditions...."

              !+++++++++++++++++++++++++++++++++++++++++++++++++++!
              ! Assign coordinates value in xblk(4,2)             !
              ! Below shows 4 vertices defined in one single block!
              !                                                   !
              !      3--------------4    y                        !
              !      |              |    |                        !
              !      |              |    |                        !
              !      |              |    --- x                    !
              !      |              |                             !
              !      1--------------2                             !
              !                                                   !
              !+++++++++++++++++++++++++++++++++++++++++++++++++++!

              DO i =1,2
                   
                  ! Vertex 1
                  xp(i,1,1) = xblk(i,1)

                  ! Vertex 2
                  xp(i,imax,1) = xblk(i,2)

                  ! Vertex 3
                  xp(i,1,jmax) = xblk(i,3)

                  ! Vertex 4
                  xp(i,imax,jmax) = xblk(i,4)

              ENDDO

              !+++++++++++++++++++++++++++++++++++++++++++++++++++++!
              ! Assign coordinates value at every edge of the domain!
              !                                                     !
              !      +------(4)-----+    y                          !
              !      |              |    |                          !
              !      |              |    |                          !
              !     (1)            (2)    --- x                     !
              !      |              |                               !
              !      +------(3)-----+                               !
              !                                                     !
              !+++++++++++++++++++++++++++++++++++++++++++++++++++++!

              ! Edge (1)

              DO j=2,jmax-1

                  xp(1,1,j) = Uniform(xblk(1,1),xblk(1,3),j,jmax)
                  xp(2,1,j) = Stretching(xblk(2,1),xblk(2,3),j,jmax,Cy)

              ENDDO

              ! Edge (2)

              DO j=2,jmax-1

                  xp(1,imax,j) = Uniform(xblk(1,2),xblk(1,4),j,jmax)
                  xp(2,imax,j) = Stretching(xblk(2,2),xblk(2,4),j,jmax,Cy)

              ENDDO

              ! Edge (3)

              DO i=2,imax-1

                  xp(1,i,1) = Bottom(1,i)
                  xp(2,i,1) = Bottom(2,i)
                  !xp(1,i,1) = Uniform(xblk(1,1),xblk(1,2),i,imax)
                  !xp(2,i,1) = Uniform(xblk(2,1),xblk(2,2),i,imax)

              ENDDO

              ! Edge (4)

              DO i=2,imax-1

                  xp(1,i,jmax) = Uniform(xblk(1,3),xblk(1,4),i,imax)
                  xp(2,i,jmax) = Uniform(xblk(2,3),xblk(2,4),i,imax)

              ENDDO

      END SUBROUTINE SetBCs

      SUBROUTINE GridInternal()

              USE SimVars_m, ONLY:imax,jmax,xp,xblk,Cy
              
              IMPLICIT NONE

              INTEGER :: i,j,k

              !WRITE(*,'(A)') "Writing Internal Points......"


              !+++++++++++++++++++++++++++++++++++++++++
              ! "front plane"
              !     +------------+
              !     |            |   y(j)
              !     | i-j plane  |    |
              !     |            |    |
              !     1------------+    ---- x(i)
              !
              !j=jmax @---@---@---@---@
              !       |   |   |   |   |  @: edge points (known)
              !       @---o---o---o---@  o: interior points (unknown)
              !       |   |   |   |   |
              !       @---o---o---o---@
              !       |   |   |   |   |
              !   j=1 @---@---@---@---@
              !      i=1             i=imax
              ! x-coordinate is determined along the i=const lines
              ! y-coordinate is same as y of corner (1)
              !+++++++++++++++++++++++++++++++++++++++++



              DO i= 2, imax-1
                  DO j=2,jmax-1

                      xp(1,i,j) = Uniform(xp(1,i,1),xp(1,i,jmax),j,jmax)
                      xp(2,i,j) = Stretching(xp(2,i,1),xp(2,i,jmax),j,jmax,Cy)

                  ENDDO
              ENDDO

      END SUBROUTINE GridInternal


      FUNCTION naca(x) RESULT(y)

              IMPLICIT NONE

              REAL(KIND=wp) xint, thick, x,y

              xint = 1.008930411365_wp

              thick = 0.15_wp

              y = 0.2969_wp * sqrt(xint * x) - 0.126_wp * xint * x - 0.3516_wp * &
                      (xint * x)**2 + 0.2843_wp * (xint * x)**3 - 0.1015_wp * (xint * x)**4

              y = 5.0_wp*thick*y


      END FUNCTION naca

      FUNCTION Uniform(xmin,xmax,j,jmax) RESULT(output)

              !Distribute interior grid points based on edge points' coordinates.
              !Linear Interpolateion is made by referring to (i,j) indices

              IMPLICIT NONE

              REAL(KIND=wp),INTENT(IN) :: xmin,xmax
              INTEGER,INTENT(IN) :: j,jmax
              REAL(KIND=wp) :: output,A

              A = REAL(j - 1) / REAL(jmax - 1)

              output = xmin + A*(xmax-xmin)

       END FUNCTION Uniform

       FUNCTION Stretching(xmin,xmax,j,jmax,Cy) RESULT(output)

               !Distribute interior grid points based on stretching coefficient
               !Interpolateion is made by referring to (i,j) indices

               IMPLICIT NONE

               REAL(KIND=wp),INTENT(IN) :: Cy
               REAL(KIND=wp),INTENT(IN) :: xmin,xmax
               INTEGER,INTENT(IN) :: j,jmax
               REAL(KIND=wp) :: output,A

               A = LOG(1.0_wp + (exp(-Cy) - 1.0_wp) * REAL(j - 1) / REAL(jmax - 1))

               output = xmin - A*(xmax-xmin)/Cy

        END FUNCTION Stretching


END MODULE GridSetup_m
