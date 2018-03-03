!> /file GridTransform.F90
!> /author Marc Salvadori

MODULE GridTransform_m

      USE parameters_m,ONLY: wp
      USE SimVars_m,ONLY: imax,jmax,xp,Cy
      USE thomas_m,ONLY: SY

      IMPLICIT NONE

      REAL(KIND=wp), ALLOCATABLE,DIMENSION(:,:) :: alpha,beta,gm
      REAL(KIND=wp),ALLOCATABLE,DIMENSION(:,:) :: phi,psi


      CONTAINS

              SUBROUTINE InitArrays()

                      IMPLICIT NONE

                      ALLOCATE(alpha(imax,jmax))
                      ALLOCATE(beta(imax,jmax))
                      ALLOCATE(gm(imax,jmax))

                      ALLOCATE(phi(imax,jmax))
                      ALLOCATE(psi(imax,jmax))

                      alpha = 0.0_wp
                      beta = 0.0_wp
                      gm = 0.0_wp

                      phi = 0.0_wp
                      psi = 0.0_wp


              END SUBROUTINE InitArrays

              SUBROUTINE EndArrays()

                      IMPLICIT NONE

                      DEALLOCATE(alpha)
                      DEALLOCATE(beta)
                      DEALLOCATE(gm)
                      DEALLOCATE(phi)
                      DEALLOCATE(psi)

              END SUBROUTINE EndArrays


              SUBROUTINE CalcCoeff()

                      IMPLICIT NONE

                      ! Evaluate the following coefficients:
                      ! Alpha = (x_eta)^2 + (y_eta)^2
                      ! Beta = (x_xi*x_eta) + (y_xi*y_eta)
                      ! Gamma = (x_xi)^2 + (y_xi)^2

                      !NOTE: xi represents the x-axis in the logical domain
                      !      eta represents the y-axis in the logical domain


                      INTEGER :: i,j
                      REAL(KIND=wp) ::  x_eta,x_xi,y_eta,y_xi

                      DO i =2 ,imax-1
                        DO j = 2,jmax-1

                            x_eta = 0.5_wp*(xp(1,i,j+1) - xp(1,i,j-1))
                            x_xi = 0.5_wp*(xp(1,i+1,j) - xp(1,i-1,j))
                            y_eta =0.5_wp*(xp(2,i,j+1) - xp(2,i,j-1))
                            y_xi = 0.5_wp*(xp(2,i+1,j) - xp(2,i-1,j))

                            alpha(i,j) = x_eta**2 + y_eta**2
                            beta(i,j) = x_xi*x_eta + y_xi*y_eta
                            gm(i,j) = x_xi**2 + y_xi**2

                        ENDDO
                      ENDDO 


              END SUBROUTINE CalcCoeff

              SUBROUTINE PhiPsi()

                      USE GridSetup_m,ONLY:Uniform,Stretching

                      IMPLICIT NONE

                      ! In this subroutine we calculate the boundary conditions for PHI and PSI

                      INTEGER :: i,j
                      REAL(KIND=wp) :: x_eta,x_xi,y_xi,y_eta,&
                              x_xixi,y_xixi,x_etaeta,y_etaeta

                      !WRITE(*,'(A)') 'Calculating Phi and Psi for contorlling elliptic grid.......'

                      ! Evaluate Psi at the boundary i=1, and i=imax
                      DO i=1,imax,imax-1
                        DO j = 2,jmax-1
                          
                          x_eta = 0.5_wp*(xp(1,i,j+1) - xp(1,i,j-1))
                          y_eta = 0.5_wp*(xp(2,i,j+1) - xp(2,i,j-1))

                          x_etaeta = xp(1,i,j+1) - 2.0_wp*xp(1,i,j) + xp(1,i,j-1)
                          y_etaeta = xp(2,i,j+1) - 2.0_wp*xp(2,i,j) + xp(2,i,j-1)

                          IF (ABS(x_eta)>ABS(y_eta)) THEN

                                  psi(i,j) = -x_etaeta/x_eta
                          ELSE
                                  psi(i,j) = -y_etaeta/y_eta

                          ENDIF

                       ENDDO
                     ENDDO

                     ! Evaluate Phi at the boundary j=1, and j=jmax
                      DO j=1,jmax,jmax-1
                        DO i = 2,imax-1
                          
                          x_xi = 0.5_wp*(xp(1,i+1,j) - xp(1,i-1,j))
                          y_xi = 0.5_wp*(xp(2,i+1,j) - xp(2,i-1,j))

                          x_xixi = xp(1,i+1,j) - 2.0_wp*xp(1,i,j) + xp(1,i-1,j)
                          y_xixi = xp(2,i+1,j) - 2.0_wp*xp(2,i,j) + xp(2,i-1,j)

                          IF (ABS(x_xi)>ABS(y_xi)) THEN

                                  phi(i,j) = -x_xixi/x_xi
                          ELSE
                                  phi(i,j) = -y_xixi/y_xi

                          ENDIF

                       ENDDO
                     ENDDO


                     ! Evaluate Phi and Psi at the interior points

                     DO i=2,imax-1
                      DO j=2,jmax-1

                        ! Initial distribution of Psi and Phi
                        !psi(i,j) = Uniform(psi(1,j),psi(imax,j),i,imax)
                        !phi(i,j) = Uniform(phi(i,1),phi(i,jmax),j,jmax)

                        ! Modified distribution of Psi and Phi for Grid#5
                        psi(i,j) = Stretching(psi(1,j),psi(imax,j),i,imax,0.001_wp)
                        phi(i,j) = Stretching(phi(i,1),phi(i,jmax),j,jmax,-11.0_wp)

                     ENDDO
                    ENDDO

              END SUBROUTINE PhiPsi

              SUBROUTINE TriDiag(RMS)

                      USE SimVars_m,ONLY:xp,imax,jmax
                      ! This routine solves a tridiagonal matrix using Thomas Alogirithm.

                      IMPLICIT NONE

                      REAL(KIND=wp),DIMENSION(imax) :: a,b,c,d
                      REAL(KIND=wp),INTENT(OUT) :: RMS
                      REAL(KIND=wp) :: x_xieta,x_eta,y_xieta,y_eta
                      INTEGER :: n

                      INTEGER :: i,j

                      RMS = 0.0_wp
                      n = 0

                      DO j = 2,jmax-1

                        DO i = 1,imax

                          IF (i==1 .OR. i ==imax) THEN

                                  a(i) = 0.0_wp
                                  b(i) = 1.0_wp
                                  c(i) = 0.0_wp
                                  d(i) = xp(1,i,j)

                          ELSE
                                  a(i) = alpha(i,j)*(1.0_wp - 0.5_wp*phi(i,j))
                                  b(i) = -2.0_wp*(alpha(i,j) + gm(i,j))
                                  c(i) = alpha(i,j)*(1.0_wp + 0.5_wp*phi(i,j))
                                  x_eta = 0.5_wp*(xp(1,i,j+1) - xp(1,i,j-1))
                                  x_xieta = 0.25_wp*(xp(1,i+1,j+1) - xp(1,i+1,j-1) - &
                                          xp(1,i-1,j+1) + xp(1,i-1,j-1))
                                  d(i) = 2.0_wp*beta(i,j)*x_xieta - gm(i,j)*( &
                                          xp(1,i,j+1) + xp(1,i,j-1) + psi(i,j)*x_eta)
                          ENDIF
                        ENDDO


                        ! CALLING THOMAS ALGORITHM
                        CALL SY(1,imax,a,b,c,d)

                        DO i = 1,imax

                          RMS = RMS+(d(i)-xp(1,i,j))*(d(i)-xp(1,i,j))
                          n = n+1
                          xp(1,i,j) = d(i)

                        ENDDO


                        DO i = 1,imax

                          IF (i==1 .OR. i ==imax) THEN

                                  a(i) = 0.0_wp
                                  b(i) = 1.0_wp
                                  c(i) = 0.0_wp
                                  d(i) = xp(2,i,j)

                          ELSE
                                  a(i) = alpha(i,j)*(1.0_wp - 0.5_wp*phi(i,j))
                                  b(i) = -2.0_wp*(alpha(i,j) + gm(i,j))
                                  c(i) = alpha(i,j)*(1.0_wp + 0.5_wp*phi(i,j))
                                  y_eta = 0.5_wp*(xp(2,i,j+1) - xp(2,i,j-1))
                                  y_xieta = 0.25_wp*(xp(2,i+1,j+1) - xp(2,i+1,j-1)-&
                                          xp(2,i-1,j+1) + xp(2,i-1,j-1))
                                  d(i) = 2.0_wp*beta(i,j)*y_xieta - gm(i,j)*(&
                                          xp(2,i,j+1) + xp(2,i,j-1) + psi(i,j)*y_eta)
                          ENDIF
                        ENDDO

                        ! CALLING THOMAS ALGORITHM
                        CALL SY(1,imax,a,b,c,d)

                        DO i = 1,imax

                            RMS = RMS +(d(i)-xp(2,i,j))*(d(i)-xp(2,i,j))
                            n = n+1
                            xp(2,i,j) = d(i)
              
                        ENDDO
                      ENDDO


                        RMS = (RMS/n)**(0.5_wp)


                   END SUBROUTINE TriDiag

                    SUBROUTINE Jacobian()

                            USE SimVars_m,ONLY:xp,Jinverse,imax,jmax

                            IMPLICIT NONE

                            INTEGER :: i,j

                            REAL(KIND=wp) :: x_xi,y_xi,x_eta,y_eta
                            ! Temporary ghost cells for calculations of partial derivatives
                            REAL(KIND=wp) :: x_g,y_g

                            DO i =1,imax
                              DO j = 1,jmax

                                ! Calculate x_xi,y_xi

                                IF ( i == 1) THEN

                                  x_g =  xp(1,i,j) - (xp(1,i+1,j)-xp(1,i,j))
                                  y_g = xp(2,i,j) - (xp(2,i+1,j)-xp(2,i,j))

                                  x_xi = 0.5_wp*(xp(1,i+1,j)-x_g)
                                  y_xi = 0.5_wp*(xp(2,i+1,j)-y_g)

                                ELSE IF (i == imax) THEN

                                  x_g =  xp(1,i,j) + (xp(1,i,j)-xp(1,i-1,j))
                                  y_g = xp(2,i,j) + (xp(2,i,j)-xp(2,i-1,j))

                                  x_xi = 0.5_wp*(x_g - xp(1,i-1,j))
                                  y_xi = 0.5_wp*(y_g -xp(2,i-1,j))

                                ELSE

                                  x_xi = 0.5_wp*(xp(1,i+1,j)-xp(1,i-1,j))
                                  y_xi = 0.5_wp*(xp(2,i+1,j)-xp(2,i-1,j))

                               END IF

                               ! Calculate x_eta,y_eta

                                IF ( j == 1) THEN

                                  x_g =  xp(1,i,j) - (xp(1,i,j+1)-xp(1,i,j))
                                  y_g = xp(2,i,j) - (xp(2,i,j+1)-xp(2,i,j))

                                  x_eta = 0.5_wp*(xp(1,i,j+1)-x_g)
                                  y_eta = 0.5_wp*(xp(2,i,j+1)-y_g)

                                ELSE IF (j == jmax) THEN

                                  x_g =  xp(1,i,j) + (xp(1,i,j)-xp(1,i,j-1))
                                  y_g = xp(2,i,j) + (xp(2,i,j)-xp(2,i,j-1))

                                  x_eta = 0.5_wp*(x_g - xp(1,i,j-1))
                                  y_eta = 0.5_wp*(y_g -xp(2,i,j-1))

                                ELSE

                                  x_eta = 0.5_wp*(xp(1,i,j+1)-xp(1,i,j-1))
                                  y_eta = 0.5_wp*(xp(2,i,j+1)-xp(2,i,j-1))

                               END IF


                               ! Inverse of Jacobian I = 1/J
                               Jinverse(i,j) = x_xi*y_eta - x_eta*y_xi

                             ENDDO
                           ENDDO

                    END SUBROUTINE Jacobian

END MODULE