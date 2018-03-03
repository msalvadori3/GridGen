!> \file: thomas.F90
!> \author: Marc Salvadori

MODULE thomas_m

      USE parameters_M,ONLY:wp
      IMPLICIT NONE

CONTAINS 


      SUBROUTINE SY(IL,IU,BB,DD,AA,CC)

              IMPLICIT NONE

              INTEGER,INTENT(IN) :: IL,IU
              REAL(KIND=wp),DIMENSION(IL:IU),INTENT(IN) :: AA,BB
              REAL(KIND=wp),DIMENSION(IL:IU),INTENT(INOUT) :: CC,DD


              !IL = SUBSCRIPT OF FIRT EQUATION
              !IU = SUBSCRIPT OF LAST EQUATION
              !BB = COEFFICIENT BEHIND DIAGONAL
              !DD = COEFFICIENT ON DIAGONAL
              !AA = COEFFICIENT AHEAD OF DIAGONAL
              !CC = ELEMENT OF CONSTANT VECTOR

              INTEGER :: LP,I,J
              REAL(KIND=wp) :: R

              LP = IL+1

              DO I = LP,IU

                  R = BB(I)/DD(I-1)
                  DD(I) = DD(I) - R*AA(I-1)
                  CC(I) = CC(I) - R*CC(I-1)
              ENDDO

              !BACK SUBSTITUTION

              CC(IU) = CC(IU)/DD(IU)

              DO I = LP,IU

                  J = IU - I +IL
                  CC(J) = (CC(J) - AA(J)*CC(J+1))/DD(J)

              ENDDO


              !SOLUTION STORED IN CC

       END SUBROUTINE SY


END MODULE thomas_m
