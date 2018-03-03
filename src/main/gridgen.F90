!> \file:gridgen.F90
!> \author: Marc Salvadori

PROGRAM main
     
      USE xml_data_input_file 
      USE GridSetup_m,ONLY:InitGrid,GridInternal,EndVars
      USE parameters_m,ONLY:wp
      USE SimVars_m,ONLY:fileLength,rms,c1,c2,cr,elapse_time,rate
      USE GridTransform_m,ONLY: InitArrays,CalcCoeff,PhiPsi,TriDiag,Jacobian,&
              EndArrays
      USE output_m,ONLY:WritePlotFile,WriteRMS

      IMPLICIT NONE

      TYPE(input_type_t) :: inputData
      CHARACTER(LEN=fileLength) ::output= 'Grid5'
      CHARACTER(LEN=fileLength) :: rmsout = 'rms5'
      INTEGER :: i

      ! Start the time measurements
      elapse_time = 0.0_wp

      CALL system_clock(count_rate=cr)
      rate = REAL(cr)
      CALL system_clock(c1)

      CALL InitGrid(inputData)
      CALL InitArrays
      CALL Jacobian
      WRITE(*,*) 'Writing Out Intial Algebraic Grid...............'
      CALL WritePlotFile('InitGrid','"x","y","Jacobian","Phi","Psi"',inputData)
      IF (inputData%setup%iControl == 1) CALL PhiPsi
      WRITE(*,*) 'Solving Elliptical Grid...............'
      DO i=1,inputData%setup%nmax
                WRITE(*,*) 'IT = ',i
                CALL CalcCoeff
                CALL TriDiag(rms)
                CALL WriteRMS(i,rms,rmsout,inputData)
                IF (rms <= inputData%setup%RMSres) EXIT
      ENDDO
      CALL Jacobian
      CALL WritePlotFile(output,'"x","y","Jacobian","Phi","Psi"',inputData)
      CALL EndArrays
      CALL EndVars

      WRITE(*,*) '###########################################'
      WRITE(*,'(4A)') 'Simulation Complete!'
      WRITE(*,*) '###########################################'

      CALL system_clock(c2)

      elapse_time = REAL(c2-c1,KIND=wp)/rate

      WRITE(*,*) ""
      WRITE(*,'(A,F10.6,A)') "| Total elapse time: ",elapse_time, " [s]|"

END PROGRAM main