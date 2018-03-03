!> \file: output.F90
!> \author: Marc Salvadori
!> \brief: This module provides routines to output simulation data

MODULE output_m

      USE xml_data_input_file
      USE parameters_m,ONLY:wp
      USE SimVars_m,ONLY:imax,jmax,xp,Jinverse
      USE SimVars_m,ONLY: IOunit,fileLength
      USE GridTransform_m,ONLY:phi,psi
      USE stringUtils_m

      IMPLICIT NONE

CONTAINS


      SUBROUTINE WritePlotFile(fileName,varNames,inputData)

              IMPLICIT NONE
              
              TYPE(input_type_t),INTENT(IN) :: inputData
              CHARACTER(LEN=*),INTENT(IN) :: fileName
              CHARACTER(LEN=*),INTENT(IN) :: varNames

              ! Local variables
              CHARACTER(LEN=fileLength) :: file_name1,prjTitle
              INTEGER :: i,j,ierror


              prjTitle = inputData%setup%Project

              IF (fileName == 'None' .OR. fileName == 'none') THEN
                      file_name1 = 'output/' // 'Grid_Cy_'//&
                              num2str(inputData%setup%Cy,'(F5.3)',5)//&
                              '_iControl_'//num2str(REAL(inputData%setup%iControl,8),&
                              '(F3.1)',5)
              ELSE
                      file_name1 = 'output/'//TRIM(fileName)//'_Cy_'//&
                              num2str(inputData%setup%Cy,'(F5.3)',5)//&
                              '_iControl_'//num2str(REAL(inputData%setup%iControl,8),&
                              '(F3.1)',5)
              ENDIF

              IF (inputData%PostProcessing%plot%iPost == 1 .AND. &
                      inputData%PostProcessing%plot%Method == 'TecPlot') THEN

                      WRITE(*,*) 'Printing Results for TecPlot .................'

                      OPEN(IOunit,FILE=TRIM(file_name1)//'.tec',FORM='FORMATTED',&
                                    ACTION='WRITE',STATUS='REPLACE',IOSTAT=ierror)
                      ! Writing TecPLot headers

                       WRITE(IOunit,'(A)') 'Title="' // TRIM(prjTitle) // '"'
                       WRITE(IOunit,'(A)') 'Variables=' // TRIM(varNames) 

                      WRITE(IOunit,'(A)') ""
                      WRITE(IOunit,'(A,I6,A,I6,A)') 'Zone I=', imax, ', J=',&
                                                           jmax, ', F=POINT'

                      DO j = 1, jmax
                        DO i = 1, imax
                          WRITE(IOunit,'(6g15.6)') xp(1,i,j), xp(2,i,j),Jinverse(i,j),psi(i,j),phi(i,j)
                        ENDDO
                      ENDDO
                      
                      CLOSE(IOunit)
              ELSE IF (inputData%PostProcessing%plot%iPost == 1 .AND. &
                      inputData%PostProcessing%plot%Method == 'Python') THEN

                      WRITE(*,*) 'Priting Results for Python ...................'
                      WRITE(*,*) 'Variables printing :'//TRIM(varNames)

                      OPEN(IOunit,FILE=TRIM(file_name1)//'.dat',FORM='FORMATTED',&
                              ACTION='WRITE',STATUS='REPLACE',IOSTAT=ierror)

                       DO j = 1, jmax
                        DO i = 1, imax
                          WRITE(IOunit,'(6g15.6)') xp(1,i,j), xp(2,i,j),Jinverse(i,j),psi(i,j),phi(i,j)
                        ENDDO
                      ENDDO

                      CLOSE(IOunit)

              ELSE

                      WRITE(*,*) 'Something went wrong in the generation of the'
                      WRITE(*,*) 'output file.',ierror
                      WRITE(*,*) 'Check that you have the output folder'
                      WRITE(*,*) 'in the main directory, otherwise run'
                      WRITE(*,*) ' ./setup.py -u create_directories'

              ENDIF

      END SUBROUTINE WritePlotFile

      SUBROUTINE WriteRMS(niter,rms,fileName,inputData)

              ! Write out RMS and number of iteration for Thomas loop
              ! This subroutine was done in such a  way I can append
              ! the data while running the nmax loop for Thomas Algorithm

              IMPLICIT NONE


              TYPE(input_type_t),INTENT(IN) :: inputData
              CHARACTER(LEN=*),INTENT(IN) :: fileName
              REAL(KIND=wp),INTENT(IN) :: rms
              INTEGER,INTENT(IN) :: niter

              CHARACTER(LEN=fileLength) :: file_name1



              IF (fileName == 'None' .OR. fileName == 'none') THEN
                      file_name1 = 'output/' // 'RMSout_'//&
                              num2str(inputData%setup%Cy,'(F5.3)',5)//&
                              '_iControl_'//num2str(REAL(inputData%setup%iControl,8),&
                              '(F3.1)',5)

              ELSE
                      file_name1 = 'output/'//TRIM(fileName)//'_Cy_'//&
                              num2str(inputData%setup%Cy,'(F5.3)',5)//&
                              '_iControl_'//num2str(REAL(inputData%setup%iControl,8),&
                              '(F3.1)',5)

              ENDIF

              IF (niter == 1) THEN

                      OPEN(IOunit,FILE=TRIM(file_name1)//'.dat',FORM='FORMATTED',ACTION='WRITE')

              ELSE

                      OPEN(IOunit,FILE=TRIM(file_name1)//'.dat',FORM='FORMATTED',ACTION='WRITE',&
                              ACCESS='APPEND')
              ENDIF

              WRITE(IOunit,'(I6,G15.6)')niter,rms
              CLOSE(IOunit)

      END SUBROUTINE WriteRMS



END MODULE output_m