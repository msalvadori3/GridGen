!> \file: io.F90
!> \author: Marc Salvadori
!> \brief: This module provides routines to read input

MODULE io_m

      USE xml_data_input_file
      IMPLICIT NONE

CONTAINS

      SUBROUTINE ReadInput(inputData)

              USE SimVars_m, ONLY: imax,jmax,xblk,Cy,nmax,&
                      Geopts,FEsize,DCsize,Geosize,iControl,&
                      IOunit,fileLength,prjTitle


              IMPLICIT NONE

              TYPE(input_type_t),INTENT(OUT) :: inputData
              LOGICAL :: fexist
              INTEGER :: ierror,k
              CHARACTER(LEN=64) :: buffer,curDir
              CHARACTER(LEN=64),DIMENSION(:),ALLOCATABLE :: key_list

              ! Define keywords in the path.dat file

              ALLOCATE(key_list(1))
              key_list(1) = 'CURRENT_DIR'

              ! Check if path.dat exist
              INQUIRE(FILE='path.dat',EXIST=fexist)

              IF (fexist) THEN

                      OPEN(UNIT=1,FILE='path.dat',STATUS='OLD',ACTION='READ',&
                              IOSTAT=ierror)
                      DO k =1,SIZE(key_list)
                        READ(1,IOSTAT=ierror,FMT="(A)") buffer
                        IF (ierror == 0) THEN
                                IF (buffer == TRIM(key_list(k))) THEN
                                        READ(1,IOSTAT=ierror,FMT="(A)") curDir
                                ELSE
                                        WRITE(*,*) ' Not able to read the information from'
                                        WRITE(*,*) 'path.dat file after the keyword:'
                                        WRITE(*,*) key_list(k)
                                        EXIT
                                ENDIF
                        ELSE
                                WRITE(*,*) 'Not able to read the following information from'
                                WRITE(*,*) ' path.dat file'
                                WRITE(*,*) key_list(k)
                                EXIT
                        ENDIF
                      ENDDO
                      CLOSE(1)

              ELSE
                      WRITE(*,*) 'File path.dat not found. Please run:'
                      WRITE(*,*) './setup.py -u set_path'
                      STOP
              ENDIF
              DEALLOCATE(key_list)

              
              CALL read_xml_file_input_file('input_file.xml')
              inputData = input_data
              
              prjTitle = inputData%setup%Project
              imax = inputData%geometry%imax
              jmax = inputData%geometry%jmax

              xblk(1,1) = inputData%geometry%x1
              xblk(2,1) = inputData%geometry%y1
              xblk(1,2) = inputData%geometry%x2
              xblk(2,2) = inputData%geometry%y2
              xblk(1,3) = inputData%geometry%x3
              xblk(2,3) = inputData%geometry%y3
              xblk(1,4) = inputData%geometry%x4
              xblk(2,4) = inputData%geometry%y4
              Geopts(1,1) = inputData%geometry%Geoptx1
              Geopts(2,1) = inputData%geometry%Geopty1
              Geopts(1,2) = inputData%geometry%Geoptx2
              Geopts(2,2) = inputData%geometry%Geopty2

              FEsize = inputData%geometry%FEsize
              DCsize = inputData%geometry%DCsize
              Geosize = inputData%geometry%Geosize

              iControl = inputData%setup%iControl
              Cy = inputData%setup%Cy
              nmax = inputData%setup%nmax

              WRITE(*,*) '#############################################'
              WRITE(*,'(4A)') 'Project Title:', '"',TRIM(prjTitle),'"'
              WRITE(*,*) '#############################################'

        END SUBROUTINE ReadInput


END MODULE io_m







