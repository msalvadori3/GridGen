Code development
================

The current project is for developing elliptic grid generator in 2-dimensional domain. Hereafter, the program developed in this project is called 'GridGen'.


GridGen Code summary
--------------------

The present project is to make a grid-generator for 2-D computational domain around a modified NACA 00xx series airfoil in a channel.

The source code contains the following directories:

* io - input/output related routines
* main - main program driver
* math - thomas algorithm 
* modules - main grid generator solver routines
* utils - list of useful FORTRAN utilities used within the program
* gridpy - python wrapper for gridgen main program

 Also a 'CMakeLists.txt' file is also included for cmake compiling.

::

   $ cd GridGen/src/
   $ ls
   $ CMakeLists.txt  io  main math modules utils gridpy

The **io** folder has **io.F90** file which contains **ReadInput(inputData)** subroutine. It also includes **input_file_xml** which describes the structure of the user run-time input file located in the main 'src' directory, and **output.F90** for storing data in bothb Tecplot and Python format.

The **main** folder is only used for containing the code driver file. The main routines is run by **gridgen.F90** which calls important subroutines from the rest of folders.

Details of GridGen development
------------------------------

The GridGen code is made for creating 2-D computational domain with pre-described points value along the 2D airfoil geometry. The schematic below shows the flow chart of how the GridGen code runs. 

The source code shown below is **gridgen.F90** and it calls skeletal subroutines for generating grid structure. The main features of the main code is to (1) read input file, (2) make initialized variable arrays, (3) set initial algebraic grid points, (4) create elliptic grid points, and (5) finally write output files::

        PROGRAM main
             
              USE xml_data_input_file 
              USE GridSetup_m,ONLY:InitGrid,GridInternal,EndVars
              USE parameters_m,ONLY:wp
              USE SimVars_m,ONLY:fileLength
              USE GridTransform_m,ONLY: InitArrays,CalcCoeff,PhiPsi,TriDiag,Jacobian,&
                      EndArrays
              USE output_m,ONLY:WritePlotFile,WriteRMS

              IMPLICIT NONE

              TYPE(input_type_t) :: inputData
              CHARACTER(LEN=fileLength) ::output= 'none'
              CHARACTER(LEN=fileLength) :: rmsout = 'rmslog.dat'
              INTEGER :: i
              REAL(KIND=wp) :: rms

              CALL InitGrid(inputData)
              CALL InitArrays
              CALL Jacobian
              CALL WritePlotFile('InitGrid','"x","y","Jacobian","Phi","Psi"',inputData)
              IF (inputData%setup%iControl == 1) CALL PhiPsi
              WRITE(*,*) 'Solving Elliptical Grid...............'
              DO i=1,inputData%setup%nmax
                        WRITE(*,*) 'IT = ',i
                        CALL CalcCoeff
                        CALL TriDiag(rms)
                        CALL WriteRMS(i,rms,rmsout)
                        IF (rms <= inputData%setup%RMSres) EXIT
              ENDDO
              CALL Jacobian
              CALL WritePlotFile(output,'"x","y","Jacobian","Phi","Psi"',inputData)
              CALL EndArrays
              CALL EndVars

        END PROGRAM main

Creation of algebraic grid points
+++++++++++++++++++++++++++++++++

The code starts to run by reading the important input parameters defined by the user in the **input_file.xml** file. The input data file first contains all the details geometrical description of our grid points. Then the code reads airfoil geometry data from this input file, which provides the bottom edge points of the domain. The input file also contains four vertex points in :math:`(x,y)` coordinates. Thus those points forms a 2-dimensional surface. Based on these boundary grid points, the code runs with Algebratic grid generating subroutine and gives initial conditions for elliptic solution for grid transformation.


The **gridgen.F90** file first refers to **InitGrid** subroutine defined in **GridSetup.F90** file. The main function of this routine is to call again multiple subroutines defined in same file. The subroutine definition shown below summarizes the how the code runs for the grid initialization::

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


* **ReadGridInput**: Reads  user defined variables and parameters for grid configuration.

* **InitVars**: Initialize the single- and multi-dimensional arrays and set their size based on the input parameters.

* **BottomEdge**: Generate point values for airfoil geometry.

* **SetBcs**: Generate grid points along 4 edges of the computational domain.

* **GridInternal**: Based on grid points along the edges and surfaces, this routine will create interior grid points that are aligned with user-defined grid point interpolations.


Creaction of elliptic grid points
+++++++++++++++++++++++++++++++++

In order to determine the elliptic grid points with the pre-specified boundary points, the following Poisson equations, which is given in previous **Project description** section, have to be resolved numerically. The coefficients of the equations can be determined by:

.. math::
   {\alpha}=x_{\eta}^{2} + y_{\eta}^{2}

   {\beta}=x_{\xi}x_{\eta} + y_{\xi}y_{\eta}

   {\gamma}=x_{\xi}^{2} + y_{\xi}^{2}



Then, applying finite difference approximation to the governing equations can be transformed into the linear system of equations. The arranged matrix form of equations shown below can be solved for unknown implicitly at every pseudo-time level. At every time loop, the code updates the coefficients composed of :math:`\phi` and :math:`\psi`, and adjacent points. The detailed relations of each coefficients are not shown here for brevity.

.. math::
   a_{i,j} x_{i-1,j}^{n+1} + b_{i,j} x_{i,j}^{n+1} + c_{i,j} x_{i+1,j}^{n+1} = d_{i,j}

   e_{i,j} y_{i-1,j}^{n+1} + f_{i,j} y_{i,j}^{n+1} + g_{i,j} y_{i+1,j}^{n+1} = h_{i,j}


Above equations can be numerically evaluated by the following descritized expressions:

.. math::
   a_{i,j} = e_{i,j} = {\alpha}_{\text{ }i,j}^{n}  \left(1 - \frac{\phi_{i,j}^{n}}{2} \right)

   b_{i,j} = f_{i,j} = -2 \left({\alpha}_{\text{ }i,j} + {\gamma}_{\text{ }i,j} \right)

   c_{i,j} = g_{i,j} = {\alpha}_{\text{ }i,j}^{n}  \left(1 + \frac{\phi_{i,j}^{n}}{2} \right)

   e_{i,j} = \frac{{\beta}_{\text{ }i,j}^{n}}{2} \left(x_{i+1,j}^{n} - x_{i+1,j-1}^{n+1} - x_{i-1,j+1}^{n} - x_{i-1,j-1}^{n+1} \right) - {\gamma}_{\text{ }i,j}^{n} \left( x_{i,j+1}^{n} + x_{i,j-1}^{n+1} \right) - \frac{{\beta}_{\text{ }i,j}^{n}}{2} \psi_{i,j}^{n} \left( x_{i,j+1}^{n} - x_{i,j-1}^{n+1} \right)

   h_{i,j} = \frac{{\beta}_{\text{ }i,j}^{n}}{2} \left(y_{i+1,j}^{n} - y_{i+1,j-1}^{n+1} - y_{i-1,j+1}^{n} - y_{i-1,j-1}^{n+1} \right) - {\gamma}_{\text{ }i,j}^{n} \left( y_{i,j+1}^{n} + y_{i,j-1}^{n+1} \right) - \frac{{\beta}_{\text{ }i,j}^{n}}{2} \psi_{i,j}^{n} \left( y_{i,j+1}^{n} - y_{i,j-1}^{n+1} \right)

where :math:`n` and :math:`n+1` indicate pseudo time index. Thus above equations will update grid point coordinates for :math:`n+1` time level by referring to already resolved :math:`n` time level solution. Note that the pseudo time looping goes along the successive :math:`j`-constant lines. Therefore, when writing the code, time level index in above equations was not considered as a separate program variable because :math:`j-1` constant line is already updated in the previous loop.

The expressions above are only evaluted in the interior grid points. The points on the boundaries are evaluated seprately by applying given solutions as problem handout.

Once initial algebraic grid points are created, the code is ready to make elliptic grid points with some control terms in terms of :math:`\phi` and :math:`\psi`. **gridgen.F90** file contains the necessary subroutine calls to evaluate the elipptical grid  as shown below::

      IF (inputData%setup%iControl == 1) CALL PhiPsi
      WRITE(*,*) 'Solving Elliptical Grid...............'
      DO i=1,inputData%setup%nmax
                WRITE(*,*) 'IT = ',i
                CALL CalcCoeff
                CALL TriDiag(rms)
                CALL WriteRMS(i,rms,rmsout)
                IF (rms <= inputData%setup%RMSres) EXIT
      ENDDO


Before going into the main loop for solving poisson equations, the code calculate control terms with :math:`\phi` and :math:`\psi` only if the user defines the option in the **input_file.xml** file. Even though the assigned project made an assumption of linear interpolated distribution of :math:`\phi` and :math:`\psi` at interior points, the GridGen code is designed to allow :math:`\phi` and :math:`\psi` be weighted in :math:`j` and :math:`i` directions, respectively. This effect is made by the grid stretching formula.

Here, main DO-loop routine goes with setup of coefficients of governing equations and Thomas loop. The Thomas loop operates with line Gauss-Siedel method for resolving unknown variables, :math:`x` and :math:`y`, with tri-diagonal matrix of coefficients of finite difference approximation equation in a :math:`j` = constant line.


RMS residual
++++++++++++

In order to avoid infinite time-looping for the Thomas method, the GridGen code employs the following definition of RMS residual based on the new (:math:`n+1`) and old(:math:`n`) values of grid point coordinates.

.. math::

   \text{RMS}^{n} = \sqrt{\frac{1}{N} \sum_{i=2}^{imax-1} \sum_{jmax-1}^{j=2} \left[\left(x_{i,j}^{n+1} - x_{i,j}^{n} \right)^{2} + \left(y_{i,j}^{n+1} - y_{i,j}^{n} \right)^{2} \right]}

where :math:`N = 2x(\text{imax}-2) x (\text{jmax}-2)` and the RMS criterion is user-specified as as small number or by default it is set as: :math:`1\text{x}10^{-6}`. In this code, the convergend is assumed to be achieved when RMS residual is less than the RMS criterion.
