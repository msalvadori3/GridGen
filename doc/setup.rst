.. _setup-gridgen:

Setup
=====

.. _setup:

Setting up a Grid
-----------------

.. _structure:

Code Structure
--------------

The |GridGen| code is divided into three major parts:

#. |  **Input**: User input is entered into a file called ``input_file.xml``. 

#. |  **Main Program**: The main program for solving the elliptical grid is ``gridgen.F90`` in the ``<parent directory>/src/main`` folder. 

#. |  **Output**: Once the code is run, the results are stored in the ``<parent directory>/output/`` folder.

.. _input:

Input File
----------

The inputs for GridGenerator are specified in the ``input_file.xml``. The input file is accessed as follows:

#. Go to the parent directory::

     $ cd <path to |GridGen|>
    
   Make sure that you are in the directory that contains the files :command:`setup.py`, :command:`input_file.xml`, and the folder :command:`src`.

#. Open the input file::

     $ vi input_file.xml  


#. The main parts of the input file are shown below

The input file is divided into different parts. The geometry is set in the ``<geometry>`` module, which is shown below:

.. code-block:: xml

    <geometry>
      <x1>-0.8</x1>
      <y1>0.0</y1>
      <x2>1.8</x2>
      <y2>0.0</y2>
      <x3>-0.8</x3>
      <y3>1.0</y3>
      <x4>1.8</x4>
      <y4>1.0</y4>
      <FEsize>11</FEsize>
      <DCsize>11</DCsize>
      <Geosize>21</Geosize>
      <Geoptx1>0.0</Geoptx1>
      <Geopty1>0.0</Geopty1>
      <Geoptx2>1.0</Geoptx2>
      <Geopty2>0.0</Geopty2>
      <imax>41</imax>
      <jmax>19</jmax>
    </geometry>

For setup of the solver, most of the inputs are set in the ``<setup>`` module. A  snippet of this module is shown below:

.. code-block:: xml
   
    <setup>
      <Project>2D_Grid_Generator</Project>
      <nmax>500</nmax>
      <iControl>1</iControl>
      <Cy>2.0</Cy>
      <RMSres>1.0e-6</RMSres>
    </setup>

.. _compilation:

Compilation
-----------

The following sequence of commands is used to compile a single simulation.

#. Go to the parent directory::

    $ cd <path to |GridGen|>
    
   Make sure that you are in the directory that contains the files :command:`setup.py`, :command:`input_file.xml`, and the folder :command:`src`.

#. Clean existing results::

    $ ./setup.py -u clean heavy
    
   This command removes the existing files in the output folder ``|GridGen|/output/`` and deletes the object files from previous compilations. **Backup any required results before using this command.**
    
#. Set working directory path::
    
    $ ./setup.py -u set_path
    
   This command sets the working directory path

#. Set the output directory::

   $ ./setup.py -u create_subdirectories
    
#. Compile the build::

    $ ./setup.py -e configure
    
   An empty CMake window opens. Press :class:`[c]` on the keyboard to configure the program.
    
    This brings up the CMake window. There are two options for the :command:`CMAKE_BUILD_TYPE` :
      * :class:`Release`: This compiles the program in regular mode; debugging flags are disabled.
      * :class:`Debug`:   This compiles the program in debug mode; errors and warnings are displayed on the terminal.
      
    Press :class:`[Enter]` on the keyboard to edit the option (to change from :class:`Release` to :class:`Debug` or vice versa)
    
    The file :command:`grid.x` will now be generated in the parent directory
    
#. Execute the program::

     $ ./grid.x
     
   This command runs the program. If Debug mode is enabled in :command:`GRIDGEN_COMPILE_DEFS`, appropriate output is printed on the Terminal screen. 

.. _results:   

Results
-------

The results are stored in the ``output/`` folder inside the parent directory. The output directory contains several files **.dat** and **.tec** where the calculations are written. In addition, there is also a ``output/plot`` folder, where figures from the calculated data are plotted. To plot the results. open the inputfile and enter the name of the **.dat** file that was generated in the ``files`` entry (as shown below).

.. code-block:: xml

      <PostProcessing> 
          <plot>
             <files>RESULTDATFILE</files>
             ...
          </plot>
      </PostProcessing>
         
Then, from the parent directory execute the following command to plot the results using |GridGen|'s inbuilt plotting utility::

    $ ./setup.py -p single_plot
    
This will generate the RMS line plots from the results, which will be stored in the ``output/plot`` folder.


.. * :ref:`genindex`
.. * :ref:`modindex`
.. * :ref:`search`
