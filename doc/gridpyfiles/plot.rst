.. _plot:

Plotting Utility
================

|GridGen| has a builtin plotting utility that allows for plotting of the generated data without the use of external tools. Similar to all the other features, the plotting utility is also accessed through the input file, which is accessed as follows.

#. Go to the parent directory::

     $ cd <path to |GridGen|>
    
   Make sure that you are in the directory that contains the files :command:`setup.py`, :command:`input_file.xml`, and the folder :command:`src`.

#. Open the input file::

     $ vi input_file.xml  
     
The section of the inputfile devoted to post-processing is shown below:

.. code-block:: xml

   <PostProcessing>
     <plot>
       <iPost>1</iPost>
       <Method>TecPlot</Method>
       <files>file1.dat</files>
       <style>k +r -c :g -.y --m</style>
       <label>Var1</label>
       <LegendFontSize>16</LegendFontSize>
       <FigureSize>10 7</FigureSize>
       <AxisLabelSize>21</AxisLabelSize>
       <AxisTitleSize>22</AxisTitleSize>
       <XTickSize>23</XTickSize>
       <YTickSize>24</YTickSize>
     </plot>
   </PostProcessing>    
   
.. note::

  Before running the plotting tool it is required to run create the subdirectory output.
   
Options in the plotting module
++++++++++++++++++++++++++++++
   
#.  The data files generated from a |GridGen| run (or any external data file) is placed in the :command:`output` folder. The name of the file should not have any spaces or special characters (like colon :, quotation marks "" or '', brackets or parenthesis () [] etc). The file name is entered into the ``<files>`` field in the input file. If there are more than 1 file, they are entered one after another. 

#.  The ``<style>`` entry refers to the line style and color used. There are seven available colors in Python by default: RGBCMYK (Red, Green, Blue, Cyan, Magenta, Yellow and Black). Markers can be placed on the lines using the marker symbols (eg. ``+r`` generates a red line with + shaped markers). Line styles can also be changed using appropriate symbols (eg. ``--m`` generates a dashed magenta line, while ``-.y`` generates a dot-dashed yellow line).

#.  The ``<label>`` entry is to populate the legend. If there are N files for N different variables, then the legend is populated according to the entries in this field.

#.  The remaining entries are to adjust the figure parameters, like font size and figure size.

Using the plotting module
+++++++++++++++++++++++++

#.  Enter the names of the data files in the input file

#.  Set the required number of legend entries and line styles, depending on  how many files/variables are being plotted. 

#.  Adjust any other plot parameters as required.

#.  The plotting module also provides a generalized feature to make plots from input data. This feature is also accessed through the ``input_file.xml``. The input data (.dat) files are entered in the ``files`` section of the input file. The plotting utility is then accessed using the command::

     $ ./setup.py -p single_plot options "{<plot options>}"

  An example of this input for the plotting utility is::

     $ ./setup.py -p single_plot options "{'title':['RMS Residual for Grid#5'],
       'xlabel':['Number of Iterations'],'ylabel':['RMS'],
       'grid':['on'],'legend':['False'],'legend_frame':['True'],
       'legend_loc':['lower right']}"
       
  The options give flexibility to set the graph title, the labels for the axes, select the columns of data from the input .dat file that we want to plot, set the legend properties, etc.
    Note that if no options are provided as in the example::

     $ ./setup.py -p single_plot options

    default options wil be used for the picture name and axes/title labels.
    Note that the file and picture options must be provided in the input file, 
    in the ``<PostProcessing>`` section.

.. note::

   The above command is meant to be used with a generic .dat file, without header, where data is distributed column-wise.

.. * :ref:`genindex`
.. * :ref:`modindex`
.. * :ref:`search`
