.. _inputfile:

Input File
==========

The input file is central location for setting any and all parameters for all the features in |GridGen|. Depending on the type of operation being performed on |GridGen|, the relevant input options are set in the input file.

.. _str-geometry:

Structure of the Geometry Module
--------------------------------

.. parsed-literal::

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



.. _str-setup:

Structure of Setup Module
-------------------------

.. parsed-literal::

   <setup>
      <Project>2D_Grid_Generator</Project>
      <nmax>500</nmax>
      <iControl>1</iControl>
      <Cy>2.0</Cy>
      <RMSres>1.0e-6</RMSres>
    </setup>
  
.. _str-post:

Structure of Postprocessing Modules
------------------------------------------------

.. _plot-input:

|GridGen| also allows the user to graphically visualize the results through the use of graphs and contours. Inputs to this plotting utility are also provided through the input file. The relevant block for this utility is shown below, and linked to the dedicated plotting utility page.

.. parsed-literal::

    <PostProcessing>
      <plot>
          <iPost>1</iPost>
          <Method>TecPlot</Method>
          <files>rmslog.dat</files>
          <style>k +r</style>
          <label>Grid</label>
          <LegendFontSize>16</LegendFontSize>
          <FigureSize>10 7</FigureSize>
          <AxisLabelSize>21</AxisLabelSize>
          <AxisTitleSize>22</AxisTitleSize>
          <XTickSize>23</XTickSize>
          <YTickSize>24</YTickSize>
      </plot>
    </PostProcessing>


.. * :ref:`genindex`
.. * :ref:`modindex`
.. * :ref:`search`
