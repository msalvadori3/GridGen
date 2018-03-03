.. _documentation:

Documentation
=============

The |GridGen| documentation is written using restructured text (reST) Sphinx, and can be manipulated using python routines in Gridpy.

Building
+++++++++

The documentation can be built using the command::

  $ ./setup.py -u doc build

This runs Sphinx in the documentation directory, and creates the html files using a make command. The command runs Sphinx twice to make sure references are interlinked properly.

Viewing
+++++++

This command is used for viewing the documentation. It is only available after running the build command above, so that the documentation is created. The documentation can be opened using the command::

  $ ./setup.py -u doc open

The documentation requires a compatible version of a web broswer (preferably Google Chrome or Mozilla Firefox). If a compatible browser is installed, the documentation will open when the above command is executed. Otherwise, an error message will be displayed. In case an error message is displayed, the documentation can manually be opened on a web browser from the directory **<GridGen parent directory>/doc/_build/html/index.html**

.. note::

    If using MAC OS X please be aware to simply run $ open **<GridGen parent directory>/doc/_build/html/index.html** from the terminal.


Cleaning
++++++++

The built documentation can be cleaned (i.e. all the compiled files can be deleted while retaining the source content) using the command::

  $ ./setup.py -u doc clean
  
Closing Notes
+++++++++++++

#.  As with any browser-based content, the appearance of the content is dependent on the capability of the browser to render the elements on the webpage. Depending on the bworser present on the machine, the content may appear different.

.. Indices and tables
.. ==================

.. * :ref:`genindex`
.. * :ref:`modindex`
.. * :ref:`search`
