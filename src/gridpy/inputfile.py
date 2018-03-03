""" this class handles the input file i/o """

import os
import sys
import ast
from lxml import etree,objectify
from optparse import OptionParser
import re

def parseXML(xmlfile,entry,value):

    with open(xmlfile) as f:
        xml = f.read()

    root = objectify.fromstring(xml)

    # returns attributes in element node as dict
    attrib = root.attrib

    try:
        exec("%s = %s" %('root.'+entry,str(' '.join(value))))
    except:
        exec("%s = %s" %('root.'+entry,str(value)))
    # print the xml
    obj_xml = etree.tostring(root,pretty_print=True)

    # remove the py:pytype stuff
    objectify.deannotate(root)
    etree.cleanup_namespaces(root)
    obj_xml = etree.tostring(root,pretty_print=True)

    # save your xml
    with open(xmlfile,'w') as f:
        f.write(obj_xml)

class read_input_file(object):
    """
    This class accesses the xml inputfile. The output is the data
    structure containing all the input file entries that can
    be used later for all purposes.

    """
    def __init__(self,filename):
        ### read the input file level 0
        input_file = etree.parse(filename)

        input_data = input_file.find('input_data')
        geometry = input_data.find('geometry')
        PostProcessing = input_data.find('PostProcessing')
        setup = input_data.find('setup')


        ### read inputfile level 1
        plotData = PostProcessing.find('plot')

        # Plot Section 
        files_plot1 = plotData.find('files')
        style_plot1 = plotData.find('style')
        label_plot1 = plotData.find('label')
        plot_legsize = plotData.find('LegendFontSize')
        plot_figsize = plotData.find('FigureSize')
        plot_axislsize = plotData.find('AxisLabelSize')
        plot_axistsize = plotData.find('AxisTitleSize')
        plot_xticks = plotData.find('XTickSize')
        plot_yticks = plotData.find('YTickSize')

        # Assign to object

        self.plot_files = files_plot1.text.split()
        self.plot_names = [(re.sub('\.dat$','',f))for f in self.plot_files]
        self.plot_files = [''.join(['./output/',f]) for f in self.plot_files]
        self.style = style_plot1.text.split()
        self.label = label_plot1.text.split()

        self.legsize =plot_legsize.text
        self.figsize = tuple(float(i) for i in plot_figsize.text.split(" "))
        self.axislsize = plot_axislsize.text
        self.axistsize = plot_axistsize.text
        self.xticks = plot_xticks.text
        self.yticks = plot_yticks.text

		# make some checks
        #if len(self.style) != len(self.plot_files):
        #    print 'the style choice must have the same length'
        #    print 'of your number of files. Correct your inputs.'
        #elif len(self.label) != len(self.plot_files):
        #    print 'the label choice must have the same length'
        #    print 'of your number of files. Correct your inputs.'


def usage():
    """Defines the usage for the setup.py script."""
    usage = "usage: %prog [options] argv[program to compile]"
    parser = OptionParser(usage)
    parser.allow_interspersed_args=True
    parser.add_option("-e", "--exe", dest="GrdGen", default=None,
        help="This option will compile or run utility program for pre and "
        "post-processing files used by code. You must specify a utility "
        "type, UTILITY, as well as the name of the utility within that type. "
        "For more help and a list of options run, ./setup.py -u HELP")

    parser.add_option("-u", "--utility", dest="utility", default=None,
        help="This option will compile or run utility program for pre and "
        "post-processing files used by code. You must specify a utility "
        "type, UTILITY, as well as the name of the utility within that type. "
        "For more help and a list of options run, ./setup.py -u HELP")


    parser.add_option("-p", "--postproc", dest="postprocessing",default=None,
            help="This option will compile or run postprocessing routines"
            "For more help and a list of options run, ./setup.py -u HELP")

    (options,args) = parser.parse_args()

    if options.GrdGen != None:
        action = "GrdGen"
    elif options.utility != None:
        action = "utility"
    elif options.postprocessing !=None:
        action = "postprocessing"
    else:
        action = None

    # inputargs is re-defined as a dictonary for the input variables
    # specified at command line, if not specified defaults are used.
    inputargs = {}
    if len(args) == 0:
        inputargs['name'] = "NO INPUT"
    else:
        inputargs['name'] = args[0]
        
    # update dictionary values using any additional command-line inputs
    if len(args) > 1:
        inputargs.update(ast.literal_eval(sys.argv[len(sys.argv)-1]))
    
    return options,inputargs, action
