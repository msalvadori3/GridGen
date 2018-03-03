import os
import sys
import numpy as np
from inputfile import *
import matplotlib.pyplot as plt
from matplotlib import rcParams
from math import atan2,degrees
from utilities import read_path
import matplotlib.ticker as ticker
from matplotlib import cm
from matplotlib import pylab
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d import proj3d
from matplotlib.ticker import LinearLocator, FormatStrFormatter
rcParams.update({'figure.autolayout': True})

class plot_results(object):

    def get_plot_properties_single(self,inputfile,args):
        """ In this routine we set some properties
            for the single plot
        """
        # plot properties from the input file
        self.fnames = inputfile.plot_names
        self.style  = inputfile.style
        self.label  = inputfile.label

        # plot properties from user input
        if 'title' in args:
            self.title = args['title']
        else:
            self.title = ['example']
        if 'xlabel' in args:
            self.xlabel = args['xlabel']
        else:
            self.xlabel = []
        if 'ylabel' in args:
            self.ylabel = args['ylabel']
        else:
            self.ylabel = []
        if 'legend' in args:
            self.legend = args['legend']
            if 'legend_frame' in args:
                self.legend_frame = args['legend_frame'][0]
            else:
                self.legend_frame = False
            if 'legend_loc' in args:
                self.legend_loc = args['legend_loc'][0]
            else:
                self.legend_loc = 'upper right'
        else:
            self.legend = False
        if 'grid' in args:
            self.grid = args['grid']
        else:
            self.grid = 'False'
        if 'log' in args:
            self.log = args['log']
            if 'log_x' in args:
                self.log_x = args['log_x'][0]
            else:
                self.log_x = False
            if 'log_y' in args:
                self.log_y = args['log_y'][0]
            else:
                self.log_y = False
        else:
            self.log = False
        if 'variables' in args:
            if int(len(args['variables'][0].split(' '))) > 2:
                print 'Error: the variables position has to be'
                print 'in the format "{\'variables\':[\'<num1> <num2>\']}"'
                print 'where \'<num1>\' and \'<num2>\' must be integers. Got: ',args['variables']
                sys.exit()
            elif int(len(args['variables'][0].split(' '))) == 2: 
                x_t = args['variables'][0].split(' ')
                for i in range(0,2):
                    if int(x_t[i] < 0):
                        print 'Error. The option entries in the \'variables\' item must be non-negative integers. Got: ', args['variables']
                        sys.exit()
                    else:
                        self.variables = []
                        for i in range(0,len(x_t)):
                            self.variables.append(int(x_t[i]))
        else:
            # if no inputs are provided, by default we assign x = 1 and y = 2 as positions for the variables to be plot
            self.variables = [1,2]

    def get_plot_properties_2d(self,inputfile):
        """ In this routine we set some properties
            for the 2d contour plots
        """
        if ((inputfile.analysis == 'TPY') or (inputfile.analysis == 'TPX')):
            size1 = len(inputfile.T)
            size2 = len(inputfile.p)
            if size1 > 1:
                size1 = inputfile.T[size1-1]
                X = 'Temperature'
                varXunits = '(K)'
                X_idx = 0
            else:
                print 'temperature size must be larger than one.'
                sys.exit()
            if size2 > 1:
                size2 = inputfile.p[size2-1]
                F = 'Pressure'
                Y_idx = 1
            else:
                print 'pressure size must be larger than one'
        else:
            print 'your analysis type is not yet supported: ',inputfile.analysis
        self.varXname = X
        self.varFname = F

        self.sizeX = size1
        self.sizeY = size2
        self.idx_X = X_idx
        self.idx_Y = Y_idx
        self.fname = []
        self.varXunits = varXunits

    def __init__(self, name, args, outdir="./output"):

        # read from input file
        inputfile = read_input_file('input_file.xml')

        # set plot options
        params1 = {
           'legend.fontsize': inputfile.legsize,
           'figure.figsize':  inputfile.figsize,
           'axes.labelsize':  inputfile.axislsize,
           'axes.titlesize':  inputfile.axistsize,
           'xtick.labelsize': inputfile.xticks,
           'ytick.labelsize': inputfile.yticks,
           }

        pylab.rcParams.update(params1)

        if name == '2d_contour':

            # get some plot properties
            self.get_plot_properties_2d(inputfile)

            # make plot
            self.plot_main(inputfile,name,args)

        elif name == 'single_plot':

            if args['name'] == 'options':
                # read the parsed keywords and continue

                # get some plot properties
                self.get_plot_properties_single(inputfile,args)

                # make plot
                self.plot_main(inputfile,name,args)
            else:
                # tell user how to specify the inputs
                print 'Error: wrong argument \'name\' parsed:'
                print 'args[\'name\'] = ',args['name']
                print 'The correct syntax for this utility is:'
                print './setup.py -p single_plot options '
                print '"{\'title\':[\'<user title>\'],\'xlabel\':[\'<user xlabel>\'],\'ylabel\':[\'<user ylabel>\'],\'variables\':[\'<var. positions (integers)>\'],\'grid\':[\'<logical>\'],\'legend\':[\'<logical>\'],\'legend_frame\':[\'<logical>\'],\'legend_loc\':[\'<options>\'],\'log\':[\'<logical>\'],\'log_x\':[\'<logical>\'],\'log_y\':[\'<logical>\']}"'
                print 'Example:'
                print './setup.py -p single_plot options "{\'title\':[\'RMSvsIter\'],\'xlabel\':[\'Iteration Number\'],\'ylabel\':[\'RMS\'],\'variables\':[\'2\'],\'grid\':[\'True\'],\'legend\':[\'True\'],\'legend_frame\':[\'True\'],\'legend_loc\':[\'lower right\'],\'log\':[\'True\'],\'log_x\':[\'False\'],\'log_y\':[\'True\']}"'
                print '================================:'
                print 'More infos: <type>, <default>'
                print '\'title\': <string> (optional),  \'example\''
                print '\'xlabel\': <string> (optional), \'None\''
                print '\'ylabel\': <string> (optional), \'None\''
                print '\'variables\': <string> w/ integers (optional), \'1 2\''
                print '\'grid\': <string> (logical/optional), \'False\''
                print '\'legend\': <string> (logical/optional), \'False\''
                print '\'legend_frame\': <string> (logical/optional - requires legend=\'True\'), \'False\''
                print '\'legend_loc\': <string> (logical/optional - requires legend=\'True\'), \'False\''
                print 'available options:'
                print     'upper right'
                print     'upper left'
                print     'lower left'
                print     'lower right'
                print     'right'
                print     'center left'
                print     'center right'
                print     'lower center'
                print     'upper center'
                print     'center'
                print '\'log\': <string> (logical/optional), \'False\''
                print '\'log_x\': <string> (logical/optional), \'False\''
                print '\'log_y\': <string> (logical/optional), \'False\''

                sys.exit()


    def plot_main(self,inputfile,name,args):
            # main call for all kind of plots

            if name == '2d_contour':

                for fil in inputfile.plot_files:
                    # open file to read header
                    data = []
                    f = open(fil,'r')
                    for line in f.readlines():
                        l_string = line.split()
                        if l_string[0] == '#':
                            header = [str(n) for n in l_string[1:-1]]
                            units = header[1::2]
                            header = header[0::2]
                            idxX = header.index(self.varXname)
                            idXF = header.index(self.varFname)
                        else:
                            l_number = [float(n) for n in l_string[:-1]]
                            data.append(l_number)
                    f.close()

                    # adjust the data in a 2D matrix
                    varX = []#np.zeros(int(self.sizeX))
                    varY = []#np.zeros(int(self.sizeY))
                    nx = 0
                    ny = 0
                    for n in range(0,len(data[:])):
                        if n == 0:
                            varX.append(data[n][0])
                            varY.append(data[n][1])
                            nx = nx + 1
                            ny = ny + 1
                        else:
                            if data[n][0] != varX[nx - 1]:
                                varX.append(data[n][0])
                                nx = nx + 1
                            if (data[n][1] != varY[ny - 1]) and (data[n][1] not in varY):
                                varY.append(data[n][1])
                                ny = ny + 1
                    n = 0
                    data2D = np.zeros((int(len(varX)),int(len(varY)),len(units)-2))
                    for nx in range(0,len(varX)):
                        for ny in range(0,len(varY)):
                            data2D[nx,ny,:] = data[n][2:]
                            n = n + 1
                    # note here i set len(header)-4 because other plots are meanigless for now
                    for k in range(2,len(header)-4):
                        xi, yi = np.meshgrid(varX, varY)
                        zi = data2D[:,:,k-2]
                        if k-2 == 6:
                            # better visualization
                            clip_percent = 0.05
                            #pl = plt.scatter(xi, yi, c=(np.clip(zi.transpose(),zi.min(),clip_percent*zi.max())))
                            pl = plt.contourf(xi, yi, np.clip(zi.transpose(),zi.min(),clip_percent*zi.max()),100)
                        else:
                            #pl = plt.scatter(xi, yi, c=zi.transpose())
                            pl = plt.contourf(xi, yi, zi.transpose(),100)
                        #pl = plt.contourf(xi,yi,zi.transpose())
                        cbar = plt.colorbar(pl,ticks=np.linspace(zi.min(),zi.max(),10),cmap="jet")
                        #cbar.ax.tick_params(labelsize=14)
                        cbar.locator = ticker.MaxNLocator(nbins=15)
                        cbar.update_ticks()
                        plt.title(header[k]+' '+units[k])
                        plt.xlabel(header[self.idx_X]+' '+units[self.idx_X])
                        plt.ylabel(header[self.idx_Y]+' '+units[self.idx_Y])
                        # save picture
                        filename = header[k]
                        if not os.path.exists('./output/' +'/plot/'):
                            os.makedirs('./output/' +'/plot/')
                        filename_plot = os.path.join('./output/' +'/plot/', filename+'.'+'png')
                        plt.savefig(filename_plot, format='png')
                        plt.close()
            elif name == 'single_plot':
                # this plot routine reads a simple data files and
                # variables to plot are taken from the input file
                outpath = read_path('OUT_DIR')

                # assign user inputs to local variables for convenience
                files = self.fnames
                style = self.style
                label = self.label
                title = self.title
                xlabel = self.xlabel
                ylabel = self.ylabel
                variables = self.variables
                legend    = self.legend
                log = self.log
                if legend:
                    legend_frame = self.legend_frame
                    legend_loc = self.legend_loc
                if log:
                    log_x = self.log_x
                    log_y = self.log_y
                grid = self.grid

                # import data from file
                n = 0
                for fil in files:
                    data = np.genfromtxt(outpath+fil+'.dat')
                    if log[0] == True:
                        if log_x == True:
                            plt.semilogx(data[:,variables[0]-1],data[:,variables[1]-1],style[n],label=label[n])
                        elif log_y == True:
                            plt.semilogy(data[:,variables[0]-1],data[:,variables[1]-1],style[n],label=label[n])
                    
                    plt.plot(data[:,variables[0]-1],data[:,variables[1]-1],style[n],label=label[n])

                    n = n + 1
                # add picture title and axes labels
                plt.ylabel(ylabel[0])
                plt.xlabel(xlabel[0])
                plt.title(title[0])
                plt.grid(grid[0])
                if legend[0]:
                    plt.legend(
                        loc           = legend_loc,
                        borderpad     = 0.25,
                        handletextpad = 0.25,
                        borderaxespad = 0.25,
                        labelspacing  = 0.0,
                        handlelength  = 2.0,
                        numpoints=1)
                    legendText = plt.gca().get_legend().get_texts()
                    plt.setp(legendText,fontsize=inputfile.legsize)
                    legend_fram = plt.gca().get_legend()
                    legend_fram.draw_frame(legend_frame)

                # Save Picture in the ../output/plot/ folder as png. Folder is created if not present.
                if not os.path.exists('./output/' +'/plot/'):
                    os.makedirs('./output/' +'/plot/')
                filename_plot = os.path.join('./output/' +'/plot/', '_'.join(title[0].split(' '))+'.'+'png')
                plt.savefig(filename_plot, format='png')

def postproc(opts,args):

    # reference list containing the available plot options
    avail_plot_opts = ['2d_contour','single_plot']

    if opts in avail_plot_opts:
        plot_results(opts,args)
    else:
        print 'this option is not recognized: ',opts
        sys.exit()
