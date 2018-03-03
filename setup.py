#!/usr/bin/env python
import sys
import os
Current_dir = os.getcwd()
sys.path.insert(0,Current_dir+'/src/gridpy')
from inputfile import *
from utilities import *
from plot_results import *

def GridGen(opts,args):
    """ General routines calls to handle the code """
    if opts == "configure":
        configure()
    if opts == "configure_silent":
        configure_silent(args)
    elif opts == "compile":
        compil()
    else:
        sys.exit('Unrecognized command')

def configure():
    # create the build directory and cd into it
    os.system('mkdir build')
    os.chdir('build')

    # run ccmake based on the CMakeLists file
    os.system('ccmake ../src/')

    # run make and make install
    os.system('make && make install')

def configure_silent(args):
    # create the build directory and cd into it
    os.system('mkdir build')
    os.chdir('build')

    cm_flag = False
    if args['name'] == 'options':
        if 'cmake_file' in args:
            cm_file = args['cmake_file']
            cm_flag = True

            # move the Cache file into the current build directory
            os.system('mv ../'+cm_file+' .')
        else:
            print 'Error. Only the keyword cmake_file is accepted.'
            print 'Example:'
            print './setup.py -e configure_silent options "{\'cmake_file\':\'<CMakeCache_example.txt>\'}"'
    else:
        print 'Error. Only the keyword options is accepted.'
        print 'Example:'
        print './setup.py -e configure_silent options "{\'cmake_file\':\'<CMakeCache_example.txt>\'}"'

    # run ccmake based on the CMakeLists file
    if cm_flag:
        os.system('cmake -C '+cm_file+' ../src/')
    else:
        os.system('cmake ../src/')

    # run make and make install
    os.system('make && make install')

def compil():
    try:
        # cd into the biuld directory
        os.chdir('build')

        # run make and make install
        os.system('make && make install')
    except:
        sys.error('build directory does not exist. You should \
                    run ./setup.py -e configure first.')

# get user inputs
(options,args,action) = usage()

if action == 'GrdGen':
    GridGen(options.GrdGen,args)
elif action == 'utility':
    utility(options.utility,args)
elif action == 'postprocessing':
    postproc(options.postprocessing,args)
else:
    sys.exit('Unrecognized command')










