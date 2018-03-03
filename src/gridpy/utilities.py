import sys
import os
import subprocess as sp
from inputfile import *
import cmakeScript as CMS

def read_path(key):

    try:
        f = open('path.dat','r')
        lines = f.readlines()
        found = False

        for n in range(0, len(lines)):
            if (key == lines[n].split()[0]):
                p = lines[n+1].split()[0]
                found = True
                break
        f.close()

        if (found == False):
            print 'your keyword ', key, 'was not found in path.dat.'
            sys.exit()
    except:
        print 'something went wrong while trying to access'
        print 'path.dat. It is likely that it does not exist.'
        print 'run first:'
        print './setup.py -u set_path'
        sys.exit()
    return p

def set_path():
	curr_dir = sp.check_output('pwd',shell=True).strip('\n')
	curr_dir = curr_dir+'/'
	#curr_dir = '{}/'.format(str(curr_dir))
	f = open('path.dat','w')
	f.write('CURRENT_DIR\n')
	f.write(curr_dir+'\n')
	f.write('SRC_DIR\n')
	f.write(curr_dir+'src/\n')
	f.write('OUT_DIR\n')
	f.write(curr_dir+'output/')
	f.close()

def clean(args):

    if args['name'] == 'NO INPUT':
        os.system('rm -rf build/')
    elif args['name'] ==  'heavy':
        print 'Deleting executables, results, binaries and documentation ...'
        os.system('rm -rf build*')
        os.system('rm -rf fort*')
        os.system('rm -rf *.dat')
        os.system('rm -rf output/*')
        os.system('rm -rf *.x')
        os.system('make clean')
    elif args['name'] ==  'results':
        os.system('rm -rf output/*')
    else:
        print 'argument ', args['name'], ' is not recognized.'
        print 'run ./setup.py -u help to list the available utilities'
        sys.exit()

def create_directories():
	# here we create all the directories we need for the project
	if not os.path.exists('./output'):
		os.makedirs('./output')

def doc(args):
    # simple documentation utilities for build/delete doc
    if args['name'] == 'build':
        os.chdir('doc/')
        os.system('make html && make html')
        print 'Documentation built in doc/_build/html/'
        print 'open the html file using:'
        print './setup.py -u doc open'
    elif args['name'] == 'clean':
        os.chdir('doc/')
        print 'Deleting documentation ...'
        os.system('make clean')
        print 'Done.'
    elif args['name'] == 'open':
        # try with chrome first, if not try firefox.
        try:
            os.chdir('doc/_build/html/')
        except:
            print 'Error. Directory doc/_build/html/ not found.'
            print 'Possibly you did not build the documentation.'
            print 'make sure to run:'
            print './setup.py -u doc build'
            sys.exit()

        isfile = os.path.isfile('index.html')
        if isfile == False:
            print 'Error. File index.html not found.'
            print 'Possibly you did not build the documentation.'
            print 'make sure to run:'
            print './setup.py -u doc build'
            sys.exit()
        else:
            try:
                os.system('google-chrome index.html')
            except:
                print 'google chrome not found. Trying Firefox ...'
                try:
                    os.system('firefox index.html')
                except:
                    print 'Browser not found! Exiting ...'
                    sys.exit()


    else:
        print 'argument ', args['name'], ' is not recognized.'
        print 'run ./setup.py -u help to list the available utilities'
        sys.exit()
def utility(opts,args):

    if opts == 'clean':
        clean(args)
    elif opts == 'set_path':
        set_path()
    elif opts == 'create_directories':
        create_directories()
    elif opts == 'doc':
        doc(args)
    else:
        print 'option ', opts, ' is not recognized.'	
        print 'run ./setup.py -u help to list the available utilities'
        sys.exit()
	
