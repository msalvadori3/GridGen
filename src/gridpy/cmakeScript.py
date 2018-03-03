"""Write simple CMake script files."""
import subprocess
import random
import string

class CMakeOption(object):
    """This provides a derivative of the dictionary class that stores
    the CMake variable type in a hidden field for each key:value pair.
    """

    def __init__(self, varName, cmakeType='STRING', value="", info="", cache=True, force=True):
        """Create an option in CMake

        varName is the name of the variable
        cmakeType is either: 'FILEPATH', 'PATH', 'STRING', 'BOOL', or 'INTERNAL'
        value is the value of the variable. Currently no checking is done to ensure it is
              consistent with the cmakeType
        info is the description string in the set command
        cache is a bool variable indicating whether the variable should be in the cache or not.
        force is a bool indicating if the FORCE option should be included
        """

        self.varName = varName
        self.cmakeType = cmakeType

        if value.find(' ') > 0:
            self.value = '"%s"' %(value)
        else:
            self.value = value

        if info.find(' ') > 0:
            self.info = '"%s"' %(info)
        elif info == "":
            self.info = '""'
        else:
            self.info = info

        self.cache = cache
        self.force = force

    def __str__(self):
        return str(self.__dict__)

    def create_script_line(self):
        if self.cache:
            if self.force:
                scriptLine = 'set(%s %s CACHE %s %s FORCE)\n' % (self.varName, self.value,
                                                                 self.cmakeType,
                                                                 self.info)
            else:
                scriptLine = 'set(%s %s CACHE %s %s)\n' % (self.varName, self.value,
                                                           self.cmakeType,
                                                           self.info)
        else:
            scriptLine = 'set(%s %s)\n' % (self.varName, self.value)

        return scriptLine

class CMakeScript(object):
    """This is used to generate CMake scripts that only perform very basic things like
    setting variables to initialize."""

    def __init__(self, cmakeListsPath=None, initialOpts=None, advancedOpts=False,
                 parsePrefix=tuple(string.ascii_letters)):
       """Create a new CMakeScript class. If passed the cmakeListsPath, it will
       generate a list of available options in the CMakeLists.txt file specified
       at the given location.

       If initialOpts (dictionary) is passed, it will generate the available options
       assuming those options are turned on to start.

       If advancedOpts is true, the list of available types will include the options
       marked as advanced.

       If parsePrefix is not None, it is a tuple containing the option prefixes to be included
       in the available options.
       """

       self.initialOpts = initialOpts

       self.advancedOpts = advancedOpts
       self.parsePrefix = parsePrefix

       self.options = {}

       if cmakeListsPath is not None:
           cmakeOptsDict = self.get_available_options(cmakeListsPath)
           for var in cmakeOptsDict:
               self.options[var] = CMakeOption(varName = var,
                                               cmakeType = cmakeOptsDict[var][0],
                                               value = cmakeOptsDict[var][1],
                                               cache = True,
                                               force = True)

    def get_available_options(self, cmakeListsPath):
       """Build a dictionary of available options in the CMakeLists.txt file
       at the given path and put it in the self.availableOpts variable
       """

       # Create a temporary directory to run in, otherwise the cache will be generated
       # in the current directory

       # Generate a random name for the directory so it's not going to be there already
       name = ''.join(random.choice(string.ascii_uppercase + string.digits) \
                          for x in range(16))
       subprocess.call("mkdir %s" %(name), shell=True)

       # Build the cmake command
       cmakeCmd = 'cmake '
       if self.advancedOpts:
          cmakeCmd = ''.join([cmakeCmd,'-LA '])
       else:
          cmakeCmd = ''.join([cmakeCmd,'-L '])

       if self.initialOpts is not None:
          for opt, val in self.initialOpts.iteritems():
             cmakeCmd = ''.join([cmakeCmd,'-D{0:s}:{1:s}="{2:s}" '.format(opt[0],opt[1],val)])

       cmakeCmd = ''.join([cmakeCmd,cmakeListsPath])

       # Call cmake to list the variables
       cmakeBuild = subprocess.Popen(cmakeCmd, shell=True,
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE,
                                     cwd='./%s' % (name))

       cmakeStdOutput, cmakeStdErr = cmakeBuild.communicate()

       cmakeOptionsRaw = [opt for opt in cmakeStdOutput.split('\n') \
                              if not opt.startswith('--') and not opt == '' and \
                              opt.startswith(self.parsePrefix)]

       subprocess.call("rm -rf %s" %(name), shell=True)

       cmakeOptionsDict = {}

       for opt in cmakeOptionsRaw:
           # Split at the = sign first, in case the value has : in it for some reason
           splitOpt = opt.split('=')
           # The first item in the list should be OPTION_NAME:CMAKETYPE
           optionName = splitOpt[0].split(':')[0]
           cmakeType = splitOpt[0].split(':')[1]
           value = splitOpt[1]

           cmakeOptionsDict[optionName] = (cmakeType,value)

       return cmakeOptionsDict

    def get_cache_options(self, cmakeCacheFile):
       """Build a dictionary of available options in the cache file
       at the given path and put it in the self.availableOpts variable
       """

       with open(cmakeCacheFile, 'r') as fid:
           cmakeOptionsRaw = fid.readlines()

       cmakeOptions = [opt for opt in cmakeOptionsRaw \
                           if not opt.startswith('#') and \
                              not opt.startswith('//') and \
                              not opt.startswith('\n')]

       cmakeOptionsDict = {}

       for opt in cmakeOptions:
           # Split at the = sign first, in case the value has : in it for some reason
           splitOpt = opt.split('=')
           # The first item in the list should be OPTION_NAME:CMAKETYPE
           optionName = splitOpt[0].split(':')[0]
           cmakeType = splitOpt[0].split(':')[1]
           value = splitOpt[1].rstrip()

           cmakeOptionsDict[optionName] = (cmakeType,value)

       return cmakeOptionsDict

    def update_items(self, updateDict, includeOnly=None, excludeOnly=None):
       """Update items in the input file by passing in a dictionary of tuples.
       No effort is made to ensure it is a valid option, so spelling mistakes are
       the fault of the user. The first element of the tuple is the CMake variable
       type while the second is the value and the third and fourth are bools indicating
       if it should be cached and forced respectively if behavior other than default
       is desired.

       includeOnly and excludeOnly are lists, where the items indicate
       which variables are in the include or exclude list. Variables in the includeOnly
       lists are the only ones modified by this routine while variables in excludeOnly
       lists are the only ones not modified by this routine.
       """

       if includeOnly is not None:
          for variable, dataTuple in \
                 dict([(k, v) for k, v in updateDict.iteritems() \
                     if k in includeOnly]).iteritems():
              if len(dataTuple) == 2:
                  self.options[variable] = CMakeOption(varName = variable,
                                                       cmakeType = dataTuple[0],
                                                       value = dataTuple[1])
              elif len(dataTuple) == 3:
                  self.options[variable] = CMakeOption(varName = variable,
                                                       cmakeType = dataTuple[0],
                                                       value = dataTuple[1],
                                                       cache = dataTuple[2])
              elif len(dataTuple) == 4:
                  self.options[variable] = CMakeOption(varName = variable,
                                                       cmakeType = dataTuple[0],
                                                       value = dataTuple[1],
                                                       cache = dataTuple[2],
                                                       force = dataTuple[3])

       elif excludeOnly is not None:
           for variable, dataTuple in \
                   dict([(k,v) for k,v in updateDict.iteritems() \
                             if not k in excludeOnly]).iteritems():
               if len(dataTuple) == 2:
                   self.options[variable] = CMakeOption(varName = variable,
                                                        cmakeType = dataTuple[0],
                                                        value = dataTuple[1])
               elif len(dataTuple) == 3:
                   self.options[variable] = CMakeOption(varName = variable,
                                                        cmakeType = dataTuple[0],
                                                        value = dataTuple[1],
                                                        cache = dataTuple[2])
               elif len(dataTuple) == 4:
                   self.options[variable] = CMakeOption(varName = variable,
                                                        cmakeType = dataTuple[0],
                                                        value = dataTuple[1],
                                                        cache = dataTuple[2],
                                                        force = dataTuple[3])

       else:
          for variable, dataTuple in updateDict.iteritems():
              if isinstance(dataTuple, CMakeOption):
                  self.options[variable] = dataTuple
              elif len(dataTuple) == 2:
                  self.options[variable] = CMakeOption(varName = variable,
                                                       cmakeType = dataTuple[0],
                                                       value = dataTuple[1])
              elif len(dataTuple) == 3:
                  self.options[variable] = CMakeOption(varName = variable,
                                                       cmakeType = dataTuple[0],
                                                       value = dataTuple[1],
                                                       cache = dataTuple[2])
              elif len(dataTuple) == 4:
                  self.options[variable] = CMakeOption(varName = variable,
                                                       cmakeType = dataTuple[0],
                                                       value = dataTuple[1],
                                                       cache = dataTuple[2],
                                                       force = dataTuple[3])


    def write_script(self, scriptFile):
        """Write a CMake script file based on the options ."""

        with open(scriptFile, 'w') as fid:
            for opt in self.options.values():
                fid.write(opt.create_script_line())
