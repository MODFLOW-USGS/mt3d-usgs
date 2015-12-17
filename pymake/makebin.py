#! /usr/bin/env python
try:
    import pymake
except:
    msg =  'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    print(msg)
    raise Exception()

#get the arguments
args = pymake.pymake.parser()

#call main -- note that this form allows main to be called
#from python as a function.

pymake.pymake.main(args.srcdir, args.target, args.fc, args.cc, args.makeclean,
                   args.expedite, args.dryrun, args.double, args.debug, 
                   args.subdirs, args.fflags, args.arch)
