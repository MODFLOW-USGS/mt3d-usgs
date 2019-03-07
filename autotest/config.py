import os
import platform
import flopy

# Autotest information
testdir = 'temp'
if not os.path.isdir(testdir):
    os.mkdir(testdir)
target_dict = {}

exclude = None
retain = False

# Compiling information
fc = 'gfortran'
#fc = 'ifort'
target_extension = ''
target_arch = 'intel64'
if platform.system() in 'Windows':
    target_extension = '.exe'

fflags = None
if fc == 'gfortran':
    fflags = 'Werror Wtabs Wline-truncation Wcharacter-truncation'

# Development version information
testpaths = [os.path.join('..', 'test-cmp'), os.path.join('..', 'test-reg')]
srcdir = os.path.join('..', 'src')
program = 'mt3d-usgs'
version = '1.0.00'
target = os.path.join('temp', program + '_' + version + target_extension)
target_dict[os.path.basename(target)] = target

# Other programs needed for testing.  Look to see if these programs are
# already in the path, and if so, then use what's in the path.  If they
# are not in the path, then use the versions downloaded from github.
bindir = os.path.join('temp', 'bin')
for p in ['mt3dms', 'mfnwt', 'mf2005', 'mf2000']:
    exe_exists = flopy.which(p)
    if exe_exists is None:
        pwpath = 'temp/bin/{}'.format(p) + target_extension
        pwpath = os.path.abspath(pwpath)
        target_dict[p] = pwpath
    else:
        target_dict[p] = exe_exists
