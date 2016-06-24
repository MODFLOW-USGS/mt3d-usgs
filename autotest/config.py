import os
import platform

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

# Development version information
testpaths = [os.path.join('..', 'test-cmp'), os.path.join('..', 'test-reg')]
srcdir = os.path.join('..', 'src')
program = 'mt3d-usgs'
version = '1.0.00'
target = os.path.join('temp', program + '_' + version + target_extension)
target_dict[os.path.basename(target)] = target

# Release version information
loc_release = os.path.join('..', 'mt3dms')
dir_release = os.path.join('temp', 'mt3dms')
program = 'mt3dms'
srcdir_release = os.path.join(dir_release, 'src')
version_release = '5.3.00'
target_release = os.path.join('temp', program + '_' + version_release +
                              target_extension)
target_dict[os.path.basename(target_release)] = target_release
target_dict[program] = target_release

# Comparison information
target_dict['mfnwt'] = 'MODFLOW-NWT_64' + target_extension
target_dict['mf2005'] = 'mf2005' + target_extension
target_dict['mf2k'] = 'mf2000' + target_extension



# Standard versions of modflow codes to use for regressions if exe stored in repo
mf2005exe = os.path.join('bin', 'mf2005{}'.format(target_extension))


