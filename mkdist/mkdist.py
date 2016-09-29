"""
Python code to create a MODFLOW 6 distribution

"""
import os
import sys
import shutil
import zipfile

# Add the folder above so mf6pyutil can be added
pth = '..'
if pth not in sys.path:
    sys.path.append(pth)
from mf6pyutil import mf6pyutil

def zipdir(dirname, zipname):
    zipf = zipfile.ZipFile(zipname, 'w', zipfile.ZIP_DEFLATED)
    for root, dirs, files in os.walk(dirname):
        for file in files:
            zipf.write(os.path.join(root, file))
    zipf.close()
    return

destpath = '.'
version = 'mf6beta.0.5.00'
dest = os.path.join(destpath, version)

print(2*'\n')
print('Creating MODFLOW 6 distribution: {}'.format(version))
print('\n')

if os.path.exists(dest):
    # Raise Exception('Destination path exists.  Kill it first.')
    print('Clobbering destination directory: {}'.format(dest))
    print('\n')
    shutil.rmtree(dest)


# Create subdirectories
binpath = os.path.join(dest, 'bin')
docpath = os.path.join(dest, 'doc')
expath  = os.path.join(dest, 'examples')
sourcepath = os.path.join(dest, 'src')  # will create with copytree
msvspath0 = os.path.join(dest, 'msvs')
msvspath = os.path.join(dest, 'msvs', 'mf6')
pymakepath = os.path.join(dest, 'pymake')
subdirs = [dest, binpath, docpath, expath, msvspath0, msvspath, pymakepath]
print('Creating directories')
for sd in subdirs:
    print(' {}'.format(sd))
    os.mkdir(sd)
print('\n')


# Copy the executables
print('Copying mf6 executable')
bin32 = os.path.join('..', 'bin', 'mf6.exe')
shutil.copy(bin32, os.path.join(binpath, 'mf6.exe'))
print('  {} ===> {}'.format(bin32, os.path.join(binpath, 'mf6.exe')))
print('\n')


# Copy the translator executable
print('Copying translator executable')
bin32 = os.path.join('..', 'mf5to6', 'msvs', 'mf5to6', 'Release', 'mf5to6.exe')
shutil.copy(bin32, os.path.join(binpath, 'mf5to6.exe'))
print('  {} ===> {}'.format(bin32, os.path.join(binpath, 'mf5to6.exe')))
print('\n')


# Copy the source folder
s = os.path.join('..', 'src')
shutil.copytree(s, sourcepath, ignore=shutil.ignore_patterns('.DS_Store'))
print('  {} ===> {}'.format(s, sourcepath))
print('\n')


# Copy the Visual Studio solution and project files
flist = [os.path.join('..', 'msvs', 'mf6', 'mf6.sln'),
		 os.path.join('..', 'msvs', 'mf6', 'mf6.vfproj'),
		 ]
print('Copying msvs files')
for d in flist:
	print('  {} ===> {}'.format(d, msvspath)	)
	shutil.copy(d, msvspath)
print('\n')


# Copy the pymake batch and python files
flist = [os.path.join('..', 'pymake', 'makebin.py'),
		 os.path.join('..', 'pymake', 'make_win_gfortran.bat'),
		 os.path.join('..', 'pymake', 'make_win_intel.bat'),
		 ]
print('Copying pymake files')
for d in flist:
	print('  {} ===> {}'.format(d, pymakepath)	)
	shutil.copy(d, pymakepath)
print('\n')


# Copy the documentation
doclist = [os.path.join('..', 'doc', 'GwfModelReport', 'GwfModelReport.01.pdf'),
           os.path.join('..', 'doc', 'ReleaseNotes', 'ReleaseNotes.pdf'),
		   os.path.join('..', 'doc', 'UserGuide', 'userguide.pdf'),
		   os.path.join('..', 'doc', 'ConverterGuide', 'converter_mf5to6.pdf'),
		   os.path.join('..', 'doc', 'XT3DReport', 'XT3DReport.01.pdf'),
		   ]
print('Copying documentation')
for d in doclist:
	print('  {} ===> {}'.format(d, docpath)	)
	shutil.copy(d, docpath)
print('\n')


# Copy the example problems
exsrcpath = '../examples'
examplelist = [
    ['test021_twri', 'twri'],
    ['AdvGW_tidal', 'tidal'],
    ['test003_gwfs', 'flow2d'],
    ['test004_bcfss', 'bcf2ss'],
    ['test035_fhb', 'fhb'],
    ['test030_hani_col', 'hanicol'],
    ['test030_hani_row', 'hanirow'],
    ['test006_gwf3_gnc', 'mfusgP1u'],
    ['test006_2models', 'mfusgP1Lgr'],
    ['test006_2models_mvr', 'P1LgrMVR'],
    ['test007_751x751', '775x751'],
    ['test028_sfr', 'sfrEx1'],
    ['test045_lake2tr', 'lakEx2'],
    ['test045_lake4ss', 'lakEx4'],
    ['test024_Reilly', 'ReillyMAW'],
    ['test023_FlowingWell', 'FlowingMAW'],
    ['test011_mflgr_ex3', 'mflgrEx3'],
    ['test019_VilhelmsenGC', 'VilhelmsenGC'],
    ['test019_VilhelmsenGF', 'VilhelmsenGF'],
    ['test019_VilhelmsenLGR', 'VilhelmsenLGR'],
    ['test020_NevilleTonkinTransient', 'neville'],
    ['test013_Zaidel', 'zaidel'],
    ['test016_Keating', 'keating'],
    ['test034_nwtp2', 'mfnwtEx2'],
    ['test014_NWTP3High', 'mfnwtEx3H'],
    ['test014_NWTP3Low', 'mfnwtEx3L'],
    ['test050_circle_island', 'islandDISV'],
    ['test008_henry', 'henry'],
    ['test046_periodic_bc', 'periodicbc'],
    ['test031_many_gwf', 'multiGWF'],
    ['test030_hani_xt3d', 'hanixt3d'],
    ]

# Create a runall.bat file in examples
frunallbat = open(os.path.join(expath, 'runall.bat'), 'w')


# For each example, copy the necessary files from the development directory
# into the distribution directory.
print('Copying examples')
for i, (exsrc, exdest) in enumerate(examplelist):
    srcpath = os.path.join(exsrcpath, exsrc)

    prefix = 'ex{:02d}-'.format(i + 1)
    destfoldername = prefix + exdest
    dstpath = os.path.join(expath, prefix + exdest)
    print('  {:<35} ===> {:<20}'.format(exsrc, prefix + exdest))

    # Copy all of the mf6 input from srcpath to dstpath
    extrafiles = ['description.txt']
    mf6pyutil.copy_mf6_input(srcpath, dstpath, extrafiles=extrafiles)

    # Create a batch file for running the model
    fname = os.path.join(dstpath, 'run.bat')
    with open(fname, 'w') as f:
        s = r'..\..\bin\mf6.exe'
        f.write(s + '\n')
        s = 'pause'
        f.write(s + '\n')

    # Create a batch file for running the model without pausing
    fname = os.path.join(dstpath, 'run_nopause.bat')
    with open(fname, 'w') as f:
        s = r'..\..\bin\mf6.exe'
        f.write(s + '\n')

    frunallbat.write('cd ' + destfoldername + '\n')
    frunallbat.write('call run_nopause.bat' + '\n')
    frunallbat.write('cd ..' + '\n\n')
print('\n')

frunallbat.write('pause' + '\n')
frunallbat.close()

# Zip the distribution
zipname = version + '.zip'
if os.path.exists(zipname):
    print('Removing existing file: {}'.format(zipname))
    os.remove(zipname)
print('Creating zipped file: {}'.format(zipname))
zipdir(dest, zipname)
print('\n')

print('Done...')
print('\n')