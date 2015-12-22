MT3D-USGS Autotest
12.22.2015


DESCRIPTION
This folder contains autotest functionality for compiling and running MT3D-USGS.  On Windows, the autotest.bat file can be run, which will (1) compile MT3D-USG using the information in config.py, and (2) run any model found in the test_problems folder that do not have the text string 'mf2005' in the name of a name file.  Tests will pass if MT3DMS ends with 'program completed'.  Tests will fail if this string is not found in the MT3D-USGS command line output.


TODO
1.  Differentiate between regression tests and comparison tests
2.  Compile MT3DMS and make available for regression tests
3.  Add code for comparing budget tables and concentrations


DEPENDENCIES
Autotest requires the following:
1. Python (should work with both 2 and 3, but only 2 has been tested)
2. Flopy (https://github.com/modflowpy/flopy)
3. Pymake (https://github.com/modflowpy/pymake)
4. Fortran compiler; autotest can use ifort or gfortran.  If ifort is used, then autotest will look in the system path for the location of the installed Intel Fortran compiler and will use the newest one.  If gfortran is used, then gfortran must be available from the command line.  Easiest way to install gfortran on Windows is from the Anaconda Python distribution with the command: "conda install mingw"
