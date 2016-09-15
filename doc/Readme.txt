

                     MT3D-USGS - Version: 1.0.0
          Release of MT3DMS with new and expanded functionality


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

MT3D-USGS version 1.0.0 is packaged for personal computers using one of the
Microsoft Windows operating systems. Executable files for personal 
computers are provided as well as the source code at the following URL:

http://dx.doi.org/10.3133/tm6A53

The executable files were compiled on a personal computer with the Intel(R) 
Xeon(R) CPU E3-1535M v5 chipset, running the Microsoft Windows 8.1 Enterprise 
operating system, using the Microsoft Visual Studio Professional 2015 Version 
14.0.25424.00 Update 3 development environment and the Intel(R) Parallel Studio
XE Composer Edition for Fortran Windows Version 16.0.0062.14 compiler. The 
source code is provided to aid users in compilation on other computers. 
However, no support is provided for compilation.

The source files also were compiled with GNU Fortran ('gfortran') version
4.7.0 20111220 to ensure syntax will enable compilation in gfortran.  However, 
the executables produced by gfortran are not distributed with the software.

IMPORTANT: Users should review the file Summary_MT3D-USGS.txt for a description
of, and references for, this software. Users should also review the file 
release.txt, which describes changes that have been introduced into MT3D-USGS
with each official release; these changes may substantially affect users.

Instructions for installation, execution, and testing of this version of
MT3D-USGS are provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING
                         F. REFERENCES


A. DISTRIBUTION FILE

The following distribution file is for use on personal computers:

         MT3D-USGS_1.0.0.zip

The distribution file contains:

          Executables and source code for MT3D-USGS.
          MT3D-USGS documentation.
          Four MT3D-USGS sample problems.

The MT3D-USGS program and related files can be extracted from the 
distribution file.  Extracting these files is all that is required 
for installation.  It is recommended that the directory structure
in the zip file be maintained when the files are extracted.  The 
distribution file contains the following directory structure.


   |
   |--MT3D-USGS_1.0.0
   |    |--bin            ; Compiled MT3D-USGS executables for personal 
   |    |                       computers
   |    |--data           ; Sample problems
   |    |--doc            ; Documentation report for MT3D-USGS 
   |    |                   LMT8 documentation for MF-NWT and format of Flow-
   |    |                       Transport Link File
   |    |                   Input Instructions
   |    |--output_test_64 ; Output files from running the sample problems, 
   |    |                       64-bit versions
   |    |--src            ; Source code for MT3D-USGS

Included in directory MT3D-USGS_1.0.0\doc is the MT3D-USGS documentation 
report, which is a Portable Document Format (PDF) file. The PDF file is 
readable and printable on various computer platforms using Acrobat Reader 
from Adobe. The Acrobat Reader is freely available from the following World
Wide Web site:
      http://www.adobe.com/


B. INSTALLING

To make the executable versions of MT3D-USGS accessible from any
directory, the directory containing the executables (MT3D-USGS_1.0.0\bin)
should be included in the PATH environment variable.  

As an alternative, the executable files, MT3D-USGS.exe and MT3D-USGS_64.exe, 
in the MT3D-USGS_1.0.0\bin directory can be copied into a directory already
included in the PATH environment variable.


C. EXECUTING THE SOFTWARE

A 32-bit and a 64-bit executable are provided in the MT3D-USGS_1.0.0\bin  
directory. Two executables are provided because computers often use either  
the 32-bit Windows XP or the 64-bit Windows 7 (or higher) operating systems.  
Large simulations may not run on a 32-bit operating system due to limitations 
in the amount of available random access memory (RAM). A 64-bit operating
system provides much more available RAM than a 32-bit operating system. 
Thus, it is recommended that a 64-bit executable be used on a 64-bit operating  
system for large simulations.   

After the executable files in the MT3D-USGS_1.0.0\bin directory are installed in
a directory that is included in your PATH, MT3D-USGS is initiated in
a Windows Command-Prompt window using the commands:

      MT3D-USGS_32.exe [Fname]

or
      MT3D-USGS_64.exe [Fname]

The optional Fname argument is the name of the MT3D-USGS Name File.

MT3D-USGS will not work if a flow-transport link file has not first been 
generated by MODFLOW-2005 (Harbaugh, 2005) or MF-NWT (Niswonger and others, 
2011).  In order to take advantage of MT3D-USGS's new capabilities, a version
of MF-NWT greater than or equal to version 1.1.1 must be run with the LMT 
package activated by the LMT6 keyword appearing in the name file.  

The data arrays in MT3D-USGS are dynamically allocated, so models are not
limited by hard-coded array limits. However, it is best to have at least 
2 GB of RAM available to hold all of the required data. If there is 
less available RAM than the model requires, which depends on the size of the 
application, the program will use virtual memory; however, this can
slow execution significantly. If there is insufficient RAM to run
the model, then MT3D-USGS will not initiate the beginning of the 
simulation; however, the Windows Command-Prompt window may continue to 
indicate that MT3D-USGS is executing. For this circumstance, the program 
must be terminated manually using the Windows Task Manager application.

Some of the files written by MT3D-USGS are unformatted files. The structure
of these files depends on the compiler and options in the Fortran write
statement.  MT3D-USGS is compiled with the unformatted file type specified
as "BINARY". "BINARY" is a non-standard Fortran form option that can be
used with the Intel Fortan compiler.  Any program that reads the unformatted 
files produced by MT3D-USGS must be compiled with a compiler that produces 
programs that use the same structure for unformatted files.  


D. TESTING

Fifteen sample problems are distributed with MT3D-USGS to verify that 
MT3D-USGS is correctly installed and running on the system.  The sample 
problems also may be looked at as examples of how to use the program. Some of 
the test problems (all of the UZT_* problems) are compared to analytical 
solutions in Morway and others (2013), one test problem (gwt) is documented in 
Prudic and others (2004) as Test Simulation 2, one test problem (lkt) is 
documented in Merritt and Konikow (2000) as test simulation 1, one test problem 
(SFT_CrnkNic) is documented in Runkel (1998), and one test problem (Keating)
is documented in Keating and Zyvoloski (2009). Other test problems were added
to provide examples of the new input options. These test problems can be run 
using either the 32-bit or 64-bit version of the MT3D-USGS executable. Saved 
results for these simulations are included in the output_test_64 directory for 
comparison.

E. COMPILING

The executable files provided in MT3D-USGS_1.0.0\bin were created using the 
Intel(R) Parallel Studio XE Composer Edition compiler. Although executable 
versions of the program are provided, the source code also is provided in the 
MT3D-USGS_1.0.0\src directory so that MT3D-USGS can be recompiled if necessary.
However, the USGS cannot provide assistance to those compiling MT3D-USGS.

F. REFERENCES

Harbaugh, A.W., 2005, MODFLOW-2005: The U.S. Geological Survey Modular Ground-
water Model–the Ground-water Flow Process: U.S. Geological Survey Techniques and
Methods 6-A16, p. 253. Available online at
http://water.usgs.gov/ogw/modflow/MODFLOW.html

Keating, E., and Zyvoloski, G., 2009, A stable and efficient numerical algorithm
for unconfined aquifer analysis: Groundwater, v. 47, no. 4, p. 569–579.
Available at 
http://onlinelibrary.wiley.com/doi/10.1111/j.1745-6584.2009.00555.x/full

Merritt, M.L., and Konikow, K.F., 2000, Documentation of a computer program to
simulate lake-aquifer interaction using the MODFLOW ground-water flow model and
the MOC3D solute-transport model: Water-Resources Investigations Report 00-4167,
146 p. Available online at http://pubs.er.usgs.gov/publication/wri004167

Morway, E.D., Niswonger, R.G., Langevin, C.D., Bailey, R.T., and Healy, R.W.,
2013, Modeling variably saturated subsurface solute transport with MODFLOW-UZF
and MT3DMS: Groundwater, v. 51, no. 2, p. 237–251. Available at
http://onlinelibrary.wiley.com/doi/10.1111/j.1745-6584.2012.00971.x/full

Niswonger, R.G., Panday, Sorab, and Ibaraki, Motomu, 2011, MT3D-USGS, A Newton
formulation for MODFLOW-2005: U.S. Geological Survey Techniques and Methods 6-
A37, 44 p. Available online at http://pubs.usgs.gov/tm/tm6a37/

Prudic, D.E., Konikow, L.F., and Banta, E.R., 2004, A new Streamflow-Routing
(SFR1) Package to simulate stream-aquifer interaction with MODFLOW-2000: U.S.
Geological Survey Open-File Report 2004–1042, 96 p.  Available at
https://pubs.er.usgs.gov/publication/ofr20041042

Runkel, R.L., 1998, One-Dimensional Transport with Inflow and Storage (OTIS): A
Solute Transport Model for Streams and Rivers: U.S. Geological Survey Water-
Resources Investigations Report 98–4018, 73 p. Available at 
https://pubs.er.usgs.gov/publication/wri984018