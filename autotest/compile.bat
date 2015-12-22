call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2016.0.110\windows\bin\compilervars.bat" intel64
echo mt_dsp5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_dsp5.for
echo mt_gcg5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_gcg5.for
echo mt_ssm5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_ssm5.for
echo mt3dms5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt3dms5.for
echo mt_rct5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_rct5.for
echo mt_adv5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_adv5.for
echo mt_btn5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_btn5.for
echo mt_fmi5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_fmi5.for
echo mt_hss5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_hss5.for
echo mt_tob5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_tob5.for
echo mt_utl5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -c /module:.\mod_temp\ /object:.\obj_temp\ .\src_temp\mt_utl5.for
ifort.exe -heap-arrays:0 -fpe:0 -traceback -nologo -O2 -o temp\mt3d-usgs_5.3.00.exe .\obj_temp\*.obj
