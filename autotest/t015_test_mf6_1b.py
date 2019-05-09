from __future__ import print_function
import os
import flopy
import pymake
from pymake.autotest import get_namefiles, compare_budget, compare_heads
import config


test_dirs = ['ImportedP1_Case1b', #  List tests to carry out here.
             'ImportedP1_Case1d',
             'ImportedP2_nonequilibrium',
             'ImportedP2_nonlinear',
             'ImportedP3']


def run_mt3d(mf5namefile, mf6namefile, mtnamefile, regression=True):
    """
    Run the simulations.

    """

    # Set root as the directory name where namefile is located
    flowexe = config.target_dict['mfnwt']
    if '_mf2k' in mf5namefile:
        crep = '_mf2k'
        flowexe = config.target_dict['mf2000']
    elif '_mf2005' in mf5namefile:
        crep = '_mf2005'
        flowexe = config.target_dict['mf2005']
    else:
        crep = '_mf'
        flowexe = config.target_dict['mfnwt']
    testname = pymake.get_sim_name(mf5namefile.replace(crep, ''),
                                   rootpth=os.path.dirname(mf5namefile))[0]

    # Setup modflow
    testpth = os.path.join(config.testdir, testname)
    pymake.setup(mf5namefile, testpth)

    # run modflow5-variant (whether NWT or 2k5) to generate flow field for mt3d-usgs
    print('running modflow5 model...{}'.format(testname))
    nam = os.path.basename(mf5namefile)
    exe_name = flowexe
    success, buff = flopy.run_model(exe_name, nam, model_ws=testpth,
                                    silent=False, report=True)

    # Setup and run MF6 files for comparing with MF5-linked results
    flowexe = config.target_dict['mf6']
    exe_name = flowexe
    nam = os.path.basename(mf6namefile)
    pymake.setup_mf6(os.path.dirname(mf6namefile), testpth, remove_existing=False)

    cwd = os.getcwd()
    os.chdir(os.path.join(testpth))
    os.system(os.path.join('..','bin','mf6.exe'))
    os.chdir(cwd)

    # Setup mt3d
    pymake.setup(mtnamefile[0], testpth, remove_existing=False,
                 extrafiles=mtnamefile[1])

    # if modflow ran successfully, then run mt3d-usgs
    success_mf5 = success_mf6 = False
    if success:
        for i, nmfile in enumerate(mtnamefile):
            print('running mt3d-usgs model...{}'.format(nmfile))
            nam = os.path.basename(nmfile)
            exe_name = os.path.abspath(config.target)
            success, buff = flopy.run_model(exe_name, nam, model_ws=testpth,
                                            silent=False, report=True,
                                            normal_msg='program completed')
            if i==0:
                success_mf5 = success
            elif i > 0:
                success_mf6 = success

    # run ucn comparison
    if success_mf5 and success_mf6:
        success_cmp = True
    else:
        success_cmp = False

    success_reg = False
    if regression:
        if success_cmp:
            # if mt3d-usgs ran, then time to do a comparison
            #ucnfile1 = os.path.join(testpth, os.path.split(testpth)[-1] + '_Comp1_4mf5.ucn')
            #ucnfile2 = os.path.join(testpth, os.path.split(testpth)[-1] + '_Comp1_4mf6.ucn')
            namefile1 = os.path.join(testpth, os.path.split(mtnamefile[0])[-1])
            namefile2 = os.path.join(testpth, os.path.split(mtnamefile[1])[-1])
            outfileucn = os.path.join(testpth, os.path.split(testpth)[-1] + '_UCNcompareResults.txt')
            success_ucn = pymake.compare_concs(namefile1, namefile2,
                                               ctol=0.001,
                                               outfile=outfileucn)
            if success_ucn:
                success_reg = True
            else:
                success_reg = False


    # Clean things up
    if success and success_reg and not config.retain:
        pymake.teardown(testpth)
    assert success, 'model did not run'
    assert success_cmp, 'comparison model did not meet comparison criteria'
    return


def test_mt3d():
    for spth in test_dirs:
        # -- get modflow name files
        namefilesmf = []
        pth = config.testpaths[2]
        tpth = os.path.join(pth, spth)
        namefilesmf += get_namefiles(tpth, exclude='mt')
        # -- get mt3d name files
        namefilesmt = []
        tpth = os.path.join(pth, spth)
        namefilesmt += get_namefiles(tpth, exclude='mf')
        # -- process name files
        for (nm1, nm2) in zip(namefilesmf, namefilesmt):
            yield run_mt3d, nm1, nm2
    return


if __name__ == '__main__':
    for spth in test_dirs:
        # -- initialize name file lists
        namefilesmf5 = []
        namefilesmf6 = []
        namefilesmt = []

        # -- get pre-MF6 directory
        pth = config.testpaths[2]
        tpth = os.path.join(pth, spth, 'mf5_' + spth)
        namefilesmf5 += get_namefiles(tpth, exclude='_mt')

        # -- get pre-MF6 directory
        tpth = os.path.join(pth, spth, 'mf6_' + spth)
        namefilesmf6 += get_namefiles(tpth, exclude = spth + '.nam')

        # -- get mt3d name files
        tpth = os.path.join(pth, spth, 'mt_' + spth)
        namefilesmt += get_namefiles(tpth)

        # -- process name files
        run_mt3d(str(namefilesmf5[0]), str(namefilesmf6[0]), namefilesmt)
