from __future__ import print_function
import os
import flopy
import pymake
from pymake.autotest import get_namefiles, compare_budget, compare_heads
import config

test_dirs = ['P07', 'zeroth', 'hecht_1lay', 'hecht_13lay']  #'hsstest'

def run_mt3d(mfnamefile, mtnamefile, regression=True):
    """
    Run the simulations.

    """

    # Set root as the directory name where namefile is located
    flowexe = config.target_dict['mfnwt']
    if '_mf2k' in mfnamefile:
        crep = '_mf2k'
        flowexe = config.target_dict['mf2k']
    elif '_mf2005' in mfnamefile:
        crep = '_mf2005'
        flowexe = config.target_dict['mf2005']
    else:
        crep = '_mf'
        flowexe = config.target_dict['mfnwt']
    testname = pymake.get_sim_name(mfnamefile.replace(crep, ''), 
                                   rootpth=os.path.dirname(mfnamefile))[0]

    # Setup modflow
    testpth = os.path.join(config.testdir, testname)
    pymake.setup(mfnamefile, testpth)
    # Setup mt3d
    pymake.setup(mtnamefile, testpth, remove_existing=False)

    # run test models
    print('running modflow-nwt model...{}'.format(testname))
    nam = os.path.basename(mfnamefile)
    exe_name = config.target_dict['mfnwt']
    success, buff = flopy.run_model(exe_name, nam, model_ws=testpth,
                                    silent=True)

    if success:
        print('running mt3d-usgs model...{}'.format(testname))
        nam = os.path.basename(mtnamefile)
        exe_name = os.path.abspath(config.target)
        success, buff = flopy.run_model(exe_name, nam, model_ws=testpth,
                                        silent=True,
                                        normal_msg='program completed')

    success_cmp = True
    if regression:
        testname_reg = os.path.basename(config.target_release)
        testpth_reg = os.path.join(testpth, testname_reg)
        pymake.setup(mfnamefile, testpth_reg)
        pymake.setup(mtnamefile, testpth_reg, remove_existing=False)
        print('running regression {} model...{}'.format(os.path.basename(flowexe), testpth_reg))
        nam = os.path.basename(mfnamefile)
        exe_name = flowexe #config.target_dict['mfnwt']
        success_reg, buff = flopy.run_model(exe_name, nam,
                                            model_ws=testpth_reg,
                                            silent=True)
        if success_reg:
            print('running regression mt3dms model...{}'.format(testpth_reg))
            nam = os.path.basename(mtnamefile)
            exe_name = os.path.abspath(config.target_release)
            success_reg, buff = flopy.run_model(exe_name, nam,
                                                model_ws=testpth_reg,
                                                silent=True,
                                                normal_msg='program completed')
            if success_reg:
                nam = os.path.basename(mtnamefile)
                namefile1 = os.path.join(testpth, nam)
                namefile2 = os.path.join(testpth_reg, nam)
                outfileucn = os.path.join(
                             os.path.split(os.path.join(testpth, nam))[0],
                             'ucn.cmp')
                success_ucn = pymake.compare_concs(namefile1, namefile2,
                                                   ctol=0.001,
                                                   outfile=outfileucn)
                if success_reg and success_ucn:
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
        pth = config.testpaths[1]
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
        # -- get modflow name files
        namefilesmf = []
        pth = config.testpaths[1]
        tpth = os.path.join(pth, spth)
        namefilesmf += get_namefiles(tpth, exclude='mt')
        # -- get mt3d name files
        namefilesmt = []
        tpth = os.path.join(pth, spth)
        namefilesmt += get_namefiles(tpth, exclude='mf')
        # -- process name files
        for (nm1, nm2) in zip(namefilesmf, namefilesmt):
            run_mt3d(nm1, nm2)
