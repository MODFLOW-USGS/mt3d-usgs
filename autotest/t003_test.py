from __future__ import print_function
import os
import flopy
import pymake
from pymake.autotest import get_namefiles
import config
import sys

print(os.getcwd())
sys.path.insert(0, './../test-cmp/UZT_NonEq/')

import insert_stopflow_period


# tests that fail or take too long have been commented out in
# order to get testing working on Travis
test_dirs = ['UZT_NonEq']


def run_mt3d(spth, comparison=True):
    """
    Run the simulations.

    """

    # Path to folder containing tests
    pth = config.testpaths[0]

    # -- get modflow name files
    tpth = os.path.join(pth, spth)
    namefilesmf = []
    namefilesmf += get_namefiles(tpth, exclude='mt')

    # -- get mt3d name files
    tpth = os.path.join(pth, spth)
    namefilesmt = []
    namefilesmt += get_namefiles(tpth, exclude='mf')

    mfnamefile = namefilesmf[0]
    mtnamefile = namefilesmt[0]
    print(mfnamefile, mtnamefile)

    # Set root as the directory name where namefile is located
    testname = pymake.get_sim_name(mfnamefile.replace('_mf', ''),
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
                                    silent=False, report=True)

    if success:
        # For this particular test, a lengthy period of "stop flow"
        # needs to be inserted into the linker file for simulating
        # a period of equilibration.
        print('running insert_stopflow_period.py...{}'.format(testname))
        insert_stopflow_period.InsStpFlw(testpth)

        print('running mt3d-usgs model...{}'.format(testname))
        nam = os.path.basename(mtnamefile)
        exe_name = os.path.abspath(config.target)
        success, buff = flopy.run_model(exe_name, nam, model_ws=testpth,
                                        silent=False, report=True,
                                        normal_msg='program completed')

    success_cmp = True
    if success and comparison:
        action = pymake.setup_comparison(mfnamefile, testpth)
        action = pymake.setup_comparison(mtnamefile, testpth,
                                         remove_existing=False)
        testpth_cmp = os.path.join(testpth, action)
        if action is not None:
            files_cmp = None
            if action.lower() == '.cmp':
                files_cmp = []
                files = os.listdir(testpth_cmp)

                # Go through all files in the .cmp folder and do a separate
                # comparison for each one.  This will ensure that the
                # individual ucn files for sorbed and multi-species will be
                # compared.
                for file in files:
                    files1 = os.path.join(testpth, file[:-4])
                    files2 = os.path.join(testpth_cmp, file)
                    outfileucn = os.path.join(testpth, file + '.txt')
                    success_ucn = pymake.compare_concs(None, None,
                                                       ctol=0.002,
                                                       outfile=outfileucn,
                                                       files1=files1,
                                                       files2=files2)
                    if not success_ucn:
                        success_cmp = False

            else:
                print('running comparison modflow-nwt model...{}'.format(testpth_cmp))
                key = action.lower().replace('.cmp', '')
                nam = os.path.basename(mfnamefile)
                exe_name = os.path.abspath(config.target_dict['mfnwt'])
                success_cmp, buff = flopy.run_model(exe_name, nam,
                                                    model_ws=testpth_cmp,
                                                    silent=False, report=True)
                if success_cmp:
                    print('running comparison mt3dms model...{}'.format(testpth_cmp))
                    key = action.lower().replace('.cmp', '')
                    nam = os.path.basename(mtnamefile)
                    exe_name = os.path.abspath(config.target_release)
                    success_cmp, buff = flopy.run_model(exe_name, nam,
                                                        model_ws=testpth_cmp,
                                                        silent=False,
                                                        report=True,
                                                        normal_msg='program completed')

                if success_cmp:
                    nam = os.path.basename(mtnamefile)
                    namefile1 = os.path.join(testpth, nam)
                    namefile2 = os.path.join(testpth_cmp, nam)
                    outfileucn = os.path.join(
                                 os.path.split(os.path.join(testpth, nam))[0],
                                 'ucn.cmp')
                    success_ucn = pymake.compare_concs(namefile1, namefile2,
                                                       ctol=0.002,
                                                       outfile=outfileucn,
                                                       files2=files_cmp)

                if success_cmp and success_ucn:
                    success_cmp = True
                else:
                    success_cmp = False
    # Clean things up
    if success and success_cmp and not config.retain:
        pymake.teardown(testpth)
    assert success, 'model did not run'
    assert success_cmp, 'comparison model did not meet comparison criteria'
    return


def test_mt3d():
    for spth in test_dirs:
        yield run_mt3d, spth
    return


if __name__ == '__main__':
    for spth in test_dirs:
        run_mt3d(spth)
