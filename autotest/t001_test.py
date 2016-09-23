from __future__ import print_function
import os
import flopy
import pymake
from pymake.autotest import get_namefiles, compare_budget, compare_heads
import config

#  1) 'Saturated_Transient_Storage'
#  2) 'lkt'
#  3) 'SFT_CrnkNic'
#  4) 'SFT_Full_Imp'
#  5) 'UZT_Disp_Lamb01'
#  6) 'UZT_Disp_Lamb01_TVD'
#  7) 'UZT_Disp_Lamb1'
#  8) 'UZT_Disp_Lamb10'
#  9) 'Keating'
# 10) 'Keating_UZF'
# 11) 'UZT_NonLin'
# 12) 'UZT_NonEq'
# 13) 'cts0'
# 14) 'cts1'
# 15) 'cts2'
# 16) 'cts3'
# 17) 'cts4'
# 18) 'Saturated_Transient_Storage'
# 19) 'drycell'
# 20) 'gwt'
# 21) 'Legacy99Storage'
# 22) 'AltWTSorb'

test_dirs = ['Saturated_Transient_Storage', 'lkt', 'SFT_CrnkNic', 'SFT_Full_Imp', 'UZT_Disp_Lamb01', 'UZT_Disp_Lamb01_TVD', 'UZT_Disp_Lamb1', 'UZT_Disp_Lamb10', 'Keating', 'Keating_UZF', 'UZT_NonLin', 'UZT_NonEq', 'cts0', 'cts1', 'cts2', 'cts3', 'cts4', 'Saturated_Transient_Storage', 'drycell', 'gwt', 'Legacy99Storage', 'AltWTSorb']  #'Saturated_Transient_Storage', 'lkt', 'SFT_CrnkNic', 'SFT_Full_Imp', 'UZT_Disp_Lamb01', 'UZT_Disp_Lamb01_TVD', 'UZT_Disp_Lamb1', 'UZT_Disp_Lamb10', 'Keating', 'Keating_UZF', 'UZT_NonLin', 'UZT_NonEq', 'cts0', 'cts1', 'cts2', 'cts3', 'cts4', 'Saturated_Transient_Storage', 'drycell', 'gwt', 'Legacy99Storage', 'AltWTSorb'

def run_mt3d(mfnamefile, mtnamefile, comparison=True):
    """
    Run the simulations.

    """
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
                                    silent=True)

    if success:
        print('running mt3d-usgs model...{}'.format(testname))
        nam = os.path.basename(mtnamefile)
        exe_name = os.path.abspath(config.target)
        success, buff = flopy.run_model(exe_name, nam, model_ws=testpth,
                                        silent=True,
                                        normal_msg='program completed')

    success_cmp = True
    if comparison:
        action = pymake.setup_comparison(mfnamefile, testpth)
        action = pymake.setup_comparison(mtnamefile, testpth, 
                                         remove_existing=False)
        testpth_cmp = os.path.join(testpth, action)
        if action is not None:
            files_cmp = None
            if action.lower() == '.cmp':
                files_cmp = []
                files = os.listdir(testpth_cmp)
                for file in files:
                    files_cmp.append(
                            os.path.abspath(os.path.join(testpth_cmp, file)))
                success_cmp = True
                #print(files_cmp)
            else:
                print('running comparison modflow-nwt model...{}'.format(testpth_cmp))
                key = action.lower().replace('.cmp', '')
                nam = os.path.basename(mfnamefile)
                exe_name = os.path.abspath(config.target_dict['mfnwt'])
                success_cmp, buff = flopy.run_model(exe_name, nam,
                                                    model_ws=testpth_cmp,
                                                    silent=True)
                if success_cmp:
                    print('running comparison mt3dms model...{}'.format(testpth_cmp))
                    key = action.lower().replace('.cmp', '')
                    nam = os.path.basename(mtnamefile)
                    exe_name = os.path.abspath(config.target_release)
                    success_cmp, buff = flopy.run_model(exe_name, nam,
                                                        model_ws=testpth_cmp,
                                                        silent=True,
                                                        normal_msg='program completed')
            #print('success: ', success)
            #print('success_cmp: ', success_cmp)
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
        # -- get modflow name files
        namefilesmf = []
        pth = config.testpaths[0]
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
        pth = config.testpaths[0]
        tpth = os.path.join(pth, spth)
        namefilesmf += get_namefiles(tpth, exclude='mt')
        # -- get mt3d name files
        namefilesmt = []
        tpth = os.path.join(pth, spth)
        namefilesmt += get_namefiles(tpth, exclude='mf')
        # -- process name files
        for (nm1, nm2) in zip(namefilesmf, namefilesmt):
            run_mt3d(nm1, nm2)
