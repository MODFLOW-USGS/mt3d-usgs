from __future__ import print_function
import os
import flopy
import pymake
from pymake.autotest import get_namefiles, compare_budget, compare_heads
import config


def compare(namefile1, namefile2):
    """
    Compare the results from two simulations

    """

    # Compare budgets from the list files in namefile1 and namefile2
    outfile = os.path.join(os.path.split(namefile1)[0], 'bud.cmp')
    success1 = compare_budget(namefile1, namefile2, max_cumpd=0.01, max_incpd=0.01,
                   outfile=outfile)

    outfile = os.path.join(os.path.split(namefile1)[0], 'hds.cmp')
    success2 = compare_heads(namefile1, namefile2, htol=0.001,
                             outfile=outfile)

    success = False
    if success1 and success2:
        success = True

    return success

def run_mt3d(namefile, regression=True):
    """
    Run the simulation.

    """

    # Set root as the directory name where namefile is located
    testname = os.path.dirname(namefile).split(os.sep)[-1]

    # Set nam as namefile name without path
    nam = os.path.basename(namefile)

    # Setup
    testpth = os.path.join(config.testdir, testname)
    pymake.setup(namefile, testpth)

    # run test models
    print('running model...{}'.format(testname))
    exe_name = os.path.abspath(config.target)
    success, buff = flopy.run_model(exe_name, nam, model_ws=testpth,
                                    silent=True, report=True, 
                                    normal_msg='program completed.')
    f = open(os.path.join(testpth, 'output.dat'), 'w')
    for line in buff:
        f.write(line + '\n')
    f.close()
    assert success, 'model did not run successfully: {}'.format(testname)

    # If it is a regression run, then setup and run the model with the
    # release target
    success_reg = True
#    if regression:
#        testname_reg = os.path.basename(config.target_release)
#        testpth_reg = os.path.join(testpth, testname_reg)
#        pymake.setup(namefile, testpth_reg)
#        print('running regression model...{}'.format(testname_reg))
#        exe_name = os.path.abspath(config.target_release)
#        success, buff = flopy.run_model(exe_name, nam, model_ws=testpth_reg,
#                                        silent=True, report=True)
#        f = open(os.path.join(testpth_reg, 'output.dat'), 'w')
#        for line in buff:
#            f.write(line + '\n')
#        f.close()
#        assert success, 'model did not run successfully: {}'.format(testname_reg)
#
#        # Make comparison
#        success = compare(os.path.join(testpth, nam),
#                          os.path.join(testpth_reg, nam))
#        assert success, 'Models do not compare: {} and {}'.format(testname,
#                                                                  testname_reg)

    # Clean things up
    if success and not config.retain:
        pymake.teardown(testpth)
    assert success and success_reg

    return


def test_mt3d():
    namefiles = []
    for pth in config.testpaths:
        namefiles += get_namefiles(pth, exclude='mf2005')
    for namefile in namefiles:
        yield run_mt3d, namefile
    return


if __name__ == '__main__':
    namefiles = []
    for pth in config.testpaths:
        namefiles += get_namefiles(pth, exclude='mf2005')
    for namefile in namefiles:
        run_mt3d(namefile)
