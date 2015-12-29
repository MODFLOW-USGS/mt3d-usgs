from __future__ import print_function
import os
import shutil
import pymake
import config


def test_compile_dev():
    # Compile development version of the program from source.

    # Compile
    target = config.target
    pymake.main(config.srcdir, target, config.fc, 'gcc', makeclean=True,
                expedite=False, dryrun=False, double=False, debug=False,
                include_subdirs=False, arch=config.target_arch)

    # Ensure target has been built
    assert os.path.isfile(target) is True, 'Target {} does not exist.'.format(target)

    return


def test_compile_ref():
    # Compile reference version of the program from the source.

    # Remove the existing distribution directory if it exists
    dir_release = config.dir_release
    if os.path.isdir(dir_release):
        print('Removing folder ' + dir_release)
        shutil.rmtree(dir_release)

    # Setup variables
    srcdir = config.srcdir_release
    target = config.target_release

    # Copy MT3DMS into our test folder
    shutil.copytree(config.loc_release, dir_release)

    # compile
    pymake.main(srcdir, target, config.fc, 'gcc', makeclean=False,
                expedite=False, dryrun=False, double=False, debug=False,
                include_subdirs=False, arch=config.target_arch)

    assert os.path.isfile(target), 'Target {} does not exist.'.format(target)

    return

if __name__ == '__main__':
    test_compile_dev()
    test_compile_ref()
