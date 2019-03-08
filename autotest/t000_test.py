from __future__ import print_function
import sys
import os
import shutil
import pymake
import config


def getmfexes(pth='.', version='', platform=None):
    """
    Get the latest MODFLOW binary executables from a github site
    (https://github.com/MODFLOW-USGS/executables) for the specified
    operating system and put them in the specified path.

    Parameters
    ----------
    pth : str
        Location to put the executables (default is current working directory)

    version : str
        Version of the MODFLOW-USGS/executables release to use.

    platform : str
        Platform that will run the executables.  Valid values include mac,
        linux, win32 and win64.  If platform is None, then routine will
        download the latest asset from the github reposity.

    """

    # Determine the platform in order to construct the zip file name
    if platform is None:
        if sys.platform.lower() == 'darwin':
            platform = 'mac'
        elif sys.platform.lower().startswith('linux'):
            platform = 'linux'
        elif 'win' in sys.platform.lower():
            is_64bits = sys.maxsize > 2 ** 32
            if is_64bits:
                platform = 'win64'
            else:
                platform = 'win32'
        else:
            errmsg = ('Could not determine platform'
                      '.  sys.platform is {}'.format(sys.platform))
            raise Exception(errmsg)
    else:
        assert platform in ['mac', 'linux', 'win32', 'win64']
    zipname = '{}.zip'.format(platform)

    # Wanted to use github api, but this is timing out on travis too often
    #mfexes_repo_name = 'MODFLOW-USGS/executables'
    # assets = repo_latest_assets(mfexes_repo_name)

    # Determine path for file download and then download and unzip
    url = ('https://github.com/MODFLOW-USGS/executables/'
           'releases/download/{}/'.format(version))
    assets = {p: url + p for p in ['mac.zip', 'linux.zip',
                                   'win32.zip', 'win64.zip']}
    download_url = assets[zipname]
    pymake.download_and_unzip(download_url, pth)

    return


def update_mt3dfiles(srcdir):
    # Replace the getcl command with getarg
    f1 = open(os.path.join(srcdir, 'mt3dms5.for'), 'r')
    f2 = open(os.path.join(srcdir, 'mt3dms5.for.tmp'), 'w')
    for line in f1:
        f2.write(line.replace('CALL GETCL(FLNAME)', 'CALL GETARG(1,FLNAME)'))
    f1.close()
    f2.close()
    os.remove(os.path.join(srcdir, 'mt3dms5.for'))
    shutil.move(os.path.join(srcdir, 'mt3dms5.for.tmp'),
                os.path.join(srcdir, 'mt3dms5.for'))

    # Replace filespec with standard fortran
    l = '''
          CHARACTER*20 ACCESS,FORM,ACTION(2)
          DATA ACCESS/'STREAM'/
          DATA FORM/'UNFORMATTED'/
          DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
    '''
    fn = os.path.join(srcdir, 'FILESPEC.INC')
    if os.path.isfile(fn):
        os.remove(fn)
    f = open(fn, 'w')
    f.write(l)
    f.close()

    return


def test_compile_dev():
    # Compile development version of the program from source.

    # Compile
    target = config.target
    pymake.main(config.srcdir, target, config.fc, 'gcc', makeclean=True,
                expedite=False, dryrun=False, double=False, debug=False,
                include_subdirs=False, arch=config.target_arch,
                fflags=config.fflags)

    # Ensure target has been built
    assert os.path.isfile(target) is True, 'Target {} does not exist.'.format(target)

    return


def test_download_assets():
    getmfexes('temp/bin', '1.0')
    return


# def test_compile_ref():
#     # Compile reference version of the program from the source.
#
#     # Remove the existing distribution directory if it exists
#     dir_release = config.dir_release
#     if os.path.isdir(dir_release):
#         print('Removing folder ' + dir_release)
#         shutil.rmtree(dir_release)
#
#     # Setup variables
#     srcdir = config.srcdir_release
#     target = config.target_release
#
#     # Copy MT3DMS into our test folder
#     shutil.copytree(config.loc_release, dir_release)
#
#     # Modify the MT3D source files so that it compiles properly
#     update_mt3dfiles(srcdir)
#
#     # compile
#     pymake.main(srcdir, target, config.fc, 'gcc', makeclean=True,
#                 expedite=False, dryrun=False, double=False, debug=False,
#                 include_subdirs=False, arch=config.target_arch)
#
#     assert os.path.isfile(target), 'Target {} does not exist.'.format(target)
#
#     return

if __name__ == '__main__':
    test_download_assets()
    test_compile_dev()
    #test_compile_ref()
