from __future__ import print_function
import os
import shutil
import config


def test_teardown():

    if os.path.isdir(config.bindir):
        print('Removing folder ' + config.bindir)
        shutil.rmtree(config.bindir)

    if os.path.isfile(config.target):
        print('Removing ' + config.target)
        os.remove(config.target)

    if os.path.isdir(config.testdir):
        print('Removing folder ' + config.testdir)
        shutil.rmtree(config.testdir)

    return


if __name__ == '__main__':
    test_teardown()
