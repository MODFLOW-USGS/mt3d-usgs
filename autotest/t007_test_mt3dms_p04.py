import os
import sys
import numpy as np

try:
    import pymake
except:
    msg = 'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    raise Exception(msg)

try:
    import flopy
except:
    msg = 'Error. FloPy package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install flopy'
    raise Exception(msg)

import config

exe_name_mf = config.target_dict['mf2005']
exe_name_mt3dms = config.target_dict['mt3dms']
exe_name_mt3dusgs = os.path.abspath(config.target)
testdir = './temp'
testgroup = 'mt3dms_p04'

def p04mt3d(exe_name_mf, exe_name_mt, model_ws, mixelm):
            	
    nlay = 1
    nrow = 100
    ncol = 100
    delr = 10
    delc = 10
    delv = 1
    Lx = (ncol - 1) * delr
    Ly = (nrow - 1) * delc
    Ls = np.sqrt(Lx ** 2 + Ly ** 2)
    v = 1.
    prsity = 0.14
    q = v * prsity
    al = 2.
    trpt = .1
    q0 = 0.01
    c0 = 1000.

    perlen_mf = 1000.
    perlen_mt = 1000.
    hk = 1.
    laytyp = 0

    modelname_mf = 'p04_mf'
    mf = flopy.modflow.Modflow(modelname=modelname_mf, model_ws=model_ws, exe_name=exe_name_mf)
    dis = flopy.modflow.ModflowDis(mf, nlay=nlay, nrow=nrow, ncol=ncol,
                                   delr=delr, delc=delc, top=0., botm=[0 - delv],
                                   perlen=perlen_mf)
    ibound = np.ones((nlay, nrow, ncol), dtype=np.int) * -1
    ibound[:, 1:nrow - 1, 1:ncol-1] = 1

    # set strt as a linear gradient at a 45 degree angle
    h1 = q * Ls
    x = dis.sr.xcentergrid
    y = dis.sr.ycentergrid
    a = -1
    b = -1
    c = 1
    d = abs(a*x + b*y + c) / np.sqrt(2)
    strt = h1 - d / Ls * h1

    bas = flopy.modflow.ModflowBas(mf, ibound=ibound, strt=strt)
    lpf = flopy.modflow.ModflowLpf(mf, hk=hk, laytyp=laytyp)
    wel = flopy.modflow.ModflowWel(mf, stress_period_data=[[0, 79, 20, q0]])
    pcg = flopy.modflow.ModflowPcg(mf)
    lmt = flopy.modflow.ModflowLmt(mf)
    mf.write_input()
    mf.run_model(silent=True)

    modelname_mt = 'p04_mt'
    mt = flopy.mt3d.Mt3dms(modelname=modelname_mt, model_ws=model_ws, 
                           exe_name=exe_name_mt, modflowmodel=mf)
    btn = flopy.mt3d.Mt3dBtn(mt, icbund=1, prsity=prsity, sconc=0)
    dceps = 1.e-5
    nplane = 1
    npl = 0
    nph = 16
    npmin = 2
    npmax = 32
    dchmoc=1.e-3
    nlsink = nplane
    npsink = nph
    adv = flopy.mt3d.Mt3dAdv(mt, mixelm=mixelm, dceps=dceps, nplane=nplane, 
                             npl=npl, nph=nph, npmin=npmin, npmax=npmax,
                             nlsink=nlsink, npsink=npsink, percel=0.5)
    dsp = flopy.mt3d.Mt3dDsp(mt, al=al, trpt=trpt)
    spd = {0:[0, 79, 20, c0, 2]}
    ssm = flopy.mt3d.Mt3dSsm(mt, stress_period_data=spd)
    gcg = flopy.mt3d.Mt3dGcg(mt)
    mt.write_input()
    fname = os.path.join(model_ws, 'MT3D001.UCN')
    if os.path.isfile(fname):
        os.remove(fname)
    mt.run_model(silent=True)
    
    fname = os.path.join(model_ws, 'MT3D001.UCN')
    ucnobj = flopy.utils.UcnFile(fname)
    times = ucnobj.get_times()
    conc = ucnobj.get_alldata()

    fname = os.path.join(model_ws, 'MT3D001.OBS')
    if os.path.isfile(fname):
        cvt = mt.load_obs(fname)
    else:
        cvt = None

    fname = os.path.join(model_ws, 'MT3D001.MAS')
    mvt = mt.load_mas(fname)

    return mf, mt, conc, cvt, mvt



def test_mt3dmsp04a():  
    
    mixelm = 1


    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p04mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, mixelm)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p04mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, mixelm)

    msg = 'concentrations not equal {} {}'.format(conc_mt3dusgs, conc_mt3dms)
    assert  np.allclose(conc_mt3dusgs, conc_mt3dms, atol=6.0e-4), msg
    return


def test_mt3dmsp04b(): 
    
    mixelm = 0

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'b')
    mf, mt, conc_mt3dusgs, cvt, mvt = p04mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, mixelm)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p04mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, mixelm)

    msg = 'concentrations not equal {} {}'.format(conc_mt3dusgs, conc_mt3dms)
    assert np.allclose(conc_mt3dusgs, conc_mt3dms, atol=1e-4), msg
    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))
    test_mt3dmsp04a()
    test_mt3dmsp04b()
