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
testgroup = 'mt3dms_p06'

def p06mt3d(exe_name_mf, exe_name_mt, model_ws, mixelm, dt0):
            	
    nlay = 1
    nrow = 31
    ncol = 31
    delr = 900
    delc = 900
    delv = 20
    prsity = 0.30
    al = 100.
    trpt = 1.
    q0 = 86400.
    c0 = 100.

    perlen_mf = [912.5, 2737.5]
    perlen_mt = perlen_mf
    hk = 0.005 * 86400
    laytyp = 0

    modelname_mf = 'p06_mf'
    mf = flopy.modflow.Modflow(modelname=modelname_mf, model_ws=model_ws, 
                               exe_name=exe_name_mf)
    dis = flopy.modflow.ModflowDis(mf, nlay=nlay, nrow=nrow, ncol=ncol,
                                   delr=delr, delc=delc, top=0., botm=[0 - delv],
                                   nper=2, perlen=perlen_mf)
    ibound = np.ones((nlay, nrow, ncol), dtype=np.int) * -1
    ibound[:, 1:nrow - 1, 1:ncol-1] = 1
    strt = 0.

    bas = flopy.modflow.ModflowBas(mf, ibound=ibound, strt=strt)
    lpf = flopy.modflow.ModflowLpf(mf, hk=hk, laytyp=laytyp)
    welspd = {0: [[0, 15, 15, q0]], 1: [[0, 15, 15, -q0]]}
    wel = flopy.modflow.ModflowWel(mf, stress_period_data=welspd)
    sip = flopy.modflow.ModflowSip(mf)
    lmt = flopy.modflow.ModflowLmt(mf)
    mf.write_input()
    mf.run_model(silent=True)

    modelname_mt = 'p06_mt'
    mt = flopy.mt3d.Mt3dms(modelname=modelname_mt, model_ws=model_ws, 
                           exe_name=exe_name_mt, modflowmodel=mf)
    btn = flopy.mt3d.Mt3dBtn(mt, icbund=1, prsity=prsity, sconc=0, 
                             nper=2, perlen=perlen_mt, dt0=dt0, 
                             obs=[(0, 15, 15)])
    dceps = 1.e-5
    nplane = 1
    npl = 16
    nph = 16
    npmin = 4
    npmax = 32
    dchmoc=1.e-3
    nlsink = nplane
    npsink = nph
    adv = flopy.mt3d.Mt3dAdv(mt, mixelm=mixelm, dceps=dceps, nplane=nplane, 
                             npl=npl, nph=nph, npmin=npmin, npmax=npmax,
                             nlsink=nlsink, npsink=npsink, percel=0.5)
    dsp = flopy.mt3d.Mt3dDsp(mt, al=al, trpt=trpt)
    spd = {0:[0, 15, 15, c0, 2], 1:[0, 15, 15, 0., 2]}
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



def test_mt3dmsp06a():  
    
    mixelm  = 1
    dt0     = 56.25


    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p06mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, mixelm, dt0)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p06mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, mixelm, dt0)

    np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms,
                               rtol=1e-05, atol=1.0e-4)
    return


def test_mt3dmsp06b():  
    
    mixelm  = -1
    dt0     = 56.25


    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p06mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, mixelm, dt0)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p06mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, mixelm, dt0)

    np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms,
                               rtol=1e-05, atol=1.0e-4)
    return


def test_mt3dmsp06c():  
    
    mixelm  = 0
    dt0     = 56.25


    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p06mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, mixelm, dt0)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p06mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, mixelm, dt0)

    np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms,
                               rtol=1e-05, atol=1.0e-4)
    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))
    
    # Concentration at an injection/extraction well from MT3DMS documentation
    test_mt3dmsp06a()
    test_mt3dmsp06b()
    test_mt3dmsp06c()
