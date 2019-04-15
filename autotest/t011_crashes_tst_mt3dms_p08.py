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
    from flopy.utils.util_array import read1d
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
testgroup = 'mt3dms_p08'
datadir = os.path.join('mt3dms')

def p08mt3d(exe_name_mf, exe_name_mt, model_ws, mixelm):
            	
    nlay = 27
    nrow = 1
    ncol = 50
    delr = 5
    delc = 1
    delv = 0.25
    prsity = 0.35
    al = 0.5
    trpt = 0.01
    trpv = 0.01
    dmcoef = 1.34e-5 / 100 / 100 * 86400
    rech = 0.1 / 365 # m/d

    perlen_mf = 1
    perlen_mt = [5 * 365, 15 * 365]

    k1 = 5e-4 / 100. * 86400 # m/d
    k2 = 1e-2 / 100. * 86400 # m/d
    hk = k1 * np.ones((nlay, nrow, ncol), dtype=np.float)
    hk[11:19, :, 0:24] = k2
    hk[11:19, :, 36:] = k2
    laytyp = 6 * [1] + 21 * [0]

    modelname_mf = 'p08_mf'
    mf = flopy.modflow.Modflow(modelname=modelname_mf, model_ws=model_ws, 
                               exe_name=exe_name_mf)
    dis = flopy.modflow.ModflowDis(mf, nlay=nlay, nrow=nrow, ncol=ncol,
                                   delr=delr, delc=delc, top=6.75, 
                                   botm=[6.75 - delv * k for k in range(1, nlay + 1)],
                                   perlen=perlen_mf)
    f = open(os.path.join(datadir, 'p08shead.dat'))
    strt = np.empty((nlay * ncol), dtype=np.float)
    strt = read1d(f, strt).reshape((nlay, nrow, ncol))
    f.close()
    ibound = np.ones((nlay, nrow, ncol), dtype=np.int)
    ibound[5:, :, -1] = -1
    ibound[strt < 0] = 0

    bas = flopy.modflow.ModflowBas(mf, ibound=ibound, strt=strt)
    lpf = flopy.modflow.ModflowLpf(mf, hk=hk, vka=hk, laytyp=laytyp)
    rch = flopy.modflow.ModflowRch(mf, rech=rech)
    pcg = flopy.modflow.ModflowPcg(mf)
    lmt = flopy.modflow.ModflowLmt(mf)
    mf.write_input()
    mf.run_model(silent=True)

    modelname_mt = 'p08_mt'
    mt = flopy.mt3d.Mt3dms(modelname=modelname_mt, model_ws=model_ws, 
                           exe_name=exe_name_mt, modflowmodel=mf)
    btn = flopy.mt3d.Mt3dBtn(mt, icbund=1, prsity=prsity, sconc=0, nper=2,
                             perlen=perlen_mt, timprs=[8 * 365, 12 * 365, 20 * 365])
    percel = 1.0
    itrack = 3
    wd = 0.5
    dceps = 1.e-5
    nplane = 0
    npl = 0
    nph = 10
    npmin = 2
    npmax = 20
    dchmoc=1.e-3
    nlsink = nplane
    npsink = nph
    adv = flopy.mt3d.Mt3dAdv(mt, mixelm=mixelm, dceps=dceps, nplane=nplane, 
                             npl=npl, nph=nph, npmin=npmin, npmax=npmax,
                             nlsink=nlsink, npsink=npsink, percel=percel, 
                             itrack=itrack, wd=wd)
    dsp = flopy.mt3d.Mt3dDsp(mt, al=al, trpt=trpt, trpv=trpv, dmcoef=dmcoef)
    crch1 = np.zeros((nrow, ncol), dtype=np.float)
    crch1[0, 9:18] = 1.
    cnc0 = [(0, 0, j, 1, -1) for j in range(8, 16)]
    cnc1 = [(0, 0, j, 0., -1) for j in range(8, 16)]
    ssmspd = {0: cnc0, 1:cnc1}
    ssm = flopy.mt3d.Mt3dSsm(mt, stress_period_data=ssmspd)
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


def test_mt3dmsp08a():  
    
    mixelm  = 3

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p08mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, mixelm)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p08mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, mixelm)

    np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms, atol=1.0e-4)
    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))
    
    # Two-dimensional vertical transport in a heterogeneous aquifer
    test_mt3dmsp08a()
