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
testgroup = 'mt3dms_p10'
datadir = os.path.join('mt3dms')

def p10mt3d(exe_name_mf, exe_name_mt, model_ws, mixelm, perlen=1000, isothm=1, sp2=0., ttsmult=1.2):
            	
    nlay = 4
    nrow = 61
    ncol = 40
    delr = [2000, 1600, 800, 400, 200, 100] + 28*[50] + [100, 200, 400, 800, 1600, 2000]
    delc = [2000, 2000, 2000, 1600, 800, 400, 200, 100] + 45 * [50] + [100, 200, 400, 800, 1600, 2000, 2000, 2000]
    delv = 25.
    top = 780.
    botm = [top - delv * k for k in range(1, nlay + 1)]
    prsity = 0.3
    al = 10.
    trpt = 0.2
    trpv = 0.2

    perlen_mf = perlen
    perlen_mt = perlen
    hk = [60., 60., 520., 520.]
    vka = .1
    laytyp = 0

    modelname_mf = 'p10_mf'
    mf = flopy.modflow.Modflow(modelname=modelname_mf, model_ws=model_ws, 
                               exe_name=exe_name_mf)
    dis = flopy.modflow.ModflowDis(mf, nlay=nlay, nrow=nrow, ncol=ncol,
                                   delr=delr, delc=delc, top=top, 
                                   botm=botm,
                                   perlen=perlen_mf)
    ibound = np.ones((nlay, nrow, ncol), dtype=np.int)
    ibound[:, :, 0] = -1
    ibound[:, :, -1] = -1

    f = open(os.path.join(datadir, 'p10shead.dat'))
    s0 = np.empty((nrow * ncol), dtype=np.float)
    s0 = read1d(f, s0).reshape((nrow, ncol))
    f.close()
    strt = np.zeros((nlay, nrow, ncol), dtype=np.float)
    for k in range(nlay):
        strt[k] = s0
    bas = flopy.modflow.ModflowBas(mf, ibound=ibound, strt=strt)
    lpf = flopy.modflow.ModflowLpf(mf, hk=hk, layvka=1, vka=vka, laytyp=laytyp)
    welspd = [[3 - 1, 11 - 1, 29 - 1, -19230.00],
              [3 - 1, 19 - 1, 26 - 1, -19230.00],
              [3 - 1, 26 - 1, 23 - 1, -19230.00],
              [3 - 1, 33 - 1, 20 - 1, -19230.00],
              [3 - 1, 40 - 1, 17 - 1, -19230.00],
              [3 - 1, 48 - 1, 14 - 1, -19230.00],
              [3 - 1, 48 - 1,  9 - 1, -15384.00],
              [3 - 1, 52 - 1, 17 - 1, -17307.00]]
    wel = flopy.modflow.ModflowWel(mf, stress_period_data=welspd)
    rch = flopy.modflow.ModflowRch(mf, rech=1.14e-3)
    pcg = flopy.modflow.ModflowPcg(mf)
    lmt = flopy.modflow.ModflowLmt(mf)
    mf.write_input()
    fname = os.path.join(model_ws, 'MT3D001.UCN')
    if os.path.isfile(fname):
        os.remove(fname)
    mf.run_model(silent=True)

    modelname_mt = 'p10_mt'
    mt = flopy.mt3d.Mt3dms(modelname=modelname_mt, model_ws=model_ws, 
                           exe_name=exe_name_mt, modflowmodel=mf)
    f = open(os.path.join(datadir, 'p10cinit.dat'))
    c0 = np.empty((nrow * ncol), dtype=np.float)
    c0 = read1d(f, c0).reshape((nrow, ncol))
    f.close()
    sconc = np.zeros((nlay, nrow, ncol), dtype=np.float)
    sconc[1] = 0.2 * c0
    sconc[2] = c0
    obs = [[3 - 1, 11 - 1, 29 - 1],
           [3 - 1, 19 - 1, 26 - 1],
           [3 - 1, 26 - 1, 23 - 1],
           [3 - 1, 33 - 1, 20 - 1],
           [3 - 1, 40 - 1, 17 - 1],
           [3 - 1, 48 - 1, 14 - 1],
           [3 - 1, 48 - 1,  9 - 1],
           [3 - 1, 52 - 1, 17 - 1]]    
    btn = flopy.mt3d.Mt3dBtn(mt, icbund=1, prsity=prsity, sconc=sconc,
                             timprs=[500, 750, 1000], dt0=2.25, ttsmult=ttsmult, 
                             obs=obs)
    dceps = 1.e-5
    nplane = 0
    npl = 0
    nph = 16
    npmin = 2
    npmax = 32
    dchmoc = 0.01
    nlsink = nplane
    npsink = nph
    adv = flopy.mt3d.Mt3dAdv(mt, mixelm=mixelm, dceps=dceps, nplane=nplane, 
                             npl=npl, nph=nph, npmin=npmin, npmax=npmax,
                             nlsink=nlsink, npsink=npsink, percel=1.0)
    dsp = flopy.mt3d.Mt3dDsp(mt, al=al, trpt=trpt, trpv=trpv)
    ssm = flopy.mt3d.Mt3dSsm(mt, crch=0.)
    rct = flopy.mt3d.Mt3dRct(mt, isothm=isothm, igetsc=0, rhob=1.7, 
                             sp1=0.176, sp2=sp2)
    mxiter = 1
    if isothm == 4:
        mxiter = 50
    gcg = flopy.mt3d.Mt3dGcg(mt, mxiter=mxiter, iter1=500)
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


def test_mt3dmsp10a():  
    
    mixelm  = -1

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p10mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, mixelm)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p10mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, mixelm)

    np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms,
                               rtol=1e-5, atol=1.3e-2)
    return

def test_mt3dmsp10b():  
    
    mixelm  = 3

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p10mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, mixelm)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p10mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, mixelm)

    np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms, atol=1.77)
    return

def test_mt3dmsp10c():  
    
    mixelm  = 0
    ttsmult = 1.0

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p10mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, mixelm, ttsmult)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p10mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, mixelm, ttsmult)

    np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms, atol=1.0e-4)
    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))
    
    # A three-dimensional field case study
    test_mt3dmsp10a()
    test_mt3dmsp10b()
    test_mt3dmsp10c()
