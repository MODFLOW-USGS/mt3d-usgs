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
testgroup = 'mt3dms_p02'

def p02mt3d(exe_name_mf, exe_name_mt, model_ws, isothm, 
            sp1, sp2, mixelm, zeta=None, prsity2=None):
    nlay = 1
    nrow = 1
    ncol = 101
    delr = .16
    delc = 1
    delv = 1
    Lx = (ncol - 1) * delr
    v = 0.1
    prsity = 0.37
    q = v * prsity

    perlen_mf = 1.
    perlen_mt = [160, 1320]
    hk = 1.
    laytyp = 0
    rhob = 1.587

    modelname_mf = 'p02_mf'
    mf = flopy.modflow.Modflow(modelname=modelname_mf, model_ws=model_ws, exe_name=exe_name_mf)
    dis = flopy.modflow.ModflowDis(mf, nlay=nlay, nrow=nrow, ncol=ncol,
                                   delr=delr, delc=delc, top=0., botm=[0 - delv],
                                   perlen=perlen_mf)
    ibound = np.ones((nlay, nrow, ncol), dtype=np.int)
    ibound[0, 0, 0] = -1
    ibound[0, 0, -1] = -1
    strt = np.zeros((nlay, nrow, ncol), dtype=np.float)
    h1 = q * Lx
    strt[0, 0, 0] = h1
    bas = flopy.modflow.ModflowBas(mf, ibound=ibound, strt=strt)
    lpf = flopy.modflow.ModflowLpf(mf, hk=hk, laytyp=laytyp)
    pcg = flopy.modflow.ModflowPcg(mf)
    lmt = flopy.modflow.ModflowLmt(mf)
    mf.write_input()
    mf.run_model(silent=True)

    modelname_mt = 'p02_mt'
    mt = flopy.mt3d.Mt3dms(modelname=modelname_mt, model_ws=model_ws, 
                           exe_name=exe_name_mt, modflowmodel=mf)
    btn = flopy.mt3d.Mt3dBtn(mt, icbund=1, prsity=prsity, sconc=0,
                             nper=2, perlen=perlen_mt, obs=[(0, 0, 50)])
    dceps = 1.e-5
    nplane = 1
    npl = 0
    nph = 4
    npmin = 0
    npmax = 8
    nlsink = nplane
    npsink = nph
    adv = flopy.mt3d.Mt3dAdv(mt, mixelm=mixelm, dceps=dceps, nplane=nplane, 
                             npl=npl, nph=nph, npmin=npmin, npmax=npmax,
                             nlsink=nlsink, npsink=npsink, percel=0.5)
    al = 1.
    dsp = flopy.mt3d.Mt3dDsp(mt, al=al)
    rct = flopy.mt3d.Mt3dRct(mt, isothm=isothm, ireact=0, igetsc=0, rhob=rhob, 
                             sp1=sp1, sp2=sp2)
    c0 = 0.05
    spd = {0:[0, 0, 0, c0, 1], 1:[0, 0, 0, 0., 1]}
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



def test_mt3dmsp02a():  # Tests Freudlich
    
    isothm = 2 
    sp1    = 0.3
    sp2    = 0.7 
    mixelm = -1
    zeta = None
    prsity2 = None

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p02mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, isothm,
                                              sp1, sp2, mixelm, zeta)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p02mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, isothm,
                                            sp1, sp2, mixelm, zeta)

    np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms, atol=1e-4)
    return


def test_mt3dmsp02b():  # Tests Langmuir
    isothm = 3
    sp1    = 100.
    sp2    = 0.003
    mixelm = -1
    zeta = None
    prsity2 = None

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'b')
    mf, mt, conc_mt3dusgs, cvt, mvt = p02mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, isothm,
                                              sp1, sp2, mixelm, zeta)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p02mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, isothm,
                                            sp1, sp2, mixelm, zeta)

    np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms, atol=1e-4)
    return

def test_mt3dmsp02c():  # Tests Nonequilibrium
    isothm = 4
    sp1    = 0.933
    mixelm = -1
    zeta = None
    prsity2 = None

    for beta in [0, 2.e-3, 1.e-2, 20.]:

        mt3dusgs_ws = os.path.join(testdir, testgroup + 'c')
        mf, mt, conc_mt3dusgs, cvt, mvt = p02mt3d(exe_name_mf, exe_name_mt3dusgs,
                                                  mt3dusgs_ws, isothm,
                                                  sp1, beta, mixelm, zeta)

        mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
        mf, mt, conc_mt3dms, cvt, mvt = p02mt3d(exe_name_mf, exe_name_mt3dms,
                                                mt3dms_ws, isothm,
                                                sp1, beta, mixelm, zeta)

        msg = 'concentrations not equal for beta {}'.format(beta)
        np.testing.assert_allclose(conc_mt3dusgs, conc_mt3dms, atol=1e-4,
                                   err_msg=msg)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # Two-dimensional transport in a uniform flow field
    test_mt3dmsp02a()
    test_mt3dmsp02b()
    test_mt3dmsp02c()
