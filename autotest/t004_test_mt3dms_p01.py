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
testgroup = 'mt3dms_p01'


def p01mt3d(exe_name_mf, exe_name_mt, model_ws, al, retardation,
            lambda1, mixelm, zeta=None, prsity2=None):
    nlay = 1
    nrow = 1
    ncol = 101
    delr = 10.
    delc = 1.
    delv = 1.
    top = 0.
    botm = [top - delv]
    Lx = (ncol - 1) * delr
    v = 0.24
    prsity = 0.25
    q = v * prsity

    perlen = 2000.
    dt0 = perlen / 10.
    hk = 1.
    laytyp = 1
    rhob = 0.25
    kd = (retardation - 1.) * prsity / rhob

    modelname_mf = 'p01_mf'
    mf = flopy.modflow.Modflow(modelname=modelname_mf, model_ws=model_ws,
                               exe_name=exe_name_mf)
    dis = flopy.modflow.ModflowDis(mf, nlay=nlay, nrow=nrow, ncol=ncol,
                                   delr=delr, delc=delc, top=top, botm=botm,
                                   perlen=perlen)
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

    modelname_mt = 'p01_mt'
    mt = flopy.mt3d.Mt3dms(modelname=modelname_mt, model_ws=model_ws,
                           exe_name=exe_name_mt, modflowmodel=mf)
    c0 = 1.
    icbund = np.ones((nlay, nrow, ncol), dtype=np.int)
    icbund[0, 0, 0] = -1
    sconc = np.zeros((nlay, nrow, ncol), dtype=np.float)
    sconc[0, 0, 0] = c0
    btn = flopy.mt3d.Mt3dBtn(mt, laycon=laytyp, icbund=icbund,
                             prsity=prsity, sconc=sconc, dt0=dt0, ifmtcn=1)
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
    dsp = flopy.mt3d.Mt3dDsp(mt, al=al)
    isothm = 1
    if zeta is not None:
        isothm = 6
    rct = flopy.mt3d.Mt3dRct(mt, isothm=isothm, ireact=1, igetsc=0, rhob=rhob,
                             sp1=kd,
                             sp2=zeta, prsity2=prsity2, rc1=lambda1,
                             rc2=lambda1)
    ssm = flopy.mt3d.Mt3dSsm(mt)
    gcg = flopy.mt3d.Mt3dGcg(mt, mxiter=10)
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


def test_mt3dmsp01a():

    longitudinal_dispersivity = 0.
    retardation = 1.0
    zero_order_decay = 0.00
    mixelm = 0
    zeta = None
    prsity2 = None

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'a')
    mf, mt, conc_mt3dusgs, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, longitudinal_dispersivity,
                                              retardation, zero_order_decay,
                                              mixelm, zeta, prsity2)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, longitudinal_dispersivity,
                                            retardation, zero_order_decay,
                                            mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3dusgs, conc_mt3dms)
    assert  np.allclose(conc_mt3dusgs, conc_mt3dms, atol=1e-4), msg
    return


def test_mt3dmsp01b():

    longitudinal_dispersivity = 10.
    retardation = 1.0
    zero_order_decay = 0.00
    mixelm = 0
    zeta = None
    prsity2 = None

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'b')
    mf, mt, conc_mt3dusgs, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, longitudinal_dispersivity,
                                              retardation, zero_order_decay,
                                              mixelm, zeta, prsity2)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, longitudinal_dispersivity,
                                            retardation, zero_order_decay,
                                            mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3dusgs, conc_mt3dms)
    assert  np.allclose(conc_mt3dusgs, conc_mt3dms, atol=1e-4), msg
    return


def test_mt3dmsp01c():

    longitudinal_dispersivity = 10.
    retardation = 1.5
    zero_order_decay = 0.00
    mixelm = 0
    zeta = None
    prsity2 = None

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'c')
    mf, mt, conc_mt3dusgs, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, longitudinal_dispersivity,
                                              retardation, zero_order_decay,
                                              mixelm, zeta, prsity2)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, longitudinal_dispersivity,
                                            retardation, zero_order_decay,
                                            mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3dusgs, conc_mt3dms)
    assert  np.allclose(conc_mt3dusgs, conc_mt3dms, atol=1e-4), msg
    return


def test_mt3dmsp01d():

    longitudinal_dispersivity = 10.
    retardation = 1.5
    zero_order_decay = 0.002
    mixelm = 0
    zeta = None
    prsity2 = None

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'd')
    mf, mt, conc_mt3dusgs, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, longitudinal_dispersivity,
                                              retardation, zero_order_decay,
                                              mixelm, zeta, prsity2)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, longitudinal_dispersivity,
                                            retardation, zero_order_decay,
                                            mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3dusgs, conc_mt3dms)
    assert  np.allclose(conc_mt3dusgs, conc_mt3dms, atol=1e-4), msg
    return


def test_mt3dmsp01e():

    longitudinal_dispersivity = 10.
    retardation = 1.5
    zero_order_decay = 0.002
    mixelm = 0
    zeta = .1
    prsity2 = 0.05

    mt3dusgs_ws = os.path.join(testdir, testgroup + 'e')
    mf, mt, conc_mt3dusgs, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dusgs,
                                              mt3dusgs_ws, longitudinal_dispersivity,
                                              retardation, zero_order_decay,
                                              mixelm, zeta, prsity2)

    mt3dms_ws = os.path.join(mt3dusgs_ws, 'mt3dms')
    mf, mt, conc_mt3dms, cvt, mvt = p01mt3d(exe_name_mf, exe_name_mt3dms,
                                            mt3dms_ws, longitudinal_dispersivity,
                                            retardation, zero_order_decay,
                                            mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3dusgs, conc_mt3dms)
    assert  np.allclose(conc_mt3dusgs, conc_mt3dms, atol=1e-4), msg
    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))
    test_mt3dmsp01a()
    test_mt3dmsp01b()
    test_mt3dmsp01c()
    test_mt3dmsp01d()
    test_mt3dmsp01e()
