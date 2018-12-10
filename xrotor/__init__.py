from math import pi
import numpy as np
from xrotor import flib

flib.init()


def reinit():
    flib.init()


def set_case(case):
    flib.common.greek = False
    flib.common.name = '{:32s}'.format(case['name'] if 'name' in case else '')
    flib.common.free = case['free']
    flib.common.duct = case['duct']
    flib.common.wind = case['wind']

    conditions = case['conditions']
    flib.common.rho = conditions['rho']
    flib.common.vso = conditions['vso']
    flib.common.rmu = conditions['mu']
    flib.common.alt = conditions['alt']
    flib.common.vel = conditions['vel']
    flib.common.adv = conditions['adv']

    disk = case['disk']
    flib.common.nblds = disk['nblds']

    blade = disk['blade']
    sections = blade['sections']
    flib.common.naero = len(sections)
    for i, key in enumerate(sorted(sections.keys())):
        sec = sections[key]
        flib.putaero(i+1, float(key), sec['a0'] * pi / 180., sec['clmax'], sec['clmin'],
                     sec['dclda'], sec['dclda_stall'], sec['dcl_stall'],
                     sec['cdmin'], sec['clcdmin'], sec['dcddcl2'], sec['cm'], sec['mcrit'], sec['re'], sec['reexp'])

    geom = blade['geom']
    radii = np.array(geom['radii'])
    chord = np.array(geom['chord'])
    twist = np.array(geom['twist'])
    iix = radii.size

    radii.resize(flib.common.ix)
    chord.resize(flib.common.ix)
    twist.resize(flib.common.ix)

    flib.common.xi = radii
    flib.common.ch = chord
    flib.common.beta = twist * pi / 180.
    flib.common.beta0 = twist * pi / 180.

    flib.common.rad = geom['r_tip']
    flib.common.rake = geom['rake']
    flib.common.xi0 = geom['r_hub'] / geom['r_tip']
    flib.common.xiw = geom['r_wake'] / geom['r_tip']

    flib.initcase(iix, False)


def get_performance():
    rho = flib.common.rho
    vel = flib.common.vel
    rad = flib.common.rad
    adv = flib.common.adv

    tdim = flib.common.ttot * rho * vel**2 * rad**2
    qdim = flib.common.qtot * rho * vel**2 * rad**3
    pdim = flib.common.ptot * rho * vel**3 * rad**2

    efftot = flib.common.ttot / flib.common.ptot
    rpm = vel / (rad * adv * pi / 30.)

    tc = tdim / (0.5 * rho * vel**2 * pi * rad**2)
    pc = pdim / (0.5 * rho * vel**3 * pi * rad**2)

    return {'thrust': tdim, 'torque': qdim, 'power': pdim, 'efficiency': efftot, 'rpm': rpm, 'tc': tc, 'pc': pc}


def get_distributions():
    ii = flib.common.ii
    return {'xi': flib.common.xi[:ii],
            'chord': flib.common.ch[:ii], 'twist': flib.common.beta[:ii] * 180 / pi,
            'cl': flib.common.cl[:ii], 'cd': flib.common.cd[:ii], 'stalled': flib.common.stall[:ii],
            'Re': flib.common.re[:ii]}


def operate(rpm=None, thrust=None):
    if thrust is None:
        if rpm is None:
            raise ValueError('Either RPM or thrust has to be specified')

        flib.common.adv = flib.common.vel / (flib.common.rad * rpm * pi / 30.)
        flib.common.conv = False
        flib.aper(4, 2, flib.common.loprini)
    else:
        flib.common.tspec = thrust
        flib.common.conv = False
        flib.output(6)
        flib.aper(1, 2, flib.common.loprini)

    return flib.common.conv
