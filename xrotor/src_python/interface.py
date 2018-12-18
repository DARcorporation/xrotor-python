import ctypes
import numpy as np
import os

from ctypes import c_bool, c_int, c_void_p, byref, POINTER, c_float


here = os.path.dirname(__file__)
lib_file = os.path.abspath(os.path.join(here, '..', '..', 'lib', 'xrotor.so'))
lib = ctypes.cdll.LoadLibrary(lib_file)
fptr = POINTER(c_float)


def init():
    handle = c_void_p()
    lib.init(byref(handle))
    return handle


def set_case(handle, case):

    conditions = case['conditions']
    rho = c_float(conditions['rho'])
    vso = c_float(conditions['vso'])
    rmu = c_float(conditions['mu'])
    alt = c_float(conditions['alt'])
    vel = c_float(conditions['vel'])
    adv = c_float(conditions['adv'])

    disk = case['disk']
    r_hub = c_float(disk['r_hub'])
    r_tip = c_float(disk['r_tip'])
    r_wake = c_float(disk['r_wake'])
    rake = c_float(disk['rake'])

    nblds = c_int(disk['nblds'])

    blade = disk['blade']
    sections = blade['sections']
    naero = c_int(len(sections))
    aerodata = np.zeros((14, naero.value), dtype=c_float, order='F')
    for i, key in enumerate(sorted(sections.keys())):
        sec = sections[key]
        aerodata[0][i] = c_float(float(key))
        aerodata[1][i] = c_float(sec['a0'])
        aerodata[2][i] = c_float(sec['clmax'])
        aerodata[3][i] = c_float(sec['clmin'])
        aerodata[4][i] = c_float(sec['dclda'])
        aerodata[5][i] = c_float(sec['dclda_stall'])
        aerodata[6][i] = c_float(sec['dcl_stall'])
        aerodata[7][i] = c_float(sec['cdmin'])
        aerodata[8][i] = c_float(sec['clcdmin'])
        aerodata[9][i] = c_float(sec['dcddcl2'])
        aerodata[10][i] = c_float(sec['cm'])
        aerodata[11][i] = c_float(sec['mcrit'])
        aerodata[12][i] = c_float(sec['re'])
        aerodata[13][i] = c_float(sec['reexp'])

    geom = blade['geom']
    geomdata = np.array([geom['radii'], geom['chord'], geom['twist'], geom['ubody']], dtype=c_float, order='F')
    ngeom = geomdata.size[1]

    free = c_bool(case['free'])
    duct = c_bool(case['duct'])
    wind = c_bool(case['wind'])

    lib.set_case(
        handle,
        byref(rho), byref(vso), byref(rmu), byref(alt), byref(vel), byref(adv),
        byref(r_hub), byref(r_tip), byref(r_wake), byref(rake),
        byref(nblds), byref(naero), byref(ngeom),
        aerodata.ctypes.data_as(fptr), geomdata.ctypes.data_as(fptr),
        byref(free), byref(duct), byref(wind)
    )


def operate(handle, specify, value):
    lib.operate(
        handle,
        byref(c_int(specify)),
        byref(c_float(value))
    )


def print_case(handle):
    lib.show(handle)


def get_performance(handle):
    rpm = c_float()
    thrust = c_float()
    torque = c_float()
    power = c_float()
    efficiency = c_float()

    lib.get_performance(handle, byref(rpm), byref(thrust), byref(torque), byref(power), byref(efficiency))

    return {'rpm': rpm.value,
            'thrust': thrust.value,
            'torque': torque.value,
            'power': power.value,
            'efficiency': efficiency.value}
