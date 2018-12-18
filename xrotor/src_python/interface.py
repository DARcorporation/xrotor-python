import ctypes
import numpy as np
import os

from ctypes import c_bool, c_int, c_double, c_void_p, byref, POINTER, c_float


here = os.path.dirname(__file__)
lib_file = os.path.abspath(os.path.join(here, '..', '..', 'lib', 'xrotor.so'))
lib = ctypes.cdll.LoadLibrary(lib_file)


def test():
    handle = c_void_p()
    lib.init(byref(handle))

    rho = c_float(1.225)
    vso = c_float(340)
    rmu = c_float(1.789e-5)
    alt = c_float(1)
    vel = c_float(27)
    adv = c_float(.15)

    r_tip = c_float(.83)
    r_hub = c_float(.06)
    rake = c_float(0)

    n_blds = c_int(2)

    aerodata = np.array([[0, -4.5, 1, -.1, 6, 1.2, 0.14, .018, .57, .16, -.08, .6, 5e6, -.5]], dtype=c_float)

    geomdata = np.array([[0.15, 0.30, 0.45, 0.60, 0.75, 0.90],
                         [0.15, 0.16, 0.17, 0.16, 0.13, 0.09],
                         [50.0, 30.7, 21.6, 16.5, 13.4, 11.3],
                         [0.00, 0.00, 0.00, 0.00, 0.00, 0.00]], dtype=c_float)

    n_aero = c_int(1)
    n_geom = c_int(6)

    free = c_bool(True)
    duct = c_bool(False)
    wind = c_bool(False)

    dptr = POINTER(c_double)

    lib.set_case(
        handle,
        byref(rho), byref(vso), byref(rmu), byref(alt), byref(vel), byref(adv),
        byref(r_tip), byref(r_hub), byref(rake),
        byref(n_blds), byref(n_aero), byref(n_geom),
        aerodata.ctypes.data_as(dptr), geomdata.ctypes.data_as(dptr),
        byref(free), byref(duct), byref(wind)
     )

    lib.save_prop(handle)
    exit(0)

    lib.operate(
        handle,
        byref(c_int(1)),
        byref(c_float(2000))
    )

    lib.show(handle)


if __name__ == '__main__':
    test()
