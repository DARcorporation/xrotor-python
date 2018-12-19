import ctypes
import numpy as np
import os

from ctypes import c_bool, c_int, c_void_p, byref, POINTER, c_float

from model import Case, Performance


here = os.path.dirname(__file__)
lib_file = os.path.abspath(os.path.join(here, '..', '..', 'lib', 'xrotor.so'))
fptr = POINTER(c_float)


class XRotor(object):

    def __init__(self):
        self._lib = ctypes.cdll.LoadLibrary(lib_file)
        self._handle = c_void_p()
        self._lib.init(byref(self._handle))
        self._case: Case = None

    @property
    def case(self) -> Case:
        return self._case

    @case.setter
    def case(self, case: Case):
        self._case = case
        self._lib.set_case(
            self._handle,
            byref(c_float(case.conditions.rho)),
            byref(c_float(case.conditions.vso)),
            byref(c_float(case.conditions.rmu)),
            byref(c_float(case.conditions.alt)),
            byref(c_float(case.conditions.vel)),
            byref(c_float(case.conditions.adv)),
            byref(c_float(case.disk.blade.geometry.r_hub)),
            byref(c_float(case.disk.blade.geometry.r_tip)),
            byref(c_float(case.disk.blade.geometry.r_wake)),
            byref(c_float(case.disk.blade.geometry.rake)),
            byref(c_int(case.disk.n_blds)),
            byref(c_int(case.disk.blade.n_aero)),
            byref(c_int(case.disk.blade.geometry.n_geom)),
            np.asfortranarray(case.disk.blade.aerodata).ctypes.data_as(fptr),
            np.asfortranarray(case.disk.blade.geomdata).ctypes.data_as(fptr),
            byref(c_bool(case.settings.free)),
            byref(c_bool(case.settings.duct)),
            byref(c_bool(case.settings.wind))
        )

    def save_file(self):
        self._lib.save_prop(self._handle)

    def operate(self, specify, value):
        self._lib.operate(
            self._handle,
            byref(c_int(specify)),
            byref(c_float(value))
        )

    def print_case(self):
        self._lib.show(self._handle)

    @property
    def performance(self) -> Performance:
        perf = Performance()
        self._lib.get_performance(
            self._handle,
            byref(perf._rpm), byref(perf._thrust), byref(perf._torque), byref(perf._power), byref(perf._efficiency)
        )
        return perf
