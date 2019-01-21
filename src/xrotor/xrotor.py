# -*- coding: utf-8 -*-
#   Copyright (c) 2018 D. de Vries
#
#   This file is part of XRotor.
#
#   XRotor is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   XRotor is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with XRotor.  If not, see <https://www.gnu.org/licenses/>.
import numpy as np
import os
import glob
import ctypes

from ctypes import c_bool, c_int, c_void_p, byref, POINTER, c_float, cdll
from shutil import copy2
from tempfile import NamedTemporaryFile

from .model import Case, Performance

here = os.path.abspath(os.path.dirname(__file__))
lib_path = glob.glob(os.path.join(here, 'libxrotor.*'))[0]
lib_ext = lib_path[lib_path.rfind('.'):]
fptr = POINTER(c_float)


class XRotor(object):
    """Interface to the XRotor Fortran routines.

    Attributes
    ----------
    case
    performance
    """

    def __init__(self):
        super().__init__()
        tmp = NamedTemporaryFile(mode='wb', delete=False, suffix=lib_ext)
        tmp.close()
        self._lib_path = tmp.name
        copy2(lib_path, self._lib_path)
        self._lib = cdll.LoadLibrary(self._lib_path)

        self._handle = c_void_p()
        self._lib.init(byref(self._handle))
        self._case: Case = None

    def __del__(self):
        handle = self._lib._handle
        del self._lib
        try:
            ctypes.windll.kernel32.FreeLibrary(handle)
        finally:
            os.remove(self._lib_path)

    @property
    def print(self):
        """bool: True if console output should be shown."""
        return self._lib.get_print().value

    @print.setter
    def print(self, value):
        self._lib.set_print(byref(c_bool(value)))

    @property
    def case(self) -> Case:
        """Case: XRotor run case specification"""
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

    @property
    def performance(self) -> Performance:
        """Performance: Propeller performance specification"""
        perf = Performance()
        self._lib.get_performance(
            self._handle,
            byref(perf._rpm), byref(perf._thrust), byref(perf._torque), byref(perf._power), byref(perf._efficiency)
        )
        return perf

    @property
    def station_conditions(self):
        """(np.ndarray, np.ndarray): Normalized radial coordinates and corresponding local Reynolds numbers."""
        n = c_int()
        self._lib.get_number_of_stations(self._handle, byref(n))
        xi = np.zeros(n.value, dtype=c_float, order='F')
        re = np.zeros(n.value, dtype=c_float, order='F')
        self._lib.get_station_conditions(self._handle, byref(n), xi.ctypes.data_as(fptr), re.ctypes.data_as(fptr))
        return xi, re

    def operate(self, specify, value):
        """Operate the propeller at a specified RPM or thrust.

        Parameters
        ----------
        specify : int
            1 to specify RPM, 2 to specify thrust
        value : float
            Specified RPM in rev/min or thrust in N
        """
        self._lib.operate(
            self._handle,
            byref(c_int(specify)),
            byref(c_float(value))
        )

    def print_case(self):
        """Print the characteristics of the run case at the last operating point to the terminal."""
        self._lib.show(self._handle)

