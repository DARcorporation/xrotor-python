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
import multiprocessing
import numpy as np
import unittest

from .xrotor import XRotor
from .model import Case

case = {
    'conditions': {
        # Standard atmosphere at sea level
        'rho': 1.225,
        'vso': 340,
        'rmu': 1.789e-5,
        'alt': 1,
        # Operating conditions in accordance with NACA TR 212 (V = 200 mph, RPM = 2000, R_tip = 32.75 inches)
        # adv = V / (Omega * R) = V / (RPM * pi/30 * R)
        'vel': 27,
        'adv': 0.15
    },
    'disk': {
        'n_blds': 2,
        'blade': {
            'sections': {
                0: {
                    # NACA 6412 at Re = 500,000
                    'a_0'        : -4.500,
                    'Cl_max'     :  1.000,
                    'Cl_min'     : -0.100,
                    'dClda'      :  6.000,
                    'dClda_stall':  1.200,
                    'dCl_stall'  :  0.140,
                    'Cm_const'   : -0.080,
                    'M_crit'     :  0.600,
                    'Cd_min'     :  0.018,
                    'Cl_Cd_min'  :  0.570,
                    'dCddCl2'    :  0.160,
                    'Re'         :  5.0e6,
                    'Re_exp'     : -0.500
                }
            },
            'geometry': {
                # Geometry of the example prop. in NACA TR 212
                'r_hub' : 0.06,
                'r_tip' : 0.83,
                'r_wake': 0.0,
                'rake'  : 0.0,
                'radii' : np.array([0.15, 0.30, 0.45, 0.60, 0.75, 0.90]),
                'chord' : np.array([0.15, 0.16, 0.17, 0.16, 0.13, 0.09]),
                'twist' : np.array([50.0, 30.7, 21.6, 16.5, 13.4, 11.3]),
                'ubody' : np.array([0.00, 0.00, 0.00, 0.00, 0.00, 0.00])
            }
        }
    },
    'settings': {
        'free': True, 'duct': False, 'wind': False
    }

}


def print_perf(perf):
    print('\nrpm: {:10.3G}, thrust(N): {:10.8G}, torque(Nm): {:10.8G}, power(W): {:10.8G}, eff: {:10.9G}'
          .format(perf.rpm, perf.thrust, perf.torque, perf.power, perf.efficiency))


class TestXRotor(unittest.TestCase):
    """Test whether the XROTOR module functions properly."""

    def test_solve_for_rpm(self):
        """Run the test case at an RPM of 2000 and make sure the performance results match the expected values.

        Expected performance at 2000 rev/min:
             - Thrust     : T ≈ 481   N
             - Torque     : Q ≈ 105   Nm
             - Power      : P ≈  21.9 kW
             - Efficiency : η ≈   0.5933
        """
        xr = XRotor()
        xr.case = Case.from_dict(case)
        rms = xr.operate(1, 2000)
        perf = xr.performance

        print_perf(perf)

        self.assertTrue(rms < 1.0e-7)
        self.assertAlmostEqual(perf.rpm, 2000, 0)
        self.assertAlmostEqual(perf.thrust, 481, 0)
        self.assertAlmostEqual(perf.torque, 105, 0)
        self.assertAlmostEqual(perf.power/1000, 21.9, 1)
        self.assertAlmostEqual(perf.efficiency, 0.593, 3)

    def test_solve_for_thrust(self):
        """Run the test case at a thrust of 500 N and make sure the performance results match the expected values.

        Expected performance for a thrust of 500 N:
            - RPM        : Ω ≈  2019   rev/min
            - Torque     : Q ≈   107   Nm
            - Power      : P ≈    22.7 kW
            - Efficiency : η ≈     0.5962
        """
        xr = XRotor()
        xr.case = Case.from_dict(case)
        rms = xr.operate(2, 500)
        perf = xr.performance

        print_perf(perf)

        self.assertTrue(rms < 1.0e-7)
        self.assertAlmostEqual(perf.rpm, 2019, 0)
        self.assertAlmostEqual(perf.thrust, 500, 0)
        self.assertAlmostEqual(perf.torque, 107, 0)
        self.assertAlmostEqual(perf.power/1000, 22.7, 1)
        self.assertAlmostEqual(perf.efficiency, 0.596, 3)


class TestXRotorConcurrently(unittest.TestCase):
    """Test whether the tests can be run properly in parallel."""

    def test_concurrently(self):
        """Run both test cases contained in the TestXRotor test case in parallel."""
        test_xrotor = TestXRotor()

        ps = [multiprocessing.Process(target=test_xrotor.test_solve_for_rpm),
              multiprocessing.Process(target=test_xrotor.test_solve_for_thrust)]
        [p.start() for p in ps]
        [p.join() for p in ps]


if __name__ == '__main__':
    unittest.main()
