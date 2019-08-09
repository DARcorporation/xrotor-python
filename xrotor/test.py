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
            },
            'polars': {
                0.0 : np.array([ # NACA 6412 at Re = 500,000
                    # alpha       cl        cd         cm
                    [-9.5000, -0.1344,  0.0894, -0.0883],
                    [-9.2500, -0.2938,  0.0289, -0.1484],
                    [-9.0000, -0.2807,  0.0245, -0.1502],
                    [-8.7500, -0.2627,  0.0224, -0.1500],
                    [-8.5000, -0.2407,  0.0201, -0.1503],
                    [-8.2500, -0.2191,  0.0192, -0.1496],
                    [-8.0000, -0.1937,  0.0186, -0.1494],
                    [-7.7500, -0.1673,  0.0178, -0.1494],
                    [-7.5000, -0.1435,  0.0170, -0.1489],
                    [-7.2500, -0.1184,  0.0162, -0.1484],
                    [-7.0000, -0.0929,  0.0151, -0.1482],
                    [-6.7500, -0.0682,  0.0145, -0.1477],
                    [-6.5000, -0.0419,  0.0140, -0.1474],
                    [-6.0000,  0.0114,  0.0132, -0.1467],
                    [-5.7500,  0.0372,  0.0125, -0.1464],
                    [-5.5000,  0.0643,  0.0122, -0.1461],
                    [-5.2500,  0.0908,  0.0119, -0.1457],
                    [-5.0000,  0.1179,  0.0114, -0.1455],
                    [-4.7500,  0.1441,  0.0112, -0.1450],
                    [-4.5000,  0.1714,  0.0109, -0.1448],
                    [-4.2500,  0.1976,  0.0106, -0.1444],
                    [-4.0000,  0.2254,  0.0105, -0.1441],
                    [-3.7500,  0.2513,  0.0102, -0.1437],
                    [-3.5000,  0.2790,  0.0101, -0.1435],
                    [-3.2500,  0.3050,  0.0098, -0.1430],
                    [-3.0000,  0.3323,  0.0097, -0.1427],
                    [-2.7500,  0.3586,  0.0096, -0.1423],
                    [-2.5000,  0.3853,  0.0095, -0.1420],
                    [-2.2500,  0.4119,  0.0094, -0.1416],
                    [-2.0000,  0.4382,  0.0093, -0.1411],
                    [-1.7500,  0.4645,  0.0092, -0.1407],
                    [-1.5000,  0.4907,  0.0091, -0.1403],
                    [-1.2500,  0.5169,  0.0091, -0.1398],
                    [-1.0000,  0.5429,  0.0091, -0.1393],
                    [-0.7500,  0.5688,  0.0091, -0.1389],
                    [-0.5000,  0.5946,  0.0091, -0.1384],
                    [-0.2500,  0.6201,  0.0091, -0.1378],
                    [ 0.0000,  0.6458,  0.0091, -0.1373],
                    [ 0.2500,  0.6712,  0.0091, -0.1368],
                    [ 0.5000,  0.6963,  0.0091, -0.1362],
                    [ 0.7500,  0.7212,  0.0090, -0.1356],
                    [ 1.0000,  0.7419,  0.0086, -0.1343],
                    [ 1.2500,  0.8011,  0.0080, -0.1414],
                    [ 1.5000,  0.8268,  0.0081, -0.1409],
                    [ 1.7500,  0.8523,  0.0083, -0.1404],
                    [ 2.0000,  0.8782,  0.0085, -0.1399],
                    [ 2.2500,  0.9038,  0.0086, -0.1395],
                    [ 2.5000,  0.9297,  0.0088, -0.1391],
                    [ 2.7500,  0.9556,  0.0089, -0.1387],
                    [ 3.0000,  0.9810,  0.0091, -0.1382],
                    [ 3.2500,  1.0072,  0.0093, -0.1379],
                    [ 3.5000,  1.0328,  0.0094, -0.1375],
                    [ 3.7500,  1.0584,  0.0096, -0.1371],
                    [ 4.0000,  1.0843,  0.0098, -0.1367],
                    [ 4.2500,  1.1098,  0.0100, -0.1363],
                    [ 4.5000,  1.1353,  0.0102, -0.1359],
                    [ 4.7500,  1.1609,  0.0103, -0.1355],
                    [ 5.0000,  1.1861,  0.0105, -0.1351],
                    [ 5.2500,  1.2116,  0.0107, -0.1347],
                    [ 5.5000,  1.2367,  0.0109, -0.1342],
                    [ 5.7500,  1.2611,  0.0111, -0.1337],
                    [ 6.0000,  1.2856,  0.0113, -0.1331],
                    [ 6.2500,  1.3093,  0.0115, -0.1324],
                    [ 6.5000,  1.3320,  0.0117, -0.1315],
                    [ 6.7500,  1.3544,  0.0119, -0.1305],
                    [ 7.0000,  1.3764,  0.0121, -0.1295],
                    [ 7.2500,  1.3985,  0.0123, -0.1286],
                    [ 7.5000,  1.4203,  0.0126, -0.1276],
                    [ 7.7500,  1.4404,  0.0128, -0.1262],
                    [ 8.0000,  1.4601,  0.0131, -0.1248],
                    [ 8.2500,  1.4785,  0.0134, -0.1232],
                    [ 8.5000,  1.4960,  0.0138, -0.1215],
                    [ 8.7500,  1.5139,  0.0142, -0.1199],
                    [ 9.0000,  1.5288,  0.0147, -0.1178],
                    [ 9.2500,  1.5427,  0.0153, -0.1157],
                    [ 9.5000,  1.5530,  0.0161, -0.1131],
                    [ 9.7500,  1.5615,  0.0171, -0.1104],
                    [10.0000,  1.5684,  0.0182, -0.1076],
                    [10.2500,  1.5732,  0.0196, -0.1046],
                    [10.5000,  1.5761,  0.0210, -0.1016],
                    [10.7500,  1.5778,  0.0227, -0.0986],
                    [11.0000,  1.5792,  0.0244, -0.0957],
                    [11.2500,  1.5797,  0.0263, -0.0930],
                    [11.5000,  1.5818,  0.0281, -0.0906],
                    [11.7500,  1.5841,  0.0300, -0.0884],
                    [12.0000,  1.5861,  0.0320, -0.0864],
                    [12.2500,  1.5874,  0.0341, -0.0845],
                    [12.5000,  1.5904,  0.0362, -0.0829],
                    [12.7500,  1.5928,  0.0383, -0.0814],
                    [13.0000,  1.5928,  0.0408, -0.0800],
                    [13.2500,  1.5952,  0.0432, -0.0788],
                    [13.5000,  1.5959,  0.0458, -0.0778],
                    [13.7500,  1.5928,  0.0489, -0.0767],
                    [14.0000,  1.5954,  0.0515, -0.0760],
                    [14.2500,  1.5936,  0.0546, -0.0753],
                    [14.5000,  1.5871,  0.0583, -0.0746],
                    [14.7500,  1.5890,  0.0611, -0.0742],
                    [15.0000,  1.5882,  0.0643, -0.0739],
                    [15.2500,  1.5833,  0.0681, -0.0737],
                    [15.5000,  1.5733,  0.0726, -0.0735],
                    [15.7500,  1.5732,  0.0758, -0.0735],
                    [16.0000,  1.5708,  0.0794, -0.0735],
                    [16.2500,  1.5677,  0.0831, -0.0736],
                    [16.5000,  1.5633,  0.0870, -0.0739],
                    [16.7500,  1.5568,  0.0912, -0.0743],
                ])
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

    def test_solve_for_thrust(self):
        """Run the test case at a thrust of 500 N and make sure the performance results match the expected values.

        Expected performance for a thrust of 500 N:
            - Torque     : Q ≈   107   Nm
            - Power      : P ≈    22.7 kW
            - RPM        : Ω ≈  2019   rev/min
            - Efficiency : η ≈     0.596
        """
        xr = XRotor()
        xr.case = Case.from_dict(case)
        rms = xr.operate(thrust=500)
        perf = xr.performance

        print_perf(perf)

        self.assertTrue(abs(rms) < 1.0e-7)
        self.assertAlmostEqual(perf.thrust, 500, 0)
        self.assertAlmostEqual(perf.torque, 107, 0)
        self.assertAlmostEqual(perf.power/1000, 22.7, 1)
        self.assertAlmostEqual(perf.rpm, 2019, 0)
        self.assertAlmostEqual(perf.efficiency, 0.596, 3)

    def test_solve_for_torque(self):
        """Run the test case at a torque of 100 Nm and make sure the performance results match the expected values.

        Expected performance for a torque of 100 Nm:
            - Thrust     : T ≈   446   N
            - Power      : P ≈    20.6 kW
            - RPM        : Ω ≈  1963   rev/min
            - Efficiency : η ≈     0.586
        """
        xr = XRotor()
        xr.case = Case.from_dict(case)
        rms = xr.operate(torque=100)
        perf = xr.performance

        print_perf(perf)

        self.assertTrue(abs(rms) < 1.0e-7)
        self.assertAlmostEqual(perf.thrust, 446, 0)
        self.assertAlmostEqual(perf.torque, 100, 0)
        self.assertAlmostEqual(perf.power/1000, 20.6, 1)
        self.assertAlmostEqual(perf.rpm, 1963, 0)
        self.assertAlmostEqual(perf.efficiency, 0.586, 3)

    def test_solve_for_power(self):
        """Run the test case at a power of 20 kW and make sure the performance results match the expected values.

        Expected performance for a torque of 100 Nm:
            - Thrust     : T ≈   431   N
            - Torque     : Q ≈    98   Nm
            - RPM        : Ω ≈  1947   rev/min
            - Efficiency : η ≈     0.582
        """
        xr = XRotor()
        xr.case = Case.from_dict(case)
        rms = xr.operate(power=20e3)
        perf = xr.performance

        print_perf(perf)

        self.assertTrue(abs(rms) < 1.0e-7)
        self.assertAlmostEqual(perf.thrust, 431, 0)
        self.assertAlmostEqual(perf.torque, 98, 0)
        self.assertAlmostEqual(perf.power/1000, 20.0, 1)
        self.assertAlmostEqual(perf.rpm, 1947, 0)
        self.assertAlmostEqual(perf.efficiency, 0.582, 3)

    def test_solve_for_rpm(self):
        """Run the test case at an RPM of 2000 and make sure the performance results match the expected values.

        Expected performance at 2000 rev/min:
             - Thrust     : T ≈ 481   N
             - Torque     : Q ≈ 105   Nm
             - Power      : P ≈  21.9 kW
             - Efficiency : η ≈   0.593
        """
        xr = XRotor()
        xr.case = Case.from_dict(case)
        rms = xr.operate(rpm=2000)
        perf = xr.performance

        print_perf(perf)

        self.assertTrue(abs(rms) < 1.0e-7)
        self.assertAlmostEqual(perf.thrust, 481, 0)
        self.assertAlmostEqual(perf.torque, 105, 0)
        self.assertAlmostEqual(perf.power/1000, 21.9, 1)
        self.assertAlmostEqual(perf.rpm, 2000, 0)
        self.assertAlmostEqual(perf.efficiency, 0.593, 3)


class TestXRotorConcurrently(unittest.TestCase):
    """Test whether the tests can be run properly in parallel."""

    def test_concurrently(self):
        """Run all test cases contained in the TestXRotor test case in parallel."""
        test_xrotor = TestXRotor()

        ps = [multiprocessing.Process(target=test_xrotor.test_solve_for_thrust),
              multiprocessing.Process(target=test_xrotor.test_solve_for_torque),
              multiprocessing.Process(target=test_xrotor.test_solve_for_power),
              multiprocessing.Process(target=test_xrotor.test_solve_for_rpm)]
        [p.start() for p in ps]
        [p.join() for p in ps]


if __name__ == '__main__':
    unittest.main()
