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

geomdata = np.array([
    # r/R     c/R     t/c     beta
    [0.2000, 0.0759, 0.7977, 45.6410],
    [0.2500, 0.0904, 0.5492, 43.9615],
    [0.3000, 0.1062, 0.3635, 41.8157],
    [0.3500, 0.1226, 0.2405, 39.4593],
    [0.4000, 0.1382, 0.1748, 36.8433],
    [0.4500, 0.1480, 0.1366, 34.1065],
    [0.5000, 0.1520, 0.1182, 31.8362],
    [0.5500, 0.1503, 0.1078, 29.9828],
    [0.6000, 0.1461, 0.1015, 28.3983],
    [0.6500, 0.1393, 0.0955, 27.0601],
    [0.7000, 0.1313, 0.0915, 25.9288],
    [0.7500, 0.1224, 0.0884, 24.9254],
    [0.8000, 0.1121, 0.0855, 24.0828],
    [0.8500, 0.1010, 0.0839, 23.3286],
    [0.9000, 0.0886, 0.0818, 22.6380],
    [0.9500, 0.0752, 0.0812, 22.1021],
])

case = {
    'conditions': {
        # Standard atmosphere at sea level
        'rho': 1.225,
        'vso': 340,
        'rmu': 1.789e-5,
        'alt': 1,
        # Operating conditions in accordance with NASA Report No. 640
        # adv = V / (Omega * R) = V / (RPM * pi/30 * R)
        'vel': 35.56,
        'adv': 0.70
    },
    'disk': {
        'n_blds': 2,
        'blade': {
            'geometry': {
                # Geometry of the NASA Report No. 640, 5868-9, 2 blades, blade angle 25 deg at 0.75R propeller
                'r_hub' : 0.305,
                'r_tip' : 1.524,
                'r_wake': 0.0,
                'rake'  : 0.0,
                'radii' : geomdata[:, 0],
                'chord' : geomdata[:, 1],
                'twist' : geomdata[:, 3],
                'ubody' : np.zeros_like(geomdata[:, 0])
            },
            'polars': {
                0.0: np.array([
                    # Clark-y at Re = 1,000,000, M = 0.3 (ncrit = 0.01)
                    # alpha     cl      cd       cm
                    [-10.00, -0.6912, 0.0334, -0.0951],
                    [ -9.75, -0.7003, 0.0277, -0.0954],
                    [ -9.50, -0.6899, 0.0246, -0.0947],
                    [ -9.25, -0.6723, 0.0226, -0.0939],
                    [ -9.00, -0.6511, 0.0210, -0.0930],
                    [ -8.75, -0.6276, 0.0198, -0.0921],
                    [ -8.50, -0.6027, 0.0188, -0.0913],
                    [ -8.25, -0.5767, 0.0179, -0.0905],
                    [ -8.00, -0.5500, 0.0172, -0.0898],
                    [ -7.75, -0.5227, 0.0166, -0.0891],
                    [ -7.50, -0.4950, 0.0160, -0.0885],
                    [ -7.25, -0.4670, 0.0156, -0.0878],
                    [ -7.00, -0.4387, 0.0151, -0.0873],
                    [ -6.75, -0.4102, 0.0147, -0.0867],
                    [ -6.50, -0.3816, 0.0144, -0.0862],
                    [ -6.25, -0.3528, 0.0141, -0.0857],
                    [ -6.00, -0.3240, 0.0138, -0.0852],
                    [ -5.75, -0.2950, 0.0135, -0.0848],
                    [ -5.50, -0.2660, 0.0133, -0.0844],
                    [ -5.25, -0.2370, 0.0130, -0.0840],
                    [ -5.00, -0.2079, 0.0128, -0.0836],
                    [ -4.75, -0.1788, 0.0127, -0.0833],
                    [ -4.50, -0.1497, 0.0125, -0.0830],
                    [ -4.25, -0.1205, 0.0124, -0.0827],
                    [ -4.00, -0.0914, 0.0122, -0.0824],
                    [ -3.75, -0.0623, 0.0121, -0.0821],
                    [ -3.50, -0.0331, 0.0120, -0.0818],
                    [ -3.25, -0.0040, 0.0119, -0.0816],
                    [ -3.00,  0.0251, 0.0119, -0.0813],
                    [ -2.75,  0.0542, 0.0118, -0.0811],
                    [ -2.50,  0.0832, 0.0117, -0.0809],
                    [ -2.25,  0.1123, 0.0117, -0.0807],
                    [ -2.00,  0.1413, 0.0117, -0.0804],
                    [ -1.75,  0.1703, 0.0116, -0.0802],
                    [ -1.50,  0.1992, 0.0116, -0.0800],
                    [ -1.25,  0.2282, 0.0116, -0.0798],
                    [ -1.00,  0.2570, 0.0116, -0.0796],
                    [ -0.75,  0.2859, 0.0116, -0.0794],
                    [ -0.50,  0.3147, 0.0116, -0.0792],
                    [ -0.25,  0.3434, 0.0117, -0.0790],
                    [  0.00,  0.3721, 0.0117, -0.0788],
                    [  0.25,  0.4007, 0.0117, -0.0786],
                    [  0.50,  0.4293, 0.0118, -0.0784],
                    [  0.75,  0.4578, 0.0119, -0.0782],
                    [  1.00,  0.4863, 0.0119, -0.0779],
                    [  1.25,  0.5147, 0.0120, -0.0777],
                    [  1.50,  0.5430, 0.0121, -0.0774],
                    [  1.75,  0.5712, 0.0122, -0.0772],
                    [  2.00,  0.5994, 0.0123, -0.0769],
                    [  2.25,  0.6274, 0.0124, -0.0766],
                    [  2.50,  0.6554, 0.0125, -0.0763],
                    [  2.75,  0.6833, 0.0126, -0.0759],
                    [  3.00,  0.7111, 0.0127, -0.0756],
                    [  3.25,  0.7387, 0.0129, -0.0752],
                    [  3.50,  0.7662, 0.0130, -0.0748],
                    [  3.75,  0.7936, 0.0132, -0.0744],
                    [  4.00,  0.8208, 0.0133, -0.0739],
                    [  4.25,  0.8479, 0.0135, -0.0734],
                    [  4.50,  0.8748, 0.0137, -0.0729],
                    [  4.75,  0.9015, 0.0139, -0.0723],
                    [  5.00,  0.9280, 0.0141, -0.0717],
                    [  5.25,  0.9542, 0.0143, -0.0711],
                    [  5.50,  0.9802, 0.0145, -0.0704],
                    [  5.75,  1.0060, 0.0148, -0.0696],
                    [  6.00,  1.0314, 0.0150, -0.0688],
                    [  6.25,  1.0564, 0.0153, -0.0679],
                    [  6.50,  1.0811, 0.0156, -0.0670],
                    [  6.75,  1.1053, 0.0159, -0.066],
                    [  7.00,  1.1290, 0.0162, -0.0648],
                    [  7.25,  1.1514, 0.0165, -0.0635],
                    [  7.50,  1.1719, 0.0168, -0.0618],
                    [  7.75,  1.1921, 0.0172, -0.0600],
                    [  8.00,  1.2133, 0.0176, -0.0585],
                    [  8.25,  1.2342, 0.0181, -0.0571],
                    [  8.50,  1.2546, 0.0185, -0.0555],
                    [  8.75,  1.2743, 0.0191, -0.0539],
                    [  9.00,  1.2934, 0.0196, -0.0523],
                    [  9.25,  1.3118, 0.0202, -0.0506],
                    [  9.50,  1.3295, 0.0209, -0.0489],
                    [  9.75,  1.3465, 0.0216, -0.0472],
                    [ 10.00,  1.3628, 0.0224, -0.0454],
                    [ 10.25,  1.3783, 0.0232, -0.0437],
                    [ 10.50,  1.3932, 0.0241, -0.0420],
                    [ 10.75,  1.4073, 0.0251, -0.0403],
                    [ 11.00,  1.4205, 0.0262, -0.0386],
                    [ 11.25,  1.4330, 0.0274, -0.0370],
                    [ 11.50,  1.4445, 0.0287, -0.0354],
                    [ 11.75,  1.4551, 0.0302, -0.0339],
                    [ 12.00,  1.4647, 0.0317, -0.0325],
                    [ 12.25,  1.4733, 0.0334, -0.0312],
                    [ 12.50,  1.4807, 0.0353, -0.0299],
                    [ 12.75,  1.4869, 0.0373, -0.0288],
                    [ 13.00,  1.4919, 0.0395, -0.0278],
                    [ 13.25,  1.4955, 0.0420, -0.0270],
                    [ 13.50,  1.4979, 0.0446, -0.0263],
                    [ 13.75,  1.4989, 0.0474, -0.0258],
                    [ 14.00,  1.4985, 0.0505, -0.0255],
                    [ 14.25,  1.4967, 0.0539, -0.0254],
                    [ 14.50,  1.4934, 0.0576, -0.0256],
                    [ 14.75,  1.4885, 0.0615, -0.0260],
                    [ 15.00,  1.4822, 0.0657, -0.0266],
                    [ 15.25,  1.4742, 0.0703, -0.0275],
                    [ 15.50,  1.4647, 0.0751, -0.0286],
                    [ 15.75,  1.4540, 0.0802, -0.0299],
                    [ 16.00,  1.4421, 0.0856, -0.0315],
                    [ 16.25,  1.4294, 0.0912, -0.0333],
                    [ 16.50,  1.4162, 0.0969, -0.0353],
                    [ 16.75,  1.4029, 0.1028, -0.0374],
                    [ 17.00,  1.3895, 0.1087, -0.0398],
                    [ 17.25,  1.3764, 0.1147, -0.0423],
                    [ 17.50,  1.3638, 0.1207, -0.0450],
                    [ 17.75,  1.3516, 0.1267, -0.0478],
                    [ 18.00,  1.3402, 0.1327, -0.0509],
                    [ 18.25,  1.3295, 0.1387, -0.0541],
                    [ 18.50,  1.3194, 0.1447, -0.0574],
                    [ 18.75,  1.3100, 0.1507, -0.0609],
                    [ 19.00,  1.3012, 0.1567, -0.0646],
                    [ 19.25,  1.2931, 0.1627, -0.0683],
                    [ 19.50,  1.2855, 0.1686, -0.0722],
                    [ 19.75,  1.2787, 0.1746, -0.0761],
                    [ 20.00,  1.2727, 0.1805, -0.0801]
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
