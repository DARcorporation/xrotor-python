import numpy as np
import unittest
import xrotor

conditions = {'rho': 1.2260,
              'vso': 340.00,
              'mu' : 0.17800E-04,
              'alt': 999.00,
              'vel': 0.70000,
              'adv': 0.60000}

aero = {'a0': 0.00000E+00, 'dclda': 5.0000, 'clmax': 1.5000, 'clmin': -1.0000,
        'dclda_stall': 0.10000, 'dcl_stall': 0.10000, 'cm': -0.10000, 'mcrit': 0.80000,
        'cdmin': 0.44000E-01, 'clcdmin': 0.20000, 'dcddcl2': 0.60000E-01,
        're': 5000.0, 'reexp': -0.50000}

distr = np.array([
    [0.26168E-01, 0.17808E-01, 93.391, 0.00000E+00],
    [0.78432E-01, 0.52674E-01, 88.750, 0.00000E+00],
    [0.13048, 0.85299E-01, 84.219, 0.00000E+00],
    [0.18217, 0.11449, 79.858, 0.00000E+00],
    [0.23337, 0.13943, 75.716, 0.00000E+00],
    [0.28392, 0.15973, 71.830, 0.00000E+00],
    [0.33369, 0.17534, 68.218, 0.00000E+00],
    [0.38255, 0.18649, 64.889, 0.00000E+00],
    [0.43036, 0.19358, 61.840, 0.00000E+00],
    [0.47700, 0.19710, 59.063, 0.00000E+00],
    [0.52232, 0.19755, 56.542, 0.00000E+00],
    [0.56621, 0.19543, 54.262, 0.00000E+00],
    [0.60855, 0.19118, 52.204, 0.00000E+00],
    [0.64923, 0.18520, 50.351, 0.00000E+00],
    [0.68812, 0.17781, 48.686, 0.00000E+00],
    [0.72513, 0.16931, 47.193, 0.00000E+00],
    [0.76015, 0.15992, 45.856, 0.00000E+00],
    [0.79308, 0.14983, 44.663, 0.00000E+00],
    [0.82384, 0.13920, 43.602, 0.00000E+00],
    [0.85235, 0.12816, 42.662, 0.00000E+00],
    [0.87852, 0.11681, 41.833, 0.00000E+00],
    [0.90228, 0.10523, 41.108, 0.00000E+00],
    [0.92356, 0.93494E-01, 40.480, 0.00000E+00],
    [0.94232, 0.81664E-01, 39.943, 0.00000E+00],
    [0.95849, 0.69801E-01, 39.491, 0.00000E+00],
    [0.97204, 0.57980E-01, 39.120, 0.00000E+00],
    [0.98292, 0.46330E-01, 38.828, 0.00000E+00],
    [0.99111, 0.35193E-01, 38.611, 0.00000E+00],
    [0.99658, 0.25575E-01, 38.467, 0.00000E+00],
    [0.99931, 0.19616E-01, 38.396, 0.00000E+00]
])

case = {'name': 'test',
        'free': True, 'duct': False, 'wind': False,
        'conditions': conditions,
        'disk': {'nblds': 2,
                 'blade': {'sections': {0: aero},
                           'geom': {'r_hub' : 0.00000E+00,
                                    'r_tip' : 0.30000,
                                    'r_wake': 0.00000E+00,
                                    'rake'  : 0.00000E+00,
                                    'radii' : distr[:, 0],
                                    'chord' : distr[:, 1],
                                    'twist' : distr[:, 2]
                                    }
                           }
                 }

        }


class TestXRotor(unittest.TestCase):

    def test_solve_for_rpm(self):
        xrotor.set_case(case)
        self.assertTrue(xrotor.operate(rpm=1000), 'XROTOR did not converge')
        perf = xrotor.get_performance()

        print('\nrpm: {:10.3G}, thrust(N): {:10.8G}, torque(Nm): {:10.8G}, power(W): {:10.8G}, eff: {:10.9G}'
              .format(perf['rpm'], perf['thrust'], perf['torque'], perf['power'], perf['efficiency']))

        self.assertAlmostEqual(perf['rpm'], 1000, 0)
        self.assertAlmostEqual(perf['thrust'], 6.98, 2)
        self.assertAlmostEqual(perf['torque'], 0.57, 2)
        self.assertAlmostEqual(perf['power'], 60.08, 2)
        self.assertAlmostEqual(perf['efficiency'], 0.0813, 4)


if __name__ == '__main__':
    unittest.main()
