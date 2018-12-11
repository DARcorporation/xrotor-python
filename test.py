import numpy as np
import unittest
import xrotor

case = {
    'name': 'Test Propeller',
    'free': True, 'duct': False, 'wind': False,
    'conditions': {
        # Standard atmosphere at sea level
        'rho': 1.225,
        'vso': 340,
        'mu' : 1.789e-5,
        'alt': 1,
        # Operating conditions in accordance with NACA TR 212 (V = 200 mph, RPM = 2000, R_tip = 32.75 inches)
        # adv = V / (Omega * R) = V / (RPM * pi/30 * R)
        'vel': 27,
        'adv': 0.15
    },
    'disk': {
        'nblds': 2,
        'blade': {
            'sections': {
                0: {
                    # NACA 6412 at Re = 500,000
                    'a0': -4.5,
                    'dclda': 6,
                    'clmax': 1,
                    'clmin': -0.1,
                    'dclda_stall': 1.2,
                    'dcl_stall': 0.14,
                    'cm': -0.08,
                    'mcrit': 0.6,
                    'cdmin': 0.018,
                    'clcdmin': 0.57,
                    'dcddcl2': 0.16,
                    're': 5000000,
                    'reexp': -0.5
                }
            },
            'geom': {
                # Geometry of the example prop. in NACA TR 212
                'r_hub' : 0.06,
                'r_tip' : 0.83,
                'r_wake': 0.0,
                'rake'  : 0.0,
                'radii' : np.array([0.15, 0.30, 0.45, 0.60, 0.75, 0.90]),
                'chord' : np.array([0.15, 0.16, 0.17, 0.16, 0.13, 0.09]),
                'twist' : np.array([50.0, 30.7, 21.6, 16.5, 13.4, 11.3])
            }
        }
    }

}


def print_perf(perf):
    print('\nrpm: {:10.3G}, thrust(N): {:10.8G}, torque(Nm): {:10.8G}, power(W): {:10.8G}, eff: {:10.9G}'
          .format(perf['rpm'], perf['thrust'], perf['torque'], perf['power'], perf['efficiency']))


class TestXRotor(unittest.TestCase):
    """Test whether the XROTOR module functions properly."""

    def test_solve_for_rpm(self):
        """Run the test case at an RPM of 2000 and make sure the performance results match the expected values.

        Expected performance at 2000 rev/min:
             - Thrust     : T ≈   481 N
             - Torque     : Q ≈   105 Nm
             - Power      : P ≈ 21908 W
             - Efficiency : η ≈ 0.5933
        """
        xrotor.set_case(case)
        self.assertTrue(xrotor.operate(rpm=2000), 'XROTOR did not converge')
        perf = xrotor.get_performance()

        print_perf(perf)

        self.assertAlmostEqual(perf['rpm'], 2000, 0)
        self.assertAlmostEqual(perf['thrust'], 481, 0)
        self.assertAlmostEqual(perf['torque'], 105, 0)
        self.assertAlmostEqual(perf['power'], 21908, 0)
        self.assertAlmostEqual(perf['efficiency'], 0.5933, 4)

    def test_solve_for_thrust(self):
        """Run the test case at a thrust of 500 N and make sure the performance results match the expected values.

        Expected performance for a thrust of 500 N:
            - RPM        : Ω ≈  2019 rev/min
            - Torque     : Q ≈   107 Nm
            - Power      : P ≈ 22642 W
            - Efficiency : η ≈ 0.5962
        """
        xrotor.set_case(case)
        self.assertTrue(xrotor.operate(thrust=500), 'XROTOR did not converge')
        perf = xrotor.get_performance()

        print_perf(perf)

        self.assertAlmostEqual(perf['rpm'], 2019, 0)
        self.assertAlmostEqual(perf['thrust'], 500, 0)
        self.assertAlmostEqual(perf['torque'], 107, 0)
        self.assertAlmostEqual(perf['power'], 22642, 0)
        self.assertAlmostEqual(perf['efficiency'], 0.5962, 4)


if __name__ == '__main__':
    unittest.main()
