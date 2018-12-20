
General
-------
This is a stripped down version of XROTOR. All the modification, design, and graphical functionality has
been removed. The only main menu options that are available in this stripped down version are:
* OPER, which allows for the calculation of performance characteristics at given operating conditions;
* BEND, which allows for the calculation of structural loads and deformations;
* NOIS, which allows for the calculation of the acoustic signature;
* LOAD, which loads a propeller definition file from the disk;
* SAVE, which saves a propeller definition file to the disk; and
* DISP, which displays the current propeller characteristics data onscreen.

Building and Installing the Python Module
-----------------------------------------
To successfully build and install the Python module a few prerequisites have to be installed first. First of all, a 
working installation of Python is required, of course. The module targets Python 3, and does NOT support Python 2. 
Furthermore, working compilers for C and Fortran have to be installed and on the PATH. On Windows, the build and
installation have been tested with MinGW, using gcc and gfortran. To force the system to use MinGW, you may have to
create a file called `setup.cfg` in the root of the repo before building/installing with the following contents:

```INI
[build]
compiler=mingw32
```

A few packages have to be installed within the Python environment before building/installing too. The `setup.py` script
depends on these directly, so they have to be installed manually before invoking it. These packages are listed in the
`requirements.txt` file in the root of this repo. Install them simply by running:

```bash
pip install -r requirements.txt
```

Any other dependencies used by the Python packages will be installed automatically by the `setupy.py` script.

Next, the XRotor Python module can be installed simply by running the following command from the root of the repo:

```bash
pip install .
```

This should automatically build the Fortran shared object for your system and install the package for the active Python
environment.

Using the Module
----------------
All XRotor operations are performed using the `XRotor` class. So the first step when using this module is to create an
instance of this class:

```pycon
>>>  from xrotor import XRotor
>>>  xr = XRotor()
```

If this does not produce any errors, the installtion should be functioning properly. 
A test case is installed along with the module. To run it in XRotor, execute the following commands in the same python
console:

```pycon
>>>  from xrotor.model import Case
>>>  from xrotor.test import case
>>>  xr.case = Case.from_dict(case)
>>>  xr.operate(1, 2000)

 Iter     dGmax  @Imax    gGrms       Av        Aw         Be       rlx
   1    0.397E-01    1   0.167E-02    0.1553    0.1750     9.966    0.2000
   2    0.225E-01    1   0.103E-02    0.1553    0.1764     9.966    0.2000
   3    0.154E-01    1   0.733E-03    0.1553    0.1776     9.966    0.2000
   4    0.116E-01    1   0.559E-03    0.1553    0.1824     9.966    1.0000
   5    0.514E-03   29   0.224E-04    0.1553    0.1825     9.966    0.2000
   6    0.412E-03   29   0.179E-04    0.1553    0.1825     9.966    1.0000
   7    0.227E-05   29   0.742E-07    0.1553    0.1825     9.966    0.2000

```

These commands initialize a sample propeller definition in XRotor and operate it at a fixed RPM of 2000 rev/min. The 
output from the last function should be familiar to anyone who has used the original XRotor console application before:
it is the convergence history of the OPER command. The familiar solution results can also be printed to the screen with
the following command:

```pycon
>>>  xr.print_case()

 ===========================================================================
 Free Tip Potential Formulation Solution:
                                                  Wake adv. ratio:    0.18252
 no. blades :  2            radius(m)  :   0.8300     adv. ratio:     0.15532
 thrust(n)  :   481.        power(w)   :  0.219E+05   torque(n-m):   105.
 Efficiency :  0.5929       speed(m/s) :   27.000     rpm        :   2000.000
 Eff induced:  0.8510       Eff ideal  :   0.8993     Tcoef      :     0.4981
 Tnacel(n)  :     0.0132    hub rad.(m):   0.0600     disp. rad. :    0.0000
 Tvisc(n)   :   -15.5982    Pvisc(w)   :  0.615E+04
 rho(kg/m3) :   1.22500     Vsound(m/s):  340.000     mu(kg/m-s) : 0.1789E-04
 ---------------------------------------------------------------------------
 Sigma:        NaN
                Ct:    0.04658     Cp:    0.03833    j:    0.48795
                Tc:    0.49815     Pc:    0.84023  adv:    0.15532

  i  r/r   c/r  beta(deg)  cl     Cd    rEx10^6 Mach   effi  effp  na.u/u
  1 0.081 0.1458  59.81  0.433   0.0934   0.25  0.090  3.944 0.555   0.000
  2 0.108 0.1475  56.04  0.501   0.0799   0.28  0.097  1.343 0.691   0.000
  3 0.149 0.1500  50.09  0.602   0.0720   0.32  0.110  1.036 0.782   0.000
  4 0.196 0.1527  43.42  0.642   0.0687   0.38  0.127  0.953 0.808   0.000
  5 0.244 0.1558  36.98  0.624   0.0620   0.44  0.147  0.933 0.816   0.000
  6 0.292 0.1594  31.45  0.581   0.0559   0.52  0.169  0.933 0.811   0.000
  7 0.341 0.1634  27.32  0.544   0.0521   0.60  0.191  0.928 0.800   0.000
  8 0.388 0.1672  24.38  0.521   0.0495   0.69  0.214  0.914 0.789   0.000
  9 0.435 0.1697  22.20  0.506   0.0474   0.77  0.236  0.896 0.781   0.000
 10 0.481 0.1699  20.38  0.494   0.0459   0.85  0.258  0.882 0.772   0.000
 11 0.526 0.1679  18.76  0.484   0.0450   0.91  0.280  0.875 0.761   0.000
 12 0.569 0.1639  17.37  0.476   0.0444   0.95  0.301  0.871 0.749   0.000
 13 0.611 0.1583  16.20  0.471   0.0440   0.99  0.322  0.867 0.738   0.000
 14 0.652 0.1515  15.24  0.470   0.0438   1.00  0.342  0.864 0.729   0.000
 15 0.690 0.1438  14.45  0.471   0.0436   1.00  0.361  0.860 0.722   0.000
 16 0.727 0.1356  13.78  0.475   0.0436   0.99  0.380  0.856 0.715   0.000
 17 0.762 0.1271  13.22  0.479   0.0438   0.98  0.397  0.853 0.709   0.000
 18 0.794 0.1188  12.73  0.483   0.0441   0.95  0.414  0.849 0.702   0.000
 19 0.825 0.1106  12.29  0.486   0.0447   0.92  0.429  0.846 0.694   0.000
 20 0.853 0.1029  11.91  0.487   0.0455   0.88  0.443  0.843 0.685   0.000
 21 0.879 0.0958  11.57  0.485   0.0466   0.84  0.456  0.838 0.673   0.000
 22 0.903 0.0892  11.26  0.479   0.0481   0.81  0.468  0.832 0.660   0.000
 23 0.924 0.0834  10.99  0.468   0.0501   0.77  0.479  0.824 0.642   0.000
 24 0.943 0.0782  10.74  0.450   0.0529   0.74  0.488  0.813 0.618   0.000
 25 0.959 0.0738  10.53  0.423   0.0570   0.71  0.496  0.796 0.585   0.000
 26 0.972 0.0701  10.35  0.385   0.0635   0.68  0.503  0.775 0.537   0.000
 27 0.983 0.0672  10.20  0.335   0.0740   0.66  0.509  0.747 0.467   0.000
 28 0.991 0.0651  10.08  0.270   0.0913   0.65  0.513  0.713 0.365   0.000
 29 0.997 0.0636  10.01  0.193   0.1177   0.63  0.516  0.675 0.236   0.000
 30 0.999 0.0629   9.97  0.125   0.1467   0.63  0.518  0.642 0.123   0.000
```

If the module is working as it should, the output should match the output shown above.

At the time of writing, the only the two operating modes available are fixed RPM and fixed thrust with fixed blade pitch.
Both can be invoked by calling the `operate` member function on an instance of the `XRotor` class. The first argument
to this function specifies which mode is used: 1 for fixed RPM, as was demonstrated above; 2 for fixed thrust at fixed
blade pitch. The second argument to the function specifies the value for the RPM/thrust. 

See the documentation for more detailed explanation of how to use the API.
 