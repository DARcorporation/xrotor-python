
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

Installing the Python Module
----------------------------
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

To test that installation run the following commands in the python console:

```Python console
>>>  from xrotor import XRotor
>>>  xr = XRotor()
```

If this does not produce any errors, the shared object is functioning properly. A test case is installed along with the
module. To run it in XRotor, execute the following commands in the same python console:

```Python console
>>>  from xrotor.model import Case
>>>  from xrotor.test import case
>>>  xr.case = Case.from_dict(case)
>>>  xr.operate(1, 2000)
>>>  xr.print_case()
```

These commands load a sample propeller definition, run it at a fixed RPM of 2000 rev/min, and print the results to the 
screen. 

See the documentation for more detailed explanation of how to use the API.


Building a Console Application
------------------------------
With this codebase it is also still possible to build a console application, like the original XROTOR. CMake is used to
accomplish this. Make sure it is installed, and that its aware of the appropriate compilers. Then the project can be 
build using the `CMakeLists.txt` stored in the root of the repo.
 