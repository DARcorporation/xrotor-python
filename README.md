
General
-------
This is a stripped down version of XROTOR. All the modification, design, and graphical functionality has
been removed. The only main menu options that are available in this stripped down version are:
* OPER, which allows for the calculation of performance characteristics at given operating conditions;
* BEND, which allows for the calculation of structural loads and deformations;
* NOIS, which allows for the calculation of the acoustic signature;
* LOAD, which loads a propeller definition file; and
* DISP, which displays the current propeller characteristics data onscreen.

This repo can be used in two ways:
1 The stripped down version of XROTOR can be build as a standalone console application, like to original XROTOR program;
2 A python module can be build, which allows for all subroutines to be called directly from python.

Building the Console Application
--------------------------------
CMake is used to manage the building of the console application. Make sure CMake is available on the PATH, and that it 
is able to find a suitable Fortran 90 compiler. Then run the following command from the root of this repo:
```
cmake --build path/to/output/dir --target xrotor
```
The compiled application will be created in the directory pointed to by `path/to/output/dir`.

Building the python module
--------------------------
Make sure a working version of python is installed and on the path. Make sure Numpy is installed for this python 
environment. Then run the following command from the root of this repo:
```
python setup.py build
```
The module library will be created under `build/lib` as a `.pyd` file. This file can be copied to a suitable location
and can be imported as if it were any old python module. Note that the `lib` directory may be post-fixed with the 
architecture the build was performed on (for example, `lib.win-amd64-3.6`). The `.pyd` file is also likely to be 
post-fixed. Note, however, that it can simply be imported in python using `import xrotor`, without specifying the 
post-fix.
 