
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
--------------------------
Make sure a working version of python is installed and on the path. Make sure Numpy is installed for this python 
environment. Then run the following command from the root of this repo:
```
python setup.py install
```
The package will then be build and installed for the active python environment.

Note: by default, the repo is setup to compile on a Windows machine, using the MinGW system. If you want to compile it
as such, make sure MinGW's `bin` directory is on the path (while building AND to before using the installed python module).
If you want to use a different toolchain, edit `setup.cfg` accordingly.

The Python module includes the ability to present the user with a console interface, so it XROTOR can be used interactively
like the original application. To invoke this interface, run the following code in python:
```python
import xrotor
xrotor.flib.rotor()
```

Building a Console Application
------------------------------
With this codebase it is also still possible to build a console application, like the original XROTOR. CMake is used to
accomplish this. Make sure it is installed, and that its aware of the appropriate compilers. Then the project can be 
build using the `CMakeLists.txt` stored in the root of the repo.
 