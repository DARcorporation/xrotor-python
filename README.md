
General
-------
This is a stripped down version of XROTOR. All the modification, design, and graphical functionality has
been removed. The only main menu options that are available in this stripped down version are:
* OPER, which allows for the calculation of performance characteristics at given operating conditions;
* BEND, which allows for the calculation of structural loads and deformations;
* NOIS, which allows for the calculation of the acoustic signature;
* LOAD, which loads a propeller definition file; and
* DISP, which displays the current propeller characteristics data onscreen.

Installing the python module
--------------------------
Make sure a working version of python is installed and on the path. Make sure Numpy is installed for this python 
environment. Then run the following command from the root of this repo:
```
python setup.py install
```
The package will then be build and installed for the active python environment.
 