
General
-------
This is a containerized, stripped down version of XROTOR. All the modification, design, and graphical functionality has
been removed. The only main menu options that are available in this stripped down version are:
* OPER, which allows for the calculation of performance characteristics at given operating conditions;
* BEND, which allows for the calculation of structural loads and deformations;
* NOIS, which allows for the calculation of the acoustic signature;
* LOAD, which loads a propeller definition file; and
* DISP, which displays the current propeller characteristics data onscreen.

Installation
------------
The installation is straightforward thanks to the containerization. The only requirement is that [Docker]() is installed
on the system and that the `docker` command is in the `PATH` of whichever console environment is used. 
Then simply type:
```bash
docker build -t xrotor .
docker run -it xrotor
```
The first of these two commands will build the Docker image, installing all dependencies and building and installing
XROTOR. The `-t xrotor` option tags the container image with 'xrotor'. The second command starts the container and 
presents the user with a interactive shell. Through this shell, XROTOR can be started simply by entering the command
 `xrotor`.
 
 The container can be stopped using `CTRL-C` on any UNIX system (e.g. Linux, Mac OSx). On Windows machines, however, 
 this will only cause the terminal to exit the container's shell. To actually stop the container, Windows users should 
 therefore also issue the command `docker stop xrotor` after they exit the container's shell.
 
 