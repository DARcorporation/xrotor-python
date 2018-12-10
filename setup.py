import glob
from numpy.distutils.core import Extension, setup

flib = Extension(name='xrotor.flib', sources=glob.glob("xrotor/*.f90"), extra_f90_compile_args=['-Ofast'])

if __name__ == '__main__':
    setup(name='xrotor',
          packages=['xrotor'],
          ext_modules=[flib])
