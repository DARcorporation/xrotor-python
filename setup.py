import glob
from numpy.distutils.core import Extension

ext = Extension(name='xrotor', sources=glob.glob("src/*.f90"))

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(name='example',
          ext_modules=[ext])
