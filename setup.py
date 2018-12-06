import glob
from numpy.distutils.core import Extension

ext = Extension(name='xrotor', sources=glob.glob("src/fortran/*.f90"), extra_f90_compile_args=['-Ofast'])

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(name='example',
          ext_modules=[ext])
