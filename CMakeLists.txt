cmake_minimum_required(VERSION 3.12)
project(xrotor Fortran)

enable_language(Fortran)
add_library(xrotor SHARED
        src/m_common.f90
        src/m_spline.f90
        src/m_userio.f90
        src/m_vortex.f90
        src/m_xaero.f90
        src/m_xbend.f90
        src/m_xio.f90
        src/xnoise.f90
        src/xoper.f90
        src/xrotor.f90
        src/xutils.f90
        src/api.f90)
