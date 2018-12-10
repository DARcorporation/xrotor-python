cmake_minimum_required(VERSION 3.12)
project(xrotor Fortran)

enable_language(Fortran)
add_executable(xrotor
        xrotor/common.f90
        xrotor/spline.f90
        xrotor/userio.f90
        xrotor/vortex.f90
        xrotor/xaero.f90
        xrotor/xbend.f90
        xrotor/xio.f90
        xrotor/xnoise.f90
        xrotor/xoper.f90
        xrotor/xrotor.f90
        xrotor/xutils.f90)