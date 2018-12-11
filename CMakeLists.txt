cmake_minimum_required(VERSION 3.12)
project(xrotor Fortran)

enable_language(Fortran)
add_executable(xrotor
        xrotor/src_fortran/common.f90
        xrotor/src_fortran/spline.f90
        xrotor/src_fortran/userio.f90
        xrotor/src_fortran/vortex.f90
        xrotor/src_fortran/xaero.f90
        xrotor/src_fortran/xbend.f90
        xrotor/src_fortran/xio.f90
        xrotor/src_fortran/xnoise.f90
        xrotor/src_fortran/xoper.f90
        xrotor/src_fortran/xrotor.f90
        xrotor/src_fortran/xutils.f90)