cmake_minimum_required(VERSION 3.12)
project(xrotor Fortran)

enable_language(Fortran)
add_library(xrotor SHARED
        src/fortran/common.f90
        src/fortran/spline.f90
        src/fortran/userio.f90
        src/fortran/vortex.f90
        src/fortran/xaero.f90
        src/fortran/xbend.f90
        src/fortran/xio.f90
        src/fortran/xnoise.f90
        src/fortran/xoper.f90
        src/fortran/xrotor.f90
        src/fortran/xutils.f90
        src/fortran/interface.f90)
add_custom_command(TARGET xrotor POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:xrotor> ../lib/xrotor.so
)
