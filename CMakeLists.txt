cmake_minimum_required(VERSION 3.12)
project(xrotor Fortran)

enable_language(Fortran)
add_executable(xrotor
        src/spline.f
        src/userio.f
        src/vortex.f
        src/xaero.f
        src/xbend.f
        src/xio.f
        src/xnoise.f
        src/xoper.f
        src/xrotor.f
        src/xutils.f)