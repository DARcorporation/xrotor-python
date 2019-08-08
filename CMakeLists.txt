cmake_minimum_required(VERSION 3.12)
project(xrotor Fortran)
project(xrotor_exec Fortran)
project(xrotor_test Fortran)

enable_language(Fortran)
add_library(xrotor SHARED
        src/i_common.f90
        src/m_spline.f90
        src/m_userio.f90
        src/m_vortex.f90
        src/m_xaero.f90
        src/m_xbend.f90
        src/m_xio.f90
        src/m_xnoise.f90
        src/m_xoper.f90
        src/m_xrotor.f90
        src/m_xutils.f90
        src/s_xrotor.f90
        src/api.f90)

#add_executable(xrotor_exec
#        src/i_common.f90
#        src/m_spline.f90
#        src/m_userio.f90
#        src/m_vortex.f90
#        src/m_xaero.f90
#        src/m_xbend.f90
#        src/m_xio.f90
#        src/m_xnoise.f90
#        src/m_xoper.f90
#        src/m_xrotor.f90
#        src/m_xutils.f90
#        src/s_xrotor.f90
#        src/p_xbend.f90
#        src/p_xnoise.f90
#        src/p_xoper.f90
#        src/p_xrotor.f90)
#

add_executable(xrotor_test
        src/i_common.f90
        src/m_spline.f90
        src/m_userio.f90
        src/m_vortex.f90
        src/m_xaero.f90
        src/m_xbend.f90
        src/m_xio.f90
        src/m_xnoise.f90
        src/m_xoper.f90
        src/m_xrotor.f90
        src/m_xutils.f90
        src/s_xrotor.f90
        src/api.f90
        src/p_test.f90)
