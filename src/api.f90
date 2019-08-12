!*==API.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
!***********************************************************************
!   Copyright (c) 2018 D. de Vries
!
!   This file is part of XRotor.
!
!   XRotor is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   XRotor is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with XRotor.  If not, see <https://www.gnu.org/licenses/>.
!***********************************************************************

module api
    use, intrinsic :: iso_c_binding, only : c_float, c_double, c_int, c_bool, c_char
    use :: i_common, only : Common
    implicit none
    private
    public set_print, get_print, &
            set_max_iter, get_max_iter, &
            set_use_compr_corr, get_use_compr_corr, &
            init, set_case, operate, dp, show,  save_prop, &
            get_rms, get_performance, get_blade_angle_change, &
            get_number_of_stations, get_station_conditions, load_prop

    integer, parameter :: dp = kind(0.D0)

    type (Common), private :: ctxt

contains

    subroutine set_print(setting) bind(c, name = 'set_print')
        use i_common, only : show_output
        logical(c_bool), intent(in) :: setting
        show_output = setting
    end

    function get_print() bind(c, name = 'get_print')
        use i_common, only : show_output
        logical(c_bool) :: get_print
        get_print = show_output
    end

    subroutine set_max_iter(setting) bind(c, name = 'set_max_iter')
        integer(c_int), intent(in) :: setting
        ctxt%nitera = ctxt%nitera
    end

    function get_max_iter() bind(c, name = 'get_max_iter')
        integer(c_int) :: get_max_iter
        get_max_iter = ctxt%nitera
    end

    subroutine set_use_compr_corr(use_compr_corr) bind(c, name = 'set_compr_corr')
        logical(c_bool) :: use_compr_corr
        ctxt%use_compr_corr = use_compr_corr
    end

    function get_use_compr_corr() bind(c, name = 'get_use_compr_corr')
        logical(c_bool) :: get_use_compr_corr
        get_use_compr_corr = ctxt%use_compr_corr
    end

    subroutine init() bind(c, name = 'init')
        use m_xrotor, only : init_
        !ctxt = Common()
        call init_(ctxt)
    end

    subroutine set_case(&
            rho, vso, rmu, alt, vel, adv, &
            r_hub, r_tip, r_wake, rake, &
            n_blds, &
            n_geom, geomdata, &
            n_polars, n_polar_points, xi_polars, polardata, &
            free, duct, wind) bind(c, name = 'set_case')
        use i_common, only : pi
        use m_xio, only : initcase
        use m_xaero, only : putpolars

        real(c_float), intent(in) :: rho, vso, rmu, alt, vel, adv
        real(c_float), intent(in) :: r_hub, r_tip, r_wake, rake
        integer(c_int), intent(in) :: n_blds, n_geom
        real(c_float), intent(in) :: geomdata(4, n_geom)
        integer(c_int), intent(in) :: n_polars, n_polar_points(n_polars)
        real(c_float), intent(in) :: xi_polars(n_polars), polardata(sum(n_polar_points), 4)
        logical(c_bool), intent(in) :: free, duct, wind

        real :: my_polardata(sum(n_polar_points), 4)

        integer :: i

        ctxt%rho = rho
        ctxt%vso = vso
        ctxt%rmu = rmu
        ctxt%alt = alt
        ctxt%vel = vel
        ctxt%adv = adv

        ctxt%rad = r_tip
        ctxt%xi0 = r_hub / r_tip
        ctxt%xw0 = r_wake / r_tip
        ctxt%rake = rake

        ctxt%nblds = n_blds

        do i = 1, n_geom
            ctxt%xi(i) = geomdata(1, i)
            ctxt%ch(i) = geomdata(2, i)
            ctxt%beta(i) = geomdata(3, i) * pi / 180.
            ctxt%beta0(i) = ctxt%beta(i)
            ctxt%ubody(i) = geomdata(4, i)
        enddo

        my_polardata = polardata
        my_polardata(:, 1) = polardata(:, 1) * pi / 180.
        call putpolars(ctxt, n_polars, n_polar_points, xi_polars, my_polardata)

        call initcase(ctxt, n_geom, .false.)
    end

    function operate(spec, value, fix, fixed) bind(c, name = 'operate')
        use m_xoper, only : aper
        use i_common, only : show_output, pi
        real(c_float) :: operate
        integer(c_int), intent(in) :: spec
        real(c_float), intent(in) :: value
        integer(c_int), optional, intent(in) :: fix
        real(c_float), optional, intent(in) :: fixed
        integer :: ifix, i

        operate = 1.0

        if (present(fix)) then
            if (fix==1.and..not.present(fixed)) then
                print *, "If 'fix' is given, 'fixed' must be given too."
                return
            endif
            ifix = fix
        else
            ifix = 2
        endif

        select case (spec)
        case (1)
            ctxt%tspec = value
        case (2)
            ctxt%qspec = value
        case (3)
            ctxt%pspec = value
        case (4)
            ctxt%adv = ctxt%vel / (ctxt%rad * value * pi / 30.)
        case default
            print *, "Unknown value for 'spec'. Should be 1, 2, 3, or 4."
            return
        endselect

        if (ifix==1) then
            ctxt%adv = ctxt%vel / (ctxt%rad * fixed * pi / 30.)
        elseif (ifix/=2) then
            print *, "Unknown value for 'fix'. Should be 1 or 2."
            return
        endif

        ctxt%conv = .false.
        call aper(ctxt, spec, ifix, ctxt%loprini)

        if (ifix==1.and.spec/=4) then
            if (ctxt%conv) then
                !----- convergence was achieved: show blade angle change incurred
                if (show_output) write (*, 99001) ctxt%dbeta * 180.0 / pi
                99001          format (' Blade angle changed', f7.3, ' degrees')
            else
                !----- convergence failed: restore clobbered blade angles
                do i = 1, ctxt%ii
                    ctxt%beta(i) = ctxt%beta(i) - ctxt%dbeta
                    ctxt%beta0(i) = ctxt%beta0(i) - ctxt%dbeta
                enddo
            endif
        endif

        operate = ctxt%rms
    end

    subroutine show() bind(c, name = 'show')
        use m_xrotor, only : output
        call output(ctxt, 6)
    end

    subroutine save_prop() bind(c, name = 'save_prop')
        use m_xio, only : save
        call save(ctxt, 'output.json')
    end

    subroutine load_prop(fname)
        use m_xio, only : load
        character*(*) fname
        call load(ctxt, fname)
    end

    function get_rms() bind(c, name = 'get_rms')
        real(c_float) :: get_rms
        get_rms = ctxt%rms
    end

    subroutine get_performance(rpm, thrust, torque, power, efficiency) bind(c, name = 'get_performance')
        use i_common, only : pi
        real(c_float), intent(out) :: rpm, thrust, torque, power, efficiency

        thrust = ctxt%ttot * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
        torque = ctxt%qtot * ctxt%rho * ctxt%vel**2 * ctxt%rad**3
        power = ctxt%ptot * ctxt%rho * ctxt%vel**3 * ctxt%rad**2

        efficiency = ctxt%ttot / ctxt%ptot
        rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.)
    end

    function get_blade_angle_change() bind(c, name = 'get_blade_angle_change')
        use i_common, only : pi
        real(c_float) :: get_blade_angle_change
        get_blade_angle_change = ctxt%dbeta * 180.0 / pi
    end

    function get_number_of_stations() bind(c, name = 'get_number_of_stations')
        integer(c_int) :: get_number_of_stations
        get_number_of_stations = ctxt%ii
    end

    subroutine get_station_conditions(n, xi, Re, M) bind(c, name = 'get_station_conditions')
        use m_xoper, only : calcw
        integer(c_int), intent(in) :: n
        real(c_float), intent(out) :: xi(n), Re(n), M(n)
        real :: w
        integer :: i

        xi = ctxt%xi(1:n)
        Re = ctxt%re(1:n)

        do i = 1, n
            call calcw(ctxt, i, w)
            M(i) = w * ctxt%vel / ctxt%vso
        enddo
    end

end
