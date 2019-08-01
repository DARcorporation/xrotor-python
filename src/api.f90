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
    use :: m_common, only : Common
    implicit none
    private
    public init, set_case, operate, dp

    integer, parameter :: dp = kind(0.d0)

    type(Common), private :: ctxt

contains

    subroutine set_print(setting) bind(c, name = 'set_print')
        use m_common, only : show_output
        logical(c_bool), intent(in) :: setting
        show_output = setting
    end subroutine set_print

    function get_print() bind(c, name = 'get_print')
        use m_common, only : show_output
        logical(c_bool) :: get_print
        get_print = show_output
    end function get_print

    subroutine set_max_iter(setting) bind(c, name = 'set_max_iter')
        integer(c_int), intent(in) :: setting
        ctxt%nitera = ctxt%nitera
    end subroutine set_max_iter

    function get_max_iter() bind(c, name = 'get_max_iter')
        integer(c_int) :: get_max_iter
        get_max_iter = ctxt%nitera
    end function get_max_iter

    subroutine init() bind(c, name = 'init')
        use m_xrotor, only : init_
        ctxt = Common()
        call init_(ctxt)
    end subroutine init

    subroutine set_case(&
            rho, vso, rmu, alt, vel, adv, &
            r_hub, r_tip, r_wake, rake, &
            n_blds, n_aero, n_geom, &
            aerodata, geomdata, &
            free, duct, wind) bind(c, name = 'set_case')
        use m_xaero, only : putaero
        use m_common, only: pi
        use m_xio, only : initcase
        real    (c_float), intent(in) :: rho, vso, rmu, alt, vel, adv
        real    (c_float), intent(in) :: r_hub, r_tip, r_wake, rake
        integer (c_int), intent(in) :: n_blds, n_aero, n_geom
        real    (c_float), intent(in) :: aerodata(14, n_aero), geomdata(4, n_geom)
        logical (c_bool), intent(in) :: free, duct, wind

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

        ctxt%naero = n_aero
        do i = 1, n_aero
            call putaero(ctxt, i, aerodata(1, i), &
                    aerodata(2, i) * pi / 180., aerodata(3, i), aerodata(4, i), aerodata(5, i), &
                    aerodata(6, i), aerodata(7, i), aerodata(8, i), aerodata(9, i), &
                    aerodata(10, i), aerodata(11, i), aerodata(12, i), aerodata(13, i), &
                    aerodata(14, i))
        end do

        do i = 1, n_geom
            ctxt%xi(i) = geomdata(1, i)
            ctxt%ch(i) = geomdata(2, i)
            ctxt%beta(i) = geomdata(3, i) * pi / 180.
            ctxt%beta0(i) = ctxt%beta(i)
            ctxt%ubody(i) = geomdata(4, i)
        end do

        call initcase(ctxt, n_geom, .false.)
    end subroutine set_case

    function operate(spec, value, fix, fixed) bind(c, name = 'operate')
        use m_xoper, only : aper
        use m_common, only : show_output, pi
        real(c_float) :: operate
        integer(c_int), intent(in) :: spec
        real(c_float), intent(in) :: value
        integer(c_int), optional, intent(in) :: fix
        real(c_float), optional, intent(in) :: fixed
        integer :: ifix, i

        operate = 1.0

        if (present(fix)) then
            if (fix == 1 .and. .not. present(fixed)) then
                print *, "If 'fix' is given, 'fixed' must be given too."
                return
            end if
            ifix = fix
        else
            ifix = 2
        end if

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
        end select

        if (ifix == 1) then
            ctxt%adv = ctxt%vel / (ctxt%rad * fixed * pi / 30.)
        elseif (ifix /= 2) then
            print *, "Unknown value for 'fix'. Should be 1 or 2."
            return
        end if

        ctxt%conv = .false.
        call aper(ctxt, spec, ifix, ctxt%loprini)

        if (ifix == 1 .and. spec /= 4) then
            if(ctxt%conv) then
                !----- convergence was achieved: show blade angle change incurred
                if (show_output) write(*, 1550) ctxt%dbeta * 180.0 / pi
                1550 format(' Blade angle changed', f7.3, ' degrees')
            else
                !----- convergence failed: restore clobbered blade angles
                do i = 1, ctxt%ii
                    ctxt%beta(i) = ctxt%beta(i) - ctxt%dbeta
                    ctxt%beta0(i) = ctxt%beta0(i) - ctxt%dbeta
                enddo
            endif
        end if

        operate = ctxt%rms
    end function operate

    subroutine show() bind(c, name = 'show')
        use m_xrotor, only : output
        call output(ctxt, 6)
    end subroutine show

    subroutine save_prop() bind(c, name = 'save_prop')
        use m_xio, only : save
        call save(ctxt, 'output.prop')
    end subroutine save_prop

    function get_rms() bind(c, name = 'get_rms')
        real(c_float) :: get_rms
        get_rms = ctxt%rms
    end function get_rms

    subroutine get_performance(rpm, thrust, torque, power, efficiency) bind(c, name = 'get_performance')
        use m_common, only: pi
        real(c_float), intent(out) :: rpm, thrust, torque, power, efficiency

        thrust = ctxt%ttot * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
        torque = ctxt%qtot * ctxt%rho * ctxt%vel**2 * ctxt%rad**3
        power = ctxt%ptot * ctxt%rho * ctxt%vel**3 * ctxt%rad**2

        efficiency = ctxt%ttot / ctxt%ptot
        rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.)
    end subroutine get_performance

    function get_blade_angle_change() bind(c, name = 'get_blade_angle_change')
        use m_common, only: pi
        real(c_float) :: get_blade_angle_change
        get_blade_angle_change = ctxt%dbeta * 180.0 / pi
    end function get_blade_angle_change

    function get_number_of_stations() bind(c, name = 'get_number_of_stations')
        integer(c_int) :: get_number_of_stations
        get_number_of_stations = ctxt%ii
    end function get_number_of_stations

    subroutine get_station_conditions(n, xi, Re) bind(c, name = 'get_station_conditions')
        integer(c_int), intent(in) :: n
        real(c_float), intent(out) :: xi(n), Re(n)
        integer :: i

        do i = 1, n
            xi(i) = ctxt%xi(i)
            Re(i) = ctxt%re(i)
        end do
    end subroutine get_station_conditions

end module api
