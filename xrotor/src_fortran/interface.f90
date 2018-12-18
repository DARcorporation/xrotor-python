module interface
    use, intrinsic :: iso_c_binding, only: c_float, c_double, c_int, c_ptr, c_loc, c_f_pointer, c_bool, c_char
    use :: mod_common, only: Common, pi, ix, nax
    implicit none
    private
    public init, set_case, operate, dp

    integer, parameter :: dp = kind(0.d0)

contains

    subroutine init(handle) bind(c, name='init')
        type(c_ptr), intent(inout) :: handle

        type(Common), pointer :: ctxt
        allocate(ctxt)

        call init_(ctxt)

        handle = c_loc(ctxt)
    end subroutine init

    subroutine set_case(handle, &
            rho, vso, rmu, alt, vel, adv, &
            r_hub, r_tip, r_wake, rake, &
            n_blds, n_aero, n_geom, &
            aerodata, geomdata, &
            free, duct, wind) bind(c, name='set_case')
        type    (c_ptr),    intent(in), value   :: handle
        real    (c_float),  intent(in)          :: rho, vso, rmu, alt, vel, adv
        real    (c_float),  intent(in)          :: r_hub, r_tip, r_wake, rake
        integer (c_int),    intent(in)          :: n_blds, n_aero, n_geom
        real    (c_float),  intent(in)          :: aerodata(14, n_aero), geomdata(4, n_geom)
        logical (c_bool),   intent(in)          :: free, duct, wind

        integer :: i

        type(Common), pointer :: ctxt
        call c_f_pointer(handle, ctxt)

        ctxt%rho    = rho
        ctxt%vso    = vso
        ctxt%rmu    = rmu
        ctxt%alt    = alt
        ctxt%vel    = vel
        ctxt%adv    = adv

        ctxt%rad    = r_tip
        ctxt%xi0    = r_hub / r_tip
        ctxt%xw0    = r_wake / r_tip
        ctxt%rake   = rake

        ctxt%nblds  = n_blds

        ctxt%naero = n_aero
        do i=1, n_aero
            call putaero(ctxt, i,  aerodata(1 , i), &
            aerodata(2 , i) * pi / 180., aerodata(3 , i), aerodata(4 , i), aerodata(5 , i), &
            aerodata(6 , i), aerodata(7 , i), aerodata(8 , i), aerodata(9 , i), &
            aerodata(10, i), aerodata(11, i), aerodata(12, i), aerodata(13, i), &
            aerodata(14, i))
        end do

        do i=1, n_geom
            ctxt%xi(i)  = geomdata(1, i)
            ctxt%ch(i)  = geomdata(2, i)
            ctxt%beta(i) = geomdata(3, i) * pi / 180.
            ctxt%beta0(i)  = ctxt%beta(i)
            ctxt%ubody(i) = geomdata(4, i)
        end do

        call initcase(ctxt, n_geom, .false.)
    end subroutine set_case

    subroutine operate(handle, spec, val) bind(c, name='operate')
        type(c_ptr), intent(in), value :: handle
        integer(c_int), intent(in) :: spec
        real(c_float),  intent(in) :: val

        type(Common), pointer :: ctxt
        call c_f_pointer(handle, ctxt)

        if (spec == 1) then
            ctxt%adv = ctxt%vel / (ctxt%rad * val * pi / 30._dp)
            ctxt%conv = .false.
            call aper(ctxt, 4, 2, ctxt%loprini)
        elseif (spec == 2) then
            ctxt%tspec = val
            ctxt%conv = .false.
            call aper(ctxt, 1, 2, ctxt%loprini)
        else
            print *, 'Unknown value for spec. Should be 1 to specify rpm, or 2 to specify thrust.'
        end if
    end subroutine operate

    subroutine show(handle) bind(c, name='show')
        type(c_ptr), intent(in), value :: handle

        type(Common), pointer :: ctxt
        call c_f_pointer(handle, ctxt)

        call output(ctxt, 6)
    end subroutine show

    subroutine save_prop(handle) bind(c, name='save_prop')
        type(c_ptr), intent(in), value :: handle

        type(Common), pointer :: ctxt
        call c_f_pointer(handle, ctxt)

        call save(ctxt, 'output.prop')
    end subroutine save_prop

    subroutine get_performance(handle, rpm, thrust, torque, power, efficiency) bind(c, name='get_performance')
        type(c_ptr), intent(in), value :: handle
        real(c_float), intent(out) :: rpm, thrust, torque, power, efficiency

        type(Common), pointer :: ctxt
        call c_f_pointer(handle, ctxt)

        thrust = ctxt%ttot * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
        torque = ctxt%qtot * ctxt%rho * ctxt%vel**2 * ctxt%rad**3
        power  = ctxt%ptot * ctxt%rho * ctxt%vel**3 * ctxt%rad**2

        efficiency = ctxt%ttot / ctxt%ptot
        rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 180.)
    end subroutine get_performance

end module interface