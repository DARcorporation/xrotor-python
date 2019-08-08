program test_xrotor
    use, intrinsic :: iso_c_binding, only : c_int, c_bool, c_float
    use api, only : init, set_case, operate, show, get_number_of_stations, get_station_conditions
    real :: rho, vso, rmu, alt, vel, adv, r_hub, r_tip, r_wake, rake
    real :: geomdata(4, 6), polardata(20, 4)
    logical(c_bool) :: free, duct, wind
    real, allocatable :: xi(:), Re(:), M(:)
    real :: res
    integer :: n_stations

    rho = 1.225
    vso = 340.
    rmu = 1.789e-5
    alt = 1.
    vel = 27.
    adv = .15

    r_hub = .06
    r_tip = .83
    r_wake = 0.
    rake = 0.

    geomdata(1, :) = (/0.15, 0.30, 0.45, 0.60, 0.75, 0.90/)
    geomdata(2, :) = (/0.15, 0.16, 0.17, 0.16, 0.13, 0.09/)
    geomdata(3, :) = (/50.0, 30.7, 21.6, 16.5, 13.4, 11.3/)
    geomdata(4, :) = 0.

    polardata = reshape((/&
            ! alpha
            -20.0000, -18.0000, -16.0000, -14.0000, -12.0000, -10.0000,  -8.0000,  -6.0000,  -4.0000,  -2.0000, &
              2.0000,   4.0000,   6.0000,   8.0000,  10.0000,  12.0000,  14.0000,  16.0000,  18.0000,  20.0000, &
            ! cl
             -1.0550,  -1.2246,  -1.2551,  -1.3550,  -1.2452,  -1.0796,  -0.9098,  -0.6940,  -0.4275,  -0.2144, &
              0.2144,   0.4275,   0.6940,   0.9097,   1.0796,   1.2453,   1.3555,   1.3913,   1.3048,   1.1910, &
            ! cd
              0.1450,   0.0891,   0.0622,   0.0256,   0.0193,   0.0151,   0.0122,   0.0098,   0.0073,   0.0058, &
              0.0058,   0.0073,   0.0098,   0.0122,   0.0151,   0.0193,   0.0256,   0.0412,   0.0819,   0.1332, &
            ! cm
              0.0102,  -0.0132,  -0.0370,  -0.0263,  -0.0133,  -0.0054,   0.0040,   0.0041,  -0.0061,  -0.0030, &
              0.0030,   0.0061,  -0.0041,  -0.0040,   0.0054,   0.0133,   0.0262,   0.0303,   0.0111,  -0.0173/), &
    (/20, 4/))

    polardata(:, 1) = polardata(:, 1) * 3.1415 / 180.

    free = .true.
    duct = .false.
    wind = .false.

    call init()
    call set_case(&
        rho, vso, rmu, alt, vel, adv, &
        r_hub, r_tip, r_wake, rake, &
        2, &
        6, geomdata, &
        1, (/20/), (/0./), polardata, &
        free, duct, wind)
    res = operate(4, 2000.)
    call show()

    n_stations = get_number_of_stations()
    allocate(xi(n_stations))
    allocate(Re(n_stations))
    allocate(M(n_stations))
    call get_station_conditions(n_stations, xi, Re, M)
end program test_xrotor