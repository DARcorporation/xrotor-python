!*==M_XROTOR.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
!***********************************************************************
!   Copyright (c) 2018 D. de Vries
!   Original Copyright (c) 2011 Mark Drela
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
!
module m_xrotor
    implicit none
contains


    subroutine init_(ctxt)
        use i_common, only : Common, show_output, ix, jx
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        integer I
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        !--------------------------------------
        !     Initializes everything
        !--------------------------------------
        !
        ctxt%greek = .false.
        !
        !
        !---- xrotor defaults
        ctxt%urduct = 1.0
        !
        call setdef(ctxt)
        !
        if (ctxt%duct) then
            if (show_output) write (*, *) 'Aprop/Aexit initialized to 1.0'
            ctxt%urduct = 1.0
        endif
        !
        ctxt%xinf = 3.0         ! r/r at which bc at infinity is applied
        ctxt%nn = 32            ! number of perturbation potential harmonics
        ctxt%iinf = ctxt%ii + ctxt%ii / 2     ! number of discrete potential harmonic stations
        ctxt%conv = .false.    ! operating point solution existence flag
        ctxt%lstruc = .false.   ! indicates if structural properties are available
        !
        ctxt%name = ' '
        ctxt%savfil = ' '
        !
        !---- acceleration due to gravity for scaling centrifugal blade tension (m/s^2)
        ctxt%gee = 9.81
        !
        !---- adw factor (multiplies tinv/pinv in adw calculation)
        ctxt%adwfctr = 1.0
        !
        if (ctxt%ii>ix) stop 'array overflow.  ix too small'
        if (ctxt%iinf>jx) stop 'array overflow.  jx too small'
        !
        !---- actual-rotor radius is always 1 (non-dimensionalized with itself)
        ctxt%xitip = 1.0
        !
        !---- default nacelle, wake perturbation velocities (non-existent)
        do i = 1, ctxt%ii
            ctxt%ubody(i) = 0.
        enddo
        !
        !---- no slipstream velocity profiles
        ctxt%nadd = 0
        !
        !---- number of defined cases
        ctxt%ncase = 0
        ctxt%kcase = 0
        !
        !---- max number of iterations for design, analysis
        ctxt%niterd = 40
        ctxt%nitera = 40
        !
        !---- do not initialize rotor at each design cycle
        ctxt%ldesini = .false.
        !
        !---- do initialize rotor at each design cycle
        ctxt%loprini = .true.
        !
        !---- no engine load line to start
        ctxt%lpwrvar = .false.
        ctxt%npwrvar = 0
        !
        !---- no rotor yet
        ctxt%lrotor = .false.
        do i = 1, ix
            ctxt%iaero(i) = 0
        enddo
        !
    end
    ! init



    subroutine setdef(ctxt)
        use m_xaero, only : putpolars, setiaero
        use i_common, only : Common, ix, pi
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real A0, CDMIN, CLDMIN, CLMAX, CLMIN, CMCON, DCDCL2, DCLDA, DCLDA_STALL, &
                & DCL_STALL, MCRIT, REREF, REXP, XISECT
        integer I
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        ! Default polar is of a NACA 0012 at Re = 1e6 and M = 0.0
        real :: polardata(20, 4) = reshape((/&
                ! alpha (deg)
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
        ! convert angles of attack from degrees to radians
        polardata(:, 1) = polardata(:, 1) * pi / 180.
        !
        !---- hard-wired start-up defaults
        !ccihi
        ctxt%rake = 0.0
        !
        ctxt%vel = 1.0
        ctxt%alt = 0.0
        call atmo(ctxt%alt, ctxt%vso, ctxt%rho, ctxt%rmu)     ! sea level atmosphere parameters
        !cc      rho =  1.226      ! fluid density         kg/m**3
        !cc      rmu =  1.78e-05   ! dynamic viscosity     kg/m-s
        !cc      vso =  340.0      ! speed of sound        m/s
        !
        !--- Install data into aero section #1
        call putpolars(ctxt, 1, (/20/), (/0./), polardata)
        !
        ctxt%xpitch = 0.3      ! x/c location of pitch axis
        !
        ctxt%ii = 30          ! number of radial stations
        ctxt%incr = 2        ! radial station increment for terminal output
        ctxt%ixspac = 2        ! r/r spacing flag
        !
        ctxt%vrtx = .false.   ! vortex wake (ctxt%t)        / graded momentum(f) flag
        ctxt%fast = .false.   ! graded momentum(ctxt%t)     / potential formulation(f) flag
        ctxt%free = .true.    ! self-deforming wake(ctxt%t) / rigid wake(f) flag
        ctxt%duct = .false.   ! ducted (ctxt%t)             / ctxt%free-tip (f)  flag
        !
        ctxt%terse = .false.   ! ctxt%terse-output flag
        !
        ctxt%lvnorm = .true.   ! flight speed used for normalization
        !
    end
    ! setdef


    subroutine atmo(alspec, vsoalt, rhoalt, rmualt)
        use i_common, only : show_output
        !*** Start of declarations inserted by SPAG
        real ALFRAC, ALSPEC, DALT, DRHO, DRMU, DVSO, RHOALT, RMUALT, VSOALT
        integer I, N
        !*** End of declarations inserted by SPAG
        !---------------------------------------------------------
        !     Returns speed of sound (vso) in m/s, density (rho)
        !     in kg/m^3, and dynamic viscosity (rmu) in kg/m-s
        !     of standard atmosphere at specified altitude alspec
        !     (in kilometers).  If alspec=-1, water properties
        !     at 15 Celsius are returned.
        !
        !     Reference:  "u.s. Standard Atmosphere", noaa.
        !---------------------------------------------------------
        logical first
        !
        parameter (n = 44)
        real alt(n), vso(n), rho(n), rmu(n)
        !
        data first / .true. /
        data alt&
                / 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, &
                10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, &
                20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, &
                30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0, &
                40.0, 45.0, 60.0, 75.0 /
        data vso&
                / 340.0, 336.0, 332.0, 329.0, 325.0, 320.0, 316.0, 312.0, 308.0, 304.0, &
                299.0, 295.0, 295.0, 295.0, 295.0, 295.0, 295.0, 295.0, 295.0, 295.0, &
                295.0, 295.8, 296.4, 297.1, 297.8, 298.5, 299.1, 299.8, 300.5, 301.1, &
                301.8, 302.5, 303.1, 305.0, 306.8, 308.7, 310.5, 312.3, 314.0, 316.0, &
                318.0, 355.0, 372.0, 325.0 /
        data rho&
                / 1.226, 1.112, 1.007, 0.909, 0.820, 0.737, 0.660, 0.589, 0.526, 0.467, &
                0.413, 0.364, 0.311, 0.265, 0.227, 0.194, 0.163, 0.141, 0.121, 0.103, &
                .0880, .0749, .0637, .0543, .0463, .0395, .0338, .0288, .0246, .0210, &
                .0180, .0154, .0132, .0113, .0096, .0082, .0070, .0060, .0052, .0044, &
                0.004, 0.002, 3.9e-4, 8.0e-5 /
        data rmu&
                / 1.780, 1.749, 1.717, 1.684, 1.652, 1.619, 1.586, 1.552, 1.517, 1.482, &
                1.447, 1.418, 1.418, 1.418, 1.418, 1.418, 1.418, 1.418, 1.418, 1.418, &
                1.418, 1.427, 1.433, 1.438, 1.444, 1.449, 1.454, 1.460, 1.465, 1.471, &
                1.476, 1.481, 1.487, 1.502, 1.512, 1.532, 1.546, 1.561, 1.580, 1.600, &
                1.700, 1.912, 2.047, 1.667 /
        !
        !---- special case: Water at stp
        if (alspec==-1.0) then
            vsoalt = 1500.
            rhoalt = 1000.
            rmualt = 1.15E-3
            if (show_output) write (*, *)                                       &
                    &'                              o        '
            if (show_output) write (*, *)                                       &
                    &'atmo: You are underwater at 15  Celsius'
            return
        endif
        !
        !---- linearly interpolate quantities from tabulated values
        do i = 2, n
            if (alspec<=alt(i)) then
                !
                dalt = alt(i) - alt(i - 1)
                dvso = vso(i) - vso(i - 1)
                drho = rho(i) - rho(i - 1)
                drmu = rmu(i) - rmu(i - 1)
                !
                alfrac = (alspec - alt(i - 1)) / dalt
                !
                vsoalt = vso(i - 1) + dvso * alfrac
                rhoalt = rho(i - 1) + drho * alfrac
                rmualt = rmu(i - 1) + drmu * alfrac
                rmualt = rmualt * 1.0E-5
                !
                return
            endif
        enddo
        !
        !
        if (alspec>alt(n)) then
            if (show_output) write (*, *) ' '
            if (show_output) write (*, *)                                       &
                    &'atmo: You''re in low earth orbit.  Good luck.'
            vsoalt = vso(n)
            rhoalt = rho(n)
            rmualt = rmu(n) * 1.0E-5
            return
        endif
        !
        !      if(first) then
        !       do i=1, n
        !         rho(i) = alog(rho(i))
        ! end do
        !       call spline(vso,vsoh,alt,n)
        !       call splind(rho,rhoh,alt,n,999.0,0.0)
        !       call spline(rmu,rmuh,alt,n)
        !       first = .false.
        !      endif
        !c
        !c---- interpolate quantities from splines
        !      vsoalt = seval(alspec,vso,vsoh,alt,n)
        !      rhoalt = seval(alspec,rho,rhoh,alt,n)
        !      rmualt = seval(alspec,rmu,rmuh,alt,n) * 1.0e-5
        !      rhoalt = exp(rhoalt)
        !c
    end
    ! atmo


    subroutine flosho(lu, vso, rho, rmu)
        !*** Start of declarations inserted by SPAG
        real GAM, P, R, RHO, RMU, RNU, T, VSO
        integer LU
        !*** End of declarations inserted by SPAG
        data r, gam/287.0, 1.4/
        rnu = rmu / rho
        p = rho * vso**2 / gam
        t = p / (rho * r)
        write (lu, 99001) vso, rho, rmu, rnu, p, t
        99001  format (/' Speed of sound (m/s):', f10.3/' Density   (kg/m^3)  :', &
                &f10.5/' Viscosity (kg/m-s)  :', e11.4/' Kin. Visc. (m^2/s)  :', &
                & e11.4//' Air pressure (Pa)   :', g13.5/' Air temperature (k) :', &
                & g12.4)
    end
    ! flosho


    subroutine reinit(ctxt)
        use m_xoper, only : aper
        use m_userio, only : askl, askr
        use i_common, only : Common, pi
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real A0, ADV0, ANG, RPM, RPM0
        integer I, IS
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        logical yes
        !-----------------------------------------------
        !     Re-initializes advance ratio and gammas
        !-----------------------------------------------
        !
        !---- estimate reasonable advance ratio to start iterative routines
        is = ctxt%ii / 2 + 1
        !---hhy had to set a0 to 0.0 as a0 is now section property
        a0 = 0.0
        ang = ctxt%beta(is) - a0
        !
        rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.)
        !
        adv0 = ctxt%xi(is) * sin(ang) / cos(ang)
        rpm0 = ctxt%vel / (ctxt%rad * adv0 * pi / 30.)

        !      write(*,*) 'Current    rpm ',rpm
        !      write(*,*) 'Initialize rpm ',rpm0
        call askr('Enter initialization rpm?^', rpm)

        ctxt%adv = ctxt%vel / (rpm * ctxt%rad * pi / 30.)
        ctxt%adv = max(0.1, ctxt%adv)
        ctxt%adw = ctxt%adv
        !
        !---- Set the blade angle back to reference angle
        call askl('Restore blade angles to original?^', yes)
        if (yes) then
            do i = 1, ctxt%ii
                ctxt%beta0(i) = ctxt%beta(i)
            enddo
        endif
        !---- calculate current operating point
        call aper(ctxt, 4, 2, .true.)
        if (ctxt%conv) call output(ctxt, ctxt%luwrit)
        !
    end
    ! reinit

    subroutine setx(ctxt)
        use i_common, only : Common, pi
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        integer I
        real TP, XM, XP
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        !
        !-------------------------------------------------------
        !     Fills stretched radial coordinate array x (and xv)
        !-------------------------------------------------------
        !
        ctxt%dt = 0.5 * pi / float(ctxt%ii)
        xm = ctxt%xi0
        ctxt%xv(1) = ctxt%xi0
        do i = 1, ctxt%ii
            ctxt%t(i) = ctxt%dt * (float(i) - 0.5)
            tp = ctxt%dt * float(i)
            !
            if (ctxt%ixspac==2) then
                !------- Usual sine stretching, adjusted for nonzero root radius
                ctxt%xi(i) = sqrt(ctxt%xitip * sin(ctxt%t(i))**2 + (ctxt%xi0 * cos(ctxt%t(i)))**2)
                xp = sqrt(ctxt%xitip * sin(tp)**2 + (ctxt%xi0 * cos(tp))**2)
            else
                !------- Cosine stretching for more root resolution (also in tinvrt)
                ctxt%xi(i) = 0.5 * (1.0 - cos(2.0 * ctxt%t(i))) * (ctxt%xitip - ctxt%xi0) + ctxt%xi0
                xp = 0.5 * (1.0 - cos(2.0 * tp)) * (ctxt%xitip - ctxt%xi0) + ctxt%xi0
            endif
            !
            ctxt%xi(i) = (xp + xm) * 0.5
            ctxt%dxi(i) = xp - xm
            !
            xm = xp
            ctxt%xv(i + 1) = xp
        enddo
        ctxt%xv(ctxt%ii + 1) = ctxt%xitip
        !
    end
    ! setx



    subroutine opfile(lu, fname)
        use m_userio, only : asks, askc
        use i_common, only : show_output
        !*** Start of declarations inserted by SPAG
        integer K, LU, NF
        !*** End of declarations inserted by SPAG
        character*(*) fname
        !
        character*4 comand
        character*128 comarg, tmp
        character*1 ans, dummy
        !
        !---- get filename if it hasn't been already specified
        if (fname(1:1)==' ') call asks('Enter output filename^', fname)
        !
        !---- try to open file
        open (lu, file = fname, status = 'old', err = 100)
        !
        !---- file exists... ask how to proceed
        nf = index(fname, ' ') - 1
        tmp = 'File  ' // fname(1:nf) // &
                '  exists.  Overwrite / Append / New file ?^'
        call askc(tmp, comand, comarg)
        ans = comand(1:1)
        !
        !---- ask again if reply is invalid
        if (index('OoAaNn', ans)==0) then
            call askc(' o / a / n  ?^', comand, comarg)
            ans = comand(1:1)
            !
            if (index('OoAaNn', ans)==0) then
                !------- Still bad reply. Give up asking and just return
                if (show_output) write (*, *) 'No action taken'
                return
            endif
        endif
        !
        !---- at this point, file is open and reply is valid
        if (index('Oo', ans)/=0) then
            !------ go to beginning of file to overwrite
            rewind (lu)
            goto 200
        elseif (index('Aa', ans)/=0) then
            !------ go to end of file to append
            do k = 1, 12345678
                read (lu, 99001, end = 200) dummy
                99001          format (a)
            enddo
        else
            !------ new file... get filename from command argument, or ask if not supplied
            fname = comarg
            if (fname(1:1)==' ') call asks('Enter output filename^', fname)
        endif
        !
        !---- at this point, file fname is new or is to be overwritten
        100   open (lu, file = fname, status = 'unknown', err = 300)
        rewind (lu)
        !
        200   return
        !
        300   if (show_output) write (*, *) 'Bad filename.'
    end
    ! opfile


    subroutine output(ctxt, lu)
        use m_xoper, only : cscalc
        use i_common, only : Common, pi
        use m_spline, only : spline, seval
        use s_xrotor, only : uvadd
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real BDEG, CH34, CI, CI_ADV, CI_VT, CP, CPH, CT, CTH, CTOS, CW, DIA, &
                & EFFI, EFFIND, EFFTOT, EIDEAL, EN, FOM, MACH, PC
        real PDIM, PHI, PVDIM, P_ADV, P_VA, P_VT, QDIM, REEXP, REMAX, RPM, &
                & SI, SIGMA, SI_VA, SW, TC, TCLIM
        real TDIM, TNACEL, TVDIM, UTOT, UTOTW, VA, VAW, VA_ADW, VD, VD_ADW, VT, &
                & VT_ADW, VW, W, WA, WT, W_ADV, W_VA, W_VT, XRE
        integer I, IADD, LU
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        logical lheli
        character*1 schar
        !---------------------------------------------
        !     Dumps operating state output to unit lu
        !---------------------------------------------
        !
        iadd = 1
        if (lu==ctxt%luwrit) iadd = ctxt%incr
        !
        write (lu, 99001)
        !....................................................................
        !
        99001  format (/1x, 75('='))
        if (.not.ctxt%conv) write (lu, 99002)
        99002  format (/19x, '********** not converged **********'/)
        !
        lheli = .false.
        !
        !---- dimensional thrust, power, torque, rpm
        tdim = ctxt%ttot * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
        qdim = ctxt%qtot * ctxt%rho * ctxt%vel**2 * ctxt%rad**3
        pdim = ctxt%ptot * ctxt%rho * ctxt%vel**3 * ctxt%rad**2
        !
        tvdim = ctxt%tvis * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
        pvdim = ctxt%pvis * ctxt%rho * ctxt%vel**3 * ctxt%rad**2
        !
        efftot = ctxt%ttot / ctxt%ptot
        rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.)
        dia = 2.0 * ctxt%rad
        !
        !---- Nacelle (or body) thrust is difference between thrust on
        !     equivalent prop and real prop
        tnacel = (ctxt%twak - ctxt%tinv) * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
        !
        !---- blade solidity
        ctxt%w1(1:ctxt%ii) = spline(ctxt%xi(1:ctxt%ii), ctxt%ch(1:ctxt%ii))
        ch34 = seval(0.75, ctxt%ch, ctxt%w1, ctxt%xi)
        sigma = float(ctxt%nblds) * ch34 / pi
        !
        !---- standard coefficients based on forward speed
        tc = tdim / (0.5 * ctxt%rho * ctxt%vel**2 * pi * ctxt%rad**2)
        pc = pdim / (0.5 * ctxt%rho * ctxt%vel**3 * pi * ctxt%rad**2)
        !
        !---- standard coefficients based on rotational speed
        en = rpm / 60.0
        ct = tdim / (ctxt%rho * en**2 * dia**4)
        cp = pdim / (ctxt%rho * en**3 * dia**5)
        !
        !---- induced efficiency (including nacelle thrust effect)
        effind = ctxt%twak / ctxt%pwak
        !
        !---- ideal (actuator disk) efficiency
        tclim = max(-1.0, tc)
        eideal = 2.0 / (1.0 + sqrt(tclim + 1.0))
        !
        !---- define low advance ratio (helicopter?) related data
        if (ctxt%adv<0.1) then
            ctxt%w1(1:ctxt%ii) = spline(ctxt%xi(1:ctxt%ii), ctxt%ch(1:ctxt%ii))
            cth = ct / 7.7516
            cph = cp / 24.352
            ctos = cth / sigma
            fom = 0.7979 * abs(ct)**1.5 / cp
            lheli = .true.
        endif
        !
        !
        if (ctxt%duct) then
            if (ctxt%iwtyp==1) write (lu, 99019) ctxt%name
            if (ctxt%iwtyp==2) write (lu, 99003) ctxt%name
            99003      format (' Ducted Potential Formulation Solution:  ', a32)
            if (ctxt%iwtyp==3) write (lu, 99019) ctxt%name
        else
            if (ctxt%iwtyp==1) write (lu, 99004) ctxt%name
            99004      format (' Free Tip Graded Mom. Formulation Solution:  ', a32)
            if (ctxt%iwtyp==2) write (lu, 99005) ctxt%name
            99005      format (' Free Tip Potential Formulation Solution:  ', a32)
            if (ctxt%iwtyp==3) write (lu, 99006) ctxt%name
            99006      format (' Free Tip Vortex Wake Formulation Solution:  ', a32)
        endif
        if (ctxt%nadd>1) then
            write (lu, 99007) ctxt%adw
            99007      format (' (External slipstream present)', 19x, 'Wake adv. ratio:', &
                    & f11.5)
        elseif (ctxt%duct) then
            write (lu, 99008) ctxt%urduct, ctxt%adw
            99008      format (' Vdisk/Vslip:', f11.5, 25x, 'Wake adv. ratio:', f11.5)
        else
            write (lu, 99009) ctxt%adw
            99009      format (50x, 'Wake adv. ratio:', f11.5)
        endif
        if (ctxt%adw<0.5 * ctxt%adv) write (lu, 99010)
        99010  format (' Reverse far-slipstream velocity implied.', &
                &' Interpret results carefully !')
        write (lu, 99011) ctxt%nblds, ctxt%rad, ctxt%adv, tdim, pdim, qdim, &
                & efftot, ctxt%vel, rpm, effind, eideal, tc, tnacel, &
                & ctxt%xi0 * ctxt%rad, ctxt%xw0 * ctxt%rad, tvdim, pvdim, &
                & ctxt%rho, ctxt%vso, ctxt%rmu
        99011  format (' no. blades :', i3, 12x, 'radius(m)  :', f9.4, 5x, 'adv. ratio: ', &
                & f11.5, /' thrust(n)  :', g11.3, 4x, 'power(w)   :', g11.3, 3x, &
                &'torque(n-m):', g11.3, /' Efficiency :', f8.4, 7x, 'speed(m/s) :', &
                & f9.3, 5x, 'rpm        :', f11.3, /' Eff induced:', f8.4, 7x, &
                &'Eff ideal  :', f9.4, 5x, 'Tcoef      :', f11.4, /' Tnacel(n)  :', &
                & f11.4, 4x, 'hub rad.(m):', f9.4, 5x, 'disp. rad. :', f10.4, &
                &/' Tvisc(n)   :', f11.4, 4x, 'Pvisc(w)   :', g11.3, /' rho(kg/m3) :', &
                & f10.5, 5x, 'Vsound(m/s):', f9.3, 5x, 'mu(kg/m-s) :', e11.4/1x, 75('-'))
        !
        !---- low advance ratio (helicopter?) data
        if (lheli) then
            write (lu, 99012) sigma, ctos, fom
            99012      format ('Helicopter: ', ' Sigma:', f11.5, '  cTh/s:', f11.5, '  fom:', &
                    & f11.5)
        else
            write (lu, 99013) sigma
            99013      format (' Sigma:', f11.5)
        endif
        !
        !---- coefficients based on rotational speed
        write (lu, 99014) ct, cp, ctxt%adv * pi
        99014  format (12x, '    Ct:', f11.5, '     Cp:', f11.5, '    j:', f11.5)
        !---- coefficients based on forward speed
        write (lu, 99015) tc, pc, ctxt%adv
        99015  format (12x, '    Tc:', f11.5, '     Pc:', f11.5, '  adv:', f11.5)

        !c      write(lu,1017) pvis * adv**3 * 2.0/pi,
        !c     &               pwak * adv**3 * 2.0/pi

        !
        if (ctxt%terse) return
        !
        !----- find maximum re on blade
        remax = 0.0
        do i = 1, ctxt%ii
            remax = max(ctxt%re(i), remax)
        enddo
        reexp = 1.0
        if (remax>=1.0E6) then
            reexp = 6.0
        elseif (remax>=1.0E3) then
            reexp = 3.0
        endif
        !
        if (reexp==1.0) then
            write (lu, 99016)
            99016      format (/'  i  r/r    c/r  beta(deg)', &
                    &'   cl      Cd    re    Mach   effi  effp  na.u/u')
        else
            write (lu, 99017) ifix(reexp)
            99017      format (/'  i  r/r   c/r  beta(deg)', '  cl     Cd    rEx10^', i1, &
                    &' Mach   effi  effp  na.u/u')
        endif
        !
        do i = 1, ctxt%ii, iadd
            !
            !------ use equivalent prop to define local efficiency
            call uvadd(ctxt, ctxt%xi(i), wa, wt)
            vw = ctxt%vwak(i)
            vaw = vw * ctxt%xw(i) / ctxt%adw
            !------ Freestream velocity component on equiv prop
            utotw = ctxt%urduct
            cw = ctxt%xi(i) / ctxt%adv - wt - vw
            sw = utotw + wa + vaw
            effi = (cw / sw) * ctxt%adv / ctxt%xw(i)
            !
            !------ use real prop to define Mach number
            call cscalc(ctxt, i, utot, wa, wt, &
                    vt, vt_adw, &
                    va, va_adw, &
                    vd, vd_adw, &
                    ci, ci_adv, ci_vt, &
                    si, si_va, &
                    w, w_adv, w_vt, w_va, &
                    phi, p_adv, p_vt, p_va)
            !
            mach = w * ctxt%vel / ctxt%vso
            !
            bdeg = ctxt%beta(i) * 180. / pi
            xre = ctxt%re(i) / (10.0**reexp)
            !
            schar = ' '
            if (ctxt%stall(i)) schar = 's'
            !
            write (lu, 99018) i, ctxt%xi(i), ctxt%ch(i), bdeg, ctxt%cl(i), schar, &
                    & ctxt%cd(i), xre, mach, effi, ctxt%effp(i), &
                    & ctxt%ubody(i)
            99018      format (1x, i2, f6.3, f7.4, f7.2, f7.3, 1x, a1, f7.4, 1x, f6.2, 1x, f6.3, 1x, f6.3, &
                    & f6.3, f8.3, f10.6)
            !c     &    ,rad*ch(i)*sin(beta(i))*39.36
        enddo
        !c      write(lu,1000)
        !c      write(lu,*   ) ' '
        !
        return
        99019  format (' Ducted Graded Mom. Formulation Solution:  ', a32)
        99020  format (' Cpv:', f11.5, '    Cpi:', f11.5)
    end
    ! output


end
