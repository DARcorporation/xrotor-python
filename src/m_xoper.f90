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

module m_xoper
contains


    subroutine getpvar(lu, ndim, n, xrpm, xpwr)
        use m_common, only : show_output
        dimension xpwr(ndim), xrpm(ndim)
        character*1 dummy
        !
        1000 format(a)
        read(lu, 1000) dummy
        !
        do i = 1, 12345
            read(lu, *, end = 11, err = 99) xx, yy
            xrpm(i) = xx
            xpwr(i) = yy
        enddo
        11   continue
        n = i - 1
        return
        !
        99    if (show_output) write(*, *) 'File read error'
        n = 0
        return
    end


    subroutine shocas(lu, ndim, n, par, rad, name)
        use m_common, only : show_output, pi
        dimension par(0:ndim, *)
        character name*(*)
        !
        if(ndim < 11) then
            if (show_output) write(*, *) 'Error in shocas: ndim too small for par array'
            return
        endif
        !
        !pi = 4.0 * atan(1.0)
        !
        write(lu, 900) name
        write(lu, 1000)
        do i = 1, n
            adv = par(1, i)
            vel = par(2, i)
            bet = par(3, i) * 180.0 / pi
            alt = par(4, i)
            rho = par(5, i)
            rmu = par(6, i) * 1.0e5
            vso = par(7, i)
            convflg = par(8, i)
            pow = par(8, i) * 0.001
            thr = par(9, i)
            trq = par(10, i)
            eff = par(11, i)
            rpm = vel / (rad * adv) * 30.0 / pi
            if(convflg == 999.0) then
                write(lu, 1200) i, adv, bet, vel, rpm, rho, rmu, vso, alt
            else
                write(lu, 1200) i, adv, bet, vel, rpm, rho, rmu, vso, alt, &
                        pow, thr, trq, eff
            endif
        enddo
        return
        !
        900 format(a)
        !
        !        1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7
        !23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
        !iixggggggggggggxggggggggggggxggggggggggggxggggggggggggxggggggggggggxggggggggggggxggggggggggggxggggggggggggxggggggggggggxggggggggggggxggggggggggggxggggggggggggxgggggggggggg
        !  n         v/wr         Btip            v          rpm          rho       mu*1e5       Vsound            h        p(kw)         t(n)       q(n-m)          eff',
        !
        1000 format(&
                '  n'&
                '         v/wr         Btip            v          rpm'&
                '          rho       mu*1e5       Vsound            h'&
                '        p(kw)         t(n)       q(n-m)          eff'&
                /160('-'))
        1200 format(i3, 12(1x, g12.5))
        !
        ! 1000 format( '  n   v/wr   Btip      v       rpm ',
        !     &        '    rho   mu*1e5   Vsound    h  ',
        !     &        '       p(kw)        t(n)         q(n-m)      eff',
        !     &       /' --  ------  -----  ------- ------- ',
        !     &        ' -----  -------  -------  ------ ',
        !     &        '  -----------  -----------  -----------  -----')
        ! 1200 format(i3,1x,f7.3,1x,f6.2,1x,f7.2,1x,f8.1,1x,f6.3,1x,f8.3,
        !     &       1x,f8.2,1x,f7.2,2x,g12.5,1x,g12.5,1x,g12.5,1x,f6.3)
        !
        !--- new format (old one ran out of sig. digits, lets remove the 80 column limit!
        !  n   v/wr   Btip     v       rpm     rho    mu*1e5   Vsound    h        p(kw)        t(n)         q(n-m)      eff
        ! --  ------  -----  ------  -------  -----  -------  -------  -----   -----------  -----------  -----------  -----
        ! 12 x0.2345 x14.25 x123.23 x10000.0 x1.225 x123.123 x1234.12 x123.12 x123456.1234 x123456.1234 x123456.1234 x0.111
        !iii fffffff ffffff fffffff ffffffff ffffff ffffffff ffffffff fffffff gggggggggggg gggggggggggg gggggggggggg ffffff
        !
        !
        !---------------------------------------------------------------------------
        ! 1000 format(/'  n   v/wr   Btip     v     rpm',
        !     &        '      rho     p(kw)    t(n)     q(n-m)   eff',
        !     &       /' --  -----  -----  ------  -------',
        !     &        '  -----  --------  -------  -------  -----')
        ! 1200 format(i3,1x,f6.3,1x,f6.2,1x,f6.2,1x,f8.1,1x,f6.3,
        !     &       1x,f9.4,1x,f8.1,1x,f8.1,1x,f6.3)
        !
        !  n   v/wr   Btip     v     rpm      rho     p(kw)     t(n)     q(n-m)   eff
        ! --  -----  -----  ------  -------  -----  --------  -------  -------  -----
        ! 12 x0.234 x14.25 x123.23 x10000.0 x1.225 x123.1234 x11000.0 x11125.0 x0.111
        !iii ffffff ffffff fffffff ffffffff ffffff fffffffff ffffffff ffffffff ffffff
        !---------------------------------------------------------------------------
        !
        !---------------------------------------------------------------------------
        ! 1000 format(
        !     & /'  n    v/wr    Btip     v      rpm  ',
        !     &  '    rho     mu*1e5  Vsound     h  '
        !     & /'  --   ------  -----  ------  -------',
        !     &  '  -------  -------  ------   -----')
        !c          12   0.2345  14.25  123.23  10000.0
        !c             1.225  115.000  1000.0   25.00
        ! 1200 format(
        !     &   i3, f9.4,    f7.2,   f8.2,     f9.1,
        !     &       f9.3,    f9.3,     f8.1,  f8.2  )
        !---------------------------------------------------------------------------
        !
        !---------------------------------------------------------------------------
        ! 1000 format(
        !     & /'  n    v/wr    Btip     v     rpm  ',
        !     &  '  rho     p(w)     t(n)     q(n-m)',
        !     & /'  --   ------  -----  ------  ------',
        !     &  '  -----  -------  ------   ------')
        !
        ! 1200 format(
        !     &   i3, f9.4,    f7.2,   f8.2,     f9.1,
        !     &       f7.4,  f10.2,   f9.3,  f9.3  )
        !---------------------------------------------------------------------------
        !
    end


    subroutine getcas(lu, ndim, ncas, par)
        use m_userio, only : getflt
        use m_common, only : show_output, pi
        dimension par(0:ndim, *), a(16)
        character dummy*1, line*128, cname*32
        logical error
        !
        if(ndim < 11) then
            if (show_output) write(*, *) 'Error in getcas: ndim too small for par array'
            return
        endif
        !
        !pi = 4.0 * atan(1.0)
        !
        1000 format(a)
        read(lu, 1000) cname
        !c      write(*,*) 'Case name: ',cname
        read(lu, 1000) dummy
        read(lu, 1000) dummy
        !
        do i = 1, 12345
            !cc        read(line,err=99) idum,adv,bet,vel,rpm,rho,rmu,vso,alt,
            !cc     &                    pow,thr,trq,eff
            read(lu, 1000, end = 11) line
            n = 13
            call getflt(line, a, n, error)
            if(error) go to 99
            adv = a(2)
            bet = a(3) * pi / 180.0
            vel = a(4)
            rho = a(6)
            rmu = a(7) / 1.0e5
            vso = a(8)
            alt = a(9)
            pow = 999.0
            thr = 999.0
            trq = 999.0
            eff = 999.0
            if(n == 13) then
                pow = a(10) * 1000.0
                thr = a(11)
                trq = a(12)
                eff = a(13)
            endif
            !--- Set parameters for cases
            par(1, i) = adv
            par(2, i) = vel
            par(3, i) = bet
            par(4, i) = alt
            par(5, i) = rho
            par(6, i) = rmu
            par(7, i) = vso
            par(8, i) = pow
            par(9, i) = thr
            par(10, i) = trq
            par(11, i) = eff
        enddo
        11   continue
        ncas = i - 1
        return
        !
        99    if (show_output) write(*, *) 'File read error'
        ncas = 0
        return
    end


    subroutine setcas(ctxt, itype, ninput, rinput)
        use m_userio, only : askr, asks, askl, lc2uc
        use m_common, only : Common, show_output, icasx, pi
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        dimension rinput(*)
        !---------------------------------------------------
        !     Sets operating parameters over a range
        !     of parameters of type itype where
        !       itype    Parameter for range
        !         1      Advance ratio
        !         2      rpm
        !         3      Blade angle
        !         4      Velocity with fixed pitch
        !xxxx     5      Velocity with fixed rpm
        !---------------------------------------------------
        character*1 ans, ans4*4
        logical yes
        !
        !      write(*,*)
        !      write(*,*) 'Overwrite or Append  to case accumulator?  o'
        !      read (*,1000) ans
        ! 1000 format(a)
        !      if(index('Aa',ans) == 0) ncase = 0
        !
        !
        if(ctxt%ncase > 0) then
            if (show_output) write(*, *)
            if (show_output) write(*, *) 'Appending to current case accumulator...'
        endif
        !
        ctxt%kcase = 0
        !
        !---------------------------------------------------------------------
        !--- Sequence of advance ratio
        if(itype == 1) then
            ctxt%kcase = 1
            !
            if    (ninput >= 3) then
                adv1 = rinput(1)
                adv2 = rinput(2)
                ctxt%dadv = rinput(3)
            elseif(ninput >= 2) then
                adv1 = rinput(1)
                adv2 = rinput(2)
                ctxt%dadv = 999.
                call askr('enter advance ratio increment  ^', ctxt%dadv)
            elseif(ninput >= 1) then
                adv1 = rinput(1)
                adv2 = 999.
                call askr('Enter last  advance ratio value^', adv2)
                ctxt%dadv = 999.
                call askr('enter advance ratio increment  ^', ctxt%dadv)
            else
                adv1 = 999.
                call askr('Enter first advance ratio value^', adv1)
                adv2 = 999.
                call askr('Enter last  advance ratio value^', adv2)
                ctxt%dadv = 999.
                call askr('enter advance ratio increment  ^', ctxt%dadv)
            endif
            if(adv1 == adv2) return
            ctxt%dadv = sign(ctxt%dadv, adv2 - adv1)
            np = 1
            if(ctxt%dadv /= 0.0) np = int((adv2 - adv1) / ctxt%dadv + 0.5) + 1
            if(np <= 0) return
            !
            !--- Check for use of rpm/power relationship to set power
            yes = .false.
            xans = 0.
            if(ctxt%lpwrvar) call askl('use engine rpm/power line ?^', yes)
            if(yes) xans = 100.0
            !
            if(ctxt%ncase + np > icasx) then
                if (show_output) write(*, *) 'Limiting number of cases to array limit:', icasx
                np = icasx - ctxt%ncase
            endif
            !
            do ip = 1, np
                ctxt%ncase = ctxt%ncase + 1
                ctxt%caspar(0, ctxt%ncase) = xans + float(ctxt%kcase)
                ctxt%caspar(1, ctxt%ncase) = adv1 + ctxt%dadv * float(ip - 1)
                ctxt%caspar(2, ctxt%ncase) = ctxt%vel
                ctxt%caspar(3, ctxt%ncase) = ctxt%beta(ctxt%ii)
                ctxt%caspar(4, ctxt%ncase) = ctxt%alt
                ctxt%caspar(5, ctxt%ncase) = ctxt%rho
                ctxt%caspar(6, ctxt%ncase) = ctxt%rmu
                ctxt%caspar(7, ctxt%ncase) = ctxt%vso
                ctxt%caspar(8, ctxt%ncase) = 999.
                ctxt%caspar(9, ctxt%ncase) = 999.
                ctxt%caspar(10, ctxt%ncase) = 999.
                ctxt%caspar(11, ctxt%ncase) = 999.
            enddo
            !
            !---------------------------------------------------------------------
            !--- Sequence of rpm
        elseif(itype == 2) then
            ctxt%kcase = 2
            !
            if    (ninput >= 3) then
                rpm1 = rinput(1)
                rpm2 = rinput(2)
                drpm = rinput(3)
            elseif(ninput >= 2) then
                rpm1 = rinput(1)
                rpm2 = rinput(2)
                drpm = 999.
                call askr('Enter rpm increment  ^', drpm)
            elseif(ninput >= 1) then
                rpm1 = rinput(1)
                rpm2 = 999.
                call askr('Enter last  rpm value^', rpm2)
                drpm = 999.
                call askr('Enter rpm increment  ^', drpm)
            else
                rpm1 = 999.
                call askr('Enter first rpm value^', rpm1)
                rpm2 = 999.
                call askr('Enter last  rpm value^', rpm2)
                drpm = 999.
                call askr('Enter rpm increment  ^', drpm)
            endif
            if(rpm1 == rpm2) return
            drpm = sign(drpm, rpm2 - rpm1)
            np = 1
            if(drpm /= 0.0) np = int((rpm2 - rpm1) / drpm + 0.5) + 1
            if(np <= 0) return
            !
            !--- Check for use of rpm/power relationship to set power
            yes = .false.
            xans = 0.
            if(ctxt%lpwrvar) call askl('use engine rpm/power line ?^', yes)
            if(yes) xans = 100.0
            !
            ans = ' '
            call asks('fix power p or thrust ctxt%t or blade pitch a ?^', ans)
            call lc2uc(ans)
            if(ans == 't') xans = xans + 1000.0
            if(ans == 'q') xans = xans + 2000.0
            if(ans == 'p') xans = xans + 3000.0
            !
            if(ctxt%ncase + np > icasx) then
                if (show_output) write(*, *) 'Limiting number of cases to array limit:', icasx
                np = icasx - ctxt%ncase
            endif
            !
            do ip = 1, np
                ctxt%ncase = ctxt%ncase + 1
                rpm = rpm1 + drpm * float(ip - 1)
                ctxt%caspar(0, ctxt%ncase) = xans + float(ctxt%kcase)
                ctxt%caspar(1, ctxt%ncase) = ctxt%vel / (rpm * ctxt%rad) * 30.0 / pi
                ctxt%caspar(2, ctxt%ncase) = ctxt%vel
                ctxt%caspar(3, ctxt%ncase) = ctxt%beta(ctxt%ii)
                ctxt%caspar(4, ctxt%ncase) = ctxt%alt
                ctxt%caspar(5, ctxt%ncase) = ctxt%rho
                ctxt%caspar(6, ctxt%ncase) = ctxt%rmu
                ctxt%caspar(7, ctxt%ncase) = ctxt%vso
                ctxt%caspar(8, ctxt%ncase) = 999.
                ctxt%caspar(9, ctxt%ncase) = 999.
                ctxt%caspar(10, ctxt%ncase) = 999.
                ctxt%caspar(11, ctxt%ncase) = 999.
            enddo
            !
            !---------------------------------------------------------------------
            !--- Sequence of blade angle
        elseif(itype == 3) then
            ctxt%kcase = 3
            !
            if    (ninput >= 3) then
                bet1 = rinput(1)
                bet2 = rinput(2)
                ctxt%dbet = rinput(3)
            elseif(ninput >= 2) then
                bet1 = rinput(1)
                bet2 = rinput(2)
                ctxt%dbet = 999.
                call askr('enter tip angle increment   (deg) ^', ctxt%dbet)
            elseif(ninput >= 1) then
                bet1 = rinput(1)
                bet2 = 999.
                call askr('Enter last  tip angle value (deg) ^', bet2)
                ctxt%dbet = 999.
                call askr('enter tip angle increment   (deg) ^', ctxt%dbet)
            else
                bet1 = 999.
                call askr('Enter first tip angle value (deg) ^', bet1)
                bet2 = 999.
                call askr('Enter last  tip angle value (deg) ^', bet2)
                ctxt%dbet = 999.
                call askr('enter tip angle increment   (deg) ^', ctxt%dbet)
            endif
            if(bet1 == bet2) return
            ctxt%dbet = sign(ctxt%dbet, bet2 - bet1)
            np = 1
            if(ctxt%dbet /= 0.0) np = int((bet2 - bet1) / ctxt%dbet + 0.5) + 1
            if(np <= 0) return
            !
            !--- Check for use of rpm/power relationship to set power
            yes = .false.
            xans = 0.
            if(ctxt%lpwrvar) call askl('use engine rpm/power line ?^', yes)
            if(yes) xans = 100.0
            !
            if(ctxt%ncase + np > icasx) then
                if (show_output) write(*, *) 'Limiting number of cases to array limit:', icasx
                np = icasx - ctxt%ncase
            endif
            !
            do ip = 1, np
                ctxt%ncase = ctxt%ncase + 1
                bet = bet1 + ctxt%dbet * float(ip - 1)
                ctxt%caspar(0, ctxt%ncase) = xans + float(ctxt%kcase)
                ctxt%caspar(1, ctxt%ncase) = ctxt%adv
                ctxt%caspar(2, ctxt%ncase) = ctxt%vel
                ctxt%caspar(3, ctxt%ncase) = bet * pi / 180.0
                ctxt%caspar(4, ctxt%ncase) = ctxt%alt
                ctxt%caspar(5, ctxt%ncase) = ctxt%rho
                ctxt%caspar(6, ctxt%ncase) = ctxt%rmu
                ctxt%caspar(7, ctxt%ncase) = ctxt%vso
                ctxt%caspar(8, ctxt%ncase) = 999.
                ctxt%caspar(9, ctxt%ncase) = 999.
                ctxt%caspar(10, ctxt%ncase) = 999.
                ctxt%caspar(11, ctxt%ncase) = 999.
            enddo
            !
            !---------------------------------------------------------------------
            !--- Sequence of velocities
        elseif(itype == 4) then
            ctxt%kcase = 4
            !
            if    (ninput >= 3) then
                vel1 = rinput(1)
                vel2 = rinput(2)
                dvel = rinput(3)
            elseif(ninput >= 2) then
                vel1 = rinput(1)
                vel2 = rinput(2)
                dvel = 999.
                call askr('Enter speed increment   (m/s) ^', dvel)
            elseif(ninput >= 1) then
                vel1 = rinput(1)
                vel2 = 999.
                call askr('Enter last  speed value (m/s) ^', vel2)
                dvel = 999.
                call askr('Enter speed increment   (m/s) ^', dvel)
            else
                vel1 = 999.
                call askr('Enter first speed value (m/s) ^', vel1)
                vel2 = 999.
                call askr('Enter last  speed value (m/s) ^', vel2)
                dvel = 999.
                call askr('Enter speed increment   (m/s) ^', dvel)
            endif
            if(vel1 == vel2) return
            dvel = sign(dvel, vel2 - vel1)
            np = 1
            if(dvel /= 0.0) np = int((vel2 - vel1) / dvel + 0.5) + 1
            if(np <= 0) return
            !
            !--- Check for use of rpm/power relationship to set power
            yes = .false.
            xans = 0.
            if(ctxt%lpwrvar) call askl('use engine rpm/power line ?^', yes)
            if(yes) xans = 100.0
            !
            !--- What do we hold constant, pitch or rpm?
            20     ans4 = 'cs'
            call asks('fp fixed-pitch or cs constant-speed^', ans4)
            call lc2uc(ans4)
            if(ans4 /= 'cs' .and. ans4 /= 'fp') go to 20
            if(ans4 == 'cs') then
                rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.)
                call askr('Enter constant rpm value^', rpm)
                ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.)
                ctxt%kcase = 5
                if(xans /= 100.0) then
                    if(ctxt%pspec <= 0.0 .and. ctxt%ptot > 0.0)&
                            ctxt%pspec = ctxt%ptot * (ctxt%rho * ctxt%vel**3 * ctxt%rad**2)
                    call askr('enter constant power value^', ctxt%pspec)
                endif
            endif
            !
            if(ctxt%ncase + np > icasx) then
                if (show_output) write(*, *) 'Limiting number of cases to array limit:', icasx
                np = icasx - ctxt%ncase
            endif
            !
            do ip = 1, np
                ctxt%ncase = ctxt%ncase + 1
                vvel = vel1 + dvel * float(ip - 1)
                ctxt%caspar(0, ctxt%ncase) = xans + float(ctxt%kcase)
                ctxt%caspar(1, ctxt%ncase) = vvel * ctxt%adv / ctxt%vel
                ctxt%caspar(2, ctxt%ncase) = vvel
                ctxt%caspar(3, ctxt%ncase) = ctxt%beta(ctxt%ii)
                ctxt%caspar(4, ctxt%ncase) = ctxt%alt
                ctxt%caspar(5, ctxt%ncase) = ctxt%rho
                ctxt%caspar(6, ctxt%ncase) = ctxt%rmu
                ctxt%caspar(7, ctxt%ncase) = ctxt%vso
                ctxt%caspar(8, ctxt%ncase) = 999.
                ctxt%caspar(9, ctxt%ncase) = 999.
                ctxt%caspar(10, ctxt%ncase) = 999.
                ctxt%caspar(11, ctxt%ncase) = 999.
            enddo
            !
        endif
        !
        return
    end
    ! setcas



    subroutine aper(ctxt, ispec, icon, linit)
        use m_common, only : Common, show_output
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        logical linit
        !-------------------------------------------
        !     Sets reasonable initial circulation.
        !     Converges arbitrary operating point.
        !
        !     ispec controls the quantity used as a target quantity
        !       ispec = 1   Drive thrust to tspec
        !       ispec = 2   Drive torque to qspec
        !       ispec = 3   Drive power  to pspec
        !       ispec = 4   Fix advance ratio to current value
        !       ispec = 5   Drive to power specified by rpm (engine power-rpm line)
        !     icon controls the constrained quantity
        !       icon = 1    Advance ratio(rpm) fixed
        !       icon = 2   Blade pitch fixed
        !     linit is flag for initialization of rotor condition
        !-------------------------------------------
        !
        !--- Initialize circulations if requested
        if(linit) then
            !cc        write(*,*) 'apinit called...'
            call apinit(ctxt)
        endif
        !cc      call plot_data(name)
        !
        !cc      write(*,*) 'Before apiter adv,adw ',adv, adw
        call apiter(ctxt, ispec, icon)
        !
        if(.not.ctxt%conv) then
            if (show_output) write(*, *)
            if (show_output) write(*, *) 'Iteration limit exceeded'
            if (show_output) write(*, *) 'gres fres ares =', ctxt%gresmx, ctxt%fresmx, ctxt%aresmx
        endif
        !
        return
    end
    ! aper


    subroutine apinit(ctxt)
        use s_xrotor, only : uvadd
        use m_xaero, only : getclcdcm
        use m_common, only : Common, show_output, ix, pi
        implicit real (m)
        !---------------------------------------------------------
        !     Sets reasonable initial circulation.
        !     Initial circulations are set w/o induced effects
        !     An iteration is done using the self-induced velocity
        !     from graded momentum theory to converge an approximate
        !     wake advance ratio
        !----------------------------------------------------------
        !
        data niterg / 10 /
        type(Common), intent(inout) :: ctxt
        !
        blds = float(ctxt%nblds)
        ctxt%dbeta = 0.0
        !
        uduct = 0.0
        vaduct_va = 1.0
        if(ctxt%duct) then
            uduct = ctxt%urduct - 1.0
            vaduct_va = 2.0 * ctxt%urduct
        endif
        ctxt%adw = ctxt%adv * (1.0 + uduct)
        !
        !======================================================================
        !---- Initialize section circulation neglecting induced velocity
        tsum = 0.
        do i = 1, ctxt%ii
            utot = ctxt%urduct + ctxt%ubody(i)
            call uvadd(ctxt, ctxt%xi(i), wa, wt)
            !
            si = utot + wa
            ci = ctxt%xi(i) / ctxt%adv - wt
            !
            wsq = ci * ci + si * si
            w = sqrt(wsq)
            phi = atan2(si, ci)
            !
            alfa = ctxt%beta(i) - phi
            rey = ctxt%ch(i) * abs(w) * ctxt%rho * ctxt%vel * ctxt%rad / ctxt%rmu
            call getclcdcm(ctxt, i, alfa, w, rey, &
                    ctxt%cl(i), cl_al, cl_w, &
                    clmax, clmin, dclstall, ctxt%stall(i), &
                    ctxt%cd(i), cd_alf, cd_w, cd_rey, &
                    ctxt%cm(i), cm_al, cm_w)
            !
            ctxt%gam(i) = 0.5 * ctxt%cl(i) * w * ctxt%ch(i)
            tsum = tsum + blds * ctxt%gam(i) * ci * ctxt%dxi(i)
            !c        write(8,997) 'i,alfa,cl,gam,tsum ',i,alfa,cl(i),gam(i),tsum
        enddo
        997  format(a, ' ', i4, 5(1x, f10.5))
        !
        !---- use momentum theory estimate of axial velocity to set wake adv. ratio
        vhsq = 0.5 * tsum / pi
        vhsq = max(vhsq, -0.25)
        ctxt%adw = ctxt%adv * 0.5 * (1.0 + sqrt(1.0 + 4.0 * vhsq))
        !
        !cc      write(*,*) 'apinit noVind tsum,adw ',tsum,adw
        !cc      call plot_data(name)
        !
        !---- recalculate Vtan using new gam values
        call vcalc(ctxt)
        !c    go to 101
        !
        !======================================================================
        !---- Refine the initial guess with a graded-momentum theory estimate
        !     Use momentum theory to estimate axial induced velocity to drive
        !     equation for wake advance ratio
        !
        do iterg = 1, niterg
            !
            call gradmo(ix, ctxt%ii, ctxt%nblds, ctxt%duct, ctxt%rake, &
                    ctxt%xi, ctxt%xv, ctxt%gam, ctxt%adw, ctxt%vind_gam, ctxt%vind_adw)
            !
            tsum = 0.
            t_adw = 0.
            !
            dclmax = 0.
            rlxmin = 1.0
            !
            do i = 1, ctxt%ii
                !
                !--- Redefine vt and va to diagonal self-influences
                vt = ctxt%vind_gam(3, i, i) * ctxt%gam(i)
                vt_gam = ctxt%vind_gam(3, i, i)
                vt_adw = ctxt%vind_adw(3, i)
                !
                va = ctxt%vind_gam(1, i, i) * ctxt%gam(i)
                va_gam = ctxt%vind_gam(1, i, i)
                va_adw = ctxt%vind_adw(1, i)
                !
                !------ include duct effect on freestream and induced axial velocity
                uduct = 0.0
                vaduct_va = 1.0
                if(ctxt%duct) then
                    uduct = ctxt%urduct - 1.0
                    vaduct_va = 2.0 * ctxt%urduct
                endif
                !
                utot = 1.0 + uduct + ctxt%ubody(i)
                call uvadd(ctxt, ctxt%xi(i), wa, wt)
                !
                ci = ctxt%xi(i) / ctxt%adv - wt - vt
                ci_adv = -ctxt%xi(i) / ctxt%adv**2
                ci_vt = -  1.0
                !
                si = utot + wa + va * vaduct_va
                si_va = vaduct_va
                !cc        si     = utot + wa  +  va
                !cc        si_va  =               1.0
                !
                wsq = ci * ci + si * si
                w = sqrt(wsq)
                w_adv = (ci * ci_adv) / w
                w_vt = (ci * ci_vt) / w
                w_va = (si * si_va) / w
                !
                phi = atan2(si, ci)
                p_adv = (- si * ci_adv) / wsq
                p_vt = (- si * ci_vt) / wsq
                p_va = (ci * si_va) / wsq
                !
                alfa = ctxt%beta(i) - phi
                al_vt = - p_vt
                al_va = - p_va
                !
                rey = ctxt%ch(i) * abs(w) * ctxt%rho * ctxt%vel * ctxt%rad / ctxt%rmu
                call getclcdcm(ctxt, i, alfa, w, rey, &
                        ctxt%cl(i), cl_al, cl_w, &
                        clmax, clmin, dclstall, ctxt%stall(i), &
                        ctxt%cd(i), cd_alf, cd_w, cd_rey, &
                        ctxt%cm(i), cm_al, cm_w)
                !cc          write(*,*) 'iterg,i,cl ',iterg,i,cl(i)
                !
                !-------- Res( cl( al w ) , w , gam )
                rez = ctxt%ch(i) * ctxt%cl(i) * w - 2.0 * ctxt%gam(i)
                z_cl = ctxt%ch(i) * w
                z_w = ctxt%ch(i) * ctxt%cl(i)
                z_g = - 2.0
                !
                !-------- Res( al( vt adw ) , w( vt adw ) , gam )
                z_al = z_cl * cl_al
                z_w = z_cl * cl_w + z_w
                !
                !-------- Res( vt(gam adw) , adw , gam )
                z_vt = z_w * w_vt + z_al * al_vt
                z_va = z_w * w_va + z_al * al_va
                !
                !-------- Res( adw , gam )
                z_adw = z_vt * vt_adw + z_va * va_adw
                z_g = z_vt * vt_gam + z_va * va_gam + z_g
                !
                delg = -rez / z_g
                dcl = 2.0 * delg / (ctxt%ch(i) * w)
                !
                !---- Apply limiter to gam update based on cl change
                ctxt%rlx = 1.0
                if(ctxt%rlx * abs(dcl) > 0.2) then
                    if(dcl /= 0.0) then
                        ctxt%rlx = min(ctxt%rlx, 0.2 / abs(dcl))
                        !cc        write(*,998) 'aper cl limiter i,rlx,dcl,cl',i,rlx,dcl,cl(i)
                    endif

                endif
                998    format(a, 2x, i5, 3(2x, f12.5))
                !
                if(abs(dcl) > abs(dclmax)) dclmax = dcl
                if(abs(ctxt%rlx) < rlxmin)      rlxmin = ctxt%rlx
                !
                ctxt%gam(i) = ctxt%gam(i) + ctxt%rlx * delg
                !-------- drez = z_g*dg + z_adw*dadw = 0
                g_adw = -z_adw / z_g
                !
                !cc Forces for raked blade corrected for cos of rake angle
                !          cosr = cos(rake)
                cosr = 1.0
                !
                tsum = tsum + blds * ctxt%gam(i) * ci * ctxt%dxi(i) * cosr
                t_g = blds * ci * ctxt%dxi(i) * cosr
                t_vt = blds * ctxt%gam(i) * ci_vt * ctxt%dxi(i) * cosr
                t_adw = t_adw + (t_g + t_vt * vt_gam) * g_adw&
                        + t_vt * vt_adw
            end do
            !
            !---- Momentum theory estimate of induced axial velocity
            vhsq = 0.5 * tsum / pi
            vhsq = max(vhsq, -0.2499)
            vhsq_t = 0.5 / pi
            !
            rez = ctxt%adw - ctxt%adv * 0.5 * (1.0 + sqrt(1.0 + 4.0 * vhsq))
            z_adw = 1.0 - ctxt%adv / sqrt(1.0 + 4.0 * vhsq) * vhsq_t * t_adw
            !c      z_adw = 1.0
            if (z_adw == 0.0 .and. show_output) write(*, *) 'apinit z_adw ', z_adw
            !
            ctxt%dadw = -rez / z_adw
            ctxt%dadw = min(ctxt%dadw, 10.0 * ctxt%adw)
            ctxt%dadw = max(ctxt%dadw, -0.9 * ctxt%adw)
            ctxt%adw = ctxt%adw + ctxt%dadw
            !
            if(rlxmin < 0.2) then
                !cc          write(*,*) 'apinit filtering gam'
                call filter(ctxt%gam, 0.2 * ctxt%ii, ctxt%ii)
            endif
            !cc        write(*,*) 'apinit Vind iter,tsum,adw ',iterg,tsum,adw
            !cc        write(*,*) 'apinit adw,dadw,dclmax ',adw,dadw,dclmax
            !
            if(abs(dclmax) < 0.001) go to 101
            !
        end do
        !cc      write(*,*) 'apinit No convergence'
        !
        101  return
    end


    subroutine apiter(ctxt, ispec, icon)
        use m_xutils, only : gauss
        use m_vortex, only : vrtxco
        use m_xaero, only : getclcdcm
        !-------------------------------------------------------
        !     Converges arbitrary performance operating point
        !
        !     ispec controls the quantity used as a target quantity

        !       ispec = 1   Drive thrust to tspec
        !       ispec = 2   Drive torque to qspec
        !       ispec = 3   Drive power  to pspec
        !       ispec = 4   Fix advance ratio to current value
        !       ispec = 5   Drive to power specified by rpm (engine power-rpm line)
        !
        !     icon controls the constrained quantity
        !       icon = 1    Advance ratio(rpm) fixed
        !       icon = 2    Blade pitch fixed
        !-------------------------------------------------------
        use m_common, only : Common, ix, show_output, pi, iq
        use m_spline, only : seval, deval

        implicit real (m)
        dimension clmax(ix), clmin(ix), dclstall(ix)
        !
        !---- convergence tolerance
        data eps / 1.0e-07 /
        type(Common), intent(inout) :: ctxt
        !
        k1 = ctxt%ii + 1
        k2 = ctxt%ii + 2
        k3 = ctxt%ii + 3
        if (show_output) write(*, 2000)
        !
        do iter = 1, max(ctxt%nitera, 1)
            !
            !---- if wake advance ratio changed, recalculate Vtan influence coefficients
            if(ctxt%free .or. iter == 1) then
                if(ctxt%fast) then
                    call gradmo(ix, ctxt%ii, ctxt%nblds, ctxt%duct, ctxt%rake, &
                            ctxt%xi, ctxt%xv, ctxt%gam, ctxt%adw, ctxt%vind_gam, ctxt%vind_adw)
                    ctxt%iwtyp = 1
                elseif(.not.ctxt%vrtx) then
                    call helico(ix, ctxt%ii, ctxt%nblds, ctxt%duct, ctxt%rake, &
                            ctxt%xi, ctxt%xv, ctxt%gam, ctxt%adw, ctxt%vind_gam, ctxt%vind_adw)
                    ctxt%iwtyp = 2
                elseif(ctxt%vrtx) then
                    call vrtxco(ix, ctxt%ii, ctxt%nblds, ctxt%duct, ctxt%rake, &
                            ctxt%xi, ctxt%xv, ctxt%gam, ctxt%adw, ctxt%vind_gam, ctxt%vind_adw)
                    ctxt%iwtyp = 3
                endif
            endif
            !
            !---- recalculate Vtan
            call vcalc(ctxt)
            !
            !---- recalculate wake radius array and Vwak
            call setxw(ctxt)
            !
            !---- recalculate thrust, power, and sensitivities for current solution
            call tpq(ctxt, 1)
            !
            !---- initialize max residuals
            ctxt%gresmx = 0.
            ctxt%fresmx = 0.
            ctxt%aresmx = 0.
            !
            do j = 1, k3
                ctxt%q(k2, j) = 0.
            enddo
            !
            !---- The wake advance ratio equation is only approximate, normally the
            !     tangential induced velocity is ignored (inconsistent with a rigid
            !     wake with constant wake advance ratio).  This calculates a factor
            !     to compensate for the Vt term at one (representative) radial station
            !
            do i = 1, ctxt%ii
                if(ctxt%xi(i) > 0.75) go to 40
            end do
            40   i75 = i
            call cscalc(ctxt, i75, utot, wa, wt, &
                    vt75, vt_adw, &
                    va75, va_adw, &
                    vd75, vd_adw, &
                    ci75, ci_adv, ci_vt, &
                    si75, si_va, &
                    w75, w_adv, w_vt, w_va, &
                    phi75, p_adv, p_vt, p_va)
            !---- Factor for omeg*r-vt correction to wake advance ratio
            advfact = 1.0 / (1.0 - ctxt%adv * vt75 / ctxt%xi(i75))
            !cc      write(*,*) 'adv factor ',advfact
            !---- Set to 1.0 for now... hhy
            advfact = 1.0
            !
            if(ctxt%free) then
                !----- Set up equation to converge wake advance ratio based on
                !      average axial velocity consistent with basic momentum theory
                !
                !---- Use "equivalent" prop thrust and power
                ctxt%dq(k2) = ctxt%adwfctr * ctxt%adw * ctxt%twak / ctxt%pwak - ctxt%adv * advfact
                z_tw = ctxt%adwfctr * ctxt%adw / ctxt%pwak
                z_pw = -ctxt%adwfctr * ctxt%adw * ctxt%twak / ctxt%pwak**2
                do j = 1, ctxt%ii
                    ctxt%q(k2, j) = z_tw * ctxt%tw_gam(j) + z_pw * ctxt%pw_gam(j)
                end do
                ctxt%q(k2, k1) = z_tw * ctxt%tw_adv + z_pw * ctxt%pw_adv - advfact
                ctxt%q(k2, k2) = z_tw * ctxt%tw_adw + z_pw * ctxt%pw_adw + ctxt%adwfctr * ctxt%twak / ctxt%pwak
                ctxt%aresmx = max(ctxt%aresmx, abs(ctxt%dq(k2) / ctxt%adv))
            else
                !----- specify zero change of wake advance ratios
                ctxt%dq(k2) = 0.
                ctxt%q(k2, k2) = 1.0
            endif
            !
            !---- go over stations, enforcing Gamma-cl relation at real prop
            do i = 1, ctxt%ii
                !
                call cscalc(ctxt, i, utot, wa, wt, &
                        vt, vt_adw, &
                        va, va_adw, &
                        vd, vd_adw, &
                        ci, ci_adv, ci_vt, &
                        si, si_va, &
                        w, w_adv, w_vt, w_va, &
                        phi, p_adv, p_vt, p_va)
                !
                alfa = ctxt%beta(i) - phi
                al_dbe = 1.0
                al_p = -1.0
                !
                rey = ctxt%ch(i) * abs(w) * ctxt%rho * ctxt%vel * ctxt%rad / ctxt%rmu
                call getclcdcm(ctxt, i, alfa, w, rey, &
                        ctxt%cl(i), cl_al, cl_w, &
                        clmax(i), clmin(i), dclstall(i), ctxt%stall(i), &
                        ctxt%cd(i), cd_alf, cd_w, cd_rey, &
                        ctxt%cm(i), cm_al, cm_w)
                !
                !------ Enforce local Gamma-cl relation
                ctxt%dq(i) = ctxt%ch(i) * ctxt%cl(i) * w - 2.0 * ctxt%gam(i)             ! residual
                z_cl = ctxt%ch(i) * w
                z_w = ctxt%ch(i) * ctxt%cl(i)
                !
                z_gi = - 2.0
                z_vt = z_cl * (cl_al * al_p * p_vt + cl_w * w_vt) + z_w * w_vt
                z_va = z_cl * (cl_al * al_p * p_va + cl_w * w_va) + z_w * w_va
                z_adv = z_cl * (cl_al * al_p * p_adv + cl_w * w_adv) + z_w * w_adv
                z_dbe = z_cl * (cl_al * al_dbe)
                !
                do j = 1, ctxt%ii
                    ctxt%q(i, j) = z_vt * ctxt%vind_gam(3, i, j)&
                            + z_va * ctxt%vind_gam(1, i, j)                ! dres/dgamj
                enddo
                ctxt%q(i, i) = ctxt%q(i, i) + z_gi                         ! dres/dgami
                ctxt%q(i, k1) = z_adv                                ! dres/ctxt%dadv
                ctxt%q(i, k2) = z_vt * vt_adw + z_va * va_adw    ! dres/ctxt%dadw
                ctxt%q(i, k3) = z_dbe                                ! dres/ctxt%dbeta
                !
                ctxt%gresmx = max(ctxt%gresmx, abs(ctxt%dq(i) / (0.1 * w)))
                !
            end do
            !
            !---- equivalent prop will be used to define inviscid thrust
            if(ispec == 1) then
                !----- drive thrust to specified value
                t_spec = ctxt%tspec / (ctxt%rho * ctxt%vel**2 * ctxt%rad**2)
                ctxt%dq(k1) = ctxt%twak + ctxt%tvis - t_spec
                do j = 1, ctxt%ii
                    ctxt%q(k1, j) = ctxt%tw_gam(j) + ctxt%tv_gam(j)
                enddo
                ctxt%q(k1, k1) = ctxt%tw_adv + ctxt%tv_adv
                ctxt%q(k1, k2) = ctxt%tw_adw + ctxt%tv_adw
                ctxt%q(k1, k3) = ctxt%tv_dbe
                !
                ctxt%fresmx = max(ctxt%fresmx, abs(ctxt%dq(k1)))
                !
            else if(ispec == 2) then
                !----- drive torque (= ptot*adv) to specified value
                q_spec = ctxt%qspec / (ctxt%rho * ctxt%vel**2 * ctxt%rad**3)
                ctxt%dq(k1) = (ctxt%pwak + ctxt%pvis) * ctxt%adv - q_spec
                do j = 1, ctxt%ii
                    ctxt%q(k1, j) = (ctxt%pw_gam(j) + ctxt%pv_gam(j)) * ctxt%adv
                enddo
                ctxt%q(k1, k1) = (ctxt%pw_adv + ctxt%pv_adv) * ctxt%adv + ctxt%pwak + ctxt%pvis
                ctxt%q(k1, k2) = (ctxt%pw_adw + ctxt%pv_adw) * ctxt%adv
                ctxt%q(k1, k3) = (ctxt%pv_dbe) * ctxt%adv
                !
                ctxt%fresmx = max(ctxt%fresmx, abs(ctxt%dq(k1)))
                !
            else if(ispec == 3) then
                !----- drive power to specified value
                p_spec = ctxt%pspec / (ctxt%rho * ctxt%vel**3 * ctxt%rad**2)
                ctxt%dq(k1) = ctxt%pwak + ctxt%pvis - p_spec
                do j = 1, ctxt%ii
                    ctxt%q(k1, j) = ctxt%pw_gam(j) + ctxt%pv_gam(j)
                enddo
                ctxt%q(k1, k1) = ctxt%pw_adv + ctxt%pv_adv
                ctxt%q(k1, k2) = ctxt%pw_adw + ctxt%pv_adw
                ctxt%q(k1, k3) = ctxt%pv_dbe
                !
                ctxt%fresmx = max(ctxt%fresmx, abs(ctxt%dq(k1)))
                !
            else if(ispec == 4) then
                !----- fix advance ratio
                ctxt%dq(k1) = 0.
                do j = 1, ctxt%ii
                    ctxt%q(k1, j) = 0.
                enddo
                ctxt%q(k1, k1) = 1.0
                ctxt%q(k1, k2) = 0.
                ctxt%q(k1, k3) = 0.
                !
            else if(ispec == 5) then
                !----- drive power to value given by rpm
                p_spec = ctxt%pspec / (ctxt%rho * ctxt%vel**3 * ctxt%rad**2)
                p_spec_adv = 0.0
                if(ctxt%lpwrvar) then
                    rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.)
                    rpm_adv = -rpm / ctxt%adv
                    !
                    !----- fix 5/15/03 use linear interpolation for engine power/rpm line
                    !cc         call sevlin(rpm,pwrvar,rpmvar,npwrvar,pspec,pspec_rpm)
                    ctxt%pspec = seval(rpm, ctxt%pwrvar(1:ctxt%npwrvar), ctxt%xpwrvar(1:ctxt%npwrvar), ctxt%rpmvar(1:ctxt%npwrvar))
                    pspec_rpm = deval(rpm, ctxt%pwrvar(1:ctxt%npwrvar), ctxt%xpwrvar(1:ctxt%npwrvar), ctxt%rpmvar(1:ctxt%npwrvar))
                    !
                    pspec_adv = pspec_rpm * rpm_adv
                    !
                    p_spec = ctxt%pspec / (ctxt%rho * ctxt%vel**3 * ctxt%rad**2)
                    p_spec_adv = pspec_adv / (ctxt%rho * ctxt%vel**3 * ctxt%rad**2)
                endif
                !
                ctxt%dq(k1) = ctxt%pwak + ctxt%pvis - p_spec
                do j = 1, ctxt%ii
                    ctxt%q(k1, j) = ctxt%pw_gam(j) + ctxt%pv_gam(j)
                enddo
                ctxt%q(k1, k1) = ctxt%pw_adv + ctxt%pv_adv - p_spec_adv
                ctxt%q(k1, k2) = ctxt%pw_adw + ctxt%pv_adw
                ctxt%q(k1, k3) = ctxt%pv_dbe
                !
                ctxt%fresmx = max(ctxt%fresmx, abs(ctxt%dq(k1)))
                !
            endif
            !
            !---- Constraint conditions
            ctxt%dq(k3) = 0.
            do j = 1, k3
                ctxt%q(k3, j) = 0.
            enddo
            if(icon == 1) ctxt%q(k3, k1) = 1.0      ! advance ratio(rpm) fixed
            if(icon == 2) ctxt%q(k3, k3) = 1.0      ! blade pitch fixed
            !
            !---- solve linearized Newton system
            call gauss(iq, k3, ctxt%q(1, 1), ctxt%dq(1), 1)
            !
            !
            ctxt%rlx = 1.0
            !---  Set initial iterations to underrelax
            if(iter <= 2) ctxt%rlx = 0.2
            !---- Apply limiters to the Newton updates based on physical properties
            do i = 1, ctxt%ii
                ctxt%dgam(i) = -ctxt%dq(i)
                !
                !---- limit cl changes near +- stall
                dcl = 2.0 * ctxt%dgam(i) / (ctxt%ch(i) * w)
                dclmin = max(1.5 * dclstall(i), abs(ctxt%cl(i) - clmin(i)))
                dclmax = max(1.5 * dclstall(i), abs(clmax(i) - ctxt%cl(i)))
                !
                dcllim = min(0.5, dclmin, dclmax)
                dcllim = max(dcllim, 0.01)
                if(ctxt%rlx * abs(dcl) > dcllim) then
                    ctxt%rlx = min(ctxt%rlx, dcllim / abs(dcl))
                    !cc      write(1,998) 'dcl lim i,rlx,cl,dcl ',i,rlx,cl(i),dcl,dcllim
                    !cc      write(1,998) 'clmax,clmin,dclstall ',i,clmax(i),clmin(i),
                    !cc     &              dclstall(i)
                endif
                998    format(a, 2x, i5, 4(2x, f12.5))
                !
                !---- limit gam changes that change sign
                if(ctxt%dgam(i) * ctxt%dgamold(i) < 0.0) then
                    if(abs(ctxt%dgam(i)) > 0.2 * abs(ctxt%dgamold(i))) then
                        ctxt%rlx = min(ctxt%rlx, 0.2)
                        !c        write(*,998) 'dgam lim i,rlx,gam,dgam ',i,rlx,gam(i),
                        !c     &               dgam(i),dgamold(i)
                    endif
                endif
                !
            enddo
            !
            ctxt%dadv = -ctxt%dq(k1)
            ctxt%dadw = -ctxt%dq(k2)
            ctxt%dbet = -ctxt%dq(k3)
            !
            if(ctxt%nitera == 0) ctxt%rlx = 0.0
            !
            !---- limit blade angle change to 0.05 radians  (~3 degrees)
            if(ctxt%rlx * ctxt%dbet > 0.05) ctxt%rlx = min(ctxt%rlx, 0.05 / ctxt%dbet)
            if(ctxt%rlx * ctxt%dbet < -.05) ctxt%rlx = min(ctxt%rlx, -0.05 / ctxt%dbet)
            !
            !---- limit advance ratio changes
            !      if(rlx*dadv > 0.8*adv) rlx = min(rlx,0.8*adv/dadv)
            !      if(rlx*dadv < -.5*adv) rlx = min(rlx,-.5*adv/dadv)
            !
            !      if(rlx*dadw > 0.8*adw) rlx = min(rlx, 0.8*adw/dadw)
            !      if(rlx*dadw < -.5*adw) rlx = min(rlx,-0.5*adw/dadw)
            !
            if(ctxt%rlx * ctxt%dadv > 0.5 * ctxt%adv) ctxt%rlx = min(ctxt%rlx, 0.5 * ctxt%adv / ctxt%dadv)
            if(ctxt%rlx * ctxt%dadv < -.3 * ctxt%adv) ctxt%rlx = min(ctxt%rlx, -.3 * ctxt%adv / ctxt%dadv)
            if(ctxt%rlx * ctxt%dadw > 0.5 * ctxt%adw) ctxt%rlx = min(ctxt%rlx, 0.5 * ctxt%adw / ctxt%dadw)
            if(ctxt%rlx * ctxt%dadw < -.3 * ctxt%adw) ctxt%rlx = min(ctxt%rlx, -0.3 * ctxt%adw / ctxt%dadw)
            !---- update circulation, blade angle arrays
            ctxt%rms = 0.
            gmx = 0.
            imx = 0
            do i = 1, ctxt%ii
                ctxt%gam(i) = ctxt%gam(i) + ctxt%rlx * ctxt%dgam(i)
                ctxt%beta(i) = ctxt%beta(i) + ctxt%rlx * ctxt%dbet
                ctxt%beta0(i) = ctxt%beta0(i) + ctxt%rlx * ctxt%dbet
                !
                ctxt%rms = ctxt%rms + ctxt%dgam(i)**2 / (1.0 + 1.0 / ctxt%adv**2)
                if(abs(ctxt%dgam(i)) >= abs(gmx)) then
                    gmx = ctxt%dgam(i)
                    imx = i
                endif
                ctxt%dgamold(i) = ctxt%dgam(i)
            enddo
            !
            !---- update incremental blade angle
            ctxt%dbeta = ctxt%dbeta + ctxt%rlx * ctxt%dbet
            !
            !---- update advance ratios
            ctxt%adv = ctxt%adv + ctxt%rlx * ctxt%dadv
            ctxt%adw = ctxt%adw + ctxt%rlx * ctxt%dadw
            !
            ctxt%rms = sqrt(ctxt%rms / float(ctxt%ii))
            !
            !---- display iteration history
            if (show_output) write(*, 2100) iter, gmx, imx, ctxt%rms, &
                    ctxt%adv, ctxt%adw, ctxt%beta(ctxt%ii) * 180.0 / pi, ctxt%rlx
            !
            2000 format(/' Iter     dGmax  @Imax    gGrms       Av        ', &
                    'Aw         Be       rlx')
            2100 format(1x, i3, 3x, e10.3, 2x, i3, 2x, e10.3, 2(2x, f8.4), 2x, f8.3, 2x, f8.4)
            !
            ! Iter     dGmax    (i)    gGrms      Av        Aw         Be       rlx
            !iiixxxeeeeeeeeeexxiiixxeeeeeeeeeexxff.ffffxxxff.ffffxxffff.fffxxfff.ffff
            !
            !
            !---- Smooth filter the gam for low relaxation factors
            if(ctxt%rlx < 0.2) then
                if (show_output) write(*, *) 'apiter filtering ctxt%gam'
                call filter(ctxt%gam, 0.2 * ctxt%ii, ctxt%ii)
            endif
            !
            !---- test for convergence
            if(ctxt%rms <= eps) then
                !----- final update of various quantities corresponding to converged solution
                !
                if(ctxt%free) then
                    if(ctxt%fast) then
                        call gradmo(ix, ctxt%ii, ctxt%nblds, ctxt%duct, ctxt%rake, &
                                ctxt%xi, ctxt%xv, ctxt%gam, ctxt%adw, ctxt%vind_gam, ctxt%vind_adw)
                        ctxt%iwtyp = 1
                    elseif(.not.ctxt%vrtx) then
                        call helico(ix, ctxt%ii, ctxt%nblds, ctxt%duct, ctxt%rake, &
                                ctxt%xi, ctxt%xv, ctxt%gam, ctxt%adw, ctxt%vind_gam, ctxt%vind_adw)
                        ctxt%iwtyp = 2
                    elseif(ctxt%vrtx) then
                        call vrtxco(ix, ctxt%ii, ctxt%nblds, ctxt%duct, ctxt%rake, &
                                ctxt%xi, ctxt%xv, ctxt%gam, ctxt%adw, ctxt%vind_gam, ctxt%vind_adw)
                        ctxt%iwtyp = 3
                    endif
                endif
                !
                call vcalc(ctxt)
                call tpq(ctxt, 1)
                !
                ctxt%conv = .true.
                return
            endif
            !c      if(mod(iter,5) == 0) call apinit(ctxt)
            !
        end do
        !
        return
    end
    ! apiter


    subroutine cscalc(ctxt, i, utot, wa, wt, &
            vt, vt_adw, &
            va, va_adw, &
            vd, vd_adw, &
            ci, ci_adv, ci_vt, &
            si, si_va, &
            w, w_adv, w_vt, w_va, &
            phi, p_adv, p_vt, p_va)
        use s_xrotor, only : uvadd
        !
        !---- Calculate velocity components at radial station i on real prop
        !
        use m_common, only : Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !
        vt = ctxt%vind(3, i)
        vt_adw = ctxt%vind_adw(3, i)
        !
        va = ctxt%vind(1, i)
        va_adw = ctxt%vind_adw(1, i)
        !
        !---- Include duct effect on freestream and induced axial velocity
        uduct = 0.0
        vaduct_va = 1.0
        if(ctxt%duct) then
            uduct = ctxt%urduct - 1.0
            vaduct_va = 2.0 * ctxt%urduct
        endif
        !------ duct induced axial velocity
        vd = va * (vaduct_va - 1.0)
        vd_va = (vaduct_va - 1.0)
        vd_adw = vd_va * va_adw
        !
        !---- Freestream, body induced and added inflow velocities
        utot = 1.0 + uduct + ctxt%ubody(i)
        call uvadd(ctxt, ctxt%xi(i), wa, wt)
        !
        ci = ctxt%xi(i) / ctxt%adv - wt - vt
        ci_adv = -ctxt%xi(i) / ctxt%adv**2
        ci_vt = -  1.0
        !
        si = utot + wa + va + vd
        si_va = 1.0 + vd_va
        !
        !---- Redefine va to include duct induced velocity
        !cc      va     =  va + vd
        !
        wsq = ci * ci + si * si
        w = sqrt(wsq)
        w_adv = (ci * ci_adv) / w
        w_vt = (ci * ci_vt) / w
        w_va = (si * si_va) / w
        !
        phi = atan2(si, ci)
        p_adv = (- si * ci_adv) / wsq
        p_vt = (- si * ci_vt) / wsq
        p_va = (ci * si_va) / wsq
        !
        !c      write(*,*) 'i,vt,va ',i,vt,va
        return
    end
    ! cscalc


    subroutine xwinit(ctxt)
        use m_common, only : Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !------------------------------------------------------------
        !     Initial estimate for equivalent prop radial coordinate
        !     array (xw)
        !------------------------------------------------------------
        !
        uduct = 0.0
        vaduct_va = 1.0
        if(ctxt%duct) then
            uduct = ctxt%urduct - 1.0
            vaduct_va = 2.0 * ctxt%urduct
        endif

        xm = ctxt%xw0
        do i = 1, ctxt%ii
            urat = 1.0 + uduct + ctxt%ubody(i)
            ctxt%dxw(i) = sqrt(xm**2 + 2.0 * urat * ctxt%xi(i) * ctxt%dxi(i)) - xm
            xp = xm + ctxt%dxw(i)
            ctxt%xw(i) = 0.5 * (xp + xm)
            !
            ctxt%xw_adv(i) = 0.
            ctxt%xw_adw(i) = 0.
            do j = 1, ctxt%ii
                ctxt%xw_gam(i, j) = 0.
            end do
            !
            ctxt%vwak(i) = ctxt%vind(3, i) * ctxt%xi(i) / ctxt%xw(i)
            ctxt%vw_adv(i) = 0.
            ctxt%vw_adw(i) = ctxt%vind_adw(3, i) * ctxt%xi(i) / ctxt%xw(i)
            do j = 1, ctxt%ii
                ctxt%vw_gam(i, j) = ctxt%vind_gam(3, i, j) * ctxt%xi(i) / ctxt%xw(i)
            end do
            !
            xm = xp
        end do
        !
        ctxt%xwtip = xm
        !
        return
    end
    ! xwinit


    subroutine setxw(ctxt)
        use m_common, only : Common, ix, show_output
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        real xwm_gam(ix), z_gam(ix)
        !---------------------------------------------------------------------
        !     Calculates Xw (radial coordinate) and Vwak (Vtheta) for
        !     the "equivalent prop"
        !     The radial stream function s xi dxi (Vax r dr) is used to
        !     define radial coordinate for the equivalent prop. The angular
        !     momentum is preserved to define the equivalent prop Vwak (Vtheta)
        !----------------------------------------------------------------------
        !
        xwm = ctxt%xw0
        !
        do j = 1, ctxt%ii
            xwm_gam(j) = 0.
        end do
        xwm_adv = 0.
        xwm_adw = 0.
        !cc      write(*,*) 'setxw adv,adw ',adv,adw
        !
        do i = 1, ctxt%ii
            xdx = ctxt%xi(i) * ctxt%dxi(i)
            !
            call cscalc(ctxt, i, utot, wa, wt, &
                    vt, vt_adw, &
                    va, va_adw, &
                    vd, vd_adw, &
                    ci, ci_adv, ci_vt, &
                    si, si_va, &
                    w, w_adv, w_vt, w_va, &
                    phi, p_adv, p_vt, p_va)
            !
            !------ first guess for xwo
            dxwo = sqrt(xwm**2 + 2.0 * utot * xdx) - xwm
            xwo = xwm + 0.5 * dxwo
            !
            !------ Newton loop for xwo
            do itx = 1, 30
                !
                vw = vt * ctxt%xi(i) / xwo
                vw_xwo = -vw / xwo
                vw_vt = ctxt%xi(i) / xwo
                !
                !------ swirl velocity on equivalent prop
                !cc************ not used
                cw = xwo / ctxt%adv - wt - vw
                cw_xwo = 1.0 / ctxt%adv - vw_xwo
                !
                utotw = ctxt%urduct
                !------ axial velocity on equivalent prop (derived from swirl)
                vaw = vw * xwo / ctxt%adw
                !------ no duct effect on freestream or axial induced velocity for equiv prop
                vaw_vw = xwo / ctxt%adw
                vaw_xwo = vw / ctxt%adw + vaw_vw * vw_xwo
                !
                sw = utotw + wa + vaw
                sw_xwo = vaw_xwo
                !
                rez = sw * xwo * 2.0 * (xwo - xwm) - si * xdx
                rez_xwo = sw * 2.0 * (2.0 * xwo - xwm) + sw_xwo * xwo * 2.0 * (xwo - xwm)
                delxwo = -rez / rez_xwo
                !
                ctxt%rlx = 1.0
                if(abs(delxwo) > 0.2 * (xwo - xwm))&
                        ctxt%rlx = 0.2 * (xwo - xwm) / abs(delxwo)
                !
                xwo = xwo + ctxt%rlx * delxwo
                if(abs(delxwo) < 1.0e-6) go to 101
                !
            end do
            if (show_output) write(*, 990) 'setxw: ctxt%xw convergence failed.  i, r/r, ctxt%dxw :', &
                    i, ctxt%xi(i), delxwo
            990    format(a, i5, 2(1x, f12.6))
            !
            101   continue
            !
            dxwo = 2.0 * (xwo - xwm)
            !
            !------ Vw( xwo , Vt(Adw Gj) )
            vw = vt * ctxt%xi(i) / xwo
            vw_xwo = -vw / xwo
            vw_vt = ctxt%xi(i) / xwo
            !
            !------ swirl velocity on equivalent prop
            !cc************ not used
            cw = xwo / ctxt%adv - wt - vw
            cw_xwo = 1.0 / ctxt%adv - vw_xwo
            cw_vt = - vw_vt
            cw_adv = -xwo / ctxt%adv**2
            !
            utotw = ctxt%urduct
            !------ axial velocity on equivalent prop (derived from swirl)
            !------ no duct effect on freestream or axial induced velocity for equiv prop
            vaw = vw * xwo / ctxt%adw
            vaw_vw = xwo / ctxt%adw
            vaw_xwo = vw / ctxt%adw
            vaw_adw = -vaw / ctxt%adw
            !
            sw = utotw + wa + vaw
            sw_xwo = vaw_xwo + vaw_vw * vw_xwo
            sw_vt = vaw_vw * vw_vt
            sw_adw = vaw_adw
            !
            !        write(*,9999) 'setxw xi,xwvt,vw,sw ',xi(i),xwo,vt,vw,sw
            ! 9999   format(a,5f10.5)
            !
            !------ Res ( xwo , xwm , Sw(Adw Vt xw) , s(Adw Vt) )
            !cc       rez = sw*xwo*2.0*(xwo-xwm) - si*xdx
            z_xwo = sw * 2.0 * (2.0 * xwo - xwm)
            z_xwm = -sw * xwo * 2.0
            z_sw = xwo * 2.0 * (xwo - xwm)
            z_si = -xdx
            !
            !------ Res ( xwo , xwm(Gj Adv Adw) , vt(Gj Adw) , va(Gj Adw) , Adw )
            z_xwo = z_sw * sw_xwo + z_xwo
            z_vt = z_sw * sw_vt
            z_va = z_si * si_va
            z_adw = z_sw * sw_adw
            !
            !------ Res ( xwo , Adv , Adw , Gj )
            z_adv = z_xwm * xwm_adv
            z_adw = z_xwm * xwm_adw + z_vt * vt_adw&
                    + z_va * va_adw + z_adw
            do j = 1, ctxt%ii
                z_gam(j) = z_xwm * xwm_gam(j) + z_vt * ctxt%vind_gam(3, i, j)&
                        + z_va * ctxt%vind_gam(1, i, j)
            end do
            !
            !------ xwo( Adv , Adw , Gj )
            ctxt%xw_adv(i) = -z_adv / z_xwo
            ctxt%xw_adw(i) = -z_adw / z_xwo
            do j = 1, ctxt%ii
                ctxt%xw_gam(i, j) = -z_gam(j) / z_xwo
            end do
            !
            !------ Vw( xwo(Adv Adw Gj) , Vt(Adw Gj) )
            ctxt%vwak(i) = vw
            !------ Vw( Adv Adw Gj )
            ctxt%vw_adv(i) = vw_xwo * ctxt%xw_adv(i)
            ctxt%vw_adw(i) = vw_xwo * ctxt%xw_adw(i) + vw_vt * vt_adw
            do j = 1, ctxt%ii
                ctxt%vw_gam(i, j) = vw_xwo * ctxt%xw_gam(i, j) + vw_vt * ctxt%vind_gam(3, i, j)
            end do
            !
            !
            ctxt%xw(i) = xwo
            !
            !------ dxw( xwo(Adv Adw Gj) , xwm(Adv Adw Gj) )
            ctxt%dxw(i) = 2.0 * (xwo - xwm)
            ctxt%dxw_adv(i) = 2.0 * (ctxt%xw_adv(i) - xwm_adv)
            ctxt%dxw_adw(i) = 2.0 * (ctxt%xw_adw(i) - xwm_adw)
            do j = 1, ctxt%ii
                ctxt%dxw_gam(i, j) = 2.0 * (ctxt%xw_gam(i, j) - xwm_gam(j))
            end do
            !
            !------ new  xwm(Adv Adw Gj)  for next loop pass
            xwm = 2.0 * xwo - xwm
            xwm_adv = 2.0 * ctxt%xw_adv(i) - xwm_adv
            xwm_adw = 2.0 * ctxt%xw_adw(i) - xwm_adw
            do j = 1, ctxt%ii
                xwm_gam(j) = 2.0 * ctxt%xw_gam(i, j) - xwm_gam(j)
            end do
            !
        end do
        !
        ctxt%xwtip = xwm
        !      write(*,*) 'xwtip ',xwtip
        !      do i=1,ii
        !        write(20,*) 'xi,xw,vwak ',xi(i),xw(i),vwak(i)
        !      end do
        !
        return
    end
    ! setxw



    subroutine tpq(ctxt, itype)
        use m_xaero, only : getclcdcm, getalf
        use s_xrotor, only : uvadd
        use m_common, only : Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !----------------------------------------------------------
        !     Sets Thrust, Torque, Power, and their sensitivities
        !     wrt  beta, chord(i), Vtan(i), and lambda
        !----------------------------------------------------------
        !
        ctxt%tinv = 0.
        ctxt%pinv = 0.
        !
        ctxt%twak = 0.
        ctxt%pwak = 0.
        !
        vaAavg = 0.
        vaTavg = 0.
        tmom = 0.
        pmom = 0.
        !
        ctxt%tvis = 0.
        ctxt%pvis = 0.
        !
        ctxt%ti_adv = 0.
        ctxt%pi_adv = 0.
        ctxt%ti_adw = 0.
        ctxt%pi_adw = 0.
        !
        ctxt%tw_adv = 0.
        ctxt%pw_adv = 0.
        ctxt%tw_adw = 0.
        ctxt%pw_adw = 0.
        !
        ctxt%tv_adv = 0.
        ctxt%pv_adv = 0.
        ctxt%tv_adw = 0.
        ctxt%pv_adw = 0.
        ctxt%tv_dbe = 0.
        ctxt%pv_dbe = 0.
        !
        do i = 1, ctxt%ii
            ctxt%ti_gam(i) = 0.
            ctxt%pi_gam(i) = 0.
            ctxt%tw_gam(i) = 0.
            ctxt%pw_gam(i) = 0.
            ctxt%tv_gam(i) = 0.
            ctxt%pv_gam(i) = 0.
        enddo
        !
        cosr = cos(ctxt%rake)
        !
        !---- go over radial stations, setting viscous thrust and power
        blds = float(ctxt%nblds)
        do i = 1, ctxt%ii
            bdx = blds * ctxt%dxi(i)
            !
            xx = ctxt%xi(i) / ctxt%adv
            xx_adv = -xx / ctxt%adv
            !
            !------ set  w(Adv,Adw,Vt)  and  Phi(Adv,Adw,Vt)  sensitivities
            call cscalc(ctxt, i, utot, wa, wt, &
                    vt, vt_adw, &
                    va, va_adw, &
                    vd, vd_adw, &
                    ci, ci_adv, ci_vt, &
                    si, si_va, &
                    w, w_adv, w_vt, w_va, &
                    phi, p_adv, p_vt, p_va)
            !
            alfa = ctxt%beta(i) - phi
            al_dbe = 1.0
            al_p = -1.0
            !
            !
            if(itype == 1) then
                !------- analysis case:  fix local Beta (except for pitch change)
                !
                !------- set alfa(Gi,dBeta,Adv,Vt) sensitivites
                alfa = ctxt%beta(i) - phi
                al_gi = 0.
                al_dbe = 1.0
                al_adv = -p_adv
                al_vt = -p_vt
                al_va = -p_va
                !
                !------- set cl(Gi,dBeta,Adv,Adw,Vt) sensitivites
                rey = ctxt%ch(i) * abs(w) * ctxt%rho * ctxt%vel * ctxt%rad / ctxt%rmu
                call getclcdcm(ctxt, i, alfa, w, rey, &
                        ctxt%cl(i), cl_al, cl_w, &
                        clmax, clmin, dclstall, ctxt%stall(i), &
                        ctxt%cd(i), cd_alf, cd_w, cd_rey, &
                        ctxt%cm(i), cm_al, cm_w)
                cl_gi = cl_al * al_gi
                cl_dbe = cl_al * al_dbe
                cl_adv = cl_al * al_adv + cl_w * w_adv
                cl_vt = cl_al * al_vt + cl_w * w_vt
                cl_va = cl_al * al_va + cl_w * w_va
                !
                !------- set c(Gi,Adv,Vt) sensitivites  (chord is fixed)
                ch_gi = 0.
                ch_adv = 0.
                ch_vt = 0.
                ch_va = 0.
                !
            else if(itype == 2) then
                !------- design case:  fix local cl and set chord based on circulation
                !
                !------- set alfa(Gi,dBeta,Adv,Adw,Vt) sensitivites
                !c         write(*,*) 'tpq2 getalf i,cl,w ',i,cl(i),w
                call getalf(ctxt, i, ctxt%cl(i), w, alfa, al_cl, al_w, ctxt%stall(i))
                al_gi = 0.
                al_dbe = 0.
                al_adv = al_w * w_adv
                al_vt = al_w * w_vt
                al_va = al_w * w_va
                !
                !------- set cl(Gi,dBeta,Adv,Adw,Vt) sensitivites
                cl_gi = 0.
                cl_dbe = 0.
                cl_adv = 0.
                cl_vt = 0.
                cl_va = 0.
                !
                !------- set c(Gi,Adv,Adw,Vt) sensitivites
                chnew = 2.0 * ctxt%gam(i) / (w * ctxt%cl(i))
                !--- Check for chord going zero or negative and use nearby station data
                !    for this iteration
                if(chnew <= 0.0) then
                    !c           write(*,*) 'tpq negative chord @i = ',i,chnew
                    if(i == 1) then
                        ctxt%ch(i) = ctxt%ch(i + 1)
                    elseif(i == ctxt%ii) then
                        ctxt%ch(i) = ctxt%ch(i - 1)
                    else
                        ctxt%ch(i) = 0.5 * (ctxt%ch(i - 1) + ctxt%ch(i + 1))
                    endif
                    ch_gi = 0.0
                    ch_adv = 0.0
                    ch_vt = 0.0
                    ch_va = 0.0
                else
                    ctxt%ch(i) = 2.0 * ctxt%gam(i) / (w * ctxt%cl(i))
                    ch_gi = 2.0 / (w * ctxt%cl(i))
                    ch_adv = (-ctxt%ch(i) / w) * w_adv
                    ch_vt = (-ctxt%ch(i) / w) * w_vt
                    ch_va = (-ctxt%ch(i) / w) * w_va
                endif
                !
                ctxt%beta(i) = alfa + phi
                ctxt%beta0(i) = ctxt%beta(i)
                !
            else if(itype == 3) then
                !------- design case:  fix local chord and set angles based on cl
                !
                !------- set cl(Gi,dBeta,Adv,Adw,Vt) sensitivites
                ctxt%cl(i) = 2.0 * ctxt%gam(i) / (w * ctxt%ch(i))
                cl_gi = 2.0 / (w * ctxt%ch(i))
                cl_dbe = 0.
                cl_adv = (-ctxt%cl(i) / w) * w_adv
                cl_vt = (-ctxt%cl(i) / w) * w_vt
                cl_va = (-ctxt%cl(i) / w) * w_va
                !
                !------- set alfa(Gi,dBeta,Adv,Adw,Vt) sensitivites
                !c         write(*,*) 'tpq3 getalf i,cl,w ',i,cl(i)
                call getalf(ctxt, i, ctxt%cl(i), w, alfa, al_cl, al_w, ctxt%stall(i))
                al_gi = al_cl * cl_gi
                al_dbe = al_cl * cl_dbe
                al_adv = al_cl * cl_adv + al_w * w_adv
                al_vt = al_cl * cl_vt + al_w * w_vt
                al_va = al_cl * cl_va + al_w * w_va
                !
                !------- set c(Gi,Adv,Adw,Vt) sensitivites
                ch_gi = 0.
                ch_adv = 0.
                ch_vt = 0.
                ch_va = 0.
                !
                ctxt%beta(i) = alfa + phi
                ctxt%beta0(i) = ctxt%beta(i)
                !
            endif
            !
            ctxt%re(i) = ctxt%ch(i) * abs(w) * ctxt%rho * ctxt%vel * ctxt%rad / ctxt%rmu
            re_w = ctxt%ch(i) * ctxt%rho * ctxt%vel * ctxt%rad / ctxt%rmu
            re_ch = abs(w) * ctxt%rho * ctxt%vel * ctxt%rad / ctxt%rmu
            !
            !------ set Re(Gi,Adv,Adw,Vt) sensitivites
            re_gi = re_ch * ch_gi
            re_adv = re_ch * ch_adv + re_w * w_adv
            re_vt = re_ch * ch_vt + re_w * w_vt
            re_va = re_ch * ch_va + re_w * w_va
            !
            !------ set cm and (not used at present) sensitivites
            !------ set cd(Gi,dBeta,Adv,Adw,Vt) sensitivites
            call getclcdcm(ctxt, i, alfa, w, ctxt%re(i), &
                    ctxt%cl(i), cl_al, cl_w, &
                    clmax, clmin, dclstall, ctxt%stall(i), &
                    ctxt%cd(i), cd_al, cd_w, cd_re, &
                    ctxt%cm(i), cm_al, cm_w)
            !cc        write(*,*) 'tpq alfa,cl,cd,cm ',i,alfa,cl(i),cd(i),cm(i)
            cd_gi = cd_al * al_gi + cd_re * re_gi
            cd_adv = cd_al * al_adv + cd_re * re_adv + cd_w * w_adv
            cd_vt = cd_al * al_vt + cd_re * re_vt + cd_w * w_vt
            cd_va = cd_al * al_va + cd_re * re_va + cd_w * w_va
            cd_dbe = cd_al * al_dbe
            !
            !------ set total local efficiency
            eff = (ctxt%cl(i) * ci - ctxt%cd(i) * si) / (ctxt%cd(i) * ci + ctxt%cl(i) * si) / xx
            !---Correct for blade rake
            eff = eff * cosr
            !
            !------ set induced and profile local efficiencies
            effi = ci / (si * xx)
            !---Correct for blade rake
            effi = effi * cosr
            !
            ctxt%effp(i) = eff / effi
            !
            hwc = 0.5 * w * ctxt%ch(i)
            hwc_w = 0.5 * ctxt%ch(i)
            hwc_ch = 0.5 * w
            !
            !
            !*******************************************************
            !------ Viscous Thrust & Power contributions on real prop
            !c      cosrv = cosr
            cosrv = 1.0
            !
            !------ dTv ( Cd , s , w , c ) sensitivites
            dtv = -hwc * ctxt%cd(i) * si * bdx * cosrv
            !
            dtv_cd = -hwc * si * bdx * cosrv
            dtv_si = -hwc * ctxt%cd(i) * bdx * cosrv
            dtv_w = -hwc_w * ctxt%cd(i) * si * bdx * cosrv
            dtv_ch = -hwc_ch * ctxt%cd(i) * si * bdx * cosrv
            !
            !------ set Tv(Gi,dBeta,Adv,Vt) sensitivites using chain rule
            dtv_gi = dtv_cd * cd_gi + dtv_ch * ch_gi
            dtv_dbe = dtv_cd * cd_dbe
            dtv_adv = dtv_cd * cd_adv + dtv_ch * ch_adv&
                    + dtv_w * w_adv
            dtv_vt = dtv_cd * cd_vt + dtv_ch * ch_vt&
                    + dtv_w * w_vt
            dtv_va = dtv_cd * cd_va + dtv_ch * ch_va&
                    + dtv_si * si_va + dtv_w * w_va
            !
            !------ accumulate viscous Thrust and sensitivities
            ctxt%tvis = ctxt%tvis + dtv
            ctxt%tv_adv = ctxt%tv_adv + dtv_adv
            ctxt%tv_adw = dtv_vt * vt_adw + dtv_va * va_adw
            ctxt%tv_dbe = ctxt%tv_dbe + dtv_dbe
            !
            ctxt%tv_gam(i) = ctxt%tv_gam(i) + dtv_gi
            do j = 1, ctxt%ii
                ctxt%tv_gam(j) = ctxt%tv_gam(j) + dtv_vt * ctxt%vind_gam(3, i, j)&
                        + dtv_va * ctxt%vind_gam(1, i, j)
            enddo
            !
            !------ dPv( Cd , c , w , c )
            dpv = hwc * ctxt%cd(i) * ci * bdx * xx
            !
            dpv_cd = hwc * ci * bdx * xx
            dpv_ci = hwc * ctxt%cd(i) * bdx * xx
            dpv_w = hwc_w * ctxt%cd(i) * ci * bdx * xx
            dpv_ch = hwc_ch * ctxt%cd(i) * ci * bdx * xx
            !
            !------ set Pv(Gi,dBeta,Adv,Vt) sensitivites using chain rule
            dpv_gi = dpv_cd * cd_gi + dpv_ch * ch_gi
            dpv_dbe = dpv_cd * cd_dbe
            dpv_adv = dpv_cd * cd_adv + dpv_ch * ch_adv&
                    + dpv_ci * ci_adv + dpv_w * w_adv&
                    + hwc * ctxt%cd(i) * ci * bdx * xx_adv
            dpv_vt = dpv_cd * cd_vt + dpv_ch * ch_vt&
                    + dpv_ci * ci_vt + dpv_w * w_vt
            dpv_va = dpv_cd * cd_va + dpv_ch * ch_va&
                    + dpv_w * w_va
            !
            !------ accumulate viscous Power and sensitivities
            ctxt%pvis = ctxt%pvis + dpv
            ctxt%pv_adv = ctxt%pv_adv + dpv_adv
            ctxt%pv_adw = dpv_vt * vt_adw + dpv_va * va_adw
            ctxt%pv_dbe = ctxt%pv_dbe + dpv_dbe
            !
            ctxt%pv_gam(i) = ctxt%pv_gam(i) + dpv_gi
            do j = 1, ctxt%ii
                ctxt%pv_gam(j) = ctxt%pv_gam(j) + dpv_vt * ctxt%vind_gam(3, i, j)&
                        + dpv_va * ctxt%vind_gam(1, i, j)
            enddo
            !
            !
            !*******************************************************
            !------ Inviscid Thrust & Power contributions on real prop
            !c      cosri = cosr
            cosri = 1.0
            !
            !------ dTi( Gi , c( Adv Vt ) )
            dti = ctxt%gam(i) * ci * bdx * cosri
            !
            dti_ci = ctxt%gam(i) * bdx * cosri
            dti_gi = ci * bdx * cosri
            !
            !------ dTi( Adv , Vt(Adw Gj) )
            dti_vt = dti_ci * ci_vt
            dti_adv = dti_ci * ci_adv
            dti_adw = dti_vt * vt_adw
            !
            !------ accumulate inviscid Thrust and sensitivities
            ctxt%tinv = ctxt%tinv + dti
            ctxt%ti_adv = ctxt%ti_adv + dti_adv
            ctxt%ti_adw = ctxt%ti_adw + dti_adw
            !------ Resolve dTi dependencies ( Vt ) to Gamma
            ctxt%ti_gam(i) = ctxt%ti_gam(i) + dti_gi
            do j = 1, ctxt%ii
                ctxt%ti_gam(j) = ctxt%ti_gam(j) + dti_vt * ctxt%vind_gam(3, i, j)
            enddo
            !
            !------ dPi( s(Va) , Gi, Adv )
            dpi = ctxt%gam(i) * si * bdx * xx
            !
            dpi_si = ctxt%gam(i) * bdx * xx
            dpi_gi = si * bdx * xx
            dpi_xx = ctxt%gam(i) * si * bdx
            !
            !------ dPi( Va(Gj Adw) , Adv , Adw , Gi )
            dpi_va = dpi_si * si_va
            dpi_adv = dpi_xx * xx_adv
            dpi_adw = dpi_va * va_adw
            !
            !------ accumulate inviscid Power and sensitivities
            ctxt%pinv = ctxt%pinv + dpi
            ctxt%pi_adv = ctxt%pi_adv + dpi_adv
            ctxt%pi_adw = ctxt%pi_adw + dpi_adw
            !------ Resolve dPi dependencies ( Va ) to Gamma
            ctxt%pi_gam(i) = ctxt%pi_gam(i) + dpi_gi
            do j = 1, ctxt%ii
                ctxt%pi_gam(j) = ctxt%pi_gam(j) + dpi_va * ctxt%vind_gam(1, i, j)
            enddo
            !
            !*******************************************************


            !*******************************************************
            !------ Inviscid Thrust & Power contributions on equivalent prop
            !       Assumes Omega and Gamma are same in real and equivalent prop
            !
            vw = ctxt%vwak(i)
            utotw = ctxt%urduct
            call uvadd(ctxt, ctxt%xi(i), wa, wt)
            !
            !------ Cw defined by same omega as real prop
            cw = ctxt%xw(i) / ctxt%adv - wt - vw
            cw_adv = -ctxt%xw(i) / ctxt%adv**2
            cw_vw = -  1.0
            cw_xw = 1.0 / ctxt%adv
            !------ Sw( Adw , xw , Vw ) ;  xw, Vw( Gj , Adv , Adw )
            sw = utotw + wa + vw * ctxt%xw(i) / ctxt%adw
            sw_adw = -  vw * ctxt%xw(i) / ctxt%adw**2
            sw_vw = ctxt%xw(i) / ctxt%adw
            sw_xw = vw / ctxt%adw
            !
            !------ dTw( Gi , cw( Adv Vw ) , dxw( Gj, Adv, Adw) )
            dtw = ctxt%gam(i) * cw * blds * ctxt%dxw(i)
            !
            dtw_gi = cw * blds * ctxt%dxw(i)
            dtw_cw = ctxt%gam(i) * blds * ctxt%dxw(i)
            dtw_dxw = ctxt%gam(i) * cw * blds
            !------ dTw( Vt(Adw Gj) , Adv , Adw , Gi , dxw(Gj Adv Adw) )
            dtw_vw = dtw_cw * cw_vw
            dtw_adv = dtw_cw * cw_adv + dtw_vw * ctxt%vw_adv(i)&
                    + dtw_dxw * ctxt%dxw_adv(i)
            dtw_adw = dtw_vw * ctxt%vw_adw(i) + dtw_dxw * ctxt%dxw_adw(i)
            !
            !------ accumulate Thrust and sensitivities
            ctxt%twak = ctxt%twak + dtw
            ctxt%tw_adv = ctxt%tw_adv + dtw_adv
            ctxt%tw_adw = ctxt%tw_adw + dtw_adw
            !
            !------ Resolve dTw dependencies ( Vt, Va, dxw ) to Gamma
            ctxt%tw_gam(i) = ctxt%tw_gam(i) + dtw_gi
            do j = 1, ctxt%ii
                ctxt%tw_gam(j) = ctxt%tw_gam(j) + dtw_vw * ctxt%vw_gam(i, j)&
                        + dtw_dxw * ctxt%dxw_gam(i, j)
            enddo
            !
            !
            !------ dPw( s(Va) , Gi , Adv )
            dpw = ctxt%gam(i) * si * bdx * ctxt%xi(i) / ctxt%adv
            !
            dpw_si = ctxt%gam(i) * bdx * ctxt%xi(i) / ctxt%adv
            dpw_gi = si * bdx * ctxt%xi(i) / ctxt%adv
            dpw_adv = -dpw / ctxt%adv
            !
            !------ dPw( Adv , Adw , Va(Gj Adw) , Gi )
            dpw_va = dpw_si * si_va
            dpw_adw = dpw_va * va_adw
            !
            !------ accumulate Power and sensitivities
            ctxt%pwak = ctxt%pwak + dpw
            ctxt%pw_adv = ctxt%pw_adv + dpw_adv
            ctxt%pw_adw = ctxt%pw_adw + dpw_adw
            !
            !------ Resolve dPw dependencies ( Va ) to Gamma
            ctxt%pw_gam(i) = ctxt%pw_gam(i) + dpw_gi
            do j = 1, ctxt%ii
                ctxt%pw_gam(j) = ctxt%pw_gam(j) + dpw_va * ctxt%vind_gam(1, i, j)
            enddo
            !
            !        write(*,1011) 'dtw dpw dti dpi ',dtw,dpw,dti,dpi,cw/sw,ci/si
            ! 1011   format(a,6f11.5)
            !
            !------ Save blade thrust and power contributions (per blade, per span)
            ctxt%dtii(i) = dti / bdx
            ctxt%dpii(i) = dpi / bdx
            ctxt%dtwi(i) = dtw / bdx
            ctxt%dpwi(i) = dpw / bdx
            ctxt%dtvi(i) = dtv / bdx
            ctxt%dpvi(i) = dpv / bdx
            !
            !cc        write(21,*) xi(i),twak
            !
            !*******************************************************
            !------ Inviscid Thrust & Power from momentum
            !
            !------ dTmom( s(Va) , Va )
            vtgm = ctxt%gam(i) * blds / (4.0 * pi * ctxt%xi(i))
            vagm = vtgm * ctxt%xi(i) / ctxt%adw
            dtm = 2.0 * pi * ctxt%xi(i) * ctxt%dxi(i) * si * (2.0 * va)
            tmom = tmom + dtm
            !cc        write(20,*) xi(i),tmom,va,vd
            !
            !------ dPmom( s(Va) )
            dpm = dtm * (si - vd)
            pmom = pmom + dpm
            !
            vaTavg = vaTavg + dtw * (va + vd)
            vaaavg = vaaavg + 2.0 * pi * ctxt%xi(i) * ctxt%dxi(i) * (va + vd)
            !
        end do
        !cc        write(20,*) '&'
        !cc        write(21,*) '&'
        !
        ctxt%ttot = ctxt%twak + ctxt%tvis
        ctxt%ptot = ctxt%pwak + ctxt%pvis
        ctxt%qtot = ctxt%ptot * ctxt%adv
        !cc      write(*,*) 'tw,ti,tm ',twak,tinv,tmom
        !      write(*,*) 'tblDdim ',tinv*rho*vel**2*rad**2
        !      write(*,*) 'twaKdim ',twak*rho*vel**2*rad**2
        !      write(*,*) 'ttoTdim ',ttot*rho*vel**2*rad**2
        !      write(*,*) 'tmoMdim ',tmom*rho*vel**2*rad**2
        !
        !      write(*,*) 'pblDdim ',pinv*rho*vel**3*rad**2
        !      write(*,*) 'pwaKdim ',pwak*rho*vel**3*rad**2
        !      write(*,*) 'pmoMdim ',pmom*rho*vel**3*rad**2
        !
        !---- disk area
        adisk = pi * (1.0 - ctxt%xi0**2)
        vaAavg = vaAavg / adisk
        vatavg = vatavg / ctxt%twak
        !      write(*,*) '     va Aavg ',vaAavg*vel
        !      write(*,*) '     va Tavg ',vaTavg*vel
        !
        tdim = ctxt%twak * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
        pdim = ctxt%pwak * ctxt%rho * ctxt%vel**3 * ctxt%rad**2
        !      write(*,*) 'Vinduced from pwak/twak ',pdim/tdim
        !
        return
    end
    ! tpq



    subroutine vcalc(ctxt)
        use m_common, only : Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !---------------------------------------------
        !     Calculates cartesian induced velocities
        !---------------------------------------------
        do i = 1, ctxt%ii
            vxsum = 0.
            vysum = 0.
            vzsum = 0.
            do j = 1, ctxt%ii
                vxsum = vxsum + ctxt%vind_gam(1, i, j) * ctxt%gam(j)
                vysum = vysum + ctxt%vind_gam(2, i, j) * ctxt%gam(j)
                vzsum = vzsum + ctxt%vind_gam(3, i, j) * ctxt%gam(j)
            enddo
            ctxt%vind(1, i) = vxsum
            ctxt%vind(2, i) = vysum
            ctxt%vind(3, i) = vzsum
        enddo
        !
        return
    end
    ! vcalc



    subroutine gradmo(imax, ii, nblds, lduct, rake, &
            xi, xv, gam, adw, vind_gam, vind_adw)
        use m_common, only: pi
        dimension xi(imax), xv(imax), gam(imax)
        dimension vind_adw(3, imax), vind_gam(3, imax, imax)
        logical lduct
        !-----------------------------------------
        !     Calculates "Graded Momentum"
        !     Gamma-swirl influence coefficients
        !
        !     Input:
        !       imax         array dimension
        !       ii           number of radial points on blade (circulation stations)
        !       nn           number of Fourier harmonics
        !       nblds        number of blades
        !       lduct        t for duct outer bc
        !       xi(i)        radial coordinate array
        !       gam(i)       circulation array
        !       adw          wake advance ratio  v/wr
        !
        !     Output:
        !
        !     Output:
        !       vind_gam(i,j)  sensitivity of velocity at i to circulation at j
        !       vind_adw(i)    sensitivity of velocity at i to wake advance ratio
        !
        !        Where vind_xxx(1,i,j) is the axial component
        !              vind_xxx(3,i,j) is the swirl component
        !-----------------------------------------
        blds = float(nblds)
        !
        !pi = 4.0 * atan(1.0)
        !
        xi0 = xv(1)
        xitip = xv(ii + 1)
        !
        if(lduct) then
            !
            !----- Circulation defines mean swirl at blade
            !----- use simple mean swirl to get swirl at blade
            do i = 1, ii
                do j = 1, ii
                    vind_gam(1, i, j) = 0.
                    vind_gam(2, i, j) = 0.
                    vind_gam(3, i, j) = 0.
                enddo
                vind_gam(3, i, i) = blds / (4.0 * pi * xi(i))
                vind_adw(3, i) = 0.0
                vind_adw(2, i) = 0.0
                vind_gam(1, i, i) = vind_gam(3, i, i) * xi(i) / adw
                vind_adw(1, i) = -vind_gam(1, i, i) * gam(i) / adw
            enddo
            !
        else
            !
            !----- Circulation defines mean swirl at blade
            !----- Free-tip treatment incorporates Prandtl's averaging factor f
            sfac = sqrt(1.0 + 1.0 / adw**2)
            sf_adw = 0.5 / sfac * (-2.0 / adw**3)
            !
            do i = 1, ii
                !
                do j = 1, ii
                    vind_gam(1, i, j) = 0.
                    vind_gam(2, i, j) = 0.
                    vind_gam(3, i, j) = 0.
                enddo
                vind_adw(1, i) = 0.0
                vind_adw(2, i) = 0.0
                vind_adw(3, i) = 0.0
                !
                arg = min(20.0, 0.5 * blds * (1.0 - xi(i) / xitip) * sfac)
                ek = exp(-arg)
                ek_adw = -ek * 0.5 * blds * (1.0 - xi(i) / xitip) * sf_adw
                fk = sqrt(1.0 - ek * ek)
                fk_adw = 0.5 / fk * (-2.0 * ek * ek_adw)
                f = atan2(fk, ek) * 2.0 / pi
                f_adw = (ek * fk_adw - fk * ek_adw) / (ek * ek + fk * fk) * 2.0 / pi
                !
                vind_gam(3, i, i) = blds / (4.0 * pi * f * xi(i))
                vind_adw(3, i) = blds / (4.0 * pi * f * xi(i)) * gam(i) * (-f_adw / f)
                vind_gam(1, i, i) = vind_gam(3, i, i) * xi(i) / adw
                vind_adw(1, i) = vind_adw(3, i) * xi(i) / adw&
                        - vind_gam(1, i, i) * gam(i) / adw
                !
                !--- Reverse vz signs
                !         vind_gam(3,i,i) = -vind_gam(3,i,i)
                !         vind_adw(3,i)   = -vind_adw(3,i)
                !cc          va_adw = vind_adw(1,i) - va/adw
                !
            end do
        endif
        !
        return
    end
    ! gradmo



    subroutine helico(imax, ii, nblds, lduct, rake, &
            xi, xv, gam, adw, vind_gam, vind_adw)
        use m_common, only : show_output, pi
        dimension xi(imax), xv(imax), gam(imax)
        dimension vind_adw(3, imax), vind_gam(3, imax, imax)
        !
        logical lduct
        !--------------------------------------------------------------------------
        !     Calculates Swirl-Gamma influence coefficients by a mixed
        !     spectral/finite-difference method.
        !
        !     The modified Bessel equation for each perturbation potential
        !     Fourier harmonic is solved by finite-differencing which gives
        !     a simple tri-diagonal system.
        !
        !     The equation for each harmonic is set up and forward-eliminated
        !     in the same pass, which gives speed and storage efficiency at
        !     some expense of clarity.
        !
        !     Input:
        !       imax         array dimension
        !       ii           number of radial points on blade (circulation stations)
        !       nblds        number of blades
        !       lduct        t for duct outer bc
        !       xi(i)        r/r radial coordinate array
        !       xv(i)        r/r vortex  leg   coordinate array
        !       gam(i)       circulation array
        !       adw          wake advance ratio  v/wr
        !
        !     Output:
        !       vind_gam(i,j)  sensitivity of velocity at i to circulation at j
        !       vind_adw(i)    sensitivity of velocity at i to wake advance ratio
        !
        !        Where vind_xxx(1,i,j) is the axial component
        !              vind_xxx(3,i,j) is the swirl component
        !--------------------------------------------------------------------------
        parameter (idim = 150)
        dimension x(0:idim), ainv(0:idim), csav(0:idim), &
                an_gam(0:idim, 0:idim), an_adw(0:idim)
        dimension sys(4, idim)
        !
        if(idim < imax) stop 'helico: Array overflow:  Increase idim.'
        !
        !pi = 4.0 * atan(1.0)
        !
        !---- number of Fourier harmonics
        nn = 128
        !
        !
        !---- set radial coordinate array for finite-difference solution
        do i = 1, ii
            x(i) = xi(i)
        enddo
        !
        xi0 = xv(1)
        xitip = xv(ii + 1)
        !
        !---- radial coordinate array is also needed outside of blade
        x(0) = 2.0 * xi0 - xi(1)
        x(ii + 1) = 2.0 * xitip - xi(ii)
        !
        if(lduct) then
            iimax = ii + 1
            !
        else
            !------ position of outermost point (at "infinity")
            xinf = 4.0 * xitip
            !
            !------ set first few points beyond tip at mirror locations of those inside tip
            x(ii + 2) = 2.0 * xitip - xi(ii - 1)
            x(ii + 3) = 2.0 * xitip - xi(ii - 2)
            x(ii + 4) = 2.0 * xitip - xi(ii - 3)
            !
            !------ set remaining points with exponential stretching to outermost point
            xfac = (x(ii + 4) - x(ii + 3)) / (x(ii + 3) - x(ii + 2))
            dx = (x(ii + 4) - x(ii + 3)) * xfac
            do i = ii + 5, idim - 1
                x(i) = x(i - 1) + dx
                if(x(i) >= xinf) go to 5
                dx = dx * xfac
            enddo
            if (show_output) write(*, *) 'helico: Local array too small. Increase idim.'
            5      continue
            iimax = i
        endif
        !
        !
        do i = 1, ii
            do j = 1, ii
                vind_gam(1, i, j) = 0.
                vind_gam(2, i, j) = 0.
                vind_gam(3, i, j) = 0.
            enddo
            vind_adw(1, i) = 0.
            vind_adw(2, i) = 0.
            vind_adw(3, i) = 0.
        enddo
        !
        !==== Set up tridiagonal system
        adwinv = 1.0 / adw**2
        qbsq = 0.25 * float(nblds)**2
        !
        do i = 1, iimax - 1
            sys(1, i) = (x(i) + x(i - 1)) / (x(i) - x(i - 1))
            sys(3, i) = (x(i + 1) + x(i)) / (x(i + 1) - x(i))
            sys(2, i) = qbsq * (1.0 / x(i) + x(i) * adwinv) * (x(i + 1) - x(i - 1))
            sys(4, i) = qbsq * (-2.0 * x(i) * adwinv / adw) * (x(i + 1) - x(i - 1))
        enddo
        !
        i = iimax
        if(lduct) then
            sys(1, i) = -1.0
        else
            sys(1, i) = 1.0
        endif
        !
        !==== Loop over all nn harmonics for n = 2,4,6,...
        do n = 2, nn, 2
            rn = float(n)
            !
            !------ set up and factor tridiagonal system for this n
            !
            !------ inner bc:  dAn/dx = 0
            ainv(0) = 1.0
            csav(0) = -1.0
            !                                                2          2
            !------ interior equations:  d[ x dAn/dx ]/dx - n k An  =  n k Gam
            do i = 1, iimax - 1
                b = sys(1, i)
                a = -(sys(1, i) + sys(2, i) * rn**2 + sys(3, i))
                c = sys(3, i)
                !
                !-------- set 1 / (modified diagonal element)
                ainv(i) = 1.0 / (a - b * csav(i - 1))
                !
                !-------- set normalized upper diagonal element for back substitution
                csav(i) = c * ainv(i)
            enddo
            !
            !------ outer bc:  dAn/dx = 0  (duct) ,  or   An = 0  (free tip)
            i = iimax
            b = sys(1, i)
            a = 1.0
            !
            ainv(i) = 1.0 / (a - b * csav(i - 1))
            !
            !
            !====== solve  An, dAn(i)/dGam(j) problems
            !
            !
            !------ set righthand sides
            do i = 0, iimax
                do j = 0, ii
                    an_gam(i, j) = 0.
                enddo
            enddo
            !
            do i = 1, ii
                an_gam(i, 0) = sys(2, i) * rn**2 * gam(i)
                an_gam(i, i) = sys(2, i) * rn**2
            enddo
            !
            !
            !------ back-substitute rhSs
            do i = 1, iimax
                im = i - 1
                b = sys(1, i)
                !
                !-------- eliminate and normalize only up to nonzero elements
                jlast = min(i, ii)
                do j = 0, jlast
                    an_gam(i, j) = (an_gam(i, j) - b * an_gam(im, j)) * ainv(i)
                enddo
            enddo
            !
            do i = iimax - 1, 0, -1
                ip = i + 1
                do j = 0, ii
                    an_gam(i, j) = an_gam(i, j) - csav(i) * an_gam(ip, j)
                enddo
            enddo
            !
            !
            !====== solve dAn(i)/dAdw problem
            !
            !------ set rhs
            an_adw(0) = 0.
            do i = 1, iimax - 1
                an_adw(i) = sys(4, i) * rn**2 * (gam(i) + an_gam(i, 0))
            enddo
            an_adw(iimax) = 0.
            !
            !------ back-substitute rhs
            do i = 1, iimax
                im = i - 1
                b = sys(1, i)
                an_adw(i) = (an_adw(i) - b * an_adw(im)) * ainv(i)
            enddo
            !
            do i = iimax - 1, 0, -1
                ip = i + 1
                an_adw(i) = an_adw(i) - csav(i) * an_adw(ip)
            enddo
            !
            !
            !------ sum potential harmonics to get Swirl-Gamma influence coefficients
            do i = 1, ii
                do j = 1, ii
                    vind_gam(3, i, j) = vind_gam(3, i, j) + an_gam(i, j)
                enddo
                !
                vind_gam(3, i, i) = vind_gam(3, i, i) + 1.0
                vind_adw(3, i) = vind_adw(3, i) + an_adw(i)
            enddo
            !
        end do
        !
        !
        !---- extrapolate the series to the next nn terms
        !-     assuming the known aymptotic behavior (An + Gam) ~ 1/n^2
        !
        if(.not.lduct) then
            !
            fsum = 0.
            do n = nn + 2, 4 * nn, 2
                fsum = fsum + (float(nn) / float(n))**2
            enddo
            !
            do i = 1, ii
                do j = 1, ii
                    vind_gam(3, i, j) = vind_gam(3, i, j) + an_gam(i, j) * fsum
                enddo
                !
                vind_gam(3, i, i) = vind_gam(3, i, i) + 1.0 * fsum
                vind_adw(3, i) = vind_adw(3, i) + an_adw(i) * fsum
            enddo
            !
        endif
        !
        !---- Add on sawtooth self-influence term and scale properly
        do i = 1, ii
            bfac = float(nblds) / (2.0 * pi * x(i))
            !
            vind_gam(3, i, i) = vind_gam(3, i, i) + 0.5
            do j = 1, ii
                vind_gam(3, i, j) = vind_gam(3, i, j) * bfac
            enddo
            vind_adw(3, i) = vind_adw(3, i) * bfac
            !
        enddo
        !
        !---- Define other velocity components vx,vy from vz
        do i = 1, ii
            vsum = 0.0
            do j = 1, ii
                vind_gam(1, i, j) = vind_gam(3, i, j) * xi(i) / adw
                vind_gam(2, i, j) = 0.0
                vsum = vsum + gam(j) * vind_gam(3, i, j)
            enddo
            !c        vind_adw(1,i) = vind_adw(3,i)*xi(i)/adw
            vind_adw(1, i) = vind_adw(3, i) * xi(i) / adw - vsum * xi(i) / adw**2
            vind_adw(2, i) = 0.0
            !
            !--- Reverse vz signs
            !        do j = 1, ii
            !         vind_gam(3,i,j) = -vind_gam(3,i,j)
            !        enddo
            !        vind_adw(3,i) = -vind_adw(3,i)
            !
        enddo
        !
        return
    end
    ! helico





    subroutine filter(q, smlen, n)
        use m_spline, only : trisol
        use m_common, only : show_output
        !-----------------------------------------
        !     Smooths array q.
        !     smlen is the number of points over
        !     which information is smeared.
        !-----------------------------------------
        implicit real (a-h, m, o-z)
        dimension q(n)
        !
        parameter (nmax = 500)
        dimension a(nmax), b(nmax), c(nmax)
        !
        if(n > nmax) then
            if (show_output) write(*, *) 'filter:  Array overflow.  No action taken'
            return
        endif
        !
        !---- set up and solve tridiagonal system for smoothed q
        !
        con = smlen**2
        a(1) = 1.0
        c(1) = 0.
        do i = 2, n - 1
            b(i) = -con
            a(i) = 2.0 * con + 1.0
            c(i) = -con
        end do
        a(n) = 1.0
        b(n) = 0.
        !
        call trisol(a, b, c, q)
        !
        return
    end
    ! filter

end module m_xoper