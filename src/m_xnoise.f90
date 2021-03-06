!*==M_XNOISE.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
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

module m_xnoise
    implicit none
contains


    subroutine ptrace(xobs, yobs, zobs, &
            nblds, ii, xi, dxi, aoc, ch, gam, &
            adv, rad, vel, vso, rho, &
            ntmax, nt, pres, time)
        !------------------------------------------------------------------------
        !     Calculates acoustic pressure p(t) trace
        !     over one blade-passing period.
        !
        !     Input:
        !        xobs    observer location relative to prop
        !        yobs     (x = fwd, y = left, z = up)
        !        zobs
        !        nblds   number of blades
        !        ii      number of radial stations
        !        xi(.)   r/r radial coordinate array
        !        ch(.)   c/r chord distribution
        !        aoc(.)  airfoil area/chord**2 distribution
        !        gam(.)  Gamma/vr circulation distribution
        !        adv     advance ratio  v/wr
        !        rad     tip radius r
        !        vel     freestream speed v
        !        vso     freestream speed of sound
        !        rho     freestream density
        !        nt      number of circumferential prop positions to be sampled
        !                and length of pres,time arrays returned
        !                  (nt=90 works well for most cases)
        !     Output:
        !        pres(i,1)  near-field loading pressure,  i = 1..nt
        !        pres(i,2)  far-field  loading pressure
        !        pres(i,3)  thickness pressure
        !        time(i,)   time coordinate for pres
        !                  (over one blade-passing period, non-uniformly spaced)
        !
        !------------------------------------------------------------------------

        use i_common, only : show_output, pi
        use m_spline, only : seval
        implicit real(A-H, M, O-Z)
        !*** Start of declarations inserted by SPAG
        real ADV, AOC, BPTIME, CH, COST, DXI, FDOTM, FX, FXT, FY, FYT, FZ, FZT, &
                & GAM, MACH, MAX, MAXT, MAXTT, MAY, MAYT
        real MAYTT, MAZ, MAZT, MAZTT, MDOTM, MOR, MORT, MORTT, MR, MRI, MRIT, &
                & MRITT, MRT, MRTT, MTIP, OMEGA, PAVG, PEL, PEL_T, PINT
        real PLFF, PLNF, PRES, PSUM, PTU, R, RAD, RDOTF, RDOTFT, RDOTMT, RHO, &
                & RHOVR, ROBS, ROTIME, RT, RTT, SINT, TAU
        real TDELAY, TEL, TEL0, TELB, TELN, TH, THOBS, TIME, TIME0, TMID, TOB, &
                & TOFF, VEL, VR, VSO, X, XI, XN, XOBS, XT
        real XTT, XX, Y, YN, YOBS, YT, YTT, Z, ZN, ZOBS, ZT, ZTT
        integer I, IB, II, IT, L, NBLDS, NT, NTMAX
        !*** End of declarations inserted by SPAG
        !
        dimension aoc(ii), xi(ii), dxi(ii), ch(ii), gam(ii)
        dimension pres(0:ntmax, 3), time(0:ntmax)
        !
        integer, parameter :: ntx = 160, ix = 40
        dimension pel(0:ntx, ix, 3), tel(0:ntx, ix), pel_t(0:ntx, ix)
        !
        !
        if (ii>ix) stop 'ptrace: Array overflow. ix too small.'
        if (nt>ntx) stop 'ptrace: Array overflow. ntx too small.'
        !
        !---- prop rotational speed
        omega = vel / (adv * rad)
        !
        !---- freestream and tip speed Mach numbers
        mach = vel / vso
        mtip = mach / adv
        !
        rhovr = rho * vel * rad
        vr = vel * rad
        !
        !---- set distance to observer and average acoustic delay time
        robs = sqrt(xobs**2 + yobs**2 + zobs**2)
        tdelay = robs / vso
        !
        thobs = atan2(-yobs, zobs)
        !
        !---- rotate one blade through 360 degrees in nt steps
        do it = 0, nt
            !
            !------ set rotation angle,  th=0 is for blade vertical (along z direction)
            th = 2.0 * pi * float(it) / float(nt) + thobs + 0.5 * pi
            !
            sint = sin(th)
            cost = cos(th)
            !
            !------ set retarded time
            tau = th / omega
            !
            !------ go over blade elements at current rotation angle
            do i = 1, ii
                !
                xx = xi(i) / adv
                !
                !-------- components and modulus of vector from blade element to observer
                x = xobs
                y = yobs + xi(i) * rad * sint
                z = zobs - xi(i) * rad * cost
                r = sqrt(x * x + y * y + z * z)
                !
                !-------- unit vector to observer
                xn = x / r
                yn = y / r
                zn = z / r
                !
                !-------- time derivative of vector from blade element to observer
                xt = -vel
                yt = vel * xx * cost
                zt = vel * xx * sint
                rt = (x * xt + y * yt + z * zt) / r
                !
                !-------- 2nd time derivative of vector from blade element to observer
                xtt = 0.
                ytt = -vel * xx * sint * omega
                ztt = vel * xx * cost * omega
                rtt = (x * xtt + y * ytt + z * ztt) / r + (xt * xt + yt * yt + zt * zt) / r - rt * rt / r
                !
                !-------- Mach number components of blade element relative to still air
                max = mach
                may = -mach * xx * cost
                maz = -mach * xx * sint
                !
                !-------- time derivatives of Mach number components
                maxt = 0.
                mayt = mach * xx * sint * omega
                mazt = -mach * xx * cost * omega
                !
                !-------- 2nd time derivatives of Mach number components
                maxtt = 0.
                maytt = mach * xx * cost * omega**2
                maztt = mach * xx * sint * omega**2
                !
                !-------- components of lift force on air by blade element
                fx = -rhovr * gam(i) * xx * dxi(i) * vr
                fy = -rhovr * gam(i) * cost * dxi(i) * vr
                fz = -rhovr * gam(i) * sint * dxi(i) * vr
                !
                !-------- time derivative of lift force
                fxt = 0.
                fyt = rhovr * gam(i) * sint * dxi(i) * vr * omega
                fzt = -rhovr * gam(i) * cost * dxi(i) * vr * omega
                !
                !-------- Mach number component along blade element --> observer direction
                mr = (x * max + y * may + z * maz) / r
                !
                if (mr>=1.0) then
                    if (show_output) write (*, 99001) mr, xi(i), (th * 180.0 / pi)
                    99001              format (/' warning.  Relative approach Mach number =', f6.3, &
                            &'  at r/r =', f6.3, '    theta =', f6.1, ' deg.')
                    mr = 0.995
                endif
                !
                mri = 1.0 / (1.0 - mr)
                !
                !-------- assorted time derivatives
                mrt = (xt * max + yt * may + zt * maz) / r&
                        + (x * maxt + y * mayt + z * mazt) / r&
                        - rt * mr / r
                mrit = mri**2 * mrt
                !
                mrtt = (xtt * max + ytt * may + ztt * maz) / r&
                        + 2.0 * (xt * maxt + yt * mayt + zt * mazt) / r&
                        + (x * maxtt + y * maytt + z * maztt) / r&
                        - 2.0 * rt * mrt / r - rtt * mr / r
                mritt = 2.0 * mri * mrit * mrt + mri**2 * mrtt
                !
                !-------- various dot products
                rdotf = xn * fx + yn * fy + zn * fz
                rdotft = xn * fxt + yn * fyt + zn * fzt
                rdotmt = xn * maxt + yn * mayt + zn * mazt
                !
                mdotm = max * max + may * may + maz * maz
                fdotm = fx * max + fy * may + fz * maz
                !
                !-------- set far-field and near-field pressures due to lift
                plff = (rdotft / vso + rdotf * mri * rdotmt / vso) * mri**2 / r
                plnf = (rdotf * (1.0 - mdotm) * mri - fdotm) * mri**2 / r**2
                !
                !-------- set unit pressure due to thickness
                mor = mri / r
                mort = mrit / r - rt * mor / r
                mortt = mritt / r - rtt * mor / r - 2.0 * rt * mort / r
                ptu = mri * (mrit * mort + mri * mortt)
                !
                !-------- set pressure due to thickness
                pt = ptu * rho * aoc(i) * ch(i)**2 * dxi(i) * rad**3
                !
                !-------- set entire pressure due to blade element at retarded time
                pel(it, i, 1) = plff / (4.0 * pi)
                pel(it, i, 2) = plnf / (4.0 * pi)
                pel(it, i, 3) = pt / (4.0 * pi)
                !
                !-------- set observer time at which he will see the pressure signal
                !         (subtract off average delay time to make observer time near zero)
                tob = tau + r / vso
                tel(it, i) = tob - tdelay
                !
            enddo
        enddo
        !
        !
        !---- make sure pressure is exactly periodic
        do i = 1, ii
            pel(nt, i, 1) = pel(0, i, 1)
            pel(nt, i, 2) = pel(0, i, 2)
            pel(nt, i, 3) = pel(0, i, 3)
        enddo
        !
        !
        !---- full-rotation time
        rotime = 2.0 * pi / omega
        !
        !---- blade-passing time
        bptime = rotime / float(nblds)
        !
        !---- set time array over one blade-passing time
        !###
        !c      do it=1, nt
        !c        time(it) = bptime * float(it)/float(nt)
        !c      enddo

        i = ii
        tmid = tel(nt / 2, i)
        do it = 0, nt
            time(it) = tmid + (tel(it, i) - tmid) / float(nblds)
        enddo
        !
        !---- go over pressure components
        do l = 1, 3
            !------ periodic-spline p(t) for each blade element
            do i = 1, ii
                call psplin(pel(0, i, l), pel_t(0, i), tel(0, i), nt + 1)
            enddo
            !
            !------ set total acoustic p(t) by adding up all blade elements from all blades
            do it = 1, nt
                psum = 0.
                !
                !-------- go over all radial stations
                do i = 1, ii
                    tel0 = tel(0, i)
                    teln = tel(nt, i)
                    !
                    !---------- go over all blades at this radial station
                    do ib = 1, nblds
                        !------------ time at which current blade passes the current blade 1 angle
                        telb = time(it) + bptime * float(ib - 1)
                        !
                        !------------ remove whole multiples of blade period to get into spline range
                        toff = tel0 + amod((telb - tel0), (teln - tel0))
                        if (toff<tel0) toff = toff + (teln - tel0)
                        !
                        if (toff<tel0.or.toff>teln) then
                            if (show_output) write (*, *)                       &
                                    &'? ptrace: Time out of spline range.'
                            if (show_output) write (*, *) 't   t0   tn', toff, &
                                    & tel0, teln
                        endif
                        !
                        ! todo: test this
                        ! psum = psum + seval_old(toff, pel(0, i, l), pel_t(0, i), tel(0, i), nt + 1)
                        psum = psum + seval(toff, pel(0:nt, i, l), pel_t(0:nt, i), tel(0:nt, i))
                    enddo
                enddo
                !
                !-------- set total pressure signal at current observer time
                pres(it, l) = psum
                !
            enddo
            !
            !------ make sure pressure is exactly periodic
            pres(0, l) = pres(nt, l)
        enddo
        !
        time0 = time(0)
        do it = 0, nt
            time(it) = time(it) - time0
        enddo
        !
        !---- subtract off average pressure
        do l = 1, 3
            pint = 0.
            do it = 1, nt
                pint = pint + (pres(it, l) + pres(it - 1, l)) * (time(it) - time(it - 1))
            enddo
            pavg = pint * 0.5 / (time(nt) - time(0))
            !
            do it = 0, nt
                pres(it, l) = pres(it, l) - pavg
            enddo
        enddo
        !
    end


    subroutine sft(y, t, n, fampl, phase, nf)
        use i_common, only : pi
        use i_common, only : pi
        !*** Start of declarations inserted by SPAG
        real COST, CSUM, DT, FAMPL, OMEGA, PHASE, RK, SINT, SSUM, T, TK, Y
        integer I, IO, IP, K, N, NF
        !*** End of declarations inserted by SPAG
        dimension y(n), t(n)
        dimension fampl(nf), phase(nf)
        !---------------------------------------------------
        !     Calculates Slow Fourier Transform of periodic
        !     input function y(t) with period t(n+1)-t(1).
        !     Hence, y(1) and y(n+1) should be equal.
        !     c(k) is the complex amplitude of the k'th
        !     multiple of the fundamental harmonic.
        !     The first nf harmonics are calculated.
        !---------------------------------------------------
        dimension sint(361), cost(361)
        !
        if (n + 1>361) stop 'sft: Array overflow'
        !
        ! pi = 4.0 * atan(1.0)
        !
        omega = 2.0 * pi / (t(n + 1) - t(1))
        !
        do k = 1, nf
            rk = float(k)
            !
            do i = 1, n
                tk = omega * rk * t(i)
                sint(i) = sin(tk)
                cost(i) = cos(tk)
            enddo
            sint(n + 1) = sint(1)
            cost(n + 1) = cost(1)
            !
            ssum = 0.
            csum = 0.
            do io = 1, n
                ip = io + 1
                dt = t(ip) - t(io)
                ssum = ssum + 0.5 * (sint(io) * y(io) + sint(ip) * y(ip)) * dt
                csum = csum + 0.5 * (cost(io) * y(io) + cost(ip) * y(ip)) * dt
            enddo
            !
            fampl(k) = sqrt(ssum**2 + csum**2) * omega / pi
            phase(k) = atan2(ssum, csum)
        enddo
        !
    end
    ! sft


    subroutine psplin(x, xp, s, ii)
        !*** Start of declarations inserted by SPAG
        real A, B, C, DSMI, DSPI, DXM, DXP, S, X, XP
        integer I, II
        !*** End of declarations inserted by SPAG
        !
        dimension x(ii), xp(ii), s(ii)
        dimension a(480), b(480), c(480)
        !
        if (ii>480) stop 'psplin: Array overflow'
        if (x(ii)/=x(1)) stop 'psplin: Data not periodic'
        !
        do i = 1, ii - 1
            !
            !------ Periodic point
            if (i==1) then
                dsmi = 1.0 / (s(ii) - s(ii - 1))
                dxm = x(ii) - x(ii - 1)
                dspi = 1.0 / (s(i + 1) - s(i))
                dxp = x(i + 1) - x(i)
                !
                !------ Interior points
            else
                dsmi = 1.0 / (s(i) - s(i - 1))
                dxm = x(i) - x(i - 1)
                dspi = 1.0 / (s(i + 1) - s(i))
                dxp = x(i + 1) - x(i)
            endif
            !
            b(i) = dsmi
            a(i) = 2.0 * (dsmi + dspi)
            c(i) = dspi
            xp(i) = 3.0 * (dxp * dspi**2 + dxm * dsmi**2)
            !
        enddo
        !
        call ptriso(a, b, c, xp, ii - 1)
        !
        xp(ii) = xp(1)
        !
    end


    subroutine ptriso(a, b, c, d, kk)
        !*** Start of declarations inserted by SPAG
        real A, AINV, B, C, D
        integer K, KK, KM
        !*** End of declarations inserted by SPAG
        !
        dimension a(kk), b(kk), c(kk), d(kk)
        !
        do k = 2, kk
            km = k - 1
            ainv = 1.0 / a(km)
            c(km) = c(km) * ainv
            d(km) = d(km) * ainv
            b(km) = b(km) * ainv
            a(k) = a(k) - b(k) * c(km)
            d(k) = d(k) - b(k) * d(km)
            if (k<kk) then
                b(k) = -b(k) * b(km)
            else
                a(k) = a(k) - b(k) * b(km)
            endif
        enddo
        !
        c(kk) = c(kk) / a(kk)
        d(kk) = d(kk) / a(kk)
        !
        do k = kk, 2, -1
            km = k - 1
            d(km) = d(km) - c(km) * d(k) - b(km) * d(kk)
            c(km) = -c(km) * c(k) - b(km) * c(kk)
        enddo
        !
        d(1) = d(1) / (1.0 + c(1))
        !
        do k = 2, kk
            d(k) = d(k) - c(k) * d(1)
        enddo
        !
    end


    subroutine dbfoot(nblds, ii, xi, dxi, aoc, ch, gam, &
            adv, rad, vel, vso, rho, &
            galt, dclimb, unitl, nt, &
            nxdim, nydim, nx, ny, x, y, d)
        use i_common, only : show_output
        !*** Start of declarations inserted by SPAG
        real ADV, AOC, CH, COSC, D, DCLIMB, DELT, DELY, DTR, DXI, GALT, GAM, &
                & PAVG, PCOMP, PRES, PRMS, RAD, RHO, SINC, TIME
        real UNITL, VEL, VSO, X, XG, XI, XOBS, Y, YG, YOBS, ZG, ZOBS
        integer I, II, IT, J, J0, JPOS, NBLDS, NT, NTX, NX, NXDIM, NY, NYDIM
        !*** End of declarations inserted by SPAG
        !--------------------------------------------------------
        !     Calculates db noise levels on a ground plane grid.
        !
        !     Input:
        !        nblds   number of blades
        !        ii      number of radial stations
        !        xi(.)   r/r radial coordinate array
        !        ch(.)   c/r chord distribution
        !        aoc(.)  airfoil area/chord**2 distribution
        !        gam(.)  Gamma/vr circulation distribution
        !        adv     advance ratio  v/wr
        !        rad     tip radius r
        !        vel     freestream speed v
        !        vso     freestream speed of sound
        !        rho     freestream density
        !        galt    aircraft altitude above ground plane
        !        dclimb  aircraft climb angle (deg)
        !        unitl   length unit of galt,x,y, in meters
        !        nt      number of circumferential prop positions to be sampled
        !
        !        nxdim,nydim   grid array dimensions
        !        nx,ny         grid size
        !        x(..)         ground-plane grid where db is to be calculated
        !        y(..)          "
        !
        !     Output:
        !        d(..)         db sound level at ground plane
        !
        !--------------------------------------------------------
        dimension aoc(ii), xi(ii), dxi(ii), ch(ii), gam(ii)
        !
        dimension d(nxdim, nydim)
        dimension x(nxdim, nydim)
        dimension y(nxdim, nydim)
        !
        !---- local arrays for calculating p(t) trace
        parameter (ntx = 180)
        dimension pcomp(0:ntx, 3), pres(0:ntx), time(0:ntx)
        !
        !---- degrees-to-radians conversion factor
        dtr = atan(1.0) / 45.0
        !
        cosc = cos(dclimb * dtr)
        sinc = sin(dclimb * dtr)
        !
        !---- find j index of y=0 line
        dely = abs(y(1, ny) - y(1, 1))
        do j0 = 1, ny
            if (y(1, j0)>-0.0001 * dely) goto 100
        enddo
        j0 = 1
        !
        100   do i = 1, nx
            if (show_output) write (*, 99001) i, nx
            99001      format (5x, i3, ' /', i3)
            !
            do j = j0, ny
                !-------- set observer position assuming airplane is level
                xg = x(i, j) / unitl
                yg = y(i, j) / unitl
                zg = -galt / unitl
                !
                !-------- rotate point through climb angle about y axis
                xobs = cosc * xg + sinc * zg
                yobs = yg
                zobs = -sinc * xg + cosc * zg
                !
                !-------- calculate p(t) pressure signature
                call ptrace(xobs, yobs, zobs, nblds, ii, xi, dxi, aoc, ch, gam, adv, rad, &
                        & vel, vso, rho, ntx, nt, pcomp, time)
                do it = 0, nt
                    pres(it) = pcomp(it, 1) + pcomp(it, 2) + pcomp(it, 3)
                enddo
                !
                !-------- set rms pressure signature and corresponding db level
                prms = 0.
                do it = 1, nt
                    delt = time(it) - time(it - 1)
                    pavg = (pres(it) + pres(it - 1)) * 0.5
                    prms = prms + pavg**2 * delt
                enddo
                prms = sqrt(prms / (time(nt) - time(0)))
                !
                d(i, j) = 20.0 * alog10(prms / 20.0E-6)
            enddo
            !
            !------ set values for negative y by symmetry
            do j = 1, j0 - 1
                jpos = 2 * j0 - j - 1 + mod(ny, 2)
                d(i, j) = d(i, jpos)
            enddo
            !
        enddo
        !
    end
end
