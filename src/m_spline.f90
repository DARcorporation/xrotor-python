!*==M_SPLINE.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
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

module m_spline
    implicit none
    public
contains
    function spline(s, x) result(xs)
        real, intent(in) :: s(:), x(:)
        real, allocatable :: xs(:), a(:), b(:), c(:)

        integer :: i, n
        real :: dsm, dsp
        !-------------------------------------------------------
        !     Calculates spline coefficients for x(s).          |
        !     Zero 2nd derivative end conditions are used.      |
        !     To evaluate the spline at some value of s,        |
        !     use seval and/or deval.                           |
        !                                                       |
        !     s        independent variable array (input)       |
        !     x        dependent variable array   (input)       |
        !     xs       dx/ds array                (calculated)  |
        !     n        number of points           (input)       |
        !                                                       |
        !-------------------------------------------------------
        n = min(size(s), size(x))
        allocate (xs(n))

        if (n==1) then
            xs(1) = 0.
            return
        endif

        allocate (a(n), b(n), c(n))

        b(1) = 0.
        c(n) = 0.

        do i = 2, n - 1
            dsm = s(i) - s(i - 1)
            dsp = s(i + 1) - s(i)
            b(i) = dsp
            a(i) = 2.0 * (dsm + dsp)
            c(i) = dsm
            xs(i) = 3.0 * ((x(i + 1) - x(i)) * dsm / dsp + (x(i) - x(i - 1)) * dsp / dsm)
        enddo

        !---- set zero second derivative end conditions
        a(1) = 2.0
        c(1) = 1.0
        xs(1) = 3.0 * (x(2) - x(1)) / (s(2) - s(1))
        b(n) = 1.0
        a(n) = 2.0
        xs(n) = 3.0 * (x(n) - x(n - 1)) / (s(n) - s(n - 1))

        !---- solve for derivative array xs
        call trisol(a, b, c, xs)
    end
    ! spline

    function splind(s, x, xs1, xs2) result(xs)
        real, intent(in) :: s(:), x(:), xs1, xs2
        real, allocatable :: xs(:), a(:), b(:), c(:)

        integer :: i, n
        real :: dsm, dsp
        !-------------------------------------------------------
        !     Calculates spline coefficients for x(s).          |
        !     Specified 1st derivative and/or zero 2nd          |
        !     or 3rd derivative end conditions can be used.     |
        !     To evaluate the spline at some value of s,        |
        !     use seval and/or deval.                           |
        !                                                       |
        !     s        independent variable array (input)       |
        !     x        dependent variable array   (input)       |
        !     xs       dx/ds array                (calculated)  |
        !     n        number of points           (input)       |
        !     xs1,xs2  endpoint derivatives       (input)       |
        !              If =  999.0, use zero 2nd derivative     |
        !              If = -999.0, use zero 3rd derivative     |
        !                                                       |
        !-------------------------------------------------------
        n = min(size(s), size(x))
        allocate (xs(n))

        if (n==1) then
            xs(1) = 0.
            return
        endif

        allocate (a(n), b(n), c(n))

        do i = 2, n - 1
            dsm = s(i) - s(i - 1)
            dsp = s(i + 1) - s(i)
            b(i) = dsp
            a(i) = 2.0 * (dsm + dsp)
            c(i) = dsm
            xs(i) = 3.0 * ((x(i + 1) - x(i)) * dsm / dsp + (x(i) - x(i - 1)) * dsp / dsm)
        enddo

        !---- set left end condition
        if (xs1==999.0) then
            !----- zero 2nd derivative
            a(1) = 2.0
            c(1) = 1.0
            xs(1) = 3.0 * (x(2) - x(1)) / (s(2) - s(1))
        elseif (xs1==-999.0) then
            !----- set zero 3rd derivative
            a(1) = 1.0
            c(1) = 1.0
            xs(1) = 2.0 * (x(2) - x(1)) / (s(2) - s(1))
        else
            !----- specified 1st derivative
            a(1) = 1.0
            c(1) = 0.
            xs(1) = xs1
        endif

        !---- set right end condition
        if (xs2==999.0) then
            !----- zero 2nd derivative
            b(n) = 1.0
            a(n) = 2.0
            xs(n) = 3.0 * (x(n) - x(n - 1)) / (s(n) - s(n - 1))
        elseif (xs2==-999.0) then
            !----- zero 3rd derivative
            b(n) = 1.0
            a(n) = 1.0
            xs(n) = 2.0 * (x(n) - x(n - 1)) / (s(n) - s(n - 1))
        else
            !----- specified 1st derivative
            a(n) = 1.0
            b(n) = 0.
            xs(n) = xs2
        endif

        !---- if only two points, cannot have zero third derivatives at both ends
        if (n==2.and.xs1==-999.0.and.xs2==-999.0) then
            !----- set zero 2nd derivative at right end (left end will also be zero)
            b(n) = 1.0
            a(n) = 2.0
            xs(n) = 3.0 * (x(n) - x(n - 1)) / (s(n) - s(n - 1))
        endif

        !---- solve for derivative array xs
        call trisol(a, b, c, xs)
    end
    ! splind

    function splina(s, x) result(xs)
        real, intent(in) :: s(:), x(:)
        real, allocatable :: xs(:)

        logical :: lend
        integer :: i, n
        real :: ds, dx, xs1, xs2
        !-------------------------------------------------------
        !     Calculates spline coefficients for x(s).          |
        !     a simple averaging of adjacent segment slopes     |
        !     is used to achieve non-oscillatory curve          |
        !     End conditions are set by end segment slope       |
        !     To evaluate the spline at some value of s,        |
        !     use seval and/or deval.                           |
        !                                                       |
        !     s        independent variable array (input)       |
        !     x        dependent variable array   (input)       |
        !     xs       dx/ds array                (calculated)  |
        !     n        number of points           (input)       |
        !                                                       |
        !-------------------------------------------------------
        n = min(size(s), size(x))
        allocate (xs(n))

        if (n==1) then
            xs(1) = 0.
            return
        endif

        lend = .true.
        do i = 1, n - 1
            ds = s(i + 1) - s(i)
            if (ds==0.) then
                xs(i) = xs1
                lend = .true.
            else
                dx = x(i + 1) - x(i)
                xs2 = dx / ds
                if (lend) then
                    xs(i) = xs2
                    lend = .false.
                else
                    xs(i) = 0.5 * (xs1 + xs2)
                endif
            endif
            xs1 = xs2
        enddo
        xs(n) = xs1
    end
    ! splina

    subroutine trisol(a, b, c, d)
        real, intent(inout) :: a(:), b(:), c(:), d(:)
        integer :: k, kk, km
        !-----------------------------------------
        !     Solves kk long, tri-diagonal system |
        !                                         |
        !             a c          d              |
        !             b a c        d              |
        !               b a .      .              |
        !                 . . c    .              |
        !                   b a    d              |
        !                                         |
        !     The righthand side d is replaced by |
        !     the solution.  a, c are destroyed.  |
        !-----------------------------------------
        kk = min(size(a), size(b), size(c), size(d))

        do k = 2, kk
            km = k - 1
            c(km) = c(km) / a(km)
            d(km) = d(km) / a(km)
            a(k) = a(k) - b(k) * c(km)
            d(k) = d(k) - b(k) * d(km)
        enddo

        d(kk) = d(kk) / a(kk)

        do k = kk - 1, 1, -1
            d(k) = d(k) - c(k) * d(k + 1)
        enddo
    end
    ! trisol

    real function seval(ss, x, xs, s)result(val)
        real, intent(in) :: ss, x(:), xs(:), s(:)
        integer :: i, ilow, imid, n
        real :: ds, t, cx1, cx2
        !--------------------------------------------------
        !     Calculates x(ss)                             |
        !     xs array must have been calculated by spline |
        !--------------------------------------------------
        n = min(size(x), size(xs), size(s))
        if (n==1) then
            val = x(1)
            return
        endif

        ilow = 1
        i = n

        do while (i - ilow>1)
            imid = (i + ilow) / 2
            if (ss<s(imid)) then
                i = imid
            else
                ilow = imid
            endif
        enddo

        ds = s(i) - s(i - 1)
        t = (ss - s(i - 1)) / ds
        cx1 = ds * xs(i - 1) - x(i) + x(i - 1)
        cx2 = ds * xs(i) - x(i) + x(i - 1)
        val = t * x(i) + (1.0 - t) * x(i - 1) + (t - t * t) * ((1.0 - t) * cx1 - t * cx2)
    end
    ! seval

    function deval(ss, x, xs, s) result(val)
        real, intent(in) :: ss, x(:), xs(:), s(:)
        integer :: i, ilow, imid, n
        real :: ds, t, cx1, cx2, val
        !--------------------------------------------------
        !     Calculates dx/ds(ss)                         |
        !     xs array must have been calculated by spline |
        !--------------------------------------------------
        n = min(size(x), size(xs), size(s))
        if (n==1) then
            val = x(1)
            return
        endif

        ilow = 1
        i = n

        do while (i - ilow>1)
            imid = (i + ilow) / 2
            if (ss<s(imid)) then
                i = imid
            else
                ilow = imid
            endif
        enddo

        ds = s(i) - s(i - 1)
        t = (ss - s(i - 1)) / ds
        cx1 = ds * xs(i - 1) - x(i) + x(i - 1)
        cx2 = ds * xs(i) - x(i) + x(i - 1)
        val = x(i) - x(i - 1) + (1. - 4.0 * t + 3.0 * t * t) * cx1 + t * (3.0 * t - 2.) * cx2
        val = val / ds
    end
    ! deval

    function segspl(s, x) result(xs)
        real, intent(in) :: s(:), x(:)
        real, allocatable :: xs(:)
        integer :: iseg0, iseg, nseg, n
        !-----------------------------------------------
        !     Splines x(s) array just like spline,      |
        !     but allows derivative discontinuities     |
        !     at segment joints.  Segment joints are    |
        !     defined by identical successive s values. |
        !-----------------------------------------------
        n = min(size(s), size(x))
        if (n==1) then
            xs(1) = 0.
            return
        endif

        if (s(1)==s(2)) stop 'segspl:  First input point duplicated'
        if (s(n)==s(n - 1)) stop 'segspl:  Last  input point duplicated'

        iseg0 = 1
        do iseg = 2, n - 2
            if (s(iseg)==s(iseg + 1)) then
                nseg = iseg - iseg0 + 1
                xs(iseg0:iseg0 + nseg) = splind(s(iseg0:iseg0 + nseg), x(iseg0:iseg0 + nseg), -999.0, -999.0)
                iseg0 = iseg + 1
            endif
        enddo

        nseg = n - iseg0 + 1
        xs(iseg0:iseg0 + nseg) = splind(s(iseg0:iseg0 + nseg), x(iseg0:iseg0 + nseg), -999.0, -999.0)
    end
    ! segspl
end
