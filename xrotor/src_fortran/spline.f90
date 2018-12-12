!***********************************************************************
!    Module:  spline.f
! 
!    Copyright (C) 2011 Mark Drela 
! 
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation; either version 2 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program; if not, write to the Free Software
!    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!***********************************************************************

module mod_spline
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

        if(n == 1) then
            xs(1) = 0.
            return
        end if

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
        end do

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

        if (n == 1) then
            xs(1) = 0.
            return
        end if

        allocate (a(n), b(n), c(n))

        do i = 2, n - 1
            dsm = s(i) - s(i - 1)
            dsp = s(i + 1) - s(i)
            b(i) = dsp
            a(i) = 2.0 * (dsm + dsp)
            c(i) = dsm
            xs(i) = 3.0 * ((x(i + 1) - x(i)) * dsm / dsp + (x(i) - x(i - 1)) * dsp / dsm)
        end do

        !---- set left end condition
        if(xs1 == 999.0) then
            !----- zero 2nd derivative
            a(1) = 2.0
            c(1) = 1.0
            xs(1) = 3.0 * (x(2) - x(1)) / (s(2) - s(1))
        else if(xs1 == -999.0) then
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
        if(xs2 == 999.0) then
            !----- zero 2nd derivative
            b(n) = 1.0
            a(n) = 2.0
            xs(n) = 3.0 * (x(n) - x(n - 1)) / (s(n) - s(n - 1))
        else if(xs2 == -999.0) then
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
        if(n == 2 .and. xs1 == -999.0 .and. xs2 == -999.0) then
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
        !     A simple averaging of adjacent segment slopes     |
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

        if (n == 1) then
            xs(1) = 0.
            return
        end if

        lend = .true.
        do i = 1, n - 1
            ds = s(i + 1) - s(i)
            if (ds == 0.) then
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
        end do
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
        end do

        d(kk) = d(kk) / a(kk)

        do k = kk - 1, 1, -1
            d(k) = d(k) - c(k) * d(k + 1)
        end do
    end
    ! trisol
end module mod_spline


FUNCTION SEVAL(SS, X, XS, S, N)
    IMPLICIT REAL (A-H, M, O-Z)
    DIMENSION X(N), XS(N), S(N)
    !--------------------------------------------------
    !     Calculates X(SS)                             |
    !     XS array must have been calculated by SPLINE |
    !--------------------------------------------------
    IF(N == 1) THEN
        SEVAL = X(1)
        RETURN
    ENDIF
    !
    ILOW = 1
    I = N
    !
    10 IF(I - ILOW <= 1) GO TO 11
    !
    IMID = (I + ILOW) / 2
    IF(SS < S(IMID)) THEN
        I = IMID
    ELSE
        ILOW = IMID
    ENDIF
    GO TO 10
    !
    11 DS = S(I) - S(I - 1)
    T = (SS - S(I - 1)) / DS
    CX1 = DS * XS(I - 1) - X(I) + X(I - 1)
    CX2 = DS * XS(I) - X(I) + X(I - 1)
    SEVAL = T * X(I) + (1.0 - T) * X(I - 1) + (T - T * T) * ((1.0 - T) * CX1 - T * CX2)
    RETURN
END
! SEVAL

FUNCTION DEVAL(SS, X, XS, S, N)
    IMPLICIT REAL (A-H, M, O-Z)
    DIMENSION X(N), XS(N), S(N)
    !--------------------------------------------------
    !     Calculates dX/dS(SS)                         |
    !     XS array must have been calculated by SPLINE |
    !--------------------------------------------------
    IF(N == 1) THEN
        DEVAL = XS(1)
        RETURN
    ENDIF
    !
    ILOW = 1
    I = N
    !
    10 IF(I - ILOW <= 1) GO TO 11
    !
    IMID = (I + ILOW) / 2
    IF(SS < S(IMID)) THEN
        I = IMID
    ELSE
        ILOW = IMID
    ENDIF
    GO TO 10
    !
    11 DS = S(I) - S(I - 1)
    T = (SS - S(I - 1)) / DS
    CX1 = DS * XS(I - 1) - X(I) + X(I - 1)
    CX2 = DS * XS(I) - X(I) + X(I - 1)
    DEVAL = X(I) - X(I - 1) + (1. - 4.0 * T + 3.0 * T * T) * CX1 + T * (3.0 * T - 2.) * CX2
    DEVAL = DEVAL / DS
    RETURN
END
! DEVAL


SUBROUTINE SEGSPL(X, XS, S, N)
    use mod_spline
    IMPLICIT REAL (A-H, M, O-Z)
    DIMENSION X(N), XS(N), S(N)
    !-----------------------------------------------
    !     Splines X(S) array just like SPLINE,      |
    !     but allows derivative discontinuities     |
    !     at segment joints.  Segment joints are    |
    !     defined by identical successive S values. |
    !-----------------------------------------------
    IF(N == 1) THEN
        XS(1) = 0.
        RETURN
    ENDIF
    !
    IF(S(1) == S(2)) STOP 'SEGSPL:  First input point duplicated'
    IF(S(N) == S(N - 1)) STOP 'SEGSPL:  Last  input point duplicated'
    !
    ISEG0 = 1
    do ISEG = 2, N - 2
        IF(S(ISEG) == S(ISEG + 1)) THEN
            NSEG = ISEG - ISEG0 + 1
            xs(iseg0:iseg0+nseg) = splind(s(iseg0:iseg0+nseg), x(iseg0:iseg0+nseg), -999.0, -999.0)
            ISEG0 = ISEG + 1
        ENDIF
    end do
    !
    NSEG = N - ISEG0 + 1
    xs(iseg0:iseg0+nseg) = splind(s(iseg0:iseg0+nseg), x(iseg0:iseg0+nseg), -999.0, -999.0)
    !
    RETURN
END
! SEGSPL