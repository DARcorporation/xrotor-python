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

subroutine gauss(nsiz, nn, z, r, nrhs)
    !     *******************************************************
    !     *                                                     *
    !     *   Solves general Nxn system in n unknowns           *
    !     *    with arbitrary number (nrhs) of righthand sides. *
    !     *   Assumes system is invertible...                   *
    !     *    ...if it isn't, a divide by zero will result.    *
    !     *                                                     *
    !     *   z is the coefficient matrix...                    *
    !     *     ...destroyed during solution process.           *
    !     *   r is the righthand side(s)...                     *
    !     *     ...replaced by the solution vector(s).          *
    !     *                                                     *
    !     *                              Mark Drela  1984       *
    !     *******************************************************
    !
    dimension z(nsiz, nsiz), r(nsiz, nrhs)
    !
    do np = 1, nn - 1
        np1 = np + 1
        !
        !------ find max pivot index nx
        nx = np
        do n = np1, nn
            if(abs(z(n, np)) - abs(z(nx, np))) 11, 11, 111
            111      nx = n
        11 end do
        !
        pivot = 1.0 / z(nx, np)
        !
        !------ switch pivots
        z(nx, np) = z(np, np)
        !
        !------ switch rows & normalize pivot row
        do l = np1, nn
            temp = z(nx, l) * pivot
            z(nx, l) = z(np, l)
            z(np, l) = temp
        end do
        !
        do l = 1, nrhs
            temp = r(nx, l) * pivot
            r(nx, l) = r(np, l)
            r(np, l) = temp
        end do
        !
        !------ forward eliminate everything
        do k = np1, nn
            ztmp = z(k, np)
            !
            !          if(ztmp == 0.0) go to 15
            !
            do l = np1, nn
                z(k, l) = z(k, l) - ztmp * z(np, l)
            end do
            do l = 1, nrhs
                r(k, l) = r(k, l) - ztmp * r(np, l)
            end do
        end do
        !
    end do
    !
    !---- solve for last row
    do l = 1, nrhs
        r(nn, l) = r(nn, l) / z(nn, nn)
    end do
    !
    !---- back substitute everything
    do np = nn - 1, 1, -1
        np1 = np + 1
        do l = 1, nrhs
            do k = np1, nn
                r(np, l) = r(np, l) - z(np, k) * r(k, l)
            end do
        end do
    end do
    !
    return
end
! gauss