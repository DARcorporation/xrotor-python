!***********************************************************************
!    Module:  xutils.f
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

SUBROUTINE GAUSS(NSIZ, NN, Z, R, NRHS)
    !     *******************************************************
    !     *                                                     *
    !     *   Solves general NxN system in N unknowns           *
    !     *    with arbitrary number (NRHS) of righthand sides. *
    !     *   Assumes system is invertible...                   *
    !     *    ...if it isn't, a divide by zero will result.    *
    !     *                                                     *
    !     *   Z is the coefficient matrix...                    *
    !     *     ...destroyed during solution process.           *
    !     *   R is the righthand side(s)...                     *
    !     *     ...replaced by the solution vector(s).          *
    !     *                                                     *
    !     *                              Mark Drela  1984       *
    !     *******************************************************
    !
    DIMENSION Z(NSIZ, NSIZ), R(NSIZ, NRHS)
    !
    do NP = 1, NN - 1
        NP1 = NP + 1
        !
        !------ find max pivot index NX
        NX = NP
        do N = NP1, NN
            IF(ABS(Z(N, NP)) - ABS(Z(NX, NP))) 11, 11, 111
            111      NX = N
        11 end do
        !
        PIVOT = 1.0 / Z(NX, NP)
        !
        !------ switch pivots
        Z(NX, NP) = Z(NP, NP)
        !
        !------ switch rows & normalize pivot row
        do L = NP1, NN
            TEMP = Z(NX, L) * PIVOT
            Z(NX, L) = Z(NP, L)
            Z(NP, L) = TEMP
        end do
        !
        do L = 1, NRHS
            TEMP = R(NX, L) * PIVOT
            R(NX, L) = R(NP, L)
            R(NP, L) = TEMP
        end do
        !
        !------ forward eliminate everything
        do K = NP1, NN
            ZTMP = Z(K, NP)
            !
            !          IF(ZTMP == 0.0) GO TO 15
            !
            do L = NP1, NN
                Z(K, L) = Z(K, L) - ZTMP * Z(NP, L)
            end do
            do L = 1, NRHS
                R(K, L) = R(K, L) - ZTMP * R(NP, L)
            end do
        end do
        !
    end do
    !
    !---- solve for last row
    do L = 1, NRHS
        R(NN, L) = R(NN, L) / Z(NN, NN)
    end do
    !
    !---- back substitute everything
    do NP = NN - 1, 1, -1
        NP1 = NP + 1
        do L = 1, NRHS
            do K = NP1, NN
                R(NP, L) = R(NP, L) - Z(NP, K) * R(K, L)
            end do
        end do
    end do
    !
    RETURN
END
! GAUSS