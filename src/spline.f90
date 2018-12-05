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

SUBROUTINE SPLINE(X, XS, S, N)
    IMPLICIT REAL (A-H, M, O-Z)
    DIMENSION X(N), XS(N), S(N)
    PARAMETER (NMAX = 1000)
    COMMON /SPLCOM/ A(NMAX), B(NMAX), C(NMAX)
    !-------------------------------------------------------
    !     Calculates spline coefficients for X(S).          |
    !     Zero 2nd derivative end conditions are used.      |
    !     To evaluate the spline at some value of S,        |
    !     use SEVAL and/or DEVAL.                           |
    !                                                       |
    !     S        independent variable array (input)       |
    !     X        dependent variable array   (input)       |
    !     XS       dX/dS array                (calculated)  |
    !     N        number of points           (input)       |
    !                                                       |
    !-------------------------------------------------------
    IF(N.GT.NMAX) STOP 'SPLINE: array overflow, increase NMAX'
    !
    IF(N.EQ.1) THEN
        XS(1) = 0.
        RETURN
    ENDIF
    !
    do I = 2, N - 1
        DSM = S(I) - S(I - 1)
        DSP = S(I + 1) - S(I)
        B(I) = DSP
        A(I) = 2.0 * (DSM + DSP)
        C(I) = DSM
        XS(I) = 3.0 * ((X(I + 1) - X(I)) * DSM / DSP + (X(I) - X(I - 1)) * DSP / DSM)
    end do
    !
    !---- set zero second derivative end conditions
    A(1) = 2.0
    C(1) = 1.0
    XS(1) = 3.0 * (X(2) - X(1)) / (S(2) - S(1))
    B(N) = 1.0
    A(N) = 2.0
    XS(N) = 3.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
    !
    !---- solve for derivative array XS
    CALL TRISOL(A, B, C, XS, N)
    !
    RETURN
END
! SPLINE


SUBROUTINE SPLIND(X, XS, S, N, XS1, XS2)
    IMPLICIT REAL (A-H, M, O-Z)
    DIMENSION X(N), XS(N), S(N)
    PARAMETER (NMAX = 1000)
    COMMON /SPLCOM/ A(NMAX), B(NMAX), C(NMAX)
    !-------------------------------------------------------
    !     Calculates spline coefficients for X(S).          |
    !     Specified 1st derivative and/or zero 2nd          |
    !     or 3rd derivative end conditions can be used.     |
    !     To evaluate the spline at some value of S,        |
    !     use SEVAL and/or DEVAL.                           |
    !                                                       |
    !     S        independent variable array (input)       |
    !     X        dependent variable array   (input)       |
    !     XS       dX/dS array                (calculated)  |
    !     N        number of points           (input)       |
    !     XS1,XS2  endpoint derivatives       (input)       |
    !              If =  999.0, use zero 2nd derivative     |
    !              If = -999.0, use zero 3rd derivative     |
    !                                                       |
    !-------------------------------------------------------
    IF(N.GT.NMAX) STOP 'SPLIND: array overflow, increase NMAX'
    !
    IF(N.EQ.1) THEN
        XS(1) = 0.
        RETURN
    ENDIF
    !
    do I = 2, N - 1
        DSM = S(I) - S(I - 1)
        DSP = S(I + 1) - S(I)
        B(I) = DSP
        A(I) = 2.0 * (DSM + DSP)
        C(I) = DSM
        XS(I) = 3.0 * ((X(I + 1) - X(I)) * DSM / DSP + (X(I) - X(I - 1)) * DSP / DSM)
    end do
    !
    !---- set left end condition
    IF(XS1.EQ.999.0) THEN
        !----- zero 2nd derivative
        A(1) = 2.0
        C(1) = 1.0
        XS(1) = 3.0 * (X(2) - X(1)) / (S(2) - S(1))
    ELSE IF(XS1.EQ.-999.0) THEN
        !----- set zero 3rd derivative
        A(1) = 1.0
        C(1) = 1.0
        XS(1) = 2.0 * (X(2) - X(1)) / (S(2) - S(1))
    ELSE
        !----- specified 1st derivative
        A(1) = 1.0
        C(1) = 0.
        XS(1) = XS1
    ENDIF
    !
    !---- set right end condition
    IF(XS2.EQ.999.0) THEN
        !----- zero 2nd derivative
        B(N) = 1.0
        A(N) = 2.0
        XS(N) = 3.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
    ELSE IF(XS2.EQ.-999.0) THEN
        !----- zero 3rd derivative
        B(N) = 1.0
        A(N) = 1.0
        XS(N) = 2.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
    ELSE
        !----- specified 1st derivative
        A(N) = 1.0
        B(N) = 0.
        XS(N) = XS2
    ENDIF
    !
    !---- if only two points, cannot have zero third derivatives at both ends
    IF(N.EQ.2 .AND. XS1.EQ.-999.0 .AND. XS2.EQ.-999.0) THEN
        !----- set zero 2nd derivative at right end (left end will also be zero)
        B(N) = 1.0
        A(N) = 2.0
        XS(N) = 3.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
    ENDIF
    !
    !---- solve for derivative array XS
    CALL TRISOL(A, B, C, XS, N)
    !
    RETURN
END
! SPLIND



SUBROUTINE SPLINA(X, XS, S, N)
    DIMENSION X(N), XS(N), S(N)
    LOGICAL LEND
    !-------------------------------------------------------
    !     Calculates spline coefficients for X(S).          |
    !     A simple averaging of adjacent segment slopes     |
    !     is used to achieve non-oscillatory curve          |
    !     End conditions are set by end segment slope       |
    !     To evaluate the spline at some value of S,        |
    !     use SEVAL and/or DEVAL.                           |
    !                                                       |
    !     S        independent variable array (input)       |
    !     X        dependent variable array   (input)       |
    !     XS       dX/dS array                (calculated)  |
    !     N        number of points           (input)       |
    !                                                       |
    !-------------------------------------------------------
    !
    IF(N.EQ.1) THEN
        XS(1) = 0.
        RETURN
    ENDIF
    !
    LEND = .TRUE.
    do I = 1, N - 1
        DS = S(I + 1) - S(I)
        IF (DS.EQ.0.) THEN
            XS(I) = XS1
            LEND = .TRUE.
        ELSE
            DX = X(I + 1) - X(I)
            XS2 = DX / DS
            IF (LEND) THEN
                XS(I) = XS2
                LEND = .FALSE.
            ELSE
                XS(I) = 0.5 * (XS1 + XS2)
            ENDIF
        ENDIF
        XS1 = XS2
    end do
    XS(N) = XS1
    !
    RETURN
END
! SPLINA




SUBROUTINE TRISOL(A, B, C, D, KK)
    IMPLICIT REAL (A-H, M, O-Z)
    DIMENSION A(KK), B(KK), C(KK), D(KK)
    !-----------------------------------------
    !     Solves KK long, tri-diagonal system |
    !                                         |
    !             A C          D              |
    !             B A C        D              |
    !               B A .      .              |
    !                 . . C    .              |
    !                   B A    D              |
    !                                         |
    !     The righthand side D is replaced by |
    !     the solution.  A, C are destroyed.  |
    !-----------------------------------------
    !
    do K = 2, KK
        KM = K - 1
        C(KM) = C(KM) / A(KM)
        D(KM) = D(KM) / A(KM)
        A(K) = A(K) - B(K) * C(KM)
        D(K) = D(K) - B(K) * D(KM)
    end do
    !
    D(KK) = D(KK) / A(KK)
    !
    do K = KK - 1, 1, -1
        D(K) = D(K) - C(K) * D(K + 1)
    end do
    !
    RETURN
END
! TRISOL


FUNCTION SEVAL(SS, X, XS, S, N)
    IMPLICIT REAL (A-H, M, O-Z)
    DIMENSION X(N), XS(N), S(N)
    !--------------------------------------------------
    !     Calculates X(SS)                             |
    !     XS array must have been calculated by SPLINE |
    !--------------------------------------------------
    IF(N.EQ.1) THEN
        SEVAL = X(1)
        RETURN
    ENDIF
    !
    ILOW = 1
    I = N
    !
    10 IF(I - ILOW .LE. 1) GO TO 11
    !
    IMID = (I + ILOW) / 2
    IF(SS .LT. S(IMID)) THEN
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
    IF(N.EQ.1) THEN
        DEVAL = XS(1)
        RETURN
    ENDIF
    !
    ILOW = 1
    I = N
    !
    10 IF(I - ILOW .LE. 1) GO TO 11
    !
    IMID = (I + ILOW) / 2
    IF(SS .LT. S(IMID)) THEN
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
    IMPLICIT REAL (A-H, M, O-Z)
    DIMENSION X(N), XS(N), S(N)
    !-----------------------------------------------
    !     Splines X(S) array just like SPLINE,      |
    !     but allows derivative discontinuities     |
    !     at segment joints.  Segment joints are    |
    !     defined by identical successive S values. |
    !-----------------------------------------------
    IF(N.EQ.1) THEN
        XS(1) = 0.
        RETURN
    ENDIF
    !
    IF(S(1).EQ.S(2)) STOP 'SEGSPL:  First input point duplicated'
    IF(S(N).EQ.S(N - 1)) STOP 'SEGSPL:  Last  input point duplicated'
    !
    ISEG0 = 1
    do ISEG = 2, N - 2
        IF(S(ISEG).EQ.S(ISEG + 1)) THEN
            NSEG = ISEG - ISEG0 + 1
            !cc         CALL SPLINE(X(ISEG0),XS(ISEG0),S(ISEG0),NSEG)
            CALL SPLIND(X(ISEG0), XS(ISEG0), S(ISEG0), NSEG, -999.0, -999.0)
            ISEG0 = ISEG + 1
        ENDIF
    end do
    !
    NSEG = N - ISEG0 + 1
    !cc      CALL SPLINE(X(ISEG0),XS(ISEG0),S(ISEG0),NSEG)
    CALL SPLIND(X(ISEG0), XS(ISEG0), S(ISEG0), NSEG, -999.0, -999.0)
    !
    RETURN
END
! SEGSPL