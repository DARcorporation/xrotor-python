!***********************************************************************
!    Module:  xbend.f
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
!
SUBROUTINE BEND
    USE common
    IMPLICIT REAL (M)
    CHARACTER*4 COMAND
    CHARACTER*132 COMARG
    !
    DIMENSION IINPUT(20)
    DIMENSION RINPUT(20)
    LOGICAL ERROR
    !
    !---------------------------------------------
    !     Run rotor at arbitrary operating points
    !---------------------------------------------
    !
    GREEK = .FALSE.
    !
    IF(LSTRUC) THEN
        WRITE(*, *)
        WRITE(*, *) 'Structural properties available'
    ELSE
        WRITE(*, *)
        WRITE(*, *) 'Structural properties not available'
    ENDIF
    !
    900  CALL ASKC('.BEND^', COMAND, COMARG)
    !
    DO I = 1, 20
        IINPUT(I) = 0
        RINPUT(I) = 0.0
    ENDDO
    NINPUT = 0
    CALL GETINT(COMARG, IINPUT, NINPUT, ERROR)
    NINPUT = 0
    CALL GETFLT(COMARG, RINPUT, NINPUT, ERROR)
    !
    IF(COMAND == '    ') RETURN
    IF(COMAND == '?   ') WRITE(*, 1100)
    IF(COMAND == '?   ') GO TO 900
    IF(COMAND == 'READ') GO TO 10
    IF(COMAND == 'CLR ') GO TO 20
    IF(COMAND == 'EVAL') GO TO 30
    IF(COMAND == 'DEFL') GO TO 40
    IF(COMAND == 'REST') GO TO 50
    IF(COMAND == 'WRIT') GO TO 70
    IF(COMAND == 'SETS') GO TO 80
    IF(COMAND == 'MCLR') GO TO 85
    IF(COMAND == 'HELP') GO TO 100
    !
    !-------------------------------------------------------------
    WRITE(*, 1000) COMAND
    GO TO 900
    !
    !-------------------------------------------------------------
    10   CALL EILOAD(COMARG)
    GO TO 900
    !
    !-------------------------------------------------------------
    20   CALL STCLR
    GO TO 900
    !
    !-------------------------------------------------------------
    30   IF(.NOT.LSTRUC) THEN
        WRITE(*, *) 'Structural properties not available'
        WRITE(*, *) 'Assuming zero mass, infinite stiffness...'
    ENDIF
    !cc      CALL STLOAD
    CALL STCALC
    CALL STWRIT(LUWRIT)
    GO TO 900
    !
    !-------------------------------------------------------------
    40   CALL STADD
    GO TO 900
    !
    !-------------------------------------------------------------
    50   do I = 1, II
        BETA(I) = BETA0(I)
    end do
    CONV = .FALSE.
    GO TO 900
    !
    !-------------------------------------------------------------
    70   IF(COMARG(1:1) /= ' ') SAVFIL = COMARG
    CALL OPFILE(LUSAVE, SAVFIL)
    CALL STWRIT(LUSAVE)
    CLOSE(LUSAVE)
    GO TO 900
    !
    !-------------------------------------------------------------
    80   CALL STSET
    GO TO 900
    !
    !-------------------------------------------------------------
    85   CALL MCLR
    GO TO 900
    !
    !-------------------------------------------------------------
    100  WRITE(*, 3000)
    GO TO 900
    !
    !.......................................................................
    !
    1000 FORMAT(1X, A4, ' command not recognized.  Type a "?" for list')
    1100 FORMAT(&
            /'   READ f Read in blade structural properties'&
            /'   EVAL   Evaluate structural loads and deflections'&
            /'   CLR    Clear all structural deflections'&
            //'   DEFL   Set new twist  =  static  +  structural twist'&
            /'   REST   Set new twist  =  static twist'&
            /'   SETS   Set static twist = current - structural twist'&
            //'   WRIT f Write structural solution to disk file'&
            //'   HELP   Display help on structural calculation')
    !
    !
    3000 FORMAT(/&
            'The axis definitions are:'//&
            '  X  aft along prop rotation axis'/&
            '  Y  radial alng blade'/&
            '  Z  perpendicular to blade:    X x Y = Z'//&
            'The structural solution contains two groups of data, '&
            'the first group has:'//&
            '  u/R    deflections in the X direction'/&
            '  w/R    deflections in the Z direction'/&
            '   t     torsional twist (positive in the increasing '&
            'incidence direction)'/&
            '   Mz    bending moment about the Z axis'/&
            '   Mx    bending moment about the X axis'/&
            '   T     moment about the radial Y axis (i.e. torsion)'/&
            '   P     tensile load (shear in Y direction)'/&
            '   Sx    shear in X direction'/&
            '   Sz    shear in Z direction'//&
            'The second group of structural data contains:'//&
            '   Ex    strain due to bending in the X direction'/&
            '   Ey    strain due to extension in the Y direction'/&
            '   Ez    strain due to bending in the Z direction'/&
            '   Emax  maximum strain calculated by  '&
            'Emax = sqrt(Ex^2 + Ez^2) + Ey'/&
            '   g     shear strain due to twist t'//&
            '   Note that Ex, Ez, and g are evaluated at the '&
            'local radius RST from'/&
            '   the structural axis (RST is an input quantity, '&
            'normally set to the '/&
            '   distance of the highest or lowest profile point '&
            'from the structural axis)'/)
    !
END
! BEND



SUBROUTINE EILOAD(FNAME1)
    USE common
    IMPLICIT REAL (M)
    CHARACTER*(*) FNAME1
    DIMENSION XT(IX)
    !----------------------------------------------------
    !     Reads and splines blade structural properties.
    !----------------------------------------------------
    !
    !    XT =    R    radius
    !    W0 = EIXX    in-plane stiffness
    !    W1 = EIYY    out-of-plane stiffness
    !    W2 =   EA    extensional stiffness
    !    W3 =   GJ    torsional stiffness
    !    W4 =   EK    extensional/torsional cross-stiffness
    !    W5 =    M    mass density / length
    !    W6 =  MXX    pitch axis inertia / length
    !    W7 = XOCG    x/c of section CG
    !    W8 = XOSC    x/c of section shear center (structural axis)
    !    W9 = RST     structural radius for strain evaluation
    !
    LU = 14
    !
    FNAME = FNAME1
    IF(FNAME(1:1) == ' ') CALL ASKS('Enter input filename^', FNAME)
    !
    OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 200)
    READ(LU, 1000) DUMMY
    READ(LU, 1000) DUMMY
    READ(LU, 1000) DUMMY
    do IT = 1, IX
        READ(LU, *, END = 11, ERR = 210) XT(IT), &
                W0(IT), W1(IT), W2(IT), W3(IT), W4(IT), &
                W5(IT), W6(IT), W7(IT), W8(IT), W9(IT)
        XT(IT) = XT(IT) / RAD
    end do
    WRITE(*, *) 'EILOAD: Array overflow.  Too many radial stations.'
    11   CONTINUE
    NT = IT - 1
    CLOSE(LU)
    !
    CALL SEGSPL(W0, T0, XT, NT)
    CALL SEGSPL(W1, T1, XT, NT)
    CALL SEGSPL(W2, T2, XT, NT)
    CALL SEGSPL(W3, T3, XT, NT)
    CALL SEGSPL(W4, T4, XT, NT)
    CALL SEGSPL(W5, T5, XT, NT)
    CALL SEGSPL(W6, T6, XT, NT)
    CALL SEGSPL(W7, T7, XT, NT)
    CALL SEGSPL(W8, T8, XT, NT)
    CALL SEGSPL(W9, T9, XT, NT)
    !
    do I = 1, II
        EIXXB(I) = SEVAL(XI(I), W0, T0, XT, NT)
        EIYYB(I) = SEVAL(XI(I), W1, T1, XT, NT)
        EAB(I) = SEVAL(XI(I), W2, T2, XT, NT)
        GJB(I) = SEVAL(XI(I), W3, T3, XT, NT)
        EKB(I) = SEVAL(XI(I), W4, T4, XT, NT)
        MB(I) = SEVAL(XI(I), W5, T5, XT, NT)
        MXXB(I) = SEVAL(XI(I), W6, T6, XT, NT)
        XOCG(I) = SEVAL(XI(I), W7, T7, XT, NT)
        XOSC(I) = SEVAL(XI(I), W8, T8, XT, NT)
        RSTB(I) = SEVAL(XI(I), W9, T9, XT, NT)
    end do
    !
    MASS = 0.0
    MRSQ = 0.0
    MAXX = 0.0
    do I = 1, II
        MASS = MASS + MB(I) * RAD * DXI(I)
        MRSQ = MRSQ + MB(I) * RAD * DXI(I) * (XI(I) * RAD)**2
        MAXX = MAXX + MXXB(I) * RAD * DXI(I)
    end do
    !
    WRITE(*, 3100) MASS, MAXX, MRSQ
    !
    LSTRUC = .TRUE.
    RETURN
    !
    200 WRITE(*, 1010) FNAME(1:32)
    RETURN
    !
    210 WRITE(*, 1020) FNAME(1:32)
    CLOSE(LU)
    CONV = .FALSE.
    RETURN
    !..............................
    1000 FORMAT(32A1)
    1010 FORMAT(' File  ', A, ' not found'/)
    1020 FORMAT(' File  ', A, ' has incompatible format'/&
            ' Loading not completed'/)
    3100 FORMAT(/' Blade mass =', G12.4&
            /' Pitch-axis inertia =', G14.5&
            /' Rotational inertia =', G14.5)
END
! EILOAD


SUBROUTINE STCLR
    USE common
    IMPLICIT REAL (M)
    !
    do I = 1, II
        TX(I) = 0.0
        TY(I) = 0.0
        TZ(I) = 0.0
        WX(I) = 0.0
        WY(I) = 0.0
        WZ(I) = 0.0
        MOMX(I) = 0.0
        MOMY(I) = 0.0
        MOMZ(I) = 0.0
        SHRX(I) = 0.0
        SHRY(I) = 0.0
        SHRZ(I) = 0.0
    end do
    !
    RETURN
END
! STCLR


SUBROUTINE MCLR
    USE common
    IMPLICIT REAL (M)
    !
    do I = 1, II
        MB(I) = 0.0
        MXXB(I) = 0.0
        EKB(I) = 0.0
    end do
    !
    RETURN
END
! MCLR



SUBROUTINE STLOAD
    USE common
    IMPLICIT REAL (M)
    !-----------------------------------------------------------
    !     Calculates force and moment loadings along blade.
    ! HHY 3/99 a local x',y',z' system is assumed that is tilted by
    !          a rake angle about the z axis, the y' axis runs out
    !          the blade axis.  z' = z, x',y' are rotated from x,y by rake angle
    !
    !     The ()_A and ()_B sensitivities below should be set
    !     only if BETA(.) and the aero solution CL(.), GAM(.),
    !     etc. are updated every iteration in STCALC.
    !-----------------------------------------------------------
    !
    FX = 0.0
    FY = 0.0
    FZ = 0.0
    SINR = SIN(RAKE)
    COSR = COS(RAKE)
    !
    do I = 1, II
        !
        DXII = DXI(I) / COSR
        !
        CALL CSCALC(I, UTOT, WA, WT, &
                VT, VT_ADW, &
                VA, VA_ADW, &
                VD, VD_ADW, &
                CI, CI_ADV, CI_VT, &
                SI, SI_VA, &
                W, W_ADV, W_VT, W_VA, &
                PHI, P_ADV, P_VT, P_VA)
        !
        WSQ = W * W
        !
        SINB = SIN(BETA(I))
        COSB = COS(BETA(I))
        !
        TXA = 0.5 * (TX(I) + TX(I + 1))
        TZA = 0.5 * (TZ(I) + TZ(I + 1))
        WZA = 0.5 * (WZ(I) + WZ(I + 1))
        !
        !--- Aerodynamic lift and drag forces (act in blade normal plane)
        FLIFT = 0.5 * WSQ * CH(I) * CL(I)
        FDRAG = 0.5 * WSQ * CH(I) * CD(I)
        !
        FLIFT_A = 0.0
        !cc     FLIFT_A = 0.5*WSQ*CH(I)*DCLDA
        !--- blade resolved force components
        FXA = -CI / W * FLIFT + SI / W * FDRAG
        FYA = 0.0
        FZA = SI / W * FLIFT + CI / W * FDRAG
        !
        !***************************************
        !--- Integrate blade aerodynamic forces
        FX = FX + COSR * DXII * FXA
        FY = FY + SINR * DXII * FXA
        FZ = FZ + DXII * FZA
        !***************************************
        !
        !
        !--- Centrifugal forces (y direction)
        FCENT = 0.0
        IF(LSTRUC) FCENT = MB(I) * XI(I) / ADV**2&
                / (RHO * RAD**2)
        !--- blade resolved force components
        FXC = -SINR * FCENT
        FYC = COSR * FCENT
        FZC = 0.0
        !
        !--- Assemble force loadings
        PX(I) = FXA + FXC + FYC * TZA * COSR
        PY(I) = FYA + FYC
        PZ(I) = FZA + FZC + FYC * (WZA / XI(I) - TXA)
        PX_TY(I) = -FLIFT_A * CI / W
        PX_TZ(I) = FYC * COSR
        PZ_TX(I) = -FYC
        PZ_TY(I) = FLIFT_A * SI / W
        PZ_WZ(I) = FYC / XI(I)
        !
        !        PX(I) =  -FLIFT*CI/W + FDRAG*SI/W + FCENT* TZA
        !        PZ(I) =   FLIFT*SI/W + FDRAG*CI/W + FCENT*(WZA/XI(I) - TXA)
        !        PY(I) =                             FCENT
        !        PX_TY(I) = -FLIFT_A*CI/W
        !        PX_TZ(I) =  FCENT
        !        PZ_TX(I) = -FCENT
        !        PZ_TY(I) =  FLIFT_A*SI/W
        !        PZ_WZ(I) =  FCENT/XI(I)
        !
        !--- Define moment components
        IF(LSTRUC) THEN
            MCENT = SINB * (XOCG(I) - XOSC(I)) * CH(I) * MB(I) * XI(I) / ADV**2&
                    / (RHO * RAD**2)
            MPREC = -SINB * COSB * MXXB(I) / ADV**2&
                    / (RHO * RAD**4)
            MAERO = 0.5 * WSQ * (CL(I) * (XOSC(I) - 0.25) + CM(I)) * CH(I)**2
        ELSE
            MCENT = 0.0
            MPREC = 0.0
            MAERO = 0.5 * WSQ * (CL(I) * (0.40 - 0.25) + CM(I)) * CH(I)**2
        ENDIF
        !
        MCENT_B = 0.0
        MPREC_B = 0.0
        MAERO_A = 0.0
        !cc     MCENT_B = COSB * (XOCG(I)-XOSC(I))*CH(I) * MB(I)*XI(I)/ADV**2
        !cc  &          / (RHO * RAD**2)
        !cc     MPREC_B = -(COSB**2 - SINB**2) * MXXB(I)/ADV**2
        !cc  &          / (RHO * RAD**4)
        !cc     MAERO_A = 0.5*WSQ*(DCLDA*(XOSC(I)-0.25)     ) * CH(I)**2
        !
        !--- Assemble imposed moments
        MX(I) = 0.0
        MY(I) = MAERO + MPREC
        MZ(I) = MCENT + MPREC * (WZA / XI(I) - TXA)
        !
        MY_TY(I) = MAERO_A + MPREC_B * (WZA / XI(I) - TXA)
        MZ_TY(I) = MCENT_B
        MZ_TX(I) = - MPREC
        MZ_WZ(I) = MPREC / XI(I)
        !
    end do
    !
    !--- Print the blade aerodynamic forces
    WRITE(*, 20) FX * RHO * VEL**2 * RAD**2, &
            FY * RHO * VEL**2 * RAD**2, &
            FZ * RHO * VEL**2 * RAD**2
    20   FORMAT(/'Blade aerodynamic forces:', &
            /' FX (axial)      = ', F12.6, &
            /' FY (radial)     = ', F12.6, &
            /' FZ (tangential) = ', F12.6)
    !
    RETURN
END
! STLOAD



SUBROUTINE STCALC
    USE common
    IMPLICIT REAL (M)
    !------------------------------------------------------------
    !     Updates resultants and deflections along blade.
    !     Uses current loading distributions PX,PY,PZ, MX,MY,MZ.
    !------------------------------------------------------------
    REAL AA(12, 12, IXP), BB(12, 12, IXP), CC(12, 12, IXP), RR(12, IXP)
    REAL RRLIM(12), RLXR(12)
    !
    EPS = 1.0E-5
    !
    RRLIM(1) = 0.10
    RRLIM(2) = 0.10
    RRLIM(3) = 0.10
    !
    RRLIM(4) = 0.2 / ADV**2
    RRLIM(5) = 0.5 / ADV**2
    RRLIM(6) = 0.2 / ADV**2
    !
    RRLIM(7) = 0.2 / ADV**2
    RRLIM(8) = 20.0 / ADV**2
    RRLIM(9) = 0.2 / ADV**2
    !
    RRLIM(10) = 0.10
    RRLIM(11) = 0.01
    RRLIM(12) = 0.10
    !
    WRITE(*, *)
    !
    !---- Newton iteration loop
    do ITER = 1, 10
        !
        CALL STLOAD
        !
        do I = 1, II + 1
            do K = 1, 12
                do J = 1, 12
                    AA(K, J, I) = 0.0
                    BB(K, J, I) = 0.0
                    CC(K, J, I) = 0.0
                end do
                RR(K, I) = 0.0
            end do
        end do
        !
        !
        !---- fix deflection angles at root
        I = 1
        AA(1, 1, I) = 1.0
        AA(2, 2, I) = 1.0
        AA(3, 3, I) = 1.0
        !
        AA(10, 10, I) = 1.0
        AA(11, 11, I) = 1.0
        AA(12, 12, I) = 1.0
        !
        !---- non-dimensionalizing factors
        EAREF = RHO * VEL**2 * RAD**2
        EKREF = RHO * VEL**2 * RAD**3
        EIREF = RHO * VEL**2 * RAD**4
        !
        COSR = COS(RAKE)
        !
        !---- go over radial intervals
        do I = 1, II
            !
            DXII = DXI(I) / COSR
            !
            SX2 = SHRX(I + 1)
            SY2 = SHRY(I + 1)
            SZ2 = SHRZ(I + 1)
            !
            SX1 = SHRX(I)
            SY1 = SHRY(I)
            SZ1 = SHRZ(I)
            !
            MX2 = MOMX(I + 1)
            MY2 = MOMY(I + 1)
            MZ2 = MOMZ(I + 1)
            !
            MX1 = MOMX(I)
            MY1 = MOMY(I)
            MZ1 = MOMZ(I)
            !
            TX2 = TX(I + 1)
            TY2 = TY(I + 1)
            TZ2 = TZ(I + 1)
            !
            TX1 = TX(I)
            TY1 = TY(I)
            TZ1 = TZ(I)
            !
            WX2 = WX(I + 1)
            WY2 = WY(I + 1)
            WZ2 = WZ(I + 1)
            !
            WX1 = WX(I)
            WY1 = WY(I)
            WZ1 = WZ(I)
            !
            !
            !        PX_TY(I) = -FLIFT_A*CI/W
            !        PX_TZ(I) =  FCENT
            !        PZ_TX(I) = -FCENT
            !        PZ_TY(I) =  FLIFT_A*SI/W
            !        PZ_WZ(I) =  FCENT/XI(I)
            !
            !        MY_TY(I) = MAERO_A + MPREC_B*(WZA/XI(I) - TXA)
            !        MZ_TY(I) = MCENT_B
            !        MZ_TX(I) =         - MPREC
            !        MZ_WZ(I) =           MPREC/XI(I)
            !

            !------ x-moment
            RR(4, I) = MX2 - MX1&
                    - (MY2 + MY1) * 0.5 * (TZ2 - TZ1)&
                    + (MZ2 + MZ1) * 0.5 * (TY2 - TY1)&
                    - ((SZ2 + SZ1) * 0.5 - MX(I)) * DXII
            !
            AA(4, 2, I) = -(MZ2 + MZ1) * 0.5
            AA(4, 3, I) = (MY2 + MY1) * 0.5
            AA(4, 4, I) = -1.0
            AA(4, 5, I) = -0.5 * (TZ2 - TZ1)
            AA(4, 6, I) = 0.5 * (TY2 - TY1)
            AA(4, 9, I) = -0.5 * DXII
            CC(4, 2, I) = (MZ2 + MZ1) * 0.5
            CC(4, 3, I) = -(MY2 + MY1) * 0.5
            CC(4, 4, I) = 1.0
            CC(4, 5, I) = -0.5 * (TZ2 - TZ1)
            CC(4, 6, I) = 0.5 * (TY2 - TY1)
            CC(4, 9, I) = -0.5 * DXII
            !
            !------ y-moment
            RR(5, I) = MY2 - MY1&
                    + (MX2 + MX1) * 0.5 * (TZ2 - TZ1)&
                    - (MZ2 + MZ1) * 0.5 * (TX2 - TX1)&
                    + (MY(I)) * DXII
            AA(5, 1, I) = (MZ2 + MZ1) * 0.5
            AA(5, 2, I) = MY_TY(I) * DXII * 0.5
            AA(5, 3, I) = -(MX2 + MX1) * 0.5
            AA(5, 4, I) = 0.5 * (TZ2 - TZ1)
            AA(5, 5, I) = -1.0
            AA(5, 6, I) = -0.5 * (TX2 - TX1)
            CC(5, 1, I) = -(MZ2 + MZ1) * 0.5
            CC(5, 2, I) = MY_TY(I) * DXII * 0.5
            CC(5, 3, I) = (MX2 + MX1) * 0.5
            CC(5, 4, I) = 0.5 * (TZ2 - TZ1)
            CC(5, 5, I) = 1.0
            CC(5, 6, I) = -0.5 * (TX2 - TX1)
            !
            !------ z-moment
            RR(6, I) = MZ2 - MZ1&
                    + (MY2 + MY1) * 0.5 * (TX2 - TX1)&
                    - (MX2 + MX1) * 0.5 * (TY2 - TY1)&
                    + ((SX2 + SX1) * 0.5 + MZ(I)) * DXII
            AA(6, 1, I) = -(MY2 + MY1) * 0.5
            AA(6, 2, I) = (MX2 + MX1) * 0.5 + MZ_TY(I) * DXII * 0.5
            AA(6, 3, I) = MZ_TX(I) * DXII * 0.5
            AA(6, 4, I) = -0.5 * (TY2 - TY1)
            AA(6, 5, I) = 0.5 * (TX2 - TX1)
            AA(6, 6, I) = -1.0
            AA(6, 7, I) = 0.5 * DXII
            AA(6, 12, I) = + MZ_WZ(I) * DXII * 0.5
            CC(6, 1, I) = (MY2 + MY1) * 0.5
            CC(6, 2, I) = -(MX2 + MX1) * 0.5 + MZ_TY(I) * DXII * 0.5
            CC(6, 3, I) = MZ_TX(I) * DXII * 0.5
            CC(6, 4, I) = -0.5 * (TY2 - TY1)
            CC(6, 5, I) = 0.5 * (TX2 - TX1)
            CC(6, 6, I) = 1.0
            CC(6, 7, I) = 0.5 * DXII
            CC(6, 12, I) = + MZ_WZ(I) * DXII * 0.5
            !
            !
            !------ x-shear
            RR(7, I) = SX2 - SX1&
                    + (SY2 + SY1) * 0.5 * (TZ2 - TZ1)&
                    + (SZ2 + SZ1) * 0.5 * (TY2 - TY1) - PX(I) * DXII
            !
            AA(7, 2, I) = -(SZ2 + SZ1) * 0.5 - PX_TY(I) * DXII * 0.5
            AA(7, 3, I) = -(SY2 + SY1) * 0.5 - PX_TZ(I) * DXII * 0.5
            AA(7, 7, I) = -1.0
            AA(7, 8, I) = 0.5 * (TZ2 - TZ1)
            AA(7, 9, I) = 0.5 * (TY2 - TY1)
            CC(7, 2, I) = (SZ2 + SZ1) * 0.5 - PX_TY(I) * DXII * 0.5
            CC(7, 3, I) = (SY2 + SY1) * 0.5 - PX_TZ(I) * DXII * 0.5
            CC(7, 7, I) = 1.0
            CC(7, 8, I) = 0.5 * (TZ2 - TZ1)
            CC(7, 9, I) = 0.5 * (TY2 - TY1)
            !
            !------ y-shear
            RR(8, I) = SY2 - SY1&
                    - (SX2 + SX1) * 0.5 * (TZ2 - TZ1)&
                    + (SZ2 + SZ1) * 0.5 * (TX2 - TX1) + PY(I) * DXII
            AA(8, 1, I) = (SZ2 + SZ1) * 0.5
            AA(8, 3, I) = -(SX2 + SX1) * 0.5
            AA(8, 7, I) = -0.5 * (TZ2 - TZ1)
            AA(8, 8, I) = -1.0
            AA(8, 9, I) = 0.5 * (TX2 - TX1)
            CC(8, 1, I) = -(SZ2 + SZ1) * 0.5
            CC(8, 3, I) = (SX2 + SX1) * 0.5
            CC(8, 7, I) = -0.5 * (TZ2 - TZ1)
            CC(8, 8, I) = 1.0
            CC(8, 9, I) = 0.5 * (TX2 - TX1)
            !
            !------ z-shear
            RR(9, I) = SZ2 - SZ1&
                    - (SY2 + SY1) * 0.5 * (TX2 - TX1)&
                    - (SX2 + SX1) * 0.5 * (TY2 - TY1) - PZ(I) * DXII
            !
            AA(9, 1, I) = (SY2 + SY1) * 0.5 - PZ_TX(I) * DXII * 0.5
            AA(9, 2, I) = (SX2 + SX1) * 0.5 - PZ_TY(I) * DXII * 0.5
            AA(9, 7, I) = -0.5 * (TY2 - TY1)
            AA(9, 8, I) = -0.5 * (TX2 - TX1)
            AA(9, 9, I) = -1.0
            AA(9, 12, I) = - PZ_WZ(I) * DXII * 0.5
            CC(9, 1, I) = -(SY2 + SY1) * 0.5 - PZ_TX(I) * DXII * 0.5
            CC(9, 2, I) = -(SX2 + SX1) * 0.5 - PZ_TY(I) * DXII * 0.5
            CC(9, 7, I) = -0.5 * (TY2 - TY1)
            CC(9, 8, I) = -0.5 * (TX2 - TX1)
            CC(9, 9, I) = 1.0
            CC(9, 12, I) = - PZ_WZ(I) * DXII * 0.5
            !
            !
            SB = SIN(BETA(I))
            CB = COS(BETA(I))
            !
            IF(LSTRUC) THEN
                GJ = GJB(I) / EIREF
                EK = EKB(I) / EKREF
                EA = EAB(I) / EAREF
                !
                EIZZ = (EIXXB(I) * CB**2 + EIYYB(I) * SB**2) / EIREF
                EIXX = (EIXXB(I) * SB**2 + EIYYB(I) * CB**2) / EIREF
                EIXZ = (EIXXB(I) * SB * CB - EIYYB(I) * SB * CB) / EIREF
                !
                EIZZ_B = (-EIXXB(I) + EIYYB(I)) * 2.0 * SB * CB / EIREF
                EIXX_B = (EIXXB(I) - EIYYB(I)) * 2.0 * SB * CB / EIREF
                EIXZ_B = (EIXXB(I) - EIYYB(I)) * (CB * CB - SB * SB) / EIREF
            ELSE
                EIBIG = 1.0E+8
                !
                GJ = EIBIG
                EK = 0.0
                EA = EIBIG
                !
                EIZZ = EIBIG
                EIXX = EIBIG
                EIXZ = 0.0
                !
                EIZZ_B = 0.0
                EIXX_B = 0.0
                EIXZ_B = 0.0
            ENDIF
            !
            MOMXD = (MOMX(I) + MOMX(I + 1)) * 0.5 * DXII
            MOMYD = (MOMY(I) + MOMY(I + 1)) * 0.5 * DXII
            MOMZD = (MOMZ(I) + MOMZ(I + 1)) * 0.5 * DXII
            !
            !
            !------ x-deflection angle
            RR(1, I + 1) = EIZZ * (TX2 - TX1)&
                    - EIXZ * (TZ2 - TZ1) - (MX2 + MX1) * 0.5 * DXII
            !
            BB(1, 1, I + 1) = -EIZZ
            BB(1, 2, I + 1) = EIZZ_B * (TX2 - TX1) * 0.5&
                    - EIXZ_B * (TZ2 - TZ1) * 0.5
            BB(1, 3, I + 1) = EIXZ
            BB(1, 4, I + 1) = -0.5 * DXII
            AA(1, 1, I + 1) = EIZZ
            AA(1, 2, I + 1) = EIZZ_B * (TX2 - TX1) * 0.5&
                    - EIXZ_B * (TZ2 - TZ1) * 0.5
            AA(1, 3, I + 1) = -EIXZ
            AA(1, 4, I + 1) = -0.5 * DXII
            !
            !------ y-deflection angle (twist)
            RR(2, I + 1) = GJ * (TY2 - TY1) - (MY2 + MY1) * 0.5 * DXII&
                    - EK * (WY2 - WY1)
            !
            BB(2, 2, I + 1) = -GJ
            BB(2, 5, I + 1) = -0.5 * DXII
            BB(2, 11, I + 1) = EK
            AA(2, 2, I + 1) = GJ
            AA(2, 5, I + 1) = -0.5 * DXII
            AA(2, 11, I + 1) = -EK
            !
            !------ z-deflection angle
            RR(3, I + 1) = EIXX * (TZ2 - TZ1)&
                    - EIXZ * (TX2 - TX1) - (MZ2 + MZ1) * 0.5 * DXII
            !
            BB(3, 1, I + 1) = EIXZ
            BB(3, 2, I + 1) = EIXX_B * (TZ2 - TZ1)&
                    - EIXZ_B * (TX2 - TX1)
            BB(3, 3, I + 1) = -EIXX
            BB(3, 6, I + 1) = -0.5 * DXII
            AA(3, 1, I + 1) = -EIXZ
            AA(3, 2, I + 1) = EIXX_B * (TZ2 - TZ1)&
                    - EIXZ_B * (TX2 - TX1)
            AA(3, 3, I + 1) = EIXX
            AA(3, 6, I + 1) = -0.5 * DXII
            !
            !
            !------ x-deflection
            RR(10, I + 1) = -WX2 + WX1 + (TZ2 + TZ1) * 0.5 * DXII
            BB(10, 3, I + 1) = 0.5 * DXII
            BB(10, 10, I + 1) = 1.0
            AA(10, 3, I + 1) = 0.5 * DXII
            AA(10, 10, I + 1) = -1.0
            !
            !------ y-deflection
            RR(11, I + 1) = -WY2 + WY1 + (SY2 + SY1) * 0.5 * DXII / EA
            BB(11, 5, I + 1) = 0.5 * DXII / EA
            BB(11, 11, I + 1) = 1.0
            AA(11, 5, I + 1) = 0.5 * DXII / EA
            AA(11, 11, I + 1) = -1.0
            !
            !------ z-deflection
            RR(12, I + 1) = -WZ2 + WZ1 + (TX2 + TX1) * 0.5 * DXII
            BB(12, 1, I + 1) = 0.5 * DXII
            BB(12, 12, I + 1) = 1.0
            AA(12, 1, I + 1) = 0.5 * DXII
            AA(12, 12, I + 1) = -1.0
            !
        end do
        !
        !---- set tip  M,S  to zero
        I = II + 1
        AA(4, 4, I) = 1.0
        AA(5, 5, I) = 1.0
        AA(6, 6, I) = 1.0
        AA(7, 7, I) = 1.0
        AA(8, 8, I) = 1.0
        AA(9, 9, I) = 1.0
        !
        !
        CALL B12SOL(AA, BB, CC, RR, II + 1)
        !
        !      do i=1, ii+1
        !        write(*,6666) i, (rr(k,i),k=1, 9)
        ! 6666   format(1x,i2,9f8.3)
        ! end do
        !
        RMAX = 0.0
        RMS = 0.0
        !
        !---- set under-relaxation factors
        do K = 1, 12
            RLXR(K) = 1.0
        end do
        !
        do I = 1, II + 1
            do K = 1, 12
                IF(RLXR(K) * RR(K, I) > RRLIM(K)) RLXR(K) = RRLIM(K) / RR(K, I)
                IF(RLXR(K) * RR(K, I) < -RRLIM(K)) RLXR(K) = -RRLIM(K) / RR(K, I)
                !
                RMAX = MAX(RMAX, ABS(RR(K, I) / RRLIM(K)))
                RMS = RMS + (RR(K, I) / RRLIM(K))**2
            end do
        end do
        !
        RMS = SQRT(RMS / FLOAT(9 * II))
        !
        !---- set minimum under-relaxation factor over all variables
        RLX = 1.0
        do K = 1, 12
            RLX = AMIN1(RLX, RLXR(K))
        end do
        !
        !---- update solution
        do I = 1, II + 1
            TX(I) = TX(I) - RLX * RR(1, I)
            TY(I) = TY(I) - RLX * RR(2, I)
            TZ(I) = TZ(I) - RLX * RR(3, I)
            MOMX(I) = MOMX(I) - RLX * RR(4, I)
            MOMY(I) = MOMY(I) - RLX * RR(5, I)
            MOMZ(I) = MOMZ(I) - RLX * RR(6, I)
            SHRX(I) = SHRX(I) - RLX * RR(7, I)
            SHRY(I) = SHRY(I) - RLX * RR(8, I)
            SHRZ(I) = SHRZ(I) - RLX * RR(9, I)
            WX(I) = WX(I) - RLX * RR(10, I)
            WY(I) = WY(I) - RLX * RR(11, I)
            WZ(I) = WZ(I) - RLX * RR(12, I)
            !        WRITE(*,*)
            !        WRITE(*,*) I
            !        WRITE(*,1200) (RR(K,I),K=1,12)
            ! 1200   FORMAT( 4(1X, 3E12.4 /) )
        end do
        !
        !
        !c      WRITE(*,1250) (RLXR(K), K=1, 12)
        !c 1250 FORMAT(1X, 11F8.3)
        !
        WRITE(*, 1800) ITER, RMAX, RMS, RLX
        1800 FORMAT(1X, I3, '   max:', E9.3, '   rms:', E9.3, '   RLX =', F7.4)
        !
        IF(RMAX <= EPS) GO TO 101
        !
    end do
    WRITE(*, *) 'STCALC: Convergence failed.  Continuing ...'
    !
    101  CONTINUE
    !
    !---- integrate towards tip for X displacements
    !      I = 1
    !      WX(I) = 0.0
    !      do I=1, II
    !        WX(I+1) =  WX(I)  -  (  TZ(I) +   TZ(I+1))*0.5 * DXII
    ! end do
    !
    RETURN
END
! STCALC



SUBROUTINE STADD
    USE common
    IMPLICIT REAL (M)
    !------------------------------------------------------
    !     Adds on structural twist to static blade angles
    !------------------------------------------------------
    !
    do I = 1, II
        BETA(I) = BETA0(I) + (TY(I) + TY(I + 1)) * 0.5
    end do
    !
    WRITE(*, 1000) (BETA(II) - BETA0(II)) * 180.0 / PI
    !
    CONV = .FALSE.
    RETURN
    !
    1000 FORMAT(/' New working blade angles set.'&
            /' Tip angle deflection =', F8.3, '  deg.')
END
! STADD


SUBROUTINE STSET
    USE common
    IMPLICIT REAL (M)
    !------------------------------------------------------
    !     Removes structural twist to get static blade angles
    !------------------------------------------------------
    !
    do I = 1, II
        BETA0(I) = BETA(I) - (TY(I) + TY(I + 1)) * 0.5
    end do
    !
    WRITE(*, 1000) (BETA(II) - BETA0(II)) * 180.0 / PI
    !
    CONV = .FALSE.
    RETURN
    !
    1000 FORMAT(/' New static blade angles set.'&
            /' Tip angle deflection =', F8.3, '  deg.')
END
! STSET



SUBROUTINE STWRIT(LU)
    USE common
    IMPLICIT REAL (M)
    !---------------------------------------------
    !     Dumps blade force output to unit LU
    !---------------------------------------------
    !
    RTD = 180.0 / PI
    !
    IADD = 1
    IF(LU == LUWRIT) IADD = INCR
    !
    WRITE(LU, 1020)
    !
    MOMREF = RHO * VEL**2 * RAD**3
    !
    !--- Deflections, moments and forces on blade beam
    do I = 1, II, IADD
        !
        !********* still need to be averaged to i+1/2
        !
        !--- deflections
        WXA = WX(I)
        WYA = WY(I)
        WZA = WZ(I)
        !--- angular deflections (deg)
        TXA = TX(I) * RTD
        TYA = TY(I) * RTD
        TZA = TZ(I) * RTD
        !--- bending moments
        MXA = MOMX(I) * RHO * VEL**2 * RAD**3
        MYA = MOMY(I) * RHO * VEL**2 * RAD**3
        MZA = MOMZ(I) * RHO * VEL**2 * RAD**3
        !--- shear forces
        SXA = SHRX(I) * RHO * VEL**2 * RAD**2
        SYA = SHRY(I) * RHO * VEL**2 * RAD**2
        SZA = SHRZ(I) * RHO * VEL**2 * RAD**2
        !
        WRITE(LU, 1035) I, XI(I), WXA, WZA, TYA, MZA, MXA, MYA, SYA, SXA, SZA
    end do
    !
    !....................................................................
    !
    1020 FORMAT(&
            /'  i    r/R     u/R     w/R     t         Mz          Mx'&
            '           T            P            Sx           Sz'&
            /'                             (deg)      (N-m)       (N-m)'&
            '       (N-m)         (N)          (N)          (N)')
    !
    1035 FORMAT(1X, &
            I2, F7.3, F8.4, F8.4, F7.2, 6(1X, G12.4))
    !
    !c  i    r/R     u/R     w/R     t         Mz          Mx           T            P            Sx           Sz
    !c                            (deg)      (N-m)       (N-m)       (N-m)         (N)          (N)          (N)
    !c  1  0.307  0.0000  0.0000   0.00   0.1338       0.5678E-01  -0.1882        529.7        3.883       -1.804
    !cXiifffffffFFFFFFFFffffffffFFFFFFFXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGG
    !
    !
    !--- Display the strain components on the blade beam
    COSR = COS(RAKE)
    WRITE(LU, 2020)
    !
    do I = 1, II, IADD
        !
        RST = RSTB(I) / RAD
        DXII = DXI(I) / COSR
        !
        !------ bending strains
        EX = RST * (TZ(I + 1) - TZ(I)) / DXII * 1000.0
        EZ = -RST * (TX(I + 1) - TX(I)) / DXII * 1000.0
        !
        !------ extensional strain
        EY = (WY(I + 1) - WY(I)) / DXII * 1000.0
        !------ torsional shear
        GT = RST * (TY(I + 1) - TY(I)) / DXII * 1000.0
        !------ max normal strain
        EMAX = SQRT(EX**2 + EZ**2) + EY
        !
        WRITE(LU, 2030) I, XI(I), EX, EZ, EY, EMAX, GT
    end do
    !
    RETURN

    !
    2020 FORMAT(&
            /' i    r/R      Ex       Ez       Ey      Emax'&
            '      g     x 1000')
    !          10  0.425   10.002   14.002   20.203   12.000   13.450
    2030 FORMAT(1X, &
            I2, F7.3, 5F9.4)
    !
END
! STWRIT



SUBROUTINE B12SOL(A, B, C, R, II)
    DIMENSION A(12, 12, II), B(12, 12, II), C(12, 12, II)
    DIMENSION R(12, 1, II)
    !-------------------------------------------------------
    !      Solves the 12x12 block-tridiagonal Newton system
    !      by a standard block elimination scheme.
    !      The solutions are returned in the R vectors.
    !
    !      |A C      ||d|   |R..|
    !      |B A C    ||d|   |R..|
    !      |  B . .  ||.| = |R..|
    !      |    . . C||.|   |R..|
    !      |      B A||d|   |R..|
    !-------------------------------------------------------
    !
    NRHS = 1
    !
    !CC** Forward sweep: Elimination of lower block diagonal (B's).
    do I = 1, II
        !
        IM = I - 1
        !
        !------ don't eliminate first B block because it doesn't exist
        IF(I == 1) GO TO 12
        !
        !------ eliminate Bi block, thus modifying Ai and Ci blocks
        do K = 1, 12
            do L = 1, 12
                A(K, L, I) = A(K, L, I)&
                        - (B(K, 1, I) * C(1, L, IM)&
                                + B(K, 2, I) * C(2, L, IM)&
                                + B(K, 3, I) * C(3, L, IM)&
                                + B(K, 4, I) * C(4, L, IM)&
                                + B(K, 5, I) * C(5, L, IM)&
                                + B(K, 6, I) * C(6, L, IM)&
                                + B(K, 7, I) * C(7, L, IM)&
                                + B(K, 8, I) * C(8, L, IM)&
                                + B(K, 9, I) * C(9, L, IM)&
                                + B(K, 10, I) * C(10, L, IM)&
                                + B(K, 11, I) * C(11, L, IM)&
                                + B(K, 12, I) * C(12, L, IM))
            end do
            do L = 1, NRHS
                R(K, L, I) = R(K, L, I)&
                        - (B(K, 1, I) * R(1, L, IM)&
                                + B(K, 2, I) * R(2, L, IM)&
                                + B(K, 3, I) * R(3, L, IM)&
                                + B(K, 4, I) * R(4, L, IM)&
                                + B(K, 5, I) * R(5, L, IM)&
                                + B(K, 6, I) * R(6, L, IM)&
                                + B(K, 7, I) * R(7, L, IM)&
                                + B(K, 8, I) * R(8, L, IM)&
                                + B(K, 9, I) * R(9, L, IM)&
                                + B(K, 10, I) * R(10, L, IM)&
                                + B(K, 11, I) * R(11, L, IM)&
                                + B(K, 12, I) * R(12, L, IM))
            end do
        end do
        !
        !                                                              -1
        !CC---- multiply Ci block and righthand side Ri vectors by (Ai)
        !       using Gaussian elimination.
        !
        !cc        CALL SHOBLK(12,I,A(1,1,I))
        !
        12   do KPIV = 1, 11
            KP1 = KPIV + 1
            !
            !-------- find max pivot index KX
            KX = KPIV
            do K = KP1, 12
                IF(ABS(A(K, KPIV, I)) - ABS(A(KX, KPIV, I))) 131, 131, 1311
                1311        KX = K
            131 end do
            !
            IF(A(KX, KPIV, I) == 0.0) THEN
                WRITE(*, *) 'Singular A block, i = ', I
                STOP
            ENDIF
            !
            PIVOT = 1.0 / A(KX, KPIV, I)
            !
            !-------- switch pivots
            A(KX, KPIV, I) = A(KPIV, KPIV, I)
            !
            !-------- switch rows & normalize pivot row
            do L = KP1, 12
                TEMP = A(KX, L, I) * PIVOT
                A(KX, L, I) = A(KPIV, L, I)
                A(KPIV, L, I) = TEMP
            end do
            !
            do L = 1, 12
                TEMP = C(KX, L, I) * PIVOT
                C(KX, L, I) = C(KPIV, L, I)
                C(KPIV, L, I) = TEMP
            end do
            !
            do L = 1, NRHS
                TEMP = R(KX, L, I) * PIVOT
                R(KX, L, I) = R(KPIV, L, I)
                R(KPIV, L, I) = TEMP
            end do
            !
            !-------- forward eliminate everything
            do K = KP1, 12
                ATMP = -A(K, KPIV, I)
                IF(ATMP == 0.0) GO TO 135
                do L = KP1, 12
                    A(K, L, I) = A(K, L, I) + ATMP * A(KPIV, L, I)
                end do
                C(K, 1, I) = C(K, 1, I) + ATMP * C(KPIV, 1, I)
                C(K, 2, I) = C(K, 2, I) + ATMP * C(KPIV, 2, I)
                C(K, 3, I) = C(K, 3, I) + ATMP * C(KPIV, 3, I)
                C(K, 4, I) = C(K, 4, I) + ATMP * C(KPIV, 4, I)
                C(K, 5, I) = C(K, 5, I) + ATMP * C(KPIV, 5, I)
                C(K, 6, I) = C(K, 6, I) + ATMP * C(KPIV, 6, I)
                C(K, 7, I) = C(K, 7, I) + ATMP * C(KPIV, 7, I)
                C(K, 8, I) = C(K, 8, I) + ATMP * C(KPIV, 8, I)
                C(K, 9, I) = C(K, 9, I) + ATMP * C(KPIV, 9, I)
                C(K, 10, I) = C(K, 10, I) + ATMP * C(KPIV, 10, I)
                C(K, 11, I) = C(K, 11, I) + ATMP * C(KPIV, 11, I)
                C(K, 12, I) = C(K, 12, I) + ATMP * C(KPIV, 12, I)
                do L = 1, NRHS
                    R(K, L, I) = R(K, L, I) + ATMP * R(KPIV, L, I)
                end do
            135 end do
            !
        end do
        !
        !------ solve for last row
        IF(A(12, 12, I) == 0.0) THEN
            WRITE(*, *) 'Singular A block, i = ', I
            STOP
        ENDIF
        PIVOT = 1.0 / A(12, 12, I)
        C(12, 1, I) = C(12, 1, I) * PIVOT
        C(12, 2, I) = C(12, 2, I) * PIVOT
        C(12, 3, I) = C(12, 3, I) * PIVOT
        C(12, 4, I) = C(12, 4, I) * PIVOT
        C(12, 5, I) = C(12, 5, I) * PIVOT
        C(12, 6, I) = C(12, 6, I) * PIVOT
        C(12, 7, I) = C(12, 7, I) * PIVOT
        C(12, 8, I) = C(12, 8, I) * PIVOT
        C(12, 9, I) = C(12, 9, I) * PIVOT
        C(12, 10, I) = C(12, 10, I) * PIVOT
        C(12, 11, I) = C(12, 11, I) * PIVOT
        C(12, 12, I) = C(12, 12, I) * PIVOT
        do L = 1, NRHS
            R(12, L, I) = R(12, L, I) * PIVOT
        end do
        !
        !------ back substitute everything
        do KPIV = 10, 1, -1
            KP1 = KPIV + 1
            do K = KP1, 12
                C(KPIV, 1, I) = C(KPIV, 1, I) - A(KPIV, K, I) * C(K, 1, I)
                C(KPIV, 2, I) = C(KPIV, 2, I) - A(KPIV, K, I) * C(K, 2, I)
                C(KPIV, 3, I) = C(KPIV, 3, I) - A(KPIV, K, I) * C(K, 3, I)
                C(KPIV, 4, I) = C(KPIV, 4, I) - A(KPIV, K, I) * C(K, 4, I)
                C(KPIV, 5, I) = C(KPIV, 5, I) - A(KPIV, K, I) * C(K, 5, I)
                C(KPIV, 6, I) = C(KPIV, 6, I) - A(KPIV, K, I) * C(K, 6, I)
                C(KPIV, 7, I) = C(KPIV, 7, I) - A(KPIV, K, I) * C(K, 7, I)
                C(KPIV, 8, I) = C(KPIV, 8, I) - A(KPIV, K, I) * C(K, 8, I)
                C(KPIV, 9, I) = C(KPIV, 9, I) - A(KPIV, K, I) * C(K, 9, I)
                C(KPIV, 10, I) = C(KPIV, 10, I) - A(KPIV, K, I) * C(K, 10, I)
                C(KPIV, 11, I) = C(KPIV, 11, I) - A(KPIV, K, I) * C(K, 11, I)
                C(KPIV, 12, I) = C(KPIV, 12, I) - A(KPIV, K, I) * C(K, 12, I)
                do L = 1, NRHS
                    R(KPIV, L, I) = R(KPIV, L, I) - A(KPIV, K, I) * R(K, L, I)
                end do
            end do
        end do
    end do
    !
    !CC** Backward sweep: Back substitution using upper block diagonal (Ci's).
    do I = II - 1, 1, -1
        IP = I + 1
        do L = 1, NRHS
            do K = 1, 12
                R(K, L, I) = R(K, L, I)&
                        - (R(1, L, IP) * C(K, 1, I)&
                                + R(2, L, IP) * C(K, 2, I)&
                                + R(3, L, IP) * C(K, 3, I)&
                                + R(4, L, IP) * C(K, 4, I)&
                                + R(5, L, IP) * C(K, 5, I)&
                                + R(6, L, IP) * C(K, 6, I)&
                                + R(7, L, IP) * C(K, 7, I)&
                                + R(8, L, IP) * C(K, 8, I)&
                                + R(9, L, IP) * C(K, 9, I)&
                                + R(10, L, IP) * C(K, 10, I)&
                                + R(11, L, IP) * C(K, 11, I)&
                                + R(12, L, IP) * C(K, 12, I))
            end do
        end do
    end do
    !
    RETURN
END
! B12SOL