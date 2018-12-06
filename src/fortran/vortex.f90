!***********************************************************************
!    Module:  vortex.f
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

SUBROUTINE VRTXCO(IMAX, II, NBLDS, LDUCT, RAKE, &
        XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
    !
    PARAMETER (NTDIM = 5000)
    DIMENSION XI(IMAX), XV(IMAX), GAM(IMAX)
    DIMENSION VIND_ADW(3, IMAX), VIND_GAM(3, IMAX, IMAX)
    !
    DIMENSION A(3), B(3), UVW(3), UVW_A(3, 3), UVW_B(3, 3)
    DIMENSION VSUM(3), VADW(3)
    DIMENSION THETSPC(5000)
    !
    LOGICAL LDUCT
    !-----------------------------------------
    !     Calculates "Vortex Momentum"
    !     Gamma-swirl influence coefficients
    !
    !     Input:
    !       IMAX         array dimension
    !       II           number of radial points on blade (circulation stations)
    !       NN           number of Fourier harmonics
    !       NBLDS        number of blades
    !       LDUCT        T for duct outer BC
    !       RAKE         blade rake angle from Y axis in XY plane
    !       XI(i)        r/R control point coordinate array
    !       XV(i)        r/R vortex  leg   coordinate array
    !       GAM(i)       circulation array
    !       ADW          wake advance ratio  V/wR
    !
    !     Output:
    !       VT_GAM(i,j)  sensitivity of swirl velocity to circulation
    !       VT_ADW(i)    sensitivity of swirl velocity to wake advance ratio
    !-----------------------------------------
    BLDS = FLOAT(NBLDS)
    !
    PI = 4.0 * ATAN(1.0)
    !
    XI0 = XV(1)
    XITIP = XV(II + 1)
    TANRAK = TAN(RAKE)

    DO I = 1, II
        DO J = 1, II
            VIND_GAM(1, I, J) = 0.
            VIND_GAM(2, I, J) = 0.
            VIND_GAM(3, I, J) = 0.
        ENDDO
        VIND_ADW(1, I) = 0.
        VIND_ADW(2, I) = 0.
        VIND_ADW(3, I) = 0.
    ENDDO
    !
    !--- Set up variable theta spacing for near, intermediate and far field
    DTH1 = PI / 60.
    RAD1 = 2.0
    THET1 = RAD1 / ADW
    DTH2 = PI / 20.
    RAD2 = 4.0
    THET2 = RAD2 / ADW
    DTH3 = PI / 8.
    RAD3 = 50.0
    THET3 = RAD3 / ADW
    !
    N1 = IFIX(2.0 * (THET1) / (DTH2 + DTH1))
    N2 = IFIX(2.0 * (THET2 - THET1) / (DTH3 + DTH2))
    DDTH1 = (DTH2 - DTH1) / FLOAT(N1 - 1)
    DDTH2 = (DTH3 - DTH2) / FLOAT(N2 - 1)
    !
    THET = 0.0
    DTH = DTH1
    DO I = 1, NTDIM
        IF(THET < THET1) THEN
            THETSPC(I) = THET
            THET = THET + DTH
            DTH = DTH + DDTH1
        ELSEIF(THET < THET2) THEN
            THETSPC(I) = THET
            THET = THET + DTH
            DTH = DTH + DDTH2
        ELSEIF(THET < THET3) THEN
            THETSPC(I) = THET
            DTH = DTH3
            THET = THET + DTH
        ELSE
            NTHET = I - 1
            GO TO 100
        ENDIF
    END DO
    WRITE(*, *) 'Too many vortex segments for spacing array'
    NTHET = NTDIM
    100  CONTINUE
    !
    IF(LDUCT) THEN
        !----- use simple mean swirl to get swirl at blade
        DO I = 1, II
            DO J = 1, II
                VIND_GAM(1, I, J) = 0.
                VIND_GAM(2, I, J) = 0.
                VIND_GAM(3, I, J) = 0.
            ENDDO
            VIND_GAM(3, I, I) = BLDS / (4.0 * PI * XI(I))
            VIND_ADW(3, I) = 0.0
            VIND_ADW(2, I) = 0.0
            VIND_GAM(1, I, I) = VIND_GAM(3, I, I) * XI(I) / ADW
            VIND_ADW(1, I) = -VIND_GAM(1, I, I) * GAM(I) / ADW
        ENDDO
        !
    ELSE
        !
        !----- Do a discrete vortex integration of slipstream vortices
        DTBLD = 2.0 * PI / FLOAT(NBLDS)
        write(*, 20) nblds * nthet
        20    format(/'Vortex points/radial station = ', I6)
        !
        !--- velocity influences for point - R0
        DO I = 1, II
            R0X = XI(I) * TANRAK
            R0Y = XI(I)
            R0Z = 0.0
            !         write(*,*) 'CP ',r0x,r0y,r0z
            !
            VIND_ADW(1, I) = 0.0
            VIND_ADW(2, I) = 0.0
            VIND_ADW(3, I) = 0.0
            !
            !----- For each vortex trailing leg (II+1 legs starting at XV(J))
            DO J = 1, II + 1
                !
                VSUM(1) = 0.0
                VSUM(2) = 0.0
                VSUM(3) = 0.0
                VADW(1) = 0.0
                VADW(2) = 0.0
                VADW(3) = 0.0
                !
                RV = XV(J)
                XXV = RV * TANRAK
                !
                !----- For each blade
                THETOFF = 0.0
                DO N = 1, NBLDS
                    !
                    !----- For angles around helix to the far-field
                    THET1 = THETSPC(1)
                    R1X = XXV + THET1 * XITIP * ADW
                    R1Y = RV * COS(THET1 + THETOFF)
                    R1Z = RV * SIN(THET1 + THETOFF)
                    R1_ADW = THET1 * XITIP
                    !
                    DO L = 1, NTHET - 1
                        !
                        THET2 = THETSPC(L + 1)
                        R2X = XXV + THET2 * XITIP * ADW
                        R2Y = RV * COS(THET2 + THETOFF)
                        R2Z = RV * SIN(THET2 + THETOFF)
                        R2_ADW = THET2 * XITIP
                        !
                        A(1) = R1X - R0X
                        A(2) = R1Y - R0Y
                        A(3) = R1Z - R0Z
                        B(1) = R2X - R0X
                        B(2) = R2Y - R0Y
                        B(3) = R2Z - R0Z
                        CALL VORSEGVEL(A, B, UVW, UVW_A, UVW_B)
                        !
                        VSUM(1) = VSUM(1) + UVW(1)
                        VSUM(2) = VSUM(2) + UVW(2)
                        VSUM(3) = VSUM(3) + UVW(3)
                        !
                        VADW(1) = VADW(1) + UVW_A(1, 1) * R1_ADW&
                                + UVW_B(1, 1) * R2_ADW
                        VADW(2) = VADW(2) + UVW_A(2, 1) * R1_ADW&
                                + UVW_B(2, 1) * R2_ADW
                        VADW(3) = VADW(3) + UVW_A(3, 1) * R1_ADW&
                                + UVW_B(3, 1) * R2_ADW
                        !
                        THET1 = THET2
                        R1X = R2X
                        R1Y = R2Y
                        R1Z = R2Z
                        R1_ADW = R2_ADW
                        !
                        !               if(i == 1) then
                        !                write(88,10) r1x,r1y,r1z
                        !                write(*,10) 'r1 ',i,j,n,l,r2x,r2y,r2z
                        !               endif
                        10            format(3f10.3)
                        12            format(A, 2I6, 2f10.3)

                    END DO ! L loop
                    !
                    THETOFF = THETOFF + DTBLD
                    !
                END DO ! N loop
                !
                VSUM(3) = -VSUM(3)
                VADW(3) = -VADW(3)
                !---- alternate + and - influence for each vortex line into velocity
                !     influence matrix
                !
                !---Open wake, interdigitate all vortex lines
                IF(J <= II) THEN
                    VIND_GAM(1, I, J) = -VSUM(1)
                    VIND_GAM(2, I, J) = -VSUM(2)
                    VIND_GAM(3, I, J) = -VSUM(3)
                    VIND_ADW(1, I) = VIND_ADW(1, I) - VADW(1) * GAM(J)
                    VIND_ADW(2, I) = VIND_ADW(2, I) - VADW(2) * GAM(J)
                    VIND_ADW(3, I) = VIND_ADW(3, I) - VADW(3) * GAM(J)
                ENDIF
                IF(J > 1) THEN
                    VIND_GAM(1, I, J - 1) = VIND_GAM(1, I, J - 1) + VSUM(1)
                    VIND_GAM(2, I, J - 1) = VIND_GAM(2, I, J - 1) + VSUM(2)
                    VIND_GAM(3, I, J - 1) = VIND_GAM(3, I, J - 1) + VSUM(3)
                    VIND_ADW(1, I) = VIND_ADW(1, I) + VADW(1) * GAM(J - 1)
                    VIND_ADW(2, I) = VIND_ADW(2, I) + VADW(2) * GAM(J - 1)
                    VIND_ADW(3, I) = VIND_ADW(3, I) + VADW(3) * GAM(J - 1)
                ENDIF
                !
            END DO ! J loop
            !
        END DO ! I loop
        !
    ENDIF
    !
    RETURN
END
! VRTXCO


SUBROUTINE VORSEGVEL(A, B, UVW, UVW_A, UVW_B)
    !-------------------------------------------------------------------
    !     Calculates the velocity induced by a vortex segment
    !     of unit strength, with no core radius.
    !
    !     The point where the velocity is calculated is at 0,0,0.
    !
    !     Positive circulation is by righthand rule from A to B.
    !
    !  Input:
    !     A(3)    coordinates of vertex #1 of the vortex
    !     B(3)    coordinates of vertex #2 of the vortex
    !
    !  Output:
    !     UVW(3)      induced velocity
    !     UVW_A(3,3)  dUVW/dA  sensitivity
    !     UVW_B(3,3)  dUVW/dB  sensitivity
    !
    !-------------------------------------------------------------------
    DIMENSION A(3), B(3), UVW(3), UVW_A(3, 3), UVW_B(3, 3)
    !
    !
    DIMENSION AXB(3), AXB_A(3, 3), AXB_B(3, 3)
    !
    DATA PI4  / 12.56637062 /
    !
    ASQ = A(1)**2 + A(2)**2 + A(3)**2
    BSQ = B(1)**2 + B(2)**2 + B(3)**2
    !
    AMAG = SQRT(ASQ)
    BMAG = SQRT(BSQ)
    !
    DO K = 1, 3
        UVW(K) = 0.
        DO L = 1, 3
            UVW_A(K, L) = 0.
            UVW_B(K, L) = 0.
        ENDDO
    ENDDO
    !
    !---- contribution from the vortex leg
    IF (AMAG * BMAG /= 0.0) THEN
        AXB(1) = A(2) * B(3) - A(3) * B(2)
        AXB(2) = A(3) * B(1) - A(1) * B(3)
        AXB(3) = A(1) * B(2) - A(2) * B(1)
        !
        AXB_A(1, 1) = 0.0
        AXB_A(1, 2) = B(3)
        AXB_A(1, 3) = -B(2)
        AXB_A(2, 1) = -B(3)
        AXB_A(2, 2) = 0.0
        AXB_A(2, 3) = B(1)
        AXB_A(3, 1) = B(2)
        AXB_A(3, 2) = -B(1)
        AXB_A(3, 3) = 0.0
        !
        AXB_B(1, 1) = 0.0
        AXB_B(1, 2) = -A(3)
        AXB_B(1, 3) = A(2)
        AXB_B(2, 1) = A(3)
        AXB_B(2, 2) = 0.0
        AXB_B(2, 3) = -A(1)
        AXB_B(3, 1) = -A(2)
        AXB_B(3, 2) = A(1)
        AXB_B(3, 3) = 0.0
        !
        ADB = A(1) * B(1) + A(2) * B(2) + A(3) * B(3)
        !
        DEN = AMAG * BMAG + ADB
        DEN_ASQ = 0.5 * BMAG / AMAG
        DEN_BSQ = 0.5 * AMAG / BMAG
        !
        T = (1.0 / AMAG + 1.0 / BMAG) / DEN
        !
        T_ADB = -T / DEN
        T_ASQ = -T / DEN * DEN_ASQ - 0.5 / (DEN * AMAG * ASQ)
        T_BSQ = -T / DEN * DEN_BSQ - 0.5 / (DEN * BMAG * BSQ)
        !
        DO K = 1, 3
            UVW(K) = AXB(K) * T
            !
            DO L = 1, 3
                UVW_A(K, L) = AXB(K) * T_ASQ * A(L) * 2.0&
                        + AXB(K) * T_ADB * B(L)&
                        + AXB_A(K, L) * T
                UVW_B(K, L) = AXB(K) * T_BSQ * B(L) * 2.0&
                        + AXB(K) * T_ADB * A(L)&
                        + AXB_B(K, L) * T
            ENDDO
        ENDDO
    ENDIF
    !
    DO K = 1, 3
        UVW(K) = UVW(K) / PI4
        DO L = 1, 3
            UVW_A(K, L) = UVW_A(K, L) / PI4
            UVW_B(K, L) = UVW_B(K, L) / PI4
        ENDDO
    ENDDO
    !
    RETURN
END
! VORVEL
