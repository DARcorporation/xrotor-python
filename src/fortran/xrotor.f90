!***********************************************************************
!    Module:  xrotor.f
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
PROGRAM XROTOR
    !
    !--- module statement for Windoze DVFortran
    !cc   USE DFLIB
    !
    USE common
    IMPLICIT REAL (M)
    CHARACTER*7 COMAND
    CHARACTER*128 COMARG
    !
    DIMENSION IINPUT(20)
    DIMENSION RINPUT(20)
    LOGICAL ERROR
    !
    !====================================================
    !
    !      Interactive Design and Analysis Program
    !          for Free-tip and Ducted Rotors
    !
    !      October 1992
    !      Copyright Mark Drela
    !      Versions 6.7-7.x
    !      Copyright Mark Drela, Harold Youngren
    !
    !====================================================
    !
    VERSION = 7.55
    !
    !---- logical unit numbers
    LUREAD = 5    ! terminal read
    LUWRIT = 6    ! terminal write
    LUTEMP = 3    ! general-use disk I/O unit  (usually available)
    LUSAVE = 4    ! save file                  (usually open)
    !
    !
    WRITE(*, 1000) VERSION
    !
    CALL INIT
    !
    FNAME = ' '
    !--- Get command line args (if present)
    NARG = IARGC()
    !
    IF(NARG > 0) CALL GETARG(1, FNAME)
    IF(FNAME(1:1) /= ' ') CALL LOAD(FNAME)
    !
    FNAME = ' '
    IF(NARG > 1) CALL GETARG(2, FNAME)
    IF(FNAME(1:1) /= ' ') THEN
        NCASE = 0
        OPEN(LUTEMP, FILE = FNAME, STATUS = 'OLD', ERR = 2)
        CALL GETCAS(LUTEMP, NPARX, NCASE, CASPAR)
        CLOSE(LUTEMP)
        IF(NCASE > 0) THEN
            KF = INDEX(FNAME, ' ') - 1
            WRITE(*, *) 'Operating cases read from file  ', &
                    FNAME(1:KF), ' ...'
            CALL SHOCAS(LUWRIT, NPARX, NCASE, CASPAR, RAD, NAME)
        ENDIF
        2      CONTINUE
    ENDIF
    !
    WRITE(*, 1100)
    !
    900  CONTINUE
    CALL ASKC(' XROTOR^', COMAND, COMARG)
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
    GREEK = .TRUE.
    IF(COMAND == '    ') GO TO 900
    IF(COMAND == '?   ') WRITE(*, 1100)
    IF(COMAND == '?   ') GO TO 900
    IF(COMAND == 'QUIT') THEN
        STOP
    ENDIF
    !
    IF(COMAND == 'OPER') CALL OPER
    IF(COMAND == 'BEND') CALL BEND
    IF(COMAND == 'LOAD') CALL LOAD(COMARG)
    IF(COMAND == 'NOIS') CALL NOISE
    IF(COMAND == 'DISP') GO TO 100
    IF(GREEK) WRITE(*, 1050) COMAND
    GO TO 900
    !
    !---------------------------------------------------------------------
    100 CALL OUTPUT(LUWRIT)
    GO TO 900
    !
    !.....................................................................
    !
    1000 FORMAT(/' ========================='&
            /'    XROTOR Version', F5.2&
            /' =========================')
    1050 FORMAT(1X, A4, ' command not recognized.  Type a "?" for list')
    1100 FORMAT(&
            /'   QUIT   Exit program'&
            /'  .OPER   Calculate off-design operating points'&
            /'  .BEND   Calculate structural loads and deflections'&
            /'  .NOIS   Calculate and plot acoustic signature'&
            /'   LOAD f Read rotor from restart file'&
            /'   DISP   Display current design point')
END
! XROTOR


SUBROUTINE INIT
    USE common
    IMPLICIT REAL (M)
    !--------------------------------------
    !     Initializes everything
    !--------------------------------------
    !
    GREEK = .FALSE.
    !
    !
    !---- XROTOR defaults
    URDUCT = 1.0
    !
    CALL SETDEF
    !
    IF(DUCT) THEN
        WRITE(*, *) 'Aprop/Aexit initialized to 1.0'
        URDUCT = 1.0
    ENDIF
    !
    XINF = 3.0        ! r/R at which BC at infinity is applied
    NN = 32           ! number of perturbation potential harmonics
    IINF = II + II / 2  ! number of discrete potential harmonic stations
    CONV = .FALSE.   ! operating point solution existence flag
    LSTRUC = .FALSE.  ! indicates if structural properties are available
    !
    NAME = ' '
    SAVFIL = ' '
    !
    !---- acceleration due to gravity for scaling centrifugal blade tension (m/s^2)
    GEE = 9.81
    !
    !---- ADW factor (multiplies TINV/PINV in ADW calculation)
    ADWFCTR = 1.0
    !
    IF(II > IX) STOP 'Array overflow.  IX too small'
    IF(IINF > JX) STOP 'Array overflow.  JX too small'
    !
    !---- actual-rotor radius is always 1 (non-dimensionalized with itself)
    XITIP = 1.0
    !
    !---- default nacelle, wake perturbation velocities (non-existent)
    DO I = 1, II
        UBODY(I) = 0.
    END DO
    !
    !---- no slipstream velocity profiles
    NADD = 0
    !
    !---- number of defined cases
    NCASE = 0
    KCASE = 0
    !
    !---- max number of iterations for design, analysis
    NITERD = 40
    NITERA = 40
    !
    !---- do not initialize rotor at each design cycle
    LDESINI = .FALSE.
    !
    !---- do initialize rotor at each design cycle
    LOPRINI = .TRUE.
    !
    !---- no engine load line to start
    LPWRVAR = .FALSE.
    NPWRVAR = 0
    !
    !---- no rotor yet
    LROTOR = .FALSE.
    DO I = 1, IX
        IAERO(I) = 0
    END DO
    !
    RETURN
END
! INIT



SUBROUTINE SETDEF
    USE common
    IMPLICIT REAL (M)
    !
    !---- hard-wired start-up defaults
    !ccIHI
    RAKE = 0.0
    !
    VEL = 1.0
    ALT = 0.0
    CALL ATMO(ALT, VSO, RHO, RMU) ! sea level atmosphere parameters
    !CC      RHO =  1.226      ! fluid density         kg/m**3
    !CC      RMU =  1.78E-05   ! dynamic viscosity     kg/m-s
    !CC      VSO =  340.0      ! speed of sound        m/s
    !
    !--- Default aero properties for section #1
    A0 = 0.           ! zero lift angle of attack   radians
    DCLDA = 6.28     ! lift curve slope            /radian
    CLMAX = 1.5     ! stall Cl
    CLMIN = -0.5     ! negative stall Cl
    DCL_STALL = 0.1 ! CL increment from incipient to total stall
    DCLDA_STALL = 0.1 ! stalled lift curve slope    /radian
    CMCON = -0.1      ! section Cm  (for pitch-axis moments)
    CDMIN = 0.013    ! minimum Cd
    CLDMIN = 0.5     ! Cl at minimum Cd
    DCDCL2 = 0.004   ! d(Cd)/d(Cl**2)
    REREF = 200000.  ! Reynolds Number at which Cd values apply
    REXP = -0.4      ! Exponent for Re scaling of Cd:  Cd ~ Re**exponent
    MCRIT = 0.8      ! Critical Mach number
    !--- Install data into aero section #1
    NAERO = 1
    XISECT = 0.0
    CALL PUTAERO(NAERO, XISECT, A0, CLMAX, CLMIN, &
            DCLDA, DCLDA_STALL, DCL_STALL, &
            CDMIN, CLDMIN, DCDCL2, CMCON, MCRIT, REREF, REXP)
    DO I = 1, IX
        IAERO(I) = 1
    END DO
    !
    XPITCH = 0.3     ! x/c location of pitch axis
    !
    II = 30         ! number of radial stations
    INCR = 2       ! radial station increment for terminal output
    IXSPAC = 2       ! r/R spacing flag
    !
    VRTX = .FALSE.  ! Vortex Wake (T)        / Graded Momentum(F) flag
    FAST = .FALSE.  ! Graded Momentum(T)     / Potential Formulation(F) flag
    FREE = .TRUE.   ! Self-deforming wake(T) / rigid wake(F) flag
    DUCT = .FALSE.  ! Ducted (T)             / Free-tip (F)  flag
    !
    TERSE = .FALSE.  ! terse-output flag
    !
    LVNORM = .TRUE.  ! flight speed used for normalization
    !
    RETURN
END
! SETDEF


SUBROUTINE ATMO(ALSPEC, VSOALT, RHOALT, RMUALT)
    !---------------------------------------------------------
    !     Returns speed of sound (VSO) in m/s, density (RHO)
    !     in kg/m^3, and dynamic viscosity (RMU) in kg/m-s
    !     of standard atmosphere at specified altitude ALSPEC
    !     (in kilometers).  If ALSPEC=-1, water properties
    !     at 15 Celsius are returned.
    !
    !     Reference:  "U.S. Standard Atmosphere", NOAA.
    !---------------------------------------------------------
    LOGICAL FIRST
    !
    PARAMETER (N = 44)
    REAL ALT(N), VSO(N), RHO(N), RMU(N)
    !
    DATA FIRST / .TRUE. /
    DATA ALT&
            / 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, &
            10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, &
            20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, &
            30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0, &
            40.0, 45.0, 60.0, 75.0 /
    DATA VSO&
            / 340.0, 336.0, 332.0, 329.0, 325.0, 320.0, 316.0, 312.0, 308.0, 304.0, &
            299.0, 295.0, 295.0, 295.0, 295.0, 295.0, 295.0, 295.0, 295.0, 295.0, &
            295.0, 295.8, 296.4, 297.1, 297.8, 298.5, 299.1, 299.8, 300.5, 301.1, &
            301.8, 302.5, 303.1, 305.0, 306.8, 308.7, 310.5, 312.3, 314.0, 316.0, &
            318.0, 355.0, 372.0, 325.0 /
    DATA RHO&
            / 1.226, 1.112, 1.007, 0.909, 0.820, 0.737, 0.660, 0.589, 0.526, 0.467, &
            0.413, 0.364, 0.311, 0.265, 0.227, 0.194, 0.163, 0.141, 0.121, 0.103, &
            .0880, .0749, .0637, .0543, .0463, .0395, .0338, .0288, .0246, .0210, &
            .0180, .0154, .0132, .0113, .0096, .0082, .0070, .0060, .0052, .0044, &
            0.004, 0.002, 3.9E-4, 8.0E-5 /
    DATA RMU&
            / 1.780, 1.749, 1.717, 1.684, 1.652, 1.619, 1.586, 1.552, 1.517, 1.482, &
            1.447, 1.418, 1.418, 1.418, 1.418, 1.418, 1.418, 1.418, 1.418, 1.418, &
            1.418, 1.427, 1.433, 1.438, 1.444, 1.449, 1.454, 1.460, 1.465, 1.471, &
            1.476, 1.481, 1.487, 1.502, 1.512, 1.532, 1.546, 1.561, 1.580, 1.600, &
            1.700, 1.912, 2.047, 1.667 /
    !
    !---- special case: Water at STP
    IF(ALSPEC == -1.0) THEN
        VSOALT = 1500.
        RHOALT = 1000.
        RMUALT = 1.15E-3
        WRITE(*, *) '                              o        '
        WRITE(*, *) 'ATMO: You are underwater at 15  Celsius'
        RETURN
    ENDIF
    !
    !---- linearly interpolate quantities from tabulated values
    do I = 2, N
        IF(ALSPEC > ALT(I)) GO TO 10
        !
        DALT = ALT(I) - ALT(I - 1)
        DVSO = VSO(I) - VSO(I - 1)
        DRHO = RHO(I) - RHO(I - 1)
        DRMU = RMU(I) - RMU(I - 1)
        !
        ALFRAC = (ALSPEC - ALT(I - 1)) / DALT
        !
        VSOALT = VSO(I - 1) + DVSO * ALFRAC
        RHOALT = RHO(I - 1) + DRHO * ALFRAC
        RMUALT = RMU(I - 1) + DRMU * ALFRAC
        RMUALT = RMUALT * 1.0E-5
        !
        RETURN
    10 end do
    !
    !
    IF(ALSPEC > ALT(N)) THEN
        WRITE(*, *) ' '
        WRITE(*, *) 'ATMO: You''re in low earth orbit.  Good luck.'
        VSOALT = VSO(N)
        RHOALT = RHO(N)
        RMUALT = RMU(N) * 1.0E-5
        RETURN
    ENDIF
    !
    !      IF(FIRST) THEN
    !       do I=1, N
    !         RHO(I) = ALOG(RHO(I))
    ! end do
    !       CALL SPLINE(VSO,VSOH,ALT,N)
    !       CALL SPLIND(RHO,RHOH,ALT,N,999.0,0.0)
    !       CALL SPLINE(RMU,RMUH,ALT,N)
    !       FIRST = .FALSE.
    !      ENDIF
    !C
    !C---- interpolate quantities from splines
    !      VSOALT = SEVAL(ALSPEC,VSO,VSOH,ALT,N)
    !      RHOALT = SEVAL(ALSPEC,RHO,RHOH,ALT,N)
    !      RMUALT = SEVAL(ALSPEC,RMU,RMUH,ALT,N) * 1.0E-5
    !      RHOALT = EXP(RHOALT)
    !C
    RETURN
END
! ATMO


SUBROUTINE FLOSHO(LU, VSO, RHO, RMU)
    DATA R, GAM / 287.0, 1.4 /
    RNU = RMU / RHO
    P = RHO * VSO**2 / GAM
    T = P / (RHO * R)
    WRITE(LU, 5000) VSO, RHO, RMU, RNU, P, T
    5000 FORMAT(/' Speed of sound (m/s):', F10.3&
            /' Density   (kg/m^3)  :', F10.5&
            /' Viscosity (kg/m-s)  :', E11.4&
            /' Kin. Visc. (m^2/s)  :', E11.4&
            //' Air pressure (Pa)   :', G13.5&
            /' Air temperature (K) :', G12.4)
    RETURN
END
! FLOSHO


SUBROUTINE REINIT
    USE common
    IMPLICIT REAL (M)
    LOGICAL YES
    !-----------------------------------------------
    !     Re-initializes advance ratio and gammas
    !-----------------------------------------------
    !
    !---- estimate reasonable advance ratio to start iterative routines
    IS = II / 2 + 1
    !---HHY had to set A0 to 0.0 as A0 is now section property
    A0 = 0.0
    ANG = BETA(IS) - A0
    !
    RPM = VEL / (RAD * ADV * PI / 30.)
    !
    ADV0 = XI(IS) * SIN(ANG) / COS(ANG)
    RPM0 = VEL / (RAD * ADV0 * PI / 30.)

    !      WRITE(*,*) 'Current    RPM ',RPM
    !      WRITE(*,*) 'Initialize RPM ',RPM0
    CALL ASKR('Enter initialization RPM?^', RPM)

    ADV = VEL / (RPM * RAD * PI / 30.)
    ADV = MAX(0.1, ADV)
    ADW = ADV
    !
    !---- Set the blade angle back to reference angle
    CALL ASKL('Restore blade angles to original?^', YES)
    IF(YES) THEN
        DO I = 1, II
            BETA0(I) = BETA(I)
        END DO
    ENDIF
    !---- calculate current operating point
    CALL APER(4, 2, .TRUE.)
    IF(CONV) CALL OUTPUT(LUWRIT)
    !
    RETURN
END
! REINIT

SUBROUTINE SETX
    USE common
    IMPLICIT REAL (M)
    !
    !-------------------------------------------------------
    !     Fills stretched radial coordinate array X (and XV)
    !-------------------------------------------------------
    !
    DT = 0.5 * PI / FLOAT(II)
    XM = XI0
    XV(1) = XI0
    do I = 1, II
        T(I) = DT * (FLOAT(I) - 0.5)
        TP = DT * FLOAT(I)
        !
        IF(IXSPAC == 2) THEN
            !------- Usual sine stretching, adjusted for nonzero root radius
            XI(I) = SQRT(XITIP * SIN(T(I))**2 + (XI0 * COS(T(I)))**2)
            XP = SQRT(XITIP * SIN(TP)**2 + (XI0 * COS(TP))**2)
        ELSE
            !------- Cosine stretching for more root resolution (also in TINVRT)
            XI(I) = 0.5 * (1.0 - COS(2.0 * T(I))) * (XITIP - XI0) + XI0
            XP = 0.5 * (1.0 - COS(2.0 * TP)) * (XITIP - XI0) + XI0
        ENDIF
        !
        XI(I) = (XP + XM) * 0.5
        DXI(I) = XP - XM
        !
        XM = XP
        XV(I + 1) = XP
    end do
    XV(II + 1) = XITIP
    !
    RETURN
END
! SETX



SUBROUTINE OPFILE(LU, FNAME)
    CHARACTER*(*) FNAME
    !
    CHARACTER*4 COMAND
    CHARACTER*128 COMARG, TMP
    CHARACTER*1 ANS, DUMMY
    !
    !---- get filename if it hasn't been already specified
    IF(FNAME(1:1) == ' ') CALL ASKS('Enter output filename^', FNAME)
    !
    !---- try to open file
    OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 50)
    !
    !---- file exists... ask how to proceed
    NF = INDEX(FNAME, ' ') - 1
    TMP = 'File  ' // FNAME(1:NF) // &
            '  exists.  Overwrite / Append / New file ?^'
    CALL ASKC(TMP, COMAND, COMARG)
    ANS = COMAND(1:1)
    !
    !---- ask again if reply is invalid
    IF(INDEX('OoAaNn', ANS) == 0) THEN
        CALL ASKC(' O / A / N  ?^', COMAND, COMARG)
        ANS = COMAND(1:1)
        !
        IF(INDEX('OoAaNn', ANS) == 0) THEN
            !------- Still bad reply. Give up asking and just return
            WRITE(*, *) 'No action taken'
            RETURN
        ENDIF
    ENDIF
    !
    !---- at this point, file is open and reply is valid
    IF    (INDEX('Oo', ANS) /= 0) THEN
        !------ go to beginning of file to overwrite
        REWIND(LU)
        GO TO 60
    ELSEIF(INDEX('Aa', ANS) /= 0) THEN
        !------ go to end of file to append
        DO K = 1, 12345678
            READ(LU, 1000, END = 60) DUMMY
            1000     FORMAT(A)
        ENDDO
    ELSE
        !------ new file... get filename from command argument, or ask if not supplied
        FNAME = COMARG
        IF(FNAME(1:1) == ' ') CALL ASKS('Enter output filename^', FNAME)
    ENDIF
    !
    !---- at this point, file FNAME is new or is to be overwritten
    50   OPEN(LU, FILE = FNAME, STATUS = 'UNKNOWN', ERR = 90)
    REWIND(LU)
    !
    60   RETURN
    !
    90   WRITE(*, *) 'Bad filename.'
    RETURN
END
! OPFILE


SUBROUTINE OUTPUT(LU)
    USE common
    IMPLICIT REAL (M)
    LOGICAL LHELI
    CHARACTER*1 SCHAR
    !---------------------------------------------
    !     Dumps operating state output to unit LU
    !---------------------------------------------
    !
    IADD = 1
    IF(LU == LUWRIT) IADD = INCR
    !
    WRITE (LU, 1000)
    IF(.NOT.CONV) WRITE(LU, 2000)
    !
    LHELI = .FALSE.
    !
    !---- dimensional thrust, power, torque, rpm
    TDIM = TTOT * RHO * VEL**2 * RAD**2
    QDIM = QTOT * RHO * VEL**2 * RAD**3
    PDIM = PTOT * RHO * VEL**3 * RAD**2
    !
    TVDIM = TVIS * RHO * VEL**2 * RAD**2
    PVDIM = PVIS * RHO * VEL**3 * RAD**2
    !
    EFFTOT = TTOT / PTOT
    RPM = VEL / (RAD * ADV * PI / 30.)
    DIA = 2.0 * RAD
    !
    !---- Nacelle (or body) thrust is difference between thrust on
    !     equivalent prop and real prop
    TNACEL = (TWAK - TINV) * RHO * VEL**2 * RAD**2
    !
    !---- blade solidity
    CALL SPLINE(CH, W1, XI, II)
    CH34 = SEVAL(0.75, CH, W1, XI, II)
    SIGMA = FLOAT(NBLDS) * CH34 / PI
    !
    !---- standard coefficients based on forward speed
    TC = TDIM / (0.5 * RHO * VEL**2 * PI * RAD**2)
    PC = PDIM / (0.5 * RHO * VEL**3 * PI * RAD**2)
    !
    !---- standard coefficients based on rotational speed
    EN = RPM / 60.0
    CT = TDIM / (RHO * EN**2 * DIA**4)
    CP = PDIM / (RHO * EN**3 * DIA**5)
    !
    !---- induced efficiency (including nacelle thrust effect)
    EFFIND = TWAK / PWAK
    !
    !---- ideal (actuator disk) efficiency
    TCLIM = MAX(-1.0, TC)
    EIDEAL = 2.0 / (1.0 + SQRT(TCLIM + 1.0))
    !
    !---- define low advance ratio (helicopter?) related data
    IF(ADV < 0.1) THEN
        CALL SPLINE(CH, W1, XI, II)
        CTH = CT / 7.7516
        CPH = CP / 24.352
        CTOS = CTH / SIGMA
        FOM = 0.7979 * ABS(CT)**1.5 / CP
        LHELI = .TRUE.
    ENDIF
    !
    !
    IF(DUCT) THEN
        IF(IWTYP == 1) WRITE(LU, 1001) NAME
        IF(IWTYP == 2) WRITE(LU, 1002) NAME
        IF(IWTYP == 3) WRITE(LU, 1001) NAME
    ELSE
        IF(IWTYP == 1) WRITE(LU, 1011) NAME
        IF(IWTYP == 2) WRITE(LU, 1012) NAME
        IF(IWTYP == 3) WRITE(LU, 1013) NAME
    ENDIF
    IF(NADD > 1) THEN
        WRITE(LU, 1021) ADW
    ELSE IF(DUCT) THEN
        WRITE(LU, 1022) URDUCT, ADW
    ELSE
        WRITE(LU, 1023) ADW
    ENDIF
    IF(ADW < 0.5 * ADV) WRITE(LU, 1024)
    WRITE(LU, 1010) NBLDS, RAD, ADV, &
            TDIM, PDIM, QDIM, &
            EFFTOT, VEL, RPM, &
            EFFIND, EIDEAL, TC, &
            TNACEL, XI0 * RAD, XW0 * RAD, &
            TVDIM, PVDIM, &
            RHO, VSO, RMU
    !
    !---- low advance ratio (helicopter?) data
    IF(LHELI) THEN
        WRITE(LU, 1116) SIGMA, CTOS, FOM
    ELSE
        WRITE(LU, 1117) SIGMA
    ENDIF
    !
    !---- coefficients based on rotational speed
    WRITE(LU, 1015) CT, CP, ADV * PI
    !---- coefficients based on forward speed
    WRITE(LU, 1016) TC, PC, ADV

    !c      write(LU,1017) PVIS * ADV**3 * 2.0/PI,
    !c     &               PWAK * ADV**3 * 2.0/PI

    !
    IF(TERSE) RETURN
    !
    !----- find maximum RE on blade
    REMAX = 0.0
    DO I = 1, II
        REMAX = MAX(RE(I), REMAX)
    END DO
    REEXP = 1.0
    IF(REMAX >= 1.0E6) THEN
        REEXP = 6.0
    ELSEIF(REMAX >= 1.0E3) THEN
        REEXP = 3.0
    ENDIF
    !
    IF(REEXP == 1.0) THEN
        WRITE(LU, 1020)
    ELSE
        WRITE(LU, 1120) IFIX(REEXP)
    ENDIF
    !
    do I = 1, II, IADD
        !
        !------ use equivalent prop to define local efficiency
        CALL UVADD(XI(I), WA, WT)
        VW = VWAK(I)
        VAW = VW * XW(I) / ADW
        !------ Freestream velocity component on equiv prop
        UTOTW = URDUCT
        CW = XI(I) / ADV - WT - VW
        SW = UTOTW + WA + VAW
        EFFI = (CW / SW) * ADV / XW(I)
        !
        !------ use real prop to define Mach number
        CALL CSCALC(I, UTOT, WA, WT, &
                VT, VT_ADW, &
                VA, VA_ADW, &
                VD, VD_ADW, &
                CI, CI_ADV, CI_VT, &
                SI, SI_VA, &
                W, W_ADV, W_VT, W_VA, &
                PHI, P_ADV, P_VT, P_VA)
        !
        MACH = W * VEL / VSO
        !
        BDEG = BETA(I) * 180. / PI
        XRE = RE(I) / (10.0**REEXP)
        !
        SCHAR = ' '
        IF(STALL(I)) SCHAR = 's'
        !
        WRITE(LU, 1030)&
                I, XI(I), CH(I), BDEG, CL(I), SCHAR, CD(I), XRE, MACH, &
                EFFI, EFFP(I), UBODY(I)
        !c     &    ,rad*ch(i)*sin(beta(i))*39.36
    end do
    !c      WRITE(LU,1000)
    !c      WRITE(LU,*   ) ' '
    !
    RETURN
    !....................................................................
    !
    1000 FORMAT(/1X, 75('='))
    1001 FORMAT(' Ducted Graded Mom. Formulation Solution:  ', A32)
    1002 FORMAT(' Ducted Potential Formulation Solution:  ', A32)
    1011 FORMAT(' Free Tip Graded Mom. Formulation Solution:  ', A32)
    1012 FORMAT(' Free Tip Potential Formulation Solution:  ', A32)
    1013 FORMAT(' Free Tip Vortex Wake Formulation Solution:  ', A32)
    1021 FORMAT(' (External slipstream present)', 19X, &
            'Wake adv. ratio:', F11.5)
    1022 FORMAT(' Vdisk/Vslip:', F11.5, 25X, &
            'Wake adv. ratio:', F11.5)
    1023 FORMAT(50X, 'Wake adv. ratio:', F11.5)
    1024 FORMAT(' Reverse far-slipstream velocity implied.', &
            ' Interpret results carefully !')
    1010 FORMAT(' no. blades :', I3, 12X, 'radius(m)  :', F9.4, 5X, &
            'adv. ratio: ', F11.5, &
            /' thrust(N)  :', G11.3, 4X, 'power(W)   :', G11.3, 3X, &
            'torque(N-m):', G11.3, &
            /' Efficiency :', F8.4, 7X, 'speed(m/s) :', F9.3, 5X, &
            'rpm        :', F11.3, &
            /' Eff induced:', F8.4, 7X, 'Eff ideal  :', F9.4, 5X, &
            'Tcoef      :', F11.4, &
            /' Tnacel(N)  :', F11.4, 4X, 'hub rad.(m):', F9.4, 5X, &
            'disp. rad. :', F10.4, &
            /' Tvisc(N)   :', F11.4, 4X, 'Pvisc(W)   :', G11.3, &
            /' rho(kg/m3) :', F10.5, 5X, 'Vsound(m/s):', F9.3, 5X, &
            'mu(kg/m-s) :', E11.4&
            /1X, 75('-'))
    1015 FORMAT(12X, '    Ct:', F11.5, '     Cp:', F11.5, '    J:', F11.5)
    1016 FORMAT(12X, '    Tc:', F11.5, '     Pc:', F11.5, '  adv:', F11.5)
    1116 FORMAT('Helicopter: ', &
            ' Sigma:', F11.5, '  CTh/s:', F11.5, '  FOM:', F11.5)
    1117 FORMAT(' Sigma:', F11.5)
    1017 FORMAT(' Cpv:', F11.5, '    Cpi:', F11.5)
    1020 FORMAT(/'  i  r/R    c/R  beta(deg)', &
            '   CL      Cd    RE    Mach   effi  effp  na.u/U')
    1120 FORMAT(/'  i  r/R   c/R  beta(deg)', &
            '  CL     Cd    REx10^', I1, ' Mach   effi  effp  na.u/U')
    1030 FORMAT(1X, I2, F6.3, F7.4, F7.2, F7.3, 1X, A1, F7.4, 1X, &
            F6.2, 1X, F6.3, 1X, F6.3, F6.3, F8.3, f10.6)
    2000 FORMAT(/19X, '********** NOT CONVERGED **********'/)
END
! OUTPUT


SUBROUTINE UVADD(XIW, WA, WT)
    USE common
    IMPLICIT REAL (M)
    !
    WA = 0.0
    WT = 0.0
    !
    IF(NADD <= 1) RETURN
    !
    RDIM = XIW * RAD
    IF(RDIM >= RADD(1) .AND. RDIM <= RADD(NADD)) THEN
        WA = SEVAL(RDIM, UADD, UADDR, RADD, NADD) / VEL
        WT = SEVAL(RDIM, VADD, VADDR, RADD, NADD) / VEL
    ENDIF
    !
    RETURN
END

















