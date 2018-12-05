!***********************************************************************
!    Module:  xoper.f
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

SUBROUTINE OPER
    INCLUDE 'XROTOR.INC'
    CHARACTER*4 COMAND, ANS
    CHARACTER*132 COMARG, ANSARG
    CHARACTER*1 CHKEY
    !
    DIMENSION IINPUT(20)
    DIMENSION RINPUT(20)
    LOGICAL ERROR
    !
    !---------------------------------------------
    !     Run rotor at arbitrary operating points
    !---------------------------------------------
    PLFAC1 = 0.7
    PLFAC2 = 0.8
    PLFACD = 0.6
    XORG = 0.15
    YORG = 0.10
    !
    GREEK = .FALSE.
    !
    900  CONTINUE
    CALL ASKC('.OPER^', COMAND, COMARG)
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
    IF(COMAND == '    ') THEN
        RETURN
    ENDIF
    IF(COMAND == '?   ') WRITE(*, 1100)
    IF(COMAND == '?   ') GO TO 900
    IF(COMAND == 'FORM') GO TO 2
    IF(COMAND == 'TERS') GO TO 4
    IF(COMAND == 'DISP') GO TO 10
    IF(COMAND == 'NAME') GO TO 15
    IF(COMAND == 'WRIT') GO TO 20
    IF(COMAND == 'DUCT') GO TO 22
    IF(COMAND == 'VRAT') GO TO 24
    IF(COMAND == 'ATMO') GO TO 35
    IF(COMAND == 'VELO') GO TO 38
    IF(COMAND == 'ANGL') GO TO 40
    IF(COMAND == 'ADVA') GO TO 42
    IF(COMAND == 'RPM ') GO TO 45
    IF(COMAND == 'THRU') GO TO 50
    IF(COMAND == 'TORQ') GO TO 60
    IF(COMAND == 'POWE') GO TO 70
    IF(COMAND == 'ASEQ') GO TO 81
    IF(COMAND == 'RSEQ') GO TO 82
    IF(COMAND == 'BSEQ') GO TO 83
    IF(COMAND == 'VSEQ') GO TO 84
    IF(COMAND == 'CLRC') GO TO 90
    IF(COMAND == 'ADDC') GO TO 92
    IF(COMAND == 'CPUT') GO TO 94
    IF(COMAND == 'CGET') GO TO 96
    IF(COMAND == 'CASE') GO TO 97
    IF(COMAND == 'LIST') GO TO 98
    !
    IF(COMAND == 'N')    GO TO 72
    IF(COMAND == 'ITER') GO TO 75
    IF(COMAND == 'INIT') GO TO 76
    IF(COMAND == 'REIN') GO TO 78
    !
    !--- Hack to check ADW equation sensitivity, get rid of this later... HHY
    IF(COMAND == 'ADW') THEN
        WRITE(*, *) 'Current ADW factor =', ADWFCTR
        CALL ASKR('Enter new ADW factor^', ADWFCTR)
        GO TO 900
    ENDIF
    !
    WRITE(*, 1050) COMAND
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Select options for slipstream and velocity calculation
    2    CONTINUE
    WRITE(*, 3)
    CALL ASKC('.FORM^', COMAND, COMARG)
    !
    IF(COMAND == 'GRAD') THEN
        VRTX = .FALSE.
        FAST = .TRUE.
    ELSEIF(COMAND == 'POT') THEN
        VRTX = .FALSE.
        FAST = .FALSE.
    ELSEIF(COMAND == 'VRTX') THEN
        VRTX = .TRUE.
    ELSEIF(COMAND == 'WAKE') THEN
        FREE = .NOT.FREE
    ELSEIF(COMAND == ' ') THEN
        GO TO 900
    ENDIF
    !
    IF(VRTX) THEN
        WRITE(*, *)'Discrete Vortex Formulation selected'
    ELSE
        IF(FAST) THEN
            WRITE(*, *) 'Graded Momentum Formulation selected'
        ELSE
            WRITE(*, *)'Potential Formulation selected'
        ENDIF
    ENDIF
    !
    IF(FREE) THEN
        WRITE(*, *)'Self-deforming wake selected'
    ELSE
        WRITE(*, *)'Rigid wake selected'
    ENDIF
    GO TO 2
    !
    3    FORMAT(&
            /' Select options for calculation of slipstream velocities'&
            /'   GRAD     use Graded Momentum       Formulation '&
            /'   POT      use Potential (Goldstein) Formulation '&
            /'   VRTX     use discrete Vortex Wake  Formulation '&
            /'   WAKE     Toggle between rigid and self-deforming wake')
    !
    !
    !---------------------------------------------------------------------
    !--- Output data on blade stations with each case (verbose)
    4 TERSE = .NOT.TERSE
    IF(TERSE)      WRITE(*, *)'Terse output selected'
    IF(.NOT.TERSE) WRITE(*, *)'Verbose output selected'
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Display current prop operating point data
    10 CALL OUTPUT(LUWRIT)
    !cc      CALL CPROJ
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Change case name
    15 NAME = COMARG
    IF(NAME(1:1) == ' ')&
            CALL ASKS('Enter case name (32 characters max)^', NAME)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Write current prop operating point data to file
    20 IF(COMARG(1:1) /= ' ') SAVFIL = COMARG
    CALL OPFILE(LUSAVE, SAVFIL)
    CALL OUTPUT(LUSAVE)
    CLOSE(LUSAVE)
    GO TO 900
    !
    !--------------------------------------------------------------
    22   DUCT = .NOT.DUCT
    IF(DUCT) THEN
        WRITE(*, *) 'Duct option selected'
        IF(NINPUT >= 1) THEN
            URDUCT = RINPUT(1)
        ELSE
            CALL ASKR('Enter Aexit/Aprop for duct^', URDUCT)
        ENDIF
    ELSE
        WRITE(*, *) 'Free-tip option selected'
        URDUCT = 1.0
    ENDIF
    GO TO 900
    !
    !--------------------------------------------------------------
    24   IF(DUCT) THEN
        IF(NINPUT >= 1) THEN
            URDUCT = RINPUT(1)
        ELSE
            CALL ASKR('Enter Aexit/Aprop for duct^', URDUCT)
        ENDIF
    ELSE
        WRITE(*, *) '*** Select duct option first'
    ENDIF
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Change altitude
    35 IF(NINPUT >= 1) THEN
        ALT = RINPUT(1)
    ELSE
        CALL ASKR('flight altitude (km)^', ALT)
    ENDIF
    CALL ATMO(ALT, VSO, RHO, RMU)
    CALL FLOSHO(LUWRIT, VSO, RHO, RMU)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Change flight velocity
    38 VELOLD = VEL
    IF(NINPUT >= 1) THEN
        VEL = RINPUT(1)
    ELSE
        CALL ASKR('flight speed (m/s)^', VEL)
    ENDIF
    !--- Change CT,CQ,CP to give same thrust,torque,power
    THR = TTOT * (RHO * VELOLD**2 * RAD**2)
    TTOT = THR / (RHO * VEL**2 * RAD**2)
    TRQ = QTOT * (RHO * VELOLD**2 * RAD**3)
    QTOT = TRQ / (RHO * VEL**2 * RAD**3)
    PWR = PTOT * (RHO * VELOLD**3 * RAD**2)
    PTOT = PWR / (RHO * VEL**3 * RAD**2)
    CONV = .FALSE.
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Change blade pitch
    40 IF(NINPUT >= 1) THEN
        DELB = RINPUT(1)
    ELSE
        CALL ASKR('angle change (deg)^', DELB)
    ENDIF
    DO I = 1, II
        BETA(I) = BETA(I) + DELB * PI / 180.
        BETA0(I) = BETA0(I) + DELB * PI / 180.
    ENDDO
    CONV = .FALSE.
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Specify advance ratio and solve
    42 IF(NINPUT >= 1) THEN
        ADV = RINPUT(1)
    ELSE
        CALL ASKR('advance ratio     ^', ADV)
    ENDIF
    CONV = .FALSE.
    CALL APER(4, 2, LOPRINI)
    !
    IF(CONV) CALL OUTPUT(LUWRIT)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Specify RPM and solve
    45 IF(NINPUT >= 1) THEN
        RPM = RINPUT(1)
    ELSE
        RPM = VEL / (RAD * ADV * PI / 30.)
        CALL ASKR('rpm               ^', RPM)
    ENDIF
    ADV = VEL / (RAD * RPM * PI / 30.)
    CONV = .FALSE.
    CALL APER(4, 2, LOPRINI)
    !
    IF(CONV) CALL OUTPUT(LUWRIT)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Specify thrust and solve
    50 IF(NINPUT >= 1) THEN
        TSPEC = RINPUT(1)
    ELSE
        TSPEC = TTOT * (RHO * VEL**2 * RAD**2)
        CALL ASKR('thrust (N)        ^', TSPEC)
    ENDIF
    RPM = VEL / (RAD * ADV * PI / 30.0)
    WRITE(*, 1530) RPM
    51 CALL ASKC('fix Pitch / fix Rpm ( P/R )?^', &
            ANS, ANSARG)
    IF(ANS /= 'R' .AND. ANS /= 'P') GO TO 51
    !
    CONV = .FALSE.
    BSAV = BETA(II)
    IF(ANS == 'P') CALL APER(1, 2, LOPRINI)
    IF(ANS == 'R') THEN
        CALL ASKR('rpm:^', RPM)
        ADV = VEL / (RAD * RPM * PI / 30.0)
        CALL APER(1, 1, LOPRINI)
    ENDIF
    !
    IF(CONV) CALL OUTPUT(LUWRIT)
    !---- Check for valid blade angle change
    IF(ANS /= 'P') THEN
        IF(CONV) THEN
            !----- convergence was achieved: show blade angle change incurred
            WRITE(*, 1550) DBETA * 180.0 / PI
        ELSE
            !----- convergence failed: restore clobbered blade angles
            DO I = 1, II
                BETA(I) = BETA(I) - DBETA
                BETA0(I) = BETA0(I) - DBETA
            ENDDO
        ENDIF
    ENDIF
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Specify torque and solve
    60 IF(NINPUT >= 1) THEN
        QSPEC = RINPUT(1)
    ELSE
        QSPEC = QTOT * (RHO * VEL**2 * RAD**3)
        CALL ASKR('torque (N-m)      ^', QSPEC)
    ENDIF
    RPM = VEL / (RAD * ADV * PI / 30.0)
    WRITE(*, 1530) RPM
    61 CALL ASKC('fix Pitch / fix Rpm ( P/R )?^', &
            ANS, ANSARG)
    IF(ANS /= 'R' .AND. ANS /= 'P') GO TO 61
    !
    CONV = .FALSE.
    IF(ANS == 'P') CALL APER(2, 2, LOPRINI)
    IF(ANS == 'R') THEN
        CALL ASKR('rpm:^', RPM)
        ADV = VEL / (RAD * RPM * PI / 30.0)
        CALL APER(2, 1, LOPRINI)
    ENDIF
    !
    IF(CONV) CALL OUTPUT(LUWRIT)
    !---- Check for valid blade angle change
    IF(ANS /= 'P') THEN
        IF(CONV) THEN
            !----- convergence was achieved: show blade angle change incurred
            WRITE(*, 1550) DBETA * 180.0 / PI
        ELSE
            !----- convergence failed: restore clobbered blade angles
            DO I = 1, II
                BETA(I) = BETA(I) - DBETA
                BETA0(I) = BETA0(I) - DBETA
            ENDDO
        ENDIF
    ENDIF
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Specify power and solve
    70 IF(NINPUT >= 1) THEN
        PSPEC = RINPUT(1)
    ELSE
        PSPEC = PTOT * (RHO * VEL**3 * RAD**2)
        CALL ASKR('Power (W)         ^', PSPEC)
    ENDIF
    RPM = VEL / (RAD * ADV * PI / 30.0)
    WRITE(*, 1530) RPM
    71 CALL ASKC('fix pitch / fix rpm ( P/R )?^', &
            ANS, ANSARG)
    IF(ANS /= 'R' .AND. ANS /= 'P') GO TO 71
    !
    CONV = .FALSE.
    IF(ANS == 'P') CALL APER(3, 2, LOPRINI)
    IF(ANS == 'R') THEN
        CALL ASKR('rpm:^', RPM)
        ADV = VEL / (RAD * RPM * PI / 30.0)
        CALL APER(3, 1, LOPRINI)
    ENDIF
    !
    IF(CONV) CALL OUTPUT(LUWRIT)
    !---- Check for valid blade angle change
    IF(ANS /= 'P') THEN
        IF(CONV) THEN
            !----- convergence was achieved: show blade angle change incurred
            WRITE(*, 1550) DBETA * 180.0 / PI
        ELSE
            !----- convergence failed: restore clobbered blade angles
            DO I = 1, II
                BETA(I) = BETA(I) - DBETA
                BETA0(I) = BETA0(I) - DBETA
            ENDDO
        ENDIF
    ENDIF
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Change number of radial points for blade stations
    72   CONTINUE
    IF(LROTOR) THEN
        IISAV = II
        DO I = 1, IISAV
            W1(I) = XI(I)
            W2(I) = CH(I)
            W4(I) = BETA(I)
            W6(I) = UBODY(I)
            W8(I) = CLDES(I)
        ENDDO
        CALL SPLINE(W2, W3, W1, II)
        CALL SPLINE(W4, W5, W1, II)
        CALL SPLINE(W6, W7, W1, II)
        CALL SPLINE(W8, W9, W1, II)
    ENDIF
    !
    73   CALL ASKI('Enter new number of radial points^', II)
    IF(II > IX) THEN
        WRITE(*, *)
        WRITE(*, *) 'Maximum number is', IX
        GO TO 73
    ENDIF
    !
    IINF = II + II / 2
    CALL SETX
    IF(LROTOR) THEN
        DO I = 1, II
            CH(I) = SEVAL(XI(I), W2, W3, W1, IISAV)
            BETA(I) = SEVAL(XI(I), W4, W5, W1, IISAV)
            UBODY(I) = SEVAL(XI(I), W6, W7, W1, IISAV)
            CLDES(I) = SEVAL(XI(I), W8, W9, W1, IISAV)
            BETA0(I) = BETA(I)
        ENDDO
    ENDIF
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Set max number or iterations for nonlinear solution
    75   IF(NINPUT >= 1) THEN
        NITERA = IINPUT(1)
    ELSE
        CALL ASKI('Max number of iterations^', NITERA)
    ENDIF
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Toggle initialization flag
    76   LOPRINI = .NOT.LOPRINI
    IF(LOPRINI) THEN
        WRITE(*, *) 'Analysis case will be initialized'
    ELSE
        WRITE(*, *) 'Analysis case will not be initialized'
    ENDIF
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Reinitialize operating point
    78   CALL REINIT
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Read or use engine rpm/power line file
    79   IF(LPWRVAR .AND. NPWRVAR > 0) THEN
        WRITE(*, *) ' '
        WRITE(*, *) 'Current RPM/Power Engine Line'
        DO L = 1, NPWRVAR
            WRITE(*, *) L, RPMVAR(L), PWRVAR(L)
        END DO
        WRITE(*, *) ' '
    ENDIF
    !
    LU = 12
    FNAME = COMARG
    IF(FNAME(1:1) == ' ') THEN
        CALL ASKS('Enter power/rpm filename^', FNAME)
    ENDIF
    IF(FNAME(1:1) /= ' ') THEN
        OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 795)
        CALL GETPVAR(LU, IX, NPWRVAR, RPMVAR, PWRVAR)
        WRITE(*, *) ' '
        WRITE(*, *) 'RPM/Power Engine Line'
        DO L = 1, NPWRVAR
            WRITE(*, *) L, RPMVAR(L), PWRVAR(L)
            PWRVAR(L) = PWRVAR(L)
        END DO
        WRITE(*, *) ' '
        CALL SPLINA(PWRVAR, XPWRVAR, RPMVAR, NPWRVAR)
        CLOSE(LU)
        LPWRVAR = .TRUE.
    ENDIF
    !
    !
    !--- Use the engine rpm/power to define operating point
    IF(LPWRVAR) THEN
        791  CALL ASKC('fix Pitch / fix Rpm / fix Velocity ( P/R/V )?^', &
                ANS, ANSARG)
        IF(ANS == ' ') GO TO 900
        IF(ANS /= 'R' .AND. ANS /= 'P' .AND. ANS /= 'V') GO TO 791
        !
        CONV = .FALSE.
        IF(ANS == 'P') CALL APER(5, 2, LOPRINI)
        IF(ANS == 'R') THEN
            CALL ASKR('rpm:^', RPM)
            ADV = VEL / (RAD * RPM * PI / 30.0)
            CALL APER(5, 1, LOPRINI)
        ENDIF
        IF(ANS == 'V') THEN
            CALL ASKR('vel:^', VEL)
            ADV = VEL / (RAD * RPM * PI / 30.0)
            CALL APER(5, 2, LOPRINI)
        ENDIF
        !
        IF(CONV) CALL OUTPUT(LUWRIT)
        !---- Was the pitch changed?
        IF(ANS == 'R') THEN
            IF(CONV) THEN
                !----- convergence was achieved: show blade angle change incurred
                WRITE(*, 1550) DBETA * 180.0 / PI
            ELSE
                !----- convergence failed: restore clobbered blade angles
                DO I = 1, II
                    BETA(I) = BETA(I) - DBETA
                    BETA0(I) = BETA0(I) - DBETA
                ENDDO
            ENDIF
        ENDIF
        GO TO 900
    ENDIF
    !
    795  NF = INDEX(FNAME, ' ') - 1
    WRITE(*, *) 'OPEN error on file  ', FNAME(1:NF)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Do sequence of advance ratios
    81   WRITE(*, *) ' '
    WRITE(*, *) 'Sequence of advance ratios...'
    CALL SETCAS(1, NINPUT, RINPUT)
    CALL SHOCAS(LUWRIT, NPARX, NCASE, CASPAR, RAD, NAME)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Do sequence of RPMs
    82   WRITE(*, *) ' '
    WRITE(*, *) 'Sequence of RPMs...'
    CALL SETCAS(2, NINPUT, RINPUT)
    CALL SHOCAS(LUWRIT, NPARX, NCASE, CASPAR, RAD, NAME)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Do sequence of pitch angles
    83   WRITE(*, *) ' '
    WRITE(*, *) 'Sequence of blade angles...'
    CALL SETCAS(3, NINPUT, RINPUT)
    CALL SHOCAS(LUWRIT, NPARX, NCASE, CASPAR, RAD, NAME)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Do sequence of velocities
    84   WRITE(*, *) ' '
    WRITE(*, *) 'Sequence of velocity with fixed pitch or RPM...'
    CALL SETCAS(4, NINPUT, RINPUT)
    CALL SHOCAS(LUWRIT, NPARX, NCASE, CASPAR, RAD, NAME)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Save current operating point to case arrays
    90   NCASE = 0
    KCASE = 0
    GO TO 900
    !
    92   IF(NCASE >= ICASX) THEN
        WRITE(*, *) 'Case arrays too small.  Increase ICASX.'
        GO TO 900
    ENDIF
    !
    NCASE = NCASE + 1
    CASPAR(1, NCASE) = ADV
    CASPAR(2, NCASE) = VEL
    CASPAR(3, NCASE) = BETA(II)
    CASPAR(4, NCASE) = ALT
    CASPAR(5, NCASE) = RHO
    CASPAR(6, NCASE) = RMU
    CASPAR(7, NCASE) = VSO
    CASPAR(8, NCASE) = 999.
    CASPAR(9, NCASE) = 999.
    CASPAR(10, NCASE) = 999.
    CASPAR(11, NCASE) = 999.
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Write case accumulation arrays to file
    94   IF(NCASE <= 0) THEN
        WRITE(*, *)
        WRITE(*, *) 'No cases saved'
        GO TO 900
    ENDIF
    !
    LU = 12
    FNAME = COMARG
    IF(FNAME(1:1) == ' ') CALL ASKS('Enter case save filename^', FNAME)
    OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 945)
    WRITE(*, *) 'File exists.  Overwrite?  Y'
    READ (*, 1000) CHKEY
    IF(INDEX('Nn', CHKEY) /= 0) THEN
        CLOSE(LU)
        GO TO 900
    ELSE
        REWIND LU
        GO TO 946
    ENDIF
    !
    945  OPEN(LU, FILE = FNAME, STATUS = 'UNKNOWN', ERR = 94)
    946  CALL SHOCAS(LU, NPARX, NCASE, CASPAR, RAD, NAME)
    CLOSE(LU)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Read case accumulation arrays from saved file
    96   CONTINUE
    LU = 12
    FNAME = COMARG
    IF(FNAME(1:1) == ' ') CALL ASKS('Enter case save filename^', FNAME)
    OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 965)
    CALL GETCAS(LU, NPARX, NCASE, CASPAR)
    CLOSE(LU)
    CALL SHOCAS(LUWRIT, NPARX, NCASE, CASPAR, RAD, NAME)
    GO TO 900
    !
    965  NF = INDEX(FNAME, ' ') - 1
    WRITE(*, *) 'OPEN error on file  ', FNAME(1:NF)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- Rerun case operating point
    97   IF(NCASE <= 0) THEN
        WRITE(*, *)
        WRITE(*, *) 'No cases saved'
        GO TO 900
    ENDIF
    !
    IF(NINPUT >= 1) THEN
        ICASE = IINPUT(1)
    ELSE
        CALL SHOCAS(LUWRIT, NPARX, NCASE, CASPAR, RAD, NAME)
        ICASE = 0
        CALL ASKI('Select case number (0 to cancel)^', ICASE)
    ENDIF
    IF(ICASE <= 0) GO TO 900
    IF(ICASE > NCASE) THEN
        NINPUT = 0
        GO TO 97
    ENDIF
    !
    ADV = CASPAR(1, ICASE)
    VEL = CASPAR(2, ICASE)
    BET = CASPAR(3, ICASE)
    ALT = CASPAR(4, ICASE)
    RHO = CASPAR(5, ICASE)
    RMU = CASPAR(6, ICASE)
    VSO = CASPAR(7, ICASE)
    POW = CASPAR(8, ICASE)
    THR = CASPAR(9, ICASE)
    TRQ = CASPAR(10, ICASE)
    EFF = CASPAR(11, ICASE)
    !
    DELB = BET - BETA(II)
    DO I = 1, II
        BETA(I) = BETA(I) + DELB
        BETA0(I) = BETA0(I) + DELB
    ENDDO
    !
    CONV = .FALSE.
    CALL APER(4, 2, .TRUE.)
    !
    IF(CONV) THEN
        CASPAR(8, ICASE) = PTOT * RHO * VEL**3 * RAD**2
        CASPAR(9, ICASE) = TTOT * RHO * VEL**2 * RAD**2
        CASPAR(10, ICASE) = QTOT * RHO * VEL**2 * RAD**3
        CASPAR(11, ICASE) = TTOT / PTOT
    ENDIF
    !
    IF(CONV) CALL OUTPUT(LUWRIT)
    GO TO 900
    !
    !---------------------------------------------------------------------
    !--- List rotor dimensional data ?
    98   CONTINUE
    DO I = 1, II
        WRITE(*, *) XI(I), NBLDS * GAM(I) * RAD * VEL, VIND(3, I) * VEL, &
                NBLDS * GAM(I) / (4.0 * PI * VIND(3, I) * XI(I))
    ENDDO
    GO TO 900

    !.......................................................................
    !
    1000 FORMAT(A)
    1050 FORMAT(1X, A4, ' command not recognized.' //&
            '  Type "?" for list, <Return> to exit menu.')
    1100 FORMAT(&
            /'   ADVA r   Prescribe advance ratio'&
            /'   RPM  r   Prescribe rpm'&
            /'   THRU r   Prescribe thrust'&
            /'   TORQ r   Prescribe torque'&
            /'   POWE r   Prescribe power'&
            //'   ASEQ rrr Calculate case sequence of advance ratios'&
            /'   RSEQ rrr Calculate case sequence of rpms'&
            /'   BSEQ rrr Calculate case sequence of blade angles'&
            /'   VSEQ rrr Calculate case sequence of speeds at fixed pitch'&
            /'   CLRC     Clear case accumulator'&
            /'   ADDC     Add current point point to case accumulator'&
            /'   CPUT f   Write current case accumulator to file'&
            /'   CGET f   Read cases from file'&
            /'   CASE i   Select case'&
            //'   ATMO r   Set fluid properties from standard atmosphere'&
            /'   VELO r   Set or change flight speed'&
            /'   ANGL r   Change blade pitch angle'&
            /'   PVAR f   Enter and use engine rpm/power line'&
            //'   FORM     Select slipstream and velocity formulation'&
            //'   NAME s   Set or change case name'&
            /'   WRIT f   Write current operating point to disk file'&
            /'   DISP     Display current operating state'&
            /'   INIT     Initialize next analysis case'&
            /'   REIN     Re-initialize prop to known operating state'&
            /'   TERS     Toggle between terse and verbose output'&
            /'   ITER i   Change max number of Newton iterations'&
            /'   N    i   Change number of radial points')
    1530 FORMAT(/' Current rpm:', F9.2)
    1550 FORMAT(' Blade angle changed', F7.3, ' degrees')
    !
END
! OPER



SUBROUTINE GETPVAR(LU, NDIM, N, XRPM, XPWR)
    DIMENSION XPWR(NDIM), XRPM(NDIM)
    CHARACTER*1 DUMMY
    !
    1000 FORMAT(A)
    READ(LU, 1000) DUMMY
    !
    DO I = 1, 12345
        READ(LU, *, END = 11, ERR = 99) XX, YY
        XRPM(I) = XX
        XPWR(I) = YY
    ENDDO
    11   CONTINUE
    N = I - 1
    RETURN
    !
    99   WRITE(*, *) 'File read error'
    N = 0
    RETURN
END


SUBROUTINE SHOCAS(LU, NDIM, N, PAR, RAD, NAME)
    DIMENSION PAR(0:NDIM, *)
    CHARACTER NAME*(*)
    !
    IF(NDIM < 11) THEN
        WRITE(*, *) 'Error in SHOCAS: NDIM too small for PAR array'
        RETURN
    ENDIF
    !
    PI = 4.0 * ATAN(1.0)
    !
    WRITE(LU, 900) NAME
    WRITE(LU, 1000)
    DO I = 1, N
        ADV = PAR(1, I)
        VEL = PAR(2, I)
        BET = PAR(3, I) * 180.0 / PI
        ALT = PAR(4, I)
        RHO = PAR(5, I)
        RMU = PAR(6, I) * 1.0E5
        VSO = PAR(7, I)
        CONVFLG = PAR(8, I)
        POW = PAR(8, I) * 0.001
        THR = PAR(9, I)
        TRQ = PAR(10, I)
        EFF = PAR(11, I)
        RPM = VEL / (RAD * ADV) * 30.0 / PI
        IF(CONVFLG == 999.0) THEN
            WRITE(LU, 1200) I, ADV, BET, VEL, RPM, RHO, RMU, VSO, ALT
        ELSE
            WRITE(LU, 1200) I, ADV, BET, VEL, RPM, RHO, RMU, VSO, ALT, &
                    POW, THR, TRQ, EFF
        ENDIF
    ENDDO
    RETURN
    !
    900 FORMAT(A)
    !
    !        1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7
    !23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
    !IIXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGGXGGGGGGGGGGGG
    !  n         V/wR         Btip            V          rpm          rho       mu*1e5       Vsound            h        P(kW)         T(N)       Q(N-m)          eff',
    !
    1000 FORMAT(&
            '  n'&
            '         V/wR         Btip            V          rpm'&
            '          rho       mu*1e5       Vsound            h'&
            '        P(kW)         T(N)       Q(N-m)          eff'&
            /160('-'))
    1200 FORMAT(I3, 12(1X, G12.5))
    !
    ! 1000 FORMAT( '  n   V/wR   Btip      V       rpm ',
    !     &        '    rho   mu*1e5   Vsound    h  ',
    !     &        '       P(kW)        T(N)         Q(N-m)      eff',
    !     &       /' --  ------  -----  ------- ------- ',
    !     &        ' -----  -------  -------  ------ ',
    !     &        '  -----------  -----------  -----------  -----')
    ! 1200 FORMAT(I3,1X,F7.3,1X,F6.2,1X,F7.2,1X,F8.1,1X,F6.3,1X,F8.3,
    !     &       1X,F8.2,1X,F7.2,2X,G12.5,1X,G12.5,1X,G12.5,1X,F6.3)
    !
    !--- new format (old one ran out of sig. digits, lets remove the 80 column limit!
    !  n   V/wR   Btip     V       rpm     rho    mu*1e5   Vsound    h        P(kW)        T(N)         Q(N-m)      eff
    ! --  ------  -----  ------  -------  -----  -------  -------  -----   -----------  -----------  -----------  -----
    ! 12 x0.2345 x14.25 x123.23 x10000.0 x1.225 x123.123 x1234.12 x123.12 x123456.1234 x123456.1234 x123456.1234 x0.111
    !iii fffffff ffffff fffffff ffffffff ffffff ffffffff ffffffff fffffff gggggggggggg gggggggggggg gggggggggggg ffffff
    !
    !
    !---------------------------------------------------------------------------
    ! 1000 FORMAT(/'  n   V/wR   Btip     V     rpm',
    !     &        '      rho     P(kW)    T(N)     Q(N-m)   eff',
    !     &       /' --  -----  -----  ------  -------',
    !     &        '  -----  --------  -------  -------  -----')
    ! 1200 FORMAT(I3,1X,F6.3,1X,F6.2,1X,F6.2,1X,F8.1,1X,F6.3,
    !     &       1X,F9.4,1X,F8.1,1X,F8.1,1X,F6.3)
    !
    !  n   V/wR   Btip     V     rpm      rho     P(kW)     T(N)     Q(N-m)   eff
    ! --  -----  -----  ------  -------  -----  --------  -------  -------  -----
    ! 12 x0.234 x14.25 x123.23 x10000.0 x1.225 x123.1234 x11000.0 x11125.0 x0.111
    !iii ffffff ffffff fffffff ffffffff ffffff fffffffff ffffffff ffffffff ffffff
    !---------------------------------------------------------------------------
    !
    !---------------------------------------------------------------------------
    ! 1000 FORMAT(
    !     & /'  n    V/wR    Btip     V      rpm  ',
    !     &  '    rho     mu*1e5  Vsound     h  '
    !     & /'  --   ------  -----  ------  -------',
    !     &  '  -------  -------  ------   -----')
    !C          12   0.2345  14.25  123.23  10000.0
    !C             1.225  115.000  1000.0   25.00
    ! 1200 FORMAT(
    !     &   I3, F9.4,    F7.2,   F8.2,     F9.1,
    !     &       F9.3,    F9.3,     F8.1,  F8.2  )
    !---------------------------------------------------------------------------
    !
    !---------------------------------------------------------------------------
    ! 1000 FORMAT(
    !     & /'  n    V/wR    Btip     V     rpm  ',
    !     &  '  rho     P(W)     T(N)     Q(N-m)',
    !     & /'  --   ------  -----  ------  ------',
    !     &  '  -----  -------  ------   ------')
    !
    ! 1200 FORMAT(
    !     &   I3, F9.4,    F7.2,   F8.2,     F9.1,
    !     &       F7.4,  F10.2,   F9.3,  F9.3  )
    !---------------------------------------------------------------------------
    !
END


SUBROUTINE GETCAS(LU, NDIM, NCAS, PAR)
    DIMENSION PAR(0:NDIM, *), A(16)
    CHARACTER DUMMY*1, LINE*128, CNAME*32
    LOGICAL ERROR
    !
    IF(NDIM < 11) THEN
        WRITE(*, *) 'Error in GETCAS: NDIM too small for PAR array'
        RETURN
    ENDIF
    !
    PI = 4.0 * ATAN(1.0)
    !
    1000 FORMAT(A)
    READ(LU, 1000) CNAME
    !c      WRITE(*,*) 'Case name: ',CNAME
    READ(LU, 1000) DUMMY
    READ(LU, 1000) DUMMY
    !
    DO I = 1, 12345
        !cc        READ(LINE,ERR=99) IDUM,ADV,BET,VEL,RPM,RHO,RMU,VSO,ALT,
        !cc     &                    POW,THR,TRQ,EFF
        READ(LU, 1000, END = 11) LINE
        N = 13
        CALL GETFLT(LINE, A, N, ERROR)
        IF(ERROR) GO TO 99
        ADV = A(2)
        BET = A(3) * PI / 180.0
        VEL = A(4)
        RHO = A(6)
        RMU = A(7) / 1.0E5
        VSO = A(8)
        ALT = A(9)
        POW = 999.0
        THR = 999.0
        TRQ = 999.0
        EFF = 999.0
        IF(N == 13) THEN
            POW = A(10) * 1000.0
            THR = A(11)
            TRQ = A(12)
            EFF = A(13)
        ENDIF
        !--- Set parameters for cases
        PAR(1, I) = ADV
        PAR(2, I) = VEL
        PAR(3, I) = BET
        PAR(4, I) = ALT
        PAR(5, I) = RHO
        PAR(6, I) = RMU
        PAR(7, I) = VSO
        PAR(8, I) = POW
        PAR(9, I) = THR
        PAR(10, I) = TRQ
        PAR(11, I) = EFF
    ENDDO
    11   CONTINUE
    NCAS = I - 1
    RETURN
    !
    99   WRITE(*, *) 'File read error'
    NCAS = 0
    RETURN
END


SUBROUTINE SETCAS(ITYPE, NINPUT, RINPUT)
    INCLUDE 'XROTOR.INC'
    DIMENSION RINPUT(*)
    !---------------------------------------------------
    !     Sets operating parameters over a range
    !     of parameters of type ITYPE where
    !       ITYPE    Parameter for range
    !         1      Advance ratio
    !         2      RPM
    !         3      Blade angle
    !         4      Velocity with fixed pitch
    !xxxx     5      Velocity with fixed RPM
    !---------------------------------------------------
    CHARACTER*1 ANS, ANS4*4
    LOGICAL YES
    !
    !      WRITE(*,*)
    !      WRITE(*,*) 'Overwrite or Append  to case accumulator?  O'
    !      READ (*,1000) ANS
    ! 1000 FORMAT(A)
    !      IF(INDEX('Aa',ANS) == 0) NCASE = 0
    !
    !
    IF(NCASE > 0) THEN
        WRITE(*, *)
        WRITE(*, *) 'Appending to current case accumulator...'
    ENDIF
    !
    KCASE = 0
    !
    !---------------------------------------------------------------------
    !--- Sequence of advance ratio
    IF(ITYPE == 1) THEN
        KCASE = 1
        !
        IF    (NINPUT >= 3) THEN
            ADV1 = RINPUT(1)
            ADV2 = RINPUT(2)
            DADV = RINPUT(3)
        ELSEIF(NINPUT >= 2) THEN
            ADV1 = RINPUT(1)
            ADV2 = RINPUT(2)
            DADV = 999.
            CALL ASKR('Enter advance ratio increment  ^', DADV)
        ELSEIF(NINPUT >= 1) THEN
            ADV1 = RINPUT(1)
            ADV2 = 999.
            CALL ASKR('Enter last  advance ratio value^', ADV2)
            DADV = 999.
            CALL ASKR('Enter advance ratio increment  ^', DADV)
        ELSE
            ADV1 = 999.
            CALL ASKR('Enter first advance ratio value^', ADV1)
            ADV2 = 999.
            CALL ASKR('Enter last  advance ratio value^', ADV2)
            DADV = 999.
            CALL ASKR('Enter advance ratio increment  ^', DADV)
        ENDIF
        IF(ADV1 == ADV2) RETURN
        DADV = SIGN(DADV, ADV2 - ADV1)
        NP = 1
        IF(DADV /= 0.0) NP = INT((ADV2 - ADV1) / DADV + 0.5) + 1
        IF(NP <= 0) RETURN
        !
        !--- Check for use of rpm/power relationship to set power
        YES = .FALSE.
        XANS = 0.
        IF(LPWRVAR) CALL ASKL('Use engine rpm/power line ?^', YES)
        IF(YES) XANS = 100.0
        !
        IF(NCASE + NP > ICASX) THEN
            WRITE(*, *) 'Limiting number of cases to array limit:', ICASX
            NP = ICASX - NCASE
        ENDIF
        !
        DO IP = 1, NP
            NCASE = NCASE + 1
            CASPAR(0, NCASE) = XANS + FLOAT(KCASE)
            CASPAR(1, NCASE) = ADV1 + DADV * FLOAT(IP - 1)
            CASPAR(2, NCASE) = VEL
            CASPAR(3, NCASE) = BETA(II)
            CASPAR(4, NCASE) = ALT
            CASPAR(5, NCASE) = RHO
            CASPAR(6, NCASE) = RMU
            CASPAR(7, NCASE) = VSO
            CASPAR(8, NCASE) = 999.
            CASPAR(9, NCASE) = 999.
            CASPAR(10, NCASE) = 999.
            CASPAR(11, NCASE) = 999.
        ENDDO
        !
        !---------------------------------------------------------------------
        !--- Sequence of RPM
    ELSEIF(ITYPE == 2) THEN
        KCASE = 2
        !
        IF    (NINPUT >= 3) THEN
            RPM1 = RINPUT(1)
            RPM2 = RINPUT(2)
            DRPM = RINPUT(3)
        ELSEIF(NINPUT >= 2) THEN
            RPM1 = RINPUT(1)
            RPM2 = RINPUT(2)
            DRPM = 999.
            CALL ASKR('Enter rpm increment  ^', DRPM)
        ELSEIF(NINPUT >= 1) THEN
            RPM1 = RINPUT(1)
            RPM2 = 999.
            CALL ASKR('Enter last  rpm value^', RPM2)
            DRPM = 999.
            CALL ASKR('Enter rpm increment  ^', DRPM)
        ELSE
            RPM1 = 999.
            CALL ASKR('Enter first rpm value^', RPM1)
            RPM2 = 999.
            CALL ASKR('Enter last  rpm value^', RPM2)
            DRPM = 999.
            CALL ASKR('Enter rpm increment  ^', DRPM)
        ENDIF
        IF(RPM1 == RPM2) RETURN
        DRPM = SIGN(DRPM, RPM2 - RPM1)
        NP = 1
        IF(DRPM /= 0.0) NP = INT((RPM2 - RPM1) / DRPM + 0.5) + 1
        IF(NP <= 0) RETURN
        !
        !--- Check for use of rpm/power relationship to set power
        YES = .FALSE.
        XANS = 0.
        IF(LPWRVAR) CALL ASKL('Use engine rpm/power line ?^', YES)
        IF(YES) XANS = 100.0
        !
        ANS = ' '
        CALL ASKS('Fix power P or thrust T or blade pitch A ?^', ANS)
        CALL LC2UC(ANS)
        IF(ANS == 'T') XANS = XANS + 1000.0
        IF(ANS == 'Q') XANS = XANS + 2000.0
        IF(ANS == 'P') XANS = XANS + 3000.0
        !
        IF(NCASE + NP > ICASX) THEN
            WRITE(*, *) 'Limiting number of cases to array limit:', ICASX
            NP = ICASX - NCASE
        ENDIF
        !
        DO IP = 1, NP
            NCASE = NCASE + 1
            RPM = RPM1 + DRPM * FLOAT(IP - 1)
            CASPAR(0, NCASE) = XANS + FLOAT(KCASE)
            CASPAR(1, NCASE) = VEL / (RPM * RAD) * 30.0 / PI
            CASPAR(2, NCASE) = VEL
            CASPAR(3, NCASE) = BETA(II)
            CASPAR(4, NCASE) = ALT
            CASPAR(5, NCASE) = RHO
            CASPAR(6, NCASE) = RMU
            CASPAR(7, NCASE) = VSO
            CASPAR(8, NCASE) = 999.
            CASPAR(9, NCASE) = 999.
            CASPAR(10, NCASE) = 999.
            CASPAR(11, NCASE) = 999.
        ENDDO
        !
        !---------------------------------------------------------------------
        !--- Sequence of blade angle
    ELSEIF(ITYPE == 3) THEN
        KCASE = 3
        !
        IF    (NINPUT >= 3) THEN
            BET1 = RINPUT(1)
            BET2 = RINPUT(2)
            DBET = RINPUT(3)
        ELSEIF(NINPUT >= 2) THEN
            BET1 = RINPUT(1)
            BET2 = RINPUT(2)
            DBET = 999.
            CALL ASKR('Enter tip angle increment   (deg) ^', DBET)
        ELSEIF(NINPUT >= 1) THEN
            BET1 = RINPUT(1)
            BET2 = 999.
            CALL ASKR('Enter last  tip angle value (deg) ^', BET2)
            DBET = 999.
            CALL ASKR('Enter tip angle increment   (deg) ^', DBET)
        ELSE
            BET1 = 999.
            CALL ASKR('Enter first tip angle value (deg) ^', BET1)
            BET2 = 999.
            CALL ASKR('Enter last  tip angle value (deg) ^', BET2)
            DBET = 999.
            CALL ASKR('Enter tip angle increment   (deg) ^', DBET)
        ENDIF
        IF(BET1 == BET2) RETURN
        DBET = SIGN(DBET, BET2 - BET1)
        NP = 1
        IF(DBET /= 0.0) NP = INT((BET2 - BET1) / DBET + 0.5) + 1
        IF(NP <= 0) RETURN
        !
        !--- Check for use of rpm/power relationship to set power
        YES = .FALSE.
        XANS = 0.
        IF(LPWRVAR) CALL ASKL('Use engine rpm/power line ?^', YES)
        IF(YES) XANS = 100.0
        !
        IF(NCASE + NP > ICASX) THEN
            WRITE(*, *) 'Limiting number of cases to array limit:', ICASX
            NP = ICASX - NCASE
        ENDIF
        !
        DO IP = 1, NP
            NCASE = NCASE + 1
            BET = BET1 + DBET * FLOAT(IP - 1)
            CASPAR(0, NCASE) = XANS + FLOAT(KCASE)
            CASPAR(1, NCASE) = ADV
            CASPAR(2, NCASE) = VEL
            CASPAR(3, NCASE) = BET * PI / 180.0
            CASPAR(4, NCASE) = ALT
            CASPAR(5, NCASE) = RHO
            CASPAR(6, NCASE) = RMU
            CASPAR(7, NCASE) = VSO
            CASPAR(8, NCASE) = 999.
            CASPAR(9, NCASE) = 999.
            CASPAR(10, NCASE) = 999.
            CASPAR(11, NCASE) = 999.
        ENDDO
        !
        !---------------------------------------------------------------------
        !--- Sequence of velocities
    ELSEIF(ITYPE == 4) THEN
        KCASE = 4
        !
        IF    (NINPUT >= 3) THEN
            VEL1 = RINPUT(1)
            VEL2 = RINPUT(2)
            DVEL = RINPUT(3)
        ELSEIF(NINPUT >= 2) THEN
            VEL1 = RINPUT(1)
            VEL2 = RINPUT(2)
            DVEL = 999.
            CALL ASKR('Enter speed increment   (m/s) ^', DVEL)
        ELSEIF(NINPUT >= 1) THEN
            VEL1 = RINPUT(1)
            VEL2 = 999.
            CALL ASKR('Enter last  speed value (m/s) ^', VEL2)
            DVEL = 999.
            CALL ASKR('Enter speed increment   (m/s) ^', DVEL)
        ELSE
            VEL1 = 999.
            CALL ASKR('Enter first speed value (m/s) ^', VEL1)
            VEL2 = 999.
            CALL ASKR('Enter last  speed value (m/s) ^', VEL2)
            DVEL = 999.
            CALL ASKR('Enter speed increment   (m/s) ^', DVEL)
        ENDIF
        IF(VEL1 == VEL2) RETURN
        DVEL = SIGN(DVEL, VEL2 - VEL1)
        NP = 1
        IF(DVEL /= 0.0) NP = INT((VEL2 - VEL1) / DVEL + 0.5) + 1
        IF(NP <= 0) RETURN
        !
        !--- Check for use of rpm/power relationship to set power
        YES = .FALSE.
        XANS = 0.
        IF(LPWRVAR) CALL ASKL('Use engine rpm/power line ?^', YES)
        IF(YES) XANS = 100.0
        !
        !--- What do we hold constant, pitch or rpm?
        20     ANS4 = 'CS'
        CALL ASKS('FP fixed-pitch or CS constant-speed^', ANS4)
        CALL LC2UC(ANS4)
        IF(ANS4 /= 'CS' .AND. ANS4 /= 'FP') GO TO 20
        IF(ANS4 == 'CS') THEN
            RPM = VEL / (RAD * ADV * PI / 30.)
            CALL ASKR('Enter constant rpm value^', RPM)
            ADV = VEL / (RAD * RPM * PI / 30.)
            KCASE = 5
            IF(XANS /= 100.0) THEN
                IF(PSPEC <= 0.0 .AND. PTOT > 0.0)&
                        PSPEC = PTOT * (RHO * VEL**3 * RAD**2)
                CALL ASKR('Enter constant power value^', PSPEC)
            ENDIF
        ENDIF
        !
        IF(NCASE + NP > ICASX) THEN
            WRITE(*, *) 'Limiting number of cases to array limit:', ICASX
            NP = ICASX - NCASE
        ENDIF
        !
        DO IP = 1, NP
            NCASE = NCASE + 1
            VVEL = VEL1 + DVEL * FLOAT(IP - 1)
            CASPAR(0, NCASE) = XANS + FLOAT(KCASE)
            CASPAR(1, NCASE) = VVEL * ADV / VEL
            CASPAR(2, NCASE) = VVEL
            CASPAR(3, NCASE) = BETA(II)
            CASPAR(4, NCASE) = ALT
            CASPAR(5, NCASE) = RHO
            CASPAR(6, NCASE) = RMU
            CASPAR(7, NCASE) = VSO
            CASPAR(8, NCASE) = 999.
            CASPAR(9, NCASE) = 999.
            CASPAR(10, NCASE) = 999.
            CASPAR(11, NCASE) = 999.
        ENDDO
        !
    ENDIF
    !
    RETURN
END
! SETCAS



SUBROUTINE APER(ISPEC, ICON, LINIT)
    INCLUDE 'XROTOR.INC'
    LOGICAL LINIT
    !-------------------------------------------
    !     Sets reasonable initial circulation.
    !     Converges arbitrary operating point.
    !
    !     ISPEC controls the quantity used as a target quantity
    !       ISPEC = 1   Drive thrust to TSPEC
    !       ISPEC = 2   Drive torque to QSPEC
    !       ISPEC = 3   Drive power  to PSPEC
    !       ISPEC = 4   Fix advance ratio to current value
    !       ISPEC = 5   Drive to power specified by RPM (engine power-RPM line)
    !     ICON controls the constrained quantity
    !       ICON = 1    Advance ratio(rpm) fixed
    !       ICON = 2   Blade pitch fixed
    !     LINIT is flag for initialization of rotor condition
    !-------------------------------------------
    !
    !--- Initialize circulations if requested
    IF(LINIT) THEN
        !cc        WRITE(*,*) 'APINIT called...'
        CALL APINIT
    ENDIF
    !cc      CALL PLOT_DATA(NAME)
    !
    !cc      WRITE(*,*) 'Before APITER ADV,ADW ',adv, adw
    CALL APITER(ISPEC, ICON)
    !
    IF(.NOT.CONV) THEN
        WRITE(*, *)
        WRITE(*, *) 'Iteration limit exceeded'
        WRITE(*, *) 'Gres Fres Ares =', GRESMX, FRESMX, ARESMX
    ENDIF
    !
    RETURN
END
! APER


SUBROUTINE APINIT
    INCLUDE 'XROTOR.INC'
    !---------------------------------------------------------
    !     Sets reasonable initial circulation.
    !     Initial circulations are set w/o induced effects
    !     An iteration is done using the self-induced velocity
    !     from graded momentum theory to converge an approximate
    !     wake advance ratio
    !----------------------------------------------------------
    !
    DATA NITERG / 10 /
    !
    BLDS = FLOAT(NBLDS)
    DBETA = 0.0
    !
    UDUCT = 0.0
    VADUCT_VA = 1.0
    IF(DUCT) THEN
        UDUCT = URDUCT - 1.0
        VADUCT_VA = 2.0 * URDUCT
    ENDIF
    ADW = ADV * (1.0 + UDUCT)
    !
    !======================================================================
    !---- Initialize section circulation neglecting induced velocity
    TSUM = 0.
    DO I = 1, II
        UTOT = URDUCT + UBODY(I)
        CALL UVADD(XI(I), WA, WT)
        !
        SI = UTOT + WA
        CI = XI(I) / ADV - WT
        !
        WSQ = CI * CI + SI * SI
        W = SQRT(WSQ)
        PHI = ATAN2(SI, CI)
        !
        ALFA = BETA(I) - PHI
        REY = CH(I) * ABS(W) * RHO * VEL * RAD / RMU
        CALL GETCLCDCM(I, ALFA, W, REY, &
                CL(I), CL_AL, CL_W, &
                CLMAX, CLMIN, DCLSTALL, STALL(I), &
                CD(I), CD_ALF, CD_W, CD_REY, &
                CM(I), CM_AL, CM_W)
        !
        GAM(I) = 0.5 * CL(I) * W * CH(I)
        TSUM = TSUM + BLDS * GAM(I) * CI * DXI(I)
        !c        write(8,997) 'i,alfa,cl,gam,tsum ',i,alfa,cl(i),gam(i),tsum
    ENDDO
    997  format(A, ' ', i4, 5(1x, f10.5))
    !
    !---- use momentum theory estimate of axial velocity to set wake adv. ratio
    VHSQ = 0.5 * TSUM / PI
    VHSQ = MAX(VHSQ, -0.25)
    ADW = ADV * 0.5 * (1.0 + SQRT(1.0 + 4.0 * VHSQ))
    !
    !cc      WRITE(*,*) 'APINIT noVind TSUM,ADW ',TSUM,ADW
    !cc      CALL PLOT_DATA(NAME)
    !
    !---- recalculate Vtan using new GAM values
    CALL VCALC
    !C    GO TO 101
    !
    !======================================================================
    !---- Refine the initial guess with a graded-momentum theory estimate
    !     Use momentum theory to estimate axial induced velocity to drive
    !     equation for wake advance ratio
    !
    do ITERG = 1, NITERG
        !
        CALL GRADMO(IX, II, NBLDS, DUCT, RAKE, &
                XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
        !
        TSUM = 0.
        T_ADW = 0.
        !
        DCLMAX = 0.
        RLXMIN = 1.0
        !
        do I = 1, II
            !
            !--- Redefine VT and VA to diagonal self-influences
            VT = VIND_GAM(3, I, I) * GAM(I)
            VT_GAM = VIND_GAM(3, I, I)
            VT_ADW = VIND_ADW(3, I)
            !
            VA = VIND_GAM(1, I, I) * GAM(I)
            VA_GAM = VIND_GAM(1, I, I)
            VA_ADW = VIND_ADW(1, I)
            !
            !------ include duct effect on freestream and induced axial velocity
            UDUCT = 0.0
            VADUCT_VA = 1.0
            IF(DUCT) THEN
                UDUCT = URDUCT - 1.0
                VADUCT_VA = 2.0 * URDUCT
            ENDIF
            !
            UTOT = 1.0 + UDUCT + UBODY(I)
            CALL UVADD(XI(I), WA, WT)
            !
            CI = XI(I) / ADV - WT - VT
            CI_ADV = -XI(I) / ADV**2
            CI_VT = -  1.0
            !
            SI = UTOT + WA + VA * VADUCT_VA
            SI_VA = VADUCT_VA
            !cc        SI     = UTOT + WA  +  VA
            !cc        SI_VA  =               1.0
            !
            WSQ = CI * CI + SI * SI
            W = SQRT(WSQ)
            W_ADV = (CI * CI_ADV) / W
            W_VT = (CI * CI_VT) / W
            W_VA = (SI * SI_VA) / W
            !
            PHI = ATAN2(SI, CI)
            P_ADV = (- SI * CI_ADV) / WSQ
            P_VT = (- SI * CI_VT) / WSQ
            P_VA = (CI * SI_VA) / WSQ
            !
            ALFA = BETA(I) - PHI
            AL_VT = - P_VT
            AL_VA = - P_VA
            !
            REY = CH(I) * ABS(W) * RHO * VEL * RAD / RMU
            CALL GETCLCDCM(I, ALFA, W, REY, &
                    CL(I), CL_AL, CL_W, &
                    CLMAX, CLMIN, DCLSTALL, STALL(I), &
                    CD(I), CD_ALF, CD_W, CD_REY, &
                    CM(I), CM_AL, CM_W)
            !cc          write(*,*) 'iterg,i,cl ',iterg,i,cl(i)
            !
            !-------- Res( CL( AL W ) , W , GAM )
            REZ = CH(I) * CL(I) * W - 2.0 * GAM(I)
            Z_CL = CH(I) * W
            Z_W = CH(I) * CL(I)
            Z_G = - 2.0
            !
            !-------- Res( AL( VT ADW ) , W( VT ADW ) , GAM )
            Z_AL = Z_CL * CL_AL
            Z_W = Z_CL * CL_W + Z_W
            !
            !-------- Res( VT(GAM ADW) , ADW , GAM )
            Z_VT = Z_W * W_VT + Z_AL * AL_VT
            Z_VA = Z_W * W_VA + Z_AL * AL_VA
            !
            !-------- Res( ADW , GAM )
            Z_ADW = Z_VT * VT_ADW + Z_VA * VA_ADW
            Z_G = Z_VT * VT_GAM + Z_VA * VA_GAM + Z_G
            !
            DELG = -REZ / Z_G
            DCL = 2.0 * DELG / (CH(I) * W)
            !
            !---- Apply limiter to GAM update based on CL change
            RLX = 1.0
            IF(RLX * ABS(DCL) > 0.2) THEN
                IF(DCL /= 0.0) THEN
                    RLX = MIN(RLX, 0.2 / ABS(DCL))
                    !cc        write(*,998) 'APER CL limiter i,rlx,dcl,cl',i,rlx,dcl,cl(i)
                ENDIF

            ENDIF
            998    format(a, 2x, i5, 3(2x, F12.5))
            !
            IF(ABS(DCL) > ABS(DCLMAX)) DCLMAX = DCL
            IF(ABS(RLX) < RLXMIN)      RLXMIN = RLX
            !
            GAM(I) = GAM(I) + RLX * DELG
            !-------- dREZ = Z_G*dG + Z_ADW*dADW = 0
            G_ADW = -Z_ADW / Z_G
            !
            !cc Forces for raked blade corrected for COS of rake angle
            !          COSR = COS(RAKE)
            COSR = 1.0
            !
            TSUM = TSUM + BLDS * GAM(I) * CI * DXI(I) * COSR
            T_G = BLDS * CI * DXI(I) * COSR
            T_VT = BLDS * GAM(I) * CI_VT * DXI(I) * COSR
            T_ADW = T_ADW + (T_G + T_VT * VT_GAM) * G_ADW&
                    + T_VT * VT_ADW
        end do
        !
        !---- Momentum theory estimate of induced axial velocity
        VHSQ = 0.5 * TSUM / PI
        VHSQ = MAX(VHSQ, -0.2499)
        VHSQ_T = 0.5 / PI
        !
        REZ = ADW - ADV * 0.5 * (1.0 + SQRT(1.0 + 4.0 * VHSQ))
        Z_ADW = 1.0 - ADV / SQRT(1.0 + 4.0 * VHSQ) * VHSQ_T * T_ADW
        !c      Z_ADW = 1.0
        IF(Z_ADW == 0.0) WRITE(*, *) 'APINIT Z_ADW ', Z_ADW
        !
        DADW = -REZ / Z_ADW
        DADW = MIN(DADW, 10.0 * ADW)
        DADW = MAX(DADW, -0.9 * ADW)
        ADW = ADW + DADW
        !
        IF(RLXMIN < 0.2) THEN
            !cc          WRITE(*,*) 'APINIT filtering GAM'
            CALL FILTER(GAM, 0.2 * II, II)
        ENDIF
        !cc        WRITE(*,*) 'APINIT Vind iter,TSUM,ADW ',ITERG,TSUM,ADW
        !cc        WRITE(*,*) 'APINIT ADW,DADW,DCLMAX ',ADW,DADW,DCLMAX
        !
        IF(ABS(DCLMAX) < 0.001) GO TO 101
        !
    end do
    !cc      WRITE(*,*) 'APINIT No convergence'
    !
    101  RETURN
END


SUBROUTINE APITER(ISPEC, ICON)
    !-------------------------------------------------------
    !     Converges arbitrary performance operating point
    !
    !     ISPEC controls the quantity used as a target quantity

    !       ISPEC = 1   Drive thrust to TSPEC
    !       ISPEC = 2   Drive torque to QSPEC
    !       ISPEC = 3   Drive power  to PSPEC
    !       ISPEC = 4   Fix advance ratio to current value
    !       ISPEC = 5   Drive to power specified by RPM (engine power-RPM line)
    !
    !     ICON controls the constrained quantity
    !       ICON = 1    Advance ratio(rpm) fixed
    !       ICON = 2    Blade pitch fixed
    !-------------------------------------------------------
    INCLUDE 'XROTOR.INC'
    DIMENSION CLMAX(IX), CLMIN(IX), DCLSTALL(IX)
    !
    !---- convergence tolerance
    DATA EPS / 1.0E-07 /
    !
    K1 = II + 1
    K2 = II + 2
    K3 = II + 3
    WRITE(*, 2000)
    !
    do ITER = 1, MAX(NITERA, 1)
        !
        !---- if wake advance ratio changed, recalculate Vtan influence coefficients
        IF(FREE .OR. ITER == 1) THEN
            IF(FAST) THEN
                CALL GRADMO(IX, II, NBLDS, DUCT, RAKE, &
                        XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
                IWTYP = 1
            ELSEIF(.NOT.VRTX) THEN
                CALL HELICO(IX, II, NBLDS, DUCT, RAKE, &
                        XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
                IWTYP = 2
            ELSEIF(VRTX) THEN
                CALL VRTXCO(IX, II, NBLDS, DUCT, RAKE, &
                        XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
                IWTYP = 3
            ENDIF
        ENDIF
        !
        !---- recalculate Vtan
        CALL VCALC
        !
        !---- recalculate wake radius array and Vwak
        CALL SETXW
        !
        !---- recalculate thrust, power, and sensitivities for current solution
        CALL TPQ(1)
        !
        !---- initialize max residuals
        GRESMX = 0.
        FRESMX = 0.
        ARESMX = 0.
        !
        DO J = 1, K3
            Q(K2, J) = 0.
        ENDDO
        !
        !---- The wake advance ratio equation is only approximate, normally the
        !     tangential induced velocity is ignored (inconsistent with a rigid
        !     wake with constant wake advance ratio).  This calculates a factor
        !     to compensate for the Vt term at one (representative) radial station
        !
        DO I = 1, II
            IF(XI(I) > 0.75) GO TO 40
        END DO
        40   I75 = I
        CALL CSCALC(I75, UTOT, WA, WT, &
                VT75, VT_ADW, &
                VA75, VA_ADW, &
                VD75, VD_ADW, &
                CI75, CI_ADV, CI_VT, &
                SI75, SI_VA, &
                W75, W_ADV, W_VT, W_VA, &
                PHI75, P_ADV, P_VT, P_VA)
        !---- Factor for OMEG*R-VT correction to wake advance ratio
        ADVFACT = 1.0 / (1.0 - ADV * VT75 / XI(I75))
        !cc      WRITE(*,*) 'ADV factor ',ADVFACT
        !---- Set to 1.0 for now... HHY
        ADVFACT = 1.0
        !
        IF(FREE) THEN
            !----- Set up equation to converge wake advance ratio based on
            !      average axial velocity consistent with basic momentum theory
            !
            !---- Use "equivalent" prop thrust and power
            DQ(K2) = ADWFCTR * ADW * TWAK / PWAK - ADV * ADVFACT
            Z_TW = ADWFCTR * ADW / PWAK
            Z_PW = -ADWFCTR * ADW * TWAK / PWAK**2
            DO J = 1, II
                Q(K2, J) = Z_TW * TW_GAM(J) + Z_PW * PW_GAM(J)
            END DO
            Q(K2, K1) = Z_TW * TW_ADV + Z_PW * PW_ADV - ADVFACT
            Q(K2, K2) = Z_TW * TW_ADW + Z_PW * PW_ADW + ADWFCTR * TWAK / PWAK
            ARESMX = MAX(ARESMX, ABS(DQ(K2) / ADV))
        ELSE
            !----- specify zero change of wake advance ratios
            DQ(K2) = 0.
            Q(K2, K2) = 1.0
        ENDIF
        !
        !---- go over stations, enforcing Gamma-CL relation at real prop
        do I = 1, II
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
            ALFA = BETA(I) - PHI
            AL_DBE = 1.0
            AL_P = -1.0
            !
            REY = CH(I) * ABS(W) * RHO * VEL * RAD / RMU
            CALL GETCLCDCM(I, ALFA, W, REY, &
                    CL(I), CL_AL, CL_W, &
                    CLMAX(I), CLMIN(I), DCLSTALL(I), STALL(I), &
                    CD(I), CD_ALF, CD_W, CD_REY, &
                    CM(I), CM_AL, CM_W)
            !
            !------ Enforce local Gamma-CL relation
            DQ(I) = CH(I) * CL(I) * W - 2.0 * GAM(I)             ! Residual
            Z_CL = CH(I) * W
            Z_W = CH(I) * CL(I)
            !
            Z_GI = - 2.0
            Z_VT = Z_CL * (CL_AL * AL_P * P_VT + CL_W * W_VT) + Z_W * W_VT
            Z_VA = Z_CL * (CL_AL * AL_P * P_VA + CL_W * W_VA) + Z_W * W_VA
            Z_ADV = Z_CL * (CL_AL * AL_P * P_ADV + CL_W * W_ADV) + Z_W * W_ADV
            Z_DBE = Z_CL * (CL_AL * AL_DBE)
            !
            DO J = 1, II
                Q(I, J) = Z_VT * VIND_GAM(3, I, J)&
                        + Z_VA * VIND_GAM(1, I, J)                ! dRes/dGamj
            ENDDO
            Q(I, I) = Q(I, I) + Z_GI                         ! dRes/dGami
            Q(I, K1) = Z_ADV                                ! dRes/dAdv
            Q(I, K2) = Z_VT * VT_ADW + Z_VA * VA_ADW    ! dRes/dAdw
            Q(I, K3) = Z_DBE                                ! dRes/dBeta
            !
            GRESMX = MAX(GRESMX, ABS(DQ(I) / (0.1 * W)))
            !
        end do
        !
        !---- equivalent prop will be used to define inviscid thrust
        IF(ISPEC == 1) THEN
            !----- drive thrust to specified value
            T_SPEC = TSPEC / (RHO * VEL**2 * RAD**2)
            DQ(K1) = TWAK + TVIS - T_SPEC
            DO J = 1, II
                Q(K1, J) = TW_GAM(J) + TV_GAM(J)
            ENDDO
            Q(K1, K1) = TW_ADV + TV_ADV
            Q(K1, K2) = TW_ADW + TV_ADW
            Q(K1, K3) = TV_DBE
            !
            FRESMX = MAX(FRESMX, ABS(DQ(K1)))
            !
        ELSE IF(ISPEC == 2) THEN
            !----- drive torque (= PTOT*ADV) to specified value
            Q_SPEC = QSPEC / (RHO * VEL**2 * RAD**3)
            DQ(K1) = (PWAK + PVIS) * ADV - Q_SPEC
            DO J = 1, II
                Q(K1, J) = (PW_GAM(J) + PV_GAM(J)) * ADV
            ENDDO
            Q(K1, K1) = (PW_ADV + PV_ADV) * ADV + PWAK + PVIS
            Q(K1, K2) = (PW_ADW + PV_ADW) * ADV
            Q(K1, K3) = (PV_DBE) * ADV
            !
            FRESMX = MAX(FRESMX, ABS(DQ(K1)))
            !
        ELSE IF(ISPEC == 3) THEN
            !----- drive power to specified value
            P_SPEC = PSPEC / (RHO * VEL**3 * RAD**2)
            DQ(K1) = PWAK + PVIS - P_SPEC
            DO J = 1, II
                Q(K1, J) = PW_GAM(J) + PV_GAM(J)
            ENDDO
            Q(K1, K1) = PW_ADV + PV_ADV
            Q(K1, K2) = PW_ADW + PV_ADW
            Q(K1, K3) = PV_DBE
            !
            FRESMX = MAX(FRESMX, ABS(DQ(K1)))
            !
        ELSE IF(ISPEC == 4) THEN
            !----- fix advance ratio
            DQ(K1) = 0.
            DO J = 1, II
                Q(K1, J) = 0.
            ENDDO
            Q(K1, K1) = 1.0
            Q(K1, K2) = 0.
            Q(K1, K3) = 0.
            !
        ELSE IF(ISPEC == 5) THEN
            !----- drive power to value given by RPM
            P_SPEC = PSPEC / (RHO * VEL**3 * RAD**2)
            P_SPEC_ADV = 0.0
            IF(LPWRVAR) THEN
                RPM = VEL / (RAD * ADV * PI / 30.)
                RPM_ADV = -RPM / ADV
                !
                !----- fix 5/15/03 use linear interpolation for engine power/rpm line
                !cc         CALL SEVLIN(RPM,PWRVAR,RPMVAR,NPWRVAR,PSPEC,PSPEC_RPM)
                PSPEC = SEVAL(RPM, PWRVAR, XPWRVAR, RPMVAR, NPWRVAR)
                PSPEC_RPM = DEVAL(RPM, PWRVAR, XPWRVAR, RPMVAR, NPWRVAR)
                !
                PSPEC_ADV = PSPEC_RPM * RPM_ADV
                !
                P_SPEC = PSPEC / (RHO * VEL**3 * RAD**2)
                P_SPEC_ADV = PSPEC_ADV / (RHO * VEL**3 * RAD**2)
            ENDIF
            !
            DQ(K1) = PWAK + PVIS - P_SPEC
            DO J = 1, II
                Q(K1, J) = PW_GAM(J) + PV_GAM(J)
            ENDDO
            Q(K1, K1) = PW_ADV + PV_ADV - P_SPEC_ADV
            Q(K1, K2) = PW_ADW + PV_ADW
            Q(K1, K3) = PV_DBE
            !
            FRESMX = MAX(FRESMX, ABS(DQ(K1)))
            !
        ENDIF
        !
        !---- Constraint conditions
        DQ(K3) = 0.
        DO J = 1, K3
            Q(K3, J) = 0.
        ENDDO
        IF(ICON == 1) Q(K3, K1) = 1.0      ! advance ratio(rpm) fixed
        IF(ICON == 2) Q(K3, K3) = 1.0      ! blade pitch fixed
        !
        !---- solve linearized Newton system
        CALL GAUSS(IQ, K3, Q(1, 1), DQ(1), 1)
        !
        !
        RLX = 1.0
        !---  Set initial iterations to underrelax
        IF(ITER <= 2) RLX = 0.2
        !---- Apply limiters to the Newton updates based on physical properties
        DO I = 1, II
            DGAM(I) = -DQ(I)
            !
            !---- limit CL changes near +- stall
            DCL = 2.0 * DGAM(I) / (CH(I) * W)
            DCLMIN = MAX(1.5 * DCLSTALL(I), ABS(CL(I) - CLMIN(I)))
            DCLMAX = MAX(1.5 * DCLSTALL(I), ABS(CLMAX(I) - CL(I)))
            !
            DCLLIM = MIN(0.5, DCLMIN, DCLMAX)
            DCLLIM = MAX(DCLLIM, 0.01)
            IF(RLX * ABS(DCL) > DCLLIM) THEN
                RLX = MIN(RLX, DCLLIM / ABS(DCL))
                !cc      write(1,998) 'DCL lim i,rlx,cl,dcl ',i,rlx,cl(i),dcl,dcllim
                !cc      write(1,998) 'clmax,clmin,dclstall ',i,clmax(i),clmin(i),
                !cc     &              dclstall(i)
            ENDIF
            998    format(a, 2x, i5, 4(2x, F12.5))
            !
            !---- limit GAM changes that change sign
            IF(DGAM(I) * DGAMOLD(I) < 0.0) THEN
                IF(ABS(DGAM(I)) > 0.2 * ABS(DGAMOLD(I))) THEN
                    RLX = MIN(RLX, 0.2)
                    !c        write(*,998) 'DGAM lim i,rlx,gam,dgam ',i,rlx,gam(i),
                    !c     &               dgam(i),dgamold(i)
                ENDIF
            ENDIF
            !
        ENDDO
        !
        DADV = -DQ(K1)
        DADW = -DQ(K2)
        DBET = -DQ(K3)
        !
        IF(NITERA == 0) RLX = 0.0
        !
        !---- limit blade angle change to 0.05 radians  (~3 degrees)
        IF(RLX * DBET > 0.05) RLX = MIN(RLX, 0.05 / DBET)
        IF(RLX * DBET < -.05) RLX = MIN(RLX, -0.05 / DBET)
        !
        !---- limit advance ratio changes
        !      IF(RLX*DADV > 0.8*ADV) RLX = MIN(RLX,0.8*ADV/DADV)
        !      IF(RLX*DADV < -.5*ADV) RLX = MIN(RLX,-.5*ADV/DADV)
        !
        !      IF(RLX*DADW > 0.8*ADW) RLX = MIN(RLX, 0.8*ADW/DADW)
        !      IF(RLX*DADW < -.5*ADW) RLX = MIN(RLX,-0.5*ADW/DADW)
        !
        IF(RLX * DADV > 0.5 * ADV) RLX = MIN(RLX, 0.5 * ADV / DADV)
        IF(RLX * DADV < -.3 * ADV) RLX = MIN(RLX, -.3 * ADV / DADV)
        IF(RLX * DADW > 0.5 * ADW) RLX = MIN(RLX, 0.5 * ADW / DADW)
        IF(RLX * DADW < -.3 * ADW) RLX = MIN(RLX, -0.3 * ADW / DADW)
        !---- update circulation, blade angle arrays
        RMS = 0.
        GMX = 0.
        IMX = 0
        DO I = 1, II
            GAM(I) = GAM(I) + RLX * DGAM(I)
            BETA(I) = BETA(I) + RLX * DBET
            BETA0(I) = BETA0(I) + RLX * DBET
            !
            RMS = RMS + DGAM(I)**2 / (1.0 + 1.0 / ADV**2)
            IF(ABS(DGAM(I)) >= ABS(GMX)) THEN
                GMX = DGAM(I)
                IMX = I
            ENDIF
            DGAMOLD(I) = DGAM(I)
        ENDDO
        !
        !---- update incremental blade angle
        DBETA = DBETA + RLX * DBET
        !
        !---- update advance ratios
        ADV = ADV + RLX * DADV
        ADW = ADW + RLX * DADW
        !
        RMS = SQRT(RMS / FLOAT(II))
        !
        !---- display iteration history
        WRITE(*, 2100) ITER, GMX, IMX, RMS, &
                ADV, ADW, BETA(II) * 180.0 / PI, RLX
        !
        2000 FORMAT(/' Iter     dGmax  @Imax    gGrms       Av        ', &
                'Aw         Be       RLX')
        2100 FORMAT(1X, I3, 3X, E10.3, 2X, I3, 2X, E10.3, 2(2X, F8.4), 2X, F8.3, 2X, F8.4)
        !
        ! Iter     dGmax    (I)    gGrms      Av        Aw         Be       RLX
        !IIIXXXEEEEEEEEEEXXIIIXXEEEEEEEEEEXXFF.FFFFXXXFF.FFFFXXFFFF.FFFXXFFF.FFFF
        !
        !
        !---- Smooth filter the GAM for low relaxation factors
        IF(RLX < 0.2) THEN
            WRITE(*, *) 'APITER filtering GAM'
            CALL FILTER(GAM, 0.2 * II, II)
        ENDIF
        !
        !---- test for convergence
        IF(RMS <= EPS) THEN
            !----- final update of various quantities corresponding to converged solution
            !
            IF(FREE) THEN
                IF(FAST) THEN
                    CALL GRADMO(IX, II, NBLDS, DUCT, RAKE, &
                            XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
                    IWTYP = 1
                ELSEIF(.NOT.VRTX) THEN
                    CALL HELICO(IX, II, NBLDS, DUCT, RAKE, &
                            XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
                    IWTYP = 2
                ELSEIF(VRTX) THEN
                    CALL VRTXCO(IX, II, NBLDS, DUCT, RAKE, &
                            XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
                    IWTYP = 3
                ENDIF
            ENDIF
            !
            CALL VCALC
            CALL TPQ(1)
            !
            CONV = .TRUE.
            RETURN
        ENDIF
        !c      IF(MOD(ITER,5) == 0) CALL APINIT
        !
    end do
    !
    RETURN
END
! APITER


SUBROUTINE CSCALC(I, UTOT, WA, WT, &
        VT, VT_ADW, &
        VA, VA_ADW, &
        VD, VD_ADW, &
        CI, CI_ADV, CI_VT, &
        SI, SI_VA, &
        W, W_ADV, W_VT, W_VA, &
        PHI, P_ADV, P_VT, P_VA)
    !
    !---- Calculate velocity components at radial station I on real prop
    !
    INCLUDE 'XROTOR.INC'
    !
    VT = VIND(3, I)
    VT_ADW = VIND_ADW(3, I)
    !
    VA = VIND(1, I)
    VA_ADW = VIND_ADW(1, I)
    !
    !---- Include duct effect on freestream and induced axial velocity
    UDUCT = 0.0
    VADUCT_VA = 1.0
    IF(DUCT) THEN
        UDUCT = URDUCT - 1.0
        VADUCT_VA = 2.0 * URDUCT
    ENDIF
    !------ duct induced axial velocity
    VD = VA * (VADUCT_VA - 1.0)
    VD_VA = (VADUCT_VA - 1.0)
    VD_ADW = VD_VA * VA_ADW
    !
    !---- Freestream, body induced and added inflow velocities
    UTOT = 1.0 + UDUCT + UBODY(I)
    CALL UVADD(XI(I), WA, WT)
    !
    CI = XI(I) / ADV - WT - VT
    CI_ADV = -XI(I) / ADV**2
    CI_VT = -  1.0
    !
    SI = UTOT + WA + VA + VD
    SI_VA = 1.0 + VD_VA
    !
    !---- Redefine VA to include duct induced velocity
    !cc      VA     =  VA + VD
    !
    WSQ = CI * CI + SI * SI
    W = SQRT(WSQ)
    W_ADV = (CI * CI_ADV) / W
    W_VT = (CI * CI_VT) / W
    W_VA = (SI * SI_VA) / W
    !
    PHI = ATAN2(SI, CI)
    P_ADV = (- SI * CI_ADV) / WSQ
    P_VT = (- SI * CI_VT) / WSQ
    P_VA = (CI * SI_VA) / WSQ
    !
    !c      write(*,*) 'i,vt,va ',i,vt,va
    RETURN
END
! CSCALC


SUBROUTINE XWINIT
    INCLUDE 'XROTOR.INC'
    !------------------------------------------------------------
    !     Initial estimate for equivalent prop radial coordinate
    !     array (XW)
    !------------------------------------------------------------
    !
    UDUCT = 0.0
    VADUCT_VA = 1.0
    IF(DUCT) THEN
        UDUCT = URDUCT - 1.0
        VADUCT_VA = 2.0 * URDUCT
    ENDIF

    XM = XW0
    DO I = 1, II
        URAT = 1.0 + UDUCT + UBODY(I)
        DXW(I) = SQRT(XM**2 + 2.0 * URAT * XI(I) * DXI(I)) - XM
        XP = XM + DXW(I)
        XW(I) = 0.5 * (XP + XM)
        !
        XW_ADV(I) = 0.
        XW_ADW(I) = 0.
        DO J = 1, II
            XW_GAM(I, J) = 0.
        END DO
        !
        VWAK(I) = VIND(3, I) * XI(I) / XW(I)
        VW_ADV(I) = 0.
        VW_ADW(I) = VIND_ADW(3, I) * XI(I) / XW(I)
        DO J = 1, II
            VW_GAM(I, J) = VIND_GAM(3, I, J) * XI(I) / XW(I)
        END DO
        !
        XM = XP
    END DO
    !
    XWTIP = XM
    !
    RETURN
END
! XWINIT


SUBROUTINE SETXW
    INCLUDE 'XROTOR.INC'
    REAL XWM_GAM(IX), Z_GAM(IX)
    !---------------------------------------------------------------------
    !     Calculates Xw (radial coordinate) and Vwak (Vtheta) for
    !     the "equivalent prop"
    !     The radial stream function S xi dxi (Vax r dr) is used to
    !     define radial coordinate for the equivalent prop. The angular
    !     momentum is preserved to define the equivalent prop Vwak (Vtheta)
    !----------------------------------------------------------------------
    !
    XWM = XW0
    !
    DO J = 1, II
        XWM_GAM(J) = 0.
    END DO
    XWM_ADV = 0.
    XWM_ADW = 0.
    !cc      write(*,*) 'setxw adv,adw ',adv,adw
    !
    do I = 1, II
        XDX = XI(I) * DXI(I)
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
        !------ first guess for XWO
        DXWO = SQRT(XWM**2 + 2.0 * UTOT * XDX) - XWM
        XWO = XWM + 0.5 * DXWO
        !
        !------ Newton loop for XWO
        DO ITX = 1, 30
            !
            VW = VT * XI(I) / XWO
            VW_XWO = -VW / XWO
            VW_VT = XI(I) / XWO
            !
            !------ swirl velocity on equivalent prop
            !cc************ not used
            CW = XWO / ADV - WT - VW
            CW_XWO = 1.0 / ADV - VW_XWO
            !
            UTOTW = URDUCT
            !------ axial velocity on equivalent prop (derived from swirl)
            VAW = VW * XWO / ADW
            !------ no duct effect on freestream or axial induced velocity for equiv prop
            VAW_VW = XWO / ADW
            VAW_XWO = VW / ADW + VAW_VW * VW_XWO
            !
            SW = UTOTW + WA + VAW
            SW_XWO = VAW_XWO
            !
            REZ = SW * XWO * 2.0 * (XWO - XWM) - SI * XDX
            REZ_XWO = SW * 2.0 * (2.0 * XWO - XWM) + SW_XWO * XWO * 2.0 * (XWO - XWM)
            DELXWO = -REZ / REZ_XWO
            !
            RLX = 1.0
            IF(ABS(DELXWO) > 0.2 * (XWO - XWM))&
                    RLX = 0.2 * (XWO - XWM) / ABS(DELXWO)
            !
            XWO = XWO + RLX * DELXWO
            IF(ABS(DELXWO) < 1.0E-6) GO TO 101
            !
        END DO
        WRITE(*, 990) 'SETXW: Xw convergence failed.  i, r/R, dXw :', &
                I, XI(I), DELXWO
        990    FORMAT(A, I5, 2(1X, F12.6))
        !
        101   CONTINUE
        !
        DXWO = 2.0 * (XWO - XWM)
        !
        !------ Vw( xwo , Vt(Adw Gj) )
        VW = VT * XI(I) / XWO
        VW_XWO = -VW / XWO
        VW_VT = XI(I) / XWO
        !
        !------ swirl velocity on equivalent prop
        !cc************ not used
        CW = XWO / ADV - WT - VW
        CW_XWO = 1.0 / ADV - VW_XWO
        CW_VT = - VW_VT
        CW_ADV = -XWO / ADV**2
        !
        UTOTW = URDUCT
        !------ axial velocity on equivalent prop (derived from swirl)
        !------ no duct effect on freestream or axial induced velocity for equiv prop
        VAW = VW * XWO / ADW
        VAW_VW = XWO / ADW
        VAW_XWO = VW / ADW
        VAW_ADW = -VAW / ADW
        !
        SW = UTOTW + WA + VAW
        SW_XWO = VAW_XWO + VAW_VW * VW_XWO
        SW_VT = VAW_VW * VW_VT
        SW_ADW = VAW_ADW
        !
        !        write(*,9999) 'setxw xi,xwvt,vw,sw ',xi(i),xwo,vt,vw,sw
        ! 9999   format(A,5F10.5)
        !
        !------ Res ( xwo , xwm , Sw(Adw Vt xw) , S(Adw Vt) )
        !CC       REZ = SW*XWO*2.0*(XWO-XWM) - SI*XDX
        Z_XWO = SW * 2.0 * (2.0 * XWO - XWM)
        Z_XWM = -SW * XWO * 2.0
        Z_SW = XWO * 2.0 * (XWO - XWM)
        Z_SI = -XDX
        !
        !------ Res ( xwo , xwm(Gj Adv Adw) , VT(Gj Adw) , VA(Gj Adw) , Adw )
        Z_XWO = Z_SW * SW_XWO + Z_XWO
        Z_VT = Z_SW * SW_VT
        Z_VA = Z_SI * SI_VA
        Z_ADW = Z_SW * SW_ADW
        !
        !------ Res ( xwo , Adv , Adw , Gj )
        Z_ADV = Z_XWM * XWM_ADV
        Z_ADW = Z_XWM * XWM_ADW + Z_VT * VT_ADW&
                + Z_VA * VA_ADW + Z_ADW
        DO J = 1, II
            Z_GAM(J) = Z_XWM * XWM_GAM(J) + Z_VT * VIND_GAM(3, I, J)&
                    + Z_VA * VIND_GAM(1, I, J)
        END DO
        !
        !------ xwo( Adv , Adw , Gj )
        XW_ADV(I) = -Z_ADV / Z_XWO
        XW_ADW(I) = -Z_ADW / Z_XWO
        DO J = 1, II
            XW_GAM(I, J) = -Z_GAM(J) / Z_XWO
        END DO
        !
        !------ Vw( xwo(Adv Adw Gj) , Vt(Adw Gj) )
        VWAK(I) = VW
        !------ Vw( Adv Adw Gj )
        VW_ADV(I) = VW_XWO * XW_ADV(I)
        VW_ADW(I) = VW_XWO * XW_ADW(I) + VW_VT * VT_ADW
        DO J = 1, II
            VW_GAM(I, J) = VW_XWO * XW_GAM(I, J) + VW_VT * VIND_GAM(3, I, J)
        END DO
        !
        !
        XW(I) = XWO
        !
        !------ dxw( xwo(Adv Adw Gj) , xwm(Adv Adw Gj) )
        DXW(I) = 2.0 * (XWO - XWM)
        DXW_ADV(I) = 2.0 * (XW_ADV(I) - XWM_ADV)
        DXW_ADW(I) = 2.0 * (XW_ADW(I) - XWM_ADW)
        DO J = 1, II
            DXW_GAM(I, J) = 2.0 * (XW_GAM(I, J) - XWM_GAM(J))
        END DO
        !
        !------ new  xwm(Adv Adw Gj)  for next loop pass
        XWM = 2.0 * XWO - XWM
        XWM_ADV = 2.0 * XW_ADV(I) - XWM_ADV
        XWM_ADW = 2.0 * XW_ADW(I) - XWM_ADW
        DO J = 1, II
            XWM_GAM(J) = 2.0 * XW_GAM(I, J) - XWM_GAM(J)
        END DO
        !
    end do
    !
    XWTIP = XWM
    !      write(*,*) 'xwtip ',xwtip
    !      do i=1,ii
    !        write(20,*) 'xi,xw,vwak ',xi(i),xw(i),vwak(i)
    !      end do
    !
    RETURN
END
! SETXW



SUBROUTINE TPQ(ITYPE)
    INCLUDE 'XROTOR.INC'
    !----------------------------------------------------------
    !     Sets Thrust, Torque, Power, and their sensitivities
    !     wrt  beta, chord(i), Vtan(i), and lambda
    !----------------------------------------------------------
    !
    TINV = 0.
    PINV = 0.
    !
    TWAK = 0.
    PWAK = 0.
    !
    VAAavg = 0.
    VATavg = 0.
    TMOM = 0.
    PMOM = 0.
    !
    TVIS = 0.
    PVIS = 0.
    !
    TI_ADV = 0.
    PI_ADV = 0.
    TI_ADW = 0.
    PI_ADW = 0.
    !
    TW_ADV = 0.
    PW_ADV = 0.
    TW_ADW = 0.
    PW_ADW = 0.
    !
    TV_ADV = 0.
    PV_ADV = 0.
    TV_ADW = 0.
    PV_ADW = 0.
    TV_DBE = 0.
    PV_DBE = 0.
    !
    DO I = 1, II
        TI_GAM(I) = 0.
        PI_GAM(I) = 0.
        TW_GAM(I) = 0.
        PW_GAM(I) = 0.
        TV_GAM(I) = 0.
        PV_GAM(I) = 0.
    ENDDO
    !
    COSR = COS(RAKE)
    !
    !---- go over radial stations, setting viscous thrust and power
    BLDS = FLOAT(NBLDS)
    do I = 1, II
        BDX = BLDS * DXI(I)
        !
        XX = XI(I) / ADV
        XX_ADV = -XX / ADV
        !
        !------ set  W(Adv,Adw,Vt)  and  Phi(Adv,Adw,Vt)  sensitivities
        CALL CSCALC(I, UTOT, WA, WT, &
                VT, VT_ADW, &
                VA, VA_ADW, &
                VD, VD_ADW, &
                CI, CI_ADV, CI_VT, &
                SI, SI_VA, &
                W, W_ADV, W_VT, W_VA, &
                PHI, P_ADV, P_VT, P_VA)
        !
        ALFA = BETA(I) - PHI
        AL_DBE = 1.0
        AL_P = -1.0
        !
        !
        IF(ITYPE == 1) THEN
            !------- analysis case:  fix local Beta (except for pitch change)
            !
            !------- set alfa(Gi,dBeta,Adv,Vt) sensitivites
            ALFA = BETA(I) - PHI
            AL_GI = 0.
            AL_DBE = 1.0
            AL_ADV = -P_ADV
            AL_VT = -P_VT
            AL_VA = -P_VA
            !
            !------- set CL(Gi,dBeta,Adv,Adw,Vt) sensitivites
            REY = CH(I) * ABS(W) * RHO * VEL * RAD / RMU
            CALL GETCLCDCM(I, ALFA, W, REY, &
                    CL(I), CL_AL, CL_W, &
                    CLMAX, CLMIN, DCLSTALL, STALL(I), &
                    CD(I), CD_ALF, CD_W, CD_REY, &
                    CM(I), CM_AL, CM_W)
            CL_GI = CL_AL * AL_GI
            CL_DBE = CL_AL * AL_DBE
            CL_ADV = CL_AL * AL_ADV + CL_W * W_ADV
            CL_VT = CL_AL * AL_VT + CL_W * W_VT
            CL_VA = CL_AL * AL_VA + CL_W * W_VA
            !
            !------- set c(Gi,Adv,Vt) sensitivites  (chord is fixed)
            CH_GI = 0.
            CH_ADV = 0.
            CH_VT = 0.
            CH_VA = 0.
            !
        ELSE IF(ITYPE == 2) THEN
            !------- design case:  fix local CL and set chord based on circulation
            !
            !------- set alfa(Gi,dBeta,Adv,Adw,Vt) sensitivites
            !c         write(*,*) 'tpq2 getalf i,cl,w ',i,cl(i),w
            CALL GETALF(I, CL(I), W, ALFA, AL_CL, AL_W, STALL(I))
            AL_GI = 0.
            AL_DBE = 0.
            AL_ADV = AL_W * W_ADV
            AL_VT = AL_W * W_VT
            AL_VA = AL_W * W_VA
            !
            !------- set CL(Gi,dBeta,Adv,Adw,Vt) sensitivites
            CL_GI = 0.
            CL_DBE = 0.
            CL_ADV = 0.
            CL_VT = 0.
            CL_VA = 0.
            !
            !------- set c(Gi,Adv,Adw,Vt) sensitivites
            CHNEW = 2.0 * GAM(I) / (W * CL(I))
            !--- Check for chord going zero or negative and use nearby station data
            !    for this iteration
            IF(CHNEW <= 0.0) THEN
                !c           write(*,*) 'TPQ negative chord @I = ',I,CHNEW
                IF(I == 1) THEN
                    CH(I) = CH(I + 1)
                ELSEIF(I == II) THEN
                    CH(I) = CH(I - 1)
                ELSE
                    CH(I) = 0.5 * (CH(I - 1) + CH(I + 1))
                ENDIF
                CH_GI = 0.0
                CH_ADV = 0.0
                CH_VT = 0.0
                CH_VA = 0.0
            ELSE
                CH(I) = 2.0 * GAM(I) / (W * CL(I))
                CH_GI = 2.0 / (W * CL(I))
                CH_ADV = (-CH(I) / W) * W_ADV
                CH_VT = (-CH(I) / W) * W_VT
                CH_VA = (-CH(I) / W) * W_VA
            ENDIF
            !
            BETA(I) = ALFA + PHI
            BETA0(I) = BETA(I)
            !
        ELSE IF(ITYPE == 3) THEN
            !------- design case:  fix local chord and set angles based on CL
            !
            !------- set CL(Gi,dBeta,Adv,Adw,Vt) sensitivites
            CL(I) = 2.0 * GAM(I) / (W * CH(I))
            CL_GI = 2.0 / (W * CH(I))
            CL_DBE = 0.
            CL_ADV = (-CL(I) / W) * W_ADV
            CL_VT = (-CL(I) / W) * W_VT
            CL_VA = (-CL(I) / W) * W_VA
            !
            !------- set alfa(Gi,dBeta,Adv,Adw,Vt) sensitivites
            !c         write(*,*) 'tpq3 getalf i,cl,w ',i,cl(i)
            CALL GETALF(I, CL(I), W, ALFA, AL_CL, AL_W, STALL(I))
            AL_GI = AL_CL * CL_GI
            AL_DBE = AL_CL * CL_DBE
            AL_ADV = AL_CL * CL_ADV + AL_W * W_ADV
            AL_VT = AL_CL * CL_VT + AL_W * W_VT
            AL_VA = AL_CL * CL_VA + AL_W * W_VA
            !
            !------- set c(Gi,Adv,Adw,Vt) sensitivites
            CH_GI = 0.
            CH_ADV = 0.
            CH_VT = 0.
            CH_VA = 0.
            !
            BETA(I) = ALFA + PHI
            BETA0(I) = BETA(I)
            !
        ENDIF
        !
        RE(I) = CH(I) * ABS(W) * RHO * VEL * RAD / RMU
        RE_W = CH(I) * RHO * VEL * RAD / RMU
        RE_CH = ABS(W) * RHO * VEL * RAD / RMU
        !
        !------ set Re(Gi,Adv,Adw,Vt) sensitivites
        RE_GI = RE_CH * CH_GI
        RE_ADV = RE_CH * CH_ADV + RE_W * W_ADV
        RE_VT = RE_CH * CH_VT + RE_W * W_VT
        RE_VA = RE_CH * CH_VA + RE_W * W_VA
        !
        !------ set CM and (not used at present) sensitivites
        !------ set CD(Gi,dBeta,Adv,Adw,Vt) sensitivites
        CALL GETCLCDCM(I, ALFA, W, RE(I), &
                CL(I), CL_AL, CL_W, &
                CLMAX, CLMIN, DCLSTALL, STALL(I), &
                CD(I), CD_AL, CD_W, CD_RE, &
                CM(I), CM_AL, CM_W)
        !cc        write(*,*) 'tpq alfa,cl,cd,cm ',i,alfa,cl(i),cd(i),cm(i)
        CD_GI = CD_AL * AL_GI + CD_RE * RE_GI
        CD_ADV = CD_AL * AL_ADV + CD_RE * RE_ADV + CD_W * W_ADV
        CD_VT = CD_AL * AL_VT + CD_RE * RE_VT + CD_W * W_VT
        CD_VA = CD_AL * AL_VA + CD_RE * RE_VA + CD_W * W_VA
        CD_DBE = CD_AL * AL_DBE
        !
        !------ set total local efficiency
        EFF = (CL(I) * CI - CD(I) * SI) / (CD(I) * CI + CL(I) * SI) / XX
        !---Correct for blade rake
        EFF = EFF * COSR
        !
        !------ set induced and profile local efficiencies
        EFFI = CI / (SI * XX)
        !---Correct for blade rake
        EFFI = EFFI * COSR
        !
        EFFP(I) = EFF / EFFI
        !
        HWC = 0.5 * W * CH(I)
        HWC_W = 0.5 * CH(I)
        HWC_CH = 0.5 * W
        !
        !
        !*******************************************************
        !------ Viscous Thrust & Power contributions on real prop
        !c      COSRV = COSR
        COSRV = 1.0
        !
        !------ dTv ( Cd , S , W , c ) sensitivites
        DTV = -HWC * CD(I) * SI * BDX * COSRV
        !
        DTV_CD = -HWC * SI * BDX * COSRV
        DTV_SI = -HWC * CD(I) * BDX * COSRV
        DTV_W = -HWC_W * CD(I) * SI * BDX * COSRV
        DTV_CH = -HWC_CH * CD(I) * SI * BDX * COSRV
        !
        !------ set Tv(Gi,dBeta,Adv,Vt) sensitivites using chain rule
        DTV_GI = DTV_CD * CD_GI + DTV_CH * CH_GI
        DTV_DBE = DTV_CD * CD_DBE
        DTV_ADV = DTV_CD * CD_ADV + DTV_CH * CH_ADV&
                + DTV_W * W_ADV
        DTV_VT = DTV_CD * CD_VT + DTV_CH * CH_VT&
                + DTV_W * W_VT
        DTV_VA = DTV_CD * CD_VA + DTV_CH * CH_VA&
                + DTV_SI * SI_VA + DTV_W * W_VA
        !
        !------ accumulate viscous Thrust and sensitivities
        TVIS = TVIS + DTV
        TV_ADV = TV_ADV + DTV_ADV
        TV_ADW = DTV_VT * VT_ADW + DTV_VA * VA_ADW
        TV_DBE = TV_DBE + DTV_DBE
        !
        TV_GAM(I) = TV_GAM(I) + DTV_GI
        DO J = 1, II
            TV_GAM(J) = TV_GAM(J) + DTV_VT * VIND_GAM(3, I, J)&
                    + DTV_VA * VIND_GAM(1, I, J)
        ENDDO
        !
        !------ dPv( Cd , C , W , c )
        DPV = HWC * CD(I) * CI * BDX * XX
        !
        DPV_CD = HWC * CI * BDX * XX
        DPV_CI = HWC * CD(I) * BDX * XX
        DPV_W = HWC_W * CD(I) * CI * BDX * XX
        DPV_CH = HWC_CH * CD(I) * CI * BDX * XX
        !
        !------ set Pv(Gi,dBeta,Adv,Vt) sensitivites using chain rule
        DPV_GI = DPV_CD * CD_GI + DPV_CH * CH_GI
        DPV_DBE = DPV_CD * CD_DBE
        DPV_ADV = DPV_CD * CD_ADV + DPV_CH * CH_ADV&
                + DPV_CI * CI_ADV + DPV_W * W_ADV&
                + HWC * CD(I) * CI * BDX * XX_ADV
        DPV_VT = DPV_CD * CD_VT + DPV_CH * CH_VT&
                + DPV_CI * CI_VT + DPV_W * W_VT
        DPV_VA = DPV_CD * CD_VA + DPV_CH * CH_VA&
                + DPV_W * W_VA
        !
        !------ accumulate viscous Power and sensitivities
        PVIS = PVIS + DPV
        PV_ADV = PV_ADV + DPV_ADV
        PV_ADW = DPV_VT * VT_ADW + DPV_VA * VA_ADW
        PV_DBE = PV_DBE + DPV_DBE
        !
        PV_GAM(I) = PV_GAM(I) + DPV_GI
        DO J = 1, II
            PV_GAM(J) = PV_GAM(J) + DPV_VT * VIND_GAM(3, I, J)&
                    + DPV_VA * VIND_GAM(1, I, J)
        ENDDO
        !
        !
        !*******************************************************
        !------ Inviscid Thrust & Power contributions on real prop
        !c      COSRI = COSR
        COSRI = 1.0
        !
        !------ dTi( Gi , C( Adv Vt ) )
        DTI = GAM(I) * CI * BDX * COSRI
        !
        DTI_CI = GAM(I) * BDX * COSRI
        DTI_GI = CI * BDX * COSRI
        !
        !------ dTi( Adv , Vt(Adw Gj) )
        DTI_VT = DTI_CI * CI_VT
        DTI_ADV = DTI_CI * CI_ADV
        DTI_ADW = DTI_VT * VT_ADW
        !
        !------ accumulate inviscid Thrust and sensitivities
        TINV = TINV + DTI
        TI_ADV = TI_ADV + DTI_ADV
        TI_ADW = TI_ADW + DTI_ADW
        !------ Resolve dTi dependencies ( Vt ) to Gamma
        TI_GAM(I) = TI_GAM(I) + DTI_GI
        DO J = 1, II
            TI_GAM(J) = TI_GAM(J) + DTI_VT * VIND_GAM(3, I, J)
        ENDDO
        !
        !------ dPi( S(Va) , Gi, Adv )
        DPI = GAM(I) * SI * BDX * XX
        !
        DPI_SI = GAM(I) * BDX * XX
        DPI_GI = SI * BDX * XX
        DPI_XX = GAM(I) * SI * BDX
        !
        !------ dPi( Va(Gj Adw) , Adv , Adw , Gi )
        DPI_VA = DPI_SI * SI_VA
        DPI_ADV = DPI_XX * XX_ADV
        DPI_ADW = DPI_VA * VA_ADW
        !
        !------ accumulate inviscid Power and sensitivities
        PINV = PINV + DPI
        PI_ADV = PI_ADV + DPI_ADV
        PI_ADW = PI_ADW + DPI_ADW
        !------ Resolve dPi dependencies ( Va ) to Gamma
        PI_GAM(I) = PI_GAM(I) + DPI_GI
        DO J = 1, II
            PI_GAM(J) = PI_GAM(J) + DPI_VA * VIND_GAM(1, I, J)
        ENDDO
        !
        !*******************************************************


        !*******************************************************
        !------ Inviscid Thrust & Power contributions on equivalent prop
        !       Assumes Omega and Gamma are same in real and equivalent prop
        !
        VW = VWAK(I)
        UTOTW = URDUCT
        CALL UVADD(XI(I), WA, WT)
        !
        !------ Cw defined by same omega as real prop
        CW = XW(I) / ADV - WT - VW
        CW_ADV = -XW(I) / ADV**2
        CW_VW = -  1.0
        CW_XW = 1.0 / ADV
        !------ Sw( Adw , xw , Vw ) ;  xw, Vw( Gj , Adv , Adw )
        SW = UTOTW + WA + VW * XW(I) / ADW
        SW_ADW = -  VW * XW(I) / ADW**2
        SW_VW = XW(I) / ADW
        SW_XW = VW / ADW
        !
        !------ dTw( Gi , CW( Adv Vw ) , dxw( Gj, Adv, Adw) )
        DTW = GAM(I) * CW * BLDS * DXW(I)
        !
        DTW_GI = CW * BLDS * DXW(I)
        DTW_CW = GAM(I) * BLDS * DXW(I)
        DTW_DXW = GAM(I) * CW * BLDS
        !------ dTw( Vt(Adw Gj) , Adv , Adw , Gi , dxw(Gj Adv Adw) )
        DTW_VW = DTW_CW * CW_VW
        DTW_ADV = DTW_CW * CW_ADV + DTW_VW * VW_ADV(I)&
                + DTW_DXW * DXW_ADV(I)
        DTW_ADW = DTW_VW * VW_ADW(I) + DTW_DXW * DXW_ADW(I)
        !
        !------ accumulate Thrust and sensitivities
        TWAK = TWAK + DTW
        TW_ADV = TW_ADV + DTW_ADV
        TW_ADW = TW_ADW + DTW_ADW
        !
        !------ Resolve dTw dependencies ( Vt, Va, dxw ) to Gamma
        TW_GAM(I) = TW_GAM(I) + DTW_GI
        DO J = 1, II
            TW_GAM(J) = TW_GAM(J) + DTW_VW * VW_GAM(I, J)&
                    + DTW_DXW * DXW_GAM(I, J)
        ENDDO
        !
        !
        !------ dPw( S(Va) , Gi , Adv )
        DPW = GAM(I) * SI * BDX * XI(I) / ADV
        !
        DPW_SI = GAM(I) * BDX * XI(I) / ADV
        DPW_GI = SI * BDX * XI(I) / ADV
        DPW_ADV = -DPW / ADV
        !
        !------ dPw( Adv , Adw , Va(Gj Adw) , Gi )
        DPW_VA = DPW_SI * SI_VA
        DPW_ADW = DPW_VA * VA_ADW
        !
        !------ accumulate Power and sensitivities
        PWAK = PWAK + DPW
        PW_ADV = PW_ADV + DPW_ADV
        PW_ADW = PW_ADW + DPW_ADW
        !
        !------ Resolve dPw dependencies ( Va ) to Gamma
        PW_GAM(I) = PW_GAM(I) + DPW_GI
        DO J = 1, II
            PW_GAM(J) = PW_GAM(J) + DPW_VA * VIND_GAM(1, I, J)
        ENDDO
        !
        !        write(*,1011) 'DTW DPW DTI DPI ',DTW,DPW,DTI,DPI,cw/sw,ci/si
        ! 1011   format(a,6F11.5)
        !
        !------ Save blade thrust and power contributions (per blade, per span)
        DTII(I) = DTI / BDX
        DPII(I) = DPI / BDX
        DTWI(I) = DTW / BDX
        DPWI(I) = DPW / BDX
        DTVI(I) = DTV / BDX
        DPVI(I) = DPV / BDX
        !
        !cc        write(21,*) XI(I),TWAK
        !
        !*******************************************************
        !------ Inviscid Thrust & Power from momentum
        !
        !------ dTmom( S(Va) , Va )
        VTGM = GAM(I) * BLDS / (4.0 * PI * XI(I))
        VAGM = VTGM * XI(I) / ADW
        DTM = 2.0 * PI * XI(I) * DXI(I) * SI * (2.0 * VA)
        TMOM = TMOM + DTM
        !cc        write(20,*) XI(I),TMOM,VA,VD
        !
        !------ dPmom( S(Va) )
        DPM = DTM * (SI - VD)
        PMOM = PMOM + DPM
        !
        VATavg = VATavg + DTW * (VA + VD)
        VAAavg = VAAavg + 2.0 * PI * XI(I) * DXI(I) * (VA + VD)
        !
    end do
    !cc        write(20,*) '&'
    !cc        write(21,*) '&'
    !
    TTOT = TWAK + TVIS
    PTOT = PWAK + PVIS
    QTOT = PTOT * ADV
    !cc      write(*,*) 'TW,TI,TM ',TWAK,TINV,TMOM
    !      write(*,*) 'TBLDdim ',TINV*RHO*VEL**2*RAD**2
    !      write(*,*) 'TWAKdim ',TWAK*RHO*VEL**2*RAD**2
    !      write(*,*) 'TTOTdim ',TTOT*RHO*VEL**2*RAD**2
    !      write(*,*) 'TMOMdim ',TMOM*RHO*VEL**2*RAD**2
    !
    !      write(*,*) 'PBLDdim ',PINV*RHO*VEL**3*RAD**2
    !      write(*,*) 'PWAKdim ',PWAK*RHO*VEL**3*RAD**2
    !      write(*,*) 'PMOMdim ',PMOM*RHO*VEL**3*RAD**2
    !
    !---- disk area
    ADISK = PI * (1.0 - XI0**2)
    VAAavg = VAAavg / ADISK
    VATavg = VATavg / TWAK
    !      write(*,*) '     VA Aavg ',VAAavg*VEL
    !      write(*,*) '     VA Tavg ',VATavg*VEL
    !
    TDIM = TWAK * RHO * VEL**2 * RAD**2
    PDIM = PWAK * RHO * VEL**3 * RAD**2
    !      write(*,*) 'Vinduced from PWAK/TWAK ',PDIM/TDIM
    !
    RETURN
END
! TPQ



SUBROUTINE VCALC
    INCLUDE 'XROTOR.INC'
    !---------------------------------------------
    !     Calculates cartesian induced velocities
    !---------------------------------------------
    DO I = 1, II
        VXSUM = 0.
        VYSUM = 0.
        VZSUM = 0.
        DO J = 1, II
            VXSUM = VXSUM + VIND_GAM(1, I, J) * GAM(J)
            VYSUM = VYSUM + VIND_GAM(2, I, J) * GAM(J)
            VZSUM = VZSUM + VIND_GAM(3, I, J) * GAM(J)
        ENDDO
        VIND(1, I) = VXSUM
        VIND(2, I) = VYSUM
        VIND(3, I) = VZSUM
    ENDDO
    !
    RETURN
END
! VCALC



SUBROUTINE GRADMO(IMAX, II, NBLDS, LDUCT, RAKE, &
        XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
    DIMENSION XI(IMAX), XV(IMAX), GAM(IMAX)
    DIMENSION VIND_ADW(3, IMAX), VIND_GAM(3, IMAX, IMAX)
    LOGICAL LDUCT
    !-----------------------------------------
    !     Calculates "Graded Momentum"
    !     Gamma-swirl influence coefficients
    !
    !     Input:
    !       IMAX         array dimension
    !       II           number of radial points on blade (circulation stations)
    !       NN           number of Fourier harmonics
    !       NBLDS        number of blades
    !       LDUCT        T for duct outer BC
    !       XI(i)        radial coordinate array
    !       GAM(i)       circulation array
    !       ADW          wake advance ratio  V/wR
    !
    !     Output:
    !
    !     Output:
    !       VIND_GAM(i,j)  sensitivity of velocity at i to circulation at j
    !       VIND_ADW(i)    sensitivity of velocity at i to wake advance ratio
    !
    !        Where VIND_XXX(1,i,j) is the axial component
    !              VIND_XXX(3,i,j) is the swirl component
    !-----------------------------------------
    BLDS = FLOAT(NBLDS)
    !
    PI = 4.0 * ATAN(1.0)
    !
    XI0 = XV(1)
    XITIP = XV(II + 1)
    !
    IF(LDUCT) THEN
        !
        !----- Circulation defines mean swirl at blade
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
        !----- Circulation defines mean swirl at blade
        !----- Free-tip treatment incorporates Prandtl's averaging factor F
        SFAC = SQRT(1.0 + 1.0 / ADW**2)
        SF_ADW = 0.5 / SFAC * (-2.0 / ADW**3)
        !
        do I = 1, II
            !
            DO J = 1, II
                VIND_GAM(1, I, J) = 0.
                VIND_GAM(2, I, J) = 0.
                VIND_GAM(3, I, J) = 0.
            ENDDO
            VIND_ADW(1, I) = 0.0
            VIND_ADW(2, I) = 0.0
            VIND_ADW(3, I) = 0.0
            !
            ARG = MIN(20.0, 0.5 * BLDS * (1.0 - XI(I) / XITIP) * SFAC)
            EK = EXP(-ARG)
            EK_ADW = -EK * 0.5 * BLDS * (1.0 - XI(I) / XITIP) * SF_ADW
            FK = SQRT(1.0 - EK * EK)
            FK_ADW = 0.5 / FK * (-2.0 * EK * EK_ADW)
            F = ATAN2(FK, EK) * 2.0 / PI
            F_ADW = (EK * FK_ADW - FK * EK_ADW) / (EK * EK + FK * FK) * 2.0 / PI
            !
            VIND_GAM(3, I, I) = BLDS / (4.0 * PI * F * XI(I))
            VIND_ADW(3, I) = BLDS / (4.0 * PI * F * XI(I)) * GAM(I) * (-F_ADW / F)
            VIND_GAM(1, I, I) = VIND_GAM(3, I, I) * XI(I) / ADW
            VIND_ADW(1, I) = VIND_ADW(3, I) * XI(I) / ADW&
                    - VIND_GAM(1, I, I) * GAM(I) / ADW
            !
            !--- Reverse VZ signs
            !         VIND_GAM(3,I,I) = -VIND_GAM(3,I,I)
            !         VIND_ADW(3,I)   = -VIND_ADW(3,I)
            !cc          VA_ADW = VIND_ADW(1,I) - VA/ADW
            !
        end do
    ENDIF
    !
    RETURN
END
! GRADMO



SUBROUTINE HELICO(IMAX, II, NBLDS, LDUCT, RAKE, &
        XI, XV, GAM, ADW, VIND_GAM, VIND_ADW)
    DIMENSION XI(IMAX), XV(IMAX), GAM(IMAX)
    DIMENSION VIND_ADW(3, IMAX), VIND_GAM(3, IMAX, IMAX)
    !
    LOGICAL LDUCT
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
    !       IMAX         array dimension
    !       II           number of radial points on blade (circulation stations)
    !       NBLDS        number of blades
    !       LDUCT        T for duct outer BC
    !       XI(i)        r/R radial coordinate array
    !       XV(i)        r/R vortex  leg   coordinate array
    !       GAM(i)       circulation array
    !       ADW          wake advance ratio  V/wR
    !
    !     Output:
    !       VIND_GAM(i,j)  sensitivity of velocity at i to circulation at j
    !       VIND_ADW(i)    sensitivity of velocity at i to wake advance ratio
    !
    !        Where VIND_XXX(1,i,j) is the axial component
    !              VIND_XXX(3,i,j) is the swirl component
    !--------------------------------------------------------------------------
    PARAMETER (IDIM = 150)
    DIMENSION X(0:IDIM), AINV(0:IDIM), CSAV(0:IDIM), &
            AN_GAM(0:IDIM, 0:IDIM), AN_ADW(0:IDIM)
    DIMENSION SYS(4, IDIM)
    !
    IF(IDIM < IMAX) STOP 'HELICO: Array overflow:  Increase IDIM.'
    !
    PI = 4.0 * ATAN(1.0)
    !
    !---- number of Fourier harmonics
    NN = 128
    !
    !
    !---- set radial coordinate array for finite-difference solution
    DO I = 1, II
        X(I) = XI(I)
    ENDDO
    !
    XI0 = XV(1)
    XITIP = XV(II + 1)
    !
    !---- radial coordinate array is also needed outside of blade
    X(0) = 2.0 * XI0 - XI(1)
    X(II + 1) = 2.0 * XITIP - XI(II)
    !
    IF(LDUCT) THEN
        IIMAX = II + 1
        !
    ELSE
        !------ position of outermost point (at "infinity")
        XINF = 4.0 * XITIP
        !
        !------ set first few points beyond tip at mirror locations of those inside tip
        X(II + 2) = 2.0 * XITIP - XI(II - 1)
        X(II + 3) = 2.0 * XITIP - XI(II - 2)
        X(II + 4) = 2.0 * XITIP - XI(II - 3)
        !
        !------ set remaining points with exponential stretching to outermost point
        XFAC = (X(II + 4) - X(II + 3)) / (X(II + 3) - X(II + 2))
        DX = (X(II + 4) - X(II + 3)) * XFAC
        DO I = II + 5, IDIM - 1
            X(I) = X(I - 1) + DX
            IF(X(I) >= XINF) GO TO 5
            DX = DX * XFAC
        ENDDO
        WRITE(*, *) 'HELICO: Local array too small. Increase IDIM.'
        5      CONTINUE
        IIMAX = I
    ENDIF
    !
    !
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
    !==== Set up tridiagonal system
    ADWINV = 1.0 / ADW**2
    QBSQ = 0.25 * FLOAT(NBLDS)**2
    !
    DO I = 1, IIMAX - 1
        SYS(1, I) = (X(I) + X(I - 1)) / (X(I) - X(I - 1))
        SYS(3, I) = (X(I + 1) + X(I)) / (X(I + 1) - X(I))
        SYS(2, I) = QBSQ * (1.0 / X(I) + X(I) * ADWINV) * (X(I + 1) - X(I - 1))
        SYS(4, I) = QBSQ * (-2.0 * X(I) * ADWINV / ADW) * (X(I + 1) - X(I - 1))
    ENDDO
    !
    I = IIMAX
    IF(LDUCT) THEN
        SYS(1, I) = -1.0
    ELSE
        SYS(1, I) = 1.0
    ENDIF
    !
    !==== Loop over all NN harmonics for n = 2,4,6,...
    do N = 2, NN, 2
        RN = FLOAT(N)
        !
        !------ set up and factor tridiagonal system for this n
        !
        !------ inner BC:  dAn/dx = 0
        AINV(0) = 1.0
        CSAV(0) = -1.0
        !                                                2          2
        !------ interior equations:  d[ x dAn/dx ]/dx - n K An  =  n K Gam
        DO I = 1, IIMAX - 1
            B = SYS(1, I)
            A = -(SYS(1, I) + SYS(2, I) * RN**2 + SYS(3, I))
            C = SYS(3, I)
            !
            !-------- set 1 / (modified diagonal element)
            AINV(I) = 1.0 / (A - B * CSAV(I - 1))
            !
            !-------- set normalized upper diagonal element for back substitution
            CSAV(I) = C * AINV(I)
        ENDDO
        !
        !------ outer BC:  dAn/dx = 0  (duct) ,  or   An = 0  (free tip)
        I = IIMAX
        B = SYS(1, I)
        A = 1.0
        !
        AINV(I) = 1.0 / (A - B * CSAV(I - 1))
        !
        !
        !====== solve  An, dAn(i)/dGam(j) problems
        !
        !
        !------ set righthand sides
        DO I = 0, IIMAX
            DO J = 0, II
                AN_GAM(I, J) = 0.
            ENDDO
        ENDDO
        !
        DO I = 1, II
            AN_GAM(I, 0) = SYS(2, I) * RN**2 * GAM(I)
            AN_GAM(I, I) = SYS(2, I) * RN**2
        ENDDO
        !
        !
        !------ back-substitute RHSs
        DO I = 1, IIMAX
            IM = I - 1
            B = SYS(1, I)
            !
            !-------- eliminate and normalize only up to nonzero elements
            JLAST = MIN(I, II)
            DO J = 0, JLAST
                AN_GAM(I, J) = (AN_GAM(I, J) - B * AN_GAM(IM, J)) * AINV(I)
            ENDDO
        ENDDO
        !
        DO I = IIMAX - 1, 0, -1
            IP = I + 1
            DO J = 0, II
                AN_GAM(I, J) = AN_GAM(I, J) - CSAV(I) * AN_GAM(IP, J)
            ENDDO
        ENDDO
        !
        !
        !====== solve dAn(i)/dAdw problem
        !
        !------ set RHS
        AN_ADW(0) = 0.
        DO I = 1, IIMAX - 1
            AN_ADW(I) = SYS(4, I) * RN**2 * (GAM(I) + AN_GAM(I, 0))
        ENDDO
        AN_ADW(IIMAX) = 0.
        !
        !------ back-substitute RHS
        DO I = 1, IIMAX
            IM = I - 1
            B = SYS(1, I)
            AN_ADW(I) = (AN_ADW(I) - B * AN_ADW(IM)) * AINV(I)
        ENDDO
        !
        DO I = IIMAX - 1, 0, -1
            IP = I + 1
            AN_ADW(I) = AN_ADW(I) - CSAV(I) * AN_ADW(IP)
        ENDDO
        !
        !
        !------ sum potential harmonics to get Swirl-Gamma influence coefficients
        DO I = 1, II
            DO J = 1, II
                VIND_GAM(3, I, J) = VIND_GAM(3, I, J) + AN_GAM(I, J)
            ENDDO
            !
            VIND_GAM(3, I, I) = VIND_GAM(3, I, I) + 1.0
            VIND_ADW(3, I) = VIND_ADW(3, I) + AN_ADW(I)
        ENDDO
        !
    end do
    !
    !
    !---- extrapolate the series to the next NN terms
    !-     assuming the known aymptotic behavior (An + Gam) ~ 1/n^2
    !
    IF(.NOT.LDUCT) THEN
        !
        FSUM = 0.
        DO N = NN + 2, 4 * NN, 2
            FSUM = FSUM + (FLOAT(NN) / FLOAT(N))**2
        ENDDO
        !
        DO I = 1, II
            DO J = 1, II
                VIND_GAM(3, I, J) = VIND_GAM(3, I, J) + AN_GAM(I, J) * FSUM
            ENDDO
            !
            VIND_GAM(3, I, I) = VIND_GAM(3, I, I) + 1.0 * FSUM
            VIND_ADW(3, I) = VIND_ADW(3, I) + AN_ADW(I) * FSUM
        ENDDO
        !
    ENDIF
    !
    !---- Add on sawtooth self-influence term and scale properly
    DO I = 1, II
        BFAC = FLOAT(NBLDS) / (2.0 * PI * X(I))
        !
        VIND_GAM(3, I, I) = VIND_GAM(3, I, I) + 0.5
        DO J = 1, II
            VIND_GAM(3, I, J) = VIND_GAM(3, I, J) * BFAC
        ENDDO
        VIND_ADW(3, I) = VIND_ADW(3, I) * BFAC
        !
    ENDDO
    !
    !---- Define other velocity components VX,VY from VZ
    DO I = 1, II
        VSUM = 0.0
        DO J = 1, II
            VIND_GAM(1, I, J) = VIND_GAM(3, I, J) * XI(I) / ADW
            VIND_GAM(2, I, J) = 0.0
            VSUM = VSUM + GAM(J) * VIND_GAM(3, I, J)
        ENDDO
        !c        VIND_ADW(1,I) = VIND_ADW(3,I)*XI(I)/ADW
        VIND_ADW(1, I) = VIND_ADW(3, I) * XI(I) / ADW - VSUM * XI(I) / ADW**2
        VIND_ADW(2, I) = 0.0
        !
        !--- Reverse VZ signs
        !        DO J = 1, II
        !         VIND_GAM(3,I,J) = -VIND_GAM(3,I,J)
        !        ENDDO
        !        VIND_ADW(3,I) = -VIND_ADW(3,I)
        !
    ENDDO
    !
    RETURN
END
! HELICO





SUBROUTINE FILTER(Q, SMLEN, N)
    !-----------------------------------------
    !     Smooths array Q.
    !     SMLEN is the number of points over
    !     which information is smeared.
    !-----------------------------------------
    IMPLICIT REAL (A-H, M, O-Z)
    DIMENSION Q(N)
    !
    PARAMETER (NMAX = 500)
    DIMENSION A(NMAX), B(NMAX), C(NMAX)
    !
    IF(N > NMAX) THEN
        WRITE(*, *) 'FILTER:  Array overflow.  No action taken'
        RETURN
    ENDIF
    !
    !---- set up and solve tridiagonal system for smoothed Q
    !
    CON = SMLEN**2
    A(1) = 1.0
    C(1) = 0.
    do I = 2, N - 1
        B(I) = -CON
        A(I) = 2.0 * CON + 1.0
        C(I) = -CON
    end do
    A(N) = 1.0
    B(N) = 0.
    !
    CALL TRISOL(A, B, C, Q, N)
    !
    RETURN
END
! FILTER
