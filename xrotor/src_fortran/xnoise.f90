!***********************************************************************
!    Module:  xnoise.f
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

SUBROUTINE NOISE
    !---------------------------------------
    !     Calculates the sound pressure
    !     time history of the propeller
    !     at specified observer positions.
    !---------------------------------------
    USE common
    use mod_spline
    IMPLICIT REAL (M)
    !
    PARAMETER (NTX = 160)
    PARAMETER (NHARX = NTX / 2)
    DIMENSION AOCI(IX), AOC(IX), AOCX(IX), XA(IX)
    DIMENSION PCOMP(0:NTX, 3), PRES(0:NTX), TIME(0:NTX)
    DIMENSION DECIB(0:NHARX), FAMPL(NHARX), PHASE(NHARX)
    !
    PARAMETER (NXDIM = 81, NYDIM = 81)
    DIMENSION ADB(NXDIM, NYDIM)
    DIMENSION XDB(NXDIM, NYDIM)
    DIMENSION YDB(NXDIM, NYDIM)
    DIMENSION XLIM(2), YLIM(2)
    DIMENSION NDBSIZ(2)
    !
    CHARACTER*80 PROMPT
    CHARACTER*4 COMAND, ANS
    CHARACTER*132 COMARG, ANSARG
    !
    DIMENSION IINPUT(20)
    DIMENSION RINPUT(20)
    LOGICAL ERROR, LDBCON, LPTRAC
    !
    DIMENSION XYZOBS(3)
    CHARACTER*2 ULNAM
    !
    SAVE NT
    SAVE AOC0, XYZOBS
    SAVE ULNAM, UNITL
    !
    !---- number of rotation steps for one full rotation
    DATA NT / 80 /
    !
    !---- default airfoil area/c^2 , observer location
    DATA AOC0, XYZOBS / -1.0, 0.0, 0.0, -100.0 /
    !
    !---- start by using foot input
    !cc   DATA ULNAM, UNITL / 'm ', 1.0 /
    DATA ULNAM, UNITL / 'ft', 3.28084 /
    !
    !
    GREEK = .FALSE.
    !
    LDBCON = .FALSE.
    LPTRAC = .FALSE.
    !
    !---- i,j size of grid for dB footprint contour plot
    NDBSIZ(1) = 21
    NDBSIZ(2) = 11
    !
    !---- number of blade-passing harmonics to be calculated, and annotation delta
    NHARM = NT / 2
    DHARM = 5.0
    !
    IF(AOC0 /= 0.0) THEN
        IF(AOC0 < 0.0) AOC0 = 0.0
        DO I = 1, II
            AOCI(I) = AOC0
        ENDDO
    ENDIF
    !
    IF(ULNAM == '(m) ') THEN
        WRITE(*, *) 'Coordinates currently specified in meters'
    ELSE
        WRITE(*, *) 'Coordinates currently specified in feet'
    ENDIF
    !
    !
    WRITE(*, 8100)
    8000 FORMAT(1X, A4, ' command not recognized.' //&
            '  Type "?" for list, <Return> to exit menu.')
    8100 FORMAT(&
            /'   P   rrr Calculate acoustic p(t) at observer x,y,z'&
            /'   FOOT rr Calculate dB ground noise footprint'&
            /'   NTIM i  Change number of time samples'&
            /'   UNIT    Toggle coordinate unit  m,ft'&
            //'   AOC  r  Set constant blade cross-sectional area/c**2'&
            /'   AFIL f  Set blade cross-sectional area/c**2 from file')
    !
    900  CONTINUE
    CALL ASKC('.NOIS^', COMAND, COMARG)
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
    IF(COMAND == '?   ') WRITE(*, 8100)
    IF(COMAND == '?   ') GO TO 900
    IF(COMAND == 'P   ') GO TO 10
    IF(COMAND == 'FOOT') GO TO 20
    IF(COMAND == 'NTIM') GO TO 25
    IF(COMAND == 'UNIT') GO TO 30
    IF(COMAND == 'AOC ') GO TO 40
    IF(COMAND == 'AFIL') GO TO 45

    WRITE(*, 8000) COMAND
    GO TO 900
    !
    !===========================================================================
    10   CONTINUE
    IF(NINPUT >= 3) THEN
        XYZOBS(1) = RINPUT(1)
        XYZOBS(2) = RINPUT(2)
        XYZOBS(3) = RINPUT(3)
    ELSE
        WRITE(*, 1050)
        1050  FORMAT(/' Cartesian system fixed to airplane.'&
                /'  (x forward, y left, z up):       '&
                /'  '&
                /'                              z         '&
                /'                                        '&
                /'                              .         '&
                /'                         x              '&
                /'                          .   .         '&
                /'                           .            '&
                /'                            . .         '&
                /'                             .          '&
                /'       y   .    .    . _______\\________ '&
                /'                               \\        '&
                /'                              __\\__     '&
                /'                                        ')
        !
        !CC              123456789012345678901234567890123     4567      890
        105  PROMPT = 'Enter observer x,y,z coordinates (' // ULNAM // '):  '
        WRITE(*, 1100) PROMPT(1:40), (XYZOBS(K), K = 1, 3)
        1100  FORMAT(1X, A, 3F12.2)
        CALL READR(3, XYZOBS, ERROR)
        IF(ERROR) GO TO 105
    ENDIF
    GO TO 100
    !cc      GO TO 900
    !
    !======================================================================
    20   CONTINUE
    IF(NINPUT >= 1) THEN
        GALT = RINPUT(1)
    ELSE
        PROMPT = 'Enter flight altitude above ground (' // ULNAM // ')^'
        CALL ASKR(PROMPT, GALT)
    ENDIF
    IF(NINPUT >= 2) THEN
        DCLIMB = RINPUT(2)
    ELSE
        CALL ASKR('Enter climb angle (deg)^', DCLIMB)
    ENDIF
    !
    !---- set default ground-grid limits
    XLIM(1) = -2.0 * GALT
    XLIM(2) = 2.0 * GALT
    YLIM(1) = -1.0 * GALT
    YLIM(2) = 1.0 * GALT
    !
    WRITE(*, *)
    1210 FORMAT(1X, A, 2F10.0)
    !CC             1234567890123456789012345     6789      012
    201  PROMPT = 'Enter footprint x limits (' // ULNAM // '):  '
    WRITE(*, 1210) PROMPT(1:32), XLIM(1), XLIM(2)
    CALL READR(2, XLIM, ERROR)
    IF(ERROR) GO TO 201
    !
    202  PROMPT = 'Enter footprint y limits (' // ULNAM // '):  '
    WRITE(*, 1210) PROMPT(1:32), YLIM(1), YLIM(2)
    CALL READR(2, YLIM, ERROR)
    IF(ERROR) GO TO 202
    !
    204  WRITE(*, 1250) 'Enter footprint grid size: ', NDBSIZ(1), NDBSIZ(2)
    1250 FORMAT(1X, A, 2I6)
    CALL READI(2, NDBSIZ, ERROR)
    IF(ERROR) GO TO 204
    IF(NDBSIZ(1) > NXDIM .OR. NDBSIZ(2) > NYDIM) THEN
        WRITE(*, *) 'Array dimension limits are:', NXDIM, NYDIM
        NDBSIZ(1) = MIN(NDBSIZ(1), NXDIM)
        NDBSIZ(2) = MIN(NDBSIZ(2), NYDIM)
        GO TO 204
    ENDIF
    !
    !
    THX1 = ATAN2(XLIM(1), GALT)
    THX2 = ATAN2(XLIM(2), GALT)
    THY1 = ATAN2(YLIM(1), GALT)
    THY2 = ATAN2(YLIM(2), GALT)
    DO I = 1, NDBSIZ(1)
        DO J = 1, NDBSIZ(2)
            THX = THX1 + (THX2 - THX1) * FLOAT(I - 1) / FLOAT(NDBSIZ(1) - 1)
            THY = THY1 + (THY2 - THY1) * FLOAT(J - 1) / FLOAT(NDBSIZ(2) - 1)
            XDB(I, J) = GALT * TAN(THX)
            YDB(I, J) = GALT * TAN(THY)
        ENDDO
    ENDDO
    !
    WRITE(*, *)
    WRITE(*, *) 'Calculating dB footprint...'
    NT1 = NT
    CALL DBFOOT(NBLDS, II, XI(1), DXI, AOCI, CH, GAM, &
            ADV, RAD, VEL, VSO, RHO, &
            GALT, DCLIMB, UNITL, NT1, &
            NXDIM, NYDIM, NDBSIZ(1), NDBSIZ(2), XDB, YDB, ADB)
    LDBCON = .TRUE.
    !
    !===========================================================================
    25   CONTINUE
    IF(NINPUT >= 1) THEN
        NT = IINPUT(1)
    ELSE
        251    WRITE(*, 1251) NT
        1251   FORMAT(/1X, ' Enter number of p(t) samples/revolution:', I7)
        CALL READI(1, NT, ERROR)
        IF(ERROR) GO TO 251
    ENDIF
    !
    IF(NT > NTX) THEN
        NT = NTX
        WRITE(*, *) 'Number of samples limited to array limit:', NTX
    ENDIF
    !
    NHARM = NT / 2
    GO TO 900
    !
    !======================================================================
    30   IF(ULNAM == 'ft') THEN
        ULNAM = 'm '
        UNITL = 1.0
        WRITE(*, *) 'Coordinates now specified in meters'
    ELSE
        ULNAM = 'ft'
        UNITL = 3.28084
        WRITE(*, *) 'Coordinates now specified in feet'
    ENDIF
    GO TO 900
    !
    !===========================================================================
    40   CONTINUE
    !                                      2
    !---- set local blade airfoil  area / c
    !     (this version assumes that it's constant)
    IF(NINPUT >= 1) THEN
        AOC0 = RINPUT(1)
    ELSE
        CALL ASKR&
                ('Enter blade airfoil  (cross-sectional area)/chord**2^', AOC0)
    ENDIF
    !
    DO I = 1, II
        AOCI(I) = AOC0
    ENDDO
    !
    !---- recalculate pressure signature if observer position has been chosen
    IF(LPTRAC) GO TO 100
    GO TO 900
    !
    !===========================================================================
    45   CONTINUE
    !---- this version reads in an area distribution list with increasing r/R:
    !       A/c^2   r/R
    !       A/c^2   r/R
    !       A/c^2   r/R
    !         .      .
    !         .      .
    !
    !     These are splined to the computational radial stations.
    !
    FNAME = COMARG
    IF(FNAME(1:1) == ' ') THEN
        CALL ASKS&
                ('Enter blade airfoil area/c**2 distribution filename^', FNAME)
    ENDIF
    !
    LU = LUTEMP
    OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 459)
    DO IA = 1, IX
        READ(LU, *, END = 455, ERR = 458) XA(IA), AOC(IA)
    ENDDO
    WRITE(*, *) 'Array size limited.  Not all points read in.'
    IA = IX + 1
    455  CONTINUE
    NA = IA - 1
    CLOSE(LU)
    !
    AOCX(1:NA) = spline(XA(1:NA), AOC(1:NA))
    DO I = 1, II
        AOCI(I) = SEVAL(XI(I), AOC, AOCX, XA)
    ENDDO
    AOC0 = 0.0
    !
    !---- recalculate pressure signature if observer position has been chosen
    IF(LPTRAC) GO TO 100
    GO TO 900
    !
    458  WRITE(*, *) 'File READ error'
    CLOSE(LU)
    GO TO 900
    !
    459  WRITE(*, *) 'File OPEN error'
    GO TO 900
    !
    !===========================================================================
    !===========================================================================
    !---- p(t) signature calculation
    100  CONTINUE
    XOBS = XYZOBS(1) / UNITL
    YOBS = XYZOBS(2) / UNITL
    ZOBS = XYZOBS(3) / UNITL
    CALL PTRACE(XOBS, YOBS, ZOBS, &
            NBLDS, II, XI(1), DXI, AOCI, CH, GAM, &
            ADV, RAD, VEL, VSO, RHO, &
            NTX, NT, PCOMP, TIME)
    !
    !---- set total p(t) signal
    DO IT = 0, NT
        PRES(IT) = PCOMP(IT, 1) + PCOMP(IT, 2) + PCOMP(IT, 3)
    ENDDO
    LPTRAC = .TRUE.
    !
    !---- integrate p(t) for rms pressure
    PRMS = 0.
    DO IT = 1, NT
        DELT = TIME(IT) - TIME(IT - 1)
        PAVG = (PRES(IT) + PRES(IT - 1)) * 0.5
        PRMS = PRMS + PAVG**2 * DELT
    ENDDO
    PRMS = SQRT(PRMS / (TIME(NT) - TIME(0)))
    !
    !---- get amplitude of each blade-passing harmonic component
    CALL SFT(PRES, TIME, NT, FAMPL, PHASE, NHARM)
    !
    !---- set Decibels relative to 20 microPa for each harmonic component
    DO IH = 1, NHARM
        DECIB(IH) = 20.0 * ALOG10(SQRT(0.5) * FAMPL(IH) / 20.0E-6)
    ENDDO
    !
    !---- set total dB level from r.m.s. pressure
    DECIB(0) = 20.0 * ALOG10(PRMS / 20.0E-6)
    !
    !---- print out decibel spectrum
    WRITE(*, 5000)  0, DECIB(0)
    WRITE(*, 5010) (K, DECIB(K), K = 1, NHARM)
    5000 FORMAT(&
            /' Sound level for each multiple of blade-passing frequency'&
            //'      n      dB', &
            /    1X, I6, F9.2, '  (total)')
    5010 FORMAT(1X, I6, F9.2)
    !CC            12    95.02
    !
    !CC      GO TO 900
    !.....................................................................
END


SUBROUTINE PTRACE(XOBS, YOBS, ZOBS, &
        NBLDS, II, XI, DXI, AOC, CH, GAM, &
        ADV, RAD, VEL, VSO, RHO, &
        NTMAX, NT, PRES, TIME)
    !------------------------------------------------------------------------
    !     Calculates acoustic pressure p(t) trace
    !     over one blade-passing period.
    !
    !     Input:
    !        XOBS    observer location relative to prop
    !        YOBS     (X = fwd, Y = left, Z = up)
    !        ZOBS
    !        NBLDS   number of blades
    !        II      number of radial stations
    !        XI(.)   r/R radial coordinate array
    !        CH(.)   c/R chord distribution
    !        AOC(.)  airfoil area/chord**2 distribution
    !        GAM(.)  Gamma/VR circulation distribution
    !        ADV     advance ratio  V/wR
    !        RAD     tip radius R
    !        VEL     freestream speed V
    !        VSO     freestream speed of sound
    !        RHO     freestream density
    !        NT      number of circumferential prop positions to be sampled
    !                and length of PRES,TIME arrays returned
    !                  (NT=90 works well for most cases)
    !     Output:
    !        PRES(i,1)  near-field loading pressure,  i = 1..NT
    !        PRES(i,2)  far-field  loading pressure
    !        PRES(i,3)  thickness pressure
    !        TIME(i,)   time coordinate for PRES
    !                  (over one blade-passing period, non-uniformly spaced)
    !
    !------------------------------------------------------------------------
    IMPLICIT REAL(A-H, M, O-Z)
    !
    DIMENSION AOC(II), XI(II), DXI(II), CH(II), GAM(II)
    DIMENSION PRES(0:NTMAX, 3), TIME(0:NTMAX)
    !
    PARAMETER (NTX = 160, IX = 40)
    DIMENSION PEL(0:NTX, IX, 3), TEL(0:NTX, IX), PEL_T(0:NTX, IX)
    !
    PI = 4.0 * ATAN(1.0)
    !
    IF(II > IX) STOP 'PTRACE: Array overflow. IX too small.'
    IF(NT > NTX) STOP 'PTRACE: Array overflow. NTX too small.'
    !
    !---- prop rotational speed
    OMEGA = VEL / (ADV * RAD)
    !
    !---- freestream and tip speed Mach numbers
    MACH = VEL / VSO
    MTIP = MACH / ADV
    !
    RHOVR = RHO * VEL * RAD
    VR = VEL * RAD
    !
    !---- set distance to observer and average acoustic delay time
    ROBS = SQRT(XOBS**2 + YOBS**2 + ZOBS**2)
    TDELAY = ROBS / VSO
    !
    THOBS = ATAN2(-YOBS, ZOBS)
    !
    !---- rotate one blade through 360 degrees in NT steps
    do IT = 0, NT
        !
        !------ set rotation angle,  TH=0 is for blade vertical (along Z direction)
        TH = 2.0 * PI * FLOAT(IT) / FLOAT(NT) + THOBS + 0.5 * PI
        !
        SINT = SIN(TH)
        COST = COS(TH)
        !
        !------ set retarded time
        TAU = TH / OMEGA
        !
        !------ go over blade elements at current rotation angle
        do I = 1, II
            !
            XX = XI(I) / ADV
            !
            !-------- components and modulus of vector from blade element to observer
            X = XOBS
            Y = YOBS + XI(I) * RAD * SINT
            Z = ZOBS - XI(I) * RAD * COST
            R = SQRT(X * X + Y * Y + Z * Z)
            !
            !-------- unit vector to observer
            XN = X / R
            YN = Y / R
            ZN = Z / R
            !
            !-------- time derivative of vector from blade element to observer
            XT = -VEL
            YT = VEL * XX * COST
            ZT = VEL * XX * SINT
            RT = (X * XT + Y * YT + Z * ZT) / R
            !
            !-------- 2nd time derivative of vector from blade element to observer
            XTT = 0.
            YTT = -VEL * XX * SINT * OMEGA
            ZTT = VEL * XX * COST * OMEGA
            RTT = (X * XTT + Y * YTT + Z * ZTT) / R&
                    + (XT * XT + YT * YT + ZT * ZT) / R&
                    - RT * RT / R
            !
            !-------- Mach number components of blade element relative to still air
            MAX = MACH
            MAY = -MACH * XX * COST
            MAZ = -MACH * XX * SINT
            !
            !-------- time derivatives of Mach number components
            MAXT = 0.
            MAYT = MACH * XX * SINT * OMEGA
            MAZT = -MACH * XX * COST * OMEGA
            !
            !-------- 2nd time derivatives of Mach number components
            MAXTT = 0.
            MAYTT = MACH * XX * COST * OMEGA**2
            MAZTT = MACH * XX * SINT * OMEGA**2
            !
            !-------- components of lift force on air by blade element
            FX = -RHOVR * GAM(I) * XX * DXI(I) * VR
            FY = -RHOVR * GAM(I) * COST * DXI(I) * VR
            FZ = -RHOVR * GAM(I) * SINT * DXI(I) * VR
            !
            !-------- time derivative of lift force
            FXT = 0.
            FYT = RHOVR * GAM(I) * SINT * DXI(I) * VR * OMEGA
            FZT = -RHOVR * GAM(I) * COST * DXI(I) * VR * OMEGA
            !
            !-------- Mach number component along blade element --> observer direction
            MR = (X * MAX + Y * MAY + Z * MAZ) / R
            !
            IF(MR >= 1.0) THEN
                WRITE(*, 5500) MR, XI(I), (TH * 180.0 / PI)
                5500      FORMAT(/' WARNING.  Relative approach Mach number =', F6.3, &
                        '  at r/R =', F6.3, '    theta =', F6.1, ' deg.')
                MR = 0.995
            ENDIF
            !
            MRI = 1.0 / (1.0 - MR)
            !
            !-------- assorted time derivatives
            MRT = (XT * MAX + YT * MAY + ZT * MAZ) / R&
                    + (X * MAXT + Y * MAYT + Z * MAZT) / R&
                    - RT * MR / R
            MRIT = MRI**2 * MRT
            !
            MRTT = (XTT * MAX + YTT * MAY + ZTT * MAZ) / R&
                    + 2.0 * (XT * MAXT + YT * MAYT + ZT * MAZT) / R&
                    + (X * MAXTT + Y * MAYTT + Z * MAZTT) / R&
                    - 2.0 * RT * MRT / R - RTT * MR / R
            MRITT = 2.0 * MRI * MRIT * MRT + MRI**2 * MRTT
            !
            !-------- various dot products
            RDOTF = XN * FX + YN * FY + ZN * FZ
            RDOTFT = XN * FXT + YN * FYT + ZN * FZT
            RDOTMT = XN * MAXT + YN * MAYT + ZN * MAZT
            !
            MDOTM = MAX * MAX + MAY * MAY + MAZ * MAZ
            FDOTM = FX * MAX + FY * MAY + FZ * MAZ
            !
            !-------- set far-field and near-field pressures due to lift
            PLFF = (RDOTFT / VSO + RDOTF * MRI * RDOTMT / VSO) * MRI**2 / R
            PLNF = (RDOTF * (1.0 - MDOTM) * MRI - FDOTM) * MRI**2 / R**2
            !
            !-------- set unit pressure due to thickness
            MOR = MRI / R
            MORT = MRIT / R - RT * MOR / R
            MORTT = MRITT / R - RTT * MOR / R - 2.0 * RT * MORT / R
            PTU = MRI * (MRIT * MORT + MRI * MORTT)
            !
            !-------- set pressure due to thickness
            PT = PTU * RHO * AOC(I) * CH(I)**2 * DXI(I) * RAD**3
            !
            !-------- set entire pressure due to blade element at retarded time
            PEL(IT, I, 1) = PLFF / (4.0 * PI)
            PEL(IT, I, 2) = PLNF / (4.0 * PI)
            PEL(IT, I, 3) = PT / (4.0 * PI)
            !
            !-------- set observer time at which he will see the pressure signal
            !         (subtract off average delay time to make observer time near zero)
            TOB = TAU + R / VSO
            TEL(IT, I) = TOB - TDELAY
            !
        end do
    end do
    !
    !
    !---- make sure pressure is exactly periodic
    DO I = 1, II
        PEL(NT, I, 1) = PEL(0, I, 1)
        PEL(NT, I, 2) = PEL(0, I, 2)
        PEL(NT, I, 3) = PEL(0, I, 3)
    ENDDO
    !
    !
    !---- full-rotation time
    ROTIME = 2.0 * PI / OMEGA
    !
    !---- blade-passing time
    BPTIME = ROTIME / FLOAT(NBLDS)
    !
    !---- set time array over one blade-passing time
    !###
    !c      DO IT=1, NT
    !c        TIME(IT) = BPTIME * FLOAT(IT)/FLOAT(NT)
    !c      ENDDO

    I = II
    TMID = TEL(NT / 2, I)
    DO IT = 0, NT
        TIME(IT) = TMID + (TEL(IT, I) - TMID) / FLOAT(NBLDS)
    ENDDO
    !
    !---- go over pressure components
    do L = 1, 3
        !------ periodic-spline p(t) for each blade element
        DO I = 1, II
            CALL PSPLIN(PEL(0, I, L), PEL_T(0, I), TEL(0, I), NT + 1)
        ENDDO
        !
        !------ set total acoustic p(t) by adding up all blade elements from all blades
        do IT = 1, NT
            PSUM = 0.
            !
            !-------- go over all radial stations
            do I = 1, II
                TEL0 = TEL(0, I)
                TELN = TEL(NT, I)
                !
                !---------- go over all blades at this radial station
                DO IB = 1, NBLDS
                    !------------ time at which current blade passes the current blade 1 angle
                    TELB = TIME(IT) + BPTIME * FLOAT(IB - 1)
                    !
                    !------------ remove whole multiples of blade period to get into spline range
                    TOFF = TEL0 + AMOD((TELB - TEL0), (TELN - TEL0))
                    IF(TOFF < TEL0) TOFF = TOFF + (TELN - TEL0)
                    !
                    IF(TOFF < TEL0 .OR. TOFF > TELN) THEN
                        WRITE(*, *) '? PTRACE: Time out of spline range.'
                        WRITE(*, *) 't   t0   tN', TOFF, TEL0, TELN
                    ENDIF
                    !
                    PSUM = PSUM&
                            + SEVAL_OLD(TOFF, PEL(0, I, L), PEL_T(0, I), TEL(0, I), NT + 1)
                ENDDO
            end do
            !
            !-------- set total pressure signal at current observer time
            PRES(IT, L) = PSUM
            !
        end do
        !
        !------ make sure pressure is exactly periodic
        PRES(0, L) = PRES(NT, L)
    end do
    !
    TIME0 = TIME(0)
    DO IT = 0, NT
        TIME(IT) = TIME(IT) - TIME0
    ENDDO
    !
    !---- subtract off average pressure
    DO L = 1, 3
        PINT = 0.
        DO IT = 1, NT
            PINT = PINT + (PRES(IT, L) + PRES(IT - 1, L)) * (TIME(IT) - TIME(IT - 1))
        ENDDO
        PAVG = PINT * 0.5 / (TIME(NT) - TIME(0))
        !
        DO IT = 0, NT
            PRES(IT, L) = PRES(IT, L) - PAVG
        ENDDO
    ENDDO
    !
    RETURN
END


SUBROUTINE SFT(Y, T, N, FAMPL, PHASE, NF)
    DIMENSION Y(N), T(N)
    DIMENSION FAMPL(NF), PHASE(NF)
    !---------------------------------------------------
    !     Calculates Slow Fourier Transform of periodic
    !     input function Y(T) with period T(N+1)-T(1).
    !     Hence, Y(1) and Y(N+1) should be equal.
    !     C(K) is the complex amplitude of the K'th
    !     multiple of the fundamental harmonic.
    !     The first NF harmonics are calculated.
    !---------------------------------------------------
    DIMENSION SINT(361), COST(361)
    !
    IF(N + 1 > 361) STOP 'SFT: Array overflow'
    !
    PI = 4.0 * ATAN(1.0)
    !
    OMEGA = 2.0 * PI / (T(N + 1) - T(1))
    !
    do K = 1, NF
        RK = FLOAT(K)
        !
        do I = 1, N
            TK = OMEGA * RK * T(I)
            SINT(I) = SIN(TK)
            COST(I) = COS(TK)
        end do
        SINT(N + 1) = SINT(1)
        COST(N + 1) = COST(1)
        !
        SSUM = 0.
        CSUM = 0.
        do IO = 1, N
            IP = IO + 1
            DT = T(IP) - T(IO)
            SSUM = SSUM + 0.5 * (SINT(IO) * Y(IO) + SINT(IP) * Y(IP)) * DT
            CSUM = CSUM + 0.5 * (COST(IO) * Y(IO) + COST(IP) * Y(IP)) * DT
        end do
        !
        FAMPL(K) = SQRT(SSUM**2 + CSUM**2) * OMEGA / PI
        PHASE(K) = ATAN2(SSUM, CSUM)
    end do
    !
    RETURN
END
! SFT


SUBROUTINE PSPLIN(X, XP, S, II)
    !
    DIMENSION X(II), XP(II), S(II)
    DIMENSION A(480), B(480), C(480)
    !
    IF(II > 480)     STOP 'PSPLIN: Array overflow'
    IF(X(II) /= X(1)) STOP 'PSPLIN: Data not periodic'
    !
    do I = 1, II - 1
        !
        !------ Periodic point
        IF(I == 1) THEN
            DSMI = 1.0 / (S(II) - S(II - 1))
            DXM = X(II) - X(II - 1)
            DSPI = 1.0 / (S(I + 1) - S(I))
            DXP = X(I + 1) - X(I)
            !
            !------ Interior points
        ELSE
            DSMI = 1.0 / (S(I) - S(I - 1))
            DXM = X(I) - X(I - 1)
            DSPI = 1.0 / (S(I + 1) - S(I))
            DXP = X(I + 1) - X(I)
        ENDIF
        !
        B(I) = DSMI
        A(I) = 2.0 * (DSMI + DSPI)
        C(I) = DSPI
        XP(I) = 3.0 * (DXP * DSPI**2 + DXM * DSMI**2)
        !
    end do
    !
    CALL PTRISO(A, B, C, XP, II - 1)
    !
    XP(II) = XP(1)
    !
    RETURN
END


SUBROUTINE PTRISO(A, B, C, D, KK)
    !
    DIMENSION A(KK), B(KK), C(KK), D(KK)
    !
    do K = 2, KK
        KM = K - 1
        AINV = 1.0 / A(KM)
        C(KM) = C(KM) * AINV
        D(KM) = D(KM) * AINV
        B(KM) = B(KM) * AINV
        A(K) = A(K) - B(K) * C(KM)
        D(K) = D(K) - B(K) * D(KM)
        IF(K < KK) THEN
            B(K) = - B(K) * B(KM)
        ELSE
            A(K) = A(K) - B(K) * B(KM)
        ENDIF
    end do
    !
    C(KK) = C(KK) / A(KK)
    D(KK) = D(KK) / A(KK)
    !
    do K = KK, 2, -1
        KM = K - 1
        D(KM) = D(KM) - C(KM) * D(K) - B(KM) * D(KK)
        C(KM) = - C(KM) * C(K) - B(KM) * C(KK)
    end do
    !
    D(1) = D(1) / (1.0 + C(1))
    !
    do K = 2, KK
        D(K) = D(K) - C(K) * D(1)
    end do
    !
    RETURN
END


SUBROUTINE DBFOOT(NBLDS, II, XI, DXI, AOC, CH, GAM, &
        ADV, RAD, VEL, VSO, RHO, &
        GALT, DCLIMB, UNITL, NT, &
        NXDIM, NYDIM, NX, NY, X, Y, D)
    !--------------------------------------------------------
    !     Calculates dB noise levels on a ground plane grid.
    !
    !     Input:
    !        NBLDS   number of blades
    !        II      number of radial stations
    !        XI(.)   r/R radial coordinate array
    !        CH(.)   c/R chord distribution
    !        AOC(.)  airfoil area/chord**2 distribution
    !        GAM(.)  Gamma/VR circulation distribution
    !        ADV     advance ratio  V/wR
    !        RAD     tip radius R
    !        VEL     freestream speed V
    !        VSO     freestream speed of sound
    !        RHO     freestream density
    !        GALT    aircraft altitude above ground plane
    !        DCLIMB  aircraft climb angle (deg)
    !        UNITL   length unit of GALT,X,Y, in meters
    !        NT      number of circumferential prop positions to be sampled
    !
    !        NXDIM,NYDIM   grid array dimensions
    !        NX,NY         grid size
    !        X(..)         ground-plane grid where dB is to be calculated
    !        Y(..)          "
    !
    !     Output:
    !        D(..)         dB sound level at ground plane
    !
    !--------------------------------------------------------
    DIMENSION AOC(II), XI(II), DXI(II), CH(II), GAM(II)
    !
    DIMENSION D(NXDIM, NYDIM)
    DIMENSION X(NXDIM, NYDIM)
    DIMENSION Y(NXDIM, NYDIM)
    !
    !---- local arrays for calculating p(t) trace
    PARAMETER (NTX = 180)
    DIMENSION PCOMP(0:NTX, 3), PRES(0:NTX), TIME(0:NTX)
    !
    !---- degrees-to-radians conversion factor
    DTR = ATAN(1.0) / 45.0
    !
    COSC = COS(DCLIMB * DTR)
    SINC = SIN(DCLIMB * DTR)
    !
    !---- find j index of y=0 line
    DELY = ABS(Y(1, NY) - Y(1, 1))
    DO J0 = 1, NY
        IF(Y(1, J0) > -0.0001 * DELY) GO TO 5
    ENDDO
    J0 = 1
    5    CONTINUE
    !
    do I = 1, NX
        WRITE(*, 1300) I, NX
        1300   FORMAT(5X, I3, ' /', I3)
        !
        do J = J0, NY
            !-------- set observer position assuming airplane is level
            XG = X(I, J) / UNITL
            YG = Y(I, J) / UNITL
            ZG = -GALT / UNITL
            !
            !-------- rotate point through climb angle about y axis
            XOBS = COSC * XG + SINC * ZG
            YOBS = YG
            ZOBS = -SINC * XG + COSC * ZG
            !
            !-------- calculate p(t) pressure signature
            CALL PTRACE(XOBS, YOBS, ZOBS, &
                    NBLDS, II, XI, DXI, AOC, CH, GAM, &
                    ADV, RAD, VEL, VSO, RHO, &
                    NTX, NT, PCOMP, TIME)
            DO IT = 0, NT
                PRES(IT) = PCOMP(IT, 1) + PCOMP(IT, 2) + PCOMP(IT, 3)
            ENDDO
            !
            !-------- set rms pressure signature and corresponding dB level
            PRMS = 0.
            DO IT = 1, NT
                DELT = TIME(IT) - TIME(IT - 1)
                PAVG = (PRES(IT) + PRES(IT - 1)) * 0.5
                PRMS = PRMS + PAVG**2 * DELT
            ENDDO
            PRMS = SQRT(PRMS / (TIME(NT) - TIME(0)))
            !
            D(I, J) = 20.0 * ALOG10(PRMS / 20.0E-6)
        end do
        !
        !------ set values for negative y by symmetry
        DO J = 1, J0 - 1
            JPOS = 2 * J0 - J - 1 + MOD(NY, 2)
            D(I, J) = D(I, JPOS)
        ENDDO
        !
    end do
    !
    RETURN
END