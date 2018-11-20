C***********************************************************************
C    Module:  xnoise.f
C 
C    Copyright (C) 2011 Mark Drela 
C 
C    This program is free software; you can redistribute it and/or modify
C    it under the terms of the GNU General Public License as published by
C    the Free Software Foundation; either version 2 of the License, or
C    (at your option) any later version.
C
C    This program is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU General Public License for more details.
C
C    You should have received a copy of the GNU General Public License
C    along with this program; if not, write to the Free Software
C    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
C***********************************************************************

      SUBROUTINE NOISE
C---------------------------------------
C     Calculates the sound pressure 
C     time history of the propeller 
C     at specified observer positions.
C---------------------------------------
      INCLUDE 'XROTOR.INC'
C
      PARAMETER (NTX=160)
      PARAMETER (NHARX=NTX/2)
      DIMENSION AOCI(IX), AOC(IX), AOCX(IX), XA(IX)
      DIMENSION PCOMP(0:NTX,3), PRES(0:NTX), TIME(0:NTX)
      DIMENSION DECIB(0:NHARX), FAMPL(NHARX), PHASE(NHARX)
C
      PARAMETER (NXDIM=81,NYDIM=81)
      DIMENSION ADB(NXDIM,NYDIM)
      DIMENSION XDB(NXDIM,NYDIM)
      DIMENSION YDB(NXDIM,NYDIM)
      DIMENSION XLIM(2), YLIM(2)
      DIMENSION NDBSIZ(2)
C
      CHARACTER*80 PROMPT
      CHARACTER*4 COMAND, ANS
      CHARACTER*132 COMARG, ANSARG
C
      DIMENSION IINPUT(20)
      DIMENSION RINPUT(20)
      LOGICAL ERROR, LDBCON, LPTRAC
C
      DIMENSION XYZOBS(3)
      CHARACTER*2 ULNAM
C
      SAVE NT
      SAVE AOC0, XYZOBS
      SAVE ULNAM, UNITL
C
C---- number of rotation steps for one full rotation
      DATA NT / 80 /
C
C---- default airfoil area/c^2 , observer location
      DATA AOC0, XYZOBS / -1.0, 0.0, 0.0, -100.0 /
C
C---- start by using foot input
ccc   DATA ULNAM, UNITL / 'm ', 1.0 /
      DATA ULNAM, UNITL / 'ft', 3.28084 /
C
C
      GREEK = .FALSE.
C
      LDBCON = .FALSE.
      LPTRAC = .FALSE.
C
C---- i,j size of grid for dB footprint contour plot
      NDBSIZ(1) = 21
      NDBSIZ(2) = 11
C
C---- number of blade-passing harmonics to be calculated, and annotation delta
      NHARM = NT/2
      DHARM = 5.0
C
      IF(AOC0 .NE. 0.0) THEN
        IF(AOC0 .LT. 0.0) AOC0 = 0.0
        DO I=1, II
          AOCI(I) = AOC0
        ENDDO
      ENDIF
C
      IF(ULNAM.EQ.'(m) ') THEN
       WRITE(*,*) 'Coordinates currently specified in meters'
      ELSE
       WRITE(*,*) 'Coordinates currently specified in feet'
      ENDIF
C
C
      WRITE(*,8100)
 8000 FORMAT(1X,A4,' command not recognized.' //
     &             '  Type "?" for list, <Return> to exit menu.')
 8100 FORMAT(
     &  /'   P   rrr Calculate acoustic p(t) at observer x,y,z'
     &  /'   FOOT rr Calculate dB ground noise footprint'
     &  /'   NTIM i  Change number of time samples'
     &  /'   UNIT    Toggle coordinate unit  m,ft'
     & //'   AOC  r  Set constant blade cross-sectional area/c**2'
     &  /'   AFIL f  Set blade cross-sectional area/c**2 from file')
C
 900  CONTINUE
      CALL ASKC('.NOIS^',COMAND,COMARG)
C
      DO I=1, 20
        IINPUT(I) = 0
        RINPUT(I) = 0.0
      ENDDO
      NINPUT = 0
      CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
      NINPUT = 0
      CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C
      IF(COMAND.EQ.'    ') RETURN
      IF(COMAND.EQ.'?   ') WRITE(*,8100)
      IF(COMAND.EQ.'?   ') GO TO 900
      IF(COMAND.EQ.'P   ') GO TO 10
      IF(COMAND.EQ.'FOOT') GO TO 20
      IF(COMAND.EQ.'NTIM') GO TO 25
      IF(COMAND.EQ.'UNIT') GO TO 30
      IF(COMAND.EQ.'AOC ') GO TO 40
      IF(COMAND.EQ.'AFIL') GO TO 45

      WRITE(*,8000) COMAND
      GO TO 900
C
C===========================================================================
 10   CONTINUE
      IF(NINPUT.GE.3) THEN
       XYZOBS(1) = RINPUT(1)
       XYZOBS(2) = RINPUT(2)
       XYZOBS(3) = RINPUT(3)
      ELSE
       WRITE(*,1050)
 1050  FORMAT(/' Cartesian system fixed to airplane.'
     &        /'  (x forward, y left, z up):       '
     &        /'  '
     &        /'                              z         '
     &        /'                                        '
     &        /'                              .         '
     &        /'                         x              '
     &        /'                          .   .         '
     &        /'                           .            '
     &        /'                            . .         '
     &        /'                             .          '
     &        /'       y   .    .    . _______\\________ '
     &        /'                               \\        '
     &        /'                              __\\__     '
     &        /'                                        ' )
C
CCC              123456789012345678901234567890123     4567      890
  105  PROMPT = 'Enter observer x,y,z coordinates (' // ULNAM // '):  '
       WRITE(*,1100) PROMPT(1:40), (XYZOBS(K), K=1, 3)
 1100  FORMAT(1X,A, 3F12.2)
       CALL READR(3,XYZOBS,ERROR)
       IF(ERROR) GO TO 105
      ENDIF
      GO TO 100
ccc      GO TO 900
C
C======================================================================
 20   CONTINUE
      IF(NINPUT.GE.1) THEN
        GALT = RINPUT(1)
      ELSE
        PROMPT = 'Enter flight altitude above ground (' // ULNAM // ')^'
        CALL ASKR(PROMPT,GALT)
      ENDIF
      IF(NINPUT.GE.2) THEN
        DCLIMB = RINPUT(2)
      ELSE
        CALL ASKR('Enter climb angle (deg)^',DCLIMB)
      ENDIF
C
C---- set default ground-grid limits
      XLIM(1) = -2.0*GALT
      XLIM(2) =  2.0*GALT
      YLIM(1) = -1.0*GALT
      YLIM(2) =  1.0*GALT
C
      WRITE(*,*)
 1210 FORMAT(1X,A,2F10.0)
CCC             1234567890123456789012345     6789      012
 201  PROMPT = 'Enter footprint x limits (' // ULNAM // '):  '
      WRITE(*,1210) PROMPT(1:32), XLIM(1), XLIM(2)
      CALL READR(2,XLIM,ERROR)
      IF(ERROR) GO TO 201
C
 202  PROMPT = 'Enter footprint y limits (' // ULNAM // '):  '
      WRITE(*,1210) PROMPT(1:32), YLIM(1), YLIM(2)
      CALL READR(2,YLIM,ERROR)
      IF(ERROR) GO TO 202
C
 204  WRITE(*,1250) 'Enter footprint grid size: ', NDBSIZ(1), NDBSIZ(2)
 1250 FORMAT(1X,A,2I6)
      CALL READI(2,NDBSIZ,ERROR)
      IF(ERROR) GO TO 204
      IF(NDBSIZ(1).GT.NXDIM .OR. NDBSIZ(2).GT.NYDIM) THEN
        WRITE(*,*) 'Array dimension limits are:', NXDIM, NYDIM
        NDBSIZ(1) = MIN(NDBSIZ(1),NXDIM)
        NDBSIZ(2) = MIN(NDBSIZ(2),NYDIM)
        GO TO 204
      ENDIF
C
C
      THX1 = ATAN2(XLIM(1),GALT)
      THX2 = ATAN2(XLIM(2),GALT)
      THY1 = ATAN2(YLIM(1),GALT)
      THY2 = ATAN2(YLIM(2),GALT)
      DO I = 1, NDBSIZ(1)
        DO J = 1, NDBSIZ(2)
          THX = THX1 + (THX2-THX1)*FLOAT(I-1)/FLOAT(NDBSIZ(1)-1)
          THY = THY1 + (THY2-THY1)*FLOAT(J-1)/FLOAT(NDBSIZ(2)-1)
          XDB(I,J) = GALT*TAN(THX)
          YDB(I,J) = GALT*TAN(THY)
        ENDDO
      ENDDO
C
      WRITE(*,*)
      WRITE(*,*) 'Calculating dB footprint...'
      NT1 = NT
      CALL DBFOOT(NBLDS,II,XI(1),DXI,AOCI,CH,GAM,
     &            ADV,RAD,VEL,VSO,RHO,
     &            GALT,DCLIMB,UNITL, NT1,
     &            NXDIM,NYDIM,NDBSIZ(1),NDBSIZ(2),XDB,YDB,ADB)
      LDBCON = .TRUE.
C
C===========================================================================
 25   CONTINUE
      IF(NINPUT.GE.1) THEN
        NT = IINPUT(1)
      ELSE
 251    WRITE(*,1251) NT
 1251   FORMAT(/1X,' Enter number of p(t) samples/revolution:', I7)
        CALL READI(1,NT,ERROR)
        IF(ERROR) GO TO 251
      ENDIF
C
      IF(NT.GT.NTX) THEN
        NT = NTX
        WRITE(*,*) 'Number of samples limited to array limit:', NTX
      ENDIF
C
      NHARM = NT/2
      GO TO 900
C
C======================================================================
 30   IF(ULNAM.EQ.'ft') THEN
       ULNAM = 'm '
       UNITL = 1.0
       WRITE(*,*) 'Coordinates now specified in meters'
      ELSE
       ULNAM = 'ft'
       UNITL = 3.28084
       WRITE(*,*) 'Coordinates now specified in feet'
      ENDIF
      GO TO 900
C
C===========================================================================
 40   CONTINUE
C                                      2 
C---- set local blade airfoil  area / c
C     (this version assumes that it's constant)
      IF(NINPUT.GE.1) THEN
        AOC0 = RINPUT(1)
      ELSE
        CALL ASKR
     &  ('Enter blade airfoil  (cross-sectional area)/chord**2^',AOC0)
      ENDIF
C
      DO I=1, II
        AOCI(I) = AOC0
      ENDDO
C
C---- recalculate pressure signature if observer position has been chosen
      IF(LPTRAC) GO TO 100
      GO TO 900
C
C===========================================================================
 45   CONTINUE
C---- this version reads in an area distribution list with increasing r/R:
C       A/c^2   r/R 
C       A/c^2   r/R 
C       A/c^2   r/R 
C         .      .
C         .      .
C
C     These are splined to the computational radial stations.
C
      FNAME = COMARG
      IF(FNAME(1:1) .EQ. ' ') THEN
       CALL ASKS
     &  ('Enter blade airfoil area/c**2 distribution filename^',FNAME)
      ENDIF
C
      LU = LUTEMP
      OPEN(LU,FILE=FNAME,STATUS='OLD',ERR=459)
      DO IA=1, IX
        READ(LU,*,END=455,ERR=458) XA(IA), AOC(IA)
      ENDDO
      WRITE(*,*) 'Array size limited.  Not all points read in.'
      IA = IX+1
 455  CONTINUE
      NA = IA-1
      CLOSE(LU)
C
      CALL SPLINE(AOC,AOCX,XA,NA)
      DO I=1, II
        AOCI(I) = SEVAL(XI(I),AOC,AOCX,XA,NA)
      ENDDO
      AOC0 = 0.0
C
C---- recalculate pressure signature if observer position has been chosen
      IF(LPTRAC) GO TO 100
      GO TO 900
C
 458  WRITE(*,*) 'File READ error'
      CLOSE(LU)
      GO TO 900
C
 459  WRITE(*,*) 'File OPEN error'
      GO TO 900
C
C===========================================================================
C===========================================================================
C---- p(t) signature calculation
 100  CONTINUE
      XOBS = XYZOBS(1)/UNITL
      YOBS = XYZOBS(2)/UNITL
      ZOBS = XYZOBS(3)/UNITL
      CALL PTRACE(XOBS,YOBS,ZOBS,
     &            NBLDS,II,XI(1),DXI,AOCI,CH,GAM,
     &            ADV,RAD,VEL,VSO,RHO,
     &            NTX,NT, PCOMP, TIME)
C
C---- set total p(t) signal
      DO IT=0, NT
        PRES(IT) = PCOMP(IT,1) + PCOMP(IT,2) + PCOMP(IT,3)
      ENDDO
      LPTRAC = .TRUE.
C
C---- integrate p(t) for rms pressure
      PRMS = 0.
      DO IT=1, NT
        DELT =  TIME(IT) - TIME(IT-1)
        PAVG = (PRES(IT) + PRES(IT-1))*0.5
        PRMS = PRMS + PAVG**2 * DELT
      ENDDO
      PRMS = SQRT(PRMS/(TIME(NT)-TIME(0)))
C
C---- get amplitude of each blade-passing harmonic component
      CALL SFT(PRES,TIME,NT,FAMPL,PHASE,NHARM)
C
C---- set Decibels relative to 20 microPa for each harmonic component
      DO IH=1, NHARM
        DECIB(IH) = 20.0 * ALOG10( SQRT(0.5)*FAMPL(IH) / 20.0E-6 )
      ENDDO
C
C---- set total dB level from r.m.s. pressure
      DECIB(0) = 20.0 * ALOG10( PRMS / 20.0E-6 )
C
C---- print out decibel spectrum
      WRITE(*,5000)  0,DECIB(0)
      WRITE(*,5010) (K,DECIB(K), K=1,NHARM)
 5000 FORMAT(
     &  /' Sound level for each multiple of blade-passing frequency'
     & //'      n      dB',
     &  /    1X, I6, F9.2, '  (total)')
 5010 FORMAT(1X, I6, F9.2 )
CCC            12    95.02
C
CCC      GO TO 900
C.....................................................................
      END



      SUBROUTINE PTRACE(XOBS,YOBS,ZOBS,
     &                  NBLDS,II,XI,DXI,AOC,CH,GAM,
     &                  ADV,RAD,VEL,VSO,RHO,
     &                  NTMAX,NT, PRES, TIME)
C------------------------------------------------------------------------
C     Calculates acoustic pressure p(t) trace 
C     over one blade-passing period.
C
C     Input:
C        XOBS    observer location relative to prop
C        YOBS     (X = fwd, Y = left, Z = up)
C        ZOBS
C        NBLDS   number of blades
C        II      number of radial stations
C        XI(.)   r/R radial coordinate array
C        CH(.)   c/R chord distribution
C        AOC(.)  airfoil area/chord**2 distribution
C        GAM(.)  Gamma/VR circulation distribution
C        ADV     advance ratio  V/wR
C        RAD     tip radius R
C        VEL     freestream speed V
C        VSO     freestream speed of sound
C        RHO     freestream density
C        NT      number of circumferential prop positions to be sampled
C                and length of PRES,TIME arrays returned
C                  (NT=90 works well for most cases)
C     Output:
C        PRES(i,1)  near-field loading pressure,  i = 1..NT
C        PRES(i,2)  far-field  loading pressure
C        PRES(i,3)  thickness pressure
C        TIME(i,)   time coordinate for PRES
C                  (over one blade-passing period, non-uniformly spaced)
C
C------------------------------------------------------------------------
      IMPLICIT REAL(A-H,M,O-Z)
C
      DIMENSION AOC(II), XI(II), DXI(II), CH(II), GAM(II)
      DIMENSION PRES(0:NTMAX,3), TIME(0:NTMAX)
C
      PARAMETER (NTX=160,IX=40)
      DIMENSION PEL(0:NTX,IX,3), TEL(0:NTX,IX), PEL_T(0:NTX,IX)
C
      PI = 4.0*ATAN(1.0)
C
      IF(II .GT.IX ) STOP 'PTRACE: Array overflow. IX too small.'
      IF(NT.GT.NTX) STOP 'PTRACE: Array overflow. NTX too small.'
C
C---- prop rotational speed
      OMEGA = VEL / (ADV*RAD)
C
C---- freestream and tip speed Mach numbers
      MACH = VEL/VSO
      MTIP = MACH/ADV
C
      RHOVR = RHO*VEL*RAD
      VR    =     VEL*RAD
C
C---- set distance to observer and average acoustic delay time
      ROBS = SQRT(XOBS**2 + YOBS**2 + ZOBS**2)
      TDELAY = ROBS/VSO
C
      THOBS = ATAN2(-YOBS,ZOBS)
C
C---- rotate one blade through 360 degrees in NT steps
      DO 5 IT=0, NT
C
C------ set rotation angle,  TH=0 is for blade vertical (along Z direction)
        TH = 2.0*PI * FLOAT(IT)/FLOAT(NT) + THOBS + 0.5*PI
C
        SINT = SIN(TH)
        COST = COS(TH)
C
C------ set retarded time
        TAU = TH/OMEGA
C
C------ go over blade elements at current rotation angle
        DO 51 I=1, II
C
          XX = XI(I)/ADV
C
C-------- components and modulus of vector from blade element to observer
          X = XOBS
          Y = YOBS + XI(I)*RAD*SINT
          Z = ZOBS - XI(I)*RAD*COST
          R = SQRT(X*X + Y*Y + Z*Z)
C
C-------- unit vector to observer
          XN = X/R
          YN = Y/R
          ZN = Z/R
C
C-------- time derivative of vector from blade element to observer
          XT = -VEL
          YT =  VEL * XX*COST
          ZT =  VEL * XX*SINT
          RT = (X*XT + Y*YT + Z*ZT) / R
C
C-------- 2nd time derivative of vector from blade element to observer
          XTT =  0.
          YTT = -VEL * XX*SINT * OMEGA
          ZTT =  VEL * XX*COST * OMEGA
          RTT = (X *XTT + Y *YTT + Z *ZTT) / R
     &        + (XT*XT  + YT*YT  + ZT*ZT ) / R
     &        - RT * RT/R
C
C-------- Mach number components of blade element relative to still air
          MAX =  MACH
          MAY = -MACH * XX*COST
          MAZ = -MACH * XX*SINT
C
C-------- time derivatives of Mach number components
          MAXT =  0.
          MAYT =  MACH * XX*SINT * OMEGA
          MAZT = -MACH * XX*COST * OMEGA
C
C-------- 2nd time derivatives of Mach number components
          MAXTT =  0.
          MAYTT =  MACH * XX*COST * OMEGA**2
          MAZTT =  MACH * XX*SINT * OMEGA**2
C
C-------- components of lift force on air by blade element
          FX =  -RHOVR*GAM(I) * XX  *DXI(I)*VR
          FY =  -RHOVR*GAM(I) * COST*DXI(I)*VR
          FZ =  -RHOVR*GAM(I) * SINT*DXI(I)*VR
C
C-------- time derivative of lift force
          FXT =  0.
          FYT =  RHOVR*GAM(I) * SINT*DXI(I)*VR * OMEGA
          FZT = -RHOVR*GAM(I) * COST*DXI(I)*VR * OMEGA
C
C-------- Mach number component along blade element --> observer direction
          MR = (X*MAX + Y*MAY + Z*MAZ)/R
C
          IF(MR.GE.1.0) THEN
           WRITE(*,5500) MR, XI(I), (TH*180.0/PI)
 5500      FORMAT(/' WARNING.  Relative approach Mach number =', F6.3,
     &             '  at r/R =', F6.3,'    theta =', F6.1, ' deg.')
           MR = 0.995
          ENDIF
C
          MRI = 1.0 / (1.0 - MR)
C
C-------- assorted time derivatives
          MRT = (XT*MAX  + YT*MAY  + ZT*MAZ )/R
     &        + (X *MAXT + Y *MAYT + Z *MAZT)/R
     &        - RT * MR/R
          MRIT = MRI**2 * MRT
C
          MRTT =     (XTT*MAX   + YTT*MAY   + ZTT*MAZ  )/R
     &         + 2.0*(XT *MAXT  + YT *MAYT  + ZT *MAZT )/R
     &         +     (X  *MAXTT + Y  *MAYTT + Z  *MAZTT)/R
     &         - 2.0*RT * MRT/R  -  RTT * MR/R
          MRITT = 2.0*MRI*MRIT * MRT  +  MRI**2 * MRTT
C
C-------- various dot products
          RDOTF  = XN*FX   + YN*FY   + ZN*FZ
          RDOTFT = XN*FXT  + YN*FYT  + ZN*FZT
          RDOTMT = XN*MAXT + YN*MAYT + ZN*MAZT
C
          MDOTM = MAX*MAX + MAY*MAY + MAZ*MAZ
          FDOTM =  FX*MAX +  FY*MAY +  FZ*MAZ
C
C-------- set far-field and near-field pressures due to lift
          PLFF = (RDOTFT/VSO + RDOTF*MRI*RDOTMT/VSO) * MRI**2 / R
          PLNF = (RDOTF*(1.0-MDOTM)*MRI - FDOTM    ) * MRI**2 / R**2
C
C-------- set unit pressure due to thickness
          MOR   = MRI  /R
          MORT  = MRIT /R - RT *MOR/R
          MORTT = MRITT/R - RTT*MOR/R - 2.0*RT*MORT/R
          PTU = MRI*(MRIT*MORT + MRI*MORTT)
C
C-------- set pressure due to thickness
          PT = PTU * RHO*AOC(I)*CH(I)**2 * DXI(I) * RAD**3
C
C-------- set entire pressure due to blade element at retarded time
          PEL(IT,I,1) = PLFF / (4.0*PI)
          PEL(IT,I,2) = PLNF / (4.0*PI)
          PEL(IT,I,3) = PT   / (4.0*PI)
C
C-------- set observer time at which he will see the pressure signal
C         (subtract off average delay time to make observer time near zero)
          TOB = TAU + R/VSO
          TEL(IT,I) = TOB - TDELAY
C
 51     CONTINUE
 5    CONTINUE
C
C
C---- make sure pressure is exactly periodic
      DO I=1, II
        PEL(NT,I,1) = PEL(0,I,1)
        PEL(NT,I,2) = PEL(0,I,2)
        PEL(NT,I,3) = PEL(0,I,3)
      ENDDO
C
C
C---- full-rotation time
      ROTIME = 2.0*PI / OMEGA
C
C---- blade-passing time
      BPTIME = ROTIME/FLOAT(NBLDS)
C
C---- set time array over one blade-passing time
c###
cc      DO IT=1, NT
cc        TIME(IT) = BPTIME * FLOAT(IT)/FLOAT(NT)
cc      ENDDO

      I = II
      TMID = TEL(NT/2,I)
      DO IT=0, NT
        TIME(IT) = TMID + (TEL(IT,I)-TMID)/FLOAT(NBLDS)
      ENDDO
C
C---- go over pressure components
      DO 10 L=1, 3
C------ periodic-spline p(t) for each blade element
        DO I=1, II
          CALL PSPLIN(PEL(0,I,L),PEL_T(0,I),TEL(0,I),NT+1)
        ENDDO
C
C------ set total acoustic p(t) by adding up all blade elements from all blades
        DO 105 IT=1, NT
          PSUM = 0.
C
C-------- go over all radial stations
          DO 1055 I=1, II
            TEL0 = TEL(0  ,I)
            TELN = TEL(NT,I)
C
C---------- go over all blades at this radial station
            DO IB=1, NBLDS
C------------ time at which current blade passes the current blade 1 angle
              TELB = TIME(IT) + BPTIME*FLOAT(IB-1)
C
C------------ remove whole multiples of blade period to get into spline range
              TOFF = TEL0 + AMOD((TELB-TEL0),(TELN-TEL0))
              IF(TOFF.LT.TEL0) TOFF = TOFF + (TELN-TEL0)
C
              IF(TOFF.LT.TEL0 .OR. TOFF.GT.TELN) THEN
               WRITE(*,*) '? PTRACE: Time out of spline range.'
               WRITE(*,*) 't   t0   tN', TOFF, TEL0, TELN
              ENDIF
C
              PSUM = PSUM
     &             + SEVAL(TOFF,PEL(0,I,L),PEL_T(0,I),TEL(0,I),NT+1)
            ENDDO
 1055     CONTINUE
C
C-------- set total pressure signal at current observer time
          PRES(IT,L) = PSUM
C
 105    CONTINUE
C
C------ make sure pressure is exactly periodic
        PRES(0,L) = PRES(NT,L)
 10   CONTINUE
C
      TIME0 = TIME(0)
      DO IT=0, NT
        TIME(IT) = TIME(IT) - TIME0
      ENDDO
C
C---- subtract off average pressure
      DO L=1, 3
        PINT = 0.
        DO IT=1, NT
          PINT = PINT + (PRES(IT,L)+PRES(IT-1,L))*(TIME(IT)-TIME(IT-1))
        ENDDO
        PAVG = PINT * 0.5/(TIME(NT) - TIME(0))
C
        DO IT=0, NT
          PRES(IT,L) = PRES(IT,L) - PAVG
        ENDDO
      ENDDO
C
      RETURN
      END


      SUBROUTINE SFT(Y,T,N, FAMPL, PHASE,NF)
      DIMENSION Y(N), T(N)
      DIMENSION FAMPL(NF), PHASE(NF)
C---------------------------------------------------
C     Calculates Slow Fourier Transform of periodic 
C     input function Y(T) with period T(N+1)-T(1).
C     Hence, Y(1) and Y(N+1) should be equal.
C     C(K) is the complex amplitude of the K'th 
C     multiple of the fundamental harmonic.
C     The first NF harmonics are calculated.
C---------------------------------------------------
      DIMENSION SINT(361), COST(361)
C
      IF(N+1.GT.361) STOP 'SFT: Array overflow'
C
      PI = 4.0*ATAN(1.0)
C
      OMEGA = 2.0*PI/(T(N+1) - T(1))
C
      DO 10 K=1, NF
        RK = FLOAT(K)
C
        DO 110 I=1, N
          TK = OMEGA*RK*T(I)
          SINT(I) = SIN(TK)
          COST(I) = COS(TK)
  110   CONTINUE
        SINT(N+1) = SINT(1)
        COST(N+1) = COST(1)
C
        SSUM = 0.
        CSUM = 0.
        DO 120 IO=1, N
          IP = IO + 1
          DT = T(IP) - T(IO)
          SSUM = SSUM + 0.5*(SINT(IO)*Y(IO) + SINT(IP)*Y(IP))*DT
          CSUM = CSUM + 0.5*(COST(IO)*Y(IO) + COST(IP)*Y(IP))*DT
  120   CONTINUE
C
        FAMPL(K) = SQRT(SSUM**2 + CSUM**2)*OMEGA / PI
        PHASE(K) = ATAN2( SSUM , CSUM )
   10 CONTINUE
C
      RETURN
      END ! SFT


      SUBROUTINE PSPLIN(X,XP,S,II)
C
      DIMENSION X(II),XP(II),S(II)
      DIMENSION A(480),B(480),C(480)
C
      IF(II.GT.480)     STOP 'PSPLIN: Array overflow'
      IF(X(II).NE.X(1)) STOP 'PSPLIN: Data not periodic'
C
      DO 1 I = 1, II-1
C
C------ Periodic point
        IF(I.EQ.1) THEN
         DSMI = 1.0 / (S(II) - S(II-1))
         DXM  = X(II) - X(II-1)
         DSPI = 1.0 / (S(I+1) - S(I))
         DXP  = X(I+1) - X(I)
C
C------ Interior points
        ELSE
         DSMI = 1.0 / (S(I) - S(I-1))
         DXM  = X(I) - X(I-1)
         DSPI = 1.0 / (S(I+1) - S(I))
         DXP  = X(I+1) - X(I)
        ENDIF
C
        B(I)  = DSMI
        A(I)  = 2.0 * (DSMI + DSPI)
        C(I)  = DSPI
        XP(I) = 3.0 * (DXP*DSPI**2 + DXM*DSMI**2)
C
 1    CONTINUE
C
      CALL PTRISO(A,B,C,XP,II-1)
C
      XP(II) = XP(1)
C
      RETURN
      END 


      SUBROUTINE PTRISO(A,B,C,D,KK)
C
      DIMENSION A(KK),B(KK),C(KK),D(KK)
C
      DO 1 K = 2, KK
        KM = K-1
        AINV = 1.0/A(KM)
        C(KM) = C(KM)*AINV
        D(KM) = D(KM)*AINV
        B(KM) = B(KM)*AINV
        A(K) = A(K) - B(K)*C(KM)
        D(K) = D(K) - B(K)*D(KM)
        IF(K.LT.KK) THEN
         B(K) =      - B(K)*B(KM)
        ELSE
         A(K) = A(K) - B(K)*B(KM)
        ENDIF
  1   CONTINUE
C
      C(KK) = C(KK) / A(KK)
      D(KK) = D(KK) / A(KK)
C
      DO 2 K = KK, 2, -1
        KM = K-1
        D(KM) = D(KM) - C(KM)*D(K) - B(KM)*D(KK)
        C(KM) =       - C(KM)*C(K) - B(KM)*C(KK)
  2   CONTINUE
C
      D(1) = D(1) / ( 1.0 + C(1) )
C
      DO 3 K= 2, KK
        D(K) = D(K) - C(K)*D(1)
  3   CONTINUE
C
      RETURN
      END



      SUBROUTINE DBFOOT(NBLDS,II,XI,DXI,AOC,CH,GAM,
     &                  ADV,RAD,VEL,VSO,RHO,
     &                  GALT, DCLIMB, UNITL, NT,
     &                  NXDIM,NYDIM,NX,NY,X,Y,D)
C--------------------------------------------------------
C     Calculates dB noise levels on a ground plane grid.
C
C     Input:
C        NBLDS   number of blades
C        II      number of radial stations
C        XI(.)   r/R radial coordinate array
C        CH(.)   c/R chord distribution
C        AOC(.)  airfoil area/chord**2 distribution
C        GAM(.)  Gamma/VR circulation distribution
C        ADV     advance ratio  V/wR
C        RAD     tip radius R
C        VEL     freestream speed V
C        VSO     freestream speed of sound
C        RHO     freestream density
C        GALT    aircraft altitude above ground plane
C        DCLIMB  aircraft climb angle (deg)
C        UNITL   length unit of GALT,X,Y, in meters
C        NT      number of circumferential prop positions to be sampled
C
C        NXDIM,NYDIM   grid array dimensions
C        NX,NY         grid size
C        X(..)         ground-plane grid where dB is to be calculated
C        Y(..)          "
C
C     Output:
C        D(..)         dB sound level at ground plane
C
C--------------------------------------------------------
      DIMENSION AOC(II), XI(II), DXI(II), CH(II), GAM(II)
C
      DIMENSION D(NXDIM,NYDIM)
      DIMENSION X(NXDIM,NYDIM)
      DIMENSION Y(NXDIM,NYDIM)
C
C---- local arrays for calculating p(t) trace
      PARAMETER (NTX=180)
      DIMENSION PCOMP(0:NTX,3), PRES(0:NTX), TIME(0:NTX)
C
C---- degrees-to-radians conversion factor
      DTR = ATAN(1.0)/45.0
C
      COSC = COS(DCLIMB*DTR)
      SINC = SIN(DCLIMB*DTR)
C
C---- find j index of y=0 line
      DELY = ABS(Y(1,NY) - Y(1,1))
      DO J0 = 1, NY
        IF(Y(1,J0) .GT. -0.0001*DELY) GO TO 5
      ENDDO
      J0 = 1
 5    CONTINUE
C
      DO 10 I = 1, NX
        WRITE(*,1300) I, NX
 1300   FORMAT(5X,I3,' /',I3)
C
        DO 105 J = J0, NY
C-------- set observer position assuming airplane is level
          XG = X(I,J) / UNITL
          YG = Y(I,J) / UNITL
          ZG = -GALT      / UNITL
C
C-------- rotate point through climb angle about y axis
          XOBS =  COSC*XG      + SINC*ZG
          YOBS =            YG
          ZOBS = -SINC*XG      + COSC*ZG
C
C-------- calculate p(t) pressure signature
          CALL PTRACE(XOBS,YOBS,ZOBS,
     &                NBLDS,II,XI,DXI,AOC,CH,GAM,
     &                ADV,RAD,VEL,VSO,RHO,
     &                NTX,NT, PCOMP, TIME)
          DO IT=0, NT
            PRES(IT) = PCOMP(IT,1) + PCOMP(IT,2) + PCOMP(IT,3)
          ENDDO
C
C-------- set rms pressure signature and corresponding dB level
          PRMS = 0.
          DO IT=1, NT
            DELT =  TIME(IT) - TIME(IT-1)
            PAVG = (PRES(IT) + PRES(IT-1))*0.5
            PRMS = PRMS + PAVG**2 * DELT
          ENDDO
          PRMS = SQRT(PRMS/(TIME(NT)-TIME(0)))
C
          D(I,J) = 20.0 * ALOG10( PRMS / 20.0E-6 )
 105    CONTINUE
C
C------ set values for negative y by symmetry
        DO J=1, J0-1
          JPOS = 2*J0 - J - 1 + MOD(NY,2)
          D(I,J) = D(I,JPOS)
        ENDDO
C
 10   CONTINUE
C
      RETURN
      END
