C***********************************************************************
C    Module:  xrotor.f
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
C
      PROGRAM XROTOR
C
C--- module statement for Windoze DVFortran
ccc   USE DFLIB
C
      INCLUDE 'XROTOR.INC'
      CHARACTER*7 COMAND
      CHARACTER*128 COMARG
C
      DIMENSION IINPUT(20)
      DIMENSION RINPUT(20)
      LOGICAL ERROR
C
C====================================================
C
C      Interactive Design and Analysis Program 
C          for Free-tip and Ducted Rotors
C
C      October 1992
C      Copyright Mark Drela
C      Versions 6.7-7.x 
C      Copyright Mark Drela, Harold Youngren
C
C====================================================
C
      VERSION = 7.55
C
C---- logical unit numbers
      LUREAD = 5    ! terminal read
      LUWRIT = 6    ! terminal write
      LUTEMP = 3    ! general-use disk I/O unit  (usually available)
      LUSAVE = 4    ! save file                  (usually open)
C
C
      WRITE(*,1000) VERSION
C
      CALL INIT
C
      FNAME = ' '
C--- Get command line args (if present)
      NARG = IARGC()
C
      IF(NARG.GT.0) CALL GETARG(1,FNAME)
      IF(FNAME(1:1) .NE. ' ') CALL LOAD(FNAME)
C
      FNAME = ' '
      IF(NARG.GT.1) CALL GETARG(2,FNAME)
      IF(FNAME(1:1) .NE. ' ') THEN
        NCASE = 0
        OPEN(LUTEMP,FILE=FNAME,STATUS='OLD',ERR=2)
        CALL GETCAS(LUTEMP,NPARX,NCASE,CASPAR)
        CLOSE(LUTEMP)
        IF(NCASE.GT.0) THEN
          KF = INDEX(FNAME,' ') - 1
          WRITE(*,*) 'Operating cases read from file  ',
     &                FNAME(1:KF),' ...'
          CALL SHOCAS(LUWRIT,NPARX,NCASE,CASPAR,RAD,NAME)
        ENDIF
 2      CONTINUE
      ENDIF
C
      WRITE(*,1100)
C
 900  CONTINUE
      CALL ASKC(' XROTOR^',COMAND,COMARG)
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
      GREEK = .TRUE.
      IF(COMAND.EQ.'    ') GO TO 900
      IF(COMAND.EQ.'?   ') WRITE(*,1100)
      IF(COMAND.EQ.'?   ') GO TO 900
      IF(COMAND.EQ.'QUIT') THEN
        STOP
      ENDIF
C
      IF(COMAND.EQ.'OPER') CALL OPER
      IF(COMAND.EQ.'BEND') CALL BEND
      IF(COMAND.EQ.'LOAD') CALL LOAD(COMARG)
      IF(COMAND.EQ.'NOIS') CALL NOISE
      IF(COMAND.EQ.'DISP') GO TO 100
      IF(GREEK) WRITE(*,1050) COMAND
      GO TO 900
C
C---------------------------------------------------------------------
  100 CALL OUTPUT(LUWRIT)
      GO TO 900
C
C.....................................................................
C
 1000 FORMAT(/' ========================='
     &       /'    XROTOR Version', F5.2             
     &       /' =========================')
 1050 FORMAT(1X,A4,' command not recognized.  Type a "?" for list')
 1100 FORMAT(
     &  /'   QUIT   Exit program'
     &  /'  .OPER   Calculate off-design operating points'
     &  /'  .BEND   Calculate structural loads and deflections'
     &  /'  .NOIS   Calculate and plot acoustic signature'
     &  /'   LOAD f Read rotor from restart file'
     &  /'   DISP   Display current design point')
      END ! XROTOR


      SUBROUTINE INIT
      INCLUDE 'XROTOR.INC'
C--------------------------------------
C     Initializes everything
C--------------------------------------
C
      GREEK = .FALSE.
C
C
C---- XROTOR defaults
      URDUCT = 1.0
C
      CALL SETDEF
      CALL GETDEF
C
      IF(DUCT) THEN
        WRITE(*,*) 'Aprop/Aexit initialized to 1.0'
        URDUCT = 1.0
      ENDIF
C
      XINF = 3.0        ! r/R at which BC at infinity is applied
      NN = 32           ! number of perturbation potential harmonics
      IINF = II + II/2  ! number of discrete potential harmonic stations
      CONV =  .FALSE.   ! operating point solution existence flag
      LSTRUC = .FALSE.  ! indicates if structural properties are available
C
      NAME = ' '
      SAVFIL = ' '
C
C---- acceleration due to gravity for scaling centrifugal blade tension (m/s^2)
      GEE = 9.81
C
C---- ADW factor (multiplies TINV/PINV in ADW calculation)
      ADWFCTR = 1.0
C
      IF(II  .GT.IX) STOP 'Array overflow.  IX too small'
      IF(IINF.GT.JX) STOP 'Array overflow.  JX too small'
C
C---- actual-rotor radius is always 1 (non-dimensionalized with itself)
      XITIP = 1.0
C
C---- default nacelle, wake perturbation velocities (non-existent)
      DO I=1, II
        UBODY(I) = 0.
      END DO
C
C---- no slipstream velocity profiles
      NADD = 0
C
C---- number of defined cases
      NCASE = 0
      KCASE = 0
C
C---- max number of iterations for design, analysis
      NITERD = 40
      NITERA = 40
C
C---- do not initialize rotor at each design cycle
      LDESINI = .FALSE.
C
C---- do initialize rotor at each design cycle
      LOPRINI = .TRUE.
C
C---- no engine load line to start
      LPWRVAR = .FALSE.
      NPWRVAR = 0
C
C---- no rotor yet
      LROTOR = .FALSE.
      DO I=1, IX
        IAERO(I) = 0
      END DO
C
C---- no x-y plot active yat
      XYOFF(1) = 0.
      XYOFF(2) = 0.
      XYFAC(1) = 0.
      XYFAC(2) = 0.
C
      RETURN
      END ! INIT



      SUBROUTINE SETDEF
      INCLUDE 'XROTOR.INC'
C
C---- hard-wired start-up defaults
cccIHI
      RAKE = 0.0
C
      VEL = 1.0
      ALT = 0.0
      CALL ATMO(ALT,VSO,RHO,RMU) ! sea level atmosphere parameters
CCC      RHO =  1.226      ! fluid density         kg/m**3
CCC      RMU =  1.78E-05   ! dynamic viscosity     kg/m-s
CCC      VSO =  340.0      ! speed of sound        m/s
C
C--- Default aero properties for section #1
      A0 = 0.           ! zero lift angle of attack   radians
      DCLDA =  6.28     ! lift curve slope            /radian
      CLMAX =   1.5     ! stall Cl
      CLMIN =  -0.5     ! negative stall Cl
      DCL_STALL =   0.1 ! CL increment from incipient to total stall
      DCLDA_STALL = 0.1 ! stalled lift curve slope    /radian
      CMCON = -0.1      ! section Cm  (for pitch-axis moments)
      CDMIN =  0.013    ! minimum Cd
      CLDMIN =  0.5     ! Cl at minimum Cd
      DCDCL2 =  0.004   ! d(Cd)/d(Cl**2)
      REREF =  200000.  ! Reynolds Number at which Cd values apply
      REXP =  -0.4      ! Exponent for Re scaling of Cd:  Cd ~ Re**exponent
      MCRIT =  0.8      ! Critical Mach number
C--- Install data into aero section #1
      NAERO = 1
      XISECT = 0.0
      CALL PUTAERO(NAERO,XISECT,A0,CLMAX,CLMIN,
     &             DCLDA,DCLDA_STALL,DCL_STALL,
     &             CDMIN,CLDMIN,DCDCL2,CMCON,MCRIT,REREF,REXP)
      DO I=1, IX
        IAERO(I) = 1
      END DO
C
      XPITCH = 0.3     ! x/c location of pitch axis
C
      II =  30         ! number of radial stations
      INCR   = 2       ! radial station increment for terminal output
      IXSPAC = 2       ! r/R spacing flag
C
      VRTX =  .FALSE.  ! Vortex Wake (T)        / Graded Momentum(F) flag
      FAST =  .FALSE.  ! Graded Momentum(T)     / Potential Formulation(F) flag
      FREE =  .TRUE.   ! Self-deforming wake(T) / rigid wake(F) flag
      DUCT =  .FALSE.  ! Ducted (T)             / Free-tip (F)  flag
C
      TERSE = .FALSE.  ! terse-output flag
      LLAND = .TRUE.   ! landscape-mode plot flag
      LGRID = .TRUE.   ! grid plotting flag
C
      LVNORM = .TRUE.  ! flight speed used for normalization
C
      PAR = 0.6        ! plot aspect ratio
      CSIZE = 0.014    ! character size / plot-width
C
      RETURN
      END ! SETDEF


      SUBROUTINE GETDEF
      INCLUDE 'XROTOR.INC'
C
C---- try to read start-up defaults from xrotor.def file if possible
      LU = LUTEMP
      OPEN(LU,FILE='xrotor.def',STATUS='OLD',ERR=50)
C
C--- This data in the defaults file is no longer consistent with XROTOR 7+
C--- For now just use the non-aero data
      READ(LU,*,ERR=10) RHO, VSO, RMU
      READ(LU,*,ERR=10) DCLDA, A0
      READ(LU,*,ERR=10) CDMIN, DCDCL2, CLDMIN
      READ(LU,*,ERR=10) REREF, REXP
      READ(LU,*,ERR=10) CLMAX, CLMIN, DCL_STALL
      READ(LU,*,ERR=10) CMCON
      READ(LU,*,ERR=10) XPITCH
      READ(LU,*,ERR=10) II, INCR, IXSPAC
      READ(LU,*,ERR=10) FAST, FREE, DUCT
      READ(LU,*,ERR=10) TERSE, LLAND, LGRID
      READ(LU,*,ERR=10) LVNORM
      READ(LU,*,ERR=10) PAR, CSIZE
      CLOSE(LU)
C
      WRITE(*,*) ' '
      WRITE(*,*) 'Defaults read from file  xrotor.def'
      RETURN
C
 10   WRITE(*,*) ' '
      WRITE(*,*) 'READ error on file  xrotor.def'
      WRITE(*,*) 'Hard-wired defaults used'
      CLOSE(LU)
      CALL SETDEF
C
 50   WRITE(*,*) ' '
      WRITE(*,*) 'OPEN error on file  xrotor.def'
      WRITE(*,*) 'Hard-wired defaults used'
      CALL SETDEF
C
      RETURN
      END ! GETDEF



      SUBROUTINE ATMO(ALSPEC,VSOALT,RHOALT,RMUALT)
C---------------------------------------------------------
C     Returns speed of sound (VSO) in m/s, density (RHO)
C     in kg/m^3, and dynamic viscosity (RMU) in kg/m-s
C     of standard atmosphere at specified altitude ALSPEC
C     (in kilometers).  If ALSPEC=-1, water properties
C     at 15 Celsius are returned.
C
C     Reference:  "U.S. Standard Atmosphere", NOAA.
C---------------------------------------------------------
      LOGICAL FIRST
C
      PARAMETER ( N = 44 )
      REAL ALT(N), VSO(N), RHO(N), RMU(N)
C
      DATA FIRST / .TRUE. /
      DATA ALT
     &   / 0.0,  1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0,
     &    10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0,
     &    20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0,
     &    30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0, 
     &    40.0, 45.0, 60.0, 75.0 /
      DATA VSO
     & / 340.0,336.0,332.0,329.0,325.0,320.0,316.0,312.0,308.0,304.0,
     &   299.0,295.0,295.0,295.0,295.0,295.0,295.0,295.0,295.0,295.0,
     &   295.0,295.8,296.4,297.1,297.8,298.5,299.1,299.8,300.5,301.1,
     &   301.8,302.5,303.1,305.0,306.8,308.7,310.5,312.3,314.0,316.0,
     &   318.0,355.0,372.0,325.0 /
      DATA RHO
     & / 1.226,1.112,1.007,0.909,0.820,0.737,0.660,0.589,0.526,0.467,
     &   0.413,0.364,0.311,0.265,0.227,0.194,0.163,0.141,0.121,0.103,
     &   .0880,.0749,.0637,.0543,.0463,.0395,.0338,.0288,.0246,.0210,
     &   .0180,.0154,.0132,.0113,.0096,.0082,.0070,.0060,.0052,.0044,
     &   0.004,0.002,3.9E-4,8.0E-5 /
      DATA RMU
     & / 1.780,1.749,1.717,1.684,1.652,1.619,1.586,1.552,1.517,1.482,
     &   1.447,1.418,1.418,1.418,1.418,1.418,1.418,1.418,1.418,1.418,
     &   1.418,1.427,1.433,1.438,1.444,1.449,1.454,1.460,1.465,1.471,
     &   1.476,1.481,1.487,1.502,1.512,1.532,1.546,1.561,1.580,1.600,
     &   1.700,1.912,2.047,1.667 /
C
C---- special case: Water at STP
      IF(ALSPEC.EQ.-1.0) THEN
       VSOALT = 1500.
       RHOALT = 1000.
       RMUALT = 1.15E-3
       WRITE(*,*) '                              o        '
       WRITE(*,*) 'ATMO: You are underwater at 15  Celsius'
       RETURN
      ENDIF
C
C---- linearly interpolate quantities from tabulated values
      DO 10 I=2, N
        IF(ALSPEC.GT.ALT(I)) GO TO 10
C
         DALT = ALT(I) - ALT(I-1)
         DVSO = VSO(I) - VSO(I-1)
         DRHO = RHO(I) - RHO(I-1)
         DRMU = RMU(I) - RMU(I-1)
C
         ALFRAC = (ALSPEC - ALT(I-1)) / DALT
C
         VSOALT = VSO(I-1) + DVSO*ALFRAC
         RHOALT = RHO(I-1) + DRHO*ALFRAC
         RMUALT = RMU(I-1) + DRMU*ALFRAC
         RMUALT = RMUALT * 1.0E-5
C
         RETURN
   10 CONTINUE
C
C
      IF(ALSPEC.GT.ALT(N)) THEN
       WRITE(*,*) ' '
       WRITE(*,*) 'ATMO: You''re in low earth orbit.  Good luck.'
       VSOALT = VSO(N)
       RHOALT = RHO(N)
       RMUALT = RMU(N) * 1.0E-5
       RETURN
      ENDIF
C
c      IF(FIRST) THEN
c       DO 20 I=1, N
c         RHO(I) = ALOG(RHO(I))
c 20    CONTINUE
c       CALL SPLINE(VSO,VSOH,ALT,N)
c       CALL SPLIND(RHO,RHOH,ALT,N,999.0,0.0)
c       CALL SPLINE(RMU,RMUH,ALT,N)
c       FIRST = .FALSE.
c      ENDIF
cC
cC---- interpolate quantities from splines
c      VSOALT = SEVAL(ALSPEC,VSO,VSOH,ALT,N)
c      RHOALT = SEVAL(ALSPEC,RHO,RHOH,ALT,N)
c      RMUALT = SEVAL(ALSPEC,RMU,RMUH,ALT,N) * 1.0E-5
c      RHOALT = EXP(RHOALT)
cC
      RETURN
      END ! ATMO


      SUBROUTINE FLOSHO(LU, VSO, RHO, RMU)
      DATA R, GAM / 287.0, 1.4 /
      RNU = RMU/RHO
      P = RHO*VSO**2 / GAM
      T = P / (RHO*R)
      WRITE(LU,5000) VSO, RHO, RMU, RNU, P, T
 5000 FORMAT(/' Speed of sound (m/s):',F10.3
     &       /' Density   (kg/m^3)  :',F10.5
     &       /' Viscosity (kg/m-s)  :',E11.4
     &       /' Kin. Visc. (m^2/s)  :',E11.4
     &      //' Air pressure (Pa)   :',G13.5
     &       /' Air temperature (K) :',G12.4)
      RETURN
      END ! FLOSHO


      SUBROUTINE REINIT
      INCLUDE 'XROTOR.INC'
      LOGICAL YES
C-----------------------------------------------
C     Re-initializes advance ratio and gammas
C-----------------------------------------------
C
C---- estimate reasonable advance ratio to start iterative routines
      IS = II/2 + 1
C---HHY had to set A0 to 0.0 as A0 is now section property
      A0  = 0.0 
      ANG = BETA(IS) - A0
C
      RPM  = VEL/(RAD*ADV*PI/30.)
C
      ADV0 = XI(IS)*SIN(ANG)/COS(ANG)
      RPM0 = VEL/(RAD*ADV0*PI/30.)

c      WRITE(*,*) 'Current    RPM ',RPM
c      WRITE(*,*) 'Initialize RPM ',RPM0
      CALL ASKR('Enter initialization RPM?^',RPM)

      ADV = VEL/(RPM*RAD*PI/30.)
      ADV = MAX(0.1,ADV)
      ADW = ADV
C
C---- Set the blade angle back to reference angle
      CALL ASKL('Restore blade angles to original?^',YES)
      IF(YES) THEN
        DO I = 1, II
          BETA0(I) = BETA(I)
        END DO
      ENDIF
C---- calculate current operating point
      CALL APER(4,2,.TRUE.)
      IF(CONV) CALL OUTPUT(LUWRIT)
C
      RETURN
      END ! REINIT

      SUBROUTINE SETX
      INCLUDE 'XROTOR.INC'
C
C-------------------------------------------------------
C     Fills stretched radial coordinate array X (and XV)
C-------------------------------------------------------
C
      DT = 0.5*PI/FLOAT(II)
      XM = XI0
      XV(1) = XI0
      DO 10 I=1, II
        T(I) = DT*(FLOAT(I)-0.5)
        TP   = DT* FLOAT(I)
C
        IF(IXSPAC.EQ.2) THEN
C------- Usual sine stretching, adjusted for nonzero root radius
         XI(I) = SQRT(XITIP*SIN(T(I))**2 + (XI0*COS(T(I)))**2)
         XP    = SQRT(XITIP*SIN(TP  )**2 + (XI0*COS(TP  ))**2)
        ELSE
C------- Cosine stretching for more root resolution (also in TINVRT)
         XI(I) = 0.5*(1.0-COS(2.0*T(I)))*(XITIP-XI0) + XI0
         XP    = 0.5*(1.0-COS(2.0*TP  ))*(XITIP-XI0) + XI0
        ENDIF
C
        XI(I)  = (XP + XM)*0.5
        DXI(I) =  XP - XM
C
        XM = XP
        XV(I+1) = XP
   10 CONTINUE
      XV(II+1) = XITIP
C
      RETURN
      END ! SETX



      SUBROUTINE OPFILE(LU,FNAME)
      CHARACTER*(*) FNAME
C
      CHARACTER*4 COMAND
      CHARACTER*128 COMARG,TMP
      CHARACTER*1 ANS, DUMMY
C
C---- get filename if it hasn't been already specified
      IF(FNAME(1:1).EQ.' ') CALL ASKS('Enter output filename^',FNAME)
C
C---- try to open file
      OPEN(LU,FILE=FNAME,STATUS='OLD',ERR=50)
C
C---- file exists... ask how to proceed
      NF = INDEX(FNAME,' ') - 1
      TMP = 'File  '// FNAME(1:NF)//
     &      '  exists.  Overwrite / Append / New file ?^'
      CALL ASKC(TMP,COMAND,COMARG)
      ANS = COMAND(1:1)
C
C---- ask again if reply is invalid
      IF(INDEX('OoAaNn',ANS).EQ.0) THEN
        CALL ASKC(' O / A / N  ?^',COMAND,COMARG)
        ANS = COMAND(1:1)
C
        IF(INDEX('OoAaNn',ANS).EQ.0) THEN
C------- Still bad reply. Give up asking and just return
         WRITE(*,*) 'No action taken'
         RETURN
        ENDIF
      ENDIF
C
C---- at this point, file is open and reply is valid
      IF    (INDEX('Oo',ANS) .NE. 0) THEN
C------ go to beginning of file to overwrite
        REWIND(LU)
        GO TO 60
      ELSEIF(INDEX('Aa',ANS) .NE. 0) THEN
C------ go to end of file to append
        DO K=1, 12345678
          READ(LU,1000,END=60) DUMMY
 1000     FORMAT(A)
        ENDDO
      ELSE
C------ new file... get filename from command argument, or ask if not supplied
        FNAME = COMARG
        IF(FNAME(1:1).EQ.' ') CALL ASKS('Enter output filename^',FNAME)
      ENDIF
C
C---- at this point, file FNAME is new or is to be overwritten
 50   OPEN(LU,FILE=FNAME,STATUS='UNKNOWN',ERR=90)
      REWIND(LU)
C
 60   RETURN
C
 90   WRITE(*,*) 'Bad filename.'
      RETURN
      END ! OPFILE
       

      SUBROUTINE OUTPUT(LU)
      INCLUDE 'XROTOR.INC'
      LOGICAL LHELI
      CHARACTER*1 SCHAR
C---------------------------------------------
C     Dumps operating state output to unit LU
C---------------------------------------------
C
      IADD = 1
      IF(LU.EQ.LUWRIT) IADD = INCR
C
      WRITE (LU,1000)
      IF(.NOT.CONV) WRITE(LU,2000)
C
      LHELI = .FALSE.
C
C---- dimensional thrust, power, torque, rpm
      TDIM = TTOT*RHO*VEL**2*RAD**2
      QDIM = QTOT*RHO*VEL**2*RAD**3
      PDIM = PTOT*RHO*VEL**3*RAD**2
C
      TVDIM = TVIS*RHO*VEL**2*RAD**2
      PVDIM = PVIS*RHO*VEL**3*RAD**2
C
      EFFTOT = TTOT/PTOT
      RPM = VEL/(RAD*ADV*PI/30.)
      DIA = 2.0*RAD
C
C---- Nacelle (or body) thrust is difference between thrust on 
C     equivalent prop and real prop
      TNACEL = (TWAK-TINV)*RHO*VEL**2*RAD**2
C
C---- blade solidity
      CALL SPLINE(CH,W1,XI,II)
      CH34 = SEVAL(0.75,CH,W1,XI,II)
      SIGMA = FLOAT(NBLDS)*CH34/PI
C
C---- standard coefficients based on forward speed
      TC = TDIM/(0.5*RHO*VEL**2 * PI*RAD**2)
      PC = PDIM/(0.5*RHO*VEL**3 * PI*RAD**2)
C
C---- standard coefficients based on rotational speed
      EN = RPM/60.0
      CT = TDIM/(RHO*EN**2*DIA**4)
      CP = PDIM/(RHO*EN**3*DIA**5)
C
C---- induced efficiency (including nacelle thrust effect)
      EFFIND = TWAK/PWAK
C
C---- ideal (actuator disk) efficiency
      TCLIM = MAX( -1.0 , TC )
      EIDEAL = 2.0 / (1.0 + SQRT(TCLIM + 1.0))
C
C---- define low advance ratio (helicopter?) related data
      IF(ADV.LT.0.1) THEN
       CALL SPLINE(CH,W1,XI,II)
       CTH  = CT/7.7516
       CPH  = CP/24.352
       CTOS = CTH / SIGMA
       FOM = 0.7979 * ABS(CT)**1.5 / CP
       LHELI = .TRUE.
      ENDIF
C
C
      IF(DUCT) THEN
       IF(IWTYP.EQ.1) WRITE(LU,1001) NAME
       IF(IWTYP.EQ.2) WRITE(LU,1002) NAME
       IF(IWTYP.EQ.3) WRITE(LU,1001) NAME
      ELSE
       IF(IWTYP.EQ.1) WRITE(LU,1011) NAME
       IF(IWTYP.EQ.2) WRITE(LU,1012) NAME
       IF(IWTYP.EQ.3) WRITE(LU,1013) NAME
      ENDIF
      IF(NADD.GT.1) THEN
       WRITE(LU,1021) ADW
      ELSE IF(DUCT) THEN
       WRITE(LU,1022) URDUCT, ADW
      ELSE
       WRITE(LU,1023) ADW
      ENDIF
      IF(ADW.LT.0.5*ADV) WRITE(LU,1024)
      WRITE(LU,1010) NBLDS,RAD,ADV,
     &               TDIM,PDIM,QDIM,
     &               EFFTOT,VEL,RPM,
     &               EFFIND,EIDEAL,TC,
     &               TNACEL,XI0*RAD,XW0*RAD,
     &               TVDIM,PVDIM,
     &               RHO,VSO,RMU
C
C---- low advance ratio (helicopter?) data
      IF(LHELI) THEN
       WRITE(LU,1116) SIGMA,CTOS,FOM
      ELSE
       WRITE(LU,1117) SIGMA
      ENDIF
C
C---- coefficients based on rotational speed
      WRITE(LU,1015) CT, CP, ADV*PI
C---- coefficients based on forward speed
      WRITE(LU,1016) TC, PC, ADV

cc      write(LU,1017) PVIS * ADV**3 * 2.0/PI,
cc     &               PWAK * ADV**3 * 2.0/PI 

C
      IF(TERSE) RETURN
C
C----- find maximum RE on blade
      REMAX = 0.0
      DO I=1, II
        REMAX = MAX(RE(I),REMAX)
      END DO
      REEXP = 1.0
      IF(REMAX.GE.1.0E6) THEN
        REEXP = 6.0
      ELSEIF(REMAX.GE.1.0E3) THEN
        REEXP = 3.0
      ENDIF
C
      IF(REEXP.EQ.1.0) THEN
        WRITE(LU,1020) 
       ELSE
        WRITE(LU,1120) IFIX(REEXP)
      ENDIF
C
      DO 10 I=1, II, IADD
C
C------ use equivalent prop to define local efficiency
        CALL UVADD(XI(I),WA,WT)
        VW  = VWAK(I)
        VAW = VW*XW(I)/ADW
C------ Freestream velocity component on equiv prop
        UTOTW = URDUCT
        CW = XI(I)/ADV - WT  -  VW
        SW = UTOTW     + WA  +  VAW
        EFFI = (CW/SW) * ADV/XW(I)
C
C------ use real prop to define Mach number
        CALL CSCALC(I,UTOT,WA,WT,
     &              VT,VT_ADW,
     &              VA,VA_ADW,
     &              VD,VD_ADW,
     &              CI,CI_ADV,CI_VT,
     &              SI,             SI_VA,
     &              W,  W_ADV, W_VT, W_VA,
     &              PHI,P_ADV, P_VT, P_VA)
C
        MACH = W * VEL/VSO
C
        BDEG = BETA(I)*180./PI
        XRE = RE(I)/(10.0**REEXP)
C
        SCHAR = ' '
        IF(STALL(I)) SCHAR = 's'
C
        WRITE(LU,1030)
     &    I,XI(I),CH(I),BDEG,CL(I),SCHAR,CD(I),XRE,MACH,
     &    EFFI,EFFP(I),UBODY(I)
cc     &    ,rad*ch(i)*sin(beta(i))*39.36
   10 CONTINUE
cc      WRITE(LU,1000)
cc      WRITE(LU,*   ) ' '
C
      RETURN
C....................................................................
C
 1000 FORMAT(/1X,75('='))
 1001 FORMAT(' Ducted Graded Mom. Formulation Solution:  ', A32)
 1002 FORMAT(' Ducted Potential Formulation Solution:  ', A32)
 1011 FORMAT(' Free Tip Graded Mom. Formulation Solution:  ', A32)
 1012 FORMAT(' Free Tip Potential Formulation Solution:  ', A32)
 1013 FORMAT(' Free Tip Vortex Wake Formulation Solution:  ', A32)
 1021 FORMAT(' (External slipstream present)',19X,
     &           'Wake adv. ratio:',F11.5)
 1022 FORMAT(' Vdisk/Vslip:',F11.5,25X,
     &           'Wake adv. ratio:',F11.5)
 1023 FORMAT(50X,'Wake adv. ratio:',F11.5)
 1024 FORMAT(' Reverse far-slipstream velocity implied.',
     &   ' Interpret results carefully !')
 1010 FORMAT(' no. blades :',I3,  12X,'radius(m)  :',F9.4, 5X,
     &         'adv. ratio: ',F11.5,
     &      /' thrust(N)  :',G11.3,4X,'power(W)   :',G11.3,3X,
     &         'torque(N-m):',G11.3,
     &      /' Efficiency :',F8.4, 7X,'speed(m/s) :',F9.3, 5X,
     &         'rpm        :',F11.3,
     &      /' Eff induced:',F8.4, 7X,'Eff ideal  :',F9.4, 5X,
     &         'Tcoef      :',F11.4,
     &      /' Tnacel(N)  :',F11.4,4X,'hub rad.(m):',F9.4, 5X,
     &         'disp. rad. :',F10.4,
     &      /' Tvisc(N)   :',F11.4,4X,'Pvisc(W)   :',G11.3,
     &      /' rho(kg/m3) :',F10.5,5X,'Vsound(m/s):',F9.3, 5X,
     &         'mu(kg/m-s) :',E11.4
     &      /1X,75('-'))
 1015 FORMAT(12X,'    Ct:', F11.5, '     Cp:', F11.5, '    J:', F11.5)
 1016 FORMAT(12X,'    Tc:', F11.5, '     Pc:', F11.5, '  adv:', F11.5)
 1116 FORMAT('Helicopter: ',
     &       ' Sigma:', F11.5, '  CTh/s:', F11.5, '  FOM:', F11.5)
 1117 FORMAT(' Sigma:', F11.5)
 1017 FORMAT(' Cpv:', F11.5, '    Cpi:', F11.5 )
 1020 FORMAT(/'  i  r/R    c/R  beta(deg)',
     & '   CL      Cd    RE    Mach   effi  effp  na.u/U')
 1120 FORMAT(/'  i  r/R   c/R  beta(deg)',
     & '  CL     Cd    REx10^',I1,' Mach   effi  effp  na.u/U')
 1030 FORMAT(1X,I2,F6.3,F7.4,F7.2,F7.3,1X,A1,F7.4,1X,
     &       F6.2,1X,F6.3,1X,F6.3,F6.3,F8.3,f10.6)
 2000 FORMAT(/19X,'********** NOT CONVERGED **********'/)
      END ! OUTPUT


      SUBROUTINE UVADD(XIW,WA,WT)
      INCLUDE 'XROTOR.INC'
C
      WA = 0.0
      WT = 0.0
C
      IF(NADD.LE.1) RETURN
C
      RDIM = XIW*RAD
      IF(RDIM.GE.RADD(1) .AND. RDIM.LE.RADD(NADD)) THEN
       WA = SEVAL(RDIM,UADD,UADDR,RADD,NADD) / VEL
       WT = SEVAL(RDIM,VADD,VADDR,RADD,NADD) / VEL
      ENDIF
C
      RETURN
      END


















