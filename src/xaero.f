C***********************************************************************
C    Module:  xaero.f
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


C--- Aero data stored for one or more radial aerodynamic sections
C   
C-- aero data quantities for each defined radial aerodynamic section
C   NAERO       Number of aerodynamic datasets defined (NAERO>=1)
C   XIAERO      Radial station r/R where aero dataset is defined
C   AERODATA    Aerodynamic definition of the blade section at XIAERO
C               AERODATA( 1,x) = A0 (angle of zero lift)
C               AERODATA( 2,x) = CLMAX (Max CL)
C               AERODATA( 3,x) = CLMIN (Min CL)
C               AERODATA( 4,x) = DCLDA (Incompressible 2-D lift curve slope)
C               AERODATA( 5,x) = DCLDA_STALL (2-D lift curve slope at stall)
C               AERODATA( 6,x) = DCL_STALL (CL increment, onset to full stall)
C               AERODATA( 7,x) = CDMIN (Minimum drag coefficient value)
C               AERODATA( 8,x) = CLDMIN (Lift at minimum drag value)
C               AERODATA( 9,x) = DCDCL2 (Parabolic drag param d(Cd)/dCL^2)
C               AERODATA(10,x) = CMCON (Incompressible 2-D pitching moment)
C               AERODATA(11,x) = REREF (reference Reynold's number)
C               AERODATA(12,x) = REXP (Reynold's number exponent Cd~Re^REXP)
C               AERODATA(13,x) = MCRIT (critical Mach #)
C               AERODATA(14,x) = TOC (thickness/chord)


      SUBROUTINE SETIAERO
C--------------------------------------------------
C     Sets up indices referring to aero section for 
C     each radial station
C--------------------------------------------------
      INCLUDE 'XROTOR.INC'
C
C--- Find lower index of aero data sections XIAERO(N) bounding XI(IS)
      DO I=1, II
        IAERO(I) = 1
        DO N = 1, NAERO
         IF(XIAERO(N).LE.XI(I)) THEN
           IAERO(I) = N
         ENDIF
        END DO
      END DO
      RETURN
      END


      SUBROUTINE PUTAERO(N,XISECT,A0,CLMAX,CLMIN,
     &                   DCLDA,DCLDA_STALL,DCL_STALL,
     &                   CDMIN,CLDMIN,DCDCL2,CMCON,MCRIT,REREF,REXP)
C--------------------------------------------------------
C     Puts aero data into stored section array at index N
C--------------------------------------------------------
      INCLUDE 'XROTOR.INC'
C 
      IF(N.GT.NAX) THEN
        WRITE(*,*) 'Too many aero sections defined...'
        RETURN
      ENDIF
C
      AERODATA( 1,N) = A0
      AERODATA( 2,N) = CLMAX
      AERODATA( 3,N) = CLMIN
      AERODATA( 4,N) = DCLDA
      AERODATA( 5,N) = DCLDA_STALL
      AERODATA( 6,N) = DCL_STALL
      AERODATA( 7,N) = CDMIN  
      AERODATA( 8,N) = CLDMIN
      AERODATA( 9,N) = DCDCL2
      AERODATA(10,N) = CMCON 
      AERODATA(11,N) = REREF
      AERODATA(12,N) = REXP
      AERODATA(13,N) = MCRIT
      XIAERO(N)      = XISECT
C
      RETURN
      END


C*************************************************************************
C  Interpolated aero section properties functions
C  These routines implement a functional representation of the 
C  blade aero properties (CL,CD,CM) vs ALFA
C*************************************************************************


      SUBROUTINE GETCLCDCM(IS,ALF,W,REY,
     &                     CLIFT,CL_ALF,CL_W,
     &                     CLMAX,CLMIN,DCL_STALL,STALLF,
     &                     CDRAG,CD_ALF,CD_W,CD_REY,
     &                     CMOM,CM_AL,CM_W)
C-------------------------------------------------------------
C     CL(alpha),
C      CD(alpha), 
C       CM(alpha) interpolation function for blade at station IS
C-------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      LOGICAL STALLF,STALLF2
C
C--- Check for installed aero data section index
      N = IAERO(IS)
      IF(N.LT.1 .OR. N.GT.NAERO) THEN
C
       IF(NAERO.GT.1) THEN
C--- Find lower index of aero data sections XIAERO(N) bounding XI(IS)
        DO N = 1, NAERO
         IF(XIAERO(N).LE.XI(IS)) THEN
cc          write(*,*) 'getcl iaero= ',N,' is= ',is,xiaero(N),xi(is)
           IAERO(IS) = N
          ELSE
           GO TO 10
         ENDIF
        END DO
        WRITE(*,*) 'Aero section not found for station ',XI(IS)
       ENDIF
C
       N = 1
       IAERO(IS) = N
      ENDIF
C
C--- Get section aero data from stored section array
 10   A0          = AERODATA( 1,N)
      CLMAX       = AERODATA( 2,N)
      CLMIN       = AERODATA( 3,N)
      DCLDA       = AERODATA( 4,N)
      DCLDA_STALL = AERODATA( 5,N)
      DCL_STALL   = AERODATA( 6,N)
      CDMIN       = AERODATA( 7,N)
      CLDMIN      = AERODATA( 8,N)
      DCDCL2      = AERODATA( 9,N)
      CMCON       = AERODATA(10,N)
      REREF       = AERODATA(11,N)
      REXP        = AERODATA(12,N)
      MCRIT       = AERODATA(13,N)
      XISECT1     = XIAERO(N)
C--- Get data for inner bounding aero section
      CALL CLCDCM(ALF,W,REY,
     &            CLIFT,CL_ALF,CL_W,STALLF,
     &            CDRAG,CD_ALF,CD_W,CD_REY,
     &            CMOM,CM_AL,CM_W,
     &            A0,CLMAX,CLMIN,DCLDA,DCLDA_STALL,DCL_STALL,
     &            CDMIN,CLDMIN,DCDCL2,CMCON,MCRIT,REREF,REXP)
C
C--- Check for another bounding section, if not we are done, 
C    if we have another section linearly interpolate data to station IS
      IF(N.LT.NAERO) THEN
        XISECT2 = XIAERO(N+1)
        FRAC = (XI(IS)-XISECT1)/(XISECT2-XISECT1)
        IF(FRAC.LE.0.0 .OR. FRAC.GT.1.0) THEN
cc         write(*,*) 'CL n,is,xi,frac = ',n,is,xi(is),frac
        ENDIF
C
        A0          = AERODATA( 1,N+1)
        CLMAX2      = AERODATA( 2,N+1)
        CLMIN2      = AERODATA( 3,N+1)
        DCLDA       = AERODATA( 4,N+1)
        DCLDA_STALL = AERODATA( 5,N+1)
        DCL_STALL2  = AERODATA( 6,N+1)
        CDMIN       = AERODATA( 7,N+1)
        CLDMIN      = AERODATA( 8,N+1)
        DCDCL2      = AERODATA( 9,N+1)
        CMCON       = AERODATA(10,N+1)
        REREF       = AERODATA(11,N+1)
        REXP        = AERODATA(12,N+1)
        MCRIT       = AERODATA(13,N+1)
C--- Get data for outer bounding aero section
        CALL CLCDCM(ALF,W,REY,
     &              CLIFT2,CL_ALF2,CL_W2,STALLF2,
     &              CDRAG2,CD_ALF2,CD_W2,CD_REY2,
     &              CMOM2,CM_AL2,CM_W2,
     &              A0,CLMAX2,CLMIN2,DCLDA,DCLDA_STALL,DCL_STALL2,
     &              CDMIN,CLDMIN,DCDCL2,CMCON,MCRIT,REREF,REXP)
C--- Interpolate aero data to blade station
        STALLF = STALLF .OR. STALLF2
        CLIFT  = (1.0-FRAC)*CLIFT  + FRAC*CLIFT2
        CL_ALF = (1.0-FRAC)*CL_ALF + FRAC*CL_ALF2
        CL_W   = (1.0-FRAC)*CL_W   + FRAC*CL_W2
        CLMAX  = (1.0-FRAC)*CLMAX  + FRAC*CLMAX2
        CLMIN  = (1.0-FRAC)*CLMIN  + FRAC*CLMIN2
        DCL_STALL = (1.0-FRAC)*DCL_STALL + FRAC*DCL_STALL2
C
        CMOM   = (1.0-FRAC)*CMOM   + FRAC*CMOM2
        CM_AL  = (1.0-FRAC)*CM_AL  + FRAC*CM_AL2
        CM_W   = (1.0-FRAC)*CM_W   + FRAC*CM_W2
C
        CDRAG  = (1.0-FRAC)*CDRAG  + FRAC*CDRAG2
        CD_ALF = (1.0-FRAC)*CD_ALF + FRAC*CD_ALF2
        CD_W   = (1.0-FRAC)*CD_W   + FRAC*CD_W2
        CD_REY = (1.0-FRAC)*CD_REY + FRAC*CD_REY2
      ENDIF
C
      RETURN
      END



      SUBROUTINE GETALF(IS,CLIFT,W,ALF,ALF_CL,ALF_W,STALLF)
C------------------------------------------------------------
C     Inverse alpha(CL) function 
C     Uses Newton-Raphson iteration to get ALF from CL function
C------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      LOGICAL STALLF
      DATA NITER / 10 /
      DATA EPS   / 1.0E-5 /
C
      STALLF = .FALSE.
C
C---HHY had to set A0 to first aero section as A0 is now section property
      A0  = AERODATA(1,1)
      REY = 0.0
C
      ALF = A0 
      DO ITER=1, NITER
        CALL GETCLCDCM(IS,ALF,W,REY,
     &                 CLTEMP,CL_ALF,CL_W,
     &                 CLMAX,CLMIN,DCL_STALL,STALLF,
     &                 CDRAG,CD_ALF,CD_W,CD_REY,
     &                 CMOM,CM_AL,CM_W)
cc      IF(STALLF) GO TO 20
        DALF = -(CLTEMP-CLIFT)/CL_ALF
        ALF = ALF + DALF
        ALF_CL =   1.0/CL_ALF
        ALF_W  = -CL_W/CL_ALF
        IF(ABS(DALF).LT.EPS) RETURN
      END DO
C
   20 WRITE(*,*) 'GETALF: alpha(CL) function inversion failed'
c      write(*,*) 'is,clift  ',is,clift
c      write(*,*) 'abs(dalf) ',abs(dalf)
c      write(*,*) 'cl_alf    ',cl_alf
C
      RETURN
      END ! GETALF



C*************************************************************************
C  Basic aero section properties functions
C  These routines implement a functional representation of the 
C  blade section aero properties (CL,CD,CM) vs ALFA
C*************************************************************************

      SUBROUTINE CLCDCM(ALF,W,REY,
     &                  CLIFT,CL_ALF,CL_W,STALLF,
     &                  CDRAG,CD_ALF,CD_W,CD_REY,
     &                  CMOM,CM_AL,CM_W,
     &                  A0,CLMAX,CLMIN,DCLDA,DCLDA_STALL,DCL_STALL,
     &                  CDMIN,CLDMIN,DCDCL2,CMCON,MCRIT,REREF,REXP)
C------------------------------------------------------------
C     CL(alpha) function
C     Note that in addition to setting CLIFT and its derivatives
C     CLMAX and CLMIN (+ and - stall CL's) are set in this routine
C     In the compressible range the stall CL is reduced by a factor
C     proportional to Mcrit-Mach.  Stall limiting for compressible 
C     cases begins when the compressible drag added CDC > CDMstall
C------------------------------------------------------------
C     CD(alpha) function - presently CD is assumed to be a sum
C     of profile drag + stall drag + compressibility drag
C     In the linear lift range drag is CD0 + quadratic function of CL-CLDMIN
C     In + or - stall an additional drag is added that is proportional
C     to the extent of lift reduction from the linear lift value.
C     Compressible drag is based on adding drag proportional to 
C     (Mach-Mcrit_eff)^MEXP
C------------------------------------------------------------
C     CM(alpha) function - presently CM is assumed constant,
C     varying only with Mach by Prandtl-Glauert scaling
C------------------------------------------------------------
C
      INCLUDE 'XROTOR.INC'
      LOGICAL STALLF
      DOUBLE PRECISION ECMIN, ECMAX
C
C---- Factors for compressibility drag model, HHY 10/23/00
C     Mcrit is set by user
C     Effective Mcrit is Mcrit_eff = Mcrit - CLMFACTOR*(CL-CLDmin) - DMDD
C     DMDD is the delta Mach to get CD=CDMDD (usually 0.0020)
C     Compressible drag is CDC = CDMFACTOR*(Mach-Mcrit_eff)^MEXP
C     CDMstall is the drag at which compressible stall begins
C
      CDMFACTOR = 10.0
      CLMFACTOR =  0.25
      MEXP      =  3.0
      CDMDD     =  0.0020
      CDMSTALL  =  0.1000
C
C---- Prandtl-Glauert compressibility factor
      MSQ   =   W*W*VEL**2/VSO**2
      MSQ_W = 2.0*W*VEL**2/VSO**2
      IF(MSQ.GE.1.0) THEN
       WRITE(*,*)
     &  'CLFUNC: Local Mach number limited to 0.99, was ', MSQ
       MSQ = 0.99
       MSQ_W = 0.
      ENDIF
      PG = 1.0 / SQRT(1.0 - MSQ)
      PG_W = 0.5*MSQ_W * PG**3
C
C---- Mach number and dependence on velocity
      MACH = SQRT(MSQ)
      MACH_W = 0.0
      IF(MACH.NE.0.0) MACH_W = 0.5*MSQ_W/MACH 
C
C
C------------------------------------------------------------
C--- Generate CL from dCL/dAlpha and Prandtl-Glauert scaling
      CLA     = DCLDA*PG  *(ALF-A0)
      CLA_ALF = DCLDA*PG
      CLA_W   = DCLDA*PG_W*(ALF-A0)
C
C--- Effective CLmax is limited by Mach effects
C    reduces CLmax to match the CL of onset of serious compressible drag
      CLMX = CLMAX
      CLMN = CLMIN
      DMSTALL  = (CDMSTALL/CDMFACTOR)**(1.0/MEXP)
      CLMAXM = MAX(0.0, (MCRIT+DMSTALL-MACH)/CLMFACTOR) + CLDMIN
      CLMAX  = MIN(CLMAX,CLMAXM)
      CLMINM = MIN(0.0,-(MCRIT+DMSTALL-MACH)/CLMFACTOR) + CLDMIN
      CLMIN  = MAX(CLMIN,CLMINM)
C
C--- CL limiter function (turns on after +-stall 
      ECMAX = DEXP( MIN(200.0D0,DBLE((CLA-CLMAX)/DCL_STALL)) )
      ECMIN = DEXP( MIN(200.0D0,DBLE((CLMIN-CLA)/DCL_STALL)) )
      CLLIM = DCL_STALL * DLOG( (1.0D0+ECMAX)/(1.0D0+ECMIN) )
      CLLIM_CLA = ECMAX/(1.0+ECMAX) + ECMIN/(1.0+ECMIN)
c
c      if(CLLIM.GT.0.001) then
c      write(*,999) 'cla,cllim,ecmax,ecmin ',cla,cllim,ecmax,ecmin
c      endif
c 999  format(a,2(1x,f10.6),3(1x,d12.6))
C
C--- Subtract off a (nearly unity) fraction of the limited CL function
C    This sets the dCL/dAlpha in the stalled regions to 1-FSTALL of that
C    in the linear lift range
      FSTALL = DCLDA_STALL/DCLDA
      CLIFT  = CLA     - (1.0-FSTALL)*CLLIM
      CL_ALF = CLA_ALF - (1.0-FSTALL)*CLLIM_CLA*CLA_ALF
      CL_W   = CLA_W   - (1.0-FSTALL)*CLLIM_CLA*CLA_W
C
      STALLF = .FALSE.
      IF(CLIFT.GT.CLMAX) STALLF = .TRUE.
      IF(CLIFT.LT.CLMIN) STALLF = .TRUE.
C
C
C------------------------------------------------------------
C--- CM from CMCON and Prandtl-Glauert scaling
      CMOM  = PG*CMCON
      CM_AL = 0.0
      CM_W  = PG_W*CMCON
C
C
C------------------------------------------------------------
C--- CD from profile drag, stall drag and compressibility drag 
C
C---- Reynolds number scaling factor
      IF(REY.LE.0) THEN
       RCORR = 1.0
       RCORR_REY = 0.0
      ELSE
       RCORR     = (REY/REREF)**REXP
       RCORR_REY =  REXP/REY
      ENDIF
C
C--- In the basic linear lift range drag is a function of lift
C    CD = CD0 (constant) + quadratic with CL)
      CDRAG  = (CDMIN + DCDCL2*(CLIFT-CLDMIN)**2    ) * RCORR
      CD_ALF = (    2.0*DCDCL2*(CLIFT-CLDMIN)*CL_ALF) * RCORR
      CD_W   = (    2.0*DCDCL2*(CLIFT-CLDMIN)*CL_W  ) * RCORR
      CD_REY = CDRAG*RCORR_REY
C
C--- Post-stall drag added
      FSTALL = DCLDA_STALL/DCLDA
      DCDX    = (1.0-FSTALL)*CLLIM/(PG*DCLDA)
c      write(*,*) 'cla,cllim,fstall,pg,dclda ',cla,cllim,fstall,pg,dclda
      DCD     = 2.0* DCDX**2
      DCD_ALF = 4.0* DCDX * 
     &         (1.0-FSTALL)*CLLIM_CLA*CLA_ALF/(PG*DCLDA)
      DCD_W = 4.0* DCDX * 
     &       ( (1.0-FSTALL)*CLLIM_CLA*CLA_W/(PG*DCLDA) - DCD/PG*PG_W )
c      write(*,*) 'alf,cl,dcd,dcd_alf,dcd_w ',alf,clift,dcd,dcd_alf,dcd_w
C
C--- Compressibility drag (accounts for drag rise above Mcrit with CL effects
C    CDC is a function of a scaling factor*(M-Mcrit(CL))**MEXP
C    DMDD is the Mach difference corresponding to CD rise of CDMDD at MCRIT
      DMDD = (CDMDD/CDMFACTOR)**(1.0/MEXP)
      CRITMACH = MCRIT-CLMFACTOR*ABS(CLIFT-CLDMIN) - DMDD
      CRITMACH_ALF  = -CLMFACTOR*ABS(CL_ALF)
      CRITMACH_W    = -CLMFACTOR*ABS(CL_W)
      IF(MACH.LT.CRITMACH) THEN
       CDC     = 0.0
       CDC_ALF = 0.0
       CDC_W   = 0.0
      ELSE
       CDC = CDMFACTOR*(MACH-CRITMACH)**MEXP
       CDC_W   = MEXP*MACH_W*CDC/MACH - MEXP*CRITMACH_W  *CDC/CRITMACH
       CDC_ALF =                      - MEXP*CRITMACH_ALF*CDC/CRITMACH
      ENDIF
c      write(*,*) 'critmach,mach ',critmach,mach
c      write(*,*) 'cdc,cdc_w,cdc_alf ',cdc,cdc_w,cdc_alf
C
      FAC   = 1.0
      FAC_W = 0.0
C--- Although test data does not show profile drag increases due to Mach # 
C    you could use something like this to add increase drag by Prandtl-Glauert
C    (or any function you choose) 
cc      FAC   = PG
cc      FAC_W = PG_W
C--- Total drag terms
      CDRAG  = FAC*CDRAG              + DCD     + CDC
      CD_ALF = FAC*CD_ALF             + DCD_ALF + CDC_ALF
      CD_W   = FAC*CD_W + FAC_W*CDRAG + DCD_W   + CDC_W
      CD_REY = FAC*CD_REY
C
      RETURN
      END ! CLCDCM

