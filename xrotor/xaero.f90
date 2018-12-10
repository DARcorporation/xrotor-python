!***********************************************************************
!    Module:  xaero.f
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


!--- Aero data stored for one or more radial aerodynamic sections
!   
!-- aero data quantities for each defined radial aerodynamic section
!   NAERO       Number of aerodynamic datasets defined (NAERO>=1)
!   XIAERO      Radial station r/R where aero dataset is defined
!   AERODATA    Aerodynamic definition of the blade section at XIAERO
!               AERODATA( 1,x) = A0 (angle of zero lift)
!               AERODATA( 2,x) = CLMAX (Max CL)
!               AERODATA( 3,x) = CLMIN (Min CL)
!               AERODATA( 4,x) = DCLDA (Incompressible 2-D lift curve slope)
!               AERODATA( 5,x) = DCLDA_STALL (2-D lift curve slope at stall)
!               AERODATA( 6,x) = DCL_STALL (CL increment, onset to full stall)
!               AERODATA( 7,x) = CDMIN (Minimum drag coefficient value)
!               AERODATA( 8,x) = CLDMIN (Lift at minimum drag value)
!               AERODATA( 9,x) = DCDCL2 (Parabolic drag param d(Cd)/dCL^2)
!               AERODATA(10,x) = CMCON (Incompressible 2-D pitching moment)
!               AERODATA(11,x) = REREF (reference Reynold's number)
!               AERODATA(12,x) = REXP (Reynold's number exponent Cd~Re^REXP)
!               AERODATA(13,x) = MCRIT (critical Mach #)
!               AERODATA(14,x) = TOC (thickness/chord)


SUBROUTINE SETIAERO
    !--------------------------------------------------
    !     Sets up indices referring to aero section for
    !     each radial station
    !--------------------------------------------------
    USE common
    IMPLICIT REAL (M)
    !
    !--- Find lower index of aero data sections XIAERO(N) bounding XI(IS)
    DO I = 1, II
        IAERO(I) = 1
        DO N = 1, NAERO
            IF(XIAERO(N) <= XI(I)) THEN
                IAERO(I) = N
            ENDIF
        END DO
    END DO
    RETURN
END

SUBROUTINE GETAERO(N, XISECT, A0, CLMAX, CLMIN, &
        DCLDA, DCLDA_STALL, DCL_STALL, &
        CDMIN, CLDMIN, DCDCL2, CMCON, MCRIT, REREF, REXP)
    !---------------------------------------------
    !     Gets aero data from stored section array
    !---------------------------------------------
    USE common
    IMPLICIT REAL(M)
    !
    IF(N < 1 .OR. N > NAERO) THEN
        WRITE(*, *) 'Error: index of aero section out of bounds'
        RETURN
    ENDIF
    !
    A0 = AERODATA(1, N)
    CLMAX = AERODATA(2, N)
    CLMIN = AERODATA(3, N)
    DCLDA = AERODATA(4, N)
    DCLDA_STALL = AERODATA(5, N)
    DCL_STALL = AERODATA(6, N)
    CDMIN = AERODATA(7, N)
    CLDMIN = AERODATA(8, N)
    DCDCL2 = AERODATA(9, N)
    CMCON = AERODATA(10, N)
    REREF = AERODATA(11, N)
    REXP = AERODATA(12, N)
    MCRIT = AERODATA(13, N)
    XISECT = XIAERO(N)
    !
    RETURN
END

SUBROUTINE PUTAERO(N, XISECT, A0, CLMAX, CLMIN, &
        DCLDA, DCLDA_STALL, DCL_STALL, &
        CDMIN, CLDMIN, DCDCL2, CMCON, MCRIT, REREF, REXP)
    !--------------------------------------------------------
    !     Puts aero data into stored section array at index N
    !--------------------------------------------------------
    USE common
    IMPLICIT REAL (M)
    !
    IF(N > NAX) THEN
        WRITE(*, *) 'Too many aero sections defined...'
        RETURN
    ENDIF
    !
    AERODATA(1, N) = A0
    AERODATA(2, N) = CLMAX
    AERODATA(3, N) = CLMIN
    AERODATA(4, N) = DCLDA
    AERODATA(5, N) = DCLDA_STALL
    AERODATA(6, N) = DCL_STALL
    AERODATA(7, N) = CDMIN
    AERODATA(8, N) = CLDMIN
    AERODATA(9, N) = DCDCL2
    AERODATA(10, N) = CMCON
    AERODATA(11, N) = REREF
    AERODATA(12, N) = REXP
    AERODATA(13, N) = MCRIT
    XIAERO(N) = XISECT
    !
    RETURN
END


!*************************************************************************
!  Interpolated aero section properties functions
!  These routines implement a functional representation of the 
!  blade aero properties (CL,CD,CM) vs ALFA
!*************************************************************************


SUBROUTINE GETCLCDCM(IS, ALF, W, REY, &
        CLIFT, CL_ALF, CL_W, &
        CLMAX, CLMIN, DCL_STALL, STALLF, &
        CDRAG, CD_ALF, CD_W, CD_REY, &
        CMOM, CM_AL, CM_W)
    !-------------------------------------------------------------
    !     CL(alpha),
    !      CD(alpha),
    !       CM(alpha) interpolation function for blade at station IS
    !-------------------------------------------------------------
    USE common
    IMPLICIT REAL (M)
    LOGICAL STALLF, STALLF2
    !
    !--- Check for installed aero data section index
    N = IAERO(IS)
    IF(N < 1 .OR. N > NAERO) THEN
        !
        IF(NAERO > 1) THEN
            !--- Find lower index of aero data sections XIAERO(N) bounding XI(IS)
            DO N = 1, NAERO
                IF(XIAERO(N) <= XI(IS)) THEN
                    !c          write(*,*) 'getcl iaero= ',N,' is= ',is,xiaero(N),xi(is)
                    IAERO(IS) = N
                ELSE
                    GO TO 10
                ENDIF
            END DO
            WRITE(*, *) 'Aero section not found for station ', XI(IS)
        ENDIF
        !
        N = 1
        IAERO(IS) = N
    ENDIF
    !
    !--- Get section aero data from stored section array
    10   A0 = AERODATA(1, N)
    CLMAX = AERODATA(2, N)
    CLMIN = AERODATA(3, N)
    DCLDA = AERODATA(4, N)
    DCLDA_STALL = AERODATA(5, N)
    DCL_STALL = AERODATA(6, N)
    CDMIN = AERODATA(7, N)
    CLDMIN = AERODATA(8, N)
    DCDCL2 = AERODATA(9, N)
    CMCON = AERODATA(10, N)
    REREF = AERODATA(11, N)
    REXP = AERODATA(12, N)
    MCRIT = AERODATA(13, N)
    XISECT1 = XIAERO(N)
    !--- Get data for inner bounding aero section
    CALL CLCDCM(ALF, W, REY, &
            CLIFT, CL_ALF, CL_W, STALLF, &
            CDRAG, CD_ALF, CD_W, CD_REY, &
            CMOM, CM_AL, CM_W, &
            A0, CLMAX, CLMIN, DCLDA, DCLDA_STALL, DCL_STALL, &
            CDMIN, CLDMIN, DCDCL2, CMCON, MCRIT, REREF, REXP)
    !
    !--- Check for another bounding section, if not we are done,
    !    if we have another section linearly interpolate data to station IS
    IF(N < NAERO) THEN
        XISECT2 = XIAERO(N + 1)
        FRAC = (XI(IS) - XISECT1) / (XISECT2 - XISECT1)
        IF(FRAC <= 0.0 .OR. FRAC > 1.0) THEN
            !c         write(*,*) 'CL n,is,xi,frac = ',n,is,xi(is),frac
        ENDIF
        !
        A0 = AERODATA(1, N + 1)
        CLMAX2 = AERODATA(2, N + 1)
        CLMIN2 = AERODATA(3, N + 1)
        DCLDA = AERODATA(4, N + 1)
        DCLDA_STALL = AERODATA(5, N + 1)
        DCL_STALL2 = AERODATA(6, N + 1)
        CDMIN = AERODATA(7, N + 1)
        CLDMIN = AERODATA(8, N + 1)
        DCDCL2 = AERODATA(9, N + 1)
        CMCON = AERODATA(10, N + 1)
        REREF = AERODATA(11, N + 1)
        REXP = AERODATA(12, N + 1)
        MCRIT = AERODATA(13, N + 1)
        !--- Get data for outer bounding aero section
        CALL CLCDCM(ALF, W, REY, &
                CLIFT2, CL_ALF2, CL_W2, STALLF2, &
                CDRAG2, CD_ALF2, CD_W2, CD_REY2, &
                CMOM2, CM_AL2, CM_W2, &
                A0, CLMAX2, CLMIN2, DCLDA, DCLDA_STALL, DCL_STALL2, &
                CDMIN, CLDMIN, DCDCL2, CMCON, MCRIT, REREF, REXP)
        !--- Interpolate aero data to blade station
        STALLF = STALLF .OR. STALLF2
        CLIFT = (1.0 - FRAC) * CLIFT + FRAC * CLIFT2
        CL_ALF = (1.0 - FRAC) * CL_ALF + FRAC * CL_ALF2
        CL_W = (1.0 - FRAC) * CL_W + FRAC * CL_W2
        CLMAX = (1.0 - FRAC) * CLMAX + FRAC * CLMAX2
        CLMIN = (1.0 - FRAC) * CLMIN + FRAC * CLMIN2
        DCL_STALL = (1.0 - FRAC) * DCL_STALL + FRAC * DCL_STALL2
        !
        CMOM = (1.0 - FRAC) * CMOM + FRAC * CMOM2
        CM_AL = (1.0 - FRAC) * CM_AL + FRAC * CM_AL2
        CM_W = (1.0 - FRAC) * CM_W + FRAC * CM_W2
        !
        CDRAG = (1.0 - FRAC) * CDRAG + FRAC * CDRAG2
        CD_ALF = (1.0 - FRAC) * CD_ALF + FRAC * CD_ALF2
        CD_W = (1.0 - FRAC) * CD_W + FRAC * CD_W2
        CD_REY = (1.0 - FRAC) * CD_REY + FRAC * CD_REY2
    ENDIF
    !
    RETURN
END


SUBROUTINE GETALF(IS, CLIFT, W, ALF, ALF_CL, ALF_W, STALLF)
    !------------------------------------------------------------
    !     Inverse alpha(CL) function
    !     Uses Newton-Raphson iteration to get ALF from CL function
    !------------------------------------------------------------
    USE common
    IMPLICIT REAL (M)
    LOGICAL STALLF
    DATA NITER / 10 /
    DATA EPS   / 1.0E-5 /
    !
    STALLF = .FALSE.
    !
    !---HHY had to set A0 to first aero section as A0 is now section property
    A0 = AERODATA(1, 1)
    REY = 0.0
    !
    ALF = A0
    DO ITER = 1, NITER
        CALL GETCLCDCM(IS, ALF, W, REY, &
                CLTEMP, CL_ALF, CL_W, &
                CLMAX, CLMIN, DCL_STALL, STALLF, &
                CDRAG, CD_ALF, CD_W, CD_REY, &
                CMOM, CM_AL, CM_W)
        !c      IF(STALLF) GO TO 20
        DALF = -(CLTEMP - CLIFT) / CL_ALF
        ALF = ALF + DALF
        ALF_CL = 1.0 / CL_ALF
        ALF_W = -CL_W / CL_ALF
        IF(ABS(DALF) < EPS) RETURN
    END DO
    !
    20 WRITE(*, *) 'GETALF: alpha(CL) function inversion failed'
    !      write(*,*) 'is,clift  ',is,clift
    !      write(*,*) 'abs(dalf) ',abs(dalf)
    !      write(*,*) 'cl_alf    ',cl_alf
    !
    RETURN
END
! GETALF



!*************************************************************************
!  Basic aero section properties functions
!  These routines implement a functional representation of the 
!  blade section aero properties (CL,CD,CM) vs ALFA
!*************************************************************************

SUBROUTINE CLCDCM(ALF, W, REY, &
        CLIFT, CL_ALF, CL_W, STALLF, &
        CDRAG, CD_ALF, CD_W, CD_REY, &
        CMOM, CM_AL, CM_W, &
        A0, CLMAX, CLMIN, DCLDA, DCLDA_STALL, DCL_STALL, &
        CDMIN, CLDMIN, DCDCL2, CMCON, MCRIT, REREF, REXP)
    !------------------------------------------------------------
    !     CL(alpha) function
    !     Note that in addition to setting CLIFT and its derivatives
    !     CLMAX and CLMIN (+ and - stall CL's) are set in this routine
    !     In the compressible range the stall CL is reduced by a factor
    !     proportional to Mcrit-Mach.  Stall limiting for compressible
    !     cases begins when the compressible drag added CDC > CDMstall
    !------------------------------------------------------------
    !     CD(alpha) function - presently CD is assumed to be a sum
    !     of profile drag + stall drag + compressibility drag
    !     In the linear lift range drag is CD0 + quadratic function of CL-CLDMIN
    !     In + or - stall an additional drag is added that is proportional
    !     to the extent of lift reduction from the linear lift value.
    !     Compressible drag is based on adding drag proportional to
    !     (Mach-Mcrit_eff)^MEXP
    !------------------------------------------------------------
    !     CM(alpha) function - presently CM is assumed constant,
    !     varying only with Mach by Prandtl-Glauert scaling
    !------------------------------------------------------------
    !
    USE common
    IMPLICIT REAL (M)
    LOGICAL STALLF
    DOUBLE PRECISION ECMIN, ECMAX
    !
    !---- Factors for compressibility drag model, HHY 10/23/00
    !     Mcrit is set by user
    !     Effective Mcrit is Mcrit_eff = Mcrit - CLMFACTOR*(CL-CLDmin) - DMDD
    !     DMDD is the delta Mach to get CD=CDMDD (usually 0.0020)
    !     Compressible drag is CDC = CDMFACTOR*(Mach-Mcrit_eff)^MEXP
    !     CDMstall is the drag at which compressible stall begins
    !
    CDMFACTOR = 10.0
    CLMFACTOR = 0.25
    MEXP = 3.0
    CDMDD = 0.0020
    CDMSTALL = 0.1000
    !
    !---- Prandtl-Glauert compressibility factor
    MSQ = W * W * VEL**2 / VSO**2
    MSQ_W = 2.0 * W * VEL**2 / VSO**2
    IF(MSQ >= 1.0) THEN
        WRITE(*, *)&
                'CLFUNC: Local Mach number limited to 0.99, was ', MSQ
        MSQ = 0.99
        MSQ_W = 0.
    ENDIF
    PG = 1.0 / SQRT(1.0 - MSQ)
    PG_W = 0.5 * MSQ_W * PG**3
    !
    !---- Mach number and dependence on velocity
    MACH = SQRT(MSQ)
    MACH_W = 0.0
    IF(MACH /= 0.0) MACH_W = 0.5 * MSQ_W / MACH
    !
    !
    !------------------------------------------------------------
    !--- Generate CL from dCL/dAlpha and Prandtl-Glauert scaling
    CLA = DCLDA * PG * (ALF - A0)
    CLA_ALF = DCLDA * PG
    CLA_W = DCLDA * PG_W * (ALF - A0)
    !
    !--- Effective CLmax is limited by Mach effects
    !    reduces CLmax to match the CL of onset of serious compressible drag
    CLMX = CLMAX
    CLMN = CLMIN
    DMSTALL = (CDMSTALL / CDMFACTOR)**(1.0 / MEXP)
    CLMAXM = MAX(0.0, (MCRIT + DMSTALL - MACH) / CLMFACTOR) + CLDMIN
    CLMAX = MIN(CLMAX, CLMAXM)
    CLMINM = MIN(0.0, -(MCRIT + DMSTALL - MACH) / CLMFACTOR) + CLDMIN
    CLMIN = MAX(CLMIN, CLMINM)
    !
    !--- CL limiter function (turns on after +-stall
    ECMAX = DEXP(MIN(200.0D0, DBLE((CLA - CLMAX) / DCL_STALL)))
    ECMIN = DEXP(MIN(200.0D0, DBLE((CLMIN - CLA) / DCL_STALL)))
    CLLIM = DCL_STALL * DLOG((1.0D0 + ECMAX) / (1.0D0 + ECMIN))
    CLLIM_CLA = ECMAX / (1.0 + ECMAX) + ECMIN / (1.0 + ECMIN)
    !
    !      if(CLLIM > 0.001) then
    !      write(*,999) 'cla,cllim,ecmax,ecmin ',cla,cllim,ecmax,ecmin
    !      endif
    ! 999  format(a,2(1x,f10.6),3(1x,d12.6))
    !
    !--- Subtract off a (nearly unity) fraction of the limited CL function
    !    This sets the dCL/dAlpha in the stalled regions to 1-FSTALL of that
    !    in the linear lift range
    FSTALL = DCLDA_STALL / DCLDA
    CLIFT = CLA - (1.0 - FSTALL) * CLLIM
    CL_ALF = CLA_ALF - (1.0 - FSTALL) * CLLIM_CLA * CLA_ALF
    CL_W = CLA_W - (1.0 - FSTALL) * CLLIM_CLA * CLA_W
    !
    STALLF = .FALSE.
    IF(CLIFT > CLMAX) STALLF = .TRUE.
    IF(CLIFT < CLMIN) STALLF = .TRUE.
    !
    !
    !------------------------------------------------------------
    !--- CM from CMCON and Prandtl-Glauert scaling
    CMOM = PG * CMCON
    CM_AL = 0.0
    CM_W = PG_W * CMCON
    !
    !
    !------------------------------------------------------------
    !--- CD from profile drag, stall drag and compressibility drag
    !
    !---- Reynolds number scaling factor
    IF(REY <= 0) THEN
        RCORR = 1.0
        RCORR_REY = 0.0
    ELSE
        RCORR = (REY / REREF)**REXP
        RCORR_REY = REXP / REY
    ENDIF
    !
    !--- In the basic linear lift range drag is a function of lift
    !    CD = CD0 (constant) + quadratic with CL)
    CDRAG = (CDMIN + DCDCL2 * (CLIFT - CLDMIN)**2) * RCORR
    CD_ALF = (2.0 * DCDCL2 * (CLIFT - CLDMIN) * CL_ALF) * RCORR
    CD_W = (2.0 * DCDCL2 * (CLIFT - CLDMIN) * CL_W) * RCORR
    CD_REY = CDRAG * RCORR_REY
    !
    !--- Post-stall drag added
    FSTALL = DCLDA_STALL / DCLDA
    DCDX = (1.0 - FSTALL) * CLLIM / (PG * DCLDA)
    !      write(*,*) 'cla,cllim,fstall,pg,dclda ',cla,cllim,fstall,pg,dclda
    DCD = 2.0 * DCDX**2
    DCD_ALF = 4.0 * DCDX * &
            (1.0 - FSTALL) * CLLIM_CLA * CLA_ALF / (PG * DCLDA)
    DCD_W = 4.0 * DCDX * &
            ((1.0 - FSTALL) * CLLIM_CLA * CLA_W / (PG * DCLDA) - DCD / PG * PG_W)
    !      write(*,*) 'alf,cl,dcd,dcd_alf,dcd_w ',alf,clift,dcd,dcd_alf,dcd_w
    !
    !--- Compressibility drag (accounts for drag rise above Mcrit with CL effects
    !    CDC is a function of a scaling factor*(M-Mcrit(CL))**MEXP
    !    DMDD is the Mach difference corresponding to CD rise of CDMDD at MCRIT
    DMDD = (CDMDD / CDMFACTOR)**(1.0 / MEXP)
    CRITMACH = MCRIT - CLMFACTOR * ABS(CLIFT - CLDMIN) - DMDD
    CRITMACH_ALF = -CLMFACTOR * ABS(CL_ALF)
    CRITMACH_W = -CLMFACTOR * ABS(CL_W)
    IF(MACH < CRITMACH) THEN
        CDC = 0.0
        CDC_ALF = 0.0
        CDC_W = 0.0
    ELSE
        CDC = CDMFACTOR * (MACH - CRITMACH)**MEXP
        CDC_W = MEXP * MACH_W * CDC / MACH - MEXP * CRITMACH_W * CDC / CRITMACH
        CDC_ALF = - MEXP * CRITMACH_ALF * CDC / CRITMACH
    ENDIF
    !      write(*,*) 'critmach,mach ',critmach,mach
    !      write(*,*) 'cdc,cdc_w,cdc_alf ',cdc,cdc_w,cdc_alf
    !
    FAC = 1.0
    FAC_W = 0.0
    !--- Although test data does not show profile drag increases due to Mach #
    !    you could use something like this to add increase drag by Prandtl-Glauert
    !    (or any function you choose)
    !c      FAC   = PG
    !c      FAC_W = PG_W
    !--- Total drag terms
    CDRAG = FAC * CDRAG + DCD + CDC
    CD_ALF = FAC * CD_ALF + DCD_ALF + CDC_ALF
    CD_W = FAC * CD_W + FAC_W * CDRAG + DCD_W + CDC_W
    CD_REY = FAC * CD_REY
    !
    RETURN
END
! CLCDCM
