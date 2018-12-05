!
!---- ROTOR.INC include file
!
IMPLICIT REAL (M)
!
!--- IX - max number of radial prop stations 
!--- ICASX - max number of stored cases
!--- NPARX - number of case parameters stored
!--- IWX - dimension of work arrays
PARAMETER (IX = 100, IXP = IX + 1, NPARX = 12, ICASX = 100, IWX = 200)
!
!--- NAX - max number of aerodynamic sections defined
!--- NDX - number of aerodynamic parameter defined for each section
PARAMETER (NAX = 20, NDX = 14)
!
PARAMETER (IQ = IX + 5, JX = (IQ * 3) / 2 + 1)
PARAMETER (PI = 3.141592654)
DIMENSION Q(IQ, IQ)
LOGICAL CONV, GREEK, TERSE, VRTX, FAST, FREE, DUCT, LSTRUC, &
        WIND, DEST, DESP, STALL, LDESINI, LOPRINI, &
        LROTOR, LVNORM, LPWRVAR, LEGEND
!
REAL MCRIT
!
LOGICAL LPLOT, LGRID, LLAND, LCURS, LSLOPE
CHARACTER*80 SAVFIL, FNAME
CHARACTER*32 NAME
!
COMMON/UNIT_X/ LUREAD, LUWRIT, LUTEMP, LUSAVE
!
COMMON/LOGI_X/ CONV, GREEK, TERSE, VRTX, FAST, FREE, DUCT, LSTRUC, &
        LDESINI, LOPRINI, LROTOR, LVNORM, LPWRVAR, &
        WIND, DEST, DESP, STALL(IX), LEGEND
!
COMMON/CHAR_X/ SAVFIL, FNAME, NAME
!
COMMON/DIME_X/ RHO, RMU, VSO, VEL, RAD, GEE, ALT
!
COMMON/ICON_X/ II, IINF, INCR, NN, NBLDS, IXSPAC, &
        NITERD, NITERA
COMMON/RCON_X/ VERSION, DT
!
COMMON/IGEOM_X/ IAERO(IX)
!
COMMON/RGEOM_X/ CH(IX), BETA(IX), BETA0(IX), T(IX), DBETA, &
        XI(IX), DXI(IX), XI0, XITIP, XINF, &
        XPITCH, XV(IX), RAKE
!
COMMON/IVELS_X/ NADD
COMMON/RVELS_X/ RADD(IX), &
        UADD(IX), VADD(IX), &
        UADDR(IX), VADDR(IX), &
        UBODY(IX), VBODY(IX), URDUCT
!
COMMON/AERO_X/ CL(IX), CD(IX), CM(IX), &
        RE(IX), EFFP(IX), GAM(IX), &
        DTII(IX), DPII(IX), &
        DTVI(IX), DPVI(IX), &
        DTWI(IX), DPWI(IX)
!
COMMON/IFOIL_X/ NAERO
COMMON/RFOIL_X/ XIAERO(NAX), AERODATA(NDX, NAX)
!
COMMON/LOAD_X/ PX(IX), PY(IX), PZ(IX), &
        MX(IX), MY(IX), MZ(IX), &
        PX_TY(IX), PX_TZ(IX), PZ_TX(IX), PZ_TY(IX), PZ_WZ(IX), &
        MY_TY(IX), MZ_TY(IX), MZ_TX(IX), MZ_WZ(IX), &
        TX(IXP), TY(IXP), TZ(IXP), &
        WX(IXP), WY(IXP), WZ(IXP), &
        SHRX(IXP), SHRY(IXP), SHRZ(IXP), &
        MOMX(IXP), MOMY(IXP), MOMZ(IXP), &
        EIXXB(IX), EIYYB(IX), &
        EAB(IX), GJB(IX), EKB(IX), &
        MB(IX), MXXB(IX), &
        XOCG(IX), XOSC(IX), &
        RSTB(IX)
!
COMMON/RCAS_X/ CASPAR(0:NPARX, ICASX)
COMMON/ICAS_X/ NCASE, KCASE, IWTYP
!
COMMON/OPER_X/ ADV, ADW, ADWFCTR, &
        RMS, RLX, EFFINV, &
        TSPEC, PSPEC, QSPEC, &
        TTOT, PTOT, QTOT, &
        TINV, PINV, TWAK, PWAK, TVIS, PVIS, &
        GRESMX, FRESMX, ARESMX
!
COMMON/SENS_X/ TI_ADV, PI_ADV, TI_ADW, PI_ADW, &
        TW_ADV, PW_ADV, TW_ADW, PW_ADW, &
        TV_ADV, PV_ADV, TV_ADW, PV_ADW, TV_DBE, PV_DBE, &
        TI_GAM(IX), PI_GAM(IX), &
        TW_GAM(IX), PW_GAM(IX), &
        TV_GAM(IX), PV_GAM(IX)
!
COMMON/TEMP_X/ W0(IWX), W1(IWX), W2(IWX), W3(IWX), W4(IWX), &
        W5(IWX), W6(IWX), W7(IWX), W8(IWX), W9(IWX), &
        T0(IWX), T1(IWX), T2(IWX), T3(IWX), T4(IWX), &
        T5(IWX), T6(IWX), T7(IWX), T8(IWX), T9(IWX)
!
COMMON/IPLO_X/ IDEV, IDEVRP, IPSLU, NCOLOR, &
        LPLOT, LGRID, LLAND, LCURS, LSLOPE
COMMON/RPLO_X/ SIZE, SCRNFR, PAR, CSIZE, &
        XWIND, YWIND, &
        XPAGE, YPAGE, XMARG, YMARG, &
        XYOFF(2), XYFAC(2)
!
COMMON/DESI_X/ RADDES, VELDES, ADVDES, RPMDES, R0DES, RWDES, &
        TDDES, PDDES, &
        TDES, PDES, CLDES0, CLDES(IX)
!
COMMON/IPVAR_X/ NPWRVAR
COMMON/RPVAR_X/ RPMVAR(IX), PWRVAR(IX), XPWRVAR(IX)
!
COMMON/VIND_X/ VWAK(IX), VW_GAM(IX, IX), VW_ADW(IX), VW_ADV(IX), &
        VIND(3, IX), VIND_GAM(3, IX, IX), VIND_ADW(3, IX)
!
COMMON/XWAK_X/ XW0, XWTIP, &
        XW(IX), XW_GAM(IX, IX), XW_ADW(IX), XW_ADV(IX), &
        DXW(IX), DXW_GAM(IX, IX), DXW_ADW(IX), DXW_ADV(IX)
!               
COMMON/DELT_X/ DGAM(IX), RES(IQ), DADV, DADW, DBET, DEFF, DQ(IQ), &
        DGAMOLD(IX)
!
!cc   EQUIVALENCE (A_GAMJ(0,1),Q(1,1))
!

!
!   LUREAD       Terminal input  unit number (normally 5)
!   LUWRIT       Terminal output unit number (normally 6)
!   LUTEMP       Disk file unit number
!   LUSAVE       Disk save file output unit number
!       
!   RHO         Fluid density            (dimensioned)
!   RMU         Fluid dynamic viscosity  (dimensioned)
!   VSO         Fluid speed of sound     (dimensioned)
!   VEL         Flight speed             (dimensioned)
!   RAD         Rotor tip radius         (dimensioned)
!   GEE         Earth's acceleration     (dimensioned)
!   ALT         Altitude for fluid properties (km),  999.0 if not defined
!       
!   II          Number of radial stations on blade
!   IINF        Number of radial stations on blade + outer domain
!   INCR        Radial station increment for terminal output
!   NN          Number of perturbation potential harmonic terms
!   NBLDS       Number of blades
!
!   IXSPAC      1 = cosine r/R array stretching
!               2 = sine stretching (less spacing near root)
!   
!   CH(.)       Chord array
!   BETA(.)     Twist angle array
!   BETA0(.)    Static twist angle array (with zero structural loads)
!   T(.)        Dummy radial coordinate array
!   DBETA       Accumulated change in twist angle
!   XI(.)       Radial coordinate array (r/R)
!   DXI(.)      Radial coordinate increment at each station
!   XI0         Blade root radial coordinate value
!   XITIP       Blade tip radial coordinate value  (always = 1)
!   XINF        Outer radial coordinate value where farfield BC is applied
!   XPITCH      x/c location of pitch axis for loads calculations and plots
!   IAERO       Index of inboard aero section for aero characteristics
!       
!   CL(.)       Local lift coefficient array
!   CD(.)       Local drag coefficient array
!   CM(.)       Local blade airfoil Cm
!   GAM(.)      Local circulation array
!   STALL(.)    Local profile stall flag array
!   RE(.)       Local Reynolds number array
!   EFFP(.)     Local profile efficiency array
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
!
!-- structural quantities below are referred to the following axes:
!-    X-axis points backward along rotation axis
!-    Y-axis points radially outward
!-    Z-axis points sideways at right angles to blade  (Z = X x Y)
!
!   PX PY PZ(.)  load/length in X, Y, Z directions (aero + centrifugal)
!   MX MY MZ(.)  moment/length around X, Y, Z axes (aero + centrifugal)
!
!   PX_TY(.)     sensitivities of loadings to deflections
!   PX_TZ(.)
!   PZ_TX(.)       (A_B  denotes dA/dB)
!   PZ_TY(.)
!   PZ_WZ(.)
!   MY_TY(.)
!   MZ_TY(.)
!   MZ_TX(.)
!   MZ_WZ(.)
!
!   TX TY TZ(.)  deflection angles around X, Y, Z axes
!   WX WY WZ(.)  deflections in X, Y, Z directions
!   SHRX SHRY SHRZ(.)  resultant loads (shear,tension) in X Y Z directions
!   MOMX MOMY MOMZ(.)  resultant moments (b.m., torsion) around X Y Z axes
!
!   EIXXB(.)   bending stiffness in the blade airfoil plane   (dimensioned)
!   EIYYB(.)   bending stiffness out of blade airfoil plane   (dimensioned)
!   EAB(.)     extensional stiffness                          (dimensioned)
!   GJB(.)     torsional stiffness                            (dimensioned)
!   EKB(.)     torsion moment/extension strain  stiffness     (dimensioned)
!   MB(.)      mass/length of blade                           (dimensioned)
!   MXXB(.)    pitch-axis moment of inertia/length of blade   (dimensioned)
!   XOCG(.)    x/c of blade section CG
!   XOSC(.)    x/c of blade section shear center
!   RSTB(.)    radius used for post-processing strain display (dimensioned)
!
!   CASPAR(..) case-parameter array
!          0   case run flags
!          1   advance ratio
!          2   velocity
!          3   tip angle
!          4   altitude
!          5   density
!          6   dynamic viscosity
!          7   speed of sound
!          8   power
!          9   thrust
!         10   torque
!         11   efficiency
!
!   NCASE      current number of saved operating cases
!   KCASE      indicator for independent parameter of case sweep
!            0  = none
!            1  = advance ratio
!            2  = rpm
!            3  = velocity
!            4  = blade angle
!
!   ADV         Advance ratio
!   ADW         Wake advance ratio
!   TPSPEC      Specified thrust, torque, or power
!   RMS, RLX    Rms residual and under-relaxation factor
!   EFFINV      1 / Inviscid efficiency
!   IWTYP       Type of induced velocity model emplyed currently
!                 1 = Graded Momentum,  2 = Potential Formulation
!              
!   TTOT        Rotor inviscid + viscous + nacelle thrust
!   PTOT        Rotor inviscid + viscous + nacelle power
!   QTOT        Rotor inviscid + viscous + nacelle torque  = PTOT*ADV
!   TINV, PINV  Inviscid thrust, power
!   TWAK, PWAK  Inviscid + nacelle thrust, power
!   TVIS, PVIS  Viscous thrust, power
!
!                TTOT  =  TVIS +  TWAK
!                      =  TVIS + (TINV + Tnacelle)
!
!       
!   TI_ADV      Sensitivities of TINV,PINV to advance ratio
!   PI_ADV
!   TI_ADW      Sensitivities of TINV,PINV to wake advance ratio
!   PI_ADW
!   TW_ADV      Sensitivities of TWAK,PWAK to advance ratio
!   PW_ADV
!   TW_ADW      Sensitivities of TWAK,PWAK to wake advance ratio
!   PW_ADW
!   TV_ADV      Sensitivities of TVIS,PVIS to advance ratio
!   PV_ADV
!   TV_ADW      Sensitivities of TVIS,PVIS to wake advance ratio
!   PV_ADW
!   TV_DBE      Sensitivities of TVIS,PVIS to blade angle change
!   PV_DBE
!   TI_GAM(.)   Sensitivity arrays to radial circulation distribution
!   PI_GAM(.)        "
!   TW_GAM(.)        "
!   PW_GAM(.)        "
!   TV_GAM(.)        "
!   PV_GAM(.)        "
!       
!   CONV        T if Converged solution exists
!   GREEK       T if Unrecognized command
!   TERSE       T if Terse output (no radial distributions)
!   FAST        T if Graded Momentum, Potential otherwise
!   FREE        T if Free wake
!   DUCT        T if duct is present
!   LDESINI     T if rotor is to be initialized for design each time
!   LOPRINI     T if rotor is to be initialized for analysis each time
!   LROTOR      T if rotor exists
!   LVNORM      T if flight speed V is used for normalization, wR otherwise
!   WIND        T if windmill-mode plotting is to be used
!
!   DEST        Design-to-thrust option flag
!   DESP        Design-to-power  option flag
!       
!   SAVFIL      Disk output save filename
!   FNAME       Generic filename
!   NAME        Case name
!       
!   W0-9(.)     Temporary work arrays
!   T0-9(.)     Temporary work arrays
!       
!   SIZE        Plot width in inches
!   SCRNFR      Fraction of screen taken up by graphics window
!   PAR         Plot height/width aspect ratio
!   CSIZE       Character size/plot width ratio
!   XWIND,YWIND Dimensions of X-window
!   XPAGE,YPAGE Dimensions of PostScript output page
!   XMARG,YMARG Margins on plot page
!   XYOFF(.)    x,y plot offsets  for current x-y plot, if any
!   XYFAC(.)    x,y scale factors for current x-y plot, if any
!
!   IDEV        Screen window flag
!   IDEVRP      Replotting flag
!   IPSLU       PostScript output-file flag
!   NCOLOR      Number of colors in spectrum
!
!   LPLOT       T if a plot is active
!   LGRID       T if grid is to be overlaid on x,y plots
!   LLAND       T if plots are in Landscape mode
!   LSLOPE      T if slope matching is to be used in cursor-modifications
!
!   RADDES      Design rotor radius             (dimensioned)
!   VELDES      Design speed                    (dimensioned)
!   ADVDES      Design advance ratio            (dimensioned)
!   RPMDES      Design advance ratio            (dimensioned)
!   R0DES       Design root radius              (dimensioned)
!   RWDES       Design disp. body root radius   (dimensioned)
!   TDDES       Design thrust                   (dimensioned)
!   PDDES       Design power                    (dimensioned)
!   TDES        Design thrust
!   PDES        Design power
!   CLDES0      Constant design CL
!   CLDES(.)    Radial design-CL array
!       
!   VTAN(.)     Tangential induced velocity array
!   VWAK(.)     Equivalent-rotor tangential induced velocity array
!   VT_GAM(..)  VTAN-blade bound circulation  sensitivity matrix
!   VT_ADW(.)   VTAN-wake advance ratio sensitivity matrix
!   VW_GAM(..)  VWAK-blade bound circulation  sensitivity matrix
!   VW_ADW(.)   VWAK-wake advance ratio sensitivity matrix
!   VW_ADV(.)   VWAK-advance ratio sensitivity matrix
!   XW0         Root radius of equivalent rotor (= far wake disp. body radius)
!   XWTIP       Tip radius of equivalent rotor
!   XW(.)       Equivalent-rotor radial coordinate array
!   XW_GAM(..)  XW-blade bound circulation  sensitivity matrix
!   XW_ADW(.)   XW-wake advance ratio sensitivity matrix
!   XW_ADV(.)   XW-advance ratio sensitivity matrix
!   DXW(.)      Equivalent-rotor radial coordinate increment array
!   DXW_GAM(..) DXW-blade bound circulation  sensitivity matrix
!   DXW_ADW(.)  DXW-wake advance ratio sensitivity matrix
!   DXW_ADV(.)  DXW-advance ratio sensitivity matrix
!   
!   DGAM(.)     Newton update delta array for bound circulation
!   RES(.)      Newton residual array for bound circulation
!   DADV        Newton update delta for advance ratio
!   DADW        Newton update delta for wake advance ratio
!   DBET        Newton update delta for blade angle
!   DEFF        Newton update delta for 1 / inviscid efficiency
!   DQ(.)       Generic solution vector
!   
!   UBODY(.)    Nacelle perturbation axial  velocity
!   VBODY(.)    Nacelle perturbation radial velocity
!   ABODY(.)    Nacelle cross-sectional area array
!   ZBODY(.)    Nacelle streamwise coordinate
!   NZ          Number of nacelle streamwise stations
    



