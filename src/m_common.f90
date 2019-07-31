!***********************************************************************
!   Copyright (c) 2018 D. de Vries
!   Original Copyright (c) 2011 Mark Drela
!
!   This file is part of XRotor.
!
!   XRotor is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   XRotor is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with XRotor.  If not, see <https://www.gnu.org/licenses/>.
!***********************************************************************

module m_common
    implicit none

    public
    integer :: ix, ixp, nparx, icasx, iwx, nax, ndx, iq, jx
    real :: pi
    !--- ix - max number of radial prop stations
    !--- icasx - max number of stored cases
    !--- nparx - number of case parameters stored
    !--- iwx - dimension of work arrays
    parameter (ix = 100, ixp = ix + 1, nparx = 12, icasx = 100, iwx = 200)

    !--- nax - max number of aerodynamic sections defined
    !--- ndx - number of aerodynamic parameter defined for each section
    parameter (nax = 20, ndx = 14)

    parameter (iq = ix + 5, jx = (iq * 3) / 2 + 1)
    parameter (pi = 3.141592654)

    logical, private, parameter :: f=.false.
    integer, private :: i
    integer, private, parameter :: int_ix(ix) = (/ (0, i=1, ix) /)
    real, private, parameter :: rix(ix) = (/ (0., i=1, ix) /)
    real, private, parameter :: rixp(ixp) = (/ (0., i=1, ixp) /)
    real, private, parameter :: riwx(iwx) = (/ (0., i=1, iwx) /)
    real, private, parameter :: rixix(ix, ix) = reshape((/ (0., i=1, ix*ix) /), (/ix, ix/))
    real, private, parameter :: r3ix(3, ix) = reshape((/ (0., i=1, 3*ix) /), (/3, ix/))
    real, private, parameter :: riq(iq) = (/(0., i=1, iq)/)

    logical, public :: show_output = .true.

    type, public :: Common
        real :: q(iq, iq) = reshape((/ (0, i=1, iq*iq) /), (/iq, iq/))

        character(len=80) :: savfil = '', fname = ''
        character(len=32) :: name = ''

        integer :: luread = 0, luwrit = 0, lutemp = 0, lusave = 0

        logical :: conv=f, greek=f, terse=f, vrtx=f, fast=f, free=f, duct=f, lstruc=f, &
                ldesini=f, loprini=f, lrotor=f, lvnorm=f, lpwrvar=f, &
                wind=f, dest=f, desp=f, stall(ix)=(/ (f, i=1, ix) /), legend=f

        real :: rho=0., rmu=0., vso=0., vel=0., rad=0., gee=0., alt=0.

        integer :: ii=0, iinf=0, incr=0, nn=0, nblds=0, ixspac=0, &
                niterd=0, nitera=0
        real :: version=0., dt=0.

        integer :: iaero(ix) = int_ix + 0

        real :: ch(ix)=rix, beta(ix)=rix, beta0(ix)=rix, t(ix)=rix, dbeta=0., &
                xi(ix)=rix, dxi(ix)=rix, xi0=0., xitip=0., xinf=0., &
                xpitch=0., xv(ix)=rix, rake=0.

        integer :: nadd=0
        real :: radd(ix)=rix, &
                uadd(ix)=rix, vadd(ix)=rix, &
                uaddr(ix)=rix, vaddr(ix)=rix, &
                ubody(ix)=rix, vbody(ix)=rix, urduct=0.

        real :: cl(ix)=rix, cd(ix)=rix, cm(ix)=rix, &
                re(ix)=rix, effp(ix)=rix, gam(ix)=rix, &
                dtii(ix)=rix, dpii(ix)=rix, &
                dtvi(ix)=rix, dpvi(ix)=rix, &
                dtwi(ix)=rix, dpwi(ix)=rix

        integer :: naero=0
        real :: xiaero(nax) = (/ (0., i=1, nax) /), &
                aerodata(ndx, nax) = reshape((/ (0., i=1, ndx*nax) /), (/ndx, nax/))

        real :: px(ix)=rix, py(ix)=rix, pz(ix)=rix, &
                mx(ix)=rix, my(ix)=rix, mz(ix)=rix, &
                px_ty(ix)=rix, px_tz(ix)=rix, pz_tx(ix)=rix, pz_ty(ix)=rix, pz_wz(ix)=rix, &
                my_ty(ix)=rix, mz_ty(ix)=rix, mz_tx(ix)=rix, mz_wz(ix)=rix, &
                tx(ixp)=rixp, ty(ixp)=rixp, tz(ixp)=rixp, &
                wx(ixp)=rixp, wy(ixp)=rixp, wz(ixp)=rixp, &
                shrx(ixp)=rixp, shry(ixp)=rixp, shrz(ixp)=rixp, &
                momx(ixp)=rixp, momy(ixp)=rixp, momz(ixp)=rixp, &
                eixxb(ix)=rix, eiyyb(ix)=rix, &
                eab(ix)=rix, gjb(ix)=rix, ekb(ix)=rix, &
                mb(ix)=rix, mxxb(ix)=rix, &
                xocg(ix)=rix, xosc(ix)=rix, &
                rstb(ix)=rix

        real :: caspar(0:nparx, icasx) = reshape((/ (0., i=1, (nparx+1)*icasx) /), (/nparx+1, icasx/))
        integer :: ncase=0, kcase=0, iwtyp=0

        real :: adv=0., adw=0., adwfctr=0., &
                rms=0., rlx=0., effinv=0., &
                tspec=0., pspec=0., qspec=0., &
                ttot=0., ptot=0., qtot=0., &
                tinv=0., pinv=0., twak=0., pwak=0., tvis=0., pvis=0., &
                gresmx=0., fresmx=0., aresmx=0.

        real :: ti_adv=0., pi_adv=0., ti_adw=0., pi_adw=0., &
                tw_adv=0., pw_adv=0., tw_adw=0., pw_adw=0., &
                tv_adv=0., pv_adv=0., tv_adw=0., pv_adw=0., tv_dbe=0., pv_dbe=0., &
                ti_gam(ix)=rix, pi_gam(ix)=rix, &
                tw_gam(ix)=rix, pw_gam(ix)=rix, &
                tv_gam(ix)=rix, pv_gam(ix)=rix

        real :: w0(iwx)=riwx, w1(iwx)=riwx, w2(iwx)=riwx, w3(iwx)=riwx, w4(iwx)=riwx, &
                w5(iwx)=riwx, w6(iwx)=riwx, w7(iwx)=riwx, w8(iwx)=riwx, w9(iwx)=riwx, &
                t0(iwx)=riwx, t1(iwx)=riwx, t2(iwx)=riwx, t3(iwx)=riwx, t4(iwx)=riwx, &
                t5(iwx)=riwx, t6(iwx)=riwx, t7(iwx)=riwx, t8(iwx)=riwx, t9(iwx)=riwx

        real :: raddes=0., veldes=0., advdes=0., rpmdes=0., r0des=0., rwdes=0., &
                tddes=0., pddes=0., &
                tdes=0., pdes=0., cldes0=0., cldes(ix)=rix

        integer :: npwrvar=0
        real :: rpmvar(ix)=rix, pwrvar(ix)=rix, xpwrvar(ix)=rix

        real :: vwak(ix)=rix, vw_gam(ix, ix)=rixix, vw_adw(ix)=rix, vw_adv(ix)=rix, &
                vind(3, ix)=r3ix, vind_gam(3, ix, ix)=reshape((/(0., i=1, 3*ix*ix)/), (/3, ix, ix/)), &
                vind_adw(3, ix)=r3ix

        real :: xw0=0., xwtip=0., &
                xw(ix)=rix, xw_gam(ix, ix)=rixix, xw_adw(ix)=rix, xw_adv(ix)=rix, &
                dxw(ix)=rix, dxw_gam(ix, ix)=rixix, dxw_adw(ix)=rix, dxw_adv(ix)=rix

        real :: dgam(ix)=rix, res(iq)=riq, dadv=0., dadw=0., dbet=0., deff=0., dq(iq)=riq, &
                dgamold(ix)=rix
    end type Common
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
end module m_common