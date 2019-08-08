!*==M_XAERO.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
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


!--- Aero data stored for one or more radial aerodynamic sections
!
!-- aero data quantities for each defined radial aerodynamic section
!   naero       Number of aerodynamic datasets defined (naero>=1)
!   xiaero      Radial station r/r where aero dataset is defined
!   aerodata    Aerodynamic definition of the blade section at xiaero
!               aerodata( 1,x) = a0 (angle of zero lift)
!               aerodata( 2,x) = clmax (Max cl)
!               aerodata( 3,x) = clmin (Min cl)
!               aerodata( 4,x) = dclda (Incompressible 2-d lift curve slope)
!               aerodata( 5,x) = dclda_stall (2-d lift curve slope at stall)
!               aerodata( 6,x) = dcl_stall (cl increment, onset to full stall)
!               aerodata( 7,x) = cdmin (Minimum drag coefficient value)
!               aerodata( 8,x) = cldmin (Lift at minimum drag value)
!               aerodata( 9,x) = dcdcl2 (Parabolic drag param d(Cd)/dcl^2)
!               aerodata(10,x) = cmcon (Incompressible 2-d pitching moment)
!               aerodata(11,x) = reref (reference Reynold's number)
!               aerodata(12,x) = rexp (Reynold's number exponent Cd~Re^rexp)
!               aerodata(13,x) = mcrit (critical Mach #)
!               aerodata(14,x) = toc (thickness/chord)


module m_xaero
    implicit none
contains
    subroutine setiaero(ctxt)
        !--------------------------------------------------
        !     Sets up indices referring to aero section for
        !     each radial station
        !--------------------------------------------------
        use i_common, only : Common
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        integer I, N
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        !
        !--- Find lower index of aero data sections xiaero(n) bounding xi(is)
        do i = 1, ctxt%ii
            ctxt%iaero(i) = 1
            do n = 1, ctxt%n_polars
                if (ctxt%xi_polars(n)<=ctxt%xi(i)) ctxt%iaero(i) = n
            enddo
        enddo
    end


    subroutine getpolar(ctxt, n, polar)
        use i_common, only : Common
        type(Common), intent(inout) :: ctxt
        integer, intent(in) :: n
        real, allocatable, intent(out) :: polar(:, :) ! ctxt%n_polar_points(n), 4)

        polar = ctxt%polardata(ctxt%i_polars(n):ctxt%i_polars(n+1)-1, :)
    end


    subroutine putpolars(ctxt, n_polars, n_polar_points, xi_polars, polardata)
        use i_common, only : Common
        type(Common), intent(inout) :: ctxt
        integer, intent(in) :: n_polars, n_polar_points(n_polars)
        real, intent(in) :: xi_polars(n_polars), polardata(sum(n_polar_points), 4)

        integer :: i, i_start, i_end
        integer, allocatable :: temp_indices(:), temp_n_polar_points(:)
        real, allocatable :: temp_xi_polars(:), temp_polardata(:, :)

        ctxt%n_polars = n_polars

        temp_indices = (/1, (1 + sum(n_polar_points(:i)), i=1, n_polars)/)
        call move_alloc(temp_indices, ctxt%i_polars)

        temp_n_polar_points = n_polar_points
        call move_alloc(temp_n_polar_points, ctxt%n_polar_points)

        temp_xi_polars = xi_polars
        call move_alloc(temp_xi_polars, ctxt%xi_polars)

        temp_polardata = polardata
        call move_alloc(temp_polardata, ctxt%polardata)
    end

    !*************************************************************************
    !  Interpolated aero section properties functions
    !  These routines implement a functional representation of the
    !  blade aero properties (cl,cd,cm) vs alfa
    !*************************************************************************


    subroutine getclcdcm(ctxt, is, alf, w, rey, &
            clift, cl_alf, cl_w, &
            clmax, clmin, dcl_stall, stallf, &
            cdrag, cd_alf, cd_w, cd_rey, &
            cmom, cm_al, cm_w)
        !-------------------------------------------------------------
        !     cl(alpha),
        !      cd(alpha),
        !       cm(alpha) interpolation function for blade at station is
        !-------------------------------------------------------------
        use i_common, only : Common, show_output
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real A0, ALF, CDMIN, CDRAG, CDRAG2, CD_ALF, CD_ALF2, CD_REY, CD_REY2, &
                & CD_W, CD_W2, CLDMIN, CLIFT, CLIFT2, CLMAX, CLMAX2, CLMIN, CLMIN2, &
                & CL_ALF, CL_ALF2
        real CL_W, CL_W2, CMCON, CMOM, CMOM2, CM_AL, CM_AL2, CM_W, CM_W2, DCDCL2, &
                & DCLDA, DCLDA_STALL, DCL_STALL, DCL_STALL2, FRAC, MCRIT, REREF, REXP, &
                & REY, W
        integer IS, N
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        logical stallf, stallf2
        real :: xi_polar1, xi_polar2

        real, allocatable :: polar(:, :)
        !
        !--- Check for installed aero data section index
        n = ctxt%iaero(is)
        if (n<1.or.n>ctxt%n_polars) then
            !
            if (ctxt%n_polars>1) then
                !--- Find lower index of aero data sections xiaero(n) bounding xi(is)
                do n = 1, ctxt%n_polars
                    if (ctxt%xi_polars(n)>ctxt%xi(is)) goto 100
                    !c          write(*,*) 'getcl iaero= ',n,' is= ',is,xiaero(n),xi(is)
                    ctxt%iaero(is) = n
                enddo
                if (show_output) write (*, *)                                   &
                        &'aero section not found for station '&
                        &, ctxt%xi(is)
            endif
            !
            n = 1
            ctxt%iaero(is) = n
        endif
        !
        !--- Get section aero data from stored section array
        100   call getpolar(ctxt, n, polar)
        clmax = maxval(polar(:, 2))
        clmin = minval(polar(:, 2))
        xi_polar1 = ctxt%xi_polars(n)
        !--- Get data for inner bounding aero section
        call clcdcm(ctxt, alf, w, rey, &
                clift, cl_alf, cl_w, stallf, &
                cdrag, cd_alf, cd_w, cd_rey, &
                cmom, cm_al, cm_w, &
                polar)
        !
        !--- Check for another bounding section, if not we are done,
        !    if we have another section linearly interpolate data to station is
        if (n<ctxt%n_polars) then
            xi_polar2 = ctxt%xi_polars(n + 1)
            frac = (ctxt%xi(is) - xi_polar1) / (xi_polar2 - xi_polar1)
            if (frac<=0.0.or.frac>1.0) then
                !c         write(*,*) 'cl n,is,xi,frac = ',n,is,xi(is),frac
            endif
            !
            call getpolar(ctxt, n + 1, polar)
            clmax2 = maxval(polar(:, 2))
            clmin2 = minval(polar(:, 2))
            !--- Get data for outer bounding aero section
            call clcdcm(ctxt, alf, w, rey, &
                    clift2, cl_alf2, cl_w2, stallf2, &
                    cdrag2, cd_alf2, cd_w2, cd_rey2, &
                    cmom2, cm_al2, cm_w2, &
                    polar)
            !--- Interpolate aero data to blade station
            stallf = stallf .or. stallf2
            clift = (1.0 - frac) * clift + frac * clift2
            cl_alf = (1.0 - frac) * cl_alf + frac * cl_alf2
            cl_w = (1.0 - frac) * cl_w + frac * cl_w2
            clmax = (1.0 - frac) * clmax + frac * clmax2
            clmin = (1.0 - frac) * clmin + frac * clmin2
            dcl_stall = (1.0 - frac) * dcl_stall + frac * dcl_stall2
            !
            cmom = (1.0 - frac) * cmom + frac * cmom2
            cm_al = (1.0 - frac) * cm_al + frac * cm_al2
            cm_w = (1.0 - frac) * cm_w + frac * cm_w2
            !
            cdrag = (1.0 - frac) * cdrag + frac * cdrag2
            cd_alf = (1.0 - frac) * cd_alf + frac * cd_alf2
            cd_w = (1.0 - frac) * cd_w + frac * cd_w2
            cd_rey = (1.0 - frac) * cd_rey + frac * cd_rey2
        endif
        !
    end


    subroutine getalf(ctxt, is, clift, w, alf, alf_cl, alf_w, stallf)
        !------------------------------------------------------------
        !     Inverse alpha(cl) function
        !     Uses Newton-Raphson iteration to get alf from cl function
        !------------------------------------------------------------
        use i_common, only : Common, show_output
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real A0, ALF, ALF_CL, ALF_W, CDRAG, CD_ALF, CD_REY, CD_W, CLIFT, CLMAX, &
                & CLMIN, CLTEMP, CL_ALF, CL_W, CMOM, CM_AL, CM_W, DALF, DCL_STALL, EPS
        integer IS, ITER, NITER
        real REY, W
        !*** End of declarations inserted by SPAG
        logical stallf
        data niter/10/
        data eps/1.0E-5/
        type (Common), intent(inout) :: ctxt
        !
        stallf = .false.
        !
        !---hhy had to set a0 to first aero section as a0 is now section property
        a0 = ctxt%aerodata(1, 1)
        rey = 0.0
        !
        alf = a0
        do iter = 1, niter
            call getclcdcm(ctxt, is, alf, w, rey, &
                    cltemp, cl_alf, cl_w, &
                    clmax, clmin, dcl_stall, stallf, &
                    cdrag, cd_alf, cd_w, cd_rey, &
                    cmom, cm_al, cm_w)
            !c      if(stallf) go to 20
            dalf = -(cltemp - clift) / cl_alf
            alf = alf + dalf
            alf_cl = 1.0 / cl_alf
            alf_w = -cl_w / cl_alf
            if (abs(dalf)<eps) return
        enddo
        !
        if (show_output) write(*, *) 'getalf: alpha(ctxt%cl) function inversion failed'
        !      write(*,*) 'is,clift  ',is,clift
        !      write(*,*) 'abs(dalf) ',abs(dalf)
        !      write(*,*) 'cl_alf    ',cl_alf
        !
    end
    ! getalf



    !*************************************************************************
    !  Basic aero section properties functions
    !  These routines implement a functional representation of the
    !  blade section aero properties (cl,cd,cm) vs alfa
    !*************************************************************************

    subroutine clcdcm(ctxt, alf, w, rey, &
            clift, cl_alf, cl_w, stallf, &
            cdrag, cd_alf, cd_w, cd_rey, &
            cmom, cm_al, cm_w, &
            polar, use_corrections, mcrit)
        ! NOTE: All Reynolds Nr and compressibility effects have been REMOVED from this function!
        ! The user is responsible for ensuring that the aerodynamic sections are analyzed at the
        ! correct local Reynolds and Mach numbers.
        use i_common, only : Common, show_output
        type(Common) :: ctxt
        logical :: stallf
        integer :: n_points
        real :: alf, w, rey, clift, cl_alf, cl_w, cdrag, cd_alf, cd_w, cd_rey, cmom, cm_al, cm_w, &
                deltas(4), new_data(3), deriv(3)
        real :: polar(:, :)
        logical, optional :: use_corrections
        real, optional :: mcrit
        real :: cdmfactor, clmfactor, mexp, cdmdd, cdmstall, msq, msq_w, pg, pg_w, mach, mach_w, &
                cla, cla_alf, cla_w, clmax, clmin, dmstall, clmaxm, clminm, dmdd, critmach, critmach_alf, &
                critmach_w, cdc, cdc_alf, cdc_w, fac, fac_w, cldmin

        integer :: i_below, i_above
        real :: f

        ! Ensure angle of attack is always between -180 and +180 degrees
        if (alf < -180.) then
            alf = alf + 360.
        elseif (alf > 180.) then
            alf = alf - 360.
        end if

        ! Find the indices of the angles of attack in the polar just below and just above the specified one
        i_below = maxloc(polar(:, 1), 1, polar(:, 1) <= alf)
        i_above = minloc(polar(:, 1), 1, polar(:, 1) >= alf)

        ! Compute delta values from alpha(i_below) to alpha(i_above)
        deltas = pack(polar(i_above, :), .true.) - pack(polar(i_below, :), .true.)

        ! Interpolation factor
        f = (alf - polar(i_below, 1)) / deltas(1)

        ! Compute interpolate data and local slopes
        new_data = pack(polar(i_below, 2:), .true.) + f * deltas(2:)
        deriv = deltas(2:) / deltas(1)

        ! Store results in proper places
        clift = new_data(1)
        cdrag = new_data(2)
        cmom  = new_data(3)

        cl_alf = deriv(1)
        cd_alf = deriv(2)
        cm_al  = deriv(3)

        cd_rey = 0.
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Optional Compressibility Corrections !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if (.not. present(use_corrections) .or. .not. use_corrections) then
            cl_w = 0.
            cd_w = 0.
            cm_w = 0.

        else
            if (.not. present(mcrit)) then
                if (show_output) write(*, *) 'clfunc: Compressibility corrections used but no mcrit given. Assuming 0.6'
                mcrit = 0.6
            end if
            ! TODO: Actually calculate cldmin
            cldmin = 0.

            cdmfactor = 10.0
            clmfactor = 0.25
            mexp = 3.0
            cdmdd = 0.0020
            cdmstall = 0.1000
            !
            !---- Prandtl-Glauert compressibility factor
            msq = w * w * ctxt%vel**2 / ctxt%vso**2
            msq_w = 2.0 * w * ctxt%vel**2 / ctxt%vso**2
            if(msq >= 1.0) then
                if (show_output) write(*, *) 'clfunc: Local Mach number limited to 0.99, was ', msq
                msq = 0.99
                msq_w = 0.
            endif
            pg = 1.0 / sqrt(1.0 - msq)
            pg_w = 0.5 * msq_w * pg**3
            !
            !---- Mach number and dependence on velocity
            mach = sqrt(msq)
            mach_w = 0.0
            if (mach/=0.0) mach_w = 0.5 * msq_w / mach
            !
            !
            !------------------------------------------------------------
            !--- Generate cl from dcl/dAlpha and Prandtl-Glauert scaling
            cla = clift * pg
            cla_alf = cl_alf * pg
            cla_w = clift * pg_w
            !
            !--- Effective cLmax is limited by Mach effects
            !    reduces cLmax to match the cl of onset of serious compressible drag
            clmax = maxval(polar(:, 2))
            clmin = minval(polar(:, 2))
            dmstall = (cdmstall / cdmfactor)**(1.0 / mexp)
            clmaxm = max(0.0, (mcrit + dmstall - mach) / clmfactor) + cldmin
            clmax = min(clmax, clmaxm)
            clminm = min(0.0, -(mcrit + dmstall - mach) / clmfactor) + cldmin
            clmin = max(clmin, clminm)

            !------------------------------------------------------------
            !--- cm from cmcon and Prandtl-Glauert scaling
            cmom = pg * cmom
            cm_al = 0.0
            cm_w = pg_w * cmom

            !--- Compressibility drag (accounts for drag rise above Mcrit with cl effects
            !    cdc is a function of a scaling factor*(m-Mcrit(cl))**mexp
            !    dmdd is the Mach difference corresponding to cd rise of cdmdd at mcrit
            dmdd = (cdmdd / cdmfactor)**(1.0 / mexp)
            critmach = mcrit - clmfactor * abs(clift - cldmin) - dmdd
            critmach_alf = -clmfactor * abs(cl_alf)
            critmach_w = -clmfactor * abs(cl_w)
            if (mach<critmach) then
                cdc = 0.0
                cdc_alf = 0.0
                cdc_w = 0.0
            else
                cdc = cdmfactor * (mach - critmach)**mexp
                cdc_w = mexp * mach_w * cdc / mach - mexp * critmach_w * cdc / critmach
                cdc_alf = -mexp * critmach_alf * cdc / critmach
            endif
            !      write(*,*) 'critmach,mach ',critmach,mach
            !      write(*,*) 'cdc,cdc_w,cdc_alf ',cdc,cdc_w,cdc_alf
            !
            fac = 1.0
            fac_w = 0.0
            !--- Although test data does not show profile drag increases due to Mach #
            !    you could use something like this to add increase drag by Prandtl-Glauert
            !    (or any function you choose)
            !c      fac   = pg
            !c      fac_w = pg_w
            !--- Total drag terms
            cdrag = fac * cdrag  + cdc
            cd_alf = fac * cd_alf  + cdc_alf
            cd_w = fac * cd_w + fac_w * cdrag + cdc_w
        end if
    end
    ! clcdcm

end
