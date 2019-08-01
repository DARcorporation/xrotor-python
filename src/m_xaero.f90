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
contains
    subroutine setiaero(ctxt)
        !--------------------------------------------------
        !     Sets up indices referring to aero section for
        !     each radial station
        !--------------------------------------------------
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !
        !--- Find lower index of aero data sections xiaero(n) bounding xi(is)
        do i = 1, ctxt%ii
            ctxt%iaero(i) = 1
            do n = 1, ctxt%naero
                if(ctxt%xiaero(n) <= ctxt%xi(i)) then
                    ctxt%iaero(i) = n
                endif
            end do
        end do
        return
    end

    subroutine getaero(ctxt, n, xisect, a0, clmax, clmin, &
            dclda, dclda_stall, dcl_stall, &
            cdmin, cldmin, dcdcl2, cmcon, mcrit, reref, rexp)
        !---------------------------------------------
        !     Gets aero data from stored section array
        !---------------------------------------------
        use m_common, only: Common, show_output
        implicit real(m)
        type(Common), intent(inout) :: ctxt
        !
        if(n < 1 .or. n > ctxt%naero) then
            if (show_output) write(*, *) 'Error: index of aero section out of bounds'
            return
        endif
        !
        a0 = ctxt%aerodata(1, n)
        clmax = ctxt%aerodata(2, n)
        clmin = ctxt%aerodata(3, n)
        dclda = ctxt%aerodata(4, n)
        dclda_stall = ctxt%aerodata(5, n)
        dcl_stall = ctxt%aerodata(6, n)
        cdmin = ctxt%aerodata(7, n)
        cldmin = ctxt%aerodata(8, n)
        dcdcl2 = ctxt%aerodata(9, n)
        cmcon = ctxt%aerodata(10, n)
        reref = ctxt%aerodata(11, n)
        rexp = ctxt%aerodata(12, n)
        mcrit = ctxt%aerodata(13, n)
        xisect = ctxt%xiaero(n)
        !
        return
    end

    subroutine putaero(ctxt, n, xisect, a0, clmax, clmin, &
            dclda, dclda_stall, dcl_stall, &
            cdmin, cldmin, dcdcl2, cmcon, mcrit, reref, rexp)
        !--------------------------------------------------------
        !     Puts aero data into stored section array at index n
        !--------------------------------------------------------
        use m_common, only: Common, show_output
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !
        if(n > nax) then
            if (show_output) write(*, *) 'Too many aero sections defined...'
            return
        endif
        !
        ctxt%aerodata(1, n) = a0
        ctxt%aerodata(2, n) = clmax
        ctxt%aerodata(3, n) = clmin
        ctxt%aerodata(4, n) = dclda
        ctxt%aerodata(5, n) = dclda_stall
        ctxt%aerodata(6, n) = dcl_stall
        ctxt%aerodata(7, n) = cdmin
        ctxt%aerodata(8, n) = cldmin
        ctxt%aerodata(9, n) = dcdcl2
        ctxt%aerodata(10, n) = cmcon
        ctxt%aerodata(11, n) = reref
        ctxt%aerodata(12, n) = rexp
        ctxt%aerodata(13, n) = mcrit
        ctxt%xiaero(n) = xisect
        !
        return
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
        use m_common, only: Common, show_output
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        logical stallf, stallf2
        !
        !--- Check for installed aero data section index
        n = ctxt%iaero(is)
        if(n < 1 .or. n > ctxt%naero) then
            !
            if(ctxt%naero > 1) then
                !--- Find lower index of aero data sections xiaero(n) bounding xi(is)
                do n = 1, ctxt%naero
                    if(ctxt%xiaero(n) <= ctxt%xi(is)) then
                        !c          write(*,*) 'getcl iaero= ',n,' is= ',is,xiaero(n),xi(is)
                        ctxt%iaero(is) = n
                    else
                        go to 10
                    endif
                end do
                if (show_output) write(*, *) 'aero section not found for station ', ctxt%xi(is)
            endif
            !
            n = 1
            ctxt%iaero(is) = n
        endif
        !
        !--- Get section aero data from stored section array
        10   a0 = ctxt%aerodata(1, n)
        clmax = ctxt%aerodata(2, n)
        clmin = ctxt%aerodata(3, n)
        dclda = ctxt%aerodata(4, n)
        dclda_stall = ctxt%aerodata(5, n)
        dcl_stall = ctxt%aerodata(6, n)
        cdmin = ctxt%aerodata(7, n)
        cldmin = ctxt%aerodata(8, n)
        dcdcl2 = ctxt%aerodata(9, n)
        cmcon = ctxt%aerodata(10, n)
        reref = ctxt%aerodata(11, n)
        rexp = ctxt%aerodata(12, n)
        mcrit = ctxt%aerodata(13, n)
        xisect1 = ctxt%xiaero(n)
        !--- Get data for inner bounding aero section
        call clcdcm(ctxt, alf, w, rey, &
                clift, cl_alf, cl_w, stallf, &
                cdrag, cd_alf, cd_w, cd_rey, &
                cmom, cm_al, cm_w, &
                a0, clmax, clmin, dclda, dclda_stall, dcl_stall, &
                cdmin, cldmin, dcdcl2, cmcon, mcrit, reref, rexp)
        !
        !--- Check for another bounding section, if not we are done,
        !    if we have another section linearly interpolate data to station is
        if(n < ctxt%naero) then
            xisect2 = ctxt%xiaero(n + 1)
            frac = (ctxt%xi(is) - xisect1) / (xisect2 - xisect1)
            if(frac <= 0.0 .or. frac > 1.0) then
                !c         write(*,*) 'cl n,is,xi,frac = ',n,is,xi(is),frac
            endif
            !
            a0 = ctxt%aerodata(1, n + 1)
            clmax2 = ctxt%aerodata(2, n + 1)
            clmin2 = ctxt%aerodata(3, n + 1)
            dclda = ctxt%aerodata(4, n + 1)
            dclda_stall = ctxt%aerodata(5, n + 1)
            dcl_stall2 = ctxt%aerodata(6, n + 1)
            cdmin = ctxt%aerodata(7, n + 1)
            cldmin = ctxt%aerodata(8, n + 1)
            dcdcl2 = ctxt%aerodata(9, n + 1)
            cmcon = ctxt%aerodata(10, n + 1)
            reref = ctxt%aerodata(11, n + 1)
            rexp = ctxt%aerodata(12, n + 1)
            mcrit = ctxt%aerodata(13, n + 1)
            !--- Get data for outer bounding aero section
            call clcdcm(ctxt, alf, w, rey, &
                    clift2, cl_alf2, cl_w2, stallf2, &
                    cdrag2, cd_alf2, cd_w2, cd_rey2, &
                    cmom2, cm_al2, cm_w2, &
                    a0, clmax2, clmin2, dclda, dclda_stall, dcl_stall2, &
                    cdmin, cldmin, dcdcl2, cmcon, mcrit, reref, rexp)
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
        return
    end


    subroutine getalf(ctxt, is, clift, w, alf, alf_cl, alf_w, stallf)
        !------------------------------------------------------------
        !     Inverse alpha(cl) function
        !     Uses Newton-Raphson iteration to get alf from cl function
        !------------------------------------------------------------
        use m_common, only: Common, show_output
        implicit real (m)
        logical stallf
        data niter / 10 /
        data eps   / 1.0e-5 /
        type(Common), intent(inout) :: ctxt
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
            if(abs(dalf) < eps) return
        end do
        !
        if (show_output) write(*, *) 'getalf: alpha(ctxt%cl) function inversion failed'
        !      write(*,*) 'is,clift  ',is,clift
        !      write(*,*) 'abs(dalf) ',abs(dalf)
        !      write(*,*) 'cl_alf    ',cl_alf
        !
        return
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
            a0, clmax, clmin, dclda, dclda_stall, dcl_stall, &
            cdmin, cldmin, dcdcl2, cmcon, mcrit, reref, rexp)
        !------------------------------------------------------------
        !     cl(alpha) function
        !     Note that in addition to setting clift and its derivatives
        !     clmax and clmin (+ and - stall cl's) are set in this routine
        !     In the compressible range the stall cl is reduced by a factor
        !     proportional to Mcrit-Mach.  Stall limiting for compressible
        !     cases begins when the compressible drag added cdc > cdMstall
        !------------------------------------------------------------
        !     cd(alpha) function - presently cd is assumed to be a sum
        !     of profile drag + stall drag + compressibility drag
        !     In the linear lift range drag is cd0 + quadratic function of cl-cldmin
        !     In + or - stall an additional drag is added that is proportional
        !     to the extent of lift reduction from the linear lift value.
        !     Compressible drag is based on adding drag proportional to
        !     (Mach-Mcrit_eff)^mexp
        !------------------------------------------------------------
        !     cm(alpha) function - presently cm is assumed constant,
        !     varying only with Mach by Prandtl-Glauert scaling
        !------------------------------------------------------------
        !
        use m_common, only: Common, show_output
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        logical stallf
        double precision ecmin, ecmax
        !
        !---- Factors for compressibility drag model, hhy 10/23/00
        !     Mcrit is set by user
        !     Effective Mcrit is Mcrit_eff = Mcrit - clmfactor*(cl-clDmin) - dmdd
        !     dmdd is the delta Mach to get cd=cdmdd (usually 0.0020)
        !     Compressible drag is cdc = cdmfactor*(Mach-Mcrit_eff)^mexp
        !     cdMstall is the drag at which compressible stall begins
        !
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
        if(mach /= 0.0) mach_w = 0.5 * msq_w / mach
        !
        !
        !------------------------------------------------------------
        !--- Generate cl from dcl/dAlpha and Prandtl-Glauert scaling
        cla = dclda * pg * (alf - a0)
        cla_alf = dclda * pg
        cla_w = dclda * pg_w * (alf - a0)
        !
        !--- Effective cLmax is limited by Mach effects
        !    reduces cLmax to match the cl of onset of serious compressible drag
        clmx = clmax
        clmn = clmin
        dmstall = (cdmstall / cdmfactor)**(1.0 / mexp)
        clmaxm = max(0.0, (mcrit + dmstall - mach) / clmfactor) + cldmin
        clmax = min(clmax, clmaxm)
        clminm = min(0.0, -(mcrit + dmstall - mach) / clmfactor) + cldmin
        clmin = max(clmin, clminm)
        !
        !--- cl limiter function (turns on after +-stall
        ecmax = dexp(min(200.0d0, dble((cla - clmax) / dcl_stall)))
        ecmin = dexp(min(200.0d0, dble((clmin - cla) / dcl_stall)))
        cllim = dcl_stall * dlog((1.0d0 + ecmax) / (1.0d0 + ecmin))
        cllim_cla = ecmax / (1.0 + ecmax) + ecmin / (1.0 + ecmin)
        !
        !      if(cllim > 0.001) then
        !      write(*,999) 'cla,cllim,ecmax,ecmin ',cla,cllim,ecmax,ecmin
        !      endif
        ! 999  format(a,2(1x,f10.6),3(1x,d12.6))
        !
        !--- Subtract off a (nearly unity) fraction of the limited cl function
        !    This sets the dcl/dAlpha in the stalled regions to 1-fstall of that
        !    in the linear lift range
        fstall = dclda_stall / dclda
        clift = cla - (1.0 - fstall) * cllim
        cl_alf = cla_alf - (1.0 - fstall) * cllim_cla * cla_alf
        cl_w = cla_w - (1.0 - fstall) * cllim_cla * cla_w
        !
        stallf = .false.
        if(clift > clmax) stallf = .true.
        if(clift < clmin) stallf = .true.
        !
        !
        !------------------------------------------------------------
        !--- cm from cmcon and Prandtl-Glauert scaling
        cmom = pg * cmcon
        cm_al = 0.0
        cm_w = pg_w * cmcon
        !
        !
        !------------------------------------------------------------
        !--- cd from profile drag, stall drag and compressibility drag
        !
        !---- Reynolds number scaling factor
        if(rey <= 0) then
            rcorr = 1.0
            rcorr_rey = 0.0
        else
            rcorr = (rey / reref)**rexp
            rcorr_rey = rexp / rey
        endif
        !
        !--- In the basic linear lift range drag is a function of lift
        !    cd = cd0 (constant) + quadratic with cl)
        cdrag = (cdmin + dcdcl2 * (clift - cldmin)**2) * rcorr
        cd_alf = (2.0 * dcdcl2 * (clift - cldmin) * cl_alf) * rcorr
        cd_w = (2.0 * dcdcl2 * (clift - cldmin) * cl_w) * rcorr
        cd_rey = cdrag * rcorr_rey
        !
        !--- Post-stall drag added
        fstall = dclda_stall / dclda
        dcdx = (1.0 - fstall) * cllim / (pg * dclda)
        !      write(*,*) 'cla,cllim,fstall,pg,dclda ',cla,cllim,fstall,pg,dclda
        dcd = 2.0 * dcdx**2
        dcd_alf = 4.0 * dcdx * &
                (1.0 - fstall) * cllim_cla * cla_alf / (pg * dclda)
        dcd_w = 4.0 * dcdx * &
                ((1.0 - fstall) * cllim_cla * cla_w / (pg * dclda) - dcd / pg * pg_w)
        !      write(*,*) 'alf,cl,dcd,dcd_alf,dcd_w ',alf,clift,dcd,dcd_alf,dcd_w
        !
        !--- Compressibility drag (accounts for drag rise above Mcrit with cl effects
        !    cdc is a function of a scaling factor*(m-Mcrit(cl))**mexp
        !    dmdd is the Mach difference corresponding to cd rise of cdmdd at mcrit
        dmdd = (cdmdd / cdmfactor)**(1.0 / mexp)
        critmach = mcrit - clmfactor * abs(clift - cldmin) - dmdd
        critmach_alf = -clmfactor * abs(cl_alf)
        critmach_w = -clmfactor * abs(cl_w)
        if(mach < critmach) then
            cdc = 0.0
            cdc_alf = 0.0
            cdc_w = 0.0
        else
            cdc = cdmfactor * (mach - critmach)**mexp
            cdc_w = mexp * mach_w * cdc / mach - mexp * critmach_w * cdc / critmach
            cdc_alf = - mexp * critmach_alf * cdc / critmach
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
        cdrag = fac * cdrag + dcd + cdc
        cd_alf = fac * cd_alf + dcd_alf + cdc_alf
        cd_w = fac * cd_w + fac_w * cdrag + dcd_w + cdc_w
        cd_rey = fac * cd_rey
        !
        return
    end
    ! clcdcm

end module m_xaero