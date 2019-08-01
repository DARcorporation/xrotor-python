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
!
module m_xbend
contains



    subroutine eiload(ctxt, fname1)
        use m_userio, only: asks
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        character*(*) fname1
        dimension xt(ix)
        !----------------------------------------------------
        !     Reads and splines blade structural properties.
        !----------------------------------------------------
        !
        !    xt =    r    radius
        !    w0 = eixx    in-plane stiffness
        !    w1 = eiyy    out-of-plane stiffness
        !    w2 =   ea    extensional stiffness
        !    w3 =   gj    torsional stiffness
        !    w4 =   ek    extensional/torsional cross-stiffness
        !    w5 =    m    mass density / length
        !    w6 =  mxx    pitch axis inertia / length
        !    w7 = xocg    x/c of section cg
        !    w8 = xosc    x/c of section shear center (structural axis)
        !    w9 = rst     structural radius for strain evaluation
        !
        lu = 14
        !
        ctxt%fname = fname1
        if(ctxt%fname(1:1) == ' ') call asks('enter input filename^', ctxt%fname)
        !
        open(lu, file = ctxt%fname, status = 'old', err = 200)
        read(lu, 1000) dummy
        read(lu, 1000) dummy
        read(lu, 1000) dummy
        do it = 1, ix
            read(lu, *, end = 11, err = 210) xt(it), &
                    ctxt%w0(it), ctxt%w1(it), ctxt%w2(it), ctxt%w3(it), ctxt%w4(it), &
                    ctxt%w5(it), ctxt%w6(it), ctxt%w7(it), ctxt%w8(it), ctxt%w9(it)
            xt(it) = xt(it) / ctxt%rad
        end do
        if (show_output) write(*, *) 'eiload: Array overflow.  Too many radial stations.'
        11   continue
        nt = it - 1
        close(lu)
        !
        ! todo: test these
        ctxt%t0(1:nt) = segspl(xt(1:nt), ctxt%w0(1:nt))
        ctxt%t1(1:nt) = segspl(xt(1:nt), ctxt%w1(1:nt))
        ctxt%t2(1:nt) = segspl(xt(1:nt), ctxt%w2(1:nt))
        ctxt%t3(1:nt) = segspl(xt(1:nt), ctxt%w3(1:nt))
        ctxt%t4(1:nt) = segspl(xt(1:nt), ctxt%w4(1:nt))
        ctxt%t5(1:nt) = segspl(xt(1:nt), ctxt%w5(1:nt))
        ctxt%t6(1:nt) = segspl(xt(1:nt), ctxt%w6(1:nt))
        ctxt%t7(1:nt) = segspl(xt(1:nt), ctxt%w7(1:nt))
        ctxt%t8(1:nt) = segspl(xt(1:nt), ctxt%w8(1:nt))
        ctxt%t9(1:nt) = segspl(xt(1:nt), ctxt%w9(1:nt))
        !
        do i = 1, ctxt%ii
            ! todo: test these
            ctxt%eixxb(i) = seval(ctxt%xi(i), ctxt%w0, ctxt%t0, xt)
            ctxt%eiyyb(i) = seval(ctxt%xi(i), ctxt%w1, ctxt%t1, xt)
            ctxt%eab(i) = seval(ctxt%xi(i), ctxt%w2, ctxt%t2, xt)
            ctxt%gjb(i) = seval(ctxt%xi(i), ctxt%w3, ctxt%t3, xt)
            ctxt%ekb(i) = seval(ctxt%xi(i), ctxt%w4, ctxt%t4, xt)
            ctxt%mb(i) = seval(ctxt%xi(i), ctxt%w5, ctxt%t5, xt)
            ctxt%mxxb(i) = seval(ctxt%xi(i), ctxt%w6, ctxt%t6, xt)
            ctxt%xocg(i) = seval(ctxt%xi(i), ctxt%w7, ctxt%t7, xt)
            ctxt%xosc(i) = seval(ctxt%xi(i), ctxt%w8, ctxt%t8, xt)
            ctxt%rstb(i) = seval(ctxt%xi(i), ctxt%w9, ctxt%t9, xt)
        end do
        !
        mass = 0.0
        mrsq = 0.0
        maxx = 0.0
        do i = 1, ctxt%ii
            mass = mass + ctxt%mb(i) * ctxt%rad * ctxt%dxi(i)
            mrsq = mrsq + ctxt%mb(i) * ctxt%rad * ctxt%dxi(i) * (ctxt%xi(i) * ctxt%rad)**2
            maxx = maxx + ctxt%mxxb(i) * ctxt%rad * ctxt%dxi(i)
        end do
        !
        if (show_output) write(*, 3100) mass, maxx, mrsq
        !
        ctxt%lstruc = .true.
        return
        !
        200  if (show_output) write(*, 1010) ctxt%fname(1:32)
        return
        !
        210  if (show_output) write(*, 1020) ctxt%fname(1:32)
        close(lu)
        ctxt%conv = .false.
        return
        !..............................
        1000 format(32a1)
        1010 format(' File  ', a, ' not found'/)
        1020 format(' File  ', a, ' has incompatible format'/&
                ' Loading not completed'/)
        3100 format(/' Blade mass =', g12.4&
                /' Pitch-axis inertia =', g14.5&
                /' Rotational inertia =', g14.5)
    end
    ! eiload


    subroutine stclr(ctxt)
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !
        do i = 1, ctxt%ii
            ctxt%tx(i) = 0.0
            ctxt%ty(i) = 0.0
            ctxt%tz(i) = 0.0
            ctxt%wx(i) = 0.0
            ctxt%wy(i) = 0.0
            ctxt%wz(i) = 0.0
            ctxt%momx(i) = 0.0
            ctxt%momy(i) = 0.0
            ctxt%momz(i) = 0.0
            ctxt%shrx(i) = 0.0
            ctxt%shry(i) = 0.0
            ctxt%shrz(i) = 0.0
        end do
        !
        return
    end
    ! stclr


    subroutine mclr(ctxt)
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !
        do i = 1, ctxt%ii
            ctxt%mb(i) = 0.0
            ctxt%mxxb(i) = 0.0
            ctxt%ekb(i) = 0.0
        end do
        !
        return
    end
    ! mclr



    subroutine stload(ctxt)
        use m_xoper, only: cscalc
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !-----------------------------------------------------------
        !     Calculates force and moment loadings along blade.
        ! hhy 3/99 a local x',y',z' system is assumed that is tilted by
        !          a rake angle about the z axis, the y' axis runs out
        !          the blade axis.  z' = z, x',y' are rotated from x,y by rake angle
        !
        !     The ()_a and ()_b sensitivities below should be set
        !     only if beta(.) and the aero solution cl(.), gam(.),
        !     etc. are updated every iteration in stcalc.
        !-----------------------------------------------------------
        !
        fx = 0.0
        fy = 0.0
        fz = 0.0
        sinr = sin(ctxt%rake)
        cosr = cos(ctxt%rake)
        !
        do i = 1, ctxt%ii
            !
            dxii = ctxt%dxi(i) / cosr
            !
            call cscalc(ctxt, i, utot, wa, wt, &
                    vt, vt_adw, &
                    va, va_adw, &
                    vd, vd_adw, &
                    ci, ci_adv, ci_vt, &
                    si, si_va, &
                    w, w_adv, w_vt, w_va, &
                    phi, p_adv, p_vt, p_va)
            !
            wsq = w * w
            !
            sinb = sin(ctxt%beta(i))
            cosb = cos(ctxt%beta(i))
            !
            txa = 0.5 * (ctxt%tx(i) + ctxt%tx(i + 1))
            tza = 0.5 * (ctxt%tz(i) + ctxt%tz(i + 1))
            wza = 0.5 * (ctxt%wz(i) + ctxt%wz(i + 1))
            !
            !--- Aerodynamic lift and drag forces (act in blade normal plane)
            flift = 0.5 * wsq * ctxt%ch(i) * ctxt%cl(i)
            fdrag = 0.5 * wsq * ctxt%ch(i) * ctxt%cd(i)
            !
            flift_a = 0.0
            !cc     flift_a = 0.5*wsq*ch(i)*dclda
            !--- blade resolved force components
            fxa = -ci / w * flift + si / w * fdrag
            fya = 0.0
            fza = si / w * flift + ci / w * fdrag
            !
            !***************************************
            !--- Integrate blade aerodynamic forces
            fx = fx + cosr * dxii * fxa
            fy = fy + sinr * dxii * fxa
            fz = fz + dxii * fza
            !***************************************
            !
            !
            !--- Centrifugal forces (y direction)
            fcent = 0.0
            if(ctxt%lstruc) fcent = ctxt%mb(i) * ctxt%xi(i) / ctxt%adv**2&
                    / (rho * rad**2)
            !--- blade resolved force components
            fxc = -sinr * fcent
            fyc = cosr * fcent
            fzc = 0.0
            !
            !--- Assemble force loadings
            ctxt%px(i) = fxa + fxc + fyc * tza * cosr
            ctxt%py(i) = fya + fyc
            ctxt%pz(i) = fza + fzc + fyc * (wza / ctxt%xi(i) - txa)
            ctxt%px_ty(i) = -flift_a * ci / w
            ctxt%px_tz(i) = fyc * cosr
            ctxt%pz_tx(i) = -fyc
            ctxt%pz_ty(i) = flift_a * si / w
            ctxt%pz_wz(i) = fyc / ctxt%xi(i)
            !
            !        px(i) =  -flift*ci/w + fdrag*si/w + fcent* tza
            !        pz(i) =   flift*si/w + fdrag*ci/w + fcent*(wza/xi(i) - txa)
            !        py(i) =                             fcent
            !        px_ty(i) = -flift_a*ci/w
            !        px_tz(i) =  fcent
            !        pz_tx(i) = -fcent
            !        pz_ty(i) =  flift_a*si/w
            !        pz_wz(i) =  fcent/xi(i)
            !
            !--- Define moment components
            if(ctxt%lstruc) then
                mcent = sinb * (ctxt%xocg(i) - ctxt%xosc(i)) * ctxt%ch(i) * ctxt%mb(i) * ctxt%xi(i) / ctxt%adv**2&
                        / (rho * rad**2)
                mprec = -sinb * cosb * ctxt%mxxb(i) / ctxt%adv**2&
                        / (rho * rad**4)
                maero = 0.5 * wsq * (ctxt%cl(i) * (ctxt%xosc(i) - 0.25) + ctxt%cm(i)) * ctxt%ch(i)**2
            else
                mcent = 0.0
                mprec = 0.0
                maero = 0.5 * wsq * (ctxt%cl(i) * (0.40 - 0.25) + ctxt%cm(i)) * ctxt%ch(i)**2
            endif
            !
            mcent_b = 0.0
            mprec_b = 0.0
            maero_a = 0.0
            !cc     mcent_b = cosb * (xocg(i)-xosc(i))*ch(i) * mb(i)*xi(i)/adv**2
            !cc  &          / (rho * rad**2)
            !cc     mprec_b = -(cosb**2 - sinb**2) * mxxb(i)/adv**2
            !cc  &          / (rho * rad**4)
            !cc     maero_a = 0.5*wsq*(dclda*(xosc(i)-0.25)     ) * ch(i)**2
            !
            !--- Assemble imposed moments
            ctxt%mx(i) = 0.0
            ctxt%my(i) = maero + mprec
            ctxt%mz(i) = mcent + mprec * (wza / ctxt%xi(i) - txa)
            !
            ctxt%my_ty(i) = maero_a + mprec_b * (wza / ctxt%xi(i) - txa)
            ctxt%mz_ty(i) = mcent_b
            ctxt%mz_tx(i) = - mprec
            ctxt%mz_wz(i) = mprec / ctxt%xi(i)
            !
        end do
        !
        !--- Print the blade aerodynamic forces
        if (show_output)then
            write(*, 20) fx * ctxt%rho * ctxt%vel**2 * ctxt%rad**2, &
                    fy * ctxt%rho * ctxt%vel**2 * ctxt%rad**2, &
                    fz * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
            20   format(/'Blade aerodynamic forces:', &
                    /' fx (axial)      = ', f12.6, &
                    /' fy (radial)     = ', f12.6, &
                    /' fz (tangential) = ', f12.6)
        end if
        !
        return
    end
    ! stload



    subroutine stcalc(ctxt)
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !------------------------------------------------------------
        !     Updates resultants and deflections along blade.
        !     Uses current loading distributions px,py,pz, mx,my,mz.
        !------------------------------------------------------------
        real aa(12, 12, ixp), bb(12, 12, ixp), cc(12, 12, ixp), rr(12, ixp)
        real rrlim(12), rlxr(12)
        !
        eps = 1.0e-5
        !
        rrlim(1) = 0.10
        rrlim(2) = 0.10
        rrlim(3) = 0.10
        !
        rrlim(4) = 0.2 / ctxt%adv**2
        rrlim(5) = 0.5 / ctxt%adv**2
        rrlim(6) = 0.2 / ctxt%adv**2
        !
        rrlim(7) = 0.2 / ctxt%adv**2
        rrlim(8) = 20.0 / ctxt%adv**2
        rrlim(9) = 0.2 / ctxt%adv**2
        !
        rrlim(10) = 0.10
        rrlim(11) = 0.01
        rrlim(12) = 0.10
        !
        if (show_output) write(*, *)
        !
        !---- Newton iteration loop
        do iter = 1, 10
            !
            call stload(ctxt)
            !
            do i = 1, ctxt%ii + 1
                do k = 1, 12
                    do j = 1, 12
                        aa(k, j, i) = 0.0
                        bb(k, j, i) = 0.0
                        cc(k, j, i) = 0.0
                    end do
                    rr(k, i) = 0.0
                end do
            end do
            !
            !
            !---- fix deflection angles at root
            i = 1
            aa(1, 1, i) = 1.0
            aa(2, 2, i) = 1.0
            aa(3, 3, i) = 1.0
            !
            aa(10, 10, i) = 1.0
            aa(11, 11, i) = 1.0
            aa(12, 12, i) = 1.0
            !
            !---- non-dimensionalizing factors
            earef = ctxt%rho * ctxt%vel**2 * ctxt%rad**2
            ekref = ctxt%rho * ctxt%vel**2 * ctxt%rad**3
            eiref = ctxt%rho * ctxt%vel**2 * ctxt%rad**4
            !
            cosr = cos(ctxt%rake)
            !
            !---- go over radial intervals
            do i = 1, ctxt%ii
                !
                dxii = ctxt%dxi(i) / cosr
                !
                sx2 = ctxt%shrx(i + 1)
                sy2 = ctxt%shry(i + 1)
                sz2 = ctxt%shrz(i + 1)
                !
                sx1 = ctxt%shrx(i)
                sy1 = ctxt%shry(i)
                sz1 = ctxt%shrz(i)
                !
                mx2 = ctxt%momx(i + 1)
                my2 = ctxt%momy(i + 1)
                mz2 = ctxt%momz(i + 1)
                !
                mx1 = ctxt%momx(i)
                my1 = ctxt%momy(i)
                mz1 = ctxt%momz(i)
                !
                tx2 = ctxt%tx(i + 1)
                ty2 = ctxt%ty(i + 1)
                tz2 = ctxt%tz(i + 1)
                !
                tx1 = ctxt%tx(i)
                ty1 = ctxt%ty(i)
                tz1 = ctxt%tz(i)
                !
                wx2 = ctxt%wx(i + 1)
                wy2 = ctxt%wy(i + 1)
                wz2 = ctxt%wz(i + 1)
                !
                wx1 = ctxt%wx(i)
                wy1 = ctxt%wy(i)
                wz1 = ctxt%wz(i)
                !
                !
                !        px_ty(i) = -flift_a*ci/w
                !        px_tz(i) =  fcent
                !        pz_tx(i) = -fcent
                !        pz_ty(i) =  flift_a*si/w
                !        pz_wz(i) =  fcent/xi(i)
                !
                !        my_ty(i) = maero_a + mprec_b*(wza/xi(i) - txa)
                !        mz_ty(i) = mcent_b
                !        mz_tx(i) =         - mprec
                !        mz_wz(i) =           mprec/xi(i)
                !

                !------ x-moment
                rr(4, i) = mx2 - mx1&
                        - (my2 + my1) * 0.5 * (tz2 - tz1)&
                        + (mz2 + mz1) * 0.5 * (ty2 - ty1)&
                        - ((sz2 + sz1) * 0.5 - ctxt%mx(i)) * dxii
                !
                aa(4, 2, i) = -(mz2 + mz1) * 0.5
                aa(4, 3, i) = (my2 + my1) * 0.5
                aa(4, 4, i) = -1.0
                aa(4, 5, i) = -0.5 * (tz2 - tz1)
                aa(4, 6, i) = 0.5 * (ty2 - ty1)
                aa(4, 9, i) = -0.5 * dxii
                cc(4, 2, i) = (mz2 + mz1) * 0.5
                cc(4, 3, i) = -(my2 + my1) * 0.5
                cc(4, 4, i) = 1.0
                cc(4, 5, i) = -0.5 * (tz2 - tz1)
                cc(4, 6, i) = 0.5 * (ty2 - ty1)
                cc(4, 9, i) = -0.5 * dxii
                !
                !------ y-moment
                rr(5, i) = my2 - my1&
                        + (mx2 + mx1) * 0.5 * (tz2 - tz1)&
                        - (mz2 + mz1) * 0.5 * (tx2 - tx1)&
                        + (ctxt%my(i)) * dxii
                aa(5, 1, i) = (mz2 + mz1) * 0.5
                aa(5, 2, i) = ctxt%my_ty(i) * dxii * 0.5
                aa(5, 3, i) = -(mx2 + mx1) * 0.5
                aa(5, 4, i) = 0.5 * (tz2 - tz1)
                aa(5, 5, i) = -1.0
                aa(5, 6, i) = -0.5 * (tx2 - tx1)
                cc(5, 1, i) = -(mz2 + mz1) * 0.5
                cc(5, 2, i) = ctxt%my_ty(i) * dxii * 0.5
                cc(5, 3, i) = (mx2 + mx1) * 0.5
                cc(5, 4, i) = 0.5 * (tz2 - tz1)
                cc(5, 5, i) = 1.0
                cc(5, 6, i) = -0.5 * (tx2 - tx1)
                !
                !------ z-moment
                rr(6, i) = mz2 - mz1&
                        + (my2 + my1) * 0.5 * (tx2 - tx1)&
                        - (mx2 + mx1) * 0.5 * (ty2 - ty1)&
                        + ((sx2 + sx1) * 0.5 + ctxt%mz(i)) * dxii
                aa(6, 1, i) = -(my2 + my1) * 0.5
                aa(6, 2, i) = (mx2 + mx1) * 0.5 + ctxt%mz_ty(i) * dxii * 0.5
                aa(6, 3, i) = ctxt%mz_tx(i) * dxii * 0.5
                aa(6, 4, i) = -0.5 * (ty2 - ty1)
                aa(6, 5, i) = 0.5 * (tx2 - tx1)
                aa(6, 6, i) = -1.0
                aa(6, 7, i) = 0.5 * dxii
                aa(6, 12, i) = + ctxt%mz_wz(i) * dxii * 0.5
                cc(6, 1, i) = (my2 + my1) * 0.5
                cc(6, 2, i) = -(mx2 + mx1) * 0.5 + ctxt%mz_ty(i) * dxii * 0.5
                cc(6, 3, i) = ctxt%mz_tx(i) * dxii * 0.5
                cc(6, 4, i) = -0.5 * (ty2 - ty1)
                cc(6, 5, i) = 0.5 * (tx2 - tx1)
                cc(6, 6, i) = 1.0
                cc(6, 7, i) = 0.5 * dxii
                cc(6, 12, i) = + ctxt%mz_wz(i) * dxii * 0.5
                !
                !
                !------ x-shear
                rr(7, i) = sx2 - sx1&
                        + (sy2 + sy1) * 0.5 * (tz2 - tz1)&
                        + (sz2 + sz1) * 0.5 * (ty2 - ty1) - ctxt%px(i) * dxii
                !
                aa(7, 2, i) = -(sz2 + sz1) * 0.5 - ctxt%px_ty(i) * dxii * 0.5
                aa(7, 3, i) = -(sy2 + sy1) * 0.5 - ctxt%px_tz(i) * dxii * 0.5
                aa(7, 7, i) = -1.0
                aa(7, 8, i) = 0.5 * (tz2 - tz1)
                aa(7, 9, i) = 0.5 * (ty2 - ty1)
                cc(7, 2, i) = (sz2 + sz1) * 0.5 - ctxt%px_ty(i) * dxii * 0.5
                cc(7, 3, i) = (sy2 + sy1) * 0.5 - ctxt%px_tz(i) * dxii * 0.5
                cc(7, 7, i) = 1.0
                cc(7, 8, i) = 0.5 * (tz2 - tz1)
                cc(7, 9, i) = 0.5 * (ty2 - ty1)
                !
                !------ y-shear
                rr(8, i) = sy2 - sy1&
                        - (sx2 + sx1) * 0.5 * (tz2 - tz1)&
                        + (sz2 + sz1) * 0.5 * (tx2 - tx1) + ctxt%py(i) * dxii
                aa(8, 1, i) = (sz2 + sz1) * 0.5
                aa(8, 3, i) = -(sx2 + sx1) * 0.5
                aa(8, 7, i) = -0.5 * (tz2 - tz1)
                aa(8, 8, i) = -1.0
                aa(8, 9, i) = 0.5 * (tx2 - tx1)
                cc(8, 1, i) = -(sz2 + sz1) * 0.5
                cc(8, 3, i) = (sx2 + sx1) * 0.5
                cc(8, 7, i) = -0.5 * (tz2 - tz1)
                cc(8, 8, i) = 1.0
                cc(8, 9, i) = 0.5 * (tx2 - tx1)
                !
                !------ z-shear
                rr(9, i) = sz2 - sz1&
                        - (sy2 + sy1) * 0.5 * (tx2 - tx1)&
                        - (sx2 + sx1) * 0.5 * (ty2 - ty1) - ctxt%pz(i) * dxii
                !
                aa(9, 1, i) = (sy2 + sy1) * 0.5 - ctxt%pz_tx(i) * dxii * 0.5
                aa(9, 2, i) = (sx2 + sx1) * 0.5 - ctxt%pz_ty(i) * dxii * 0.5
                aa(9, 7, i) = -0.5 * (ty2 - ty1)
                aa(9, 8, i) = -0.5 * (tx2 - tx1)
                aa(9, 9, i) = -1.0
                aa(9, 12, i) = - ctxt%pz_wz(i) * dxii * 0.5
                cc(9, 1, i) = -(sy2 + sy1) * 0.5 - ctxt%pz_tx(i) * dxii * 0.5
                cc(9, 2, i) = -(sx2 + sx1) * 0.5 - ctxt%pz_ty(i) * dxii * 0.5
                cc(9, 7, i) = -0.5 * (ty2 - ty1)
                cc(9, 8, i) = -0.5 * (tx2 - tx1)
                cc(9, 9, i) = 1.0
                cc(9, 12, i) = - ctxt%pz_wz(i) * dxii * 0.5
                !
                !
                sb = sin(ctxt%beta(i))
                cb = cos(ctxt%beta(i))
                !
                if(ctxt%lstruc) then
                    gj = ctxt%gjb(i) / eiref
                    ek = ctxt%ekb(i) / ekref
                    ea = ctxt%eab(i) / earef
                    !
                    eizz = (ctxt%eixxb(i) * cb**2 + ctxt%eiyyb(i) * sb**2) / eiref
                    eixx = (ctxt%eixxb(i) * sb**2 + ctxt%eiyyb(i) * cb**2) / eiref
                    eixz = (ctxt%eixxb(i) * sb * cb - ctxt%eiyyb(i) * sb * cb) / eiref
                    !
                    eizz_b = (-ctxt%eixxb(i) + ctxt%eiyyb(i)) * 2.0 * sb * cb / eiref
                    eixx_b = (ctxt%eixxb(i) - ctxt%eiyyb(i)) * 2.0 * sb * cb / eiref
                    eixz_b = (ctxt%eixxb(i) - ctxt%eiyyb(i)) * (cb * cb - sb * sb) / eiref
                else
                    eibig = 1.0e+8
                    !
                    gj = eibig
                    ek = 0.0
                    ea = eibig
                    !
                    eizz = eibig
                    eixx = eibig
                    eixz = 0.0
                    !
                    eizz_b = 0.0
                    eixx_b = 0.0
                    eixz_b = 0.0
                endif
                !
                momxd = (ctxt%momx(i) + ctxt%momx(i + 1)) * 0.5 * dxii
                momyd = (ctxt%momy(i) + ctxt%momy(i + 1)) * 0.5 * dxii
                momzd = (ctxt%momz(i) + ctxt%momz(i + 1)) * 0.5 * dxii
                !
                !
                !------ x-deflection angle
                rr(1, i + 1) = eizz * (tx2 - tx1)&
                        - eixz * (tz2 - tz1) - (mx2 + mx1) * 0.5 * dxii
                !
                bb(1, 1, i + 1) = -eizz
                bb(1, 2, i + 1) = eizz_b * (tx2 - tx1) * 0.5&
                        - eixz_b * (tz2 - tz1) * 0.5
                bb(1, 3, i + 1) = eixz
                bb(1, 4, i + 1) = -0.5 * dxii
                aa(1, 1, i + 1) = eizz
                aa(1, 2, i + 1) = eizz_b * (tx2 - tx1) * 0.5&
                        - eixz_b * (tz2 - tz1) * 0.5
                aa(1, 3, i + 1) = -eixz
                aa(1, 4, i + 1) = -0.5 * dxii
                !
                !------ y-deflection angle (twist)
                rr(2, i + 1) = gj * (ty2 - ty1) - (my2 + my1) * 0.5 * dxii&
                        - ek * (wy2 - wy1)
                !
                bb(2, 2, i + 1) = -gj
                bb(2, 5, i + 1) = -0.5 * dxii
                bb(2, 11, i + 1) = ek
                aa(2, 2, i + 1) = gj
                aa(2, 5, i + 1) = -0.5 * dxii
                aa(2, 11, i + 1) = -ek
                !
                !------ z-deflection angle
                rr(3, i + 1) = eixx * (tz2 - tz1)&
                        - eixz * (tx2 - tx1) - (mz2 + mz1) * 0.5 * dxii
                !
                bb(3, 1, i + 1) = eixz
                bb(3, 2, i + 1) = eixx_b * (tz2 - tz1)&
                        - eixz_b * (tx2 - tx1)
                bb(3, 3, i + 1) = -eixx
                bb(3, 6, i + 1) = -0.5 * dxii
                aa(3, 1, i + 1) = -eixz
                aa(3, 2, i + 1) = eixx_b * (tz2 - tz1)&
                        - eixz_b * (tx2 - tx1)
                aa(3, 3, i + 1) = eixx
                aa(3, 6, i + 1) = -0.5 * dxii
                !
                !
                !------ x-deflection
                rr(10, i + 1) = -wx2 + wx1 + (tz2 + tz1) * 0.5 * dxii
                bb(10, 3, i + 1) = 0.5 * dxii
                bb(10, 10, i + 1) = 1.0
                aa(10, 3, i + 1) = 0.5 * dxii
                aa(10, 10, i + 1) = -1.0
                !
                !------ y-deflection
                rr(11, i + 1) = -wy2 + wy1 + (sy2 + sy1) * 0.5 * dxii / ea
                bb(11, 5, i + 1) = 0.5 * dxii / ea
                bb(11, 11, i + 1) = 1.0
                aa(11, 5, i + 1) = 0.5 * dxii / ea
                aa(11, 11, i + 1) = -1.0
                !
                !------ z-deflection
                rr(12, i + 1) = -wz2 + wz1 + (tx2 + tx1) * 0.5 * dxii
                bb(12, 1, i + 1) = 0.5 * dxii
                bb(12, 12, i + 1) = 1.0
                aa(12, 1, i + 1) = 0.5 * dxii
                aa(12, 12, i + 1) = -1.0
                !
            end do
            !
            !---- set tip  m,s  to zero
            i = ctxt%ii + 1
            aa(4, 4, i) = 1.0
            aa(5, 5, i) = 1.0
            aa(6, 6, i) = 1.0
            aa(7, 7, i) = 1.0
            aa(8, 8, i) = 1.0
            aa(9, 9, i) = 1.0
            !
            !
            call b12sol(aa, bb, cc, rr, ctxt%ii + 1)
            !
            !      do i=1, ii+1
            !        write(*,6666) i, (rr(k,i),k=1, 9)
            ! 6666   format(1x,i2,9f8.3)
            ! end do
            !
            rmax = 0.0
            ctxt%rms = 0.0
            !
            !---- set under-relaxation factors
            do k = 1, 12
                rlxr(k) = 1.0
            end do
            !
            do i = 1, ctxt%ii + 1
                do k = 1, 12
                    if(rlxr(k) * rr(k, i) > rrlim(k)) rlxr(k) = rrlim(k) / rr(k, i)
                    if(rlxr(k) * rr(k, i) < -rrlim(k)) rlxr(k) = -rrlim(k) / rr(k, i)
                    !
                    rmax = max(rmax, abs(rr(k, i) / rrlim(k)))
                    ctxt%rms = ctxt%rms + (rr(k, i) / rrlim(k))**2
                end do
            end do
            !
            ctxt%rms = sqrt(ctxt%rms / float(9 * ctxt%ii))
            !
            !---- set minimum under-relaxation factor over all variables
            ctxt%rlx = 1.0
            do k = 1, 12
                ctxt%rlx = amin1(ctxt%rlx, rlxr(k))
            end do
            !
            !---- update solution
            do i = 1, ctxt%ii + 1
                ctxt%tx(i) = ctxt%tx(i) - ctxt%rlx * rr(1, i)
                ctxt%ty(i) = ctxt%ty(i) - ctxt%rlx * rr(2, i)
                ctxt%tz(i) = ctxt%tz(i) - ctxt%rlx * rr(3, i)
                ctxt%momx(i) = ctxt%momx(i) - ctxt%rlx * rr(4, i)
                ctxt%momy(i) = ctxt%momy(i) - ctxt%rlx * rr(5, i)
                ctxt%momz(i) = ctxt%momz(i) - ctxt%rlx * rr(6, i)
                ctxt%shrx(i) = ctxt%shrx(i) - ctxt%rlx * rr(7, i)
                ctxt%shry(i) = ctxt%shry(i) - ctxt%rlx * rr(8, i)
                ctxt%shrz(i) = ctxt%shrz(i) - ctxt%rlx * rr(9, i)
                ctxt%wx(i) = ctxt%wx(i) - ctxt%rlx * rr(10, i)
                ctxt%wy(i) = ctxt%wy(i) - ctxt%rlx * rr(11, i)
                ctxt%wz(i) = ctxt%wz(i) - ctxt%rlx * rr(12, i)
                !        write(*,*)
                !        write(*,*) i
                !        write(*,1200) (rr(k,i),k=1,12)
                ! 1200   format( 4(1x, 3e12.4 /) )
            end do
            !
            !
            !c      write(*,1250) (rlxr(k), k=1, 12)
            !c 1250 format(1x, 11f8.3)
            !
            if (show_output) then
                write(*, 1800) iter, rmax, ctxt%rms, ctxt%rlx
                1800 format(1x, i3, '   max:', e9.3, '   rms:', e9.3, '   rlx =', f7.4)
            end if
            !
            if(rmax <= eps) go to 101
            !
        end do
        if (show_output) write(*, *) 'stcalc: Convergence failed.  Continuing ...'
        !
        101  continue
        !
        !---- integrate towards tip for x displacements
        !      i = 1
        !      wx(i) = 0.0
        !      do i=1, ii
        !        wx(i+1) =  wx(i)  -  (  tz(i) +   tz(i+1))*0.5 * dxii
        ! end do
        !
        return
    end
    ! stcalc



    subroutine stadd(ctxt)
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !------------------------------------------------------
        !     Adds on structural twist to static blade angles
        !------------------------------------------------------
        !
        do i = 1, ctxt%ii
            ctxt%beta(i) = ctxt%beta0(i) + (ctxt%ty(i) + ctxt%ty(i + 1)) * 0.5
        end do
        !
        if (show_output) write(*, 1000) (ctxt%beta(ctxt%ii) - ctxt%beta0(ctxt%ii)) * 180.0 / pi
        !
        ctxt%conv = .false.
        return
        !
        1000 format(/' New working blade angles set.'&
                /' Tip angle deflection =', f8.3, '  deg.')
    end
    ! stadd


    subroutine stset(ctxt)
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !------------------------------------------------------
        !     Removes structural twist to get static blade angles
        !------------------------------------------------------
        !
        do i = 1, ctxt%ii
            ctxt%beta0(i) = ctxt%beta(i) - (ctxt%ty(i) + ctxt%ty(i + 1)) * 0.5
        end do
        !
        if (show_output) write(*, 1000) (ctxt%beta(ctxt%ii) - ctxt%beta0(ctxt%ii)) * 180.0 / pi
        !
        ctxt%conv = .false.
        return
        !
        1000 format(/' New static blade angles set.'&
                /' Tip angle deflection =', f8.3, '  deg.')
    end
    ! stset



    subroutine stwrit(ctxt, lu)
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !---------------------------------------------
        !     Dumps blade force output to unit lu
        !---------------------------------------------
        !
        rtd = 180.0 / pi
        !
        iadd = 1
        if(lu == ctxt%luwrit) iadd = ctxt%incr
        !
        write(lu, 1020)
        !
        momref = ctxt%rho * ctxt%vel**2 * ctxt%rad**3
        !
        !--- Deflections, moments and forces on blade beam
        do i = 1, ctxt%ii, iadd
            !
            !********* still need to be averaged to i+1/2
            !
            !--- deflections
            wxa = ctxt%wx(i)
            wya = ctxt%wy(i)
            wza = ctxt%wz(i)
            !--- angular deflections (deg)
            txa = ctxt%tx(i) * rtd
            tya = ctxt%ty(i) * rtd
            tza = ctxt%tz(i) * rtd
            !--- bending moments
            mxa = ctxt%momx(i) * ctxt%rho * ctxt%vel**2 * ctxt%rad**3
            mya = ctxt%momy(i) * ctxt%rho * ctxt%vel**2 * ctxt%rad**3
            mza = ctxt%momz(i) * ctxt%rho * ctxt%vel**2 * ctxt%rad**3
            !--- shear forces
            sxa = ctxt%shrx(i) * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
            sya = ctxt%shry(i) * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
            sza = ctxt%shrz(i) * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
            !
            write(lu, 1035) i, ctxt%xi(i), wxa, wza, tya, mza, mxa, mya, sya, sxa, sza
        end do
        !
        !....................................................................
        !
        1020 format(&
                /'  i    r/r     u/r     w/r     t         Mz          Mx'&
                '           t            p            Sx           Sz'&
                /'                             (deg)      (n-m)       (n-m)'&
                '       (n-m)         (n)          (n)          (n)')
        !
        1035 format(1x, &
                i2, f7.3, f8.4, f8.4, f7.2, 6(1x, g12.4))
        !
        !c  i    r/r     u/r     w/r     t         Mz          Mx           t            p            Sx           Sz
        !c                            (deg)      (n-m)       (n-m)       (n-m)         (n)          (n)          (n)
        !c  1  0.307  0.0000  0.0000   0.00   0.1338       0.5678e-01  -0.1882        529.7        3.883       -1.804
        !cXiiffffffffffffffFfffffffffffffffxggggggggggggxggggggggggggxggggggggggggxggggggggggggxggggggggggggxgggggggggggg
        !
        !
        !--- Display the strain components on the blade beam
        cosr = cos(ctxt%rake)
        write(lu, 2020)
        !
        do i = 1, ctxt%ii, iadd
            !
            rst = ctxt%rstb(i) / ctxt%rad
            dxii = ctxt%dxi(i) / cosr
            !
            !------ bending strains
            ex = rst * (ctxt%tz(i + 1) - ctxt%tz(i)) / dxii * 1000.0
            ez = -rst * (ctxt%tx(i + 1) - ctxt%tx(i)) / dxii * 1000.0
            !
            !------ extensional strain
            ey = (ctxt%wy(i + 1) - ctxt%wy(i)) / dxii * 1000.0
            !------ torsional shear
            gt = rst * (ctxt%ty(i + 1) - ctxt%ty(i)) / dxii * 1000.0
            !------ max normal strain
            emax = sqrt(ex**2 + ez**2) + ey
            !
            write(lu, 2030) i, ctxt%xi(i), ex, ez, ey, emax, gt
        end do
        !
        return

        !
        2020 format(&
                /' i    r/r      Ex       Ez       Ey      Emax'&
                '      g     x 1000')
        !          10  0.425   10.002   14.002   20.203   12.000   13.450
        2030 format(1x, &
                i2, f7.3, 5f9.4)
        !
    end
    ! stwrit



    subroutine b12sol(a, b, c, r, ii)
        use m_common, only : show_output
        dimension a(12, 12, ii), b(12, 12, ii), c(12, 12, ii)
        dimension r(12, 1, ii)
        !-------------------------------------------------------
        !      Solves the 12x12 block-tridiagonal Newton system
        !      by a standard block elimination scheme.
        !      The solutions are returned in the r vectors.
        !
        !      |a c      ||d|   |r..|
        !      |b a c    ||d|   |r..|
        !      |  b . .  ||.| = |r..|
        !      |    . . c||.|   |r..|
        !      |      b a||d|   |r..|
        !-------------------------------------------------------
        !
        nrhs = 1
        !
        !cc** Forward sweep: Elimination of lower block diagonal (b's).
        do i = 1, ii
            !
            im = i - 1
            !
            !------ don't eliminate first b block because it doesn't exist
            if(i == 1) go to 12
            !
            !------ eliminate Bi block, thus modifying Ai and Ci blocks
            do k = 1, 12
                do l = 1, 12
                    a(k, l, i) = a(k, l, i)&
                            - (b(k, 1, i) * c(1, l, im)&
                                    + b(k, 2, i) * c(2, l, im)&
                                    + b(k, 3, i) * c(3, l, im)&
                                    + b(k, 4, i) * c(4, l, im)&
                                    + b(k, 5, i) * c(5, l, im)&
                                    + b(k, 6, i) * c(6, l, im)&
                                    + b(k, 7, i) * c(7, l, im)&
                                    + b(k, 8, i) * c(8, l, im)&
                                    + b(k, 9, i) * c(9, l, im)&
                                    + b(k, 10, i) * c(10, l, im)&
                                    + b(k, 11, i) * c(11, l, im)&
                                    + b(k, 12, i) * c(12, l, im))
                end do
                do l = 1, nrhs
                    r(k, l, i) = r(k, l, i)&
                            - (b(k, 1, i) * r(1, l, im)&
                                    + b(k, 2, i) * r(2, l, im)&
                                    + b(k, 3, i) * r(3, l, im)&
                                    + b(k, 4, i) * r(4, l, im)&
                                    + b(k, 5, i) * r(5, l, im)&
                                    + b(k, 6, i) * r(6, l, im)&
                                    + b(k, 7, i) * r(7, l, im)&
                                    + b(k, 8, i) * r(8, l, im)&
                                    + b(k, 9, i) * r(9, l, im)&
                                    + b(k, 10, i) * r(10, l, im)&
                                    + b(k, 11, i) * r(11, l, im)&
                                    + b(k, 12, i) * r(12, l, im))
                end do
            end do
            !
            !                                                              -1
            !cc---- multiply Ci block and righthand side Ri vectors by (Ai)
            !       using Gaussian elimination.
            !
            !cc        call shoblk(12,i,a(1,1,i))
            !
            12   do kpiv = 1, 11
                kp1 = kpiv + 1
                !
                !-------- find max pivot index kx
                kx = kpiv
                do k = kp1, 12
                    if(abs(a(k, kpiv, i)) - abs(a(kx, kpiv, i))) 131, 131, 1311
                    1311        kx = k
                131 end do
                !
                if(a(kx, kpiv, i) == 0.0) then
                    if (show_output) write(*, *) 'Singular a block, i = ', i
                    stop
                endif
                !
                pivot = 1.0 / a(kx, kpiv, i)
                !
                !-------- switch pivots
                a(kx, kpiv, i) = a(kpiv, kpiv, i)
                !
                !-------- switch rows & normalize pivot row
                do l = kp1, 12
                    temp = a(kx, l, i) * pivot
                    a(kx, l, i) = a(kpiv, l, i)
                    a(kpiv, l, i) = temp
                end do
                !
                do l = 1, 12
                    temp = c(kx, l, i) * pivot
                    c(kx, l, i) = c(kpiv, l, i)
                    c(kpiv, l, i) = temp
                end do
                !
                do l = 1, nrhs
                    temp = r(kx, l, i) * pivot
                    r(kx, l, i) = r(kpiv, l, i)
                    r(kpiv, l, i) = temp
                end do
                !
                !-------- forward eliminate everything
                do k = kp1, 12
                    atmp = -a(k, kpiv, i)
                    if(atmp == 0.0) go to 135
                    do l = kp1, 12
                        a(k, l, i) = a(k, l, i) + atmp * a(kpiv, l, i)
                    end do
                    c(k, 1, i) = c(k, 1, i) + atmp * c(kpiv, 1, i)
                    c(k, 2, i) = c(k, 2, i) + atmp * c(kpiv, 2, i)
                    c(k, 3, i) = c(k, 3, i) + atmp * c(kpiv, 3, i)
                    c(k, 4, i) = c(k, 4, i) + atmp * c(kpiv, 4, i)
                    c(k, 5, i) = c(k, 5, i) + atmp * c(kpiv, 5, i)
                    c(k, 6, i) = c(k, 6, i) + atmp * c(kpiv, 6, i)
                    c(k, 7, i) = c(k, 7, i) + atmp * c(kpiv, 7, i)
                    c(k, 8, i) = c(k, 8, i) + atmp * c(kpiv, 8, i)
                    c(k, 9, i) = c(k, 9, i) + atmp * c(kpiv, 9, i)
                    c(k, 10, i) = c(k, 10, i) + atmp * c(kpiv, 10, i)
                    c(k, 11, i) = c(k, 11, i) + atmp * c(kpiv, 11, i)
                    c(k, 12, i) = c(k, 12, i) + atmp * c(kpiv, 12, i)
                    do l = 1, nrhs
                        r(k, l, i) = r(k, l, i) + atmp * r(kpiv, l, i)
                    end do
                135 end do
                !
            end do
            !
            !------ solve for last row
            if(a(12, 12, i) == 0.0) then
                if (show_output) write(*, *) 'Singular a block, i = ', i
                stop
            endif
            pivot = 1.0 / a(12, 12, i)
            c(12, 1, i) = c(12, 1, i) * pivot
            c(12, 2, i) = c(12, 2, i) * pivot
            c(12, 3, i) = c(12, 3, i) * pivot
            c(12, 4, i) = c(12, 4, i) * pivot
            c(12, 5, i) = c(12, 5, i) * pivot
            c(12, 6, i) = c(12, 6, i) * pivot
            c(12, 7, i) = c(12, 7, i) * pivot
            c(12, 8, i) = c(12, 8, i) * pivot
            c(12, 9, i) = c(12, 9, i) * pivot
            c(12, 10, i) = c(12, 10, i) * pivot
            c(12, 11, i) = c(12, 11, i) * pivot
            c(12, 12, i) = c(12, 12, i) * pivot
            do l = 1, nrhs
                r(12, l, i) = r(12, l, i) * pivot
            end do
            !
            !------ back substitute everything
            do kpiv = 10, 1, -1
                kp1 = kpiv + 1
                do k = kp1, 12
                    c(kpiv, 1, i) = c(kpiv, 1, i) - a(kpiv, k, i) * c(k, 1, i)
                    c(kpiv, 2, i) = c(kpiv, 2, i) - a(kpiv, k, i) * c(k, 2, i)
                    c(kpiv, 3, i) = c(kpiv, 3, i) - a(kpiv, k, i) * c(k, 3, i)
                    c(kpiv, 4, i) = c(kpiv, 4, i) - a(kpiv, k, i) * c(k, 4, i)
                    c(kpiv, 5, i) = c(kpiv, 5, i) - a(kpiv, k, i) * c(k, 5, i)
                    c(kpiv, 6, i) = c(kpiv, 6, i) - a(kpiv, k, i) * c(k, 6, i)
                    c(kpiv, 7, i) = c(kpiv, 7, i) - a(kpiv, k, i) * c(k, 7, i)
                    c(kpiv, 8, i) = c(kpiv, 8, i) - a(kpiv, k, i) * c(k, 8, i)
                    c(kpiv, 9, i) = c(kpiv, 9, i) - a(kpiv, k, i) * c(k, 9, i)
                    c(kpiv, 10, i) = c(kpiv, 10, i) - a(kpiv, k, i) * c(k, 10, i)
                    c(kpiv, 11, i) = c(kpiv, 11, i) - a(kpiv, k, i) * c(k, 11, i)
                    c(kpiv, 12, i) = c(kpiv, 12, i) - a(kpiv, k, i) * c(k, 12, i)
                    do l = 1, nrhs
                        r(kpiv, l, i) = r(kpiv, l, i) - a(kpiv, k, i) * r(k, l, i)
                    end do
                end do
            end do
        end do
        !
        !cc** Backward sweep: Back substitution using upper block diagonal (Ci's).
        do i = ii - 1, 1, -1
            ip = i + 1
            do l = 1, nrhs
                do k = 1, 12
                    r(k, l, i) = r(k, l, i)&
                            - (r(1, l, ip) * c(k, 1, i)&
                                    + r(2, l, ip) * c(k, 2, i)&
                                    + r(3, l, ip) * c(k, 3, i)&
                                    + r(4, l, ip) * c(k, 4, i)&
                                    + r(5, l, ip) * c(k, 5, i)&
                                    + r(6, l, ip) * c(k, 6, i)&
                                    + r(7, l, ip) * c(k, 7, i)&
                                    + r(8, l, ip) * c(k, 8, i)&
                                    + r(9, l, ip) * c(k, 9, i)&
                                    + r(10, l, ip) * c(k, 10, i)&
                                    + r(11, l, ip) * c(k, 11, i)&
                                    + r(12, l, ip) * c(k, 12, i))
                end do
            end do
        end do
        !
        return
    end
    ! b12sol
end module m_xbend