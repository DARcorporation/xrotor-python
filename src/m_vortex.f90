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

module m_vortex
contains
    subroutine vrtxco(imax, ii, nblds, lduct, rake, &
            xi, xv, gam, adw, vind_gam, vind_adw)
        use m_common, only : show_output, pi
        !
        parameter (ntdim = 5000)
        dimension xi(imax), xv(imax), gam(imax)
        dimension vind_adw(3, imax), vind_gam(3, imax, imax)
        !
        dimension a(3), b(3), uvw(3), uvw_a(3, 3), uvw_b(3, 3)
        dimension vsum(3), vadw(3)
        dimension thetspc(5000)
        !
        logical lduct
        !-----------------------------------------
        !     Calculates "Vortex Momentum"
        !     Gamma-swirl influence coefficients
        !
        !     Input:
        !       imax         array dimension
        !       ii           number of radial points on blade (circulation stations)
        !       nn           number of Fourier harmonics
        !       nblds        number of blades
        !       lduct        t for duct outer bc
        !       rake         blade rake angle from y axis in xy plane
        !       xi(i)        r/r control point coordinate array
        !       xv(i)        r/r vortex  leg   coordinate array
        !       gam(i)       circulation array
        !       adw          wake advance ratio  v/wr
        !
        !     Output:
        !       vt_gam(i,j)  sensitivity of swirl velocity to circulation
        !       vt_adw(i)    sensitivity of swirl velocity to wake advance ratio
        !-----------------------------------------
        blds = float(nblds)
        !
        !pi = 4.0 * atan(1.0)
        !
        xi0 = xv(1)
        xitip = xv(ii + 1)
        tanrak = tan(rake)

        do i = 1, ii
            do j = 1, ii
                vind_gam(1, i, j) = 0.
                vind_gam(2, i, j) = 0.
                vind_gam(3, i, j) = 0.
            enddo
            vind_adw(1, i) = 0.
            vind_adw(2, i) = 0.
            vind_adw(3, i) = 0.
        enddo
        !
        !--- Set up variable theta spacing for near, intermediate and far field
        dth1 = pi / 60.
        rad1 = 2.0
        thet1 = rad1 / adw
        dth2 = pi / 20.
        rad2 = 4.0
        thet2 = rad2 / adw
        dth3 = pi / 8.
        rad3 = 50.0
        thet3 = rad3 / adw
        !
        n1 = ifix(2.0 * (thet1) / (dth2 + dth1))
        n2 = ifix(2.0 * (thet2 - thet1) / (dth3 + dth2))
        ddth1 = (dth2 - dth1) / float(n1 - 1)
        ddth2 = (dth3 - dth2) / float(n2 - 1)
        !
        thet = 0.0
        dth = dth1
        do i = 1, ntdim
            if(thet < thet1) then
                thetspc(i) = thet
                thet = thet + dth
                dth = dth + ddth1
            elseif(thet < thet2) then
                thetspc(i) = thet
                thet = thet + dth
                dth = dth + ddth2
            elseif(thet < thet3) then
                thetspc(i) = thet
                dth = dth3
                thet = thet + dth
            else
                nthet = i - 1
                go to 100
            endif
        end do
        if (show_output) write(*, *) 'Too many vortex segments for spacing array'
        nthet = ntdim
        100  continue
        !
        if(lduct) then
            !----- use simple mean swirl to get swirl at blade
            do i = 1, ii
                do j = 1, ii
                    vind_gam(1, i, j) = 0.
                    vind_gam(2, i, j) = 0.
                    vind_gam(3, i, j) = 0.
                enddo
                vind_gam(3, i, i) = blds / (4.0 * pi * xi(i))
                vind_adw(3, i) = 0.0
                vind_adw(2, i) = 0.0
                vind_gam(1, i, i) = vind_gam(3, i, i) * xi(i) / adw
                vind_adw(1, i) = -vind_gam(1, i, i) * gam(i) / adw
            enddo
            !
        else
            !
            !----- Do a discrete vortex integration of slipstream vortices
            dtbld = 2.0 * pi / float(nblds)
            if (show_output) write(*, 20) nblds * nthet
            20    format(/'Vortex points/radial station = ', i6)
            !
            !--- velocity influences for point - r0
            do i = 1, ii
                r0x = xi(i) * tanrak
                r0y = xi(i)
                r0z = 0.0
                !         write(*,*) 'cp ',r0x,r0y,r0z
                !
                vind_adw(1, i) = 0.0
                vind_adw(2, i) = 0.0
                vind_adw(3, i) = 0.0
                !
                !----- For each vortex trailing leg (ii+1 legs starting at xv(j))
                do j = 1, ii + 1
                    !
                    vsum(1) = 0.0
                    vsum(2) = 0.0
                    vsum(3) = 0.0
                    vadw(1) = 0.0
                    vadw(2) = 0.0
                    vadw(3) = 0.0
                    !
                    rv = xv(j)
                    xxv = rv * tanrak
                    !
                    !----- For each blade
                    thetoff = 0.0
                    do n = 1, nblds
                        !
                        !----- For angles around helix to the far-field
                        thet1 = thetspc(1)
                        r1x = xxv + thet1 * xitip * adw
                        r1y = rv * cos(thet1 + thetoff)
                        r1z = rv * sin(thet1 + thetoff)
                        r1_adw = thet1 * xitip
                        !
                        do l = 1, nthet - 1
                            !
                            thet2 = thetspc(l + 1)
                            r2x = xxv + thet2 * xitip * adw
                            r2y = rv * cos(thet2 + thetoff)
                            r2z = rv * sin(thet2 + thetoff)
                            r2_adw = thet2 * xitip
                            !
                            a(1) = r1x - r0x
                            a(2) = r1y - r0y
                            a(3) = r1z - r0z
                            b(1) = r2x - r0x
                            b(2) = r2y - r0y
                            b(3) = r2z - r0z
                            call vorsegvel(a, b, uvw, uvw_a, uvw_b)
                            !
                            vsum(1) = vsum(1) + uvw(1)
                            vsum(2) = vsum(2) + uvw(2)
                            vsum(3) = vsum(3) + uvw(3)
                            !
                            vadw(1) = vadw(1) + uvw_a(1, 1) * r1_adw&
                                    + uvw_b(1, 1) * r2_adw
                            vadw(2) = vadw(2) + uvw_a(2, 1) * r1_adw&
                                    + uvw_b(2, 1) * r2_adw
                            vadw(3) = vadw(3) + uvw_a(3, 1) * r1_adw&
                                    + uvw_b(3, 1) * r2_adw
                            !
                            thet1 = thet2
                            r1x = r2x
                            r1y = r2y
                            r1z = r2z
                            r1_adw = r2_adw
                            !
                            !               if(i == 1) then
                            !                write(88,10) r1x,r1y,r1z
                            !                write(*,10) 'r1 ',i,j,n,l,r2x,r2y,r2z
                            !               endif
                            10            format(3f10.3)
                            12            format(a, 2i6, 2f10.3)

                        end do ! l loop
                        !
                        thetoff = thetoff + dtbld
                        !
                    end do ! n loop
                    !
                    vsum(3) = -vsum(3)
                    vadw(3) = -vadw(3)
                    !---- alternate + and - influence for each vortex line into velocity
                    !     influence matrix
                    !
                    !---Open wake, interdigitate all vortex lines
                    if(j <= ii) then
                        vind_gam(1, i, j) = -vsum(1)
                        vind_gam(2, i, j) = -vsum(2)
                        vind_gam(3, i, j) = -vsum(3)
                        vind_adw(1, i) = vind_adw(1, i) - vadw(1) * gam(j)
                        vind_adw(2, i) = vind_adw(2, i) - vadw(2) * gam(j)
                        vind_adw(3, i) = vind_adw(3, i) - vadw(3) * gam(j)
                    endif
                    if(j > 1) then
                        vind_gam(1, i, j - 1) = vind_gam(1, i, j - 1) + vsum(1)
                        vind_gam(2, i, j - 1) = vind_gam(2, i, j - 1) + vsum(2)
                        vind_gam(3, i, j - 1) = vind_gam(3, i, j - 1) + vsum(3)
                        vind_adw(1, i) = vind_adw(1, i) + vadw(1) * gam(j - 1)
                        vind_adw(2, i) = vind_adw(2, i) + vadw(2) * gam(j - 1)
                        vind_adw(3, i) = vind_adw(3, i) + vadw(3) * gam(j - 1)
                    endif
                    !
                end do ! j loop
                !
            end do ! i loop
            !
        endif
        !
        return
    end
    ! vrtxco


    subroutine vorsegvel(a, b, uvw, uvw_a, uvw_b)
        !-------------------------------------------------------------------
        !     Calculates the velocity induced by a vortex segment
        !     of unit strength, with no core radius.
        !
        !     The point where the velocity is calculated is at 0,0,0.
        !
        !     Positive circulation is by righthand rule from a to b.
        !
        !  Input:
        !     a(3)    coordinates of vertex #1 of the vortex
        !     b(3)    coordinates of vertex #2 of the vortex
        !
        !  Output:
        !     uvw(3)      induced velocity
        !     uvw_a(3,3)  duvw/da  sensitivity
        !     uvw_b(3,3)  duvw/db  sensitivity
        !
        !-------------------------------------------------------------------
        dimension a(3), b(3), uvw(3), uvw_a(3, 3), uvw_b(3, 3)
        !
        !
        dimension axb(3), axb_a(3, 3), axb_b(3, 3)
        !
        data pi4  / 12.56637062 /
        !
        asq = a(1)**2 + a(2)**2 + a(3)**2
        bsq = b(1)**2 + b(2)**2 + b(3)**2
        !
        amag = sqrt(asq)
        bmag = sqrt(bsq)
        !
        do k = 1, 3
            uvw(k) = 0.
            do l = 1, 3
                uvw_a(k, l) = 0.
                uvw_b(k, l) = 0.
            enddo
        enddo
        !
        !---- contribution from the vortex leg
        if (amag * bmag /= 0.0) then
            axb(1) = a(2) * b(3) - a(3) * b(2)
            axb(2) = a(3) * b(1) - a(1) * b(3)
            axb(3) = a(1) * b(2) - a(2) * b(1)
            !
            axb_a(1, 1) = 0.0
            axb_a(1, 2) = b(3)
            axb_a(1, 3) = -b(2)
            axb_a(2, 1) = -b(3)
            axb_a(2, 2) = 0.0
            axb_a(2, 3) = b(1)
            axb_a(3, 1) = b(2)
            axb_a(3, 2) = -b(1)
            axb_a(3, 3) = 0.0
            !
            axb_b(1, 1) = 0.0
            axb_b(1, 2) = -a(3)
            axb_b(1, 3) = a(2)
            axb_b(2, 1) = a(3)
            axb_b(2, 2) = 0.0
            axb_b(2, 3) = -a(1)
            axb_b(3, 1) = -a(2)
            axb_b(3, 2) = a(1)
            axb_b(3, 3) = 0.0
            !
            adb = a(1) * b(1) + a(2) * b(2) + a(3) * b(3)
            !
            den = amag * bmag + adb
            den_asq = 0.5 * bmag / amag
            den_bsq = 0.5 * amag / bmag
            !
            t = (1.0 / amag + 1.0 / bmag) / den
            !
            t_adb = -t / den
            t_asq = -t / den * den_asq - 0.5 / (den * amag * asq)
            t_bsq = -t / den * den_bsq - 0.5 / (den * bmag * bsq)
            !
            do k = 1, 3
                uvw(k) = axb(k) * t
                !
                do l = 1, 3
                    uvw_a(k, l) = axb(k) * t_asq * a(l) * 2.0&
                            + axb(k) * t_adb * b(l)&
                            + axb_a(k, l) * t
                    uvw_b(k, l) = axb(k) * t_bsq * b(l) * 2.0&
                            + axb(k) * t_adb * a(l)&
                            + axb_b(k, l) * t
                enddo
            enddo
        endif
        !
        do k = 1, 3
            uvw(k) = uvw(k) / pi4
            do l = 1, 3
                uvw_a(k, l) = uvw_a(k, l) / pi4
                uvw_b(k, l) = uvw_b(k, l) / pi4
            enddo
        enddo
        !
        return
    end
    ! vorvel

end module m_vortex