!*==M_XIO.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
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

module m_xio
    implicit none
contains
    subroutine save(ctxt, fname1)
        use m_userio, only : asks
        use m_xaero, only : getpolar
        use i_common, only : Common, show_output, pi
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real A0, A0DEG, BETA0DEG, CDMIN, CLDMIN, CLMAX, CLMIN, CMCON, DCDCL2, &
                & DCLDA, DCLDA_STALL, DCL_STALL, MCRIT, REREF, REXP, XISECT
        integer I, LU, N, j
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        character*(*) fname1
        !--------------------------------------------------------------------------
        !     Save rotor and operating state in new xrotor_Version 6.9 format
        !     This format saves additional parameters including the aero data
        !     sections.
        !--------------------------------------------------------------------------
        logical lvduct
        !
        character*1 ans
        real, allocatable :: polar(:, :)
        character (47) :: settings
        !
        ctxt%greek = .false.
        !c      if(.not.conv) then
        !c       write(*,1050)
        !c       return
        !c      endif
        !
        lvduct = abs(ctxt%adw - ctxt%adv * ctxt%urduct)>=5.E-5
        lu = ctxt%lutemp
        ctxt%fname = fname1
        !
        if (ctxt%fname(1:1)==' ') call asks('enter filename^', ctxt%fname)
        open (lu, file = ctxt%fname, status = 'old', err = 100)
        if (show_output) write (*, *)
        if (show_output) write (*, *) 'Output file exists.  Overwrite?  y'
        if (.not. ctxt%always_overwrite) then
            read (*, 99001) ans
        end if
        !
        !...................................................................
        99001  format (a)
        if (index('nn', ans)==0) goto 200
        !
        close (lu)
        if (show_output) write (*, *) 'Current rotor not saved.'
        return
        !
        100   open (lu, file = ctxt%fname, status = 'new', err = 300)
        200   rewind (lu)

        settings = '    "free": '
        if (ctxt%free) then
            settings = trim(settings) // ' true '
        else
            settings = trim(settings)  // ' false'
        end if
        settings = trim(settings)  // ', "duct": '
        if (ctxt%duct) then
            settings = trim(settings)  // ' true '
        else
            settings = trim(settings)  // ' false'
        end if
        settings = trim(settings)  // ', "wind": '
        if (ctxt%wind) then
            settings = trim(settings)  // ' true '
        else
            settings = trim(settings)  // ' false'
        end if

        99100 format (A)
        99101 format (A, g12.4, A)
        99102 format (A, i3, A)
        99103 format ('      ['3(f8.4, ', '), f8.4']', A)
        99104 format ('      "'f5.3'": [')
        99105 format ('        ['3(f8.4, ', '), f8.4']', A)
        write (lu, 99100) '{'
        write (lu, 99100) '  "conditions": {'
        write (lu, 99101) '    "rho": ', ctxt%rho, ','
        write (lu, 99101) '    "vso": ', ctxt%vso, ','
        write (lu, 99101) '    "rmu": ', ctxt%rmu, ','
        write (lu, 99101) '    "alt": ', ctxt%alt, ','
        write (lu, 99101) '    "vel": ', ctxt%vel, ','
        write (lu, 99101) '    "adv": ', ctxt%adv
        write (lu, 99100) '  },'
        write (lu, 99100) '  "disk": {'
        write (lu, 99102) '    "n_blds": ', ctxt%nblds, ','
        write (lu, 99100) '    "dimensions": {'
        write (lu, 99101) '      "r_hub" : ', ctxt%xi0 * ctxt%rad, ','
        write (lu, 99101) '      "r_tip" : ', ctxt%rad, ','
        write (lu, 99101) '      "r_wake": ', ctxt%xw0 * ctxt%rad, ','
        write (lu, 99101) '      "rake"  : ', ctxt%rake
        write (lu, 99100) '    },'
        write (lu, 99100) '    "geometry": ['
        do i = 1, ctxt%ii-1
            if (i < ctxt%ii-1) then
                write (lu, 99103) ctxt%xi(i), ctxt%ch(i), ctxt%beta(i) * 180. / pi, ctxt%ubody(i), ','
            else
                write (lu, 99103) ctxt%xi(i), ctxt%ch(i), ctxt%beta(i) * 180. / pi, ctxt%ubody(i)
            end if
        end do
        write (lu, 99100) '    ],'
        write (lu, 99100) '    "polars": {'
        do i = 1, ctxt%n_polars
            write (lu, 99104) ctxt%xi_polars(i)
            call getpolar(ctxt, i, polar)
            do n = 1, ctxt%n_polar_points(i)
                if (n < ctxt%n_polar_points(i)) then
                    write (lu, 99105) polar(n, 1) * 180. / pi, polar(n, 2:), ','
                else
                    write (lu, 99105) polar(n, 1) * 180. / pi, polar(n, 2:)
                end if
            end do

            if (i < ctxt%n_polars - 1) then
                write (lu, 99100) '      ],'
            else
                write (lu, 99100) '      ]'
            end if
        end do
        write (lu, 99100) '    }'
        write (lu, 99100) '  },'
        write (lu, 99100) '  "settings": {'
        write (lu, 99100) settings
        write (lu, 99100) '  },'
        write (lu, 99101) '  "u_r_duct": ', ctxt%urduct
        write (lu, 99100) '}'

!        !--- Save added velocity components
!        if (ctxt%nadd>1) then
!            write (lu, 99015) ctxt%nadd
!            99015      format ('!Nadd'/1(1x, i5), /'!        Radd         Uadd         Vadd')
!            do i = 1, ctxt%nadd
!                write (lu, 99017) ctxt%radd(i), ctxt%uadd(i), ctxt%vadd(i)
!            enddo
!            if (show_output) write (*, *)                                       &
!                    &'External slipstream included in save file'
!        endif

        !
        close (lu)
        return
        !
        300   if (show_output) write (*, *) 'Bad filename.'
        if (show_output) write (*, *) 'Current rotor not saved.'
        return
        99016  format (/' *** Converged operating solution does not exist ***')
        99017  format (5(f16.4, 1x))
        !
        !x123456789012x123456789012x123456789012x123456789012x123456789012
        !!         Rho          Vso          Rmu           Alt')
        !
    end
    ! sav

    subroutine load(ctxt, fname1)
        !------------------------------------------------------------------------
        !     Reads in previously saved rotor in new xrotor_Version >= 6.9 format
        !     This format saves more information and can have optional comment
        !     lines beginning with a ! character.
        !------------------------------------------------------------------------
        ! use m_xaero, only : putaero
        use m_userio, only : asks
        use i_common, only : Common, show_output, ix, pi
        use m_xutils, only : strip
        use m_xaero, only: putpolars
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real A0, A0DEG, BETADEG, CDMIN, CLDMIN, CLMAX, CLMIN, CMCON, DCDCL2, &
                & DCLDA, DCLDA_STALL, DCL_STALL, FILEVERS, MCRIT, REREF, REXP, XISECT
        integer I, IIX, LU, N
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        character*(*) fname1
        character*128 line
        character*4 dump
        real :: xi_tmp, point(4)
        integer :: n_geom, n_polars
        real, allocatable :: xi_polars(:), polardata(:, :), tmp_polardata(:, :)
        integer, allocatable :: n_polar_points(:), indices(:), tmp_indices(:)
        ctxt%greek = .false.
        !
        lu = ctxt%lutemp
        !
        ctxt%fname = fname1
        if (ctxt%fname(1:1)==' ') call asks('enter filename^', ctxt%fname)
        open (lu, file = 'output.json', status = 'old', err = 400)

        call rdline(lu, line) !{
        call rdline(lu, line) !  "conditions": {
        call rdline(lu, line)
        read (line(11:23), *, err = 500) ctxt%rho
        call rdline(lu, line)
        read (line(11:23), *, err = 500) ctxt%vso
        call rdline(lu, line)
        read (line(11:23), *, err = 500) ctxt%rmu
        call rdline(lu, line)
        read (line(11:23), *, err = 500) ctxt%alt
        call rdline(lu, line)
        read (line(11:23), *, err = 500) ctxt%vel
        call rdline(lu, line)
        read (line(11:23), *, err = 500) ctxt%adv
        call rdline(lu, line) !  },
        call rdline(lu, line) !  "disk": {
        call rdline(lu, line)
        read (line(14:17), *, err = 500) ctxt%nblds
        call rdline(lu, line) !    "dimensions": {
        call rdline(lu, line)
        read (line(16:28), *, err = 500) ctxt%xi0
        call rdline(lu, line)
        read (line(16:28), *, err = 500) ctxt%rad
        call rdline(lu, line)
        read (line(16:28), *, err = 500) ctxt%xw0
        call rdline(lu, line)
        read (line(16:28), *, err = 500) ctxt%rake
        ctxt%xi0 = ctxt%xi0 / ctxt%rad
        ctxt%xw0 = ctxt%xw0 / ctxt%rad
        call rdline(lu, line) !    },
        call rdline(lu, line) !    "geometry": [
        do i=1, ctxt%ii
            call rdline(lu, line)
            line = trim(adjustl(line))

            if (line(1:1) == '[') then
                call strip(line, '[],')
                read (line, *, err = 100) ctxt%xi(i), ctxt%ch(i), ctxt%beta(i), ctxt%ubody(i)
            elseif (line(1:1) == ']') then
                goto 100
            else
                goto 500
            end if
        end do
        if (show_output) write (*, 99001) ctxt%fname(1:32)
        99001  format (' File  ', a, ' contains too many radial stations.'/' Loading not completed'/)
        return

        100 ctxt%beta = ctxt%beta * pi / 180.
        ctxt%beta0 = ctxt%beta
        n_geom = i

        ctxt%n_polars = 0
        allocate(xi_polars(0))
        allocate(polardata(0, 4))
        allocate(n_polar_points(0))

        call rdline(lu, line) !    "polars": {
        do i=1, 1000
            call rdline(lu, line)
            line = adjustl(line)
            if (line(1:1) == '"') then
                call strip(line, '"":[')
                read (line(1:5), *, err = 200) xi_tmp
                xi_polars = [xi_polars, xi_tmp]
                n_polar_points = [n_polar_points, 0]
                n_polars = n_polars + 1
            elseif (line(1:1) == '[') then
                call strip(line, '[],')
                read (line, *, err = 500) point
                n_polar_points(n_polars) = n_polar_points(n_polars) + 1
                allocate(tmp_polardata(sum(n_polar_points), 4))
                tmp_polardata(1:sum(n_polar_points)-1, :) = polardata
                tmp_polardata(sum(n_polar_points), :) = point
                call move_alloc(tmp_polardata, polardata)
            elseif (line(1:1) == ']') then
                if (line(2:2) /= ',') then
                    goto 200
                end if
            else
                goto 500
            end if
        end do
        200 polardata(:, 1) = polardata(:, 1) * pi / 180.
        call putpolars(ctxt, n_polars, n_polar_points, xi_polars, polardata)

        call rdline(lu, line) !    }
        call rdline(lu, line) !  },
        call rdline(lu, line) !  "settings": {
        call rdline(lu, line)
        if (line(13:16) == 'true') then
            ctxt%free = .true.
        elseif (line(13:16) == 'false') then
            ctxt%free = .false.
        else
            goto 500
        end if
        if (line(28:31) == 'true ') then
            ctxt%free = .true.
        elseif (line(28:32) == 'false') then
            ctxt%free = .false.
        else
            goto 500
        end if
        if (line(43:46) == 'true') then
            ctxt%free = .true.
        elseif (line(43:47) == 'false') then
            ctxt%free = .false.
        else
            goto 500
        end if

!        !--- Optional duct velocity
!        ctxt%urduct = 1.0
!        call rdline(lu, line)
!        if (line/='end'.and.line/='err') read (line, *, end = 100) ctxt%urduct
!        !
!        !---- Optional slipstream velocities
!        100   ctxt%nadd = 0
!        call rdline(lu, line)
!        if (line/='end'.and.line/='err') then
!            read (line, *, end = 300) ctxt%nadd
!            if (ctxt%nadd>ix) then
!                ctxt%nadd = ix
!                if (show_output) write (*, *)                                   &
!                        &'Warning, slipstream data terminated at '&
!                        &, ix
!            endif
!            do i = 1, ctxt%nadd
!                call rdline(lu, line)
!                if (line=='end'.or.line=='err') goto 200
!                read (line, *, err = 200, end = 200) ctxt%radd(i), ctxt%uadd(i), &
!                        & ctxt%vadd(i)
!            enddo
!            if (i<ctxt%nadd) then
!                ctxt%nadd = i - 1
!                if (show_output) write (*, *)                                   &
!                        &'warning, slipstream data terminated at '&
!                        &, ctxt%nadd
!            endif
!        endif
!        goto 300
!        !
!        200   if (i>2) ctxt%nadd = i - 1
!        !
!        300   close (lu)
!        if (ctxt%nadd>1) then
!            if (show_output) write (*, *)
!            if (show_output) write (*, *)                                       &
!                    &'slipstream profiles read with #points '&
!                    &, ctxt%nadd
!        endif
!        !
!        ctxt%conv = .false.
!        !
!        !--- Check for number of analysis stations to use
!        if (iix/=ctxt%ii) then
!            350       if (show_output) write (*, 99002) iix, ctxt%ii, ctxt%ii
!            99002      format (/'Read  # input stations = ', i3, /'Using # blade stations = ', &
!                    & i3, /'Enter # stations or <cr> for ', i3, ' ', $)
!            read (*, 99003) line
!            99003      format (a)
!            if (line/=' ') read (line, *, err = 350) ctxt%ii
!        endif

        300   close (lu)
        call initcase(ctxt, n_geom, .false.)
        ctxt%lrotor = .true.
        return

        400   if (show_output) write (*, 99004) ctxt%fname(1:32)
        99004  format (' File  ', a, ' not found'/)
        return

        500   if (show_output) write (*, 99005) ctxt%fname(1:32)
        99005  format (' File  ', a, ' has incompatible format'/' Loading not completed'/)
        close (lu)
        ctxt%conv = .false.
        return
    end

    subroutine initcase(ctxt, iix, losolve)
        use m_xrotor, only : output, setx
        use m_xoper, only : aper, xwinit
        use m_xaero, only : setiaero
        use i_common, only : Common, pi
        use m_spline, only : spline, seval
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        integer I, IIX
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        logical losolve
        !---- spline blade geometry to "old" radial locations
        do i = 1, iix
            ctxt%w1(i) = ctxt%xi(i)
            ctxt%w2(i) = ctxt%ch(i)
            ctxt%w4(i) = ctxt%beta(i)
            ctxt%w6(i) = ctxt%ubody(i)
        enddo
        ctxt%w3(1:iix) = spline(ctxt%w1(1:iix), ctxt%w2(1:iix))
        ctxt%w5(1:iix) = spline(ctxt%w1(1:iix), ctxt%w4(1:iix))
        ctxt%w7(1:iix) = spline(ctxt%w1(1:iix), ctxt%w6(1:iix))
        !
        !---- set radial stations for built-in distribution scheme
        call setx(ctxt)
        call xwinit(ctxt)
        !
        !---- interpolate read-in geometry to generated radial stations
        do i = 1, ctxt%ii
            ctxt%ch(i) = seval(ctxt%xi(i), ctxt%w2(1:iix), ctxt%w3(1:iix), ctxt%w1(1:iix))
            ctxt%beta(i) = seval(ctxt%xi(i), ctxt%w4(1:iix), ctxt%w5(1:iix), ctxt%w1(1:iix))
            ctxt%ubody(i) = seval(ctxt%xi(i), ctxt%w6(1:iix), ctxt%w7(1:iix), ctxt%w1(1:iix))
            ctxt%beta0(i) = ctxt%beta(i)
            !c        write(*,*) 'load trp i,ch,beta ',i,ch(i),beta(i)
        enddo
        ctxt%iinf = ctxt%ii + ctxt%ii / 2
        !
        call setiaero(ctxt)
        !---- calculate current operating point
        if (losolve) then
            call aper(ctxt, 4, 2, .true.)
            if (ctxt%conv) call output(ctxt, ctxt%luwrit)
        endif
        !
        !---- define design quantities for design of mil prop with same parameters
        ctxt%raddes = ctxt%rad
        ctxt%veldes = ctxt%vel
        ctxt%advdes = 0.
        ctxt%rpmdes = ctxt%vel / (ctxt%rad * ctxt%adv) * 30.0 / pi
        ctxt%r0des = ctxt%xi0 * ctxt%rad
        ctxt%rwdes = ctxt%xw0 * ctxt%rad
        ctxt%tddes = ctxt%ttot * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
        ctxt%pddes = ctxt%ptot * ctxt%rho * ctxt%vel**3 * ctxt%rad**2
        ctxt%dest = .false.
        ctxt%desp = .true.
        do i = 1, ctxt%ii
            ctxt%cldes(i) = ctxt%cl(i)
        enddo
        ctxt%cldes0 = 0.
    end
    ! load



    subroutine rdline(lun, line)
        !*** Start of declarations inserted by SPAG
        integer LUN
        !*** End of declarations inserted by SPAG
        !...Purpose  Read a non-comment line from the input file
        !...Input    Data read from unit lun
        !...Output   line  Character string with input line
        !                  line is set to 'end' for end or errors
        !
        character*(*) line
        do
            read (lun, 99001, end = 100, err = 200) line
            !
            99001      format (a)
            !
            !---- skip comment line
            if (index('!#', line(1:1))==0) then
                !
                !---- skip blank line
                !
                !---- normal return after significant line
                if (line/=' ') return
            endif
        enddo
        !
        100   line = 'end '
        return
        !
        200   line = 'err '
    end


end
