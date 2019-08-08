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
        ! use m_xaero, only : getaero
        use i_common, only : Common, show_output, pi
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real A0, A0DEG, BETA0DEG, CDMIN, CLDMIN, CLMAX, CLMIN, CMCON, DCDCL2, &
                & DCLDA, DCLDA_STALL, DCL_STALL, MCRIT, REREF, REXP, XISECT
        integer I, LU, N
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
        read (*, 99001) ans
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
        !
        !
        !--- Version header and case name
        if (ctxt%name==' ') ctxt%name = 'saved blade'
        write (lu, 99002) ctxt%version, ctxt%name
        99002  format ('xrotor version: ', f5.2/a)
        !--- Altitude and atmospheric data
        write (lu, 99003)
        99003  format ('!         Rho          Vso          Rmu           Alt')
        write (lu, 99017) ctxt%rho, ctxt%vso, ctxt%rmu, ctxt%alt
        !--- Radius, velocity, advance ratio and blade rake angle
        write (lu, 99004)
        99004  format ('!         Rad          Vel          Adv          Rake')
        write (lu, 99017) ctxt%rad, ctxt%vel, ctxt%adv, ctxt%rake
        !
        write (lu, 99005)
        99005  format ('!         xi0          xiw')
        write (lu, 99017) ctxt%xi0, ctxt%xw0
        !--- Save aero data for defined aero sections
        write (lu, 99006) ctxt%naero
        99006  format ('!  Naero'/1(1x, i5))
        mcrit = 0.8
        do n = 1, ctxt%naero
            ! TODO
!            call getaero(ctxt, n, xisect, a0, clmax, clmin, dclda, dclda_stall, &
!                    & dcl_stall, cdmin, cldmin, dcdcl2, cmcon, mcrit, reref, rexp)
            write (lu, 99007)
            99007      format ('!   Xisection')
            write (lu, 99017) xisect
            a0deg = a0 * 180.0 / pi
            write (lu, 99008)
            99008      format ('!       a0deg        dcLda        cLmax         cLmin')
            write (lu, 99017) a0deg, dclda, clmax, clmin
            write (lu, 99009)
            99009      format ('!  dcLdAstall     dcLstall      Cmconst         Mcrit')
            write (lu, 99017) dclda_stall, dcl_stall, cmcon, mcrit
            write (lu, 99010)
            99010      format ('!       cDmin      clcDmin     dcDdcl^2')
            write (lu, 99017) cdmin, cldmin, dcdcl2
            write (lu, 99011)
            99011      format ('!       rEref        rEexp')
            write (lu, 99017) reref, rexp
        enddo
        !
        !--- Save logical flags for duct and windmill
        write (lu, 99012) lvduct, ctxt%duct, ctxt%wind
        99012  format ('!lvDuct  lDuct   lWind'/3(1x, l2, 5x))
        !
        !--- #radial stations and #blades
        write (lu, 99013) ctxt%ii, ctxt%nblds
        99013  format ('!   ii Nblds'/2(1x, i5), &
                &/'!         r/r          c/r     Beta0deg         Ubody')
        !--- Save blade definition with chord,twist and body velocity
        do i = 1, ctxt%ii
            beta0deg = ctxt%beta0(i) * 180.0 / pi
            write (lu, 99017) ctxt%xi(i), ctxt%ch(i), beta0deg, ctxt%ubody(i)
        enddo
        !--- Duct velocity
        write (lu, 99014)
        99014  format ('!      urDuct')
        write (lu, 99017) ctxt%urduct
        !--- Save added velocity components
        if (ctxt%nadd>1) then
            write (lu, 99015) ctxt%nadd
            99015      format ('!Nadd'/1(1x, i5), /'!        Radd         Uadd         Vadd')
            do i = 1, ctxt%nadd
                write (lu, 99017) ctxt%radd(i), ctxt%uadd(i), ctxt%vadd(i)
            enddo
            if (show_output) write (*, *)                                       &
                    &'External slipstream included in save file'
        endif
        !
        close (lu)
        return
        !
        300   if (show_output) write (*, *) 'Bad filename.'
        if (show_output) write (*, *) 'Current rotor not saved.'
        return
        99016  format (/' *** Converged operating solution does not exist ***')
        99017  format (5(1x, g12.5))
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
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real A0, A0DEG, BETADEG, CDMIN, CLDMIN, CLMAX, CLMIN, CMCON, DCDCL2, &
                & DCLDA, DCLDA_STALL, DCL_STALL, FILEVERS, MCRIT, REREF, REXP, XISECT
        integer I, IIX, LU, N
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        character*(*) fname1
        character*128 line
        ctxt%greek = .false.
        !
        lu = ctxt%lutemp
        !
        ctxt%fname = fname1
        if (ctxt%fname(1:1)==' ') call asks('enter filename^', ctxt%fname)
        open (lu, file = ctxt%fname, status = 'old', err = 400)
        !
        !--- Check for new format/old format xrotor file
        call rdline(lu, line)
        if (line=='end'.or.line=='err') goto 500
        read (line(17:22), *) filevers
        if (show_output) write (*, 99001) filevers
        99001  format (' Reading file from xrotor Version ', f5.2)
        !
        !
        !--- Case title
        call rdline(lu, line)
        ctxt%name = line
        !
        call rdline(lu, line)
        read (line, *, err = 500) ctxt%rho, ctxt%vso, ctxt%rmu, ctxt%alt
        call rdline(lu, line)
        read (line, *, err = 500) ctxt%rad, ctxt%vel, ctxt%adv, ctxt%rake
        call rdline(lu, line)
        read (line, *, err = 500) ctxt%xi0, ctxt%xw0
        call rdline(lu, line)
        !
        !--- Read aero section definitions
        read (line, *, err = 500) ctxt%naero
        do n = 1, ctxt%naero
            call rdline(lu, line)
            read (line, *, err = 500) xisect
            call rdline(lu, line)
            read (line, *, err = 500) a0deg, dclda, clmax, clmin
            call rdline(lu, line)
            read (line, *, err = 500) dclda_stall, dcl_stall, cmcon, mcrit
            call rdline(lu, line)
            read (line, *, err = 500) cdmin, cldmin, dcdcl2
            call rdline(lu, line)
            read (line, *, err = 500) reref, rexp
            !
            a0 = a0deg * pi / 180.0
            ! TODO
!            call putaero(ctxt, n, xisect, a0, clmax, clmin, dclda, dclda_stall, &
!                    & dcl_stall, cdmin, cldmin, dcdcl2, cmcon, mcrit, reref, rexp)
        enddo
        !
        !--- Read flags for wake, duct and windmill modes
        call rdline(lu, line)
        read (line, *, err = 500) ctxt%free, ctxt%duct, ctxt%wind
        !
        if (show_output) write (*, *)
        if (ctxt%free.and.show_output) write (*, *)                             &
                &'self-deforming wake option set'
        if (.not.ctxt%free.and.show_output) write (*, *) 'rigid wake option set'
        if (ctxt%duct.and.show_output) write (*, *) 'duct option set'
        if (.not.ctxt%duct.and.show_output) write (*, *) 'free-tip option set'
        if (ctxt%wind.and.show_output) write (*, *) 'windmill plotting mode set'
        if (.not.ctxt%wind.and.show_output) write (*, *)                        &
                &'propeller plotting mode set'
        !
        if (show_output) write (*, *) ' '
        call rdline(lu, line)
        if (line=='end'.or.line=='err') goto 500
        read (line, *, err = 500) iix, ctxt%nblds
        do i = 1, iix
            call rdline(lu, line)
            read (line, *, err = 500) ctxt%xi(i), ctxt%ch(i), betadeg, ctxt%ubody(i)
            ctxt%beta(i) = betadeg * pi / 180.0
            ctxt%beta0(i) = ctxt%beta(i)
            !c        write(*,*) 'load i,ch,beta ',i,ch(i),beta(i)
        enddo
        !
        !--- Optional duct velocity
        ctxt%urduct = 1.0
        call rdline(lu, line)
        if (line/='end'.and.line/='err') read (line, *, end = 100) ctxt%urduct
        !
        !---- Optional slipstream velocities
        100   ctxt%nadd = 0
        call rdline(lu, line)
        if (line/='end'.and.line/='err') then
            read (line, *, end = 300) ctxt%nadd
            if (ctxt%nadd>ix) then
                ctxt%nadd = ix
                if (show_output) write (*, *)                                   &
                        &'Warning, slipstream data terminated at '&
                        &, ix
            endif
            do i = 1, ctxt%nadd
                call rdline(lu, line)
                if (line=='end'.or.line=='err') goto 200
                read (line, *, err = 200, end = 200) ctxt%radd(i), ctxt%uadd(i), &
                        & ctxt%vadd(i)
            enddo
            if (i<ctxt%nadd) then
                ctxt%nadd = i - 1
                if (show_output) write (*, *)                                   &
                        &'warning, slipstream data terminated at '&
                        &, ctxt%nadd
            endif
        endif
        goto 300
        !
        200   if (i>2) ctxt%nadd = i - 1
        !
        300   close (lu)
        if (ctxt%nadd>1) then
            if (show_output) write (*, *)
            if (show_output) write (*, *)                                       &
                    &'slipstream profiles read with #points '&
                    &, ctxt%nadd
        endif
        !
        ctxt%conv = .false.
        !
        !--- Check for number of analysis stations to use
        if (iix/=ctxt%ii) then
            350       if (show_output) write (*, 99002) iix, ctxt%ii, ctxt%ii
            99002      format (/'Read  # input stations = ', i3, /'Using # blade stations = ', &
                    & i3, /'Enter # stations or <cr> for ', i3, ' ', $)
            read (*, 99003) line
            99003      format (a)
            if (line/=' ') read (line, *, err = 350) ctxt%ii
        endif
        !
        call initcase(ctxt, iix, .true.)
        !---- rotor now exists
        ctxt%lrotor = .true.
        return
        !
        400   if (show_output) write (*, 99004) ctxt%fname(1:32)
        99004  format (' File  ', a, ' not found'/)
        return
        !
        500   if (show_output) write (*, 99005) ctxt%fname(1:32)
        99005  format (' File  ', a, ' has incompatible format'/' Loading not completed'/)
        close (lu)
        ctxt%conv = .false.
        return
        !..............................
        99006  format (a)
        !
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
