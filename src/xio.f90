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

subroutine save(ctxt, fname1)
    use m_userio
    use m_common
    implicit real(m)
    type(Common), intent(inout) :: ctxt
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
    lvduct = abs(ctxt%adw - ctxt%adv * ctxt%urduct) >= 5.e-5
    lu = ctxt%lutemp
    ctxt%fname = fname1
    !
    if(ctxt%fname(1:1) == ' ') call asks('enter filename^', ctxt%fname)
    open(lu, file = ctxt%fname, status = 'old', err = 5)
     if (show_output) write(*, *)
     if (show_output) write(*, *) 'Output file exists.  Overwrite?  y'
    read (*, 1000) ans
    if(index('nn', ans) == 0) go to 6
    !
    close(lu)
     if (show_output) write(*, *) 'Current rotor not saved.'
    return
    !
    5    open(lu, file = ctxt%fname, status = 'new', err = 90)
    6    rewind(lu)
    !
    !
    !--- Version header and case name
    if(ctxt%name == ' ') ctxt%name = 'saved blade'
    write(lu, 1100) ctxt%version, ctxt%name
    !--- Altitude and atmospheric data
    write(lu, 1102)
    write(lu, 1200) ctxt%rho, ctxt%vso, ctxt%rmu, ctxt%alt
    !--- Radius, velocity, advance ratio and blade rake angle
    write(lu, 1103)
    write(lu, 1200) ctxt%rad, ctxt%vel, ctxt%adv, ctxt%rake
    !
    write(lu, 1104)
    write(lu, 1200) ctxt%xi0, ctxt%xw0
    !--- Save aero data for defined aero sections
    write(lu, 1105) ctxt%naero
    mcrit = 0.8
    do n = 1, ctxt%naero
        call getaero(ctxt, n, xisect, a0, clmax, clmin, &
                dclda, dclda_stall, dcl_stall, &
                cdmin, cldmin, dcdcl2, cmcon, mcrit, reref, rexp)
        write(lu, 1106)
        write(lu, 1200) xisect
        a0deg = a0 * 180.0 / pi
        write(lu, 1107)
        write(lu, 1200) a0deg, dclda, clmax, clmin
        write(lu, 1108)
        write(lu, 1200) dclda_stall, dcl_stall, cmcon, mcrit
        write(lu, 1109)
        write(lu, 1200) cdmin, cldmin, dcdcl2
        write(lu, 1110)
        write(lu, 1200) reref, rexp
    end do
    !
    !--- Save logical flags for duct and windmill
    write(lu, 1111) lvduct, ctxt%duct, ctxt%wind
    !
    !--- #radial stations and #blades
    write(lu, 1112) ctxt%ii, ctxt%nblds
    !--- Save blade definition with chord,twist and body velocity
    do i = 1, ctxt%ii
        beta0deg = ctxt%beta0(i) * 180.0 / pi
        write(lu, 1200) ctxt%xi(i), ctxt%ch(i), beta0deg, ctxt%ubody(i)
    end do
    !--- Duct velocity
    write(lu, 1113)
    write(lu, 1200) ctxt%urduct
    !--- Save added velocity components
    if(ctxt%nadd > 1) then
        write(lu, 1114) ctxt%nadd
        do i = 1, ctxt%nadd
            write(lu, 1200) ctxt%radd(i), ctxt%uadd(i), ctxt%vadd(i)
        end do
         if (show_output) write(*, *) 'External slipstream included in save file'
    endif
    !
    close(lu)
    return
    !
    90    if (show_output) write(*, *) 'Bad filename.'
     if (show_output) write(*, *) 'Current rotor not saved.'
    return
    !
    !...................................................................
    1000 format(a)
    1050 format(/' *** Converged operating solution does not exist ***')
    1100 format('xrotor version: ', f5.2/a)
    1102 format('!         Rho          Vso          Rmu           Alt')
    1103 format('!         Rad          Vel          Adv          Rake')
    1104 format('!         xi0          xiw')
    1105 format('!  Naero'/1(1x, i5))
    1106 format('!   Xisection')
    1107 format('!       a0deg        dcLda        cLmax         cLmin')
    1108 format('!  dcLdAstall     dcLstall      Cmconst         Mcrit')
    1109 format('!       cDmin      clcDmin     dcDdcl^2')
    1110 format('!       rEref        rEexp')
    1111 format('!lvDuct  lDuct   lWind'/3(1x, l2, 5x))
    1112 format('!   ii Nblds'/2(1x, i5), &
            /'!         r/r          c/r     Beta0deg         Ubody')
    1113 format('!      urDuct')
    1114 format('!Nadd'/1(1x, i5), &
            /'!        Radd         Uadd         Vadd')
    1200 format(5(1x, g12.5))
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
    use m_userio
    use m_common
    implicit real (m)
    type(Common), intent(inout) :: ctxt
    character*(*) fname1
    character*128 line
    ctxt%greek = .false.
    !
    lu = ctxt%lutemp
    !
    ctxt%fname = fname1
    if(ctxt%fname(1:1) == ' ') call asks('enter filename^', ctxt%fname)
    open(lu, file = ctxt%fname, status = 'old', err = 200)
    !
    !--- Check for new format/old format xrotor file
    call rdline(lu, line)
    if(line == 'end' .or. line == 'err') go to 210
    read(line(17:22), *) filevers
     if (show_output) write(*, 1005) filevers
    !
    !
    !--- Case title
    call rdline(lu, line)
    ctxt%name = line
    !
    call rdline(lu, line)
    read(line, *, err = 210) ctxt%rho, ctxt%vso, ctxt%rmu, ctxt%alt
    call rdline(lu, line)
    read(line, *, err = 210) ctxt%rad, ctxt%vel, ctxt%adv, ctxt%rake
    call rdline(lu, line)
    read(line, *, err = 210) ctxt%xi0, ctxt%xw0
    call rdline(lu, line)
    !
    !--- Read aero section definitions
    read(line, *, err = 210) ctxt%naero
    do n = 1, ctxt%naero
        call rdline(lu, line)
        read(line, *, err = 210) xisect
        call rdline(lu, line)
        read(line, *, err = 210) a0deg, dclda, clmax, clmin
        call rdline(lu, line)
        read(line, *, err = 210) dclda_stall, dcl_stall, cmcon, mcrit
        call rdline(lu, line)
        read(line, *, err = 210) cdmin, cldmin, dcdcl2
        call rdline(lu, line)
        read(line, *, err = 210) reref, rexp
        !
        a0 = a0deg * pi / 180.0
        call putaero(ctxt, n, xisect, a0, clmax, clmin, &
                dclda, dclda_stall, dcl_stall, &
                cdmin, cldmin, dcdcl2, cmcon, mcrit, reref, rexp)
    end do
    !
    !--- Read flags for wake, duct and windmill modes
    call rdline(lu, line)
    read(line, *, err = 210) ctxt%free, ctxt%duct, ctxt%wind
    !
     if (show_output) write(*, *)
    if (ctxt%free .and. show_output) write(*, *) 'self-deforming wake option set'
    if (.not.ctxt%free .and. show_output) write(*, *) 'rigid wake option set'
    if (ctxt%duct .and. show_output) write(*, *) 'duct option set'
    if (.not.ctxt%duct .and. show_output) write(*, *) 'free-tip option set'
    if (ctxt%wind .and. show_output) write(*, *) 'windmill plotting mode set'
    if (.not.ctxt%wind .and. show_output) write(*, *) 'propeller plotting mode set'
    !
     if (show_output) write(*, *) ' '
    call rdline(lu, line)
    if(line == 'end' .or. line == 'err') go to 210
    read(line, *, err = 210) iix, ctxt%nblds
    do i = 1, iix
        call rdline(lu, line)
        read(line, *, err = 210) ctxt%xi(i), ctxt%ch(i), betadeg, ctxt%ubody(i)
        ctxt%beta(i) = betadeg * pi / 180.0
        ctxt%beta0(i) = ctxt%beta(i)
        !c        write(*,*) 'load i,ch,beta ',i,ch(i),beta(i)
    end do
    !
    !--- Optional duct velocity
    ctxt%urduct = 1.0
    call rdline(lu, line)
    if(line == 'end' .or. line == 'err') go to 19
    read(line, *, end = 19) ctxt%urduct
    !
    !---- Optional slipstream velocities
    19   ctxt%nadd = 0
    call rdline(lu, line)
    if(line == 'end' .or. line == 'err') go to 21
    read(line, *, end = 21) ctxt%nadd
    if(ctxt%nadd > ix) then
        ctxt%nadd = ix
         if (show_output) write(*, *) 'Warning, slipstream data terminated at ', ix
    endif
    do i = 1, ctxt%nadd
        call rdline(lu, line)
        if(line == 'end' .or. line == 'err') go to 20
        read(line, *, err = 20, end = 20) ctxt%radd(i), ctxt%uadd(i), ctxt%vadd(i)
    end do
    if(i < ctxt%nadd) then
        ctxt%nadd = i - 1
         if (show_output) write(*, *) 'warning, slipstream data terminated at ', ctxt%nadd
    endif
    go to 21
    !
    20   if(i > 2) then
        ctxt%nadd = i - 1
    endif
    !
    21   close(lu)
    if(ctxt%nadd > 1) then
         if (show_output) write(*, *)
         if (show_output) write(*, *) 'slipstream profiles read with #points ', ctxt%nadd
    endif
    !
    ctxt%conv = .false.
    !
    !--- Check for number of analysis stations to use
    if(iix /= ctxt%ii) then
        22      if (show_output) write(*, 23) iix, ctxt%ii, ctxt%ii
        read(*, 24) line
        if(line /= ' ') then
            read(line, *, err = 22) ctxt%ii
        endif
        23     format(/'Read  # input stations = ', i3, &
                /'Using # blade stations = ', i3, &
                /'Enter # stations or <cr> for ', i3, ' ', $)
        24     format(a)
    endif
    !
    call initcase(ctxt, iix, .true.)
    !---- rotor now exists
    ctxt%lrotor = .true.
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
    1000 format(a)
    1005 format(' Reading file from xrotor Version ', f5.2)
    1010 format(' File  ', a, ' not found'/)
    1020 format(' File  ', a, ' has incompatible format'/&
            ' Loading not completed'/)
    !
end

subroutine initcase(ctxt, iix, losolve)
    use m_common
    use m_spline
    implicit real (m)
    type(Common), intent(inout) :: ctxt
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
    if(losolve) then
        call aper(ctxt, 4, 2, .true.)
        if(ctxt%conv) then
            call output(ctxt, ctxt%luwrit)
        end if
    end if
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
    !...Purpose  Read a non-comment line from the input file
    !...Input    Data read from unit lun
    !...Output   line  Character string with input line
    !                  line is set to 'end' for end or errors
    !
    character*(*) line
    !
    1000 format(a)
    20 read (lun, 1000, end = 80, err = 90) line
    !
    !---- skip comment line
    if(index('!#', line(1:1)) /= 0) go to 20
    !
    !---- skip blank line
    if(line == ' ') go to 20
    !
    !---- normal return after significant line
    return
    !
    80 line = 'end '
    return
    !
    90 line = 'err '
    return
end




