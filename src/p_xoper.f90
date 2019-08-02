!*==P_XOPER.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
module p_xoper
    implicit none
contains
    subroutine oper(ctxt)
        use m_xrotor, only : output, flosho, setx, atmo, reinit, opfile
        use m_xoper, only : getcas, getpvar, aper, shocas, setcas
        use m_userio, only : aski, askr, getflt, asks, getint, askc
        use i_common, only : Common, show_output, ix, pi, icasx, nparx
        use m_spline, only : spline, seval
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real BET, BSAV, DELB, EFF, PLFAC1, PLFAC2, PLFACD, POW, PWR, RINPUT, &
                & RPM, SPLINA, THR, TRQ, VELOLD, XORG, YORG
        integer I, ICASE, IINPUT, IISAV, L, LU, NF, NINPUT
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        character*4 comand, ans
        character*132 comarg, ansarg
        character*1 chkey
        !
        dimension iinput(20)
        dimension rinput(20)
        logical error
        !
        !---------------------------------------------
        !     Run rotor at arbitrary operating points
        !---------------------------------------------
        plfac1 = 0.7
        plfac2 = 0.8
        plfacd = 0.6
        xorg = 0.15
        yorg = 0.10
        !
        ctxt%greek = .false.
        100   do
            !
            call askc('.oper^', comand, comarg)
            !
            do i = 1, 20
                iinput(i) = 0
                rinput(i) = 0.0
            enddo
            ninput = 0
            call getint(comarg, iinput, ninput, error)
            ninput = 0
            call getflt(comarg, rinput, ninput, error)
            !
            if (comand=='    ') return
            if (comand=='?   '.and.show_output) write (*, 99001)
            99001      format (/'   adva r   Prescribe advance ratio'/                      &
                    &'   rpm  r   Prescribe rpm'/'   thru r   Prescribe thrust'/  &
                    &'   torq r   Prescribe torque'/'   powe r   Prescribe power' &
                    & //'   aseq rrr Calculate case sequence of advance ratios'/   &
                    &'   rseq rrr Calculate case sequence of rpms'/               &
                    &'   bseq rrr Calculate case sequence of blade angles'/       &
                    &'   vseq rrr Calculate case sequence of speeds at fixed pitch'&
                    & /'   clrc     Clear case accumulator'/                        &
                    &'   addc     Add current point point to case accumulator'/    &
                    &'   cput f   Write current case accumulator to file'/         &
                    &'   cget f   Read cases from file'/'   case i   Select case'//&
                    &'   atmo r   Set fluid properties from standard atmosphere'/  &
                    &'   velo r   Set or change flight speed'/                     &
                    &'   angl r   Change blade pitch angle'/                       &
                    &'   pvar f   Enter and use engine rpm/power line'//           &
                    &'   form     Select slipstream and velocity formulation'//    &
                    &'   name s   Set or change case name'/                        &
                    &'   writ f   Write current operating point to disk file'/     &
                    &'   disp     Display current operating state'/                &
                    &'   init     Initialize next analysis case'/                  &
                    &'   rein     Re-initialize prop to known operating state'/    &
                    &'   ters     Toggle between terse and verbose output'/        &
                    &'   iter i   Change max number of Newton iterations'/         &
                    &'   n    i   Change number of radial points')
            if (comand/='?   ') then
                if (comand=='form') then
                    do
                        !
                        !---------------------------------------------------------------------
                        !--- Select options for slipstream and velocity calculation
                        if (show_output) write (*, 99002)
                        !
                        99002                  format (/                                                &
                                &' Select options for calculation of slipstream velocities'&
                                & /'   grad     use Graded Momentum       Formulation '/    &
                                &'   pot      use Potential (Goldstein) Formulation '/     &
                                &'   vrtx     use discrete Vortex Wake  Formulation '/     &
                                &'   wake     Toggle between rigid and self-deforming wake'&
                                &)
                        call askc('.form^', comand, comarg)
                        !
                        if (comand=='grad') then
                            ctxt%vrtx = .false.
                            ctxt%fast = .true.
                        elseif (comand=='pot') then
                            ctxt%vrtx = .false.
                            ctxt%fast = .false.
                        elseif (comand=='vrtx') then
                            ctxt%vrtx = .true.
                        elseif (comand=='wake') then
                            ctxt%free = .not.ctxt%free
                        elseif (comand==' ') then
                            exit
                        endif
                        !
                        if (ctxt%vrtx) then
                            if (show_output) write (*, *)                       &
                                    &'Discrete Vortex Formulation selected'
                        elseif (ctxt%fast) then
                            if (show_output) write (*, *)                       &
                                    &'Graded Momentum Formulation selected'
                        else
                            if (show_output) write (*, *)                       &
                                    &'Potential Formulation selected'
                        endif
                        !
                        if (ctxt%free) then
                            if (show_output) write (*, *)                       &
                                    &'Self-deforming wake selected'
                        else
                            if (show_output) write (*, *) 'Rigid wake selected'
                        endif
                    enddo
                elseif (comand=='ters') then
                    !
                    !
                    !---------------------------------------------------------------------
                    !--- Output data on blade stations with each case (verbose)
                    ctxt%terse = .not.ctxt%terse
                    if (ctxt%terse.and.show_output) write (*, *)                &
                            & 'terse output selected'
                    if (.not.ctxt%terse.and.show_output) write (*, *)           &
                            & 'verbose output selected'
                elseif (comand=='disp') then
                    !
                    !---------------------------------------------------------------------
                    !--- Display current prop operating point data
                    !cc      call cproj
                    call output(ctxt, ctxt%luwrit)
                elseif (comand=='name') then
                    !
                    !---------------------------------------------------------------------
                    !--- Change case name
                    ctxt%name = comarg
                    if (ctxt%name(1:1)==' ') call asks(&
                            &'enter case ctxt%name (32 characters max)^'&
                            &, ctxt%name)
                elseif (comand=='writ') then
                    !
                    !---------------------------------------------------------------------
                    !--- Write current prop operating point data to file
                    if (comarg(1:1)/=' ') ctxt%savfil = comarg
                    call opfile(ctxt%lusave, ctxt%savfil)
                    call output(ctxt, ctxt%lusave)
                    close (ctxt%lusave)
                elseif (comand=='duct') then
                    !
                    !--------------------------------------------------------------
                    ctxt%duct = .not.ctxt%duct
                    if (ctxt%duct) then
                        if (show_output) write (*, *) 'duct option selected'
                        if (ninput>=1) then
                            ctxt%urduct = rinput(1)
                        else
                            call askr('enter aexit/aprop for ctxt%duct^', &
                                    & ctxt%urduct)
                        endif
                    else
                        if (show_output) write (*, *) 'free-tip option selected'
                        ctxt%urduct = 1.0
                    endif
                elseif (comand=='vrat') then
                    !
                    !--------------------------------------------------------------
                    if (.not.(ctxt%duct)) then
                        if (show_output) write (*, *)                           &
                                &'*** select ctxt%duct option first'
                    elseif (ninput>=1) then
                        ctxt%urduct = rinput(1)
                    else
                        call askr('enter aexit/aprop for ctxt%duct^', ctxt%urduct)
                    endif
                elseif (comand=='atmo') then
                    !
                    !---------------------------------------------------------------------
                    !--- Change altitude
                    if (ninput>=1) then
                        ctxt%alt = rinput(1)
                    else
                        call askr('flight altitude (km)^', ctxt%alt)
                    endif
                    call atmo(ctxt%alt, ctxt%vso, ctxt%rho, ctxt%rmu)
                    call flosho(ctxt%luwrit, ctxt%vso, ctxt%rho, ctxt%rmu)
                elseif (comand=='velo') then
                    !
                    !---------------------------------------------------------------------
                    !--- Change flight velocity
                    velold = ctxt%vel
                    if (ninput>=1) then
                        ctxt%vel = rinput(1)
                    else
                        call askr('flight speed (m/s)^', ctxt%vel)
                    endif
                    !--- Change ct,cq,cp to give same thrust,torque,power
                    thr = ctxt%ttot * (ctxt%rho * velold**2 * ctxt%rad**2)
                    ctxt%ttot = thr / (ctxt%rho * ctxt%vel**2 * ctxt%rad**2)
                    trq = ctxt%qtot * (ctxt%rho * velold**2 * ctxt%rad**3)
                    ctxt%qtot = trq / (ctxt%rho * ctxt%vel**2 * ctxt%rad**3)
                    pwr = ctxt%ptot * (ctxt%rho * velold**3 * ctxt%rad**2)
                    ctxt%ptot = pwr / (ctxt%rho * ctxt%vel**3 * ctxt%rad**2)
                    ctxt%conv = .false.
                elseif (comand=='angl') then
                    !
                    !---------------------------------------------------------------------
                    !--- Change blade pitch
                    if (ninput>=1) then
                        delb = rinput(1)
                    else
                        call askr('angle change (deg)^', delb)
                    endif
                    do i = 1, ctxt%ii
                        ctxt%beta(i) = ctxt%beta(i) + delb * pi / 180.
                        ctxt%beta0(i) = ctxt%beta0(i) + delb * pi / 180.
                    enddo
                    ctxt%conv = .false.
                elseif (comand=='adva') then
                    !
                    !---------------------------------------------------------------------
                    !--- Specify advance ratio and solve
                    if (ninput>=1) then
                        ctxt%adv = rinput(1)
                    else
                        call askr('advance ratio     ^', ctxt%adv)
                    endif
                    ctxt%conv = .false.
                    call aper(ctxt, 4, 2, ctxt%loprini)
                    !
                    if (ctxt%conv) call output(ctxt, ctxt%luwrit)
                elseif (comand=='rpm ') then
                    !
                    !---------------------------------------------------------------------
                    !--- Specify rpm and solve
                    if (ninput>=1) then
                        rpm = rinput(1)
                    else
                        rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.)
                        call askr('rpm               ^', rpm)
                    endif
                    ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.)
                    ctxt%conv = .false.
                    call aper(ctxt, 4, 2, ctxt%loprini)
                    !
                    if (ctxt%conv) call output(ctxt, ctxt%luwrit)
                elseif (comand=='thru') then
                    !
                    !---------------------------------------------------------------------
                    !--- Specify thrust and solve
                    if (ninput>=1) then
                        ctxt%tspec = rinput(1)
                    else
                        ctxt%tspec = ctxt%ttot * (ctxt%rho * ctxt%vel**2 * ctxt%rad**2)
                        call askr('thrust (n)        ^', ctxt%tspec)
                    endif
                    rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.0)
                    if (show_output) write (*, 99005) rpm
                    do
                        call askc('fix Pitch / fix Rpm ( p/r )?^', ans, ansarg)
                        if (ans=='r'.or.ans=='p') then
                            !
                            ctxt%conv = .false.
                            bsav = ctxt%beta(ctxt%ii)
                            if (ans=='p') call aper(ctxt, 1, 2, ctxt%loprini)
                            if (ans=='r') then
                                call askr('rpm:^', rpm)
                                ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.0)
                                call aper(ctxt, 1, 1, ctxt%loprini)
                            endif
                            !
                            if (ctxt%conv) call output(ctxt, ctxt%luwrit)
                            !---- Check for valid blade angle change
                            if (ans/='p') then
                                if (ctxt%conv) then
                                    !----- convergence was achieved: show blade angle change incurred
                                    if (show_output) write (*, 99006)           &
                                            & ctxt%dbeta * 180.0 / pi
                                else
                                    !----- convergence failed: restore clobbered blade angles
                                    do i = 1, ctxt%ii
                                        ctxt%beta(i) = ctxt%beta(i) - ctxt%dbeta
                                        ctxt%beta0(i) = ctxt%beta0(i)            &
                                                & - ctxt%dbeta
                                    enddo
                                endif
                            endif
                            exit
                        endif
                    enddo
                elseif (comand=='torq') then
                    !
                    !---------------------------------------------------------------------
                    !--- Specify torque and solve
                    if (ninput>=1) then
                        ctxt%qspec = rinput(1)
                    else
                        ctxt%qspec = ctxt%qtot * (ctxt%rho * ctxt%vel**2 * ctxt%rad**3)
                        call askr('torque (n-m)      ^', ctxt%qspec)
                    endif
                    rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.0)
                    if (show_output) write (*, 99005) rpm
                    do
                        call askc('fix Pitch / fix Rpm ( p/r )?^', ans, ansarg)
                        if (ans=='r'.or.ans=='p') then
                            !
                            ctxt%conv = .false.
                            if (ans=='p') call aper(ctxt, 2, 2, ctxt%loprini)
                            if (ans=='r') then
                                call askr('rpm:^', rpm)
                                ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.0)
                                call aper(ctxt, 2, 1, ctxt%loprini)
                            endif
                            !
                            if (ctxt%conv) call output(ctxt, ctxt%luwrit)
                            !---- Check for valid blade angle change
                            if (ans/='p') then
                                if (ctxt%conv) then
                                    !----- convergence was achieved: show blade angle change incurred
                                    if (show_output) write (*, 99006)           &
                                            & ctxt%dbeta * 180.0 / pi
                                else
                                    !----- convergence failed: restore clobbered blade angles
                                    do i = 1, ctxt%ii
                                        ctxt%beta(i) = ctxt%beta(i) - ctxt%dbeta
                                        ctxt%beta0(i) = ctxt%beta0(i)            &
                                                & - ctxt%dbeta
                                    enddo
                                endif
                            endif
                            exit
                        endif
                    enddo
                elseif (comand=='powe') then
                    !
                    !---------------------------------------------------------------------
                    !--- Specify power and solve
                    if (ninput>=1) then
                        ctxt%pspec = rinput(1)
                    else
                        ctxt%pspec = ctxt%ptot * (ctxt%rho * ctxt%vel**3 * ctxt%rad**2)
                        call askr('power (w)         ^', ctxt%pspec)
                    endif
                    rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.0)
                    if (show_output) write (*, 99005) rpm
                    do
                        call askc('fix pitch / fix rpm ( p/r )?^', ans, ansarg)
                        if (ans=='r'.or.ans=='p') then
                            !
                            ctxt%conv = .false.
                            if (ans=='p') call aper(ctxt, 3, 2, ctxt%loprini)
                            if (ans=='r') then
                                call askr('rpm:^', rpm)
                                ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.0)
                                call aper(ctxt, 3, 1, ctxt%loprini)
                            endif
                            !
                            if (ctxt%conv) call output(ctxt, ctxt%luwrit)
                            !---- Check for valid blade angle change
                            if (ans/='p') then
                                if (ctxt%conv) then
                                    !----- convergence was achieved: show blade angle change incurred
                                    if (show_output) write (*, 99006)           &
                                            & ctxt%dbeta * 180.0 / pi
                                else
                                    !----- convergence failed: restore clobbered blade angles
                                    do i = 1, ctxt%ii
                                        ctxt%beta(i) = ctxt%beta(i) - ctxt%dbeta
                                        ctxt%beta0(i) = ctxt%beta0(i)            &
                                                & - ctxt%dbeta
                                    enddo
                                endif
                            endif
                            exit
                        endif
                    enddo
                elseif (comand=='aseq') then
                    !
                    !---------------------------------------------------------------------
                    !--- Do sequence of advance ratios
                    if (show_output) write (*, *) ' '
                    if (show_output) write (*, *)                               &
                            &'Sequence of advance ratios...'
                    call setcas(ctxt, 1, ninput, rinput)
                    call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, &
                            & ctxt%rad, ctxt%name)
                elseif (comand=='rseq') then
                    !
                    !---------------------------------------------------------------------
                    !--- Do sequence of rpMs
                    if (show_output) write (*, *) ' '
                    if (show_output) write (*, *) 'Sequence of rpMs...'
                    call setcas(ctxt, 2, ninput, rinput)
                    call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, &
                            & ctxt%rad, ctxt%name)
                elseif (comand=='bseq') then
                    !
                    !---------------------------------------------------------------------
                    !--- Do sequence of pitch angles
                    if (show_output) write (*, *) ' '
                    if (show_output) write (*, *) 'Sequence of blade angles...'
                    call setcas(ctxt, 3, ninput, rinput)
                    call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, &
                            & ctxt%rad, ctxt%name)
                elseif (comand=='vseq') then
                    !
                    !---------------------------------------------------------------------
                    !--- Do sequence of velocities
                    if (show_output) write (*, *) ' '
                    if (show_output) write (*, *)                               &
                            &'Sequence of velocity with fixed pitch or rpm...'
                    call setcas(ctxt, 4, ninput, rinput)
                    call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, &
                            & ctxt%rad, ctxt%name)
                elseif (comand=='clrc') then
                    !
                    !---------------------------------------------------------------------
                    !--- Save current operating point to case arrays
                    ctxt%ncase = 0
                    ctxt%kcase = 0
                elseif (comand=='addc') then
                    !
                    if (ctxt%ncase>=icasx) then
                        if (show_output) write (*, *)                           &
                                &'Case arrays too small.  Increase icasx.'
                        cycle
                    endif
                    !
                    ctxt%ncase = ctxt%ncase + 1
                    ctxt%caspar(1, ctxt%ncase) = ctxt%adv
                    ctxt%caspar(2, ctxt%ncase) = ctxt%vel
                    ctxt%caspar(3, ctxt%ncase) = ctxt%beta(ctxt%ii)
                    ctxt%caspar(4, ctxt%ncase) = ctxt%alt
                    ctxt%caspar(5, ctxt%ncase) = ctxt%rho
                    ctxt%caspar(6, ctxt%ncase) = ctxt%rmu
                    ctxt%caspar(7, ctxt%ncase) = ctxt%vso
                    ctxt%caspar(8, ctxt%ncase) = 999.
                    ctxt%caspar(9, ctxt%ncase) = 999.
                    ctxt%caspar(10, ctxt%ncase) = 999.
                    ctxt%caspar(11, ctxt%ncase) = 999.
                else
                    if (comand=='cput') goto 400
                    if (comand=='cget') then
                        !
                        !---------------------------------------------------------------------
                        !--- Read case accumulation arrays from saved file
                        lu = 12
                        ctxt%fname = comarg
                        if (ctxt%fname(1:1)==' ')                              &
                                & call asks('enter case save filename^', ctxt%fname)
                        open (lu, file = ctxt%fname, status = 'old', err = 700)
                        call getcas(lu, nparx, ctxt%ncase, ctxt%caspar)
                        close (lu)
                        call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, &
                                & ctxt%rad, ctxt%name)
                    elseif (comand=='case') then
                        do
                            !
                            !---------------------------------------------------------------------
                            !--- Rerun case operating point
                            if (ctxt%ncase<=0) then
                                if (show_output) write (*, *)
                                if (show_output) write (*, *) 'No cases saved'
                                exit
                            endif
                            !
                            if (ninput>=1) then
                                icase = iinput(1)
                            else
                                call shocas(ctxt%luwrit, nparx, ctxt%ncase, &
                                        & ctxt%caspar, ctxt%rad, ctxt%name)
                                icase = 0
                                call aski('Select case number (0 to cancel)^', &
                                        & icase)
                            endif
                            if (icase>0) then
                                if (icase>ctxt%ncase) then
                                    ninput = 0
                                    cycle
                                endif
                                !
                                ctxt%adv = ctxt%caspar(1, icase)
                                ctxt%vel = ctxt%caspar(2, icase)
                                bet = ctxt%caspar(3, icase)
                                ctxt%alt = ctxt%caspar(4, icase)
                                ctxt%rho = ctxt%caspar(5, icase)
                                ctxt%rmu = ctxt%caspar(6, icase)
                                ctxt%vso = ctxt%caspar(7, icase)
                                pow = ctxt%caspar(8, icase)
                                thr = ctxt%caspar(9, icase)
                                trq = ctxt%caspar(10, icase)
                                eff = ctxt%caspar(11, icase)
                                !
                                delb = bet - ctxt%beta(ctxt%ii)
                                do i = 1, ctxt%ii
                                    ctxt%beta(i) = ctxt%beta(i) + delb
                                    ctxt%beta0(i) = ctxt%beta0(i) + delb
                                enddo
                                !
                                ctxt%conv = .false.
                                call aper(ctxt, 4, 2, .true.)
                                !
                                if (ctxt%conv) then
                                    ctxt%caspar(8, icase) = ctxt%ptot * ctxt%rho * &
                                            & ctxt%vel**3 * ctxt%rad**&
                                            & 2
                                    ctxt%caspar(9, icase) = ctxt%ttot * ctxt%rho * &
                                            & ctxt%vel**2 * ctxt%rad**&
                                            & 2
                                    ctxt%caspar(10, icase)                        &
                                            & = ctxt%qtot * ctxt%rho * ctxt%vel**2 * ctxt%rad&
                                            & **3
                                    ctxt%caspar(11, icase) = ctxt%ttot / ctxt%ptot
                                endif
                                !
                                if (ctxt%conv) call output(ctxt, ctxt%luwrit)
                            endif
                            exit
                        enddo
                    elseif (comand=='list') then
                        !
                        !---------------------------------------------------------------------
                        !--- List rotor dimensional data ?
                        do i = 1, ctxt%ii
                            if (show_output) write (*, *) ctxt%xi(i), &
                                    & ctxt%nblds * ctxt%gam(i)&
                                            & * ctxt%rad * ctxt%vel, &
                                    & ctxt%vind(3, i)        &
                                            & * ctxt%vel, &
                                    & ctxt%nblds * ctxt%gam(i)&
                                            & / &
                                            & (4.0 * pi * ctxt%vind(3, i)&
                                                    & * ctxt%xi(i))
                        enddo
                        !
                    elseif (comand=='n') then
                        !
                        !---------------------------------------------------------------------
                        !--- Change number of radial points for blade stations
                        if (ctxt%lrotor) then
                            iisav = ctxt%ii
                            do i = 1, iisav
                                ctxt%w1(i) = ctxt%xi(i)
                                ctxt%w2(i) = ctxt%ch(i)
                                ctxt%w4(i) = ctxt%beta(i)
                                ctxt%w6(i) = ctxt%ubody(i)
                                ctxt%w8(i) = ctxt%cldes(i)
                            enddo
                            ctxt%w3(1:ctxt%ii) = spline(ctxt%w1(1:ctxt%ii), ctxt% &
                                    & w2(1:ctxt%ii))
                            ctxt%w5(1:ctxt%ii) = spline(ctxt%w1(1:ctxt%ii), ctxt% &
                                    & w4(1:ctxt%ii))
                            ctxt%w7(1:ctxt%ii) = spline(ctxt%w1(1:ctxt%ii), ctxt% &
                                    & w6(1:ctxt%ii))
                            ctxt%w9(1:ctxt%ii) = spline(ctxt%w1(1:ctxt%ii), ctxt% &
                                    & w8(1:ctxt%ii))
                        endif
                        do
                            !
                            call aski('enter new number of radial points^', &
                                    & ctxt%ii)
                            if (ctxt%ii>ix) then
                                if (show_output) write (*, *)
                                if (show_output) write (*, *)                   &
                                        & 'Maximum number is', &
                                        & ix
                                cycle
                            endif
                            !
                            ctxt%iinf = ctxt%ii + ctxt%ii / 2
                            call setx(ctxt)
                            if (ctxt%lrotor) then
                                do i = 1, ctxt%ii
                                    ctxt%ch(i) = seval(ctxt%xi(i), ctxt%w2, ctxt%w3&
                                            &, ctxt%w1)
                                    ctxt%beta(i) = seval(ctxt%xi(i), ctxt%w4, ctxt%&
                                            & w5, ctxt%w1)
                                    ctxt%ubody(i) = seval(ctxt%xi(i), ctxt%w6, ctxt&
                                            & %w7, ctxt%w1)
                                    ctxt%cldes(i) = seval(ctxt%xi(i), ctxt%w8, ctxt&
                                            & %w9, ctxt%w1)
                                    ctxt%beta0(i) = ctxt%beta(i)
                                enddo
                            endif
                            exit
                        enddo
                    elseif (comand=='iter') then
                        !
                        !---------------------------------------------------------------------
                        !--- Set max number or iterations for nonlinear solution
                        if (ninput>=1) then
                            ctxt%nitera = iinput(1)
                        else
                            call aski('max number of iterations^', ctxt%nitera)
                        endif
                    elseif (comand=='init') then
                        !
                        !---------------------------------------------------------------------
                        !--- Toggle initialization flag
                        ctxt%loprini = .not.ctxt%loprini
                        if (ctxt%loprini) then
                            if (show_output) write (*, *)                       &
                                    &'Analysis case will be initialized'
                        else
                            if (show_output) write (*, *)                       &
                                    &'Analysis case will not be initialized'
                        endif
                    elseif (comand=='rein') then
                        !
                        !---------------------------------------------------------------------
                        !--- Reinitialize operating point
                        call reinit(ctxt)
                        cycle
                        !
                        !---------------------------------------------------------------------
                        !--- Read or use engine rpm/power line file
                        if (ctxt%lpwrvar.and.ctxt%npwrvar>0) then
                            if (show_output) write (*, *) ' '
                            if (show_output) write (*, *)                       &
                                    &'Current rpm/Power Engine Line'
                            do l = 1, ctxt%npwrvar
                                if (show_output) write (*, *) l, ctxt%rpmvar(l), &
                                        & ctxt%pwrvar(l)
                            enddo
                            if (show_output) write (*, *) ' '
                        endif
                        !
                        lu = 12
                        ctxt%fname = comarg
                        if (ctxt%fname(1:1)==' ')                              &
                                & call asks('enter power/rpm filename^', ctxt%fname)
                        if (ctxt%fname(1:1)/=' ') then
                            open (lu, file = ctxt%fname, status = 'old', err = 300)
                            call getpvar(lu, ix, ctxt%npwrvar, ctxt%rpmvar, &
                                    & ctxt%pwrvar)
                            if (show_output) write (*, *) ' '
                            if (show_output) write (*, *)                       &
                                    &'rpm/Power Engine Line'
                            do l = 1, ctxt%npwrvar
                                if (show_output) write (*, *) l, ctxt%rpmvar(l), &
                                        & ctxt%pwrvar(l)
                                ctxt%pwrvar(l) = ctxt%pwrvar(l)
                            enddo
                            if (show_output) write (*, *) ' '
                            ctxt%xpwrvar = splina(ctxt%rpmvar, ctxt%pwrvar)
                            close (lu)
                            ctxt%lpwrvar = .true.
                        endif
                        !
                        !
                        !--- Use the engine rpm/power to define operating point
                        if (ctxt%lpwrvar) then
                            do
                                call askc(&
                                        &'fix Pitch / fix Rpm / fix Velocity ( p/r/v )?^'&
                                        &, ans, ansarg)
                                if (ans==' ') goto 200
                                if (ans=='r'.or.ans=='p'.or.ans=='v') then
                                    !
                                    ctxt%conv = .false.
                                    if (ans=='p')                              &
                                            & call aper(ctxt, 5, 2, ctxt%loprini)
                                    if (ans=='r') then
                                        call askr('rpm:^', rpm)
                                        ctxt%adv = ctxt%vel / &
                                                & (ctxt%rad * rpm * pi / 30.0)
                                        call aper(ctxt, 5, 1, ctxt%loprini)
                                    endif
                                    if (ans=='v') then
                                        call askr('vel:^', ctxt%vel)
                                        ctxt%adv = ctxt%vel / &
                                                & (ctxt%rad * rpm * pi / 30.0)
                                        call aper(ctxt, 5, 2, ctxt%loprini)
                                    endif
                                    !
                                    if (ctxt%conv)                             &
                                            & call output(ctxt, ctxt%luwrit)
                                    !---- Was the pitch changed?
                                    if (ans=='r') then
                                        if (ctxt%conv) then
                                            !----- convergence was achieved: show blade angle change incurred
                                            if (show_output) write (*, 99006)   &
                                                    & ctxt%dbeta * 180.0 / pi
                                        else
                                            !----- convergence failed: restore clobbered blade angles
                                            do i = 1, ctxt%ii
                                                ctxt%beta(i) = ctxt%beta(i)      &
                                                        & - ctxt%dbeta
                                                ctxt%beta0(i) = ctxt%beta0(i)    &
                                                        & - ctxt%dbeta
                                            enddo
                                        endif
                                    endif
                                    goto 200
                                endif
                            enddo
                        endif
                        exit
                    else
                        !
                        !--- Hack to check adw equation sensitivity, get rid of this later... hhy
                        if (comand=='adw') then
                            if (show_output) write (*, *)                       &
                                    &'current ctxt%adw factor ='&
                                    &, ctxt%adwfctr
                            call askr('enter new ctxt%adw factor^', ctxt%adwfctr)
                            cycle
                        endif
                        !
                        if (show_output) write (*, 99003) comand
                        99003                  format (1x, a4, ' command not recognized.'//               &
                                &'  Type "?" for list, <Return> to exit menu.')
                    endif
                endif
            endif
        200   enddo
        !
        300   nf = index(ctxt%fname, ' ') - 1
        if (show_output) write (*, *) 'open error on file  ', ctxt%fname(1:nf)
        goto 100
        !
        !---------------------------------------------------------------------
        !--- Write case accumulation arrays to file
        400   if (ctxt%ncase<=0) then
            if (show_output) write (*, *)
            if (show_output) write (*, *) 'No cases saved'
            goto 100
        endif
        !
        lu = 12
        ctxt%fname = comarg
        if (ctxt%fname(1:1)==' ') call asks('enter case save filename^', &
                & ctxt%fname)
        open (lu, file = ctxt%fname, status = 'old', err = 500)
        if (show_output) write (*, *) 'File exists.  Overwrite?  y'
        read (*, 99004) chkey

        !.......................................................................
        !
        99004  format (a)
        if (index('nn', chkey)/=0) then
            close (lu)
            goto 100
        else
            rewind lu
            goto 600
        endif
        !
        500   open (lu, file = ctxt%fname, status = 'unknown', err = 400)
        600   call shocas(lu, nparx, ctxt%ncase, ctxt%caspar, ctxt%rad, ctxt%name)
        close (lu)
        goto 100
        !
        700   nf = index(ctxt%fname, ' ') - 1
        if (show_output) write (*, *) 'open error on file  ', ctxt%fname(1:nf)
        goto 100
        99005  format (/' Current rpm:', f9.2)
        99006  format (' Blade angle changed', f7.3, ' degrees')
        !
    end
    ! oper
end
