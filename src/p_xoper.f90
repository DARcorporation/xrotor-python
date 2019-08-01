module p_xoper
contains
    subroutine oper(ctxt)
        use m_xrotor, only : output, flosho, setx, atmo, reinit, opfile
        use m_xoper, only : getcas, getpvar, aper, shocas, setcas
        use m_userio, only : aski, askr, getflt, asks, getint, askc
        use i_common, only : Common, show_output
        use m_spline, only : spline, seval
        implicit real (m)
        type(Common), intent(inout) :: ctxt
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
        !
        900  continue
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
        if(comand == '    ') then
            return
        endif
        if (comand == '?   ' .and. show_output) write(*, 1100)
        if(comand == '?   ') go to 900
        if(comand == 'form') go to 2
        if(comand == 'ters') go to 4
        if(comand == 'disp') go to 10
        if(comand == 'name') go to 15
        if(comand == 'writ') go to 20
        if(comand == 'duct') go to 22
        if(comand == 'vrat') go to 24
        if(comand == 'atmo') go to 35
        if(comand == 'velo') go to 38
        if(comand == 'angl') go to 40
        if(comand == 'adva') go to 42
        if(comand == 'rpm ') go to 45
        if(comand == 'thru') go to 50
        if(comand == 'torq') go to 60
        if(comand == 'powe') go to 70
        if(comand == 'aseq') go to 81
        if(comand == 'rseq') go to 82
        if(comand == 'bseq') go to 83
        if(comand == 'vseq') go to 84
        if(comand == 'clrc') go to 90
        if(comand == 'addc') go to 92
        if(comand == 'cput') go to 94
        if(comand == 'cget') go to 96
        if(comand == 'case') go to 97
        if(comand == 'list') go to 98
        !
        if(comand == 'n')    go to 72
        if(comand == 'iter') go to 75
        if(comand == 'init') go to 76
        if(comand == 'rein') go to 78
        !
        !--- Hack to check adw equation sensitivity, get rid of this later... hhy
        if(comand == 'adw') then
            if (show_output) write(*, *) 'current ctxt%adw factor =', ctxt%adwfctr
            call askr('enter new ctxt%adw factor^', ctxt%adwfctr)
            go to 900
        endif
        !
        if (show_output) write(*, 1050) comand
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Select options for slipstream and velocity calculation
        2    continue
        if (show_output) write(*, 3)
        call askc('.form^', comand, comarg)
        !
        if(comand == 'grad') then
            ctxt%vrtx = .false.
            ctxt%fast = .true.
        elseif(comand == 'pot') then
            ctxt%vrtx = .false.
            ctxt%fast = .false.
        elseif(comand == 'vrtx') then
            ctxt%vrtx = .true.
        elseif(comand == 'wake') then
            ctxt%free = .not.ctxt%free
        elseif(comand == ' ') then
            go to 900
        endif
        !
        if(ctxt%vrtx) then
            if (show_output) write(*, *)'Discrete Vortex Formulation selected'
        else
            if(ctxt%fast) then
                if (show_output) write(*, *) 'Graded Momentum Formulation selected'
            else
                if (show_output) write(*, *)'Potential Formulation selected'
            endif
        endif
        !
        if(ctxt%free) then
            if (show_output) write(*, *)'Self-deforming wake selected'
        else
            if (show_output) write(*, *)'Rigid wake selected'
        endif
        go to 2
        !
        3    format(&
                /' Select options for calculation of slipstream velocities'&
                /'   grad     use Graded Momentum       Formulation '&
                /'   pot      use Potential (Goldstein) Formulation '&
                /'   vrtx     use discrete Vortex Wake  Formulation '&
                /'   wake     Toggle between rigid and self-deforming wake')
        !
        !
        !---------------------------------------------------------------------
        !--- Output data on blade stations with each case (verbose)
        4 ctxt%terse = .not.ctxt%terse
        if (ctxt%terse .and. show_output) write(*, *)'terse output selected'
        if (.not.ctxt%terse .and. show_output) write(*, *)'verbose output selected'
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Display current prop operating point data
        10 call output(ctxt, ctxt%luwrit)
        !cc      call cproj
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Change case name
        15 ctxt%name = comarg
        if(ctxt%name(1:1) == ' ')&
                call asks('enter case ctxt%name (32 characters max)^', ctxt%name)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Write current prop operating point data to file
        20 if(comarg(1:1) /= ' ') ctxt%savfil = comarg
        call opfile(ctxt%lusave, ctxt%savfil)
        call output(ctxt, ctxt%lusave)
        close(ctxt%lusave)
        go to 900
        !
        !--------------------------------------------------------------
        22   ctxt%duct = .not.ctxt%duct
        if(ctxt%duct) then
            if (show_output) write(*, *) 'duct option selected'
            if(ninput >= 1) then
                ctxt%urduct = rinput(1)
            else
                call askr('enter aexit/aprop for ctxt%duct^', ctxt%urduct)
            endif
        else
            if (show_output) write(*, *) 'free-tip option selected'
            ctxt%urduct = 1.0
        endif
        go to 900
        !
        !--------------------------------------------------------------
        24   if(ctxt%duct) then
            if(ninput >= 1) then
                ctxt%urduct = rinput(1)
            else
                call askr('enter aexit/aprop for ctxt%duct^', ctxt%urduct)
            endif
        else
            if (show_output) write(*, *) '*** select ctxt%duct option first'
        endif
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Change altitude
        35 if(ninput >= 1) then
            ctxt%alt = rinput(1)
        else
            call askr('flight altitude (km)^', ctxt%alt)
        endif
        call atmo(ctxt%alt, ctxt%vso, ctxt%rho, ctxt%rmu)
        call flosho(ctxt%luwrit, ctxt%vso, ctxt%rho, ctxt%rmu)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Change flight velocity
        38 velold = ctxt%vel
        if(ninput >= 1) then
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
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Change blade pitch
        40 if(ninput >= 1) then
            delb = rinput(1)
        else
            call askr('angle change (deg)^', delb)
        endif
        do i = 1, ctxt%ii
            ctxt%beta(i) = ctxt%beta(i) + delb * pi / 180.
            ctxt%beta0(i) = ctxt%beta0(i) + delb * pi / 180.
        enddo
        ctxt%conv = .false.
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Specify advance ratio and solve
        42 if(ninput >= 1) then
            ctxt%adv = rinput(1)
        else
            call askr('advance ratio     ^', ctxt%adv)
        endif
        ctxt%conv = .false.
        call aper(ctxt, 4, 2, ctxt%loprini)
        !
        if(ctxt%conv) call output(ctxt, ctxt%luwrit)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Specify rpm and solve
        45 if(ninput >= 1) then
            rpm = rinput(1)
        else
            rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.)
            call askr('rpm               ^', rpm)
        endif
        ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.)
        ctxt%conv = .false.
        call aper(ctxt, 4, 2, ctxt%loprini)
        !
        if(ctxt%conv) call output(ctxt, ctxt%luwrit)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Specify thrust and solve
        50 if(ninput >= 1) then
            ctxt%tspec = rinput(1)
        else
            ctxt%tspec = ctxt%ttot * (ctxt%rho * ctxt%vel**2 * ctxt%rad**2)
            call askr('thrust (n)        ^', ctxt%tspec)
        endif
        rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.0)
        if (show_output) write(*, 1530) rpm
        51 call askc('fix Pitch / fix Rpm ( p/r )?^', &
                ans, ansarg)
        if(ans /= 'r' .and. ans /= 'p') go to 51
        !
        ctxt%conv = .false.
        bsav = ctxt%beta(ctxt%ii)
        if(ans == 'p') call aper(ctxt, 1, 2, ctxt%loprini)
        if(ans == 'r') then
            call askr('rpm:^', rpm)
            ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.0)
            call aper(ctxt, 1, 1, ctxt%loprini)
        endif
        !
        if(ctxt%conv) call output(ctxt, ctxt%luwrit)
        !---- Check for valid blade angle change
        if(ans /= 'p') then
            if(ctxt%conv) then
                !----- convergence was achieved: show blade angle change incurred
                if (show_output) write(*, 1550) ctxt%dbeta * 180.0 / pi
            else
                !----- convergence failed: restore clobbered blade angles
                do i = 1, ctxt%ii
                    ctxt%beta(i) = ctxt%beta(i) - ctxt%dbeta
                    ctxt%beta0(i) = ctxt%beta0(i) - ctxt%dbeta
                enddo
            endif
        endif
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Specify torque and solve
        60 if(ninput >= 1) then
            ctxt%qspec = rinput(1)
        else
            ctxt%qspec = ctxt%qtot * (ctxt%rho * ctxt%vel**2 * ctxt%rad**3)
            call askr('torque (n-m)      ^', ctxt%qspec)
        endif
        rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.0)
        if (show_output) write(*, 1530) rpm
        61 call askc('fix Pitch / fix Rpm ( p/r )?^', &
                ans, ansarg)
        if(ans /= 'r' .and. ans /= 'p') go to 61
        !
        ctxt%conv = .false.
        if(ans == 'p') call aper(ctxt, 2, 2, ctxt%loprini)
        if(ans == 'r') then
            call askr('rpm:^', rpm)
            ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.0)
            call aper(ctxt, 2, 1, ctxt%loprini)
        endif
        !
        if(ctxt%conv) call output(ctxt, ctxt%luwrit)
        !---- Check for valid blade angle change
        if(ans /= 'p') then
            if(ctxt%conv) then
                !----- convergence was achieved: show blade angle change incurred
                if (show_output) write(*, 1550) ctxt%dbeta * 180.0 / pi
            else
                !----- convergence failed: restore clobbered blade angles
                do i = 1, ctxt%ii
                    ctxt%beta(i) = ctxt%beta(i) - ctxt%dbeta
                    ctxt%beta0(i) = ctxt%beta0(i) - ctxt%dbeta
                enddo
            endif
        endif
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Specify power and solve
        70 if(ninput >= 1) then
            ctxt%pspec = rinput(1)
        else
            ctxt%pspec = ctxt%ptot * (ctxt%rho * ctxt%vel**3 * ctxt%rad**2)
            call askr('power (w)         ^', ctxt%pspec)
        endif
        rpm = ctxt%vel / (ctxt%rad * ctxt%adv * pi / 30.0)
        if (show_output) write(*, 1530) rpm
        71 call askc('fix pitch / fix rpm ( p/r )?^', &
                ans, ansarg)
        if(ans /= 'r' .and. ans /= 'p') go to 71
        !
        ctxt%conv = .false.
        if(ans == 'p') call aper(ctxt, 3, 2, ctxt%loprini)
        if(ans == 'r') then
            call askr('rpm:^', rpm)
            ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.0)
            call aper(ctxt, 3, 1, ctxt%loprini)
        endif
        !
        if(ctxt%conv) call output(ctxt, ctxt%luwrit)
        !---- Check for valid blade angle change
        if(ans /= 'p') then
            if(ctxt%conv) then
                !----- convergence was achieved: show blade angle change incurred
                if (show_output) write(*, 1550) ctxt%dbeta * 180.0 / pi
            else
                !----- convergence failed: restore clobbered blade angles
                do i = 1, ctxt%ii
                    ctxt%beta(i) = ctxt%beta(i) - ctxt%dbeta
                    ctxt%beta0(i) = ctxt%beta0(i) - ctxt%dbeta
                enddo
            endif
        endif
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Change number of radial points for blade stations
        72   continue
        if(ctxt%lrotor) then
            iisav = ctxt%ii
            do i = 1, iisav
                ctxt%w1(i) = ctxt%xi(i)
                ctxt%w2(i) = ctxt%ch(i)
                ctxt%w4(i) = ctxt%beta(i)
                ctxt%w6(i) = ctxt%ubody(i)
                ctxt%w8(i) = ctxt%cldes(i)
            enddo
            ctxt%w3(1:ctxt%ii) = spline(ctxt%w1(1:ctxt%ii), ctxt%w2(1:ctxt%ii))
            ctxt%w5(1:ctxt%ii) = spline(ctxt%w1(1:ctxt%ii), ctxt%w4(1:ctxt%ii))
            ctxt%w7(1:ctxt%ii) = spline(ctxt%w1(1:ctxt%ii), ctxt%w6(1:ctxt%ii))
            ctxt%w9(1:ctxt%ii) = spline(ctxt%w1(1:ctxt%ii), ctxt%w8(1:ctxt%ii))
        endif
        !
        73   call aski('enter new number of radial points^', ctxt%ii)
        if(ctxt%ii > ix) then
            if (show_output) write(*, *)
            if (show_output) write(*, *) 'Maximum number is', ix
            go to 73
        endif
        !
        ctxt%iinf = ctxt%ii + ctxt%ii / 2
        call setx(ctxt)
        if(ctxt%lrotor) then
            do i = 1, ctxt%ii
                ctxt%ch(i) = seval(ctxt%xi(i), ctxt%w2, ctxt%w3, ctxt%w1)
                ctxt%beta(i) = seval(ctxt%xi(i), ctxt%w4, ctxt%w5, ctxt%w1)
                ctxt%ubody(i) = seval(ctxt%xi(i), ctxt%w6, ctxt%w7, ctxt%w1)
                ctxt%cldes(i) = seval(ctxt%xi(i), ctxt%w8, ctxt%w9, ctxt%w1)
                ctxt%beta0(i) = ctxt%beta(i)
            enddo
        endif
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Set max number or iterations for nonlinear solution
        75   if(ninput >= 1) then
            ctxt%nitera = iinput(1)
        else
            call aski('max number of iterations^', ctxt%nitera)
        endif
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Toggle initialization flag
        76   ctxt%loprini = .not.ctxt%loprini
        if(ctxt%loprini) then
            if (show_output) write(*, *) 'Analysis case will be initialized'
        else
            if (show_output) write(*, *) 'Analysis case will not be initialized'
        endif
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Reinitialize operating point
        78   call reinit(ctxt)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Read or use engine rpm/power line file
        79   if(ctxt%lpwrvar .and. ctxt%npwrvar > 0) then
            if (show_output) write(*, *) ' '
            if (show_output) write(*, *) 'Current rpm/Power Engine Line'
            do l = 1, ctxt%npwrvar
                if (show_output) write(*, *) l, ctxt%rpmvar(l), ctxt%pwrvar(l)
            end do
            if (show_output) write(*, *) ' '
        endif
        !
        lu = 12
        ctxt%fname = comarg
        if(ctxt%fname(1:1) == ' ') then
            call asks('enter power/rpm filename^', ctxt%fname)
        endif
        if(ctxt%fname(1:1) /= ' ') then
            open(lu, file = ctxt%fname, status = 'old', err = 795)
            call getpvar(lu, ix, ctxt%npwrvar, ctxt%rpmvar, ctxt%pwrvar)
            if (show_output) write(*, *) ' '
            if (show_output) write(*, *) 'rpm/Power Engine Line'
            do l = 1, ctxt%npwrvar
                if (show_output) write(*, *) l, ctxt%rpmvar(l), ctxt%pwrvar(l)
                ctxt%pwrvar(l) = ctxt%pwrvar(l)
            end do
            if (show_output) write(*, *) ' '
            ctxt%xpwrvar = splina(ctxt%rpmvar, ctxt%pwrvar)
            close(lu)
            ctxt%lpwrvar = .true.
        endif
        !
        !
        !--- Use the engine rpm/power to define operating point
        if(ctxt%lpwrvar) then
            791  call askc('fix Pitch / fix Rpm / fix Velocity ( p/r/v )?^', &
                    ans, ansarg)
            if(ans == ' ') go to 900
            if(ans /= 'r' .and. ans /= 'p' .and. ans /= 'v') go to 791
            !
            ctxt%conv = .false.
            if(ans == 'p') call aper(ctxt, 5, 2, ctxt%loprini)
            if(ans == 'r') then
                call askr('rpm:^', rpm)
                ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.0)
                call aper(ctxt, 5, 1, ctxt%loprini)
            endif
            if(ans == 'v') then
                call askr('vel:^', ctxt%vel)
                ctxt%adv = ctxt%vel / (ctxt%rad * rpm * pi / 30.0)
                call aper(ctxt, 5, 2, ctxt%loprini)
            endif
            !
            if(ctxt%conv) call output(ctxt, ctxt%luwrit)
            !---- Was the pitch changed?
            if(ans == 'r') then
                if(ctxt%conv) then
                    !----- convergence was achieved: show blade angle change incurred
                    if (show_output) write(*, 1550) ctxt%dbeta * 180.0 / pi
                else
                    !----- convergence failed: restore clobbered blade angles
                    do i = 1, ctxt%ii
                        ctxt%beta(i) = ctxt%beta(i) - ctxt%dbeta
                        ctxt%beta0(i) = ctxt%beta0(i) - ctxt%dbeta
                    enddo
                endif
            endif
            go to 900
        endif
        !
        795  nf = index(ctxt%fname, ' ') - 1
        if (show_output) write(*, *) 'open error on file  ', ctxt%fname(1:nf)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Do sequence of advance ratios
        81    if (show_output) write(*, *) ' '
        if (show_output) write(*, *) 'Sequence of advance ratios...'
        call setcas(ctxt, 1, ninput, rinput)
        call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, ctxt%rad, ctxt%name)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Do sequence of rpMs
        82    if (show_output) write(*, *) ' '
        if (show_output) write(*, *) 'Sequence of rpMs...'
        call setcas(ctxt, 2, ninput, rinput)
        call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, ctxt%rad, ctxt%name)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Do sequence of pitch angles
        83    if (show_output) write(*, *) ' '
        if (show_output) write(*, *) 'Sequence of blade angles...'
        call setcas(ctxt, 3, ninput, rinput)
        call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, ctxt%rad, ctxt%name)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Do sequence of velocities
        84    if (show_output) write(*, *) ' '
        if (show_output) write(*, *) 'Sequence of velocity with fixed pitch or rpm...'
        call setcas(ctxt, 4, ninput, rinput)
        call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, ctxt%rad, ctxt%name)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Save current operating point to case arrays
        90   ctxt%ncase = 0
        ctxt%kcase = 0
        go to 900
        !
        92   if(ctxt%ncase >= icasx) then
            if (show_output) write(*, *) 'Case arrays too small.  Increase icasx.'
            go to 900
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
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Write case accumulation arrays to file
        94   if(ctxt%ncase <= 0) then
            if (show_output) write(*, *)
            if (show_output) write(*, *) 'No cases saved'
            go to 900
        endif
        !
        lu = 12
        ctxt%fname = comarg
        if(ctxt%fname(1:1) == ' ') call asks('enter case save filename^', ctxt%fname)
        open(lu, file = ctxt%fname, status = 'old', err = 945)
        if (show_output) write(*, *) 'File exists.  Overwrite?  y'
        read (*, 1000) chkey
        if(index('nn', chkey) /= 0) then
            close(lu)
            go to 900
        else
            rewind lu
            go to 946
        endif
        !
        945  open(lu, file = ctxt%fname, status = 'unknown', err = 94)
        946  call shocas(lu, nparx, ctxt%ncase, ctxt%caspar, ctxt%rad, ctxt%name)
        close(lu)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Read case accumulation arrays from saved file
        96   continue
        lu = 12
        ctxt%fname = comarg
        if(ctxt%fname(1:1) == ' ') call asks('enter case save filename^', ctxt%fname)
        open(lu, file = ctxt%fname, status = 'old', err = 965)
        call getcas(lu, nparx, ctxt%ncase, ctxt%caspar)
        close(lu)
        call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, ctxt%rad, ctxt%name)
        go to 900
        !
        965  nf = index(ctxt%fname, ' ') - 1
        if (show_output) write(*, *) 'open error on file  ', ctxt%fname(1:nf)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- Rerun case operating point
        97   if(ctxt%ncase <= 0) then
            if (show_output) write(*, *)
            if (show_output) write(*, *) 'No cases saved'
            go to 900
        endif
        !
        if(ninput >= 1) then
            icase = iinput(1)
        else
            call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, ctxt%rad, ctxt%name)
            icase = 0
            call aski('Select case number (0 to cancel)^', icase)
        endif
        if(icase <= 0) go to 900
        if(icase > ctxt%ncase) then
            ninput = 0
            go to 97
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
        if(ctxt%conv) then
            ctxt%caspar(8, icase) = ctxt%ptot * ctxt%rho * ctxt%vel**3 * ctxt%rad**2
            ctxt%caspar(9, icase) = ctxt%ttot * ctxt%rho * ctxt%vel**2 * ctxt%rad**2
            ctxt%caspar(10, icase) = ctxt%qtot * ctxt%rho * ctxt%vel**2 * ctxt%rad**3
            ctxt%caspar(11, icase) = ctxt%ttot / ctxt%ptot
        endif
        !
        if(ctxt%conv) call output(ctxt, ctxt%luwrit)
        go to 900
        !
        !---------------------------------------------------------------------
        !--- List rotor dimensional data ?
        98   continue
        do i = 1, ctxt%ii
            if (show_output) write(*, *) ctxt%xi(i), ctxt%nblds * ctxt%gam(i) * ctxt%rad * ctxt%vel, ctxt%vind(3, i) * ctxt%vel, &
                    ctxt%nblds * ctxt%gam(i) / (4.0 * pi * ctxt%vind(3, i) * ctxt%xi(i))
        enddo
        go to 900

        !.......................................................................
        !
        1000 format(a)
        1050 format(1x, a4, ' command not recognized.' //&
                '  Type "?" for list, <Return> to exit menu.')
        1100 format(&
                /'   adva r   Prescribe advance ratio'&
                /'   rpm  r   Prescribe rpm'&
                /'   thru r   Prescribe thrust'&
                /'   torq r   Prescribe torque'&
                /'   powe r   Prescribe power'&
                //'   aseq rrr Calculate case sequence of advance ratios'&
                /'   rseq rrr Calculate case sequence of rpms'&
                /'   bseq rrr Calculate case sequence of blade angles'&
                /'   vseq rrr Calculate case sequence of speeds at fixed pitch'&
                /'   clrc     Clear case accumulator'&
                /'   addc     Add current point point to case accumulator'&
                /'   cput f   Write current case accumulator to file'&
                /'   cget f   Read cases from file'&
                /'   case i   Select case'&
                //'   atmo r   Set fluid properties from standard atmosphere'&
                /'   velo r   Set or change flight speed'&
                /'   angl r   Change blade pitch angle'&
                /'   pvar f   Enter and use engine rpm/power line'&
                //'   form     Select slipstream and velocity formulation'&
                //'   name s   Set or change case name'&
                /'   writ f   Write current operating point to disk file'&
                /'   disp     Display current operating state'&
                /'   init     Initialize next analysis case'&
                /'   rein     Re-initialize prop to known operating state'&
                /'   ters     Toggle between terse and verbose output'&
                /'   iter i   Change max number of Newton iterations'&
                /'   n    i   Change number of radial points')
        1530 format(/' Current rpm:', f9.2)
        1550 format(' Blade angle changed', f7.3, ' degrees')
        !
    end
    ! oper
end module p_xoper