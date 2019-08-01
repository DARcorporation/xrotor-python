module p_xnoise
contains
    subroutine noise(ctxt)
        use m_xnoise, only : dbfoot, sft, ptrace
        use m_userio, only : askr, readi, getflt, asks, readr, getint, askc
        !---------------------------------------
        !     Calculates the sound pressure
        !     time history of the propeller
        !     at specified observer positions.
        !---------------------------------------
        use m_common, only : Common, ix, show_output
        use m_spline, only : spline, seval
        implicit real (m)
        !
        parameter (ntx = 160)
        parameter (nharx = ntx / 2)
        dimension aoci(ix), aoc(ix), aocx(ix), xa(ix)
        dimension pcomp(0:ntx, 3), pres(0:ntx), time(0:ntx)
        dimension decib(0:nharx), fampl(nharx), phase(nharx)
        !
        parameter (nxdim = 81, nydim = 81)
        type(Common), intent(inout) :: ctxt
        dimension adb(nxdim, nydim)
        dimension xdb(nxdim, nydim)
        dimension ydb(nxdim, nydim)
        dimension xlim(2), ylim(2)
        dimension ndbsiz(2)
        !
        character*80 prompt
        character*4 comand, ans
        character*132 comarg, ansarg
        !
        dimension iinput(20)
        dimension rinput(20)
        logical error, ldbcon, lptrac
        !
        dimension xyzobs(3)
        character*2 ulnam

        integer nt_temp(1)
        !
        save nt
        save aoc0, xyzobs
        save ulnam, unitl
        !
        !---- number of rotation steps for one full rotation
        data nt / 80 /
        !
        !---- default airfoil area/c^2 , observer location
        data aoc0, xyzobs / -1.0, 0.0, 0.0, -100.0 /
        !
        !---- start by using foot input
        !cc   data ulnam, unitl / 'm ', 1.0 /
        data ulnam, unitl / 'ft', 3.28084 /
        !
        !
        ctxt%greek = .false.
        !
        ldbcon = .false.
        lptrac = .false.
        !
        !---- i,j size of grid for db footprint contour plot
        ndbsiz(1) = 21
        ndbsiz(2) = 11
        !
        !---- number of blade-passing harmonics to be calculated, and annotation delta
        nharm = nt / 2
        dharm = 5.0
        !
        if(aoc0 /= 0.0) then
            if(aoc0 < 0.0) aoc0 = 0.0
            do i = 1, ctxt%ii
                aoci(i) = aoc0
            enddo
        endif
        !
        if(ulnam == '(m) ') then
            if (show_output) write(*, *) 'Coordinates currently specified in meters'
        else
            if (show_output) write(*, *) 'Coordinates currently specified in feet'
        endif
        !
        !
        if (show_output) write(*, 8100)
        8000 format(1x, a4, ' command not recognized.' //&
                '  Type "?" for list, <Return> to exit menu.')
        8100 format(&
                /'   p   rrr Calculate acoustic p(t) at observer x,y,z'&
                /'   foot rr Calculate db ground noise footprint'&
                /'   ntim i  Change number of time samples'&
                /'   unit    Toggle coordinate unit  m,ft'&
                //'   aoc  r  Set constant blade cross-sectional area/c**2'&
                /'   afil f  Set blade cross-sectional area/c**2 from file')
        !
        900  continue
        call askc('.nois^', comand, comarg)
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
        if(comand == '    ') return
        if (comand == '?   ' .and. show_output) write(*, 8100)
        if(comand == '?   ') go to 900
        if(comand == 'p   ') go to 10
        if(comand == 'foot') go to 20
        if(comand == 'ntim') go to 25
        if(comand == 'unit') go to 30
        if(comand == 'aoc ') go to 40
        if(comand == 'afil') go to 45

        if (show_output) write(*, 8000) comand
        go to 900
        !
        !===========================================================================
        10   continue
        if(ninput >= 3) then
            xyzobs(1) = rinput(1)
            xyzobs(2) = rinput(2)
            xyzobs(3) = rinput(3)
        else
            if (show_output) write(*, 1050)
            1050  format(/' Cartesian system fixed to airplane.'&
                    /'  (x forward, y left, z up):       '&
                    /'  '&
                    /'                              z         '&
                    /'                                        '&
                    /'                              .         '&
                    /'                         x              '&
                    /'                          .   .         '&
                    /'                           .            '&
                    /'                            . .         '&
                    /'                             .          '&
                    /'       y   .    .    . _______\\________ '&
                    /'                               \\        '&
                    /'                              __\\__     '&
                    /'                                        ')
            !
            !cc              123456789012345678901234567890123     4567      890
            105  prompt = 'Enter observer x,y,z coordinates (' // ulnam // '):  '
            if (show_output) write(*, 1100) prompt(1:40), (xyzobs(k), k = 1, 3)
            1100  format(1x, a, 3f12.2)
            call readr(3, xyzobs, error)
            if(error) go to 105
        endif
        go to 100
        !cc      go to 900
        !
        !======================================================================
        20   continue
        if(ninput >= 1) then
            galt = rinput(1)
        else
            prompt = 'Enter flight altitude above ground (' // ulnam // ')^'
            call askr(prompt, galt)
        endif
        if(ninput >= 2) then
            dclimb = rinput(2)
        else
            call askr('Enter climb angle (deg)^', dclimb)
        endif
        !
        !---- set default ground-grid limits
        xlim(1) = -2.0 * galt
        xlim(2) = 2.0 * galt
        ylim(1) = -1.0 * galt
        ylim(2) = 1.0 * galt
        !
        if (show_output) write(*, *)
        1210 format(1x, a, 2f10.0)
        !cc             1234567890123456789012345     6789      012
        201  prompt = 'Enter footprint x limits (' // ulnam // '):  '
        if (show_output) write(*, 1210) prompt(1:32), xlim(1), xlim(2)
        call readr(2, xlim, error)
        if(error) go to 201
        !
        202  prompt = 'Enter footprint y limits (' // ulnam // '):  '
        if (show_output) write(*, 1210) prompt(1:32), ylim(1), ylim(2)
        call readr(2, ylim, error)
        if(error) go to 202
        !
        204   if (show_output) write(*, 1250) 'Enter footprint grid size: ', ndbsiz(1), ndbsiz(2)
        1250 format(1x, a, 2i6)
        call readi(2, ndbsiz, error)
        if(error) go to 204
        if(ndbsiz(1) > nxdim .or. ndbsiz(2) > nydim) then
            if (show_output) write(*, *) 'Array dimension limits are:', nxdim, nydim
            ndbsiz(1) = min(ndbsiz(1), nxdim)
            ndbsiz(2) = min(ndbsiz(2), nydim)
            go to 204
        endif
        !
        !
        thx1 = atan2(xlim(1), galt)
        thx2 = atan2(xlim(2), galt)
        thy1 = atan2(ylim(1), galt)
        thy2 = atan2(ylim(2), galt)
        do i = 1, ndbsiz(1)
            do j = 1, ndbsiz(2)
                thx = thx1 + (thx2 - thx1) * float(i - 1) / float(ndbsiz(1) - 1)
                thy = thy1 + (thy2 - thy1) * float(j - 1) / float(ndbsiz(2) - 1)
                xdb(i, j) = galt * tan(thx)
                ydb(i, j) = galt * tan(thy)
            enddo
        enddo
        !
        if (show_output) write(*, *)
        if (show_output) write(*, *) 'Calculating db footprint...'
        nt1 = nt
        call dbfoot(ctxt%nblds, ctxt%ii, ctxt%xi(1), ctxt%dxi, aoci, ctxt%ch, ctxt%gam, &
                ctxt%adv, ctxt%rad, ctxt%vel, ctxt%vso, ctxt%rho, &
                galt, dclimb, unitl, nt1, &
                nxdim, nydim, ndbsiz(1), ndbsiz(2), xdb, ydb, adb)
        ldbcon = .true.
        !
        !===========================================================================
        25   continue
        if(ninput >= 1) then
            nt = iinput(1)
        else
            251     if (show_output) write(*, 1251) nt
            1251   format(/1x, ' Enter number of p(t) samples/revolution:', i7)
            call readi(1, nt_temp, error)
            nt = nt_temp(1)
            if(error) go to 251
        endif
        !
        if(nt > ntx) then
            nt = ntx
            if (show_output) write(*, *) 'Number of samples limited to array limit:', ntx
        endif
        !
        nharm = nt / 2
        go to 900
        !
        !======================================================================
        30   if(ulnam == 'ft') then
            ulnam = 'm '
            unitl = 1.0
            if (show_output) write(*, *) 'Coordinates now specified in meters'
        else
            ulnam = 'ft'
            unitl = 3.28084
            if (show_output) write(*, *) 'Coordinates now specified in feet'
        endif
        go to 900
        !
        !===========================================================================
        40   continue
        !                                      2
        !---- set local blade airfoil  area / c
        !     (this version assumes that it's constant)
        if(ninput >= 1) then
            aoc0 = rinput(1)
        else
            call askr&
                    ('Enter blade airfoil  (cross-sectional area)/chord**2^', aoc0)
        endif
        !
        do i = 1, ctxt%ii
            aoci(i) = aoc0
        enddo
        !
        !---- recalculate pressure signature if observer position has been chosen
        if(lptrac) go to 100
        go to 900
        !
        !===========================================================================
        45   continue
        !---- this version reads in an area distribution list with increasing r/r:
        !       a/c^2   r/r
        !       a/c^2   r/r
        !       a/c^2   r/r
        !         .      .
        !         .      .
        !
        !     These are splined to the computational radial stations.
        !
        ctxt%fname = comarg
        if(ctxt%fname(1:1) == ' ') then
            call asks&
                    ('enter blade airfoil area/c**2 distribution filename^', ctxt%fname)
        endif
        !
        lu = ctxt%lutemp
        open(lu, file = ctxt%fname, status = 'old', err = 459)
        do ia = 1, ix
            read(lu, *, end = 455, err = 458) xa(ia), aoc(ia)
        enddo
        if (show_output) write(*, *) 'Array size limited.  Not all points read in.'
        ia = ix + 1
        455  continue
        na = ia - 1
        close(lu)
        !
        aocx(1:na) = spline(xa(1:na), aoc(1:na))
        do i = 1, ctxt%ii
            ! todo: test this
            aoci(i) = seval(ctxt%xi(i), aoc, aocx, xa)
        enddo
        aoc0 = 0.0
        !
        !---- recalculate pressure signature if observer position has been chosen
        if(lptrac) go to 100
        go to 900
        !
        458   if (show_output) write(*, *) 'File read error'
        close(lu)
        go to 900
        !
        459   if (show_output) write(*, *) 'File open error'
        go to 900
        !
        !===========================================================================
        !===========================================================================
        !---- p(t) signature calculation
        100  continue
        xobs = xyzobs(1) / unitl
        yobs = xyzobs(2) / unitl
        zobs = xyzobs(3) / unitl
        call ptrace(xobs, yobs, zobs, &
                ctxt%nblds, ctxt%ii, ctxt%xi(1), ctxt%dxi, aoci, ctxt%ch, ctxt%gam, &
                ctxt%adv, ctxt%rad, ctxt%vel, ctxt%vso, ctxt%rho, &
                ntx, nt, pcomp, time)
        !
        !---- set total p(t) signal
        do it = 0, nt
            pres(it) = pcomp(it, 1) + pcomp(it, 2) + pcomp(it, 3)
        enddo
        lptrac = .true.
        !
        !---- integrate p(t) for rms pressure
        prms = 0.
        do it = 1, nt
            delt = time(it) - time(it - 1)
            pavg = (pres(it) + pres(it - 1)) * 0.5
            prms = prms + pavg**2 * delt
        enddo
        prms = sqrt(prms / (time(nt) - time(0)))
        !
        !---- get amplitude of each blade-passing harmonic component
        call sft(pres, time, nt, fampl, phase, nharm)
        !
        !---- set Decibels relative to 20 microPa for each harmonic component
        do ih = 1, nharm
            decib(ih) = 20.0 * alog10(sqrt(0.5) * fampl(ih) / 20.0e-6)
        enddo
        !
        !---- set total db level from r.m.s. pressure
        decib(0) = 20.0 * alog10(prms / 20.0e-6)
        !
        !---- print out decibel spectrum
        if (show_output) write(*, 5000)  0, decib(0)
        if (show_output) write(*, 5010) (k, decib(k), k = 1, nharm)
        5000 format(&
                /' Sound level for each multiple of blade-passing frequency'&
                //'      n      db', &
                /    1x, i6, f9.2, '  (total)')
        5010 format(1x, i6, f9.2)
        !cc            12    95.02
        !
        !cc      go to 900
        !.....................................................................
    end
end module p_xnoise