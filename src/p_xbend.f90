module p_xbend
contains
    subroutine bend(ctxt)
        use m_xbend, only: mclr, stset, eiload, stadd, stcalc, stclr, stwrit
        use m_xrotor, only: opfile
        use m_userio, only: getflt, askc, getint
        use m_common, only: Common
        implicit real (m)
        type(Common), intent(inout) :: ctxt
        character*4 comand
        character*132 comarg
        !
        dimension iinput(20)
        dimension rinput(20)
        logical error
        !
        !---------------------------------------------
        !     Run rotor at arbitrary operating points
        !---------------------------------------------
        !
        ctxt%greek = .false.
        !
        if(ctxt%lstruc) then
            if (show_output) write(*, *)
            if (show_output) write(*, *) 'Structural properties available'
        else
            if (show_output) write(*, *)
            if (show_output) write(*, *) 'Structural properties not available'
        endif
        !
        900  call askc('.bend^', comand, comarg)
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
        if (comand == '?   ' .and. show_output) write(*, 1100)
        if(comand == '?   ') go to 900
        if(comand == 'read') go to 10
        if(comand == 'clr ') go to 20
        if(comand == 'eval') go to 30
        if(comand == 'defl') go to 40
        if(comand == 'rest') go to 50
        if(comand == 'writ') go to 70
        if(comand == 'sets') go to 80
        if(comand == 'mclr') go to 85
        if(comand == 'help') go to 100
        !
        !-------------------------------------------------------------
        if (show_output) write(*, 1000) comand
        go to 900
        !
        !-------------------------------------------------------------
        10   call eiload(ctxt, comarg)
        go to 900
        !
        !-------------------------------------------------------------
        20   call stclr(ctxt)
        go to 900
        !
        !-------------------------------------------------------------
        30   if(.not.ctxt%lstruc) then
            if (show_output) write(*, *) 'Structural properties not available'
            if (show_output) write(*, *) 'Assuming zero mass, infinite stiffness...'
        endif
        !cc      call stload(ctxt)
        call stcalc(ctxt)
        call stwrit(ctxt, ctxt%luwrit)
        go to 900
        !
        !-------------------------------------------------------------
        40   call stadd(ctxt)
        go to 900
        !
        !-------------------------------------------------------------
        50   do i = 1, ctxt%ii
            ctxt%beta(i) = ctxt%beta0(i)
        end do
        ctxt%conv = .false.
        go to 900
        !
        !-------------------------------------------------------------
        70   if(comarg(1:1) /= ' ') ctxt%savfil = comarg
        call opfile(ctxt%lusave, ctxt%savfil)
        call stwrit(ctxt, ctxt%lusave)
        close(ctxt%lusave)
        go to 900
        !
        !-------------------------------------------------------------
        80   call stset(ctxt)
        go to 900
        !
        !-------------------------------------------------------------
        85   call mclr(ctxt)
        go to 900
        !
        !-------------------------------------------------------------
        100   if (show_output) write(*, 3000)
        go to 900
        !
        !.......................................................................
        !
        1000 format(1x, a4, ' command not recognized.  Type a "?" for list')
        1100 format(&
                /'   read f Read in blade structural properties'&
                /'   eval   Evaluate structural loads and deflections'&
                /'   clr    Clear all structural deflections'&
                //'   defl   Set new twist  =  static  +  structural twist'&
                /'   rest   Set new twist  =  static twist'&
                /'   sets   Set static twist = current - structural twist'&
                //'   writ f Write structural solution to disk file'&
                //'   help   Display help on structural calculation')
        !
        !
        3000 format(/&
                'The axis definitions are:'//&
                '  x  aft along prop rotation axis'/&
                '  y  radial alng blade'/&
                '  z  perpendicular to blade:    x x y = z'//&
                'The structural solution contains two groups of data, '&
                'the first group has:'//&
                '  u/r    deflections in the x direction'/&
                '  w/r    deflections in the z direction'/&
                '   t     torsional twist (positive in the increasing '&
                'incidence direction)'/&
                '   Mz    bending moment about the z axis'/&
                '   Mx    bending moment about the x axis'/&
                '   t     moment about the radial y axis (i.e. torsion)'/&
                '   p     tensile load (shear in y direction)'/&
                '   Sx    shear in x direction'/&
                '   Sz    shear in z direction'//&
                'The second group of structural data contains:'//&
                '   Ex    strain due to bending in the x direction'/&
                '   Ey    strain due to extension in the y direction'/&
                '   Ez    strain due to bending in the z direction'/&
                '   Emax  maximum strain calculated by  '&
                'Emax = sqrt(Ex^2 + Ez^2) + Ey'/&
                '   g     shear strain due to twist t'//&
                '   Note that Ex, Ez, and g are evaluated at the '&
                'local radius rst from'/&
                '   the structural axis (rst is an input quantity, '&
                'normally set to the '/&
                '   distance of the highest or lowest profile point '&
                'from the structural axis)'/)
        !
    end
    ! bend
end module p_xbend