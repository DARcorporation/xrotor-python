!*==P_XBEND.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
module p_xbend
    implicit none
contains
    subroutine bend(ctxt)
        use m_xbend, only : mclr, stset, eiload, stadd, stcalc, stclr, stwrit
        use m_xrotor, only : opfile
        use m_userio, only : getflt, askc, getint
        use i_common, only : Common, show_output
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        integer I, IINPUT, NINPUT
        real RINPUT
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
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
        if (ctxt%lstruc) then
            if (show_output) write (*, *)
            if (show_output) write (*, *) 'Structural properties available'
        else
            if (show_output) write (*, *)
            if (show_output) write (*, *) 'Structural properties not available'
        endif
        do
            !
            call askc('.bend^', comand, comarg)
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
            99001      format (/'   read f Read in blade structural properties'/            &
                    &'   eval   Evaluate structural loads and deflections'/       &
                    &'   clr    Clear all structural deflections'//               &
                    &'   defl   Set new twist  =  static  +  structural twist'/   &
                    &'   rest   Set new twist  =  static twist'/                  &
                    &'   sets   Set static twist = current - structural twist'//  &
                    &'   writ f Write structural solution to disk file'//         &
                    &'   help   Display help on structural calculation')
            if (comand/='?   ') then
                if (comand=='read') then
                    !
                    !-------------------------------------------------------------
                    call eiload(ctxt, comarg)
                elseif (comand=='clr ') then
                    !
                    !-------------------------------------------------------------
                    call stclr(ctxt)
                elseif (comand=='eval') then
                    !
                    !-------------------------------------------------------------
                    if (.not.ctxt%lstruc) then
                        if (show_output) write (*, *)                           &
                                &'Structural properties not available'
                        if (show_output) write (*, *)                           &
                                &'Assuming zero mass, infinite stiffness...'
                    endif
                    !cc      call stload(ctxt)
                    call stcalc(ctxt)
                    call stwrit(ctxt, ctxt%luwrit)
                elseif (comand=='defl') then
                    !
                    !-------------------------------------------------------------
                    call stadd(ctxt)
                elseif (comand=='rest') then
                    !
                    !-------------------------------------------------------------
                    do i = 1, ctxt%ii
                        ctxt%beta(i) = ctxt%beta0(i)
                    enddo
                    ctxt%conv = .false.
                elseif (comand=='writ') then
                    !
                    !-------------------------------------------------------------
                    if (comarg(1:1)/=' ') ctxt%savfil = comarg
                    call opfile(ctxt%lusave, ctxt%savfil)
                    call stwrit(ctxt, ctxt%lusave)
                    close (ctxt%lusave)
                elseif (comand=='sets') then
                    !
                    !-------------------------------------------------------------
                    call stset(ctxt)
                elseif (comand=='mclr') then
                    !
                    !-------------------------------------------------------------
                    call mclr(ctxt)
                elseif (comand=='help') then
                    !
                    !-------------------------------------------------------------
                    if (show_output) write (*, 99002)
                    !
                    !
                    99002              format (/'The axis definitions are:'//                       &
                            &'  x  aft along prop rotation axis'/                 &
                            &'  y  radial alng blade'/                            &
                            &'  z  perpendicular to blade:    x x y = z'//        &
                            &'The structural solution contains two groups of data, ''the first group ha&
                            &s:'//'  u/r    deflections in the x direction'/                           &
                            &'  w/r    deflections in the z direction'/                                &
                            &'   t     torsional twist (positive in the increasing ''incidence directio&
                            &n)'/'   Mz    bending moment about the z axis'/                           &
                            &'   Mx    bending moment about the x axis'/                               &
                            &'   t     moment about the radial y axis (i.e. torsion)'/                 &
                            &'   p     tensile load (shear in y direction)'/                           &
                            &'   Sx    shear in x direction'/'   Sz    shear in z direction'//         &
                            &'The second group of structural data contains:'//                         &
                            &'   Ex    strain due to bending in the x direction'/                      &
                            &'   Ey    strain due to extension in the y direction'/                    &
                            &'   Ez    strain due to bending in the z direction'/                      &
                            &'   Emax  maximum strain calculated by  ''Emax = sqrt(Ex^2 + Ez^2) + Ey'/ &
                            &'   g     shear strain due to twist t'//                                  &
                            &'   Note that Ex, Ez, and g are evaluated at the ''local radius rst from'/&
                            &'   the structural axis (rst is an input quantity, ''normally set to the '&
                            & /                                                                         &
                            &'   distance of the highest or lowest profile point ''from the structural &
                            &axis)'/)
                else
                    !
                    !-------------------------------------------------------------
                    if (show_output) write (*, 99003) comand
                    !
                    !.......................................................................
                    !
                    99003              format (1x, a4, ' command not recognized.  Type a "?" for list'&
                            &)
                endif
            endif
        enddo
        !
    end
    ! bend
end
