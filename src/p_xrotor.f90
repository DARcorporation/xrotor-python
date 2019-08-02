!*==P_XROTOR.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
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
module p_xrotor
    implicit none
contains
    subroutine rotor() !

        !--- module statement for Windoze dvFortran
        !cc   use dflib
        !
        use m_xio, only : save, load
        use p_xnoise, only : noise
        use m_xoper, only : shocas, getcas
        use m_xrotor, only : output, init_
        use p_xoper, only : oper
        use p_xbend, only : bend
        use m_userio, only : getflt, askc, getint
        use i_common, only : Common, show_output, nparx
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        integer I, IARGC, IINPUT, KF, NARG, NINPUT
        real RINPUT
        !*** End of declarations inserted by SPAG
        type (Common) :: ctxt
        character*7 comand
        character*128 comarg
        !
        dimension iinput(20)
        dimension rinput(20)
        logical error
        !
        !====================================================
        !
        !      Interactive Design and Analysis Program
        !          for Free-tip and Ducted Rotors
        !
        !      October 1992
        !      Copyright Mark Drela
        !      Versions 6.7-7.x
        !      Copyright Mark Drela, Harold Youngren
        !
        !====================================================
        !
        ctxt = Common()

        ctxt%version = 7.55
        !
        !---- logical unit numbers
        ctxt%luread = 5     ! terminal read
        ctxt%luwrit = 6     ! terminal write
        ctxt%lutemp = 3     ! general-use disk i/o unit  (usually available)
        ctxt%lusave = 4     ! save file                  (usually open)
        !
        !
        if (show_output) write (*, 99001) ctxt%version
        !
        !.....................................................................
        !
        99001  format (/' ========================='/'    xrotor Version', &
                &f5.2/' =========================')
        !
        call init_(ctxt)
        !
        ctxt%fname = ' '
        !--- Get command line args (if present)
        narg = iargc()
        !
        if (narg>0) call getarg(1, ctxt%fname)
        if (ctxt%fname(1:1)/=' ') call load(ctxt, ctxt%fname)
        !
        ctxt%fname = ' '
        if (narg>1) call getarg(2, ctxt%fname)
        if (ctxt%fname(1:1)/=' ') then
            ctxt%ncase = 0
            open (ctxt%lutemp, file = ctxt%fname, status = 'old', err = 100)
            call getcas(ctxt%lutemp, nparx, ctxt%ncase, ctxt%caspar)
            close (ctxt%lutemp)
            if (ctxt%ncase>0) then
                kf = index(ctxt%fname, ' ') - 1
                if (show_output) write (*, *) 'Operating cases read from file  '&
                        &, ctxt%fname(1:kf), ' ...'
                call shocas(ctxt%luwrit, nparx, ctxt%ncase, ctxt%caspar, ctxt%rad, &
                        & ctxt%name)
            endif
        endif
        !
        100   if (show_output) write (*, 99003)
        do
            !
            call askc(' xrotor^', comand, comarg)
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
            ctxt%greek = .true.
            if (comand/='    ') then
                if (comand=='?   '.and.show_output) write (*, 99003)
                if (comand/='?   ') then
                    if (comand=='quit') stop
                    !
                    if (comand=='oper') call oper(ctxt)
                    if (comand=='bend') call bend(ctxt)
                    if (comand=='save') call save(ctxt, comarg)
                    if (comand=='load') call load(ctxt, comarg)
                    if (comand=='nois') call noise(ctxt)
                    if (comand=='disp') then
                        !
                        !---------------------------------------------------------------------
                        call output(ctxt, ctxt%luwrit)
                    else
                        if (ctxt%greek.and.show_output) write (*, 99002) comand
                        99002                  format (1x, a4, &
                                &' command not recognized.  Type a "?" for list')
                    endif
                endif
            endif
        enddo
        99003  format (/'   quit   Exit program'/                                       &
                &'  .oper   Calculate off-design operating points'/               &
                &'  .bend   Calculate structural loads and deflections'/          &
                &'  .nois   Calculate and plot acoustic signature'/               &
                &'   save f Save rotor to restart file'/                          &
                &'   load f Read rotor from restart file'/                        &
                &'   disp   Display current design point')
    end
    ! rotor
end
!*==XROTOR.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019

program xrotor
    use p_xrotor, only : rotor
    call rotor
end
