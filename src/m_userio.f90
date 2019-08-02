!*==M_USERIO.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
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
!
!
!==== user input routines with prompting and error trapping
!
!

module m_userio
    implicit none
contains
    subroutine aski(prompt, iinput)
        use i_common, only : show_output
        !*** Start of declarations inserted by SPAG
        integer NP
        !*** End of declarations inserted by SPAG
        !
        !---- integer input
        !
        character*(*) prompt, line*80
        integer iinput
        !
        np = index(prompt, '^') - 1
        if (np==0) np = len(prompt)
        !
        if (iinput/=999.and.show_output) write (*, 99001) iinput
        99001  format (/'Current value <ret takes default>: ', i5)
        100   if (show_output) write (*, 99002) prompt(1:np)
        !
        99002  format (a, '   i>  ', $)
        read (*, 99003, err = 100) line
        99003  format (a)
        if (line/=' ') read (line, *, err = 100) iinput
        return
    end
    ! aski


    subroutine askr(prompt, rinput)
        use i_common, only : show_output
        !*** Start of declarations inserted by SPAG
        integer NP
        !*** End of declarations inserted by SPAG
        !
        !---- real input
        !
        character*(*) prompt, line*80
        real rinput
        !
        np = index(prompt, '^') - 1
        if (np==0) np = len(prompt)
        !
        if (rinput/=999..and.show_output) write (*, 99001) rinput
        99001  format (/'Current value <ret takes default>: ', g12.6)
        100   if (show_output) write (*, 99002) prompt(1:np)
        !
        99002  format (a, '   r>  ', $)
        read (*, 99003, err = 100) line
        99003  format (a)
        if (line/=' ') read (line, *, err = 100) rinput
        return
    end
    ! askr


    subroutine askl(prompt, linput)
        use i_common, only : show_output
        !*** Start of declarations inserted by SPAG
        integer NP
        !*** End of declarations inserted by SPAG
        !
        !---- logical input
        !
        character*(*) prompt
        logical linput
        character*1 char
        !
        np = index(prompt, '^') - 1
        if (np==0) np = len(prompt)
        do
            !
            if (show_output) write (*, 99001) prompt(1:np)
            !
            99001      format (/a, ' y/n>  ', $)
            read (*, 99002) char
            99002      format (a)
            if (char=='y') char = 'y'
            if (char=='n') char = 'n'
            if (char=='y'.or.char=='n') then
                !
                linput = char=='y'
                return
            endif
        enddo
    end
    ! askl


    subroutine asks(prompt, input)
        use i_common, only : show_output
        !*** Start of declarations inserted by SPAG
        integer NP
        !*** End of declarations inserted by SPAG
        !
        !---- string of arbitrary length input
        !
        character*(*) prompt
        character*(*) input
        !
        np = index(prompt, '^') - 1
        if (np==0) np = len(prompt)
        !
        if (show_output) write (*, 99001) prompt(1:np)
        !
        99001  format (/a, '   s>  ', $)
        read (*, 99002) input
        99002  format (a)
        !
        return
    end
    ! asks



    subroutine askc(prompt, comand, cargs)
        use i_common, only : show_output
        !*** Start of declarations inserted by SPAG
        integer I, IZERO, K, KI, NCARGS, NP
        !*** End of declarations inserted by SPAG
        !
        !---- returns 4-byte character string input converted to uppercase
        !---- also returns rest of input characters in cargs string
        !
        character*(*) prompt
        character*(*) comand, cargs
        !
        character*128 line
        !
        izero = ichar('0')
        !
        np = index(prompt, '^') - 1
        if (np==0) np = len(prompt)
        !
        if (show_output) write (*, 99001) prompt(1:np)
        !
        99001  format (/a, '   c>  ', $)
        read (*, 99002) line
        99002  format (a)
        !
        !---- strip off leading blanks
        do k = 1, 128
            if (line(1:1)/=' ') exit
            line = line(2:128)
        enddo
        !
        !---- find position of first blank, "+", "-", ".", ",", or numeral
        k = index(line, ' ')
        ki = index(line, '-')
        if (ki/=0) k = min(k, ki)
        ki = index(line, '+')
        if (ki/=0) k = min(k, ki)
        ki = index(line, '.')
        if (ki/=0) k = min(k, ki)
        ki = index(line, ',')
        if (ki/=0) k = min(k, ki)
        do i = 0, 9
            ki = index(line, char(izero + i))
            if (ki/=0) k = min(k, ki)
        enddo
        !
        !      if(k == 1) then
        !c------ the "command" is a number... set entire comand string with it
        !        comand = line
        !      else
        !c------ the "command" is some string... just use the part up to the argument
        !        comand = line(1:k-1)
        !      endif
        !
        if (k<=1) k = 5
        !---- set 4-byte alphabetic command string and convert it to uppercase
        comand = line(1:k - 1)
        call lc2uc(comand)
        !
        cargs = line(k:128)
        call strip(cargs, ncargs)
        return
    end
    ! askc


    subroutine lc2uc(input)
        !*** Start of declarations inserted by SPAG
        integer I, K, N
        !*** End of declarations inserted by SPAG
        character*(*) input
        !
        character*26 lcase, ucase
        data lcase/'abcdefghijklmnopqrstuvwxyz'/
        data ucase/'abcdefghijklmnopqrstuvwxyz'/
        !
        n = len(input)
        !
        do i = 1, n
            k = index(lcase, input(i:i))
            if (k>0) input(i:i) = ucase(k:k)
        enddo
        !
    end
    ! lc2uc




    subroutine readi(n, ivar, error)
        !*** Start of declarations inserted by SPAG
        integer I, IVAR, IVTMP, N, NTMP
        !*** End of declarations inserted by SPAG
        dimension ivar(n)
        logical error
        !--------------------------------------------------
        !     Reads n integer variables, leaving unchanged
        !     if only <return> is entered.
        !--------------------------------------------------
        dimension ivtmp(40)
        character*80 line
        !
        read (*, 99001) line
        99001  format (a80)
        !
        do i = 1, n
            ivtmp(i) = ivar(i)
        enddo
        !
        ntmp = 40
        call getint(line, ivtmp, ntmp, error)
        !
        if (error) return
        !
        do i = 1, n
            ivar(i) = ivtmp(i)
        enddo
        !
    end
    ! readi


    subroutine readr(n, var, error)
        !*** Start of declarations inserted by SPAG
        integer I, N, NTMP
        real VAR, VTMP
        !*** End of declarations inserted by SPAG
        dimension var(n)
        logical error
        !-------------------------------------------------
        !     Reads n real variables, leaving unchanged
        !     if only <return> is entered.
        !-------------------------------------------------
        dimension vtmp(40)
        character*80 line
        !
        read (*, 99001) line
        99001  format (a80)
        !
        do i = 1, n
            vtmp(i) = var(i)
        enddo
        !
        ntmp = 40
        call getflt(line, vtmp, ntmp, error)
        !
        if (error) return
        !
        do i = 1, n
            var(i) = vtmp(i)
        enddo
        !
    end
    ! readr


    subroutine getint(input, a, n, error)
        !*** Start of declarations inserted by SPAG
        integer I, ILEN, ILENP, IPASS, K, KCOMMA, KSPACE, N, NINP
        !*** End of declarations inserted by SPAG
        character*(*) input
        integer a(*)
        logical error
        !----------------------------------------------------------
        !     Parses character string input into an array
        !     of integer numbers returned in a(1...n)
        !
        !     Will attempt to extract no more than n numbers,
        !     unless n = 0, in which case all numbers present
        !     in input will be extracted.
        !
        !     n returns how many numbers were actually extracted.
        !----------------------------------------------------------
        character*130 rec
        !
        !---- only first 128 characters in input will be parsed
        ilen = min(len(input), 128)
        ilenp = ilen + 2
        !
        !---- put input into local work string (which will be munched)
        rec(1:ilenp) = input(1:ilen) // ' ,'
        !
        !---- ignore everything after a "!" character
        k = index(rec, '!')
        if (k>0) rec(1:ilen) = rec(1:k - 1)
        !
        ninp = n
        !
        !---- count up how many numbers are to be extracted
        n = 0
        k = 1
        do ipass = 1, ilen
            !------ search for next space or comma starting with current index k
            kspace = index(rec(k:ilenp), ' ') + k - 1
            kcomma = index(rec(k:ilenp), ',') + k - 1
            !
            if (k==kspace) then
                !------- just skip this space
                k = k + 1
                goto 50
            endif
            !
            if (k==kcomma) then
                !------- comma found.. increment number count and keep looking
                n = n + 1
                k = k + 1
                goto 50
            endif
            !
            !------ neither space nor comma found, so we ran into a number...
            !-    ...increment number counter and keep looking after next space or comma
            n = n + 1
            k = min(kspace, kcomma) + 1
            !
            50        if (k>=ilen) exit
        enddo
        !
        !---- decide on how many numbers to read, and go ahead and read them
        if (ninp>0) n = min(n, ninp)
        read (rec(1:ilen), *, err = 100) (a(i), i = 1, n)
        error = .false.
        return
        !
        !---- bzzzt !!!
        !cc   write(*,*) 'getint: String-to-integer conversion error.'
        100   n = 0
        error = .true.
    end


    subroutine getflt(input, a, n, error)
        !*** Start of declarations inserted by SPAG
        integer I, ILEN, ILENP, IPASS, K, KCOMMA, KSPACE, N, NINP
        !*** End of declarations inserted by SPAG
        character*(*) input
        real a(*)
        logical error
        !----------------------------------------------------------
        !     Parses character string input into an array
        !     of real numbers returned in a(1...n)
        !
        !     Will attempt to extract no more than n numbers,
        !     unless n = 0, in which case all numbers present
        !     in input will be extracted.
        !
        !     n returns how many numbers were actually extracted.
        !----------------------------------------------------------
        character*130 rec
        !
        !---- only first 128 characters in input will be parsed
        ilen = min(len(input), 128)
        ilenp = ilen + 2
        !
        !---- put input into local work string (which will be munched)
        rec(1:ilenp) = input(1:ilen) // ' ,'
        !
        !---- ignore everything after a "!" character
        k = index(rec, '!')
        if (k>0) rec(1:ilen) = rec(1:k - 1)
        !
        ninp = n
        !
        !---- count up how many numbers are to be extracted
        n = 0
        k = 1
        do ipass = 1, ilen
            !------ search for next space or comma starting with current index k
            kspace = index(rec(k:ilenp), ' ') + k - 1
            kcomma = index(rec(k:ilenp), ',') + k - 1
            !
            if (k==kspace) then
                !------- just skip this space
                k = k + 1
                goto 50
            endif
            !
            if (k==kcomma) then
                !------- comma found.. increment number count and keep looking
                n = n + 1
                k = k + 1
                goto 50
            endif
            !
            !------ neither space nor comma found, so we ran into a number...
            !-    ...increment number counter and keep looking after next space or comma
            n = n + 1
            k = min(kspace, kcomma) + 1
            !
            50        if (k>=ilen) exit
        enddo
        !
        !---- decide on how many numbers to read, and go ahead and read them
        if (ninp>0) n = min(n, ninp)
        read (rec(1:ilen), *, err = 100) (a(i), i = 1, n)
        error = .false.
        return
        !
        !---- bzzzt !!!
        !cc   write(*,*) 'getflt: String-to-integer conversion error.'
        100   n = 0
        error = .true.
    end


    subroutine strip(string, ns)
        !*** Start of declarations inserted by SPAG
        integer K, K1, K2, N, NS
        !*** End of declarations inserted by SPAG
        character*(*) string
        !-------------------------------------------
        !     Strips leading blanks off string
        !     and returns length of non-blank part.
        !-------------------------------------------
        n = len(string)
        !
        !---- find last non-blank character
        do k2 = n, 1, -1
            if (string(k2:k2)/=' ') goto 100
        enddo
        k2 = 0
        !
        !---- find first non-blank character
        100   do k1 = 1, k2
            if (string(k1:k1)/=' ') exit
        enddo
        !
        !---- number of non-blank characters
        ns = k2 - k1 + 1
        if (ns==0) return
        !
        !---- shift string so first character is non-blank
        string(1:ns) = string(k1:k2)
        !
        !---- pad tail of string with blanks
        do k = ns + 1, n
            string(k:k) = ' '
        enddo
        !
    end
end
