!***********************************************************************
!    Module:  userio.f
! 
!    Copyright (c) 2011 Mark Drela 
! 
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the gnu General Public License as published by
!    the Free Software Foundation; either version 2 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but without any warranty; without even the implied warranty of
!    merchantability or fitness for a particular purpose.  See the
!    gnu General Public License for more details.
!
!    You should have received a copy of the gnu General Public License
!    along with this program; if not, write to the Free Software
!    Foundation, Inc., 675 Mass Ave, Cambridge, ma 02139, usa.
!***********************************************************************
!
!
!==== user input routines with prompting and error trapping
!
!
subroutine aski(prompt, iinput)
    !
    !---- integer input
    !
    character*(*) prompt, line*80
    integer iinput
    !
    np = index(prompt, '^') - 1
    if(np == 0) np = len(prompt)
    !
    if(iinput /= 999)  write(*, 1001) iinput
    10   write(*, 1000) prompt(1:np)
    read (*, 1002, err = 10) line
    if(line /= ' ') read (line, *, err = 10) iinput
    return
    !
    1000 format(a, '   i>  ', $)
    1001 format(/'Current value <ret takes default>: ', i5)
    1002 format(a)
end
! aski


subroutine askr(prompt, rinput)
    !
    !---- real input
    !
    character*(*) prompt, line*80
    real rinput
    !
    np = index(prompt, '^') - 1
    if(np == 0) np = len(prompt)
    !
    if(rinput /= 999.)  write(*, 1001) rinput
    10   write(*, 1000) prompt(1:np)
    read (*, 1002, err = 10) line
    if(line /= ' ') read (line, *, err = 10) rinput
    return
    !
    1000 format(a, '   r>  ', $)
    1001 format(/'Current value <ret takes default>: ', g12.6)
    1002 format(a)
end
! askr


subroutine askl(prompt, linput)
    !
    !---- logical input
    !
    character*(*) prompt
    logical linput
    character*1 char
    !
    np = index(prompt, '^') - 1
    if(np == 0) np = len(prompt)
    !
    10   write(*, 1000) prompt(1:np)
    read (*, 1010) char
    if(char == 'y') char = 'y'
    if(char == 'n') char = 'n'
    if(char /= 'y' .and. char /= 'n') go to 10
    !
    linput = char == 'y'
    return
    !
    1000 format(/a, ' y/n>  ', $)
    1010 format(a)
end
! askl


subroutine asks(prompt, input)
    !
    !---- string of arbitrary length input
    !
    character*(*) prompt
    character*(*) input
    !
    np = index(prompt, '^') - 1
    if(np == 0) np = len(prompt)
    !
    write(*, 1000) prompt(1:np)
    read (*, 1010) input
    !
    return
    !
    1000 format(/a, '   s>  ', $)
    1010 format(a)
end
! asks



subroutine askc(prompt, comand, cargs)
    !
    !---- returns 4-byte character string input converted to uppercase
    !---- also returns rest of input characters in cargs string
    !
    character*(*) prompt
    character*(*) comand, cargs
    !
    character*128 line
    logical error
    !
    izero = ichar('0')
    !
    np = index(prompt, '^') - 1
    if(np == 0) np = len(prompt)
    !
    write(*, 1000) prompt(1:np)
    read (*, 1020) line
    !
    !---- strip off leading blanks
    do k = 1, 128
        if(line(1:1) == ' ') then
            line = line(2:128)
        else
            go to 5
        endif
    enddo
    5    continue
    !
    !---- find position of first blank, "+", "-", ".", ",", or numeral
    k = index(line, ' ')
    ki = index(line, '-')
    if(ki /= 0) k = min(k, ki)
    ki = index(line, '+')
    if(ki /= 0) k = min(k, ki)
    ki = index(line, '.')
    if(ki /= 0) k = min(k, ki)
    ki = index(line, ',')
    if(ki /= 0) k = min(k, ki)
    do i = 0, 9
        ki = index(line, char(izero + i))
        if(ki /= 0) k = min(k, ki)
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
    if(k <= 1) k = 5
    !---- set 4-byte alphabetic command string and convert it to uppercase
    comand = line(1:k - 1)
    call lc2uc(comand)
    !
    cargs = line(k:128)
    call strip(cargs, ncargs)
    return
    !
    1000 format(/a, '   c>  ', $)
    1020 format(a)
end
! askc


subroutine lc2uc(input)
    character*(*) input
    !
    character*26 lcase, ucase
    data lcase / 'abcdefghijklmnopqrstuvwxyz' /
    data ucase / 'abcdefghijklmnopqrstuvwxyz' /
    !
    n = len(input)
    !
    do i = 1, n
        k = index(lcase, input(i:i))
        if(k > 0) input(i:i) = ucase(k:k)
    end do
    !
    return
end
! lc2uc




subroutine readi(n, ivar, error)
    dimension ivar(n)
    logical error
    !--------------------------------------------------
    !     Reads n integer variables, leaving unchanged
    !     if only <return> is entered.
    !--------------------------------------------------
    dimension ivtmp(40)
    character*80 line
    !
    read(*, 1000) line
    1000 format(a80)
    !
    do i = 1, n
        ivtmp(i) = ivar(i)
    end do
    !
    ntmp = 40
    call getint(line, ivtmp, ntmp, error)
    !
    if(error) return
    !
    do i = 1, n
        ivar(i) = ivtmp(i)
    end do
    !
    return
end
! readi


subroutine readr(n, var, error)
    dimension var(n)
    logical error
    !-------------------------------------------------
    !     Reads n real variables, leaving unchanged
    !     if only <return> is entered.
    !-------------------------------------------------
    dimension vtmp(40)
    character*80 line
    !
    read(*, 1000) line
    1000 format(a80)
    !
    do i = 1, n
        vtmp(i) = var(i)
    end do
    !
    ntmp = 40
    call getflt(line, vtmp, ntmp, error)
    !
    if(error) return
    !
    do i = 1, n
        var(i) = vtmp(i)
    end do
    !
    return
end
! readr


subroutine getint(input, a, n, error)
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
    if(k > 0) rec(1:ilen) = rec(1:k - 1)
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
        if(k == kspace) then
            !------- just skip this space
            k = k + 1
            go to 9
        endif
        !
        if(k == kcomma) then
            !------- comma found.. increment number count and keep looking
            n = n + 1
            k = k + 1
            go to 9
        endif
        !
        !------ neither space nor comma found, so we ran into a number...
        !-    ...increment number counter and keep looking after next space or comma
        n = n + 1
        k = min(kspace, kcomma) + 1
        !
        9     if(k >= ilen) go to 11
    end do
    !
    !---- decide on how many numbers to read, and go ahead and read them
    11   if(ninp > 0) n = min(n, ninp)
    read(rec(1:ilen), *, err = 20) (a(i), i = 1, n)
    error = .false.
    return
    !
    !---- bzzzt !!!
    20   continue
    !cc   write(*,*) 'getint: String-to-integer conversion error.'
    n = 0
    error = .true.
    return
end


subroutine getflt(input, a, n, error)
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
    if(k > 0) rec(1:ilen) = rec(1:k - 1)
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
        if(k == kspace) then
            !------- just skip this space
            k = k + 1
            go to 9
        endif
        !
        if(k == kcomma) then
            !------- comma found.. increment number count and keep looking
            n = n + 1
            k = k + 1
            go to 9
        endif
        !
        !------ neither space nor comma found, so we ran into a number...
        !-    ...increment number counter and keep looking after next space or comma
        n = n + 1
        k = min(kspace, kcomma) + 1
        !
        9     if(k >= ilen) go to 11
    end do
    !
    !---- decide on how many numbers to read, and go ahead and read them
    11   if(ninp > 0) n = min(n, ninp)
    read(rec(1:ilen), *, err = 20) (a(i), i = 1, n)
    error = .false.
    return
    !
    !---- bzzzt !!!
    20   continue
    !cc   write(*,*) 'getflt: String-to-integer conversion error.'
    n = 0
    error = .true.
    return
end


subroutine strip(string, ns)
    character*(*) string
    !-------------------------------------------
    !     Strips leading blanks off string
    !     and returns length of non-blank part.
    !-------------------------------------------
    n = len(string)
    !
    !---- find last non-blank character
    do k2 = n, 1, -1
        if(string(k2:k2) /= ' ') go to 11
    end do
    k2 = 0
    11 continue
    !
    !---- find first non-blank character
    do k1 = 1, k2
        if(string(k1:k1) /= ' ') go to 21
    end do
    21 continue
    !
    !---- number of non-blank characters
    ns = k2 - k1 + 1
    if(ns == 0) return
    !
    !---- shift string so first character is non-blank
    string(1:ns) = string(k1:k2)
    !
    !---- pad tail of string with blanks
    do k = ns + 1, n
        string(k:k) = ' '
    end do
    !
    return
end