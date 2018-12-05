!***********************************************************************
!    Module:  userio.f
! 
!    Copyright (C) 2011 Mark Drela 
! 
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation; either version 2 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program; if not, write to the Free Software
!    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!***********************************************************************
!
!
!==== user input routines with prompting and error trapping
!
!
SUBROUTINE ASKI(PROMPT, IINPUT)
    !
    !---- integer input
    !
    CHARACTER*(*) PROMPT, LINE*80
    INTEGER IINPUT
    !
    NP = INDEX(PROMPT, '^') - 1
    IF(NP.EQ.0) NP = LEN(PROMPT)
    !
    IF(IINPUT.NE.999)  WRITE(*, 1001) IINPUT
    10   WRITE(*, 1000) PROMPT(1:NP)
    READ (*, 1002, ERR = 10) LINE
    IF(LINE.NE.' ') READ (LINE, *, ERR = 10) IINPUT
    RETURN
    !
    1000 FORMAT(A, '   i>  ', $)
    1001 FORMAT(/'Current value <ret takes default>: ', I5)
    1002 FORMAT(A)
END
! ASKI


SUBROUTINE ASKR(PROMPT, RINPUT)
    !
    !---- real input
    !
    CHARACTER*(*) PROMPT, LINE*80
    REAL RINPUT
    !
    NP = INDEX(PROMPT, '^') - 1
    IF(NP.EQ.0) NP = LEN(PROMPT)
    !
    IF(RINPUT.NE.999.)  WRITE(*, 1001) RINPUT
    10   WRITE(*, 1000) PROMPT(1:NP)
    READ (*, 1002, ERR = 10) LINE
    IF(LINE.NE.' ') READ (LINE, *, ERR = 10) RINPUT
    RETURN
    !
    1000 FORMAT(A, '   r>  ', $)
    1001 FORMAT(/'Current value <ret takes default>: ', G12.6)
    1002 FORMAT(A)
END
! ASKR


SUBROUTINE ASKL(PROMPT, LINPUT)
    !
    !---- logical input
    !
    CHARACTER*(*) PROMPT
    LOGICAL LINPUT
    CHARACTER*1 CHAR
    !
    NP = INDEX(PROMPT, '^') - 1
    IF(NP.EQ.0) NP = LEN(PROMPT)
    !
    10   WRITE(*, 1000) PROMPT(1:NP)
    READ (*, 1010) CHAR
    IF(CHAR.EQ.'y') CHAR = 'Y'
    IF(CHAR.EQ.'n') CHAR = 'N'
    IF(CHAR.NE.'Y' .AND. CHAR.NE.'N') GO TO 10
    !
    LINPUT = CHAR .EQ. 'Y'
    RETURN
    !
    1000 FORMAT(/A, ' y/n>  ', $)
    1010 FORMAT(A)
END
! ASKL


SUBROUTINE ASKS(PROMPT, INPUT)
    !
    !---- string of arbitrary length input
    !
    CHARACTER*(*) PROMPT
    CHARACTER*(*) INPUT
    !
    NP = INDEX(PROMPT, '^') - 1
    IF(NP.EQ.0) NP = LEN(PROMPT)
    !
    WRITE(*, 1000) PROMPT(1:NP)
    READ (*, 1010) INPUT
    !
    RETURN
    !
    1000 FORMAT(/A, '   s>  ', $)
    1010 FORMAT(A)
END
! ASKS



SUBROUTINE ASKC(PROMPT, COMAND, CARGS)
    !
    !---- returns 4-byte character string input converted to uppercase
    !---- also returns rest of input characters in CARGS string
    !
    CHARACTER*(*) PROMPT
    CHARACTER*(*) COMAND, CARGS
    !
    CHARACTER*128 LINE
    LOGICAL ERROR
    !
    IZERO = ICHAR('0')
    !
    NP = INDEX(PROMPT, '^') - 1
    IF(NP.EQ.0) NP = LEN(PROMPT)
    !
    WRITE(*, 1000) PROMPT(1:NP)
    READ (*, 1020) LINE
    !
    !---- strip off leading blanks
    DO K = 1, 128
        IF(LINE(1:1) .EQ. ' ') THEN
            LINE = LINE(2:128)
        ELSE
            GO TO 5
        ENDIF
    ENDDO
    5    CONTINUE
    !
    !---- find position of first blank, "+", "-", ".", ",", or numeral
    K = INDEX(LINE, ' ')
    KI = INDEX(LINE, '-')
    IF(KI.NE.0) K = MIN(K, KI)
    KI = INDEX(LINE, '+')
    IF(KI.NE.0) K = MIN(K, KI)
    KI = INDEX(LINE, '.')
    IF(KI.NE.0) K = MIN(K, KI)
    KI = INDEX(LINE, ',')
    IF(KI.NE.0) K = MIN(K, KI)
    DO I = 0, 9
        KI = INDEX(LINE, CHAR(IZERO + I))
        IF(KI.NE.0) K = MIN(K, KI)
    ENDDO
    !
    !      IF(K.EQ.1) THEN
    !C------ the "command" is a number... set entire COMAND string with it
    !        COMAND = LINE
    !      ELSE
    !C------ the "command" is some string... just use the part up to the argument
    !        COMAND = LINE(1:K-1)
    !      ENDIF
    !
    IF(K.LE.1) K = 5
    !---- set 4-byte alphabetic command string and convert it to uppercase
    COMAND = LINE(1:K - 1)
    CALL LC2UC(COMAND)
    !
    CARGS = LINE(K:128)
    CALL STRIP(CARGS, NCARGS)
    RETURN
    !
    1000 FORMAT(/A, '   c>  ', $)
    1020 FORMAT(A)
END
! ASKC


SUBROUTINE LC2UC(INPUT)
    CHARACTER*(*) INPUT
    !
    CHARACTER*26 LCASE, UCASE
    DATA LCASE / 'abcdefghijklmnopqrstuvwxyz' /
    DATA UCASE / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
    !
    N = LEN(INPUT)
    !
    do I = 1, N
        K = INDEX(LCASE, INPUT(I:I))
        IF(K.GT.0) INPUT(I:I) = UCASE(K:K)
    end do
    !
    RETURN
END
! LC2UC




SUBROUTINE READI(N, IVAR, ERROR)
    DIMENSION IVAR(N)
    LOGICAL ERROR
    !--------------------------------------------------
    !     Reads N integer variables, leaving unchanged
    !     if only <return> is entered.
    !--------------------------------------------------
    DIMENSION IVTMP(40)
    CHARACTER*80 LINE
    !
    READ(*, 1000) LINE
    1000 FORMAT(A80)
    !
    do I = 1, N
        IVTMP(I) = IVAR(I)
    end do
    !
    NTMP = 40
    CALL GETINT(LINE, IVTMP, NTMP, ERROR)
    !
    IF(ERROR) RETURN
    !
    do I = 1, N
        IVAR(I) = IVTMP(I)
    end do
    !
    RETURN
END
! READI


SUBROUTINE READR(N, VAR, ERROR)
    DIMENSION VAR(N)
    LOGICAL ERROR
    !-------------------------------------------------
    !     Reads N real variables, leaving unchanged
    !     if only <return> is entered.
    !-------------------------------------------------
    DIMENSION VTMP(40)
    CHARACTER*80 LINE
    !
    READ(*, 1000) LINE
    1000 FORMAT(A80)
    !
    do I = 1, N
        VTMP(I) = VAR(I)
    end do
    !
    NTMP = 40
    CALL GETFLT(LINE, VTMP, NTMP, ERROR)
    !
    IF(ERROR) RETURN
    !
    do I = 1, N
        VAR(I) = VTMP(I)
    end do
    !
    RETURN
END
! READR


SUBROUTINE GETINT(INPUT, A, N, ERROR)
    CHARACTER*(*) INPUT
    INTEGER A(*)
    LOGICAL ERROR
    !----------------------------------------------------------
    !     Parses character string INPUT into an array
    !     of integer numbers returned in A(1...N)
    !
    !     Will attempt to extract no more than N numbers,
    !     unless N = 0, in which case all numbers present
    !     in INPUT will be extracted.
    !
    !     N returns how many numbers were actually extracted.
    !----------------------------------------------------------
    CHARACTER*130 REC
    !
    !---- only first 128 characters in INPUT will be parsed
    ILEN = MIN(LEN(INPUT), 128)
    ILENP = ILEN + 2
    !
    !---- put input into local work string (which will be munched)
    REC(1:ILENP) = INPUT(1:ILEN) // ' ,'
    !
    !---- ignore everything after a "!" character
    K = INDEX(REC, '!')
    IF(K.GT.0) REC(1:ILEN) = REC(1:K - 1)
    !
    NINP = N
    !
    !---- count up how many numbers are to be extracted
    N = 0
    K = 1
    do IPASS = 1, ILEN
        !------ search for next space or comma starting with current index K
        KSPACE = INDEX(REC(K:ILENP), ' ') + K - 1
        KCOMMA = INDEX(REC(K:ILENP), ',') + K - 1
        !
        IF(K.EQ.KSPACE) THEN
            !------- just skip this space
            K = K + 1
            GO TO 9
        ENDIF
        !
        IF(K.EQ.KCOMMA) THEN
            !------- comma found.. increment number count and keep looking
            N = N + 1
            K = K + 1
            GO TO 9
        ENDIF
        !
        !------ neither space nor comma found, so we ran into a number...
        !-    ...increment number counter and keep looking after next space or comma
        N = N + 1
        K = MIN(KSPACE, KCOMMA) + 1
        !
        9     IF(K.GE.ILEN) GO TO 11
    end do
    !
    !---- decide on how many numbers to read, and go ahead and read them
    11   IF(NINP.GT.0) N = MIN(N, NINP)
    READ(REC(1:ILEN), *, ERR = 20) (A(I), I = 1, N)
    ERROR = .FALSE.
    RETURN
    !
    !---- bzzzt !!!
    20   CONTINUE
    !cc   WRITE(*,*) 'GETINT: String-to-integer conversion error.'
    N = 0
    ERROR = .TRUE.
    RETURN
END


SUBROUTINE GETFLT(INPUT, A, N, ERROR)
    CHARACTER*(*) INPUT
    REAL A(*)
    LOGICAL ERROR
    !----------------------------------------------------------
    !     Parses character string INPUT into an array
    !     of real numbers returned in A(1...N)
    !
    !     Will attempt to extract no more than N numbers,
    !     unless N = 0, in which case all numbers present
    !     in INPUT will be extracted.
    !
    !     N returns how many numbers were actually extracted.
    !----------------------------------------------------------
    CHARACTER*130 REC
    !
    !---- only first 128 characters in INPUT will be parsed
    ILEN = MIN(LEN(INPUT), 128)
    ILENP = ILEN + 2
    !
    !---- put input into local work string (which will be munched)
    REC(1:ILENP) = INPUT(1:ILEN) // ' ,'
    !
    !---- ignore everything after a "!" character
    K = INDEX(REC, '!')
    IF(K.GT.0) REC(1:ILEN) = REC(1:K - 1)
    !
    NINP = N
    !
    !---- count up how many numbers are to be extracted
    N = 0
    K = 1
    do IPASS = 1, ILEN
        !------ search for next space or comma starting with current index K
        KSPACE = INDEX(REC(K:ILENP), ' ') + K - 1
        KCOMMA = INDEX(REC(K:ILENP), ',') + K - 1
        !
        IF(K.EQ.KSPACE) THEN
            !------- just skip this space
            K = K + 1
            GO TO 9
        ENDIF
        !
        IF(K.EQ.KCOMMA) THEN
            !------- comma found.. increment number count and keep looking
            N = N + 1
            K = K + 1
            GO TO 9
        ENDIF
        !
        !------ neither space nor comma found, so we ran into a number...
        !-    ...increment number counter and keep looking after next space or comma
        N = N + 1
        K = MIN(KSPACE, KCOMMA) + 1
        !
        9     IF(K.GE.ILEN) GO TO 11
    end do
    !
    !---- decide on how many numbers to read, and go ahead and read them
    11   IF(NINP.GT.0) N = MIN(N, NINP)
    READ(REC(1:ILEN), *, ERR = 20) (A(I), I = 1, N)
    ERROR = .FALSE.
    RETURN
    !
    !---- bzzzt !!!
    20   CONTINUE
    !cc   WRITE(*,*) 'GETFLT: String-to-integer conversion error.'
    N = 0
    ERROR = .TRUE.
    RETURN
END


SUBROUTINE STRIP(STRING, NS)
    CHARACTER*(*) STRING
    !-------------------------------------------
    !     Strips leading blanks off string
    !     and returns length of non-blank part.
    !-------------------------------------------
    N = LEN(STRING)
    !
    !---- find last non-blank character
    do K2 = N, 1, -1
        IF(STRING(K2:K2).NE.' ') GO TO 11
    end do
    K2 = 0
    11 CONTINUE
    !
    !---- find first non-blank character
    do K1 = 1, K2
        IF(STRING(K1:K1).NE.' ') GO TO 21
    end do
    21 CONTINUE
    !
    !---- number of non-blank characters
    NS = K2 - K1 + 1
    IF(NS.EQ.0) RETURN
    !
    !---- shift STRING so first character is non-blank
    STRING(1:NS) = STRING(K1:K2)
    !
    !---- pad tail of STRING with blanks
    do K = NS + 1, N
        STRING(K:K) = ' '
    end do
    !
    RETURN
END