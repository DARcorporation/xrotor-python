!***********************************************************************
!    Module:  xio.f
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


SUBROUTINE LOAD(FNAME1)
    !------------------------------------------------------------------------
    !     Reads in previously saved rotor in new XROTOR_Version >= 6.9 format
    !     This format saves more information and can have optional comment
    !     lines beginning with a ! character.
    !------------------------------------------------------------------------
    INCLUDE 'XROTOR.INC'
    CHARACTER*(*) FNAME1
    CHARACTER*128 LINE
    GREEK = .FALSE.
    !
    LU = LUTEMP
    !
    FNAME = FNAME1
    IF(FNAME(1:1) == ' ') CALL ASKS('Enter filename^', FNAME)
    OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 200)
    !
    !--- Check for new format/old format XROTOR file
    CALL RDLINE(LU, LINE)
    IF(LINE == 'END' .OR. LINE == 'ERR') GO TO 210
    READ(LINE(17:22), *) FILEVERS
    WRITE(*, 1005) FILEVERS
    !
    !
    !--- Case title
    CALL RDLINE(LU, LINE)
    NAME = LINE
    !
    CALL RDLINE(LU, LINE)
    READ(LINE, *, ERR = 210) RHO, VSO, RMU, ALT
    CALL RDLINE(LU, LINE)
    READ(LINE, *, ERR = 210) RAD, VEL, ADV, RAKE
    CALL RDLINE(LU, LINE)
    READ(LINE, *, ERR = 210) XI0, XW0
    CALL RDLINE(LU, LINE)
    !
    !--- Read aero section definitions
    READ(LINE, *, ERR = 210) NAERO
    DO N = 1, NAERO
        CALL RDLINE(LU, LINE)
        READ(LINE, *, ERR = 210) XISECT
        CALL RDLINE(LU, LINE)
        READ(LINE, *, ERR = 210) A0DEG, DCLDA, CLMAX, CLMIN
        CALL RDLINE(LU, LINE)
        READ(LINE, *, ERR = 210) DCLDA_STALL, DCL_STALL, CMCON, MCRIT
        CALL RDLINE(LU, LINE)
        READ(LINE, *, ERR = 210) CDMIN, CLDMIN, DCDCL2
        CALL RDLINE(LU, LINE)
        READ(LINE, *, ERR = 210) REREF, REXP
        !
        A0 = A0DEG * PI / 180.0
        CALL PUTAERO(N, XISECT, A0, CLMAX, CLMIN, &
                DCLDA, DCLDA_STALL, DCL_STALL, &
                CDMIN, CLDMIN, DCDCL2, CMCON, MCRIT, REREF, REXP)
    END DO
    !
    !--- Read flags for wake, duct and windmill modes
    CALL RDLINE(LU, LINE)
    READ(LINE, *, ERR = 210) FREE, DUCT, WIND
    !
    WRITE(*, *)
    IF(FREE) WRITE(*, *) 'Self-deforming wake option set'
    IF(.NOT.FREE) WRITE(*, *) 'Rigid wake option set'
    IF(DUCT) WRITE(*, *) 'Duct option set'
    IF(.NOT.DUCT) WRITE(*, *) 'Free-tip option set'
    IF(WIND) WRITE(*, *) 'Windmill plotting mode set'
    IF(.NOT.WIND) WRITE(*, *) 'Propeller plotting mode set'
    !
    WRITE(*, *) ' '
    CALL RDLINE(LU, LINE)
    IF(LINE == 'END' .OR. LINE == 'ERR') GO TO 210
    READ(LINE, *, ERR = 210) IIX, NBLDS
    DO I = 1, IIX
        CALL RDLINE(LU, LINE)
        READ(LINE, *, ERR = 210) XI(I), CH(I), BETADEG, UBODY(I)
        BETA(I) = BETADEG * PI / 180.0
        BETA0(I) = BETA(I)
        !c        write(*,*) 'load i,ch,beta ',i,ch(i),beta(i)
    END DO
    !
    !--- Optional duct velocity
    URDUCT = 1.0
    CALL RDLINE(LU, LINE)
    IF(LINE == 'END' .OR. LINE == 'ERR') GO TO 19
    READ(LINE, *, END = 19) URDUCT
    !
    !---- Optional slipstream velocities
    19   NADD = 0
    CALL RDLINE(LU, LINE)
    IF(LINE == 'END' .OR. LINE == 'ERR') GO TO 21
    READ(LINE, *, END = 21) NADD
    IF(NADD > IX) THEN
        NADD = IX
        WRITE(*, *) 'Warning, slipstream data terminated at ', IX
    ENDIF
    DO I = 1, NADD
        CALL RDLINE(LU, LINE)
        IF(LINE == 'END' .OR. LINE == 'ERR') GO TO 20
        READ(LINE, *, ERR = 20, END = 20) RADD(I), UADD(I), VADD(I)
    END DO
    IF(I < NADD) THEN
        NADD = I - 1
        WRITE(*, *) 'Warning, slipstream data terminated at ', NADD
    ENDIF
    GO TO 21
    !
    20   IF(I > 2) THEN
        NADD = I - 1
    ENDIF
    !
    21   CLOSE(LU)
    IF(NADD > 1) THEN
        WRITE(*, *)
        WRITE(*, *) 'Slipstream profiles read with #points ', NADD
    ENDIF
    !
    CONV = .FALSE.
    !
    !--- Check for number of analysis stations to use
    IF(IIX /= II) THEN
        22     WRITE(*, 23) IIX, II, II
        READ(*, 24) LINE
        IF(LINE /= ' ') THEN
            READ(LINE, *, ERR = 22) II
        ENDIF
        23     FORMAT(/'Read  # input stations = ', I3, &
                /'Using # blade stations = ', I3, &
                /'Enter # stations or <cr> for ', I3, ' ', $)
        24     FORMAT(A)
    ENDIF
    !
    !---- spline blade geometry to "old" radial locations
    DO I = 1, IIX
        W1(I) = XI(I)
        W2(I) = CH(I)
        W4(I) = BETA(I)
        W6(I) = UBODY(I)
    ENDDO
    CALL SPLINE(W2, W3, W1, IIX)
    CALL SPLINE(W4, W5, W1, IIX)
    CALL SPLINE(W6, W7, W1, IIX)
    !
    !---- set radial stations for built-in distribution scheme
    CALL SETX
    CALL XWINIT
    !
    !---- interpolate read-in geometry to generated radial stations
    DO I = 1, II
        CH(I) = SEVAL(XI(I), W2, W3, W1, IIX)
        BETA(I) = SEVAL(XI(I), W4, W5, W1, IIX)
        UBODY(I) = SEVAL(XI(I), W6, W7, W1, IIX)
        BETA0(I) = BETA(I)
        !c        write(*,*) 'load trp i,ch,beta ',i,ch(i),beta(i)
    ENDDO
    IINF = II + II / 2
    !
    CALL SETIAERO
    !---- calculate current operating point
    CALL APER(4, 2, .TRUE.)
    IF(CONV) CALL OUTPUT(LUWRIT)
    !
    !---- define design quantities for design of MIL prop with same parameters
    RADDES = RAD
    VELDES = VEL
    ADVDES = 0.
    RPMDES = VEL / (RAD * ADV) * 30.0 / PI
    R0DES = XI0 * RAD
    RWDES = XW0 * RAD
    TDDES = TTOT * RHO * VEL**2 * RAD**2
    PDDES = PTOT * RHO * VEL**3 * RAD**2
    DEST = .FALSE.
    DESP = .TRUE.
    DO I = 1, II
        CLDES(I) = CL(I)
    ENDDO
    CLDES0 = 0.
    !
    !---- rotor now exists
    LROTOR = .TRUE.
    RETURN
    !
    200 WRITE(*, 1010) FNAME(1:32)
    RETURN
    !
    210 WRITE(*, 1020) FNAME(1:32)
    CLOSE(LU)
    CONV = .FALSE.
    RETURN
    !..............................
    1000 FORMAT(A)
    1005 FORMAT(' Reading file from XROTOR Version ', F5.2)
    1010 FORMAT(' File  ', A, ' not found'/)
    1020 FORMAT(' File  ', A, ' has incompatible format'/&
            ' Loading not completed'/)
END
! LOAD



SUBROUTINE RDLINE(LUN, LINE)
    !...Purpose  Read a non-comment line from the input file
    !...Input    Data read from unit LUN
    !...Output   LINE  Character string with input line
    !                  LINE is set to 'END' for end or errors
    !
    CHARACTER*(*) LINE
    !
    1000 FORMAT(A)
    20 READ (LUN, 1000, END = 80, ERR = 90) LINE
    !
    !---- skip comment line
    IF(INDEX('!#', LINE(1:1)) /= 0) GO TO 20
    !
    !---- skip blank line
    IF(LINE == ' ') GO TO 20
    !
    !---- normal return after significant line
    RETURN
    !
    80 LINE = 'END '
    RETURN
    !
    90 LINE = 'ERR '
    RETURN
END




