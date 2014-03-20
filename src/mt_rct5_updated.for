C
      SUBROUTINE RCT5AR(IN)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED BY THE CHEMICAL 
C REACTION (RCT) PACKAGE.
C **********************************************************************
C last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: INRCT,IOUT,NCOL,NROW,NLAY,NCOMP,ICBUND,
     &                         COLD,PRSITY,BUFF,SRCONC,RETA,RHOB,
     &                         PRSITY2,RETA2,ISOTHM,RFMIN,DTRCT,
     &                         IREACT,IRCTOP,IGETSC,IFMTRF,FRAC,SP1,SP2,
     &                         RC1,RC2,
     &                         SAVUCN,IUCN2                          !# LINE 261 RCT
      USE MIN_SAT, ONLY: ICIMDRY                        !# LINE 11 ADV
C
      USE RCTMOD                                               !# LINE 33 RCT
C
      IMPLICIT  NONE
      INTEGER   IN,J,I,K,JR,IR,KR,INDEX,IERR
      INTEGER   NODES                                          !# LINE 271 RCT
      REAL      TR,TINY,EPSILON,TOTPOR
      CHARACTER ANAME*24,FLNAME*50,FINDEX*30
      PARAMETER (TINY=1.E-30,EPSILON=0.5E-6)
      LOGICAL   EXISTED                                        !# LINE 283 RCT
C
      INRCT=IN
C
C--ALLOCATE AND INITIALIZE
      ALLOCATE(IREACT,IRCTOP,IGETSC,IFESLD,ISORBIMONLY,ISP1IM,
     1  rec_FileName,NSPECIAL,NEA,NED,NSTORE,Ad_methane_name,IUMETH,
     1  RVAL,NSOLID)
      ALLOCATE(FRAC(NCOL,NROW,NLAY))
      ALLOCATE(SP1(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(SP2(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(RC1(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(RC2(NCOL,NROW,NLAY,NCOMP))
      FRAC=0.
      SP1=0.
      SP2=0.
      RC1=0.
      RC2=0.
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1000) INRCT
 1000 FORMAT(1X,'RCT5 -- CHEMICAL REACTION PACKAGE,',
     & ' VERSION 5, FEBRUARY 2010, INPUT READ FROM UNIT',I3)
C
C--READ AND ECHO SORPTION ISOTHERM TYPE AND FLAG IREACT
      READ(INRCT,'(6I10)',ERR=100,IOSTAT=IERR)
     & ISOTHM,IREACT,IRCTOP,IGETSC,IREACTION !,IFESLD            !# Amended 
  100 IF(IERR.NE.0) THEN
        IRCTOP=1
        IGETSC=0
        BACKSPACE (INRCT)
        READ(INRCT,'(2I10)') ISOTHM,IREACT
      ENDIF
C
      ISORBIMONLY=.FALSE.                                      !# LINE 56 RCT
      ISP1IM=0                                                 !# LINE 57 RCT
      IF(ISOTHM.EQ.-6) THEN                                    !# LINE 58 RCT
        ISORBIMONLY=.TRUE.                                     !# LINE 59 RCT
        ISP1IM=1                                               !# LINE 60 RCT
        ISOTHM=6                                               !# LINE 61 RCT
      ENDIF                                                    !# LINE 62 RCT
C                                                              !# LINE 63 RCT
CVSB      IREACTION=0                                          !# LINE 64 RCT
      IF(IREACT.EQ.90.OR.IREACT.EQ.91) THEN                    !# LINE 65 RCT
        IREACTION=1                                            !# LINE 66 RCT
        IREACT=IREACT-90                                       !# LINE 67 RCT
      ENDIF                                                    !# LINE 68 RCT
C                                                              !# LINE 69 RCT
      IF(ISOTHM.EQ.1) THEN
        WRITE(IOUT,1022)
      ELSEIF(ISOTHM.EQ.2) THEN
        WRITE(IOUT,1024)
      ELSEIF(ISOTHM.EQ.3) THEN
        WRITE(IOUT,1026)
      ELSEIF(ISOTHM.EQ.4) THEN
        WRITE(IOUT,1027)
      ELSEIF(ISOTHM.EQ.5) THEN
        WRITE(IOUT,2027)
      ELSEIF(ISOTHM.EQ.6) THEN
        WRITE(IOUT,3027)
        IF(ISP1IM.GE.1) WRITE(IOUT,1042)                       !# LINE 82 RCT
      ELSE
        WRITE(IOUT,1028)
      ENDIF
      IF(IREACT.EQ.0) THEN
        WRITE(IOUT,1030)
      ELSEIF(ireact.eq.1) THEN
        WRITE(IOUT,1032)
      ELSEIF(ireact.eq.2) THEN                                 !# LINE 90 RCT
        WRITE(IOUT,1033)                                       !# LINE 91 RCT
      ELSEIF(ireact.eq.3) THEN                                 !# LINE 92 RCT
        WRITE(IOUT,1035)                                       !# LINE 93 RCT
      ELSEIF(ireact.eq.100) THEN
        WRITE(IOUT,1034)
      ENDIF
C                                                                !# LINE 97 RCT
C-----DUAL-DOMAIN NOT ALLOWED WITH MONOD KINETIC DISSOLVED       !# LINE 98 RCT
      IF(IREACT.EQ.2.AND.(ISOTHM.EQ.5.OR.ISOTHM.EQ.6)) THEN      !# LINE 99 RCT
        WRITE(IOUT,*) 'IREACT=2 NOT ALLOWED WITH DUAL-DOMAIN'    !# LINE 100 RCT
        WRITE(*,*) 'IREACT=2 NOT ALLOWED WITH DUAL-DOMAIN'       !# LINE 101 RCT
        READ(*,*)                                                !# LINE 102 RCT
        STOP                                                     !# LINE 103 RCT
      ENDIF                                                      !# LINE 104 RCT
C                                                                !# LINE 105 RCT
C-----DUAL-DOMAIN NOT ALLOWED WITH MONOD KINETIC DISSOLVED       !# LINE 106 RCT
      IF(IREACT.EQ.3.AND.(ISOTHM.EQ.5.OR.ISOTHM.EQ.6)) THEN      !# LINE 107 RCT
        WRITE(IOUT,*) 'IREACT=3 NOT ALLOWED WITH DUAL-DOMAIN'    !# LINE 108 RCT
        WRITE(*,*) 'IREACT=3 NOT ALLOWED WITH DUAL-DOMAIN'       !# LINE 109 RCT
        READ(*,*)                                                !# LINE 110 RCT
        STOP                                                     !# LINE 111 RCT
      ENDIF                                                      !# LINE 112 RCT
C                                                                !# LINE 113 RCT
C-----NEED MULTIPLE SPECIED WITH IREACT=3                        !# LINE 114 RCT
      IF(IREACT.EQ.3.AND.NCOMP.LT.2) THEN                        !# LINE 115 RCT
        WRITE(IOUT,*) 'IREACT=3 NOT ALLOWED WITH SINGLE SPECIES' !# LINE 116 RCT
        WRITE(*,*) 'IREACT=3 NOT ALLOWED WITH SINGLE SPECIES'    !# LINE 117 RCT
        READ(*,*)                                                !# LINE 118 RCT
        STOP                                                     !# LINE 119 RCT
      ENDIF                                                      !# LINE 120 RCT
C                                                                !# LINE 121 RCT
      IF(IREACTION.EQ.1) THEN                                    !# LINE 122 RCT
        WRITE(IOUT,1036)                                         !# LINE 123 RCT
      ELSEIF(IREACTION.EQ.2) THEN                                !# LINE 124 RCT
        WRITE(IOUT,1038)                                         !# LINE 125 RCT
      ELSEIF(IREACTION.EQ.0) THEN                                !# LINE 126 RCT
      ELSE                                                       !# LINE 127 RCT
        WRITE(IOUT,*) 'IREACTION MUST BE 0, 1, OR 2'             !# LINE 128 RCT
        WRITE(*,*) 'IREACTION MUST BE 0, 1, OR 2'                !# LINE 129 RCT
        READ(*,*)                                                !# LINE 130 RCT
        STOP                                                     !# LINE 131 RCT
      ENDIF                                                      !# LINE 132 RCT
C                                                                !# LINE 137 RCT
 1022 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [LINEAR]')
 1024 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [FREUNDLICH]')
 1026 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [LANGMUIR]')
 1027 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [NON-EQUILIBRIUM]')
 2027 FORMAT(1X,'DUAL DOMAIN MASS TRANSFER IS SIMULATED')
 3027 FORMAT(1X,'DUAL DOMAIN MASS TRANSFER WITH SORPTION IS SIMULATED')
 1028 FORMAT(1X,'NO SORPTION [OR DUAL-DOMAIN MODEL] IS SIMULATED')
 1030 FORMAT(1X,'NO FIRST-ORDER RATE REACTION IS SIMULATED')
 1032 FORMAT(1X,'FIRST-ORDER IRREVERSIBLE REACTION',
     & ' [RADIOACTIVE DECAY OR BIODEGRADATION] IS SIMULATED')
 1033 FORMAT(1X,'MONOD KINETIC REACTION IS SIMULATED')           !# LINE 148 RCT
 1035 FORMAT(1X,'FIRST-ORDER CHAIN REACTION IS SIMULATED')       !# LINE 149 RCT
 1034 FORMAT(1X,'ZEROTH-ORDER DECAY OR PRODUCTION IS SIMULATED')
C
 1036 FORMAT(1X,'REACTION BETWEEN 1 EA AND 1 ED IS SIMULATED') !# LINE 151 RCT
 1038 FORMAT(1X,'MULTI-SPECIES KINETIC REACTION MODULE IS SIMULATED')          !# LINE 152 RCT
 1042 FORMAT(1X,'SEPARATE Kd FOR TWO DOMAINS')                   !# LINE 154 RCT
      IF(IRCTOP.LE.1) THEN
        IRCTOP=1
        WRITE(*,1050)
      ELSEIF(IRCTOP.GE.2) THEN
        IRCTOP=2
        WRITE(IOUT,1052)
      ENDIF
 1050 FORMAT(/1X,'WARNING: INPUT FILE FOR VER 1 OF [RCT] PACKAGE',
     & ' DETECTED;'/1X,'REACTION COEFFICIENTS ASSIGNED ONE VALUE',
     & ' PER LAYER'/)
 1052 FORMAT(1X,'REACTION COEFFICIENTS ASSIGNED CELL-BY-CELL')
      IF(IGETSC.EQ.0) THEN
        WRITE(IOUT,1060)
      ELSEIF(IGETSC.GT.0.AND.ISOTHM.LE.3) THEN
        WRITE(*,1061)
        CALL USTOP(' ')
      ELSEIF(IGETSC.GT.0.AND.ISOTHM.GT.3) THEN
        WRITE(IOUT,1062)
      ENDIF
 1060 FORMAT(1X,'INITIAL SORBED/IMMOBILE PHASE CONCENTRATION',
     & ' ASSIGNED BY DEFAULT')
 1061 FORMAT(1X,'ERROR: INITIAL SORBED CONCENTRATION FOR',
     & ' EQUILIBRIUM-CONTROLLED SORPTION CANNOT BE SPECIFIED;',
     & /1X,'INPUT VALUE FOR [IGETSC] MUST BE SET TO ZERO')
 1062 FORMAT(1X,'INITIAL SORBED/IMMOBILE PHASE CONCENTRATION',
     & ' READ FROM INPUT FILE')
C
C--ALLOCATE IF IREACTION=1                                     !# LINE 183 RCT
      IF(IREACTION.EQ.1) THEN                                  !# LINE 184 RCT
        ALLOCATE (CRCT(NCOL,NROW,NLAY,2),STAT=IERR)            !# LINE 185 RCT
        IF(IERR.NE.0) THEN                                     !# LINE 186 RCT
          WRITE(*,106)                                         !# LINE 187 RCT
  106     FORMAT(1X,'RCT: MEMORY ALLOCATION ERROR')            !# LINE 188 RCT
          CALL USTOP(' ')                                      !# LINE 189 RCT
        ENDIF                                                  !# LINE 190 RCT
      ENDIF                                                    !# LINE 191 RCT
C                                                              !# LINE 192 RCT
C--ALLOCATE RC3 IS IREACT=2                                    !# LINE 193 RCT
      IF(IREACT.EQ.2) THEN                                     !# LINE 194 RCT
        ALLOCATE(RC3(NCOL,NROW,NLAY,NCOMP),STAT=IERR)          !# LINE 195 RCT
        IF(IERR.NE.0) THEN                                     !# LINE 196 RCT
          WRITE(*,107)                                         !# LINE 197 RCT
  107     FORMAT(1X,'RC3: MEMORY ALLOCATION ERROR')            !# LINE 198 RCT
          CALL USTOP(' ')                                      !# LINE 199 RCT
        ENDIF                                                  !# LINE 200 RCT
      ENDIF                                                    !# LINE 201 RCT
C                                                              !# LINE 202 RCT
C--ALLOCATE YLD IS IREACT=3                                    !# LINE 203 RCT
      IF(IREACT.EQ.3) THEN                                     !# LINE 204 RCT
        ALLOCATE(YLD(NCOMP-1),STAT=IERR)                       !# LINE 205 RCT
        IF(IERR.NE.0) THEN                                     !# LINE 206 RCT
          WRITE(*,108)                                         !# LINE 207 RCT
  108     FORMAT(1X,'YLD: MEMORY ALLOCATION ERROR')            !# LINE 208 RCT
          CALL USTOP(' ')                                      !# LINE 209 RCT
        ENDIF                                                  !# LINE 210 RCT
      ENDIF                                                    !# LINE 211 RCT
C                                                              !# LINE 212 RCT
C--ALLOCATE SP1IM                                              !# LINE 213 RCT
      IF(ISP1IM.GE.1) THEN                                     !# LINE 214 RCT
        ALLOCATE(SP1IM(NCOL,NROW,NLAY,NCOMP),STAT=IERR)        !# LINE 215 RCT
        IF(IERR.NE.0) THEN                                     !# LINE 216 RCT
          WRITE(*,109)                                         !# LINE 217 RCT
  109     FORMAT(1X,'SP1IM MEMORY ALLOCATION ERROR')           !# LINE 218 RCT
          CALL USTOP(' ')                                      !# LINE 219 RCT
        ENDIF                                                  !# LINE 220 RCT
      ENDIF                                                    !# LINE 222 RCT
C                                                              !# LINE 223 RCT
      IF(ISOTHM.NE.5.AND.ISOTHM.NE.6) THEN
        IF(ICIMDRY.GE.2) THEN
          ICIMDRY=0
          WRITE(IOUT,*) ' ICIMDRY ONLY ACTIVE WITH DUAL-DOMAIN'
          WRITE(IOUT,*) ' ICIMDRY RESET TO 0'
        ENDIF
      ENDIF

CLANGEVIN--NEED TO OPEN UCN2 FILES HERE INSTEAD OF IN BTN5AR
      IF(SAVUCN) THEN
        IF(ISOTHM.GT.0) THEN
          WRITE(IOUT,2046) IUCN2+1
          FLNAME='MT3DnnnS.UCN'
          DO INDEX=1,NCOMP
            WRITE(FLNAME(5:7),'(I3.3)') INDEX
            CALL OPENFL(-(IUCN2+INDEX),0,FLNAME,1,FINDEX)
          ENDDO
        ENDIF
      ENDIF
 2046 FORMAT(1X,'SAVE SORBED/IMMOBILE PHASE CONCENTRATIONS ',
     & 'IN UNFORMATTED FILES [MT3DnnnS.UCN]'/1X,' FOR EACH SPECIES ',
     & 'ON UNITS ',I3,' AND ABOVE, ',
     & 'IF SORPTION/MASS TRANSFER SIMULATED')

C
C--PRINT A HEADER
      WRITE(IOUT,2001)
 2001 FORMAT(//1X,'SORPTION AND 1ST/0TH ORDER REACTION PARAMETERS',
     & /1X,46('-')/)
C
C--CALL RARRAY TO READ IN SORPTION PARAMETERS IF SORPTION SIMULATED
      IF(ISOTHM.LE.0 .AND. IREACTION.NE.2) GOTO 2000           !# Amended
C
      IF(ISOTHM.EQ.5 .AND. IREACTION.NE.2) GOTO 111            !# Amended
      ANAME='BULK DENSITY (RHOB)    '
      IF(IRCTOP.EQ.2) THEN
        DO K=1,NLAY
          CALL RARRAY(RHOB(1:NCOL,1:NROW,K),ANAME,NROW,NCOL,K,IN,IOUT)
        ENDDO
      ELSEIF(IRCTOP.EQ.1) THEN
        CALL RARRAY(BUFF(1:NCOL,1:NROW,1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              RHOB(J,I,K)=BUFF(1,1,K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(ISOTHM.LE.0 .AND. IREACTION.EQ.2) GOTO 2000           !# LINE 309 RCT
  111 CONTINUE
C
      IF(ISOTHM.NE.5.AND.ISOTHM.NE.6) GOTO 222
      ANAME='IMMOBILE DOMAIN POROSITY'
      IF(IRCTOP.EQ.2) THEN
        DO K=1,NLAY
          CALL RARRAY(PRSITY2(1:NCOL,1:NROW,K),ANAME,NROW,NCOL,K,IN,
     &                IOUT)
        ENDDO
      ELSEIF(IRCTOP.EQ.1) THEN
        CALL RARRAY(BUFF(1:NCOL,1:NROW,1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              PRSITY2(J,I,K)=BUFF(1,1,K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
  222 CONTINUE
C
      IF(IGETSC.EQ.0) GOTO 333
      DO INDEX=1,NCOMP
        ANAME='STARTING S/IM.C COMP. NO'
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        IF(IRCTOP.EQ.2) THEN
          DO K=1,NLAY
            CALL RARRAY(SRCONC(1:NCOL,1:NROW,K,INDEX),
     &                  ANAME,NROW,NCOL,K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF(1,1,1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                SRCONC(J,I,K,INDEX)=BUFF(1,1,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO
  333 CONTINUE
C                                                                       !# LINE 391 RCT
      DO INDEX=1,NCOMP
        ANAME='1ST SORP. COEF. COMP. NO'
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        IF(IRCTOP.EQ.2) THEN
          DO K=1,NLAY
            CALL RARRAY(SP1(1:NCOL,1:NROW,K,INDEX),ANAME,NROW,NCOL,
     &                  K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF(1:NCOL,1:NROW,1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                SP1(J,I,K,INDEX)=BUFF(1,1,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
C-----IMMOBILE DOMAIN SORPTION COEFFICIENT                              !# LINE 370 RCT
      IF(ISP1IM.GE.1) THEN                                              !# LINE 371 RCT
        DO INDEX=1,NCOMP                                                !# LINE 372 RCT
          ANAME='IMM SORP. COEF. COMP. NO'                              !# LINE 373 RCT
          WRITE(ANAME(22:24),'(I3.2)') INDEX                            !# LINE 374 RCT
          IF(IRCTOP.EQ.2) THEN                                          !# LINE 375 RCT
            DO K=1,NLAY                                                 !# LINE 376 RCT
              CALL RARRAY(SP1IM(:,:,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT) !# LINE 377 RCT
            ENDDO                                                       !# LINE 378 RCT
          ELSEIF(IRCTOP.EQ.1) THEN                                      !# LINE 379 RCT
            CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)                    !# LINE 380 RCT
            DO K=1,NLAY                                                 !# LINE 381 RCT
              DO I=1,NROW                                               !# LINE 382 RCT
                DO J=1,NCOL                                             !# LINE 383 RCT
                  SP1IM(J,I,K,INDEX)=BUFF(1,1,K)                        !# LINE 384 RCT
                ENDDO                                                   !# LINE 385 RCT
              ENDDO                                                     !# LINE 386 RCT
            ENDDO                                                       !# LINE 387 RCT
          ENDIF                                                         !# LINE 388 RCT
        ENDDO                                                           !# LINE 389 RCT
      ENDIF                                                             !# LINE 390 RCT
C
      DO INDEX=1,NCOMP
        ANAME='2ND SORP. COEF. COMP. NO'
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        IF(IRCTOP.EQ.2) THEN
          DO K=1,NLAY
            CALL RARRAY(SP2(1:NCOL,1:NROW,K,INDEX),ANAME,NROW,NCOL,
     &                  K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF(1:NCOL,1:NROW,1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
            DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                SP2(J,I,K,INDEX)=BUFF(1,1,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
C--ENSURE NO SORPTION (SP1=0) IF ISOTHM=5
C--(ISOTHM=5 IS EQUIVALENT TO ISOTHM=6 WITH SP1=0)
      IF(ISOTHM.EQ.5) THEN
        DO INDEX=1,NCOMP
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                SP1(J,I,K,INDEX)=0.0
                IF(ISP1IM.GE.1) SP1IM(J,I,K,INDEX)=0.0         !# LINE 419 RCT
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--PRESET FRACTION OF SORPTION SITES IN CONTACT WITH MOBILE WATER
C--TO RATIO OF MOBILE TO TOTAL POROSITIES
      IF(ISOTHM.EQ.6) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              FRAC(J,I,K)=1.0
              TOTPOR=PRSITY(J,I,K)+PRSITY2(J,I,K)
              IF(TOTPOR.GT.TINY) FRAC(J,I,K)=PRSITY(J,I,K)/TOTPOR
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
 2000 CONTINUE
C
C--CALL RARRAY TO READ IN 1st/0th ORDER REACTION RATE CONSTANTS
C--IF NECESSARY
      IF(IREACT.ne.1.and.IREACT.ne.100.and.IREACT.ne.2.and.    !# Amended (LINE 444 RCT)
     1   IREACT.ne.3) GOTO 3000                                !# Amended (LINE 445 RCT)
C
      DO INDEX=1,NCOMP
        ANAME='SOLUTE RXN RATE: COMP NO'
        IF(IREACT.EQ.2) ANAME='Mt * Umax      : COMP NO'       !# LINE 449 RCT
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        IF(IRCTOP.EQ.2) THEN
          DO K=1,NLAY
            CALL RARRAY(RC1(1:NCOL,1:NROW,K,INDEX),ANAME,NROW,NCOL,
     &                  K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF(1,1,1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                RC1(J,I,K,INDEX)=BUFF(1,1,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
      DO INDEX=1,NCOMP
        ANAME='SORBED RXN RATE: COMP NO'
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        IF(IRCTOP.EQ.2) THEN
          DO K=1,NLAY
            CALL RARRAY(RC2(1:NCOL,1:NROW,K,INDEX),ANAME,NROW,NCOL,
     &                  K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF(1,1,1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                RC2(J,I,K,INDEX)=BUFF(1,1,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
      IF(IREACT.EQ.2) THEN                                          !# LINE 486 RCT
        DO INDEX=1,NCOMP                                            !# LINE 487 RCT
          ANAME='HALF-SAT. CONST: COMP NO'                          !# LINE 488 RCT
          WRITE(ANAME(22:24),'(I3.2)') INDEX                        !# LINE 489 RCT
          IF(IRCTOP.EQ.2) THEN                                      !# LINE 490 RCT
            DO K=1,NLAY                                             !# LINE 491 RCT
              CALL RARRAY(RC3(:,:,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT) !# LINE 492 RCT
            ENDDO                                                   !# LINE 493 RCT
          ELSEIF(IRCTOP.EQ.1) THEN                                  !# LINE 494 RCT
            CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)                !# LINE 495 RCT
            DO K=1,NLAY                                             !# LINE 496 RCT
              DO I=1,NROW                                           !# LINE 497 RCT
                DO J=1,NCOL                                         !# LINE 498 RCT
                  RC3(J,I,K,INDEX)=BUFF(1,1,K)                      !# LINE 499 RCT
                ENDDO                                               !# LINE 500 RCT
              ENDDO                                                 !# LINE 501 RCT
            ENDDO                                                   !# LINE 502 RCT
          ENDIF                                                     !# LINE 503 RCT
        ENDDO                                                       !# LINE 504 RCT
      ENDIF                                                         !# LINE 505 RCT
C                                                                   !# LINE 506 RCT
      IF(IREACT.EQ.3) THEN                                          !# LINE 507 RCT
        DO INDEX=1,NCOMP-1                                          !# LINE 508 RCT
          READ(IN,*) YLD(INDEX)                                     !# LINE 509 RCT
          WRITE(IOUT,'(18X,2(A,I3),A,G13.6)')                       !# LINE 510 RCT
     1      'YIELD COEFFICIENT BETWEEN SPECIES ',INDEX,             !# LINE 511 RCT
     1      ' AND ',INDEX+1,' = ', YLD(INDEX)                       !# LINE 512 RCT
        ENDDO                                                       !# LINE 513 RCT
      ENDIF                                                         !# LINE 514 RCT
C                                                                   !# LINE 515 RCT
 3000 CONTINUE
C
C-----READ REACTION RELATED DATA                                    !# LINE 518 RCT
      IF(IREACTION.EQ.1) THEN                                       !# LINE 519 RCT
        READ(IN,'(2I10,F10.0)') IED,IEA,FEDEA                       !# LINE 520 RCT
        WRITE(IOUT,104) IED,IEA,FEDEA                               !# LINE 521 RCT
104     FORMAT(1X,'SIMULATED REACTION: ED + FEDEA*EA --> PRODUCT'   !# LINE 522 RCT
     1        /1X,'ELECTRON DONOR COMPONENT (IED)         = ',I3    !# LINE 523 RCT
     1        /1X,'ELECTRON ACCEPTOR COMPONENT (IEA)      = ',I3    !# LINE 524 RCT
     1        /1X,'STOICHIOMETRIC RATIO (FEDEA)           = ',G10.4) !# LINE 525 RCT
C.......CHECK FOR POSSIBLE ERRORS                                   !# LINE 526 RCT
        IF(IED.GT.NCOMP .OR. IEA.GT.NCOMP) THEN                     !# LINE 527 RCT
          WRITE(*,*) 'IEA OR IED GREATER THAN NCOMP'                !# LINE 528 RCT
          WRITE(IOUT,*) 'IEA OR IED GREATER THAN NCOMP'             !# LINE 529 RCT
          CALL USTOP(' ')                                           !# LINE 530 RCT
        ENDIF                                                       !# LINE 531 RCT
        IF(IED.EQ.IEA) THEN                                         !# LINE 532 RCT
          WRITE(*,*) 'IEA AND IED CANNOT BE THE SAME NUMBER'        !# LINE 533 RCT
          WRITE(IOUT,*) 'IEA AND IED CANNOT BE THE SAME NUMBER'     !# LINE 534 RCT
          CALL USTOP(' ')                                           !# LINE 535 RCT
        ENDIF                                                       !# LINE 536 RCT
      ELSEIF(IREACTION.EQ.2) THEN                                   !# LINE 537 RCT
C-------READ REACTION FILE NAME                                     !# LINE 538 RCT
        rec_FileName=''                                             !# LINE 539 RCT
        READ(IN,'(A1000)') rec_FileName                              !# LINE 540 RCT
        INQUIRE(FILE=rec_FileName,EXIST=EXISTED)                    !# LINE 541 RCT
        IF(.NOT.EXISTED) THEN                                       !# LINE 542 RCT
          WRITE(*,*) 'FILE ',TRIM(rec_FileName),' DOES NOT ESIST'   !# LINE 543 RCT
          CALL USTOP('  ')                                          !# LINE 544 RCT
        ELSE                                                        !# LINE 545 RCT
          NODES=NLAY*NROW*NCOL                                      !# LINE 546 RCT
          CALL REACTION_PRE(NODES)                                  !# LINE 547 RCT (Modified)
        ENDIF                                                       !# LINE 548 RCT
      ENDIF                                                         !# LINE 569 RCT
C                                                                   !# LINE 570 RCT
C--DETERMINE DEFAULT CONCENTRATION FOR THE NONEQUILIBRIUM PHASE
C--WHICH REPRESENTS SORBED PHASE IN SINGLE-DOMAIN MODEL (ISOTHM=4)
C--OR IMMOBILE-LIQUID PHASE IN DUAL-DOMAIN MODEL (ISOTHM=5 OR 6)
      IF(IGETSC.EQ.0) THEN
        DO INDEX=1,NCOMP
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                IF(ICBUND(J,I,K,INDEX).EQ.0) CYCLE
                IF(ISOTHM.EQ.4) THEN
                  SRCONC(J,I,K,INDEX)=SP1(J,I,K,INDEX)*COLD(J,I,K,INDEX)
                ELSEIF(ISOTHM.EQ.5.OR.ISOTHM.EQ.6) THEN
                  SRCONC(J,I,K,INDEX)=0.
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--CALL [SRCT5R] TO CALCULATE RETARDATION FACTORS FOR BOTH DOMAINS
C--AND SORBED CONCENTRATION (SINGLE-DOMAIN MODEL)
C--OR IMMOBILE-LIQUID PHASE CONCENTRATION (DUAL-DOMAIN MODEL)
      IF(ISOTHM.GT.0) THEN
        RFMIN=1.E30
        TR=0.
        DO INDEX=1,NCOMP
          CALL SRCT5R(NCOL,NROW,NLAY,ICBUND(:,:,:,INDEX),PRSITY,
     &     COLD(:,:,:,INDEX),RETA(:,:,:,INDEX),RFMIN,RHOB,
     &     SP1(:,:,:,INDEX),SP2(:,:,:,INDEX),RC1(:,:,:,INDEX),
     &     RC2(:,:,:,INDEX),PRSITY2,RETA2(:,:,:,INDEX),FRAC,
     &     SRCONC(:,:,:,INDEX),ISOTHM,IREACT,TR,INDEX)         !#Amended (LINE 602 RCT)
        ENDDO
      ENDIF
C
C--CALCULATE SETPSIZE WHICH MEETS STABILITY CRITERION
C--OF 1ST ORDER REACTION TERM IF AN EXPLICIT SOLUTION SCHEME IS USED
      DTRCT=1.E30
      KR=0
      IR=0
      JR=0
      IF(IREACT.ne.1.AND.ISOTHM.LE.3) GOTO 4000
C
      DO INDEX=1,NCOMP
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,INDEX).GT.0) THEN
                TR=0.
                IF(IREACT.eq.1) TR=ABS(RC1(J,I,K,INDEX))
                IF(IREACT.eq.1.AND.ISOTHM.GT.0)
     &                     TR=TR+ABS(RC2(J,I,K,INDEX))
                IF(ISOTHM.GT.4) THEN
                  TR=TR+ABS(SP2(J,I,K,INDEX))/PRSITY(J,I,K)
                ENDIF
                IF(TR.GT.TINY) TR=1./TR
                IF(TR.GT.TINY.AND.TR.LT.DTRCT) THEN
                  DTRCT=TR
                  KR=K     
                  IR=I
                  JR=J
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--PRINT OUT INFORMATION ON DTRCT
      WRITE(IOUT,3050) DTRCT,KR,IR,JR
 3050 FORMAT(/1X,'MAXIMUM STEPSIZE WHICH MEETS STABILITY CRITERION',
     & ' OF THE REACTION TERM'/1X,'=',G11.4,
     & ' AT K=',I4,', I=',I4,', J=',I4)
C
C--PRINT OUT RETARDATION FACTOR IF REQUESTED
 4000 IF(IFMTRF.EQ.0) GOTO 5000
C
      DO INDEX=1,NCOMP
        ANAME='RETARD. FACTOR: COMP. NO'
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        DO K=1,NLAY
          CALL RPRINT(RETA(1:NCOL,1:NROW,K,INDEX),
     &     ANAME,0,1,1,NCOL,NROW,K,IFMTRF,IOUT)
        ENDDO
      ENDDO
C
C--RETURN
 5000 RETURN
      END
C
C
      SUBROUTINE SRCT5R(NCOL,NROW,NLAY,ICBUND,PRSITY,CNEW,RETA,RFMIN,
     & RHOB,SP1,SP2,RC1,RC2,PRSITY2,RETA2,FRAC,SRCONC,
     & ISOTHM,IREACT,DTRANS,ICOMP)                             !# LINE 664 RCT
C ********************************************************************
C THIS SUBROUTINE CALCULATES RETARDATION FACTOR AND CONCENTRATION
C OF SORBED (UNIT: MASS/MASS) FOR SINGLE-DOMAIN MODEL OR
C IMMOBILE-LIQUID PHASE (UNIT: MASS/VOLUME) FOR DUAL-DOMAIN MODEL.
C ********************************************************************
C last modified: 10-01-2005
C
      USE       RCTMOD                                         !# LINE 672 RCT
C                                                              !# LINE 673 RCT
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,ICBUND,ISOTHM,IREACT,J,I,K,
     &          ICOMP                                          !# LINE 675 RCT
      REAL      PRSITY,CNEW,RETA,RFMIN,RHOB,SP1,SP2,RC1,RC2,
     &          PRSITY2,FRAC,SRCONC,DTRANS,TINY,
     &          RETA2,TERM1,RC1TMP,RC2TMP
      DIMENSION PRSITY(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),
     &          CNEW(NCOL,NROW,NLAY),RETA(NCOL,NROW,NLAY),
     &          RHOB(NCOL,NROW,NLAY),SRCONC(NCOL,NROW,NLAY),
     &          SP1(NCOL,NROW,NLAY),SP2(NCOL,NROW,NLAY),
     &          RC1(NCOL,NROW,NLAY),RC2(NCOL,NROW,NLAY),
     &          PRSITY2(NCOL,NROW,NLAY),FRAC(NCOL,NROW,NLAY),
     &          RETA2(NCOL,NROW,NLAY)
      PARAMETER (TINY=1.E-30)
C
C--EVALUATE RETARDATION FACTOR AND SORBED CONCONCENTRATION
C--DEPENDING ON TYPES OF SORPTION SELECTED
C
C--1. LINEAR EQUILIBRIUM...
      IF(ISOTHM.EQ.1) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K).EQ.0) CYCLE
              RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*SP1(J,I,K)
              RFMIN=MIN(RFMIN,RETA(J,I,K))
              SRCONC(J,I,K)=SP1(J,I,K)*CNEW(J,I,K)
            ENDDO
          ENDDO
        ENDDO
C
C--2. FREUNDLICH EQUILIBRIUM...
      ELSEIF(ISOTHM.EQ.2) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K).EQ.0) CYCLE
              IF(CNEW(J,I,K).LE.0) THEN
                RETA(J,I,K)=1.
                SRCONC(J,I,K)=0.
              ELSE
                RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*
     &           SP1(J,I,K)*SP2(J,I,K)*CNEW(J,I,K)**(SP2(J,I,K)-1.)
                SRCONC(J,I,K)=SP1(J,I,K)*CNEW(J,I,K)**SP2(J,I,K)
              ENDIF
              RFMIN=MIN(RFMIN,RETA(J,I,K))
            ENDDO
          ENDDO
        ENDDO
C
C--3. LANGMUIR EQUILIBRIUM...
      ELSEIF(ISOTHM.EQ.3) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K).EQ.0) CYCLE
              IF(CNEW(J,I,K).LT.0) THEN
                RETA(J,I,K)=1.
                SRCONC(J,I,K)=0.
              ELSE
                RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*
     &           SP1(J,I,K)*SP2(J,I,K)/(1.+SP1(J,I,K)*CNEW(J,I,K))**2
                SRCONC(J,I,K)=SP1(J,I,K)*SP2(J,I,K)*CNEW(J,I,K)
     &           /(1.+SP1(J,I,K)*CNEW(J,I,K))
              ENDIF
              RFMIN=MIN(RFMIN,RETA(J,I,K))
            ENDDO
          ENDDO
        ENDDO
C
C--4. LINEAR NON-EQUILIBRIUM...
      ELSEIF(ISOTHM.EQ.4) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K).EQ.0.OR.DTRANS.LT.TINY) CYCLE
              RC2TMP=0.
              IF(IREACT.eq.1.or.IREACT.eq.100.or.IREACT.eq.2.or. !# Amended (LINE 750 RCT)
     1          IREACT.eq.3)                                     !# LINE 751 RCT
     1          RC2TMP=RC2(J,I,K)                                
C--if with no reaction or with first-order reaction               
              IF(ireact.eq.0.or.ireact.eq.1.or.ireact.eq.2.or.   !# Amended (LINE 754 RCT)
     1           ireact.eq.3) THEN                               !# Amended (LINE 755 RCT)
                SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K)+
     &           RHOB(J,I,K)/DTRANS*SRCONC(J,I,K))/
     &           (RHOB(J,I,K)/DTRANS+SP2(J,I,K)/SP1(J,I,K)
     &           +RC2TMP*RHOB(J,I,K))
C--if with zeroth-order reaction      
              ELSEIF(ireact.eq.100) THEN 
                SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K)+
     &           RHOB(J,I,K)/DTRANS*SRCONC(J,I,K)
     &           -RC2TMP*RHOB(J,I,K))/
     &           (RHOB(J,I,K)/DTRANS+SP2(J,I,K)/SP1(J,I,K))
              endif
            ENDDO
          ENDDO
        ENDDO
        RFMIN=1.
C
C--5/6. DUAL DOMAIN MASS TRANSFER WITHOUT/WITH LINEAR SORPTION
      ELSEIF(ISOTHM.EQ.5.OR.ISOTHM.EQ.6) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K).EQ.0) CYCLE
CVSB              IF(ISORBIMONLY.EQ..TRUE.) THEN                   !# LINE 778 RCT
CVSB                RETA(J,I,K)=1.+FRAC(J,I,K)*RHOB(J,I,K)         !# LINE 779 RCT
CVSB     &                       *0.0E0/PRSITY(J,I,K)              !# LINE 780 RCT
CVSB              ELSE                                             !# LINE 781 RCT
                RETA(J,I,K)=1.+FRAC(J,I,K)*RHOB(J,I,K)
     &                       *SP1(J,I,K)/PRSITY(J,I,K)
CVSB              ENDIF                                            !# LINE 784 RCT
              RFMIN=MIN(RFMIN,RETA(J,I,K))
              RETA2(J,I,K)=1.0
              IF(PRSITY2(J,I,K).GT.TINY) THEN                      !# Amended (LINE 787 RCT)
                IF(ISP1IM.EQ.0) THEN                               !# Amended (LINE 787 RCT)
                  RETA2(J,I,K)=1.+(1.-FRAC(J,I,K))
     &              *RHOB(J,I,K)*SP1(J,I,K)/PRSITY2(J,I,K)
                ELSE                                               !# Amended (LINE 791 RCT)
                  RETA2(J,I,K)=1.+(1.-FRAC(J,I,K))                 !# LINE 792 RCT
     &              *RHOB(J,I,K)*SP1IM(J,I,K,ICOMP)/PRSITY2(J,I,K) !# LINE 793 RCT
                ENDIF                                              !# LINE 794 RCT
              ENDIF                                                !# LINE 795 RCT
              IF(DTRANS.LT.TINY) CYCLE
              RC1TMP=0.
              RC2TMP=0.
              IF(IREACT.eq.1.or.IREACT.eq.100) THEN
                RC1TMP=RC1(J,I,K)
                RC2TMP=RC2(J,I,K)
              ENDIF    
C--if with no reaction or with first-order reaction            
              if(ireact.eq.0.or.ireact.eq.1) then
                TERM1=PRSITY2(J,I,K)*RETA2(J,I,K)/DTRANS+SP2(J,I,K)
     &           +RC1TMP*PRSITY2(J,I,K)
     &           +RC2TMP*PRSITY2(J,I,K)*(RETA2(J,I,K)-1.)
                SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K)
     &           +PRSITY2(J,I,K)*RETA2(J,I,K)/DTRANS*SRCONC(J,I,K))
     &           /TERM1
C--if with zeroth-order reaction     
              elseif(ireact.eq.100) then 
                TERM1=PRSITY2(J,I,K)*RETA2(J,I,K)/DTRANS+SP2(J,I,K)
                SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K)
     &           -RC1TMP*PRSITY2(J,I,K)
     &           -RC2TMP*(1.-FRAC(J,I,K))*RHOB(J,I,K)
     &           +PRSITY2(J,I,K)*RETA2(J,I,K)/DTRANS*SRCONC(J,I,K))
     &           /TERM1
              endif         
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE RCT5FM(ICOMP,ICBUND,PRSITY,DH,RHOB,SP1,SP2,SRCONC,
     &                  RC1,RC2,PRSITY2,RETA2,FRAC,DTRANS,
     &                  COLD,CNEW)                             !# LINE 832 RCT
C *******************************************************************
C THIS SUBROUTINE FORMULATES THE COEFFICIENT MATRIX [A] AND THE 
C RIGHT-HAND-SIDE MATRIX [RHS] FOR SORPTION AND 1ST/0TH ORDER 
C REACTION TERMS USING THE IMPLICIT FINITE-DIFFERENCE SCHEME.
C *******************************************************************
C last modified: 10-01-2005
C
      USE RCTMOD                                               !# LINE 840 RCT
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,DELR,DELC,NODES,
     &                         UPDLHS,ISOTHM,IREACT,A,RHS,MCOMP
C
      IMPLICIT  NONE
      INTEGER   ICOMP,ICBUND,K,I,J,N
      REAL      PRSITY,RHOB,SP1,SP2,RC1,RC2,PRSITY2,FRAC,DTRANS,
     &          SRCONC,DH,RETA2,TERM1,TINY,
     &          RC1TMP,RC2TMP,
     &          COLD,CNEW                                      !# LINE 847 RCT
      DIMENSION ICBUND(NODES,NCOMP),PRSITY(NODES),
     &          RHOB(NODES),SP1(NODES,NCOMP),SP2(NODES,NCOMP),
     &          RC1(NODES,NCOMP),RC2(NODES,NCOMP),SRCONC(NODES,NCOMP),
     &          DH(NODES),PRSITY2(NODES),RETA2(NODES,NCOMP),FRAC(NODES),
     &          CNEW(NODES,NCOMP),COLD(NODES,NCOMP)            !# LINES 854 RCT
      PARAMETER (TINY=1.E-30)
      INTEGER   MM                                             !# LINES 856 RCT
C
C--CONTRIBUTIONS TO [A] AND [RHS] FROM NONEQUILIBRIUM SORPTION
C
      IF(ISOTHM.EQ.4) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
C
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
              IF(ICBUND(N,ICOMP).LE.0) CYCLE
C
C--UPDATE COEFFICIENT MATRIX A AND RHS IF NECESSARY
              RC2TMP=0.
              IF(IREACT.eq.1.or.IREACT.eq.100.or.IREACT.eq.2.or. !# Amended (LINE 871 RCT)
     1           IREACT.eq.3)                                    !# LINE 872 RCT
     1           RC2TMP=RC2(N,ICOMP)                             !# Amended (LINE 873 RCT)
C--if with no reaction or with first-order reaction             
              IF(IREACT.EQ.0.OR.IREACT.EQ.1.OR.IREACT.EQ.2.OR.   !# Amended (LINE 875 RCT)
     1           IREACT.EQ.3) THEN                               !# Amended (LINE 876 RCT)
                IF(UPDLHS) A(N)=A(N)-SP2(N,ICOMP)*DELR(J)*DELC(I)
     &           *DH(N)*(1.-SP2(N,ICOMP)/SP1(N,ICOMP)
     &           /(RHOB(N)/DTRANS+SP2(N,ICOMP)/SP1(N,ICOMP)
     &           +RC2TMP*RHOB(N)))
                RHS(N)=RHS(N)-SP2(N,ICOMP)/SP1(N,ICOMP)*DELR(J)*DELC(I)
     &           *DH(N)*RHOB(N)*SRCONC(N,ICOMP)/DTRANS
     &           /(RHOB(N)/DTRANS+SP2(N,ICOMP)/SP1(N,ICOMP)
     &           +RC2TMP*RHOB(N))
                IF(ireact.eq.3.AND.ICOMP.GT.1)                   !# LINE 885 RCT
     1          RHS(N)=RHS(N)-YLD(ICOMP-1)*RC2(N,ICOMP-1)        !# LINE 886 RCT
     &           *DELR(J)*DELC(I)*DH(N)                          !# LINE 887 RCT
     &           *RHOB(N)*SRCONC(N,ICOMP-1)                      !# LINE 888 RCT
C--if with zeroth-order reaction     
              ELSEIF(IREACT.EQ.100) then
                IF(UPDLHS) A(N)=A(N)-SP2(N,ICOMP)*DELR(J)*DELC(I)
     &           *DH(N)*(1.-SP2(N,ICOMP)/SP1(N,ICOMP)
     &           /(RHOB(N)/DTRANS+SP2(N,ICOMP)/SP1(N,ICOMP)))
                RHS(N)=RHS(N)+(SP2(N,ICOMP)/SP1(N,ICOMP)
     &           *DELR(J)*DELC(I)*DH(N)*RHOB(N)
     &           *(RC2TMP-SRCONC(N,ICOMP)/DTRANS))
     &           /(RHOB(N)/DTRANS+SP2(N,ICOMP)/SP1(N,ICOMP))              
              endif   
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--CONTRIBUTIONS TO [A] AND [RHS] FROM DUAL-DOMAIN MASS TRANSFER
C
      IF(ISOTHM.EQ.5.OR.ISOTHM.EQ.6) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
C
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
              IF(ICBUND(N,ICOMP).LE.0) CYCLE              
C
C--UPDATE COEFFICIENT MATRIX A AND RHS IF NECESSARY              
              RC1TMP=0.
              RC2TMP=0.
              IF(IREACT.eq.1.or.IREACT.eq.100) THEN
                RC1TMP=RC1(N,ICOMP)
                RC2TMP=RC2(N,ICOMP)
              ENDIF
C--if with no reaction or with first-order reaction
              if(ireact.eq.0.or.ireact.eq.1) then
                TERM1=PRSITY2(N)*RETA2(N,ICOMP)/DTRANS+SP2(N,ICOMP)
     &           +RC1TMP*PRSITY2(N)
     &           +RC2TMP*PRSITY2(N)*(RETA2(N,ICOMP)-1.)
                IF(UPDLHS) A(N)=A(N)-SP2(N,ICOMP)
     &           *DELR(J)*DELC(I)*DH(N)*(1.-SP2(N,ICOMP)/TERM1)
                RHS(N)=RHS(N)-SP2(N,ICOMP)
     &           *PRSITY2(N)*RETA2(N,ICOMP)*DELR(J)*DELC(I)*DH(N)
     &           *SRCONC(N,ICOMP)/(DTRANS*TERM1)
C--if with zeroth-order reaction      
              elseif(ireact.eq.100) then
                TERM1=PRSITY2(N)*RETA2(N,ICOMP)/DTRANS+SP2(N,ICOMP)
                IF(UPDLHS) A(N)=A(N)-SP2(N,ICOMP)
     &           *DELR(J)*DELC(I)*DH(N)*(1.-SP2(N,ICOMP)/TERM1)
                RHS(N)=RHS(N)-SP2(N,ICOMP)*DELR(J)*DELC(I)*DH(N)
     &           *(-RC1TMP*PRSITY2(N)-RC2TMP*(1.-FRAC(N))*RHOB(N)
     &           +PRSITY2(N)*RETA2(N,ICOMP)*SRCONC(N,ICOMP)/DTRANS)
     &           /TERM1             
              endif
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--CONTRIBUTIONS TO [A] AND [RHS] FROM 1ST ORDER KINETIC REACTION
C
      IF(ireact.eq.1) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
C
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
              IF(ICBUND(N,ICOMP).LE.0) CYCLE
C
C--DISSOLVED PHASE
              IF(UPDLHS) A(N)=A(N)-RC1(N,ICOMP)*PRSITY(N)
     &          *DELR(J)*DELC(I)*DH(N)
C
C--SORBED PHASE FOR EQUILIBRIUM-CONTROLLED ISOTHERMS
              IF(ISOTHM.EQ.1) THEN
                IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*RHOB(N)
     &           *DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP)
              ELSEIF(ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
                RHS(N)=RHS(N)+RC2(N,ICOMP)*DELR(J)*DELC(I)*DH(N)
     &           *RHOB(N)*SRCONC(N,ICOMP)
              ELSEIF(ISOTHM.EQ.6) THEN
CVSB                IF(ISORBIMONLY.EQ..TRUE.) THEN                !# LINE 970 RCT
CVSB                  IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*FRAC(N)*  !# LINE 971 RCT
CVSB     &            RHOB(N)*DELR(J)*DELC(I)*DH(N)*0.0E0         !# LINE 972 RCT
CVSB                ELSE                                          !# LINE 973 RCT
                IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*FRAC(N)*     
     &           RHOB(N)*DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP)    
CVSB                ENDIF                                         !# LINE 976 RCT
              ENDIF                                               !# LINE 977 RCT
            ENDDO                                                 !# LINE 978 RCT
          ENDDO                                                   !# LINE 979 RCT
        ENDDO                                                     !# LINE 980 RCT
      ENDIF                                                       !# LINE 981 RCT
C                                                                 !# LINE 982 RCT
C--MONOD KINETIC REACTION                                         !# LINE 983 RCT
C                                                                 !# LINE 984 RCT
      IF(IREACT.EQ.2) THEN                                        !# LINE 985 RCT
        DO K=1,NLAY                                               !# LINE 986 RCT
          DO I=1,NROW                                             !# LINE 987 RCT
            DO J=1,NCOL                                           !# LINE 988 RCT
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                      !# LINE 989 RCT
C                                                                 !# LINE 990 RCT
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL                !# LINE 991 RCT
              IF(ICBUND(N,ICOMP).LE.0) CYCLE                      !# LINE 992 RCT
C                                                                 !# LINE 993 RCT
C--DISSOLVED PHASE                                                !# LINE 994 RCT
              IF(UPDLHS) A(N)=A(N)-RC1(N,ICOMP)*PRSITY(N)         !# LINE 995 RCT
     &          *DELR(J)*DELC(I)*DH(N)                            !# LINE 996 RCT
     &          /(RC3(J,I,K,ICOMP)+COLD(N,ICOMP))                 !# LINE 997 RCT
            ENDDO                                                 !# LINE 998 RCT
          ENDDO                                                   !# LINE 990 RCT
        ENDDO                                                     !# LINE 1000 RCT
      ENDIF                                                       !# LINE 1001 RCT
C                                                                 !# LINE 1002 RCT
C--CONTRIBUTIONS TO [A] AND [RHS] FROM 1ST ORDER KINETIC REACTION WITH CHAIN DECAY !# LINE 1003 RCT
C                                                                 !# LINE 1004 RCT
      IF(IREACT.EQ.3) THEN                                        !# LINE 1005 RCT
        DO K=1,NLAY                                               !# LINE 1006 RCT
          DO I=1,NROW                                             !# LINE 1007 RCT
            DO J=1,NCOL                                           !# LINE 1008 RCT
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                      !# LINE 1009 RCT
C                                                                 !# LINE 1010 RCT
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL                !# LINE 1011 RCT
              IF(ICBUND(N,ICOMP).LE.0) CYCLE                      !# LINE 1012 RCT
C                                                                 !# LINE 1013 RCT
C--DISSOLVED PHASE                                                !# LINE 1014 RCT
              IF(UPDLHS) A(N)=A(N)-RC1(N,ICOMP)*PRSITY(N)         !# LINE 1015 RCT
     &          *DELR(J)*DELC(I)*DH(N)                            !# LINE 1016 RCT
              IF(ICOMP.GT.1) !.and.Cold(n,icomp-1).lt.rctms(icomp-1,2)) !# LINE 1017 RCT
     &         RHS(N)=RHS(N)-YLD(ICOMP-1)                         !# LINE 1018 RCT
     &          *RC1(N,ICOMP-1)*CNEW(N,ICOMP-1) !*Cold(N,ICOMP-1) !# LINE 1019 RCT
     &          *PRSITY(N)*DELR(J)*DELC(I)*DH(N)                  !# LINE 1020 RCT
C                                                                 !# LINE 1021 RCT
C--SORBED PHASE FOR EQUILIBRIUM-CONTROLLED ISOTHERMS              !# LINE 1022 RCT
              IF(ISOTHM.EQ.1) THEN                                !# LINE 1023 RCT
                IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*RHOB(N)         !# LINE 1024 RCT
     &           *DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP)              !# LINE 1025 RCT
                IF(ICOMP.GT.1) RHS(N)=RHS(N)-YLD(ICOMP-1)         !# LINE 1026 RCT
     &           *RC2(N,ICOMP-1)*RHOB(N)*CNEW(N,ICOMP-1) !*Cold(N,ICOMP-1) !# LINE 1027 RCT
     &           *DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP-1)            !# LINE 1028 RCT
              ELSEIF(ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN             !# LINE 1029 RCT
                RHS(N)=RHS(N)+RC2(N,ICOMP)*DELR(J)*DELC(I)*DH(N)  !# LINE 1030 RCT
     &           *RHOB(N)*SRCONC(N,ICOMP)                         !# LINE 1031 RCT
                IF(ICOMP.GT.1) RHS(N)=RHS(N)-YLD(ICOMP-1)         !# LINE 1032 RCT
     &           *RC2(N,ICOMP-1)*DELR(J)*DELC(I)*DH(N)            !# LINE 1033 RCT
     &           *RHOB(N)*SRCONC(N,ICOMP-1)                       !# LINE 1034 RCT
              ELSEIF(ISOTHM.EQ.6) THEN                            !# LINE 1035 RCT
CVSB                IF(ISORBIMONLY.EQ..TRUE.) THEN                !# LINE 1036 RCT
CVSB                  IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*FRAC(N)*  !# LINE 1037 RCT
CVSB     &            RHOB(N)*DELR(J)*DELC(I)*DH(N)*0.0E0         !# LINE 1038 RCT
CVSB                ELSE                                          !# LINE 1039 RCT
                  IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*FRAC(N)*      !# LINE 1040 RCT
     &            RHOB(N)*DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP)      !# LINE 1041 RCT
CVSB                ENDIF                                         !# LINE 1042 RCT
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--CONTRIBUTIONS TO [A] AND [RHS] FROM ZEROTH-ORDER REACTION
C
      IF(ireact.eq.100) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
C
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
              IF(ICBUND(N,ICOMP).LE.0) CYCLE
C
C--DISSOLVED PHASE
              RHS(N)=RHS(N)+RC1(N,ICOMP)*PRSITY(N)
     &          *DELR(J)*DELC(I)*DH(N)
C
C--SORBED PHASE FOR EQUILIBRIUM-CONTROLLED ISOTHERMS
              IF(ISOTHM.EQ.1.OR.ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
                RHS(N)=RHS(N)+RC2(N,ICOMP)*RHOB(N)
     &           *DELR(J)*DELC(I)*DH(N)
              ELSEIF(ISOTHM.EQ.6) THEN
                 RHS(N)=RHS(N)+RC2(N,ICOMP)*FRAC(N)*
     &           RHOB(N)*DELR(J)*DELC(I)*DH(N)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF                 
C
C--REACTION: ED + FEAED*EA --> PRODUCT                               !# LINE 1077 RCT
      IF(IREACTION.EQ.1) THEN                                        !# LINE 1078 RCT
      IF(ICOMP.EQ.IED.OR.ICOMP.EQ.IEA) THEN                          !# LINE 1079 RCT
        DO K=1,NLAY                                                  !# LINE 1080 RCT
          DO I=1,NROW                                                !# LINE 1081 RCT
            DO J=1,NCOL                                              !# LINE 1082 RCT
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                         !# LINE 1083 RCT
C                                                                    !# LINE 1084 RCT
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL                   !# LINE 1085 RCT
              IF(ICBUND(N,ICOMP).LE.0) CYCLE                         !# LINE 1086 RCT
C                                                                    !# LINE 1087 RCT
              IF(ICOMP.EQ.IED) THEN                                  !# LINE 1088 RCT
                IF(COLD(N,IED).GE.COLD(N,IEA)/FEDEA) THEN            !# LINE 1089 RCT
cvsb                  RHS(N)=RHS(N)+(COLD(N,IEA)/FEDEA)              !# LINE 1090 RCT
cvsb     &              *PRSITY(N)*DELR(J)*DELC(I)*DH(N)             !# LINE 1091 RCT
                ELSE                                                 !# LINE 1092 RCT
cvsb                  RHS(N)=RHS(N)+(COLD(N,IED))                    !# LINE 1093 RCT
cvsb     &              *PRSITY(N)*DELR(J)*DELC(I)*DH(N)             !# LINE 1094 RCT
                ENDIF                                                !# LINE 1095 RCT
              ELSE                                                   !# LINE 1096 RCT
                IF(COLD(N,IEA).GE.COLD(N,IED)*FEDEA) THEN            !# LINE 1097 RCT
cvsb                  RHS(N)=RHS(N)+(COLD(N,IED)*FEDEA)              !# LINE 1098 RCT
cvsb     &              *PRSITY(N)*DELR(J)*DELC(I)*DH(N)             !# LINE 1099 RCT
                ELSE                                                 !# LINE 1100 RCT
cvsb                  RHS(N)=RHS(N)+(COLD(N,IEA))                    !# LINE 1101 RCT
cvsb     &              *PRSITY(N)*DELR(J)*DELC(I)*DH(N)             !# LINE 1102 RCT
                ENDIF                                                !# LINE 1103 RCT
              ENDIF                                                  !# LINE 1104 RCT
            ENDDO                                                    !# LINE 1105 RCT
          ENDDO                                                      !# LINE 1106 RCT
        ENDDO                                                        !# LINE 1107 RCT
      ENDIF                                                          !# LINE 1108 RCT
C                                                                    !# LINE 1109 RCT
      ELSEIF(IREACTION.EQ.2) THEN                                    !# LINE 1110 RCT
        IF(ICOMP.EQ.NCOMP.AND.IFESLD.EQ.1) RETURN                    !# LINE 1111 RCT
        DO K=1,NLAY                                                  !# LINE 1112 RCT
          DO I=1,NROW                                                !# LINE 1113 RCT
            DO J=1,NCOL                                              !# LINE 1114 RCT
            
              IF(K.EQ.3) THEN
              IF(I.EQ.123)THEN
              IF(J.EQ.70)THEN
              CONTINUE
              ENDIF
              ENDIF
              ENDIF
            
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                         !# LINE 1115 RCT
C                                                                    !# LINE 1116 RCT
              DCDT_S(N,ICOMP)=0.                                     !# LINE 1117 RCT
              IF(ICOMP.GT.NED) DCDT_FE(N,ICOMP-NED,1:NED)=0.         !# LINE 1118 RCT
              DCDT(ICOMP)=0.                                         !# LINE 1119 RCT
              DEA_ED_DT(1:NED)=0.                                    !# LINE 1120 RCT
              RVAL=0.                                                !# LINE 1121 RCT
C                                                                    !# LINE 1122 RCT
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL                   !# LINE 1123 RCT
              IF(ICBUND(N,ICOMP).LE.0)  CYCLE                        !# LINE 1124 RCT
C                                                                    !# LINE 1125 RCT
C--DISSOLVED PHASE                                                   !# LINE 1126 RCT
              IF(UPDLHS) THEN                                        !# LINE 1127 RCT
C	          ASSIGN CONCENTRATIONS                                !# LINE 1128 RCT
                RCOLD=COLD(N,1:NCOMP)                                !# LINE 1129 RCT
                DO MM=1,NCOMP                                        !# LINE 1130 RCT
                  IF(RCOLD(MM)<0.0) RCOLD(MM)=0.0                    !# LINE 1131 RCT
                ENDDO                                                !# LINE 1132 RCT
C                                                                    !# LINE 1133 RCT
	          IF(ABS(SUM(RCOLD(1:NED))).LE.1.0E-7) CYCLE           !# LINE 1134 RCT
                IF(RCOLD(ICOMP).LE.1.0E-7) CYCLE                     !# LINE 1135 RCT
!	          IF(RCOLD(ICOMP)/INIC(J,I,K,ICOMP)<=1.E-4.and.ICOMP>NED)CYCLE !# LINE 1136 RCT
CCC                DO MM=1,MCOMP                                        !# LINE 1130 RCT
CCC	          IF(SPECIAL(MM).EQ."SOLID".AND.IFESLD.GT.0) THEN   !# LINE 1137 RCT
CCC	            MAXEC(MM)=COLD(N,NCOMP)/PRSITY(N)*RHOB(N)       !# LINE 1138 RCT
CCC	          ENDIF                                                !# LINE 1139 RCT
CCC                ENDDO
	          IF(SPECIAL(ICOMP).EQ."SOLID".AND.IFESLD.GT.0) THEN   !# LINE 1137 RCT
	            MAXEC(ICOMP)=COLD(N,NCOMP)/PRSITY(N)*RHOB(N)       !# LINE 1138 RCT
	          ENDIF                                                !# LINE 1139 RCT
C                                                                    !# LINE 1140 RCT
		        CALL reaction_sub(ICOMP,1)                         !# LINE 1141 RCT
C                                                                    !# LINE 1142 RCT
                DCDT_S(N,ICOMP)=DCDT(ICOMP)                          !# LINE 1146 RCT
C                                                                    !# LINE 1147 RCT
                IF(ICOMP.LE.NED) THEN                                !# LINE 1148 RCT
                  A(N)=A(N)+DCDT(ICOMP)*                             !# LINE 1149 RCT
     +		        PRSITY(N)*DELR(J)*DELC(I)*DH(N)                !# LINE 1150 RCT
                ELSE                                                 !# LINE 1151 RCT
                  DO MM=1,NED                                        !# LINE 1152 RCT
			          DCDT_FE(N,ICOMP-NED,MM)=DEA_ED_DT(MM) !# LINE 1144 RCT
			          RHS(N)=RHS(N)-DEA_ED_DT(MM)* COLD(N,MM)*     !# LINE 1153 RCT
     +		                      PRSITY(N)*DELR(J)*DELC(I)*DH(N)  !# LINE 1154 RCT
                  ENDDO                                              !# LINE 1155 RCT
                ENDIF                                                !# LINE 1156 RCT
              ENDIF                                                  !# LINE 1157 RCT
     		    ENDDO                                                    !# LINE 1158 RCT
          ENDDO                                                      !# LINE 1159 RCT
	  ENDDO                                                        !# LINE 1160 RCT
      ENDIF                                                          !# LINE 1161 RCT
C                                                                    !# LINE 1162 RCT
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE RCT5BD(ICOMP,DTRANS)
C **********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGET ASSOCIATED WITH REACTIONS.
C **********************************************************************
C last modified: 10-01-2005
C
      USE MIN_SAT, ONLY: VAQSAT,ICIMDRY
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,PRSITY,
     &                         DELR,DELC,DH,ISOTHM,IREACT,RHOB,SP1,SP2,
     &                         SRCONC,RC1,RC2,PRSITY2,RETA2,FRAC,CNEW,
     &                         RETA,RFMIN,RMASIO,
     &                         COLD                            !# LINE 1170 RCT
      USE RCTMOD                                               !# LINE 1176 RCT
C
      IMPLICIT  NONE
      INTEGER   ICOMP,K,I,J
      REAL      DTRANS,DCRCT,CMML,CMMS,CIML,CIMS,VOLUME,DCRCT2
      REAL      CTMP,DCRCTX,DCRCTX2                              !# LINE 1181-1182 RCT
      INTEGER   M,N                                              !# LINE 1192 RCT
C
C--UPDATE RETARDATION FACTOR AND SORBED/IMMOBILE-PHASE CONCENTRATION
C
      IF(ISOTHM.GT.0) THEN
C                                                                !# LINE 1197 RCT
C--DETERMINE WHICH CONC TO USE IF IREACTION=1                    !# LINE 1198 RCT
      IF(IREACTION.EQ.1) THEN                                    !# LINE 1199 RCT
        IF(ICOMP.EQ.IED) THEN                                    !# LINE 1200 RCT
          CALL SRCT5R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY, !# LINE 1201 RCT
     &    CRCT(:,:,:,1),RETA(:,:,:,ICOMP),RFMIN,RHOB,            !# LINE 1202 RCT
     &    SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),    !# LINE 1203 RCT
     &    RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,      !# LINE 1204 RCT
     &    SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)        !# LINE 1205 RCT
        ELSEIF(ICOMP.EQ.IEA) THEN                                !# LINE 1206 RCT
          CALL SRCT5R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY, !# LINE 1207 RCT
     &    CRCT(:,:,:,2),RETA(:,:,:,ICOMP),RFMIN,RHOB,            !# LINE 1208 RCT
     &    SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),    !# LINE 1209 RCT
     &    RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,      !# LINE 1210 RCT
     &    SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)        !# LINE 1211 RCT
        ELSE                                                     !# LINE 1212 RCT
          CALL SRCT5R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY, !# LINE 1213 RCT
     &    CNEW(:,:,:,ICOMP),RETA(:,:,:,ICOMP),RFMIN,RHOB,        !# LINE 1214 RCT
     &    SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),    !# LINE 1215 RCT
     &    RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,      !# LINE 1216 RCT
     &    SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)        !# LINE 1217 RCT
        ENDIF                                                    !# LINE 1218 RCT
      ELSE                                                       !# LINE 1219 RCT
        CALL SRCT5R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY,
     &   CNEW(:,:,:,ICOMP),RETA(:,:,:,ICOMP),RFMIN,RHOB,
     &   SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),
     &   RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,
     &   SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)         !# LINE 1224 RCT
      ENDIF                                                      !# LINE 1225 RCT
      ENDIF
C
C--CALCULATE MASS BUDGETS FOR
C--NONEQUILIBRIUM SORPTION IN SINGLE-DOMAIN MODEL
C
      IF(ISOTHM.EQ.4) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
C
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
              IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
C
C--DETERMINE WHICH CONC TO USE IF IREACTION=1                  !# LINE 1239 RCT
              IF(IREACTION.EQ.1) THEN                          !# LINE 1240 RCT
                IF(ICOMP.EQ.IED) THEN                          !# LINE 1241 RCT
                  CTMP=CRCT(J,I,K,1)                           !# LINE 1242 RCT
                ELSEIF(ICOMP.EQ.IEA) THEN                      !# LINE 1243 RCT
                  CTMP=CRCT(J,I,K,2)                           !# LINE 1244 RCT
                ELSE                                           !# LINE 1245 RCT
                  CTMP=CNEW(J,I,K,ICOMP)                       !# LINE 1246 RCT
                ENDIF                                          !# LINE 1247 RCT
              ELSE                                             !# LINE 1248 RCT
                CTMP=CNEW(J,I,K,ICOMP)                         !# LINE 1249 RCT
              ENDIF                                            !# LINE 1250 RCT
C                                                              !# LINE 1251 RCT
C--CALCULATE SOLUTE MASS CHANGE
              DCRCT=-SP2(J,I,K,ICOMP)*(CTMP !CNEW(J,I,K,ICOMP) !# LINE 1253 RCT
     &         -SRCONC(J,I,K,ICOMP)/SP1(J,I,K,ICOMP))
     &         *DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
C
C--RECORD SORBED MASS STORAGE CHANGE
              IF(DCRCT.LT.0) THEN
                RMASIO(120,2,ICOMP)=RMASIO(120,2,ICOMP)+DCRCT
              ELSE
                RMASIO(120,1,ICOMP)=RMASIO(120,1,ICOMP)+DCRCT
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--CALCULATE MASS BUDGETS FOR
C--MASS TRANSFER IN DUAL-DOMAIN MODEL
C
      IF(ISOTHM.EQ.5.OR.ISOTHM.EQ.6) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
C
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
              IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
C
C--DETERMINE WHICH CONC TO USE IF IREACTION=1                  !# LINE 1279 RCT
              IF(IREACTION.EQ.1) THEN                          !# LINE 1280 RCT
                IF(ICOMP.EQ.IED) THEN                          !# LINE 1281 RCT
                  CTMP=CRCT(J,I,K,1)                           !# LINE 1282 RCT
                ELSEIF(ICOMP.EQ.IEA) THEN                      !# LINE 1283 RCT
                  CTMP=CRCT(J,I,K,2)                           !# LINE 1284 RCT
                ELSE                                           !# LINE 1285 RCT
                  CTMP=CNEW(J,I,K,ICOMP)                       !# LINE 1286 RCT
                ENDIF                                          !# LINE 1287 RCT
              ELSE                                             !# LINE 1288 RCT
                CTMP=CNEW(J,I,K,ICOMP)                         !# LINE 1289 RCT
              ENDIF                                            !# LINE 1290 RCT
C                                                              !# LINE 1291 RCT
C--CALCULATE CHANGE IN CONCENTRATION OF MOBILE-LIQUID PHASE
              DCRCT=-SP2(J,I,K,ICOMP)*(CTMP !CNEW(J,I,K,ICOMP) !# LINE 1293 RCT
     &         -SRCONC(J,I,K,ICOMP))*DTRANS
     &         *DELR(J)*DELC(I)*DH(J,I,K)
C
C--SAVE LATEST SATURATED VOLUME OF AQUIFER TO HANDLE IMMOBILE DOMAIN IN DRY CELLS
              IF(ICIMDRY.GE.2) VAQSAT(J,I,K)=DELR(J)*DELC(I)*DH(J,I,K)
C
C--RECORD MASS STORAGE CHANGE IN IMMOBILE DOMAIN
              IF(DCRCT.LT.0) THEN
                RMASIO(121,2,ICOMP)=RMASIO(121,2,ICOMP)+DCRCT
     &          /RETA2(J,I,K,ICOMP)
                RMASIO(122,2,ICOMP)=RMASIO(122,2,ICOMP)+DCRCT
     &          *(RETA2(J,I,K,ICOMP)-1.)/RETA2(J,I,K,ICOMP)
              ELSE
                RMASIO(121,1,ICOMP)=RMASIO(121,1,ICOMP)+DCRCT
     &          /RETA2(J,I,K,ICOMP)
                RMASIO(122,1,ICOMP)=RMASIO(122,1,ICOMP)+DCRCT
     &          *(RETA2(J,I,K,ICOMP)-1.)/RETA2(J,I,K,ICOMP)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--CALCULATE MASS BUDGETS FOR
C--1st/0th ORDER IRREVERSIBLE REACTION
C
      IF(IREACT.ne.1.and.IREACT.ne.100.and.IREACT.ne.2.and.IREACT.ne.3) !# Amended (LINE 1317 RCT)
     1  goto 9999                                                       !# Amended (LINE 1318 RCT)
C
C--SKIP IF NOT SINGLE-DOMAIN MODEL
      IF(ISOTHM.EQ.5.OR.ISOTHM.EQ.6) GOTO 1000
C
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
C
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
C
C--DETERMINE WHICH CONC TO USE IF IREACTION=1                  !# LINE 1330 RCT
            IF(IREACTION.EQ.1) THEN                            !# LINE 1331 RCT
              IF(ICOMP.EQ.IED) THEN                            !# LINE 1332 RCT
                CTMP=CRCT(J,I,K,1)                             !# LINE 1333 RCT
              ELSEIF(ICOMP.EQ.IEA) THEN                        !# LINE 1334 RCT
                CTMP=CRCT(J,I,K,2)                             !# LINE 1335 RCT
              ELSE                                             !# LINE 1336 RCT
                CTMP=CNEW(J,I,K,ICOMP)                         !# LINE 1337 RCT
              ENDIF                                            !# LINE 1338 RCT
            ELSE                                               !# LINE 1339 RCT
              CTMP=CNEW(J,I,K,ICOMP)                           !# LINE 1340 RCT
            ENDIF                                              !# LINE 1341 RCT
C                                                              !# LINE 1342 RCT
C--SKIP IF CONCENTRATION IS NOT POSITIVE
            IF(CTMP.LE.0) CYCLE                                !# LINE 1344 RCT
C
C--DISSOLVED PHASE
            IF(ireact.eq.1) THEN
              DCRCT=-RC1(J,I,K,ICOMP)*CTMP !CNEW(J,I,K,ICOMP)     !# Amended (LINE 1348 RCT)
     &            *DTRANS*DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K) !# LINE 1349 RCT
            ELSEIF(ireact.eq.2) THEN                              !# LINE 1350 RCT
              DCRCT=-RC1(J,I,K,ICOMP)*CTMP*DTRANS                 !# LINE 1351 RCT
     &               *DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)     !# LINE 1352 RCT
     &              /(RC3(J,I,K,ICOMP)+CTMP)                      !# LINE 1353 RCT
            ELSEIF(ireact.eq.3) THEN                              !# LINE 1354 RCT
              DCRCT=-RC1(J,I,K,ICOMP)*CTMP !CNEW(J,I,K,ICOMP)     !# LINE 1355 RCT
     &            *DTRANS*DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)
              IF(ICOMP.GT.1) THEN !.and.                          !# LINE 1357 RCT
!     &         cnew(j,i,k,icomp-1).lt.rctms(icomp-1,2)) THEN     !# LINE 1358 RCT
                DCRCTX=YLD(ICOMP-1)*RC1(J,I,K,ICOMP-1)            !# LINE 1359 RCT
     &           *CNEW(J,I,K,ICOMP-1)*DTRANS                      !# LINE 1360 RCT
     &           *DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)         !# LINE 1361 RCT
              ELSE                                                !# LINE 1362 RCT
                DCRCTX=0.                                         !# LINE 1363 RCT
              ENDIF                                               !# LINE 1364 RCT
            ELSEIF(ireact.eq.100) THEN
              DCRCT=-RC1(J,I,K,ICOMP)
     &            *DTRANS*DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)
            ENDIF
C--SORBED PHASE
            DCRCT2=0.
            IF(ISOTHM.GT.0.and.ireact.eq.1) THEN
              DCRCT2=-RC2(J,I,K,ICOMP)*RHOB(J,I,K)
     &            *SRCONC(J,I,K,ICOMP)*DTRANS
     &            *DELR(J)*DELC(I)*DH(J,I,K)
            ELSEIF(ISOTHM.GT.0.and.ireact.eq.3) THEN              !# LINE 1375 RCT
              DCRCT2=-RC2(J,I,K,ICOMP)*RHOB(J,I,K)                !# LINE 1376 RCT
     &            *SRCONC(J,I,K,ICOMP)*DTRANS                     !# LINE 1377 RCT
     &            *DELR(J)*DELC(I)*DH(J,I,K)                      !# LINE 1378 RCT
              IF(ICOMP.GT.1) THEN                                 !# LINE 1379 RCT
                DCRCTX2=                                          !# LINE 1380 RCT
     &           YLD(ICOMP-1)*RC2(J,I,K,ICOMP-1)*RHOB(J,I,K)      !# LINE 1381 RCT
     &           *SRCONC(J,I,K,ICOMP-1)*DTRANS                    !# LINE 1382 RCT
     &           *DELR(J)*DELC(I)*DH(J,I,K)                       !# LINE 1383 RCT
              ELSE                                                !# LINE 1384 RCT
                DCRCTX2=0.                                        !# LINE 1385 RCT
              ENDIF                                               !# LINE 1386 RCT
            ELSEIF(ISOTHM.GT.0.and.ireact.eq.100) THEN
              DCRCT2=-RC2(J,I,K,ICOMP)*RHOB(J,I,K)
     &            *DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF            
C
C--CALCULATE MASS LOSS/GAIN DUE TO 1st/0th ORDER REACTION
            IF(DCRCT+DCRCT2.LT.0) THEN
              RMASIO(9,2,ICOMP)=RMASIO(9,2,ICOMP)+DCRCT+DCRCT2
            ELSE
              RMASIO(9,1,ICOMP)=RMASIO(9,1,ICOMP)+DCRCT+DCRCT2
            ENDIF
C
C--CALCULATE MASS LOSS/GAIN DUE TO PARENT'S DECAY                  !# LINE 1399 RCT
            IF(ireact.eq.3.AND.ICOMP.GT.1) THEN                    !# LINE 1400 RCT
              IF(DCRCTX+DCRCTX2.LT.0) THEN                         !# LINE 1401 RCT
                RMASIO(9,2,ICOMP)=RMASIO(9,2,ICOMP)+DCRCTX+DCRCTX2 !# LINE 1402 RCT
              ELSE                                                 !# LINE 1403 RCT
                RMASIO(9,1,ICOMP)=RMASIO(9,1,ICOMP)+DCRCTX+DCRCTX2 !# LINE 1404 RCT
              ENDIF                                                !# LINE 1405 RCT
            ENDIF                                                  !# LINE 1406 RCT
C                                                                  !# LINE 1407 RCT
C--UPDATE SORBED MASS STORAGE CHANGE FOR NONEQUILIBRIUM SORPTION
            IF(ISOTHM.EQ.4.AND.DCRCT2.GT.0) THEN
              RMASIO(120,2,ICOMP)=RMASIO(120,2,ICOMP)-DCRCT2
            ELSEIF(ISOTHM.EQ.4.AND.DCRCT2.LT.0) THEN
              RMASIO(120,1,ICOMP)=RMASIO(120,1,ICOMP)-DCRCT2
            ENDIF
C                                                                  !# LINE 1414 RCT
C--UPDATE SORBED MASS STORAGE CHANGE FOR NONEQUILIBRIUM SORPTION - PARENT'S DECAY !# LINE 1415 RCT
            IF(IREACT.EQ.3.AND.ICOMP.GT.1) THEN                    !# LINE 1416 RCT
              IF(ISOTHM.EQ.4.AND.DCRCTX2.GT.0) THEN                !# LINE 1417 RCT
                RMASIO(120,2,ICOMP)=RMASIO(120,2,ICOMP)-DCRCTX2    !# LINE 1418 RCT
              ELSEIF(ISOTHM.EQ.4.AND.DCRCT2.LT.0) THEN             !# LINE 1419 RCT
                RMASIO(120,1,ICOMP)=RMASIO(120,1,ICOMP)-DCRCTX2    !# LINE 1420 RCT
              ENDIF                                                !# LINE 1421 RCT
            ENDIF                                                  !# LINE 1422 RCT
          ENDDO
        ENDDO
      ENDDO
C
C--1ST/0TH ORDER REACTION IN DUAL-DOMAIN MODEL
 1000 IF(ISOTHM.NE.5.AND.ISOTHM.NE.6) GOTO 9999
C
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
C
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
C
C--DETERMINE WHICH CONC TO USE IF IREACTION=1                      !# LINE 1437 RCT
            IF(IREACTION.EQ.1) THEN                                !# LINE 1438 RCT
              IF(ICOMP.EQ.IED) THEN                                !# LINE 1439 RCT
                CTMP=CRCT(J,I,K,1)                                 !# LINE 1440 RCT
              ELSEIF(ICOMP.EQ.IEA) THEN                            !# LINE 1441 RCT
                CTMP=CRCT(J,I,K,2)                                 !# LINE 1442 RCT
              ELSE                                                 !# LINE 1443 RCT
                CTMP=CNEW(J,I,K,ICOMP)                             !# LINE 1444 RCT
              ENDIF                                                !# LINE 1445 RCT
            ELSE                                                   !# LINE 1446 RCT
              CTMP=CNEW(J,I,K,ICOMP)                               !# LINE 1447 RCT
            ENDIF                                                  !# LINE 1448 RCT
C                                                                  !# LINE 1449 RCT
C--SKIP IF CONCENTRATION IS NOT POSITIVE
            IF(CTMP.LE.0) CYCLE                                    !# Amended (LINE 1451 RCT)
C
C--compute mass loss/gain in each cell for all 4 phases: 
C--mobile liquid, mobile sorbed, immobile liquid, immobile sorbed
            VOLUME=DELR(J)*DELC(I)*DH(J,I,K)
            CMML=CTMP*PRSITY(J,I,K)*VOLUME !CNEW(J,I,K,ICOMP)*PRSITY(J,I,K)*VOLUME !# Amended (LINE 1456 RCT)
            CMMS=(RETA(J,I,K,ICOMP)-1.)*CMML
            CIML=PRSITY2(J,I,K)*SRCONC(J,I,K,ICOMP)*VOLUME
            CIMS=(RETA2(J,I,K,ICOMP)-1.)*CIML            
C--for 1st-order reaction            
            if(ireact.eq.1) then
              CMML=-RC1(J,I,K,ICOMP)*CMML*DTRANS
              CMMS=-RC2(J,I,K,ICOMP)*CMMS*DTRANS
              CIML=-RC1(J,I,K,ICOMP)*CIML*DTRANS
              CIMS=-RC2(J,I,K,ICOMP)*CIMS*DTRANS
C--for zero-order reaction
            elseif(ireact.eq.100) then              
              CMML=-RC1(J,I,K,ICOMP)*VOLUME*PRSITY(J,I,K)*DTRANS
              CMMS=-RC2(J,I,K,ICOMP)*VOLUME*RHOB(J,I,K)*DTRANS
     &             *FRAC(J,I,K)
              CIML=-RC1(J,I,K,ICOMP)*VOLUME*PRSITY2(J,I,K)*DTRANS
              CIMS=-RC2(J,I,K,ICOMP)*VOLUME*RHOB(J,I,K)*DTRANS
     &             *(1.-FRAC(J,I,K))
            endif            
C
C--CALCULATE MASS LOSS/GAIN DUE TO REACTION IN MOBILE DOMAIN
            IF(CMML+CMMS.LT.0) THEN
              RMASIO(9,2,ICOMP)=RMASIO(9,2,ICOMP)+CMML+CMMS
            ELSE
              RMASIO(9,1,ICOMP)=RMASIO(9,1,ICOMP)+CMML+CMMS
            ENDIF
C
C--CALCULATE MASS LOSS/GAIN DUE TO REACTION IN IMMOBILE DOMAIN
            IF(CIML+CIMS.LT.0) THEN
              RMASIO(10,2,ICOMP)=RMASIO(10,2,ICOMP)+CIML+CIMS
            ELSE
              RMASIO(10,1,ICOMP)=RMASIO(10,1,ICOMP)+CIML+CIMS
            ENDIF
C
C--RECORD MASS STORAGE CHANGE IN IMMOBILE DOMAIN
            IF(CIML.GT.0) THEN
              RMASIO(121,2,ICOMP)=RMASIO(121,2,ICOMP)-CIML
            ELSE
              RMASIO(121,1,ICOMP)=RMASIO(121,1,ICOMP)-CIML
            ENDIF
            IF(CIMS.GT.0) THEN
              RMASIO(122,2,ICOMP)=RMASIO(122,2,ICOMP)-CIMS
            ELSE
              RMASIO(122,1,ICOMP)=RMASIO(122,1,ICOMP)-CIMS
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
 9999 CONTINUE
C
C--REACTION: ED + FEAED*EA --> PRODUCT                             !# LINE 1507 RCT
      IF(IREACTION.EQ.1) THEN                                      !# LINE 1508 RCT
      IF(ICOMP.EQ.IED.OR.ICOMP.EQ.IEA) THEN                        !# LINE 1509 RCT
        DO K=1,NLAY                                                !# LINE 1510 RCT
          DO I=1,NROW                                              !# LINE 1511 RCT
            DO J=1,NCOL                                            !# LINE 1512 RCT
C              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                      !# LINE 1513 RCT
C                                                                  !# LINE 1514 RCT
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL                 !# LINE 1515 RCT
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE                     !# LINE 1516 RCT
C                                                                  !# LINE 1517 RCT
              IF(ICOMP.EQ.IED) THEN                                !# LINE 1518 RCT
                RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)-             !# LINE 1520 RCT
     &              (CRCT(J,I,K,1)-CNEW(J,I,K,IED))                !# LINE 1521 RCT
     &              *PRSITY(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)       !# LINE 1522 RCT
CVSB                IF(CNEW(J,I,K,IED).GE.CNEW(J,I,K,IEA)/FEDEA) THEN !# LINE 1524 RCT
CVSB                  RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)-       !# LINE 1525 RCT
CVSB     &              (CNEW(J,I,K,IEA)/FEDEA)                    !# LINE 1526 RCT
CVSB     &              *PRSITY(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)   !# LINE 1527 RCT
CVSB                ELSE                                           !# LINE 1528 RCT
CVSB                  RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)-       !# LINE 1529 RCT
CVSB     &              (CNEW(J,I,K,IED))                          !# LINE 1530 RCT
CVSB     &              *PRSITY(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)   !# LINE 1531 RCT
CVSB                ENDIF                                          !# LINE 1532 RCT
              ELSE                                                 !# LINE 1533 RCT
                RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)-             !# LINE 1535 RCT
     &              (CRCT(J,I,K,2)-CNEW(J,I,K,IEA))                !# LINE 1536 RCT
     &              *PRSITY(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)       !# LINE 1537 RCT
CVSB                IF(CNEW(J,I,K,IEA).GE.CNEW(J,I,K,IED)*FEDEA) THEN !# LINE 1539 RCT
CVSB                  RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)-       !# LINE 1540 RCT
CVSB     &              (CNEW(J,I,K,IED)*FEDEA)                    !# LINE 1541 RCT
CVSB     &              *PRSITY(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)   !# LINE 1542 RCT
CVSB                ELSE                                           !# LINE 1543 RCT
CVSB                  RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)-       !# LINE 1544 RCT
CVSB     &              (CNEW(J,I,K,IEA))                          !# LINE 1545 RCT
CVSB     &              *PRSITY(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)   !# LINE 1546 RCT
CVSB                ENDIF                                          !# LINE 1547 RCT
              ENDIF                                                !# LINE 1548 RCT
            ENDDO                                                  !# LINE 1549 RCT
          ENDDO                                                    !# LINE 1550 RCT
        ENDDO                                                      !# LINE 1551 RCT
      ENDIF                                                        !# LINE 1552 RCT
      ELSEIF(IREACTION.EQ.2) THEN                                  !# LINE 1553 RCT
      DO K=1,NLAY                                                  !# LINE 1554 RCT
        DO I=1,NROW                                                !# LINE 1555 RCT
          DO J=1,NCOL                                              !# LINE 1556 RCT
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                         !# LINE 1557 RCT
            IF(ICBUND(J,I,K,ICOMP).LE.0.AND.COLD(J,I,K,ICOMP)<=0) CYCLE !# LINE 1558 RCT
            IF(ICOMP<=NED) THEN                                    !# LINE 1559 RCT
              DCRCT=0.0                                            !# LINE 1560 RCT
              IF(COLD(J,I,K,ICOMP)>0.) THEN                        !# LINE 1561 RCT
                DCRCT=DCDT_S(N,ICOMP)*COLD(J,I,K,ICOMP)*DELR(J)    !# LINE 1562 RCT
     &          *DELC(I)*DH(j,i,k)*PRSITY(j,i,k)*DTRANS            !# LINE 1563 RCT
    ! &          *COLD(J,I,K,ICOMP)/                               !# LINE 1564 RCT
    ! &          ABS(CNEW(J,I,K,ICOMP)-COLD(J,I,K,ICOMP))          !# LINE 1565 RCT
              ELSEIF(COLD(J,I,K,ICOMP)<=0.) THEN                    !# LINE 1566 RCT
                !COLD(J,I,K,ICOMP)=0.0                             !# LINE 1567 RCT
                DCRCT=0.0                                          !# LINE 1568 RCT
!                DCRCT=DCDT_S(N,ICOMP)*COLD(J,I,K,ICOMP)*DELR(J)   !# LINE 1569 RCT
!     &          *DELC(I)*DH(j,i,k)*Prsity(j,i,k)*DTRANS           !# LINE 1570 RCT
              ELSE                                                 !# LINE 1571 RCT
                DCRCT=DCDT_S(N,ICOMP)*COLD(J,I,K,ICOMP)*DELR(J)    !# LINE 1572 RCT
     &          *DELC(I)*DH(J,I,K)*PRSITY(J,I,K)*DTRANS            !# LINE 1573 RCT
              ENDIF                                                !# LINE 1574 RCT
            ELSEIF(ICOMP>NED.AND.ICOMP<=NED+NEA)THEN               !# LINE 1575 RCT
              DCRCT=0.0                                            !# LINE 1576 RCT
              DO M=1,NED                                           !# LINE 1577 RCT
                IF(COLD(J,I,K,M)>0.)THEN                            !# LINE 1578 RCT
                DCRCT=DCRCT+DCDT_FE(N,ICOMP-NED,M)*COLD(J,I,K,M)   !# LINE 1579 RCT
     &           *DELR(J)*DELC(I)*PRSITY(J,I,K)*DH(J,I,K)          !# LINE 1580 RCT
     &          *DTRANS !*COLD(J,I,K,M)/                           !# LINE 1581 RCT
    !&          ABS(CNEW(J,I,K,M)-COLD(J,I,K,M))                   !# LINE 1582 RCT
                ELSEIF(COLD(J,I,K,M)<=0.)THEN                       !# LINE 1583 RCT
                DCRCT=DCRCT+0.0                                    !# LINE 1584 RCT
!                COLD(J,I,K,M)=0.0                                 !# LINE 1585 RCT
!                DCRCT=DCRCT +DCDT_FE(N,ICOMP-NED,M)*COLD(J,I,K,M) !# LINE 1586 RCT
!     &           *DELR(J)*DELC(I)*PRSITY(J,I,K)*DH(J,I,K)         !# LINE 1587 RCT
!     &          *DTRANS                                           !# LINE 1588 RCT
                ELSE                                               !# LINE 1589 RCT
                 DCRCT=DCRCT+DCDT_FE(N,ICOMP-NED,M)*COLD(J,I,K,M)  !# LINE 1590 RCT
     &           *DELR(J)*DELC(I)*PRSITY(J,I,K)*DH(J,I,K)          !# LINE 1591 RCT
     &          *DTRANS                                            !# LINE 1592 RCT
                ENDIF                                              !# LINE 1593 RCT
              ENDDO                                                !# LINE 1594 RCT
            ELSEIF(ICOMP==NCOMP.and.IFESLD>0)THEN                  !# LINE 1595 RCT

              IF(K.EQ.3) THEN
              IF(I.GE.123.AND.I.LE.124)THEN
              IF(J.EQ.70)THEN
              CONTINUE
              ENDIF
              ENDIF
              ENDIF

              DCRCT=0.0                                            !# LINE 1596 RCT
              DO M=1,NED                                           !# LINE 1597 RCT
                IF(COLD(J,I,K,M)>0)THEN                            !# LINE 1598 RCT
                DCRCT=DCRCT-DCDT_FE(N,NSOLID-NED,M)*COLD(J,I,K,M)           !# LINE 1599 RCT
     &           *DELR(J)*DELC(I)*PRSITY(J,I,K)*DH(J,I,K)*DTRANS   !# LINE 1600 RCT
!     &          *COLD(J,I,K,M)/                                   !# LINE 1601 RCT
!     &          ABS(CNEW(J,I,K,M)-COLD(J,I,K,M))                  !# LINE 1602 RCT
                ELSEIF(COLD(J,I,K,M)<=0)THEN                       !# LINE 1603 RCT
                DCRCT=DCRCT+0.0                                    !# LINE 1604 RCT
                !COLD(J,I,K,M)=0.0                                 !# LINE 1605 RCT
!                DCRCT=DCRCT-DCDT_FE(N,3,M)*COLD(J,I,K,M)          !# LINE 1606 RCT
!     &           *DELR(J)*DELC(I)*PRSITY(J,I,K)*DH(J,I,K)*DTRANS  !# LINE 1607 RCT
                ELSE                                               !# LINE 1608 RCT
                DCRCT=DCRCT-DCDT_FE(N,NSOLID-NED,M)*COLD(J,I,K,M)           !# LINE 1609 RCT
     &           *DELR(J)*DELC(I)*PRSITY(J,I,K)*DH(J,I,K)*DTRANS   !# LINE 1610 RCT
                ENDIF                                              !# LINE 1611 RCT
              ENDDO                                                !# LINE 1612 RCT
            ENDIF                                                  !# LINE 1613 RCT
              IF(K.EQ.3) THEN
              IF(I.EQ.123)THEN
              IF(J.EQ.70)THEN
              CONTINUE
              ENDIF
              ENDIF
              ENDIF
            !IF(ICOMP==9.AND.DCRCT>0.)PAUSE                        !# LINE 1614 RCT
            IF(DCRCT<0)THEN                                        !# LINE 1615 RCT
              RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)+DCRCT          !# LINE 1616 RCT
            ELSE                                                   !# LINE 1617 RCT
              RMASIO(13,1,ICOMP)=RMASIO(13,1,ICOMP)+DCRCT          !# LINE 1618 RCT
            ENDIF                                                  !# LINE 1619 RCT
          ENDDO                                                    !# LINE 1620 RCT
        ENDDO                                                      !# LINE 1621 RCT
      ENDDO                                                        !# LINE 1622 RCT
      ENDIF                                                        !# LINE 1623 RCT
C                                                                  !# LINE 1624 RCT
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE RCT5CF(ICOMP,DTRANS)
C ********************************************************************
C THIS SUBROUTINE UPDATES NONLINEAR REACTION COEFFICIENTS.
C ********************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,PRSITY,
     &                         RETA,RFMIN,RHOB,SP1,SP2,RC1,RC2,PRSITY2,
     &                         RETA2,FRAC,SRCONC,ISOTHM,IREACT,
     &                         CNEW                                 !edm
C While COLD is being passed that is not correct.  I think there was a 
C translation error when Chris passed along the modules version of the 
C MT3DMS code.  In the original code Zheng passes CNEW but picks it up 
C as COLD and so this won't translate well in the modules version.
C
      IMPLICIT  NONE
      INTEGER   ICOMP
      REAL      DTRANS
C
      IF(ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
        CALL SRCT5R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY,
     &   CNEW(:,:,:,ICOMP),RETA(:,:,:,ICOMP),RFMIN,RHOB,
     &   SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),
     &   RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,
     &   SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)       !# Amended (LINE 1655 RCT)
      ENDIF
C
C--RETURN
      RETURN
      END
C
C     !# EVERYTHING FROM HERE DOWN IS VIVEK'S CODE AND IS NOT TAGGED ON A LINE-BY-LINE BASIS
      SUBROUTINE FLASHREACT(ICOMP)
C ********************************************************************
C THIS SUBROUTINE CALCULATES FLASH CONCENTRATIONS AFTER APPLYING 
C REACTION: ED + FEAED*EA --> PRODUCT
C ********************************************************************
      USE MT3DMS_MODULE, ONLY: NCOMP,NLAY,NROW,NCOL,ICBUND,CNEW          !# NEW
      USE RCTMOD
      IMPLICIT NONE
      INTEGER ICOMP,K,I,J,N                                              !# Modified
      REAL CED,CEA  !,CNEW                                               !# CNEW REMOVED
CEDM  DIMENSION CNEW(NCOL,NROW,NLAY,NCOMP),ICBUND(NCOL,NROW,NLAY,NCOMP)  !# REMOVED
C
C      DO ICOMP=1,NCOMP
C      IF(ICOMP.EQ.IED.OR.ICOMP.EQ.IEA) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
C              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
C
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
              IF(ICBUND(J,I,K,1).LE.0) CYCLE
C
C              IF(ICOMP.EQ.IED) THEN
                IF(CNEW(J,I,K,IED).GE.CNEW(J,I,K,IEA)/FEDEA) THEN
                  CED=CNEW(J,I,K,IED)-CNEW(J,I,K,IEA)/FEDEA
                ELSE
                  CED=0.0
                ENDIF
C              ELSE
                IF(CNEW(J,I,K,IEA).GE.CNEW(J,I,K,IED)*FEDEA) THEN
                  CEA=CNEW(J,I,K,IEA)-CNEW(J,I,K,IED)*FEDEA
                ELSE
                  CEA=0.0
                ENDIF
C              ENDIF
              CRCT(J,I,K,1)=CNEW(J,I,K,IED)
              CRCT(J,I,K,2)=CNEW(J,I,K,IEA)
              CNEW(J,I,K,IED)=CED
              CNEW(J,I,K,IEA)=CEA
            ENDDO
          ENDDO
        ENDDO
C      ENDIF
C      ENDDO
C
      RETURN
      END
C
      SUBROUTINE REACTION_PRE(NODES)                               !# Modified
C ********************************************************************
C
C THIS SUBROUTINE READS THE REACTION FILE rec_FileName
C
C ********************************************************************
      USE MT3DMS_MODULE, ONLY: IOUT,MCOMP,NCOMP,NCOL,NROW,NLAY,SAVUCN           !# New
      USE RCTMOD
      CHARACTER*100 LINE
      INTEGER LLOC,ITYP1,ITYP2,ISTART,ISTOP
      INTEGER NODES                                                !# Modified
      REAL R
      LOGICAL OPND
      CHARACTER FINDEX*30                                      !# LINE 274 RCT
C
C-----OPEN FILE ON AN UNUSED UNIT NUMBER
      INUNIT=111
5     INQUIRE(UNIT=INUNIT,OPENED=OPND)
      IF(OPND) THEN
        INUNIT=INUNIT+1
        GOTO 5
      ENDIF
      OPEN(INUNIT,FILE=rec_FileName)
      WRITE(IOUT,'(/1X,4A,I5)') 'KINETIC REACTION PARAMETERS READ',
     1    ' FROM FILE: ',TRIM(rec_FileName),' ON UNIT NUMBER',INUNIT
C
      NSPECIAL=0
      NSTORE=0
      NSOLID=0
C
C-----READ INPUT FILE rec_FileName - IGNORE BLANK LINES AND PRINT COMMENT LINES
10    LINE=' '
      READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GOTO 10
      IF(LINE(1:1).EQ.'#') THEN
        WRITE(IOUT,'(A)') TRIM(LINE)
        GOTO 10
      ENDIF
C
      READ(LINE,*) NED,NEA,NSPECIAL,IFESLD
      WRITE(IOUT,100) NED,NEA,NSPECIAL
100   FORMAT(/1X,'NUMBER OF ELECTRON DONORS    = ',I3,
     1       /1X,'NUMBER OF ELECTRON ACCEPTERS = ',I3,
     1       /1X,'NUMBER OF SPECIAL COMPONENTS = ',I3)
C
      WRITE(IOUT,*)
      WRITE(IOUT,'(2(A,I2))') 'ELECTRON DONORS:    SPECIES ',1,' - ',NED
      WRITE(IOUT,'(2(A,I2))') 'ELECTRON ACCEPTERS: SPECIES ',NED+1,
     1' - ',NEA+NED
C
      ALLOCATE(RCOLD(NCOMP),RCNEW(NCOMP),SPECIAL(NED+NEA))
      ALLOCATE(MAXEC(NED+NEA),SWITCH(NED+NEA),INHIB(NED+NEA),
     1  DECAY(NED,NEA))
      ALLOCATE(YIELDC(NED,NED+NEA),DEA_ED_DT(NED),DCDT(NED+NEA))
C
      ALLOCATE(MASS_NEG(NCOMP),CON_NEG(NCOMP))
      MASS_NEG=0.0
      CON_NEG=0.0
C
      WRITE(IOUT,110)
110   FORMAT(/1x,'Species no.',5x,'Case code' , 5x, 'Max EFC',
     1       /1x,'-----------',5x,'---------' , 5x, '-------')
      DO I=1,NSPECIAL
        READ(INUNIT,'(A)') LINE
        READ(LINE,*) N
        READ(LINE,*) IDUM,SPECIAL(N),MAXEC(N)
        WRITE(IOUT,120) IDUM,SPECIAL(N),MAXEC(N)
        IF(SPECIAL(N).EQ.'STORE') NSTORE=N
        IF(SPECIAL(N).EQ.'SOLID') NSOLID=N
C
C.......'SOLID' ONLY APPLICABLE FOR EAs
        IF(SPECIAL(N).EQ.'SOLID') THEN
          IF(N.LE.NED) THEN
            WRITE(IOUT,*) 'INVALID SPECIES NO./KEYWORD (SOLID)'
            WRITE(IOUT,*) 'KEYWORD SOLID ONLY APPLICABLE WITH EAs'
            WRITE(*,*) 'INVALID SPECIES NO./KEYWORD (SOLID)'
            WRITE(*,*) 'KEYWORD SOLID ONLY APPLICABLE WITH EAs'
            READ(*,*)
            STOP
          ENDIF
        ENDIF
      ENDDO
120   FORMAT(I9,9X,A,3X,1PG15.5)
C
      IF(IFESLD.EQ.1) THEN
        IF(NCOMP.EQ.MCOMP) THEN
          IFESLD=0
          WRITE(IOUT,'(/,2A)') 'SET NCOMP>MCOMP TO SIMULATE IMMOBILE',
     1    ' SOLID-PHASE SPECIES, IFESLD RESET TO 0'
        ELSEIF(NSOLID.EQ.0) THEN
          IFESLD=0
          WRITE(IOUT,'(/,2A)') 'NO SPECIES SET TO ''SOLID''',
     1    ', IFESLD RESET TO 0'
        ELSE
          WRITE(IOUT,1040) NSOLID,NCOMP
        ENDIF
      ENDIF
1040  FORMAT(/1X,'SOLID PHASE FOR SPECIES ',I3,
     1  ' IS SIMULATED AS IMMOBILE SPECIES ',I3)
C
      WRITE(IOUT,'(/,2A)') 'EA#  SPECIES#  HALF SATURATION CONSTANT',
     1  ' INHIBITION CONSTANT'
      WRITE(IOUT,'(2A)') '---  --------  ------------------------',
     1  ' -------------------'
      DO I=1,NEA
        READ(INUNIT,*) SWITCH(I),INHIB(I)
        WRITE(IOUT,130) I,NED+I,SWITCH(I),INHIB(I)
      ENDDO
130   FORMAT(I3,5X,I3,7X,1PG15.5,5X,1PG15.5)
C
      WRITE(IOUT,'(/,A)') 'EA#  SPECIES#   DECAY RATES ED(1:NED)'
      WRITE(IOUT,'(A)') '---  --------   -------------'
      DO I=1,NEA
        READ(INUNIT,*) (DECAY(J,I),J=1,NED)
        WRITE(IOUT,140) I,NED+I,(DECAY(J,I),J=1,NED)
      ENDDO
140   FORMAT(I3,5X,I3,100(3X,1PG15.5))
C
      WRITE(IOUT,'(/,2A)') 'EA/ED  SPECIES#   YIELD COEFFICIENTS ',
     1'ED(1:NED)'
      WRITE(IOUT,'(A)') '-----  --------   ------------------'
      DO I=1,NED+NEA
        READ(INUNIT,*) (YIELDC(J,I),J=1,NED)
        IF(I.LE.NED) THEN
          WRITE(IOUT,150) I,I,(YIELDC(J,I),J=1,NED)
        ELSE
          WRITE(IOUT,151) I-NED,I,(YIELDC(J,I),J=1,NED)
        ENDIF
      ENDDO
150   FORMAT('ED',I3,5X,I3,100(5X,1PG15.5))
151   FORMAT('EA',I3,5X,I3,100(5X,1PG15.5))
      CLOSE(INUNIT)
C
      IF(NSTORE.GT.0) THEN
        ALLOCATE (MASSSTOR(NCOL,NROW,NLAY))
        MASSSTOR=0.
        IF(SAVUCN) THEN
          Ad_methane_name ='MT3D_Ad_methane.UCN'
          IUMETH=INUNIT
          FINDEX=' '
          CALL OPENFL(-IUMETH,0,Ad_methane_name,1,FINDEX)
          WRITE(IOUT,160)
160   FORMAT(1X,'SAVE THE ADDITIONAL METHANE MASS ',
     & 'IN UNFORMATTED FILE [MT3D_Ad_methane.UCN]')
          ENDIF
        ENDIF
C
      ALLOCATE(DCDT_FE(NODES,NEA,NED),DCDT_S(NODES,NEA+NED))
C
C--ADD CODE FOR SOME BASIC QA
C      IF() THEN
C      ENDIF
C
      RETURN
C
1000  CONTINUE
      WRITE(*,*) TRIM(rec_FileName),' FILE IS EMPTY'
      WRITE(IOUT,*) TRIM(rec_FileName),' FILE IS EMPTY'
      READ(*,*)
      STOP
C
      RETURN
      END
C
C
	subroutine reaction_sub(icomp, cflag)
C
	! Implements general form of Lu et al (1999) Eq. 19-24
	use RCTMOD
	implicit none
	integer        ::  m,n,k, icomp
	integer        ::  cflag
C
      m=icomp
      if (m.le.NED) then
	    do n=1,NEA
		  ! first term: reaction rate times effective EA availability
	      if(special(n+ned).eq.'MAXEC')then

	            rval = decay(m,n) * ((maxEC(n+ned)-rcold(n+ned)) /
     &	                 (switch(n) + (maxEC(n+ned)-rcold(n+ned))))
            elseif(special(n+ned).eq.'SOLID')then                                   !--JZ for solid phase iron
                if(maxEC(n+ned)<=0.)cycle
                rval = decay(m,n) * maxEC(n+ned) /(switch(n) + 
     &           maxEC(n+ned))
            elseif(special(n+ned).eq.'STORE')then
		        rval = decay(m,n) * (rcold(m) / (switch(n) + rcold(m)))  !For methane
            else
		       rval = decay(m,n) * (rcold(n+ned) / (switch(n) + rcold(n+ned)))  !for other EAs
		    end if

		  ! second term: inhibition by higher-sequence EAs
	      if(n.gt.1)then
	        do k=1,n-1
	          if(special(k+ned).eq.'MAXEC')then
	            rval = rval * inhib(k) / (inhib(k) +
     &            (maxEC(k+ned)-rcold(k+ned)))
                elseif(special(k+ned).eq.'SOLID')then
                  if(maxEC(k+ned)<=0.)cycle                                         !--JZ for solid phase iron
	            rval = rval * inhib(k) / (inhib(k) +maxEC(k+ned))
                else
			      rval = rval * inhib(k) / (inhib(k) + rcold(k+ned))
			    end if
	        end do

	      end if
		  ! update electron donor(s)
            if (cflag==0)then
                dcdt(m)   = dcdt(m) + rval *RCOLD(m)                    !Call by the standalone module 
            elseif(cflag==1)then
                dcdt(m)   = dcdt(m) + rval                              !call by MT3DMS
            endif
	    end do
          ! yield from a higher ED (added by MTONKIN in V12)
          if(m.gt.1.and.m.le.ned) then
            do k=1,m-1
              dcdt(m)=dcdt(m)+(yieldc(k,m))*dcdt(k)
            end do
          end if
        end if

	  ! ** use the average of (COLD+CNEW) to calculate consumption? **

	  if(m.gt.NED) then
		do n=1,NED
		  ! first term: reaction rate times effective EA availability
	      if(special(m).eq.'MAXEC')then
       	        rval = decay(n,m-ned) * ((maxEC(m)-rcold(m)) /
     & 	         (switch(m-ned) + (maxEC(m)-rcold(m))))       !for iron
            elseif(special(m).eq.'SOLID')then
              if(maxEC(m)<=0.)cycle
       	      rval = decay(n,m-ned) * maxEC(m) /(switch(m-ned) + 
     &  	      maxEC(m)) !for iron 
       	    elseif(special(m).eq.'STORE')then
		       rval = decay(n,m-ned) * (rcold(n) / (switch(m-ned) + rcold(n))) !for methane
	    elseif(RCOLD(m)>0.0)then
		       rval = decay(n,m-ned) * (rcold(m) / (switch(m-ned) + rcold(m))) !for other EAs
		    else 
		       rval=0.0    
		    end if
		  ! second term: inhibition by higher-sequence EAs
	      if(m.gt.ned+1)then
	        do k=1,m-ned-1
	          if(special(k+ned).eq.'MAXEC')then
	              rval = rval * inhib(k) / (inhib(k) +
     &	              (maxEC(k+ned)-rcold(k+ned)))
                elseif(special(k+ned).eq.'SOLID')then
                    if(maxEC(k+ned)<=0.)cycle
	              rval = rval * inhib(k) / (inhib(k) +maxEC(k+ned))
                else
		        rval = rval * inhib(k) / (inhib(k) + rcold(k+ned))
		        end if
	        end do
	      end if
	      ! update electron acceptor(s)
            if(cflag==0)then
                dcdt(m) = dcdt(m) + rval*yieldc(n,m) *RCOLD(n)       !Call by the standalone module
            elseif(cflag==1)then
                dea_Ed_DT(n)=rval*yieldc(n,m)
                dcdt(m) = dcdt(m) + rval*yieldc(n,m)                 !call by mt3dms,DONT MULTIPLY BY RCOLD WHEN CALLED BY MT3D AS THIS IS DONE WITHIN MT3D
            endif  
          end do                                                     !USE yieldc(n,m-ned) WHEN COMPILING WITH MT3D, and yieldc(n,m) WHEN COMPILING ALONE
	  end if

	!enddo ! ** This mimics the MT3D main NCOMP loop **

	return
	end subroutine reaction_sub
C
C
      subroutine Stor_Add_Methane(cMethane, ICOMP, DTRANS) 
!this subroutine is to check the concentration of methane, if it is over the maximum
!EFC, then the additional mass of methane will be stored into a array, and the 
!concentration of methane will be assigned to equal the maximum EFC. 
      use RCTMOD
      USE MT3DMS_MODULE, ONLY: DELR,DELC,PRSITY,DH,NROW,NLAY,NCOL
C
      REAL    cMethane(NCOL,NROW,NLAY)
      REAL    DTRANS
      INTEGER I,J,K  
C
      ! in addition the one in here, a time factor has to be timed in order to get the accumulated methane mass 
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF((cMethane(J,I,K)- MaxEC(ICOMP))>0.)then
                     MassStor(J,I,K)=MassStor(J,I,K)+(cMethane(J,I,K)
     &               -MaxEC(ICOMP))*PRSITY(J,I,K)*DELR(K)  
     &                  *DELC(I)*DH(J,I,K)*DTRANS*0.5*DTRANS 
                    cMethane(J,I,K)=MaxEC(ICOMP)                        
            ENDIF                                                   
          ENDDO                                                       
        ENDDO
      ENDDO      
C
      end subroutine
C
C
      SUBROUTINE KINETIC_SOLID(ICOMP,DTRANS)
      USE RCTMOD
      USE MT3DMS_MODULE, ONLY: DELR,DELC,PRSITY,DH,NROW,NLAY,NCOL,CNEW,
     1  ICBUND,COLD,RHOB
      INTEGER ICOMP
      REAL DTRANS
C
      DO IEDEA=1,NED+NEA                               !# Modified
        IF(SPECIAL(IEDEA)=="SOLID") THEN               !# Modified
          DO K=1,NLAY                                  !# Modified
            DO I=1,NROW                                !# Modified
              DO J=1,NCOL                              !# Modified
                NN=(K-1)*NCOL*NROW+(I-1)*NCOL+J        !# Modified
                IF(ICBUND(J,I,K,1).LE.0) CYCLE         !# Modified
                MAXEC(IEDEA)=COLD(J,I,K,ICOMP)*        !# Modified
     &                        RHOB(J,I,K)/PRSITY(J,I,K)            !# Modified
                DO II=1,NED                            !# Modified
                  MAXEC(IEDEA)=MAXEC(IEDEA)-           !# Modified
     &                          DCDT_FE(NN,IEDEA-NED,II)*          !# Modified
     &                          DTRANS*COLD(J,I,K,II)              !# Modified
                ENDDO                                  !# Modified
                CNEW(J,I,K,ICOMP)=MAXEC(IEDEA)*        !# Modified
     &                          PRSITY(J,I,K)/RHOB(J,I,K)          !# Modified
c                IF(CNEW(J,I,K,ICOMP).LT.0.)            !# Modified
c     &                          CNEW(J,I,K,ICOMP)=0.0              !# Modified
              ENDDO                                    !# Modified
            ENDDO                                      !# Modified
          ENDDO                                        !# Modified
        ENDIF                                          !# Modified
      ENDDO                                            !# Modified
C
      RETURN
      END
C
C
      SUBROUTINE DTS(ICOMP)
      USE RCTMOD
      USE MT3DMS_MODULE, ONLY: DELR,DELC,PRSITY,DH,NROW,NLAY,NCOL,CNEW,
     1  ICBUND,COLD,RHOB
      INTEGER ICOMP
      REAL DTRANS
C
C--THIS SUBROUTINE CALCULATES A STABLE TIME-STEP SIZE
C
      DO K=1,NLAY                              !# Modified
        DO I=1,NROW                            !# Modified
          DO J=1,NCOL                          !# Modified
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE !# Modified
            IF(CNEW(J,I,K,ICOMP).GT.MAXEC(ICOMP)) THEN !# Modified
              CNEW=COLD                        !# Modified
                            !N=N-1                             !# Added by Vivek? (See email 2-22-13)
                            !CYCLE TRANSPORT TIME-STEP LOOP    !# Added by Vivek? (See email 2-22-13)
CEDM                  DO NN=1,NODES                            !# LINE 647 MAIN
CEDM                    IF(IX(LCIB+NN-1)<=0)CYCLE              !# LINE 648 MAIN
CEDM                    IF(X(LCCNEW+(ICOMP-1)*NODES+NN-1).GT.  !# LINE 649 MAIN
CEDM 1                     MAXEC(ICOMP))THEN                   !# LINE 650 MAIN
                          !concentration is over the maximum EFC !# LINE 651 MAIN
CVSB                          OperFlag=2                       !# LINE 652 MAIN 
                          !time1=time1-DTRANS                  !# LINE 653 MAIN
CVSB                          IF(PRTOUT)NPS=NPS-1              !# LINE 654 MAIN
CVSB                          TIME2=TIME1                      !# LINE 655 MAIN
                          !2-DTRANS                            !# LINE 656 MAIN
CVSB                          CALL TimeStep_adjust(OperFlag,   !# LINE 657 MAIN
CVSB     &                    X(LCCOLD+(ICOMP-1)*NODES+NN-1),  !# LINE 658 MAIN
CVSB     &                    X(LCCNEW+(ICOMP-1)*NODES+NN-1),MAXEC(ICOMP),  !# LINE 659 MAIN
CVSB     &                    DTRANS,ICOMP)                    !# LINE 660 MAIN
CVSB                          IF(DTRANS<DT0)THEN               !# LINE 661 MAIN
CVSB                            DT0=DTRANS                     !# LINE 662 MAIN
CVSB                          ENDIF                            !# LINE 663 MAIN
CEDM                      X(LCCNEW:LCCNEW+NCOMP*NODES-1)=      !# LINE 664 MAIN
CEDM &                    X(LCCOLD:LCCOLD+NCOMP*NODES-1)       !# LINE 665 MAIN
CEDM                      DO III=0,NCOMP*NODES-1               !# LINE 666 MAIN
CEDM                        X(LCCNEW+III)=X(LCCOLD+III)        !# LINE 667 MAIN
CEDM                      ENDDO                                !# LINE 668 MAIN
CVSB                          N=N-1                            !# LINE 669 MAIN
CVSB                          cycle mstrans_loop               !# LINE 670 MAIN
CEDM                    ELSEIF(X(LCCNEW+(ICOMP-1)*NODES+NN-1)- !# LINE 671 MAIN
CEDM &                        MAXEC(ICOMP)<1.E-6)THEN          !# LINE 672 MAIN
CVSB                          ! restore the default time step for mass transport  !# LINE 673 MAIN
CVSB                          IF(DT0<DT00)DT0=DT00             !# LINE 674 MAIN
CEDM                    ENDIF                                  !# LINE 675 MAIN
CEDM                  ENDDO                                    !# LINE 676 MAIN
CEDM                ENDIF                                      !# LINE 677 MAIN
CEDM              ENDIF                                        !# LINE 678 MAIN
            ENDIF
          ENDDO
        ENDDO  
      ENDDO
      RETURN
      END
