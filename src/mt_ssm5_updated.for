C
      SUBROUTINE SSM5AR(IN)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED IN THE SINK & SOURCE
C MIXING (SSM) PACKAGE.
C **********************************************************************
C last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: INSSM,IOUT,NCOL,NROW,NLAY,NCOMP,
     &                         FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,
     &                         FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,FSWT,
     &                         FSFR,FUZF,IVER,
     &                         ISSGOUT,MXSS,NSS,NTSS,RECH,IRCH,CRCH,
     &                         EVTR,IEVT,CEVT,SS,SSMC,SSG,
     &                         CUZINF,UZET,CUZET,GWET,CGWET,IETFLG, !edm
     &                         FINFIL,UZFLX,UZQSTO,SURFLK,CSURFLK,  !edm
     &                         IETFLG,iUnitTRNOP,IUZFOPT,IUZFBND,   !edm
     &                         IUZFOPTG,                            !edm
     &                         KSSZERO                              !# LINE 14 SSM
      USE SFRVARS,       ONLY: NSTRM,NINTOT,MXSGMT,MXRCH,NSFINIT,   !# NEW
     &                         ISFSOLV,WIMP,WUPS,CCLOSESF,MXITERSF,      !# NEW
     &                         CRNTSF,NOBSSF,NJASF,INFLWNOD         !# NEW
C
      IMPLICIT  NONE
      INTEGER   ISTART, ISTOP, LLOC,I,J                             !edm
      INTEGER   IERR,IN,
     &          NUZTOP,IRUNFLG                                      !edm
      REAL      R                                                   !edm
      CHARACTER VERSION*11,LINE*72,BNAME*24
      CHARACTER(LEN=*),PARAMETER :: FMT2="(3I8,I7)"                 !edm
      LOGICAL   IUZFBND_CHK                                         !edm
C
      INSSM=IN
C
C--ALLOCATE
      ALLOCATE(ISSGOUT,MXSS,NSS,NTSS,IETFLG,IUZFOPTG)               !edm
      IETFLG=.FALSE.                                                !# NEW
      IF(FSFR) THEN                                                 !# NEW
        ALLOCATE(NSTRM,NINTOT,MXSGMT,MXRCH)                         !# NEW
        ALLOCATE(ISFSOLV,WIMP,WUPS,CCLOSESF,MXITERSF,CRNTSF)             !# NEW
        ALLOCATE(NOBSSF,NJASF)                                      !# NEW
      ENDIF                                                         !# NEW
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1000) INSSM
 1000 FORMAT(1X,'SSM5 -- SINK & SOURCE MIXING PACKAGE,',
     & ' VERSION 5, FEBRUARY 2010, INPUT READ FROM UNIT',I3)
C
C--READ AND PRINT FLAGS INDICATING WHICH SINK/SOURCE OPTIONS
C--ARE USED IN FLOW MODEL
      IF(IVER.EQ.1) THEN
        READ(INSSM,'(6L2)') FWEL,FDRN,FRCH,FEVT,FRIV,FGHB
      ELSEIF(IVER.EQ.2) THEN
        READ(INSSM,'(A)') LINE
        WRITE(IOUT,1010) LINE
      ENDIF
      WRITE(IOUT,1020)
      IF(FWEL) WRITE(IOUT,1340)
      IF(FDRN) WRITE(IOUT,1342)
      IF(FRCH) WRITE(IOUT,1344)
      IF(FEVT) WRITE(IOUT,1346)
      IF(FRIV) WRITE(IOUT,1348)
      IF(FGHB) WRITE(IOUT,1350)
      IF(FSTR) WRITE(IOUT,1400)
      IF(FRES) WRITE(IOUT,1402)
      IF(FFHB) WRITE(IOUT,1404)
      IF(FIBS) WRITE(IOUT,1406)
      IF(FTLK) WRITE(IOUT,1408)
      IF(FLAK) WRITE(IOUT,1410)
      IF(FMNW) WRITE(IOUT,1412)
      IF(FDRT) WRITE(IOUT,1414)
      IF(FETS) WRITE(IOUT,1416)
      IF(FSWT) WRITE(IOUT,1418)
      IF(FSFR) WRITE(IOUT,1420)
      IF(FUZF) WRITE(IOUT,1422)
 1010 FORMAT(1X,'HEADER LINE OF THE SSM PACKAGE INPUT FILE:',/1X,A)
 1020 FORMAT(1X,'MAJOR STRESS COMPONENTS PRESENT IN THE FLOW MODEL:')
 1340 FORMAT(1X,' o WELL [WEL]')
 1342 FORMAT(1X,' o DRAIN [DRN]')
 1344 FORMAT(1X,' o RECHARGE [RCH]')
 1346 FORMAT(1X,' o EVAPOTRANSPIRATION [EVT]')
 1348 FORMAT(1X,' o RIVER [RIV]')
 1350 FORMAT(1X,' o GENERAL-HEAD-DEPENDENT BOUNDARY [GHB]')
 1400 FORMAT(1X,' o STREAM [STR]')
 1402 FORMAT(1X,' o RESERVOIR [RES]')
 1404 FORMAT(1X,' o SPECIFIED-HEAD-FLOW BOUNDARY [FHB]')
 1406 FORMAT(1X,' o INTERBED STORAGE [IBS]')
 1408 FORMAT(1X,' o TRANSIENT LEAKAGE [TLK]')
 1410 FORMAT(1X,' o LAKE [LAK]')
 1412 FORMAT(1X,' o MULTI-NODE WELL [MNW]')
 1414 FORMAT(1X,' o DRAIN WITH RETURN FLOW [DRT]')
 1416 FORMAT(1X,' o SEGMENTED EVAPOTRANSPIRATION [ETS]')
 1418 FORMAT(1X,' o SUBSIDENCE-WATER TABLE [SWT]')
 1420 FORMAT(1X,' o STREAMFLOW-ROUTING [SFR]')
 1422 FORMAT(1X,' o UNSATURATED-ZONE FLOW [UZF]')
C
C--READ AND PRINT MAXIMUM NUMBER OF
C--POINT SINKS/SOURCES PRESENT IN THE FLOW MODEL
      ISSGOUT=0
      READ(INSSM,'(2I10)',ERR=1,IOSTAT=IERR) MXSS,ISSGOUT
    1 IF(IERR.NE.0) THEN
        BACKSPACE (INSSM)
        READ(INSSM,'(I10)') MXSS
      ENDIF
      WRITE(IOUT,1580) MXSS
 1580 FORMAT(1X,'MAXIMUM NUMBER OF POINT SINKS/SOURCES =',I8)
      IF(ISSGOUT.GT.0) THEN
        WRITE(IOUT,1582) ISSGOUT
 1582   FORMAT(1X,'AVERAGE CONCENTRATIONS FOR LINKED GROUP',
     &   ' SINKS/SOURCES SAVED In UNIT:',I3)
      ENDIF
C
C--ALLOCATE SPACE FOR ARRAYS
      IF(FRCH) THEN
        ALLOCATE(RECH(NCOL,NROW))
        ALLOCATE(IRCH(NCOL,NROW))
        ALLOCATE(CRCH(NCOL,NROW,NCOMP))
      ELSE
        ALLOCATE(RECH(1,1))
        ALLOCATE(IRCH(1,1))
        ALLOCATE(CRCH(1,1,1))
      ENDIF
      IF(FEVT.OR.FETS) THEN
        ALLOCATE(EVTR(NCOL,NROW))
        ALLOCATE(IEVT(NCOL,NROW))
        ALLOCATE(CEVT(NCOL,NROW,NCOMP))
      ELSE
        ALLOCATE(EVTR(1,1))
        ALLOCATE(IEVT(1,1))
        ALLOCATE(CEVT(1,1,1))
      ENDIF
      IF(FUZF) THEN                                                 !edm
        ALLOCATE(IUZFOPT(NCOL,NROW))                                !edm
        ALLOCATE(IUZFBND(NCOL,NROW))                                !edm
        ALLOCATE(UZFLX(NCOL,NROW,NLAY))                             !edm
        ALLOCATE(UZQSTO(NCOL,NROW,NLAY))                            !edm
        ALLOCATE(SURFLK(NCOL,NROW,NLAY))                            !edm
        ALLOCATE(CSURFLK(NCOL,NROW,NLAY,NCOMP))                     !edm
        ALLOCATE(FINFIL(NCOL,NROW))                                 !edm
        ALLOCATE(CUZINF(NCOL,NROW,NCOMP))                           !edm
        ALLOCATE(UZET(NCOL,NROW,NLAY))                              !edm
        ALLOCATE(CUZET(NCOL,NROW,NLAY,NCOMP))                       !edm
        ALLOCATE(GWET(NCOL,NROW,NLAY))                              !edm
        ALLOCATE(CGWET(NCOL,NROW,NLAY,NCOMP))                       !edm
      ELSE                                                          !edm
        ALLOCATE(IUZFOPT(1,1))                                      !edm
        ALLOCATE(IUZFBND(NCOL,NROW))                                !edm
        ALLOCATE(UZFLX(1,1,1))                                      !edm
        ALLOCATE(UZQSTO(1,1,1))                                     !edm
        ALLOCATE(SURFLK(1,1,1))                                     !edm
        ALLOCATE(CSURFLK(1,1,1,1))                                  !edm
        ALLOCATE(FINFIL(1,1))                                       !edm
        ALLOCATE(CUZINF(1,1,1))                                     !edm
        ALLOCATE(UZET(1,1,1))                                       !edm
        ALLOCATE(CUZET(1,1,1,1))                                    !edm
        ALLOCATE(GWET(1,1,1))                                       !edm
        ALLOCATE(CGWET(1,1,1,1))                                    !edm
      ENDIF                                                         !edm
      ALLOCATE(SS(8,MXSS))                                          !# Amended (LINE 154 SSM)
      ALLOCATE(SSMC(NCOMP,MXSS))
      ALLOCATE(SSG(5,MXSS))
      ALLOCATE(KSSZERO(MXSS))                                       !# LINE 96 SSM
      KSSZERO=0                                                     !# LINE 97 SSM
      RECH=0.
      IRCH=0
      CRCH=0.
      EVTR=0.
      IEVT=0
      CEVT=0.
      SS=0.
      SSMC=0.
      SSG=0.
C
C--INITIALIZE IUZFBND ARRAY
      DO I=1,NROW
        DO J=1,NCOL
          IUZFBND(J,I)=0
        ENDDO
      ENDDO
C                                                                   !edm
C--DETERMINE IF UZF PACKAGE IS SIMULATING ET                        !edm
C--This approach starts at the beginning of the UZF header line and !edm
C--goes one-by-one                                                  !edm
      IF(FUZF) THEN                                                 !edm
    9   READ(iUnitTRNOP(7),'(A)',ERR=100,IOSTAT=IERR) LINE          !edm
        IF(LINE(1:1).EQ.'#' .OR. LINE(1:1).EQ.'S') THEN !READ NEXT  !edm
          GOTO 9                                                    !edm
        ELSE                                                        !edm
          lloc = 1                                                  !edm
          !Reads NUZTOP
          CALL URWORD(line, LLOC, ISTART, ISTOP, 2, NUZTOP, r, IOUT, In)
          !Reads IUZFOPT
          CALL URWORD(line, LLOC, ISTART, ISTOP, 2, IUZFOPTG,r,IOUT,In)
          !IRUNFLG
          CALL URWORD(line, LLOC, ISTART, ISTOP, 2, IRUNFLG, r, IOUT,In)
C--The next line is really IETFLG, but to get URWORD to work,
C--needed to use a dummy integer variable only to convert to 
C--'logical' in the next step.
          CALL URWORD(line,LLOC,ISTART,ISTOP,2,IRUNFLG,r,IOUT,In)   !edm
          IF(IRUNFLG.EQ.0) THEN                                     !edm
            IETFLG=.FALSE.                                          !edm
          ELSE                                                      !edm
            IETFLG=.TRUE.                                           !edm
          ENDIF                                                     !edm
C--Read the IUZFBND array                                           !edm
          BNAME=' AREAL EXTENT OF UZ FLOW'                          !edm
          CALL U2DINT(IUZFBND,BNAME,NROW,NCOL,0,iUnitTRNOP(7),IOUT) !edm
        ENDIF                                                       !edm
C--Adjust IUZFOPTG (IUZFOPT-Global) according to what is stored in  !edm
C--the IUZFBND array                                                !edm
        IF(IUZFOPTG.GT.0) THEN                                      !edm
          IUZFBND_CHK = .FALSE.                                     !edm
          DO I=1,NROW                                               !edm
            DO J=1,NCOL                                             !edm
              IF(.NOT.IUZFBND(J,I).LE.0) THEN                       !edm
                IUZFBND_CHK = .TRUE.                                !edm
              ENDIF                                                 !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
C--If the check on IUZFBND called IUZFBND_CHK remains false,        !edm
C--then the value of IUZFOPT can be set equal to zero, since all    !edm
C--cells are effectively acting as though IUZFOPT=0                 !edm
          IF(.NOT.IUZFBND_CHK) THEN                                 !edm
            IUZFOPTG = 0                                            !edm
          ENDIF                                                     !edm
        ELSE                                                        !edm
          IETFLG=.FALSE.                                            !edm
        ENDIF                                                       !edm
      ENDIF
C
C--NORMAL RETURN
  100 RETURN
      END
C
      SUBROUTINE SSM5RP(KPER)
C ********************************************************************
C THIS SUBROUTINE READS CONCENTRATIONS OF SOURCES OR SINKS NEEDED BY
C THE SINK AND SOURCE MIXING (SSM) PACKAGE.
C ********************************************************************
C last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: INSSM,IOUT,NCOL,NROW,NLAY,NCOMP,ICBUND,
     &                         CNEW,
     &                         FWEL,FDRN,FRIV,FGHB,FRCH,FEVT,FSTR,FRES,
     &                         FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,FSWT,
     &                         FSFR,FUZF,
     &                         CRCH,CEVT,MXSS,NSS,SS,SSMC,
     &                         CUZINF,CUZET,CGWET,CSURFLK,IETFLG,   !edm
     &                         KSSZERO                              !# LINE 146 SSM
C
      IMPLICIT  NONE
      INTEGER   IN,KPER,JJ,II,KK,NUM,IQ,INCRCH,INCEVT,NTMP,INDEX,
     &          INCUZINF,INCUZET,INCGWET,INCSRFLK                   !edm
      REAL      CSS
      CHARACTER ANAME*24,TYPESS(-1:100)*15
C
      IN=INSSM
C
C--INITIALIZE.
      TYPESS(-1)='CONSTANT CONC. '
      TYPESS(1) ='CONSTANT HEAD  '
      TYPESS(2) ='WELL           '
      TYPESS(3) ='DRAIN          '
      TYPESS(4) ='RIVER          '
      TYPESS(5) ='HEAD DEP BOUND '
      TYPESS(15)='MASS LOADING   '
      TYPESS(21)='STREAM         '
      TYPESS(22)='RESERVOIR      '
      TYPESS(23)='SP FLW HD BOUND'
      TYPESS(24)='INTERBED STRG  '
      TYPESS(25)='TRANSIENT LEAK '
      TYPESS(26)='LAKE           '
      TYPESS(27)='MULTI-NODE WELL'
      TYPESS(28)='DRN W RET FLOW '
      TYPESS(29)='SEGMENTED ET   '
      TYPESS(50)='HSS MAS LOADING'
      TYPESS(51)='SUBSIDENCE-WT  '
      TYPESS(52)='STREAM FL ROUT.'
      TYPESS(53)='UNSAT ZONE FLOW'
C
C--READ CONCENTRATION OF DIFFUSIVE SOURCES/SINKS (RECHARGE/E.T.)
C--FOR CURRENT STRESS PERIOD IF THEY ARE SIMULATED IN FLOW MODEL
      IF(.NOT.FRCH) GOTO 10
C
C--READ FLAG INCRCH INDICATING HOW TO READ RECHARGE CONCENTRATION
      READ(IN,'(I10)') INCRCH
C
C--IF INCRCH < 0, CONCENTRATIN REUSED FROM LAST STRESS PERIOD
      IF(INCRCH.LT.0) THEN
        WRITE(IOUT,1)
        GOTO 10
      ENDIF
    1 FORMAT(/1X,'CONCENTRATION OF RECHARGE FLUXES',
     & ' REUSED FROM LAST STRESS PERIOD')
C
C--IF INCRCH >= 0, READ AN ARRAY
C--CONTAING CONCENTRATION OF RECHARGE FLUX [CRCH]
      WRITE(IOUT,2) KPER
      ANAME='RECH. CONC. COMP. NO.'
      DO INDEX=1,NCOMP
        WRITE(ANAME(19:21),'(I3.2)') INDEX
        CALL RARRAY(CRCH(1:NCOL,1:NROW,INDEX),ANAME,NROW,NCOL,0,IN,IOUT)
      ENDDO
    2 FORMAT(/1X,'CONCENTRATION OF RECHARGE FLUXES',
     & ' WILL BE READ IN STRESS PERIOD',I3)
C--READ CONCENTATION OF INFILTRATING FLUX (CUZINF)                  !edm
   10 IF(.NOT.FUZF) GOTO 11                                         !edm
C--READ FLAG INDICATING HOW TO READ APPLD AMT CONC.                 !edm
      READ(IN,'(I10)') INCUZINF                                     !edm
C                                                                   !edm
C--IF INCUZINF<0, CONC.REUSED FROM LAST STRESS PERIOD               !edm
      IF(INCUZINF.LT.0) THEN                                        !edm
        WRITE(IOUT,3)                                               !edm
        GOTO 11                                                     !edm
      ENDIF                                                         !edm
    3 FORMAT(/1X,'CONCENTRATION OF APPLIED WATER',                  !edm
     & ' REUSED FROM LAST STRESS PERIOD')                           !edm
C                                                                   !edm
C--IF INCUINF>=0, READ AN ARRAY CONTAINING CONC. OF APPL.           !edm
C--WATER [CIUZNF]                                                   !edm
      WRITE(IOUT,4) KPER                                            !edm
      ANAME='APPLD CONC. COMP.NO.'                                  !edm
      DO INDEX=1,NCOMP                                              !edm
        WRITE(ANAME(19:21),'(I3.2)') INDEX                          !edm
        CALL RARRAY(CUZINF(:,:,INDEX),ANAME,NROW,NCOL,              !edm
     &              0,IN,IOUT)                                      !edm
      ENDDO                                                         !edm
    4 FORMAT(/1X,'CONCENTRATION OF APPLIED WATER',                  !edm
     & ' WILL BE READ IN STRESS PERIOD',I3)                         !edm
C
C--READ CONCENTRAION OF EVAPOTRANSPIRATION FLUX
   11 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 14
C
      IF(KPER.EQ.1) THEN            
        DO INDEX=1,NCOMP
          DO II=1,NROW
            DO JJ=1,NCOL
              CEVT(JJ,II,INDEX)=-1.E-30
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      READ(IN,'(I10)') INCEVT
      IF(INCEVT.LT.0) THEN
        WRITE(IOUT,12)
        GOTO 20
      ENDIF
   12 FORMAT(/1X,'CONCENTRATION OF E. T. FLUXES',
     & ' REUSED FROM LAST STRESS PERIOD')
C
      WRITE(IOUT,13) KPER
      ANAME='E. T. CONC. COMP. NO.'
      DO INDEX=1,NCOMP
        WRITE(ANAME(19:21),'(I3.2)') INDEX
        CALL RARRAY(CEVT(1:NCOL,1:NROW,INDEX),ANAME,NROW,NCOL,0,IN,IOUT)
      ENDDO
   13 FORMAT(/1X,'CONCENTRATION OF E. T. FLUXES',
     & ' WILL BE READ IN STRESS PERIOD',I3)
C
C--READ CONCENTRATION OF ET FLUX WHEN UZF ET IS BEING               !edm
C--SIMULATED.                                                       !edm
C--What the CUZET should be set equal to is still unknown           !edm
C--Maybe something slightly higher than zero because the            !edm
C--the crops will remove some small amount of N or P, which         !edm
C--would act to lower the concentration in the upper unsat          !edm
C--zone.  However, the CGWET will most likely be equal to           !edm
C--the aquifer concentration when dealing with the upflux           !edm
C--of salts. Bottom line: CUZET & CGWET should be kept              !edm
C--separate and specifiable for maximum code flexibility.           !edm
   14 IF(.NOT.IETFLG) GOTO 17                                       !edm
C                                                                   !edm
      IF(KPER.EQ.1) THEN                                            !edm
        DO INDEX=1,NCOMP                                            !edm
          DO KK=1,NLAY                                              !edm
            DO II=1,NROW                                            !edm
              DO JJ=1,NCOL                                          !edm
                CUZET(JJ,II,KK,INDEX)=-1.E-30                       !edm
              ENDDO                                                 !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDIF                                                         !edm
      READ(IN,'(I10)') INCUZET                                      !edm
      IF(INCUZET.LT.0) THEN                                         !edm
        WRITE(IOUT,15)                                              !edm
        GOTO 17                                                     !edm
      ENDIF                                                         !edm
   15 FORMAT(/1X,'CONCENTRATION OF UZET FLUXES',                    !edm
     & ' REUSED FROM LAST STRESS PERIOD')                           !edm
C                                                                   !edm
      WRITE(IOUT,16) KPER                                           !edm
      ANAME='UZET. CONC. COMP. NO.'                                 !edm
      DO INDEX=1,NCOMP                                              !edm
        WRITE(ANAME(19:21),'(I3.2)') INDEX                          !edm
C--BECAUSE UZET CAN BE WITHDRAWN FROM MULTIPLE LAYERS,              !edm
C--AN 'RARRAY_UZ' FUNCTION MAY NEED TO BE ADDED.  BUT FOR NOW I'LL  !edm
C--ASSUME THAT CONSTANT VALUES WILL BE UTILIZED RELIEVING THE NEED  !edm
C--TO ADDRESS THIS RIGHT NOW.                                       !edm
        CALL RARRAY(CUZET(:,:,:,INDEX),ANAME,NROW,NCOL,             !edm
     &              NLAY,IN,IOUT)                                   !edm
C--AFTER READING IN TOP LAYERS CUZET, COPY IT TO THE REMAINING      !edm
C--LAYERS                                                           !edm
        DO KK=2,NLAY                                                !edm
          DO II=1,NROW                                              !edm
            DO JJ=1,NCOL                                            !edm
              CUZET(JJ,II,KK,INDEX)=CUZET(JJ,II,1,INDEX)            !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
   16 FORMAT(/1X,'CONCENTRATION OF UZET FLUXES',                    !edm
     & ' WILL BE READ IN STRESS PERIOD',I3)                         !edm
C                                                                   !edm
C--READ CONCENTRATION OF GWET FLUX WHEN UZF ET IS BEING             !edm
C--SIMULATED.                                                       !edm
   17 IF(.NOT.IETFLG) GOTO 20                                       !edm
C                                                                   !edm
      IF(KPER.EQ.1) THEN                                            !edm
        DO INDEX=1,NCOMP                                            !edm
          DO KK=1,NLAY                                              !edm
            DO II=1,NROW                                            !edm
              DO JJ=1,NCOL                                          !edm
                CGWET(JJ,II,KK,INDEX)=-1.E-30                       !edm
              ENDDO                                                 !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDIF                                                         !edm
      READ(IN,'(I10)') INCGWET                                      !edm
      IF(INCGWET.LT.0) THEN                                         !edm
        WRITE(IOUT,18)                                              !edm
        GOTO 20                                                     !edm
      ENDIF                                                         !edm
   18 FORMAT(/1X,'CONCENTRATION OF GWET FLUXES',                    !edm
     & ' REUSED FROM LAST STRESS PERIOD')                           !edm
C                                                                   !edm
      WRITE(IOUT,19) KPER                                           !edm
      ANAME='GWET. CONC. COMP. NO.'                                 !edm
      DO INDEX=1,NCOMP                                              !edm
        WRITE(ANAME(19:21),'(I3.2)') INDEX                          !edm
        CALL RARRAY(CGWET(:,:,:,INDEX),ANAME,NROW,NCOL,             !edm
     &              NLAY,IN,IOUT)                                   !edm
      ENDDO                                                         !edm
   19 FORMAT(/1X,'CONCENTRATION OF GWET FLUXES',                    !edm
     & ' WILL BE READ IN STRESS PERIOD',I3)                         !edm
C                                                                   !edm
C--READ CONCENTRATION OF SURFACE LEAKANCE FLUX WHEN UZF             !edm
C--PACKAGE IS BEING USED.                                           !edm
   20 IF(.NOT.FUZF) GOTO 25                                         !edm
C                                                                   !edm
      IF(KPER.EQ.1) THEN                                            !edm
        DO INDEX=1,NCOMP                                            !edm
          DO KK=1,NLAY                                              !edm
            DO II=1,NROW                                            !edm
              DO JJ=1,NCOL                                          !edm
                CSURFLK(JJ,II,KK,INDEX)=-1.E-30                     !edm
              ENDDO                                                 !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDIF                                                         !edm
      READ(IN,'(I10)') INCSRFLK                                     !edm
      IF(INCSRFLK.LT.0) THEN                                        !edm
        WRITE(IOUT,21)                                              !edm
        GOTO 25                                                     !edm
      ENDIF                                                         !edm
   21 FORMAT(/1X,'CONCENTRATION OF SURFACE LEAKANCE',               !edm
     & ' REUSED FROM LAST STRESS PERIOD')                           !edm
C                                                                   !edm
      WRITE(IOUT,22) KPER                                           !edm
      ANAME='SF LK CONC. COMP. NO.'                                 !edm
      DO INDEX=1,NCOMP                                              !edm
        WRITE(ANAME(19:21),'(I3.2)') INDEX                          !edm
        CALL RARRAY(CSURFLK(:,:,:,INDEX),ANAME,NROW,NCOL,           !edm
     &              NLAY,IN,IOUT)                                   !edm
      ENDDO                                                         !edm
   22 FORMAT(/1X,'CONCENTRATION OF SURFACE LEAKANCE FLUXES',        !edm
     & ' WILL BE READ IN STRESS PERIOD',I3)                         !edm
   25 CONTINUE
C
C--READ AND ECHO POINT SINKS/SOURCES OF SPECIFIED CONCENTRATIONS
      READ(IN,'(I10)') NTMP
C
C--RESET OLD CONCENTRATIONS IF REUSE OPTION NOT IN EFFECT
      IF(KPER.GT.1.AND.NTMP.GE.0) THEN
        DO NUM=1,NSS
          SS(4,NUM)=0.
          DO INDEX=1,NCOMP
            SSMC(INDEX,NUM)=0.
          ENDDO
          KSSZERO=0                                                 !# LINE 249 SSM
        ENDDO
      ENDIF
C
      IF(NTMP.GT.MXSS) THEN
        WRITE(*,30)
        CALL USTOP(' ')
      ELSEIF(NTMP.LT.0) THEN
        WRITE(IOUT,40)
        RETURN
      ELSEIF(NTMP.EQ.0) THEN
        WRITE(IOUT,50) NTMP,KPER
        NSS=0
        RETURN
      ELSE
        NSS=NTMP
      ENDIF
C
      WRITE(IOUT,60)
      DO NUM=1,NSS
C
        IF(NCOMP.EQ.1) THEN
          READ(IN,'(3I10,F10.0,I10)') KK,II,JJ,CSS,IQ
          SSMC(1,NUM)=CSS
        ELSE
          READ(IN,'(3I10,F10.0,I10)',ADVANCE='NO') KK,II,JJ,CSS,IQ
          READ(IN,*) (SSMC(INDEX,NUM),INDEX=1,NCOMP)
        ENDIF
C
        IF(IQ.EQ.-1) THEN
          IF(KK.EQ.0) THEN                                              !# LINE 279 SSM
            KSSZERO(NUM)=1                                              !# LINE 280 SSM
            IF(.NOT.FRCH) THEN                                          !# LINE 281 SSM
              WRITE(*,*) 'RECHARGE BOUNDARY NEEDED IF K IS SET TO 0'    !# LINE 282 SSM
              WRITE(IOUT,*) 'RECHARGE BOUNDARY NEEDED IF K IS SET TO 0' !# LINE 283 SSM
              CALL USTOP(' ')                                           !# LINE 284 SSM
            ENDIF
          ELSE                                                          !# LINE 286 SSM
            DO INDEX=1,NCOMP                                            !# LINE 287 SSM
              IF(SSMC(INDEX,NUM).GE.0) THEN                             !# LINE 288 SSM
                CNEW(JJ,II,KK,INDEX)=SSMC(INDEX,NUM)                    !# LINE 289 SSM
                ICBUND(JJ,II,KK,INDEX)=-ABS(ICBUND(JJ,II,KK,INDEX))     !# LINE 290 SSM
              ENDIF                                                     !# LINE 291 SSM
            ENDDO                                                       !# LINE 292 SSM
          ENDIF                                                         !# LINE 293 SSM
        ELSEIF(IQ.EQ.15) THEN
          SS(5,NUM)=0.
        ELSEIF(IQ.EQ.2.AND.CSS.LT.0) THEN
          NTMP=-INT(CSS)
          IF(NTMP.LT.1.OR.NTMP.GT.NCOL*NROW*NLAY) THEN
            WRITE(*,79) 
            CALL USTOP(' ')
          ENDIF            
        ELSEIF(IQ.LT.1.OR.IQ.GT.100) THEN
          WRITE(*,80)
          CALL USTOP(' ')          
        ENDIF        
        SS(1,NUM)=KK
        SS(2,NUM)=II
        SS(3,NUM)=JJ
        SS(4,NUM)=CSS
        SS(6,NUM)=IQ
C
        DO INDEX=1,NCOMP
          CSS=SSMC(INDEX,NUM)
          IF(IQ.EQ.-1.AND.KK.EQ.0) THEN                        !# LINE 314 SSM
            WRITE(IOUT,70) NUM,KK,II,JJ,CSS,TYPESS(IQ),INDEX   !# LINE 315 SSM
          ELSE                                                 !# LINE 316 SSM
            IF(CSS.NE.0 .OR. ICBUND(JJ,II,KK,INDEX).LT.0)
     &       WRITE(IOUT,70) NUM,KK,II,JJ,CSS,TYPESS(IQ),INDEX
          ENDIF                                                !# LINE 319 SSM
          IF(CSS.LT.0 .AND. IQ.EQ.2)
     &     WRITE(IOUT,71) -INT(CSS)                
        ENDDO
C
      ENDDO
   30 FORMAT(/1X,'ERROR: MAXIMUM NUMBER OF POINT SINKS/SOURCES',
     & ' EXCEEDED'/1X,'INCREASE [MXSS] IN SSM INPUT FILE')
   40 FORMAT(/1X,'POINT SINKS/SOURCES OF SPECIFIED CONCENTRATION',
     & ' REUSED FROM LAST STRESS PERIOD')
   50 FORMAT(/1X,'NO. OF POINT SINKS/SOURCES OF SPECIFIED',
     & ' CONCONCENTRATIONS =',I5,' IN STRESS PERIOD',I3)
   60 FORMAT(/5X,'  NO    LAYER   ROW   COLUMN   CONCENTRATION',
     & '       TYPE            COMPONENT')
   70 FORMAT(3X,4(I5,3X),1X,G15.7,5X,A15,I6)
   71 FORMAT(8X,'>>RECIRCULATION WELL; INPUT CONCENTRATION',
     & ' FROM NODE #',I10.8)
   79 FORMAT(/1X,'ERROR: INVALID CELL LOCATION FOR RECIRCULATION',
     &       /1X,'       WELL CONCENTRATION IN THE SSM INPUT FILE') 
   80 FORMAT(/1X,'ERROR: INVALID CODE FOR POINT SINK/SOURCE TYPE',
     &       /1X,'       IN THE SSM INPUT FILE')
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE SSM5FM(ICOMP)
C ******************************************************************
C THIS SUBROUTINE FORMULATES MATRIX COEFFICIENTS FOR THE SINK/
C SOURCE TERMS UNDER THE IMPLICIT FINITE-DIFFERENCE SCHEME.
C ******************************************************************
C last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,DELR,
     &                         DELC,DH,IRCH,RECH,CRCH,IEVT,EVTR,CEVT,
     &                         MXSS,NTSS,SS,SSMC,SSG,QSTO,CNEW,ISS,A,
     &                         RHS,NODES,UPDLHS,MIXELM,COLD,
     &                         ISS,FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,
     &                         FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,
     &                         FSWT,FSFR,FUZF,
     &                         FINFIL,IETFLG,UZET,CUZET,GWET,CGWET, !edm
     &                         CUZINF,SATNEW,SURFLK,CSURFLK,IUZFBND,!edm
     &                         RETA,COLD,IALTFM,INCTS,MXWEL,IWCTS,
     &                         CINACT,DELT,DTRANS   !# LINE 348-349 SSM
C
      IMPLICIT  NONE
      INTEGER   ICOMP,NUM,IQ,K,I,J,N,IGROUP,
     &          MHOST,KHOST,IHOST,JHOST
      REAL      CTMP,QSS,QCTMP
C
C--DETERMINE AVERAGE CONCENTRATION FOR LINKED SINK/SOURCE GROUPS
      CALL CGROUP(NCOL,NROW,NLAY,NCOMP,ICOMP,MXSS,NTSS,
     & SS,SSMC,SSG,ICBUND,CNEW,DELR,DELC,DH)
C
C--FORMULATE [A] AND [RHS] MATRICES FOR EULERIAN SCHEMES
      IF(MIXELM.GT.0) GOTO 1000
C
C--TRANSIENT FLUID STORAGE TERM
      IF(ISS.EQ.0) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
                N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
!                A(N)=A(N)+QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)     
!CDL--SEAWAT: This seems to fix the problem with storage
CEDM--HAVE PURPOSELY OMITTED VIVEK'S IALTFM OPTION, THE OLD METHOD  !# LINE 390-396 SSM
CEDM--IS WRONG                                                      !# LINE 390-396 SSM
                IF(.NOT.FUZF .OR. IUZFBND(J,I).LE.0) THEN           !edm
c                  RHS(N)=RHS(N)-COLD(J,I,K,ICOMP)*
c     &                     QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)
C
                IF(IALTFM.EQ.1) THEN
                  !IF(ABS(COLD(J,I,K,ICOMP)-CINACT).GT.1E-3) then
                  !IF(COLD(J,I,K,ICOMP).GT.1.0E-6) then
                  RHS(N)=RHS(N)-QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)
     1            *RETA(J,I,K,ICOMP)*COLD(J,I,K,ICOMP) !*DELT/DTRANS
                  !ENDIF
                ELSE
                A(N)=A(N)+QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)
!     1                 *RETA(N,ICOMP)
                ENDIF
C
                ENDIF                                               !edm

              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--AREAL SINK/SOURCE TERMS 
C--(RECHARGE)
      IF(.NOT.FRCH) GOTO 10
      DO I=1,NROW
        DO J=1,NCOL
          K=IRCH(J,I)
          IF(K.GT.0 .AND. ICBUND(J,I,K,ICOMP).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(RECH(J,I).LT.0) THEN
              IF(UPDLHS) A(N)=A(N)+RECH(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
            ELSE
              RHS(N)=RHS(N)
     &         -RECH(J,I)*CRCH(J,I,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C--(INFILTRATED) -ASSUMES THE INFILTRATION IS OCCURING AT TOP OF    !edm
C                 LAYER 1, BUT UZF ALLOWS FOR INFILTRATION TO BE    !edm
C                 SPECIFIED IN OTHER LAYERS. FOR NOW, CODE IS       !edm
C                 HARDWIRED WITH LAYER 1 AND WILL LIKELY NEED TO    !edm
C                 BE UPDATED BY READING THE IUZFBND ARRAY IN THE    !edm
C                 UZF INPUT FILE TO GLEAN WHICH LAYER INFILTRATE    !edm
C                 (AND SURFACE LEAKAGE) IS OCCURING IN (AND FROM)   !edm
   10 IF(.NOT.FUZF) GOTO 12                                         !edm
      K=1                                                           !edm
      DO I=1,NROW                                                   !edm
        DO J=1,NCOL                                                 !edm
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                         !edm
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                          !edm
C--(SURFACE LEAKANCE)                                               !edm
            IF(SURFLK(J,I,K).LT.0) THEN                             !edm
              IF(UPDLHS) A(N)=A(N)+SURFLK(J,I,K)*                   !edm
     &                      DELR(J)*DELC(I)*DH(J,I,K)               !edm
C--(INFILTRATED)                                                    !edm
            ELSEIF(FINFil(J,I).GT.0) THEN                           !edm
              RHS(N)=RHS(N)-FINFIL(J,I)*CUZINF(J,I,ICOMP)*          !edm
     &                      DELR(J)*DELC(I)*DH(J,I,K)               !edm
            ENDIF                                                   !edm
          ENDIF                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C                                                                   !edm
C--(EVAPOTRANSPIRATION)
   12 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 13
      DO I=1,NROW
        DO J=1,NCOL
          K=IEVT(J,I)
          IF(K.GT.0 .AND. ICBUND(J,I,K,ICOMP).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(EVTR(J,I).LT.0.AND.(CEVT(J,I,ICOMP).LT.0 .OR. 
     &       CEVT(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN          
              IF(UPDLHS) A(N)=A(N)+EVTR(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
            ELSEIF(CEVT(J,I,ICOMP).GT.0) THEN
              RHS(N)=RHS(N)
     &         -EVTR(J,I)*CEVT(J,I,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C                                                                   !edm
C--(UZET)                                                           !edm
   13 IF(.NOT.IETFLG) GOTO 20                                       !edm
      DO K=1,NLAY                                                   !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            IF(UZET(J,I,K).EQ.0) CYCLE                              !edm
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                       !edm
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                        !edm
              IF(UZET(J,I,K).LT.0.AND.(CUZET(J,I,K,ICOMP).LT.0 .OR. !edm
     &         CUZET(J,I,K,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN       !edm
                IF(UPDLHS) A(N)=A(N)+UZET(J,I,K)*                   !edm
     &                     DELR(J)*DELC(I)*DH(J,I,K)                !edm
              ELSEIF(CUZET(J,I,K,ICOMP).GT.0) THEN                  !edm
                RHS(N)=RHS(N)-UZET(J,I,K)*CUZET(J,I,K,ICOMP)*       !edm
     &                     DELR(J)*DELC(I)*DH(J,I,K)                !edm
              ENDIF                                                 !edm
            ENDIF                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C                                                                   !edm
C--(GWET)                                                           !edm
      DO K=1,NLAY                                                   !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            IF(GWET(J,I,K).EQ.0) CYCLE                              !edm
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                       !edm
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                        !edm
              IF(GWET(J,I,K).LT.0.AND.(CGWET(J,I,K,ICOMP).LT.0 .OR. !edm
     &         CGWET(J,I,K,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN       !edm
                IF(UPDLHS) A(N)=A(N)+GWET(J,I,K)*                   !edm
     &              DELR(J)*DELC(I)*DH(J,I,K)                       !edm
              ELSEIF(CGWET(J,I,K,ICOMP).GT.0) THEN                  !edm
                RHS(N)=RHS(N)-GWET(J,I,K)*CGWET(J,I,K,ICOMP)*       !edm
     &              DELR(J)*DELC(I)*DH(J,I,K)                       !edm
              ENDIF                                                 !edm
            ENDIF                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C
C
C--POINT SINK/SOURCE TERMS
   20 DO NUM=1,NTSS
        K=SS(1,NUM)
        I=SS(2,NUM)
        J=SS(3,NUM)
        CTMP=SS(4,NUM)
        IF(NCOMP.GT.1) CTMP=SSMC(ICOMP,NUM)
        QSS=SS(5,NUM)
        IQ=SS(6,NUM)
        IF(ICBUND(J,I,K,ICOMP).LE.0.OR.IQ.LE.0) CYCLE
C
C--SKIP IF THE WELL IS A PART OF TREATMENT SYSTEM              !# LINE 450 SSM
        IF(INCTS.GT.0) THEN                                    !# LINE 451 SSM
          IF(SS(8,NUM).GT.0) THEN                              !# LINE 452 SSM
            CONTINUE                                           !# LINE 453 SSM
CCCCC          IF(IWCTS(SS(8,NUM)).GT.0) CYCLE                 !# LINE 454 SSM
          ENDIF                                                !# LINE 455 SSM
        ENDIF                                                  !# LINE 456 SSM
C                                                              !# LINE 457 SSM
C--RESET QSS FOR MASS-LOADING SOURCES (IQ=15)        
        IF(IQ.EQ.15) THEN
          QSS=1./(DELR(J)*DELC(I)*DH(J,I,K))
C
C--GET AVERAGE CONC FOR LINKED SINK/SOURCE GROUPS (IQ=27)          
        ELSEIF(IQ.EQ.27) THEN
          IGROUP=SS(7,NUM)
          CTMP=SSG(4,IGROUP)
C
C--GET RETURN FLOW CONC FOR DRAINS WITH RETURN FLOW (IQ=28)          
        ELSEIF(IQ.EQ.28 .AND. QSS.GT.0) THEN
          MHOST=SS(7,NUM)
          KHOST=(MHOST-1)/(NCOL*NROW) + 1
          IHOST=MOD((MHOST-1),NCOL*NROW)/NCOL + 1
          JHOST=MOD((MHOST-1),NCOL) + 1
          CTMP=CNEW(JHOST,IHOST,KHOST,ICOMP)
C
C--GET CONCENTRATION FOR RECIRCULATED INJECTION WELL
C--(IF INPUT CONCENTRATION WAS SET TO A NEGATIVE INTEGER)
        ELSEIF(IQ.EQ.2 .AND. CTMP.LT.0 .AND. QSS.GT.0)  THEN
          MHOST=-INT(CTMP)
          KHOST=(MHOST-1)/(NCOL*NROW) + 1
          IHOST=MOD((MHOST-1),NCOL*NROW)/NCOL + 1
          JHOST=MOD((MHOST-1),NCOL) + 1          
          CTMP=CNEW(JHOST,IHOST,KHOST,ICOMP)      
        ENDIF
C
C--ADD CONTRIBUTIONS TO MATRICES [A] AND [RHS]        
        N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
        IF(QSS.LT.0) THEN
          IF(UPDLHS) A(N)=A(N)+QSS*DELR(J)*DELC(I)*DH(J,I,K)
        ELSE
          RHS(N)=RHS(N)-QSS*CTMP*DELR(J)*DELC(I)*DH(J,I,K)
        ENDIF        
      ENDDO
C
C--DONE WITH EULERIAN SCHEMES
      GOTO 2000
C
C--FORMULATE [A] AND [RHS] MATRICES FOR EULERIAN-LAGRANGIAN SCHEMES
 1000 CONTINUE
C
C--AREAL SINK/SOURCE TERMS
C--(RECHARGE)
      IF(.NOT.FRCH) GOTO 30
      DO I=1,NROW
        DO J=1,NCOL
          K=IRCH(J,I)
          IF(K.GT.0 .AND. ICBUND(J,I,K,ICOMP).GT.0
     &              .AND. RECH(J,I).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(UPDLHS) A(N)=A(N)-RECH(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
            RHS(N)=RHS(N)
     &       -RECH(J,I)*CRCH(J,I,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)
          ENDIF
        ENDDO
      ENDDO
C--(INFILTRATED) -WILL NEED TO MODIFIY CODE IF IUZFBND.NE.1         !edm
   30 IF(.NOT.FUZF) GOTO 31                                         !edm
      K=1                                                           !edm
      DO I=1,NROW                                                   !edm
        DO J=1,NCOL                                                 !edm
          IF(K.GT.0) THEN                                           !edm
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                       !edm
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                        !edm
              IF(SURFLK(J,I,K).LT.0) THEN                           !edm
                IF(UPDLHS) A(N)=A(N)+SURFLK(J,I,K)*                 !edm
     &                          DELR(J)*DELC(I)*DH(J,I,K)           !edm
              ELSE                                                  !edm
                RHS(N)=RHS(N)-FINFIL(J,I)*CUZINF(J,I,ICOMP)*        !edm
     &                          DELR(J)*DELC(I)*DH(J,I,K)           !edm
              ENDIF                                                 !edm
            ENDIF                                                   !edm
          ENDIF                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C                                                                   !edm
C--(SURFACE LEAKANCE)                                               !edm
   31 IF(.NOT.FUZF) GOTO 32                                         !edm
      K=1                                                           !edm
      DO I=1,NROW                                                   !edm
        DO J=1,NCOL                                                 !edm
          IF(K.GT.0 .AND. ICBUND(J,I,K,ICOMP).GT.0                  !edm
     &              .AND. SURFLK(J,I,K).GT.0) THEN                  !edm
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                          !edm
            IF(UPDLHS) A(N)=A(N)-SURFLK(J,I,K)*                     !edm
     &                       DELR(J)*DELC(I)*DH(J,I,K)              !edm
            RHS(N)=RHS(N)-SURFLK(J,I,K)*CSURFLK(J,I,K,ICOMP)*       !edm
     &                       DELR(J)*DELC(I)*DH(J,I,K)              !edm
          ENDIF
        ENDDO
      ENDDO
C
C--(EVAPOTRANSPIRATION)
   32 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 40
      DO I=1,NROW
        DO J=1,NCOL
          K=IEVT(J,I)
          IF(K.GT.0 .AND. ICBUND(J,I,K,ICOMP).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(EVTR(J,I).LT.0.AND.(CEVT(J,I,ICOMP).LT.0 .OR. 
     &       CEVT(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN             
              CYCLE
            ELSEIF(CEVT(J,I,ICOMP).GE.0) THEN  
              IF(UPDLHS) A(N)=A(N)-EVTR(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
              RHS(N)=RHS(N)
     &         -EVTR(J,I)*CEVT(J,I,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C--(UZET)                                                           !edm
   40 IF(.NOT.FUZF .AND. .NOT.IETFLG) GOTO 41                       !edm
      DO K=1,NLAY                                                   !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            IF(UZET(J,I,K).EQ.0) CYCLE                              !edm
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                       !edm
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                        !edm
              IF(UZET(J,I,K).LT.0.AND.(CUZET(J,I,K,ICOMP).LT.0 .OR. !edm
     &         CUZET(J,I,K,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN       !edm
                CYCLE                                               !edm
              ELSEIF(CUZET(J,I,K,ICOMP).GE.0) THEN                  !edm
                IF(UPDLHS) A(N)=A(N)-UZET(J,I,K)                    !edm
     &                     *DELR(J)*DELC(I)*DH(J,I,K)               !edm
                RHS(N)=RHS(N)-UZET(J,I,K)*CUZET(J,I,K,ICOMP)        !edm
     &                     *DELR(J)*DELC(I)*DH(J,I,K)               !edm
              ENDIF                                                 !edm
            ENDIF                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C--(GWET)                                                           !edm
      DO K=1,NLAY                                                   !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            IF(GWET(J,I,K).EQ.0) CYCLE                              !edm
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                       !edm
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                        !edm
              IF(GWET(J,I,K).LT.0.AND.(CGWET(J,I,K,ICOMP).LT.0 .OR. !edm
     &         CGWET(J,I,K,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN       !edm
                CYCLE                                               !edm
              ELSEIF(CGWET(J,I,K,ICOMP).GE.0) THEN                  !edm
                IF(UPDLHS) A(N)=A(N)-GWET(J,I,K)                    !edm
     &                   *DELR(J)*DELC(I)*DH(J,I,K)                 !edm
                RHS(N)=RHS(N)-GWET(J,I,K)*CGWET(J,I,K,ICOMP)        !edm
     &                   *DELR(J)*DELC(I)*DH(J,I,K)                 !edm
              ENDIF                                                 !edm
            ENDIF                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C
C--POINT SINK/SOURCE TERMS
   41 DO NUM=1,NTSS
        K=SS(1,NUM)
        I=SS(2,NUM)
        J=SS(3,NUM)
        CTMP=SS(4,NUM)
        IF(NCOMP.GT.1) CTMP=SSMC(ICOMP,NUM)
        QSS=SS(5,NUM)
        IQ=SS(6,NUM)
C
C--SKIP IF NOT ACTIVE CELL      
        IF(ICBUND(J,I,K,ICOMP).LE.0.OR.IQ.LE.0) CYCLE
C
C--SKIP IF SINK CELL
        IF(QSS.LE.0.AND.IQ.NE.15) CYCLE
C
C--COMPUTE PRODUCT OF Q*C
        QCTMP=QSS*CTMP        
C        
C--RESET Q*C FOR MASS-LOADING SOURCES (IQ=15)
        IF(IQ.EQ.15) THEN
          QSS=1./(DELR(J)*DELC(I)*DH(J,I,K))
          QCTMP=QSS*CTMP
          QSS=0.
C        
C--RESET Q*C FOR LINKED SINK/SOURCE GROUPS (IQ=27)
        ELSEIF(IQ.EQ.27) THEN
          IGROUP=SS(7,NUM)
          CTMP=SSG(4,IGROUP)
          QCTMP=QSS*CTMP
C
C--RESET Q*C FOR DRAINS WITH RETURN FLOW (IQ=28)
        ELSEIF(IQ.EQ.28.AND.QSS.GT.0) THEN
          MHOST=SS(7,NUM)
          KHOST=(MHOST-1)/(NCOL*NROW) + 1
          IHOST=MOD((MHOST-1),NCOL*NROW)/NCOL + 1
          JHOST=MOD((MHOST-1),NCOL) + 1
          CTMP=CNEW(JHOST,IHOST,KHOST,ICOMP)
          QCTMP=QSS*CTMP
C
C--GET CONCENTRATION FOR RECIRCULATED INJECTION WELL 
C--(IF INPUT CONCENTRATION WAS SET TO A NEGATIVE INTEGER)
        ELSEIF(IQ.EQ.2 .AND. CTMP.LT.0 .AND. QSS.GT.0) THEN
          MHOST=-INT(CTMP) 
          KHOST=(MHOST-1)/(NCOL*NROW) + 1
          IHOST=MOD((MHOST-1),NCOL*NROW)/NCOL + 1
          JHOST=MOD((MHOST-1),NCOL) + 1
          CTMP=CNEW(JHOST,IHOST,KHOST,ICOMP)   
          QCTMP=QSS*CTMP                                    
        ENDIF
C
C--ADD CONTRIBUTIONS TO MATRICES [A] AND [RHS]
        N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
        IF(UPDLHS) A(N)=A(N)-QSS*DELR(J)*DELC(I)*DH(J,I,K)
        RHS(N)=RHS(N)-QCTMP*DELR(J)*DELC(I)*DH(J,I,K)        
      ENDDO
C
C--DONE WITH EULERIAN-LAGRANGIAN SCHEMES
 2000 CONTINUE
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE SSM5BD(ICOMP,DTRANS)
C ********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGETS ASSOCIATED WITH ALL SINK/
C SOURCE TERMS.
C ********************************************************************
C last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY:NCOL,NROW,NLAY,NCOMP,ICBUND,DELR,DELC,
     &                        DH,IRCH,RECH,CRCH,IEVT,EVTR,CEVT,MXSS,
     &                        NTSS,SS,SSMC,SSG,QSTO,CNEW,RETA,ISS,
     &                        RMASIO,
     &                        FINFIL,CUZINF,UZET,CUZET,GWET,CGWET,  !edm
     &                        IETFLG,SATNEW,SURFLK,CSURFLK,         !edm
     &                        FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,
     &                        FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,FSWT,
     &                        FSFR,FUZF,
     &                        INCTS,MXWEL,IWCTS,COLD,IALTFM,CINACT         !# LINE 607 SSM
C
      IMPLICIT  NONE
      INTEGER   ICOMP,NUM,IQ,K,I,J,IGROUP,MHOST,KHOST,IHOST,JHOST
      REAL      DTRANS,CTMP,QSS
C
C--DETERMINE AVERAGE CONCENTRATION FOR LINKED SINK/SOURCE GROUPS
      CALL CGROUP(NCOL,NROW,NLAY,NCOMP,ICOMP,MXSS,NTSS,
     & SS,SSMC,SSG,ICBUND,CNEW,DELR,DELC,DH)      
C
C--TRANSIENT GROUNDWATER STORAGE TERM
      IF(ISS.NE.0) GOTO 50
C
C--RECORD MASS STORAGE CHANGES FOR DISSOLVED AND SORBED PHASES
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
CEDM--HAVE OMITTED VIVEK'S IALTFM OPTION BECAUSE THE OLD APPROACH
CEDM--WAS WRONG.
            IF(IALTFM.EQ.1) THEN
              CTMP=COLD(J,I,K,ICOMP)
              !IF(ABS(COLD(J,I,K,ICOMP)-CINACT).LE.1.0E-3) then
              !IF(COLD(J,I,K,ICOMP).LE.1.0E-6) then
              !  CTMP=0.
              !endif
              IF(QSTO(J,I,K).GT.0) THEN
                RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)
     &           +QSTO(J,I,K)*CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
     1                 *RETA(J,I,K,ICOMP)
              ELSE
                RMASIO(118,2,ICOMP)=RMASIO(118,2,ICOMP)
     &           +QSTO(J,I,K)*CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
     1                 *RETA(J,I,K,ICOMP)
              ENDIF
            ELSE
              CTMP=CNEW(J,I,K,ICOMP)
              IF(QSTO(J,I,K).GT.0) THEN
                RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)
     &           +QSTO(J,I,K)*CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
              ELSE
                RMASIO(118,2,ICOMP)=RMASIO(118,2,ICOMP)
     &           +QSTO(J,I,K)*CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--AREAL SINK/SOURCE TERMS
C--(RECHARGE)
   50 IF(.NOT.FRCH) GOTO 70
C
      DO I=1,NROW
        DO J=1,NCOL
          K=IRCH(J,I)
          IF(K.EQ.0 .OR. ICBUND(J,I,K,ICOMP).LE.0) CYCLE
          CTMP=CRCH(J,I,ICOMP)
          IF(RECH(J,I).LT.0) CTMP=CNEW(J,I,K,ICOMP)
          IF(RECH(J,I).GT.0) THEN
            RMASIO(7,1,ICOMP)=RMASIO(7,1,ICOMP)+RECH(J,I)*CTMP*DTRANS*
     &       DELR(J)*DELC(I)*DH(J,I,K)
          ELSE
            RMASIO(7,2,ICOMP)=RMASIO(7,2,ICOMP)+RECH(J,I)*CTMP*DTRANS*
     &       DELR(J)*DELC(I)*DH(J,I,K)
          ENDIF
        ENDDO
      ENDDO
C                                                                   !edm
C--(INFILTRATED)                                                    !edm
   70 IF(.NOT.FUZF) GOTO 80                                         !edm
      K=1                                                           !edm
      DO I=1,NROW                                                   !edm
        DO J=1,NCOL                                                 !edm
          IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE                        !edm
          CTMP=CUZINF(J,I,ICOMP)                                    !edm
          IF(FINFIL(J,I).LT.0) CTMP=CNEW(J,I,K,ICOMP)               !edm
          IF(FINFIL(J,I).GT.0) THEN                                 !edm
            RMASIO(13,1,ICOMP)=RMASIO(13,1,ICOMP)+FINFIL(J,I)*      !edm
     &          CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)               !edm
          ELSE                                                      !edm
            RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)+FINFIL(J,I)*      !edm
     &          CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)               !edm
          ENDIF                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C                                                                   !edm
C--(SURFACE LEAKANCE)                                               !edm
   80 IF(.NOT.FUZF) GOTO 100                                        !edm
      K=1                                                           !edm
      DO I=1,NROW                                                   !edm
        DO J=1,NCOL                                                 !edm
          IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE                        !edm
          CTMP=CSURFLK(J,I,K,ICOMP)                                 !edm
          IF(SURFLK(J,I,K).LT.0) CTMP=CNEW(J,I,K,ICOMP)             !edm
          IF(SURFLK(J,I,K).GT.0) THEN                               !edm
            RMASIO(16,1,ICOMP)=RMASIO(16,1,ICOMP)+SURFLK(J,I,K)*    !edm
     &         CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)                !edm
          ELSE                                                      !edm
            RMASIO(16,2,ICOMP)=RMASIO(16,2,ICOMP)+SURFLK(J,I,K)*    !edm
     &         CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)                !edm
          ENDIF                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C
C--(EVAPOTRANSPIRATION)
  100 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 110
C
      DO I=1,NROW
        DO J=1,NCOL
          K=IEVT(J,I)
          IF(K.EQ.0 .OR. ICBUND(J,I,K,ICOMP).LE.0) CYCLE
          CTMP=CEVT(J,I,ICOMP)
          IF(EVTR(J,I).LT.0.AND.(CTMP.LT.0 .or.
     &                           CTMP.GE.CNEW(J,I,K,ICOMP))) THEN
            CTMP=CNEW(J,I,K,ICOMP)
          ELSEIF(CTMP.LT.0) THEN        
            CTMP=0.
          ENDIF
          IF(EVTR(J,I).GT.0) THEN
            RMASIO(8,1,ICOMP)=RMASIO(8,1,ICOMP)+EVTR(J,I)*CTMP*DTRANS*
     &       DELR(J)*DELC(I)*DH(J,I,K)
          ELSE
            RMASIO(8,2,ICOMP)=RMASIO(8,2,ICOMP)+EVTR(J,I)*CTMP*DTRANS*
     &       DELR(J)*DELC(I)*DH(J,I,K)
          ENDIF
        ENDDO
      ENDDO
C                                                                   !edm
C--(UZET)                                                           !edm
  110 IF(.NOT.FUZF .AND. .NOT.IETFLG) GOTO 120                      !edm
      DO K=1,NLAY                                                   !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE                      !edm
            CTMP=CUZET(J,I,K,ICOMP)                                 !edm
            IF(UZET(J,I,K).LT.0.AND.(CTMP.LT.0 .OR.                 !edm
     &                             CTMP.GE.CNEW(J,I,K,ICOMP))) THEN !edm
              CTMP=CNEW(J,I,K,ICOMP)                                !edm
            ELSEIF(CTMP.LT.0) THEN                                  !edm
              CTMP=0.                                               !edm
            ENDIF                                                   !edm
            IF(UZET(J,I,K).GT.0) THEN                               !edm
              RMASIO(14,1,ICOMP)=RMASIO(14,1,ICOMP)+UZET(J,I,K)*    !edm
     &          CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)               !edm
            ELSE                                                    !edm
              RMASIO(14,2,ICOMP)=RMASIO(14,2,ICOMP)+UZET(J,I,K)*    !edm
     &          CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)               !edm
            ENDIF                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C                                                                   !edm
C--(GWET)                                                           !edm
  120 IF(.NOT.FUZF .AND. .NOT.IETFLG) GOTO 200                      !edm
      DO K=1,NLAY                                                   !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE                      !edm
            CTMP=CGWET(J,I,K,ICOMP)                                 !edm
            IF(GWET(J,I,K).LT.0.AND.(CTMP.LT.0 .OR.                 !edm
     &                             CTMP.GE.CNEW(J,I,K,ICOMP))) THEN !edm
              CTMP=CNEW(J,I,K,ICOMP)                                !edm
            ELSEIF(CTMP.LT.0) THEN                                  !edm
              CTMP=0.                                               !edm
            ENDIF                                                   !edm
            IF(GWET(J,I,K).GT.0) THEN                               !edm
              RMASIO(15,1,ICOMP)=RMASIO(15,1,ICOMP)+GWET(J,I,K)*    !edm
     &          CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)               !edm
            ELSE                                                    !edm
              RMASIO(15,2,ICOMP)=RMASIO(15,2,ICOMP)+GWET(J,I,K)*    !edm
     &          CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)               !edm
            ENDIF                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C
C--POINT SINK/SOURCE TERMS
  200 DO NUM=1,NTSS
        K=SS(1,NUM)
        I=SS(2,NUM)
        J=SS(3,NUM)
        QSS=SS(5,NUM)
        IQ=SS(6,NUM)
        CTMP=SS(4,NUM)
        IF(NCOMP.GT.1) CTMP=SSMC(ICOMP,NUM)
C
C--SKIP IF THE WELL IS A PART OF TREATMENT SYSTEM              !# LINE 729 SSM
        IF(INCTS.GT.0) THEN                                    !# LINE 730 SSM
          IF(SS(8,NUM).GT.0) THEN                              !# LINE 731 SSM
            IF(IWCTS(SS(8,NUM)).GT.0) CYCLE                    !# LINE 732 SSM
          ENDIF                                                !# LINE 733 SSM
        ENDIF                                                  !# LINE 734 SSM
C
C--SKIP IF NOT ACTIVE CELL
        IF(ICBUND(J,I,K,ICOMP).LE.0.OR.IQ.LE.0) CYCLE
C
C--RESET QSS FOR MASS-LOADING SOURCES (IQ=15)
        IF(IQ.EQ.15) THEN
          QSS=1./(DELR(J)*DELC(I)*DH(J,I,K))
C
C--GET AVERAGE CONC FOR LINKED SINK/SOURCE GROUPS (IQ=27)          
        ELSEIF(IQ.EQ.27) THEN
          IGROUP=SS(7,NUM)
          CTMP=SSG(4,IGROUP)
C
C--GET RETURN FLOW CONC FOR DRAINS WITH RETURN FLOW (IQ=28)          
        ELSEIF(IQ.EQ.28 .AND. QSS.GT.0) THEN
          MHOST=SS(7,NUM)
          KHOST=(MHOST-1)/(NCOL*NROW) + 1
          IHOST=MOD((MHOST-1),NCOL*NROW)/NCOL + 1
          JHOST=MOD((MHOST-1),NCOL) + 1
          CTMP=CNEW(JHOST,IHOST,KHOST,ICOMP)
C
C--GET CONCENTRATION FOR RECIRCULATED INJECTION WELL
C--(IF INPUT CONCENTRATION WAS SET TO A NEGATIVE INTEGER)
        ELSEIF(IQ.EQ.2 .AND. CTMP.LT.0 .AND. QSS.GT.0) THEN
          MHOST=-INT(CTMP) 
          KHOST=(MHOST-1)/(NCOL*NROW) + 1
          IHOST=MOD((MHOST-1),NCOL*NROW)/NCOL + 1
          JHOST=MOD((MHOST-1),NCOL) + 1
          CTMP=CNEW(JHOST,IHOST,KHOST,ICOMP)
        ENDIF
C        
        IF(QSS.LT.0) CTMP=CNEW(J,I,K,ICOMP)
C
        IF(ICBUND(J,I,K,ICOMP).GT.0.AND.IQ.GT.0) THEN
          IF(QSS.GT.0) THEN
            RMASIO(IQ,1,ICOMP)=RMASIO(IQ,1,ICOMP)+QSS*CTMP*DTRANS*
     &       DELR(J)*DELC(I)*DH(J,I,K)
          ELSE
            RMASIO(IQ,2,ICOMP)=RMASIO(IQ,2,ICOMP)+QSS*CTMP*DTRANS*
     &       DELR(J)*DELC(I)*DH(J,I,K)
          ENDIF
        ENDIF
C
      ENDDO
C
C--RETURN
  400 RETURN
      END
C
C
      SUBROUTINE SSM5OT(KPER,KSTP,NTRANS,TIME2)
C ******************************************************************
C THIS SUBROUTINE SAVES INFORMATION FOR MULTI-NODE WELLS.
C ******************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,MXSS,NTSS,
     &                         NSS,SS,SSG,PRTOUT,IOUT,ISSGOUT
C
      IMPLICIT  NONE
      INTEGER   kper,kstp,ntrans,NUM,IQ,K,I,J,iGroup,iFlag,IU
      REAL      CTMP,TIME2
C    
C--IF ISSGOUT = 0, SAVE AVERAGE CONC. OF MULTI-NODE WELLS TO 
C--STANDARD OUTPUT FILE WHENEVER PRTOUT IS TRUE  
C--OTHERWISE SAVE TO UNIT DEFINED BY ISSGOUT

      IF(ISSGOUT.LE.0) THEN
        IF(.NOT.PRTOUT) GOTO 1200
        IU=IOUT
        WRITE(IU,1000)
        WRITE(IU,1002)  
      ELSE
        IU=ISSGOUT
        IF(KPER*KSTP*NTRANS.EQ.1) WRITE(IU,1002)  
      ENDIF      
    
      DO NUM=1,NTSS
        K =ss(1,num)
        I =ss(2,num)
        J =ss(3,num)
        IQ=ss(6,num)
        iGroup=ss(7,num)
        if(iGroup.le.0) cycle
        ctmp=ssg(4,iGroup)
        iFlag=int(ssg(1,iGroup))
        if(iFlag.ne.-999) then
          ssg(1,iGroup)=-999
          write(IU,1004) kper,kstp,ntrans,time2,iGroup,k,i,j,ctmp
        endif
      ENDDO

      IF(ISSGOUT.LE.0) WRITE(IU,1010) 

 1000 format(/1x,80('.'))
 1002 format(1x,'Stress  Time  Transport     Total         MNW   Layer',
     & '  Row Column  Average',
     &      /1x,'Period  Step    Step     Elapsed Time    Group   [K] ',
     & '  [I]  [J]     Conc.     ')     
 1004 format(1x, i4, 2x, i5, 3x, i5, 3x, g15.7, 2x, 4i6, 1x, g15.7)
 1010 format(1x,80('.')/)
C
 1200 RETURN
      END
C
C
      subroutine cgroup(ncol,nrow,nlay,ncomp,icomp,mxss,ntss,
     & ss,ssmc,ssg,icbund,cnew,delr,delc,dh)
c **********************************************************************
c this subroutine calculates the average concentration for a linked
c group sink/source such as a multi-node well
c **********************************************************************
c last modification: 02-15-2005
c
      implicit  none
      integer   k,i,j,iGroup,num,IQ,icbund,icomp,ncomp,mxss,ntss,
     &          ncol,nrow,nlay
      real      ss,ssmc,ssg,cnew,delr,delc,dh,ctmp,qss,csink,
     &          QC_group,Q_group,Qnet_group,cavg
      dimension ss(8,mxss),ssmc(ncomp,mxss),ssg(5,mxss),
     &          cnew(ncol,nrow,nlay,ncomp),delr(ncol),delc(nrow),
     &          dh(ncol,nrow,nlay),icbund(ncol,nrow,nlay,ncomp)
c
c--clear storage array
c
      do iGroup=1,ntss
        do i=1,5
          ssg(i,iGroup)=0.
        enddo
      enddo
c
c--get cumulative QC and Q (sinks only), and net Q (sinks/sources)
c
      do num=1,ntss  
        k=ss(1,num)
        i=ss(2,num)
        j=ss(3,num)
        ctmp=ss(4,num)
        if(ncomp.gt.1) ctmp=ssmc(icomp,num)                    !# LINE 875 SSM
        qss=ss(5,num)
        IQ=ss(6,num)
        iGroup=ss(7,num)
c
c--skip if at an inactive cell        
        if(icbund(j,i,k,icomp).le.0) cycle
c        
c--skip if not a linked group sink/source
        if(iGroup.eq.0 .or. IQ.ne.27) cycle
c
c--get cell concentration
        csink=cnew(j,i,k,icomp)
c
c--get volumetric |Q|*C, |Q|, and Q
        if(qss.lt.0) then
          QC_group=abs(qss)*delr(j)*delc(i)*dh(j,i,k)*csink
          Q_group =abs(qss)*delr(j)*delc(i)*dh(j,i,k)
        else
          QC_group=0.
          Q_group =0.  
        endif
        Qnet_group = qss*delr(j)*delc(i)*dh(j,i,k)
c
c--cumulate and store in ssg
        ssg(1,iGroup) = ssg(1,iGroup) + QC_group
        ssg(2,iGroup) = ssg(2,iGroup) + Q_group
        ssg(5,iGroup) = ssg(5,iGroup) + Qnet_group
c
c--get user-specified conc for any cell in the group
        ssg(3,iGroup) = max( ctmp,ssg(3,iGroup) )
c
c--done
      enddo
c
c--get composite concentrations
c
      do iGroup=1,ntss
        cavg = 0.
        QC_group =   ssg(1,iGroup)
        Q_group  =   ssg(2,iGroup)
        Qnet_group = ssg(5,iGroup)
        ctmp       = ssg(3,iGroup)
        if(Qnet_group.gt.0) then
          cavg=(QC_group+Qnet_group*ctmp)/(Q_group+Qnet_group)
        elseif(Q_group.gt.0) then
          cavg =QC_group/Q_group
        endif
        ssg(4,iGroup) = cavg
      enddo
c
c--normal return
c
      return
      end