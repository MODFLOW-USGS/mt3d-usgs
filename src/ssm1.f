C
      SUBROUTINE SSM1AR(IN)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED IN THE SINK & SOURCE
C MIXING (SSM) PACKAGE.
C **********************************************************************
C
      USE MT3DMS_MODULE, ONLY: INSSM,IOUT,NCOL,NROW,NLAY,NCOMP,
     &                         FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,
     &                         FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,FSWT,
     &                         FSFR,FUZF,IVER,iUnitTRNOP,ISSGOUT,
     &                         MXSS,NSS,NTSS,RECH,IRCH,CRCH,EVTR,
     &                         IEVT,CEVT,SS,SSMC,SSG,KSSZERO,FMIFMT6
      USE SFRVARS,       ONLY: NSTRM,MXSGMT,MXRCH,NSFINIT,  
     &                         ISFSOLV,WIMP,WUPS,CCLOSESF,MXITERSF,
     &                         CRNTSF,NOBSSF,NJASF
      USE UZTVARS,       ONLY: CUZINF,UZET,CUZET,GWET,CGWET,IETFLG,
     &                         FINFIL,UZFLX,UZQSTO,                
     &                         IETFLG,IUZFOPT,IUZFBND,             
     &                         IUZFOPTG,UZRECH,IUZRCH,IGWET,CUZRCH
C
      IMPLICIT  NONE
      INTEGER   ISTART, ISTOP, LLOC,I,J                            
      INTEGER   IERR,IN,
     &          NUZTOP,IRUNFLG                                     
      REAL      R                                                  
      CHARACTER VERSION*11,LINE*72,BNAME*24
      CHARACTER(LEN=*),PARAMETER :: FMT2="(3I8,I7)"                
C
      INSSM=IN
C
C--ALLOCATE
      ALLOCATE(ISSGOUT,MXSS,NSS,NTSS)                              
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1000) INSSM
 1000 FORMAT(/1X,'SSM1 -- SINK & SOURCE MIXING PACKAGE,',
     & ' VERSION 1, MAY 2016, INPUT READ FROM UNIT',I3)
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
     &            ' SINKS/SOURCES SAVED In UNIT:',I3)
      ENDIF
C
C--ALLOCATE SPACE FOR ARRAYS
      IF(FRCH .AND. .NOT.FMIFMT6) THEN
        ALLOCATE(RECH(NCOL,NROW))
        ALLOCATE(IRCH(NCOL,NROW))
        ALLOCATE(CRCH(NCOL,NROW,NCOMP))
      ELSE
        ALLOCATE(RECH(1,1))
        ALLOCATE(IRCH(1,1))
        ALLOCATE(CRCH(1,1,1))
      ENDIF
      IF(FEVT.OR.FETS .AND. .NOT.FMIFMT6) THEN
        ALLOCATE(EVTR(NCOL,NROW))
        ALLOCATE(IEVT(NCOL,NROW))
        ALLOCATE(CEVT(NCOL,NROW,NCOMP))
      ELSE
        ALLOCATE(EVTR(1,1))
        ALLOCATE(IEVT(1,1))
        ALLOCATE(CEVT(1,1,1))
      ENDIF
      IF(FUZF) THEN
        ALLOCATE(UZRECH(NCOL,NROW),IUZRCH(NCOL,NROW))
        ALLOCATE(GWET(NCOL,NROW),IGWET(NCOL,NROW))
        ALLOCATE(CUZRCH(NCOL,NROW,NCOMP),CGWET(NCOL,NROW,NCOMP))
      ELSE
        ALLOCATE(UZRECH(1,1),IUZRCH(1,1))
        ALLOCATE(GWET(1,1),IGWET(1,1))
        ALLOCATE(CUZRCH(1,1,1),CGWET(1,1,1))
      ENDIF
      ALLOCATE(SS(8,MXSS))
      ALLOCATE(SSMC(NCOMP,MXSS))
      ALLOCATE(SSG(5,MXSS))
      ALLOCATE(KSSZERO(MXSS))
      KSSZERO=0              
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
C--NORMAL RETURN
  100 RETURN
      END
C
      SUBROUTINE SSM1RP(KPER)
C ********************************************************************
C THIS SUBROUTINE READS CONCENTRATIONS OF SOURCES OR SINKS NEEDED BY
C THE SINK AND SOURCE MIXING (SSM) PACKAGE.
C ********************************************************************
C
      USE MT3DMS_MODULE, ONLY: INSSM,IOUT,NCOL,NROW,NLAY,NCOMP,ICBUND,
     &                         CNEW,
     &                         FWEL,FDRN,FRIV,FGHB,FRCH,FEVT,FSTR,FRES,
     &                         FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,FSWT,
     &                         FSFR,FUZF,
     &                         CRCH,CEVT,MXSS,NSS,SS,SSMC,
     &                         KSSZERO,FMIFMT6
      USE UZTVARS,       ONLY: CUZRCH,CGWET
      USE MIN_SAT,       ONLY: MUTSSM
C
      IMPLICIT  NONE
      INTEGER   IN,KPER,JJ,II,KK,NUM,IQ,INCRCH,INCEVT,NTMP,INDEX,
     &          INCUZINF,INCUZET,INCGWET,INCSRFLK,INCUZF
      REAL      CSS
      CHARACTER ANAME*24,TYPESS(-1:100)*15
C
      IN=INSSM
C
C--INITIALIZE.
      TYPESS=''
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
      TYPESS(30)='STREAM FL ROUT.'
      TYPESS(31)='UNSAT ZONE FLOW'
C
C--READ CONCENTRATION OF DIFFUSIVE SOURCES/SINKS (RECHARGE/E.T.)
C--FOR CURRENT STRESS PERIOD IF THEY ARE SIMULATED IN FLOW MODEL
      IF(.NOT.FRCH) GOTO 11
C
C--IF MF6-STYLE LINKER FILES BEING USED, RCH ENTERED AS LIST IN SS 
C--(NOT ENTERED AS AN ARRAY)
      IF(FRCH.AND.FMIFMT6) GOTO 11
C
C--READ FLAG INCRCH INDICATING HOW TO READ RECHARGE CONCENTRATION
      READ(IN,'(I10)') INCRCH
C
C--IF INCRCH < 0, CONCENTRATION REUSED FROM LAST STRESS PERIOD
      IF(INCRCH.LT.0) THEN
        WRITE(IOUT,1)
        GOTO 11
      ENDIF
    1 FORMAT(/1X,'CONCENTRATION OF RECHARGE FLUXES',
     & ' REUSED FROM LAST STRESS PERIOD')
C
C--IF INCRCH >= 0, READ AN ARRAY
C--CONTAINING CONCENTRATION OF RECHARGE FLUX [CRCH]
      WRITE(IOUT,2) KPER
      ANAME='RECH. CONC. COMP. NO.'
      DO INDEX=1,NCOMP
        WRITE(ANAME(19:21),'(I3.2)') INDEX
        CALL RARRAY(CRCH(1:NCOL,1:NROW,INDEX),ANAME,NROW,NCOL,0,IN,IOUT)
      ENDDO
    2 FORMAT(/1X,'CONCENTRATION OF RECHARGE FLUXES',
     &           ' WILL BE READ IN STRESS PERIOD',I3)
C
C--READ CONCENTRATION OF EVAPOTRANSPIRATION FLUX
   11 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 20
C
C--IF MF6-STYLE LINKER FILES BEING USED, EVT ENTERED AS LIST IN SS 
C--(NOT ENTERED AS AN ARRAY)
      IF((FEVT.OR.FETS).AND.FMIFMT6) GOTO 20
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
     &           ' REUSED FROM LAST STRESS PERIOD')
C
      WRITE(IOUT,13) KPER
      ANAME='E. T. CONC. COMP. NO.'
      DO INDEX=1,NCOMP
        WRITE(ANAME(19:21),'(I3.2)') INDEX
        CALL RARRAY(CEVT(1:NCOL,1:NROW,INDEX),ANAME,NROW,NCOL,0,IN,IOUT)
      ENDDO
   13 FORMAT(/1X,'CONCENTRATION OF E. T. FLUXES',
     &           ' WILL BE READ IN STRESS PERIOD',I3)
   20 CONTINUE
C
C--READ CONCENTRATION OF UZF FLUX - UZRCH AND GWET
      IF(.NOT.FUZF) GOTO 39
C
C--READ FLAG INCRCH INDICATING HOW TO READ UZF CONCENTRATION
      READ(IN,'(I10)') INCUZF
C
C--IF INCUZF < 0, CONCENTRATION REUSED FROM LAST STRESS PERIOD
      IF(INCUZF.LT.0) THEN
        WRITE(IOUT,22)
        GOTO 29
      ENDIF
   22 FORMAT(/1X,'CONCENTRATION OF UZF-RCH FLUXES',
     &           ' REUSED FROM LAST STRESS PERIOD')
C
C--IF INCUZF >= 0, READ AN ARRAY
C--CONTAINING CONCENTRATION OF UZF-RCH FLUX [CUZRCH]
      WRITE(IOUT,23) KPER
      ANAME='UZRCH CONC. COMP. NO.'
      DO INDEX=1,NCOMP
        WRITE(ANAME(19:21),'(I3.2)') INDEX
        CALL RARRAY(CUZRCH(1:NCOL,1:NROW,INDEX),ANAME,NROW,NCOL,0,IN,
     &              IOUT)
      ENDDO
   23 FORMAT(/1X,'CONCENTRATION OF UZF-RCH FLUXES',
     &           ' WILL BE READ IN STRESS PERIOD',I3)
29    CONTINUE
C
C--GWET----------
C
C--READ FLAG INCRCH INDICATING HOW TO READ UZF CONCENTRATION
      READ(IN,'(I10)') INCGWET
C
C--IF INCGWET < 0, CONCENTRATION REUSED FROM LAST STRESS PERIOD
      IF(INCGWET.LT.0) THEN
        WRITE(IOUT,32)
        GOTO 39
      ENDIF
   32 FORMAT(/1X,'CONCENTRATION OF GW-ET FLUXES',
     &           ' REUSED FROM LAST STRESS PERIOD')
C
C--IF INCGWET >= 0, READ AN ARRAY
C--CONTAINING CONCENTRATION OF GW-ET FLUX [CGWET]
      WRITE(IOUT,33) KPER
      ANAME=' GWET CONC. COMP. NO.'
      DO INDEX=1,NCOMP
        WRITE(ANAME(19:21),'(I3.2)') INDEX
        CALL RARRAY(CGWET(1:NCOL,1:NROW,INDEX),ANAME,NROW,NCOL,0,IN,
     &              IOUT)
      ENDDO
   33 FORMAT(/1X,'CONCENTRATION OF GW-ET FLUXES',
     & ' WILL BE READ IN STRESS PERIOD',I3)
39    CONTINUE
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
          KSSZERO=0
        ENDDO
C
C-------RESET ICBUND TO ABS(ICBUND) FOR CONSTANT CONCENTRATION CELLS
        DO INDEX=1,NCOMP
          DO KK=1,NLAY
            DO II=1,NROW
              DO JJ=1,NCOL
                IF(ICBUND(JJ,II,KK,INDEX).LT.0) THEN
                  ICBUND(JJ,II,KK,INDEX)=ABS(ICBUND(JJ,II,KK,INDEX))
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO  
C
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
          IF(KK.EQ.0) THEN    
            KSSZERO(NUM)=1    
            IF(.NOT.FRCH) THEN
              WRITE(*,*) 'RECHARGE BOUNDARY NEEDED IF K IS SET TO 0'
              WRITE(IOUT,*) 'RECHARGE BOUNDARY NEEDED IF K IS SET TO 0'
              CALL USTOP(' ')
            ENDIF
          ELSE                                      
            DO INDEX=1,NCOMP                        
              IF(SSMC(INDEX,NUM).GE.0) THEN         
                CNEW(JJ,II,KK,INDEX)=SSMC(INDEX,NUM)
                ICBUND(JJ,II,KK,INDEX)=-ABS(ICBUND(JJ,II,KK,INDEX))
              ENDIF
            ENDDO  
          ENDIF    
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
        IF(MUTSSM.EQ.0) THEN
        DO INDEX=1,NCOMP
          CSS=SSMC(INDEX,NUM)
          IF(IQ.EQ.-1.AND.KK.EQ.0) THEN                     
            WRITE(IOUT,70) NUM,KK,II,JJ,CSS,TYPESS(IQ),INDEX
          ELSEIF(IQ.EQ.26.AND.KK.EQ.0.AND.II.EQ.0) THEN
            WRITE(IOUT,70) NUM,KK,II,JJ,CSS,TYPESS(IQ),INDEX
          ELSE                                              
            IF(CSS.NE.0 .OR. ICBUND(JJ,II,KK,INDEX).LT.0)
     &        WRITE(IOUT,70) NUM,KK,II,JJ,CSS,TYPESS(IQ),INDEX
          ENDIF
          IF(CSS.LT.0 .AND. IQ.EQ.2) WRITE(IOUT,71) -INT(CSS)  
        ENDDO
        ENDIF
      ENDDO
   30 FORMAT(/1X,'ERROR: MAXIMUM NUMBER OF POINT SINKS/SOURCES',
     &           ' EXCEEDED'/1X,'INCREASE [MXSS] IN SSM INPUT FILE')
   40 FORMAT(/1X,'POINT SINKS/SOURCES OF SPECIFIED CONCENTRATION',
     &           ' REUSED FROM LAST STRESS PERIOD')
   50 FORMAT(/1X,'NO. OF POINT SINKS/SOURCES OF SPECIFIED',
     &           ' CONCENTRATIONS =',I5,' IN STRESS PERIOD',I3)
   60 FORMAT(/5X,'  NO    LAYER   ROW   COLUMN   CONCENTRATION',
     &           '       TYPE            COMPONENT')
   70 FORMAT(3X,4(I5,3X),1X,G15.7,5X,A15,I6)
   71 FORMAT(8X, '>>RECIRCULATION WELL; INPUT CONCENTRATION',
     &           ' FROM NODE #',I10.8)
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
      SUBROUTINE SSM1FM(ICOMP,HT2,TIME1,TIME2)
C ******************************************************************
C THIS SUBROUTINE FORMULATES MATRIX COEFFICIENTS FOR THE SINK/
C SOURCE TERMS UNDER THE IMPLICIT FINITE-DIFFERENCE SCHEME.
C ******************************************************************
C
      USE UZTVARS,       ONLY: IUZFBND,THETAW,
     &                         GWET,CGWET,UZRECH,IUZRCH,IGWET,CUZRCH
      USE PKG2PKG,       ONLY: NSNK2UZF,IUZCODESK,INOD2SKUZ,QSNK2UZF
      USE MIN_SAT,       ONLY: QC7,DRYON
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,DELR,
     &                         DELC,DH,IRCH,RECH,CRCH,IEVT,EVTR,CEVT,
     &                         MXSS,NTSS,SS,SSMC,SSG,QSTO,CNEW,ISS,A,
     &                         RHS,NODES,UPDLHS,MIXELM,COLD,
     &                         ISS,FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,
     &                         FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,
     &                         FSWT,FSFR,FUZF,FSNKUZF,
     &                         RETA,COLD,IALTFM,INCTS,MXWEL,IWCTS,
     &                         CINACT,DELT,DTRANS,iUnitTRNOP,COLDFLW,
     &                         IDRY2,PRSITY,DZ,SORBMASS,FMIFMT6
C
      IMPLICIT  NONE
      INTEGER   ICOMP,NUM,IQ,K,I,J,N,IGROUP,
     &          MHOST,KHOST,IHOST,JHOST,II,IWN,Q1
      REAL      CTMP,QSS,QCTMP,VOLAQU,VOL1,VOL2,VCELL,VUNSAT1,VUNSAT2,
     &          FRAC,GWQOUT
      REAL      HT2,TIME1,TIME2
C
C--ZERO OUT QC7(:,:,:,7:9) TERMS FOR STORAGE AND BOUNDARY CONDITIONS
C--INFLOWS ARE COMPUTED AND STORED AS Q*C WHILE OUTFLOWS ARE COMPUTED AND STORED AS Q
      IF(DRYON) THEN
        QC7(:,:,:,7)=0.0 !Qin*Cin
        QC7(:,:,:,8)=0.0 !Qin
        QC7(:,:,:,9)=0.0 !Qout
      ENDIF
C
C--DETERMINE AVERAGE CONCENTRATION FOR LINKED SINK/SOURCE GROUPS
      CALL CGROUP(NCOL,NROW,NLAY,NCOMP,ICOMP,MXSS,NTSS,
     &            SS,SSMC,SSG,ICBUND,CNEW,DELR,DELC,DH)
C
C--FORMULATE [A] AND [RHS] MATRICES FOR EULERIAN SCHEMES
      IF(MIXELM.GT.0) GOTO 1000
C
C--TRANSIENT FLUID STORAGE TERM
      IF(ISS.EQ.0) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
              IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
                IF(iUnitTRNOP(7).GT.0)THEN
                  IF(IUZFBND(J,I).GT.0) THEN
                    RHS(N)=RHS(N)-QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)*
     &                     COLD(J,I,K,ICOMP)
                  ELSE
                    RHS(N)=RHS(N)-QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)
     &                     *RETA(J,I,K,ICOMP)*COLD(J,I,K,ICOMP)
                  ENDIF
                ELSE
                  IF(IALTFM.GE.2.AND.IALTFM.LE.5) THEN
                    IF(IALTFM.EQ.2.OR.IALTFM.EQ.4.OR.IALTFM.EQ.5) THEN
                      RHS(N)=RHS(N)
     &                       -QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)
     &                       *RETA(J,I,K,ICOMP)*COLD(J,I,K,ICOMP) 
C...................ADD TERM FOR M_GAIN/M_LOSS
                      IF(IALTFM.EQ.2)THEN
                        IF(QSTO(J,I,K).GT.0) THEN
                          RHS(N)=RHS(N)+QSTO(J,I,K)*DELR(J)*DELC(I)*
     &                           DH(J,I,K)*(RETA(J,I,K,ICOMP)-1.)*
     &                           COLD(J,I,K,ICOMP)
                        ELSE
                          A(N)=A(N)-QSTO(J,I,K)*DELR(J)*DELC(I)*
     &                         DH(J,I,K)*(RETA(J,I,K,ICOMP)-1.)
                        ENDIF
                      ENDIF
C...................ADD TERM FOR M_GAIN/M_LOSS FROM SORBMASS
                      IF(IALTFM.EQ.5) THEN
                        IF(QSTO(J,I,K).GT.0) THEN !FALLING HEAD
                          RHS(N)=RHS(N)+QSTO(J,I,K)*DELR(J)*DELC(I)*
     &                           DH(J,I,K)*(RETA(J,I,K,ICOMP)-1.)*
     &                           COLD(J,I,K,ICOMP)
                        ELSE  !RISING HEAD
                          VOL2=DELR(J)*DELC(I)*DH(J,I,K) +
     &                         DELR(J)*DELC(I)*DH(J,I,K)*
     &                         QSTO(J,I,K)/PRSITY(J,I,K)*(HT2-TIME2)
                          VOL1=DELR(J)*DELC(I)*DH(J,I,K) +
     &                         DELR(J)*DELC(I)*DH(J,I,K)*
     &                         QSTO(J,I,K)/PRSITY(J,I,K)*(HT2-TIME1)
                          VCELL=DELR(J)*DELC(I)*DZ(J,I,K)
                          VUNSAT1=MAX(0.0,VCELL-VOL1)
                          VUNSAT2=MAX(0.0,VCELL-VOL2)
                          IF(VUNSAT1.LE.1.0E-7) THEN
                            FRAC=0.
                          ELSE
                            FRAC=(VUNSAT1-VUNSAT2)/VUNSAT1
                          ENDIF
                          RHS(N)=RHS(N)-SORBMASS(J,I,K,ICOMP)*FRAC/
     &                           DTRANS
                        ENDIF
                      ENDIF
                    ELSEIF(IALTFM.EQ.3) THEN
                     RHS(N)=RHS(N)-QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)
     &                      *COLD(J,I,K,ICOMP) 
                    ENDIF
                  ELSE
                    IF(UPDLHS)
     &                A(N)=A(N)+QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)
                  ENDIF
                ENDIF
              ELSE
                IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
                  IF(IDRY2.EQ.1) THEN
CC-----------STORAGE
                    QC7(J,I,K,7)=-QSTO(J,I,K)*DH(J,I,K)*DELR(J)*DELC(I)
     &                           *RETA(J,I,K,ICOMP)*COLDFLW(J,I,K,ICOMP)
                    QC7(J,I,K,8)=-QSTO(J,I,K)*DH(J,I,K)*DELR(J)*DELC(I)
     &                           *RETA(J,I,K,ICOMP)
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--AREAL SINK/SOURCE TERMS 
C--(RECHARGE)
      IF(.NOT.FRCH) GOTO 12
C
C--IF MF6-STYLE LINKER FILES BEING USED, RCH ENTERED AS LIST IN SS 
C--(NOT ENTERED AS AN ARRAY)
      IF(FRCH.AND.FMIFMT6) GOTO 12
C
      DO I=1,NROW
        DO J=1,NCOL
          K=IRCH(J,I)
          IF(K.GT.0) THEN 
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
              IF(RECH(J,I).LT.0) THEN
                IF(UPDLHS) A(N)=A(N)+RECH(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
              ELSE
                RHS(N)=RHS(N)-RECH(J,I)
     &                *CRCH(J,I,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)
              ENDIF
            ELSE
              IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
                IF(DRYON) THEN
                  VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                  IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                  IF(RECH(J,I).LT.0) THEN
                    QC7(J,I,K,9)=QC7(J,I,K,9)-RECH(J,I)*ABS(VOLAQU)
                  ELSE
                    QC7(J,I,K,7)=QC7(J,I,K,7)-
     &                           RECH(J,I)*ABS(VOLAQU)*CRCH(J,I,ICOMP)
                    QC7(J,I,K,8)=QC7(J,I,K,8)-RECH(J,I)*ABS(VOLAQU)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C--(EVAPOTRANSPIRATION)
   12 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 30
C
C--IF MF6-STYLE LINKER FILES BEING USED, EVT ENTERED AS LIST IN SS
C--(NOT ENTERED AS AN ARRAY)
      IF((FEVT.OR.FETS).AND.FMIFMT6) GOTO 30
C
      DO I=1,NROW
        DO J=1,NCOL
          K=IEVT(J,I)
          IF(K.GT.0) THEN 
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
              IF(EVTR(J,I).LT.0.AND.(CEVT(J,I,ICOMP).LT.0 .OR. 
     &                 CEVT(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                IF(UPDLHS) A(N)=A(N)+EVTR(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
              ELSEIF(CEVT(J,I,ICOMP).GT.0) THEN
                RHS(N)=RHS(N)-EVTR(J,I)*CEVT(J,I,ICOMP)*DELR(J)*DELC(I)*
     &                 DH(J,I,K)
              ENDIF
            ELSE
              IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
                IF(DRYON) THEN
                  VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                  IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                  IF(EVTR(J,I).LT.0.AND.(CEVT(J,I,ICOMP).LT.0 .OR. 
     &                  CEVT(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                    QC7(J,I,K,9)=QC7(J,I,K,9)-EVTR(J,I)*ABS(VOLAQU)
                  ELSEIF(CEVT(J,I,ICOMP).GT.0) THEN
                    QC7(J,I,K,7)=QC7(J,I,K,7)-EVTR(J,I)*ABS(VOLAQU)*
     &                           CEVT(J,I,ICOMP)
                    QC7(J,I,K,8)=QC7(J,I,K,8)-EVTR(J,I)*ABS(VOLAQU)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C--UZF TERMS
30    IF(.NOT.FUZF) GOTO 20
C-----UZRECH
      DO I=1,NROW
        DO J=1,NCOL
          K=IUZRCH(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(UZRECH(J,I).LT.0) THEN
              IF(UPDLHS) A(N)=A(N)+UZRECH(J,I) 
            ELSE
              RHS(N)=RHS(N)-UZRECH(J,I)*CUZRCH(J,I,ICOMP)
            ENDIF
          ELSE
            IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                IF(UZRECH(J,I).LT.0) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-UZRECH(J,I)
                ELSE
                  QC7(J,I,K,7)=QC7(J,I,K,7)-
     &                         UZRECH(J,I)*CUZRCH(J,I,ICOMP)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-UZRECH(J,I) 
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C-----GWET
      DO I=1,NROW
        DO J=1,NCOL
          K=IGWET(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(GWET(J,I).LT.0.AND.(CGWET(J,I,ICOMP).LT.0 .OR. 
     &                 CGWET(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
              IF(UPDLHS) A(N)=A(N)+GWET(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
            ELSEIF(CGWET(J,I,ICOMP).GT.0) THEN
              RHS(N)=RHS(N)-GWET(J,I)*CGWET(J,I,ICOMP)
     1              *DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
          ELSE
            IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(GWET(J,I).LT.0.AND.(CGWET(J,I,ICOMP).LT.0 .OR. 
     &                     CGWET(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-GWET(J,I)*ABS(VOLAQU)
                ELSEIF(CGWET(J,I,ICOMP).GT.0) THEN
                  QC7(J,I,K,7)=QC7(J,I,K,7)-GWET(J,I)*CGWET(J,I,ICOMP)
     &            *ABS(VOLAQU)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-GWET(J,I)*ABS(VOLAQU)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C-----SEEPAGE
      IF(FSNKUZF) THEN
        DO II=1,NSNK2UZF
          IF(IUZCODESK(II).EQ.1) THEN !GW DISCHARGE
            N=INOD2SKUZ(II)
            CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
            GWQOUT=QSNK2UZF(II)
            IF(GWQOUT.LT.0) THEN                           
              IF(UPDLHS) A(N)=A(N)-ABS(GWQOUT) 
            ENDIF
          ENDIF
        ENDDO
      ENDIF
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
C
C--SKIP IF THE WELL IS A PART OF TREATMENT SYSTEM 
        IF(iUnitTRNOP(20).GT.0) THEN               
          IF(SS(8,NUM).GT.0) THEN                 
            IWN=NINT(REAL(SS(8,NUM)))
            IF(IWCTS(IWN).GT.0) THEN
              Q1=QSS*DELR(J)*DELC(I)*DH(J,I,K)
              IF(Q1.GT.0) CYCLE
            ENDIF
          ENDIF                                   
        ENDIF                                     
C
C--SKIP 1 LAK ENTRY IN SSM FILE; LAK CONC IS TRANSFERRED IN READGS
        IF(IQ.EQ.26.AND.K.EQ.0.AND.I.EQ.0) CYCLE
C                                                 
C--RESET QSS FOR MASS-LOADING SOURCES (IQ=15)        
        IF(IQ.EQ.15) THEN
          IF(DELR(J)*DELC(I)*DH(J,I,K).LT.1.E-8) THEN
            QSS=0.
          ELSE
            QSS=1./(DELR(J)*DELC(I)*DH(J,I,K))
          ENDIF
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
        IF(ICBUND(J,I,K,ICOMP).LE.0.OR.IQ.LE.0) THEN
          IF(ICBUND(J,I,K,ICOMP).EQ.0.AND.IQ.GT.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(QSS.LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-QSS*ABS(VOLAQU)
              ELSE
                QC7(J,I,K,7)=QC7(J,I,K,7)-QSS*ABS(VOLAQU)*CTMP
                QC7(J,I,K,8)=QC7(J,I,K,8)-QSS*ABS(VOLAQU)       
              ENDIF        
            ENDIF
          ENDIF
        ELSEIF(IQ.EQ.8) THEN
C
C-- SUPPORT FOR MF6-STYLE LINKER FILE REQUIRES SPECIAL HANDLING
C   BECAUSE ALL BOUNDARY CONCENTRATIONS REQUIRE USE OF THE SS
C   LIST OBJECT 
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(QSS.LT.0.AND.(CTMP.LT.0 .OR. 
     &         CTMP.GE.CNEW(J,I,K,ICOMP))) THEN
              IF(UPDLHS) A(N)=A(N)+QSS*DELR(J)*DELC(I)*DH(J,I,K)
            ELSEIF(CTMP.GT.0) THEN
              RHS(N)=RHS(N)-QSS*CTMP*DELR(J)*DELC(I)*
     &               DH(J,I,K)
            ENDIF
          ENDIF
        ELSE
C
C--ADD CONTRIBUTIONS TO MATRICES [A] AND [RHS]        
          N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
          IF(QSS.LT.0) THEN
            IF(UPDLHS) A(N)=A(N)+QSS*DELR(J)*DELC(I)*DH(J,I,K)
          ELSE
            RHS(N)=RHS(N)-QSS*CTMP*DELR(J)*DELC(I)*DH(J,I,K)
          ENDIF        
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
      IF(.NOT.FRCH) GOTO 32
C
C--IF MF6-STYLE LINKER FILES BEING USED, RCH ENTERED AS LIST IN SS 
C--(NOT ENTERED AS AN ARRAY)
      IF(FRCH.AND.FMIFMT6) GOTO 32
C
      DO I=1,NROW
        DO J=1,NCOL
          K=IRCH(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0.AND. RECH(J,I).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(UPDLHS) A(N)=A(N)-RECH(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
            RHS(N)=RHS(N)
     &            -RECH(J,I)*CRCH(J,I,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)
          ELSE
            IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(RECH(J,I).LT.0) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-RECH(J,I)*ABS(VOLAQU)
                ELSE
                  QC7(J,I,K,7)=QC7(J,I,K,7)-RECH(J,I)*ABS(VOLAQU)*
     &                         CRCH(J,I,ICOMP)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-RECH(J,I)*ABS(VOLAQU)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C--(EVAPOTRANSPIRATION)
   32 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 40
C
C--IF MF6-STYLE LINKER FILES BEING USED, EVT ENTERED AS LIST IN SS
C--(NOT ENTERED AS AN ARRAY)
      IF((FEVT.OR.FETS).AND.FMIFMT6) GOTO 40
C
      DO I=1,NROW
        DO J=1,NCOL
          K=IEVT(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(EVTR(J,I).LT.0.AND.(CEVT(J,I,ICOMP).LT.0 .OR. 
     &           CEVT(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN 
              CYCLE
            ELSEIF(CEVT(J,I,ICOMP).GE.0) THEN  
              IF(UPDLHS) A(N)=A(N)-EVTR(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
              RHS(N)=RHS(N)-EVTR(J,I)*CEVT(J,I,ICOMP)*DELR(J)*DELC(I)*
     &               DH(J,I,K)
            ENDIF
          ELSE
            IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(EVTR(J,I).LT.0.AND.(CEVT(J,I,ICOMP).LT.0 .OR. 
     &          CEVT(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-EVTR(J,I)*ABS(VOLAQU)
                ELSEIF(CEVT(J,I,ICOMP).GT.0) THEN
                  QC7(J,I,K,7)=QC7(J,I,K,7)-EVTR(J,I)*ABS(VOLAQU)*
     &                         CEVT(J,I,ICOMP)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-EVTR(J,I)*ABS(VOLAQU)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C--UZF TERMS
40    IF(.NOT.FUZF) GOTO 41
C-----UZRECH
      DO I=1,NROW
        DO J=1,NCOL
          K=IUZRCH(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0.AND. UZRECH(J,I).GT.0) THEN    
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(UPDLHS) A(N)=A(N)-UZRECH(J,I) 
            RHS(N)=RHS(N)-UZRECH(J,I)*CUZRCH(J,I,ICOMP)
          ELSE
            IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                IF(UZRECH(J,I).LT.0) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-
     &            UZRECH(J,I)
                ELSE
                 QC7(J,I,K,7)=QC7(J,I,K,7)-UZRECH(J,I)*CUZRCH(J,I,ICOMP)
                 QC7(J,I,K,8)=QC7(J,I,K,8)-UZRECH(J,I)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C-----GWET
      DO I=1,NROW
        DO J=1,NCOL
          K=IGWET(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(GWET(J,I).LT.0.AND.(CGWET(J,I,ICOMP).LT.0 .OR. 
     &               CGWET(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN  
              CYCLE
            ELSEIF(CGWET(J,I,ICOMP).GE.0) THEN  
              IF(UPDLHS) A(N)=A(N)-GWET(J,I)*DELR(J)*DELC(I)*DH(J,I,K) 
              RHS(N)=RHS(N)-GWET(J,I)*CGWET(J,I,ICOMP)
     &        *DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
          ELSE
            IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(GWET(J,I).LT.0.AND.(CGWET(J,I,ICOMP).LT.0 .OR. 
     &                      CGWET(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-GWET(J,I)*ABS(VOLAQU)
                ELSEIF(CGWET(J,I,ICOMP).GT.0) THEN
                  QC7(J,I,K,7)=QC7(J,I,K,7)-GWET(J,I)*CGWET(J,I,ICOMP)
     &            *ABS(VOLAQU)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-GWET(J,I)*ABS(VOLAQU)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C-----SEEPAGE
      IF(FSNKUZF) THEN
        DO II=1,NSNK2UZF
          IF(IUZCODESK(II).EQ.1) THEN !GW DISCHARGE
            N=INOD2SKUZ(II)
            CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
            GWQOUT=QSNK2UZF(II)
            IF(GWQOUT.GT.0) THEN                           
              IF(UPDLHS) A(N)=A(N)-ABS(GWQOUT)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
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
C--COMPUTE PRODUCT OF Q*C
        QCTMP=QSS*CTMP        
C
C--SKIP 1 LAK ENTRY IN SSM FILE; LAK CONC IS TRANSFERRED IN READGS
        IF(IQ.EQ.26.AND.K.EQ.0.AND.I.EQ.0) CYCLE
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
C--SKIP IF NOT ACTIVE CELL      
        IF(ICBUND(J,I,K,ICOMP).LE.0.OR.IQ.LE.0) THEN
          IF(ICBUND(J,I,K,ICOMP).EQ.0.AND.IQ.GT.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(QSS.LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-QSS*ABS(VOLAQU)
              ELSE
                QC7(J,I,K,7)=QC7(J,I,K,7)-QCTMP*ABS(VOLAQU)
                QC7(J,I,K,8)=QC7(J,I,K,8)-QSS*ABS(VOLAQU)
              ENDIF        
            ENDIF
          ENDIF
        ELSE
C
C--SKIP IF SINK CELL
        IF(QSS.LE.0.AND.IQ.NE.15) CYCLE
C
C--ADD CONTRIBUTIONS TO MATRICES [A] AND [RHS]
        N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
        IF(UPDLHS) A(N)=A(N)-QSS*DELR(J)*DELC(I)*DH(J,I,K)
        RHS(N)=RHS(N)-QCTMP*DELR(J)*DELC(I)*DH(J,I,K)        
        ENDIF
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
      SUBROUTINE SSM1BD(ICOMP,DTRANS,HT2,TIME1,TIME2)
C ********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGETS ASSOCIATED WITH ALL SINK/
C SOURCE TERMS.
C ********************************************************************
C
      USE UZTVARS,       ONLY:IUZFBND,THETAW,
     &                        GWET,CGWET,UZRECH,IUZRCH,IGWET,CUZRCH
      USE PKG2PKG,       ONLY:NSNK2UZF,IUZCODESK,INOD2SKUZ,QSNK2UZF
      USE MIN_SAT,       ONLY:QC7,DRYON
      USE MT3DMS_MODULE, ONLY:NCOL,NROW,NLAY,NCOMP,ICBUND,DELR,DELC,
     &                        DH,IRCH,RECH,CRCH,IEVT,EVTR,CEVT,MXSS,
     &                        NTSS,SS,SSMC,SSG,QSTO,CNEW,RETA,ISS,
     &                        RMASIO,
     &                        FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,
     &                        FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,FSWT,
     &                        FSFR,FUZF,FSNKUZF,
     &                        INCTS,MXWEL,IWCTS,COLD,IALTFM,CINACT,
     &                        iUnitTRNOP,DELT,COLDFLW,IDRY2,SORBMASS,
     &                        PRSITY,DZ,FMIFMT6
C
      IMPLICIT  NONE
      INTEGER   ICOMP,NUM,IQ,K,I,J,IGROUP,MHOST,KHOST,IHOST,JHOST,II,N,
     &          IWN
      REAL      DTRANS,CTMP,QSS,VOLAQU,RMULT,VOL1,VOL2,VCELL,VUNSAT1,
     &          VUNSAT2,FRAC,GWQOUT
      REAL      HT2,TIME1,TIME2
C
C--INITIALIZE
      CTMP=0.
C
C--ZERO OUT QC7(:,:,:,7:9) TERMS FOR STORAGE AND BOUNDARY CONDITIONS
C--INFLOWS ARE COMPUTED AND STORED AS Q*C WHILE OUTFLOWS ARE COMPUTED AND STORED AS Q
      IF(DRYON) THEN
        QC7(:,:,:,7)=0.0 !Qin*Cin
        QC7(:,:,:,8)=0.0 !Qin
        QC7(:,:,:,9)=0.0 !Qout
      ENDIF
C
C--DETERMINE AVERAGE CONCENTRATION FOR LINKED SINK/SOURCE GROUPS
      CALL CGROUP(NCOL,NROW,NLAY,NCOMP,ICOMP,MXSS,NTSS,
     &            SS,SSMC,SSG,ICBUND,CNEW,DELR,DELC,DH)      
C
C--TRANSIENT GROUNDWATER STORAGE TERM
      IF(ISS.NE.0) GOTO 50
C
C--RECORD MASS STORAGE CHANGES FOR DISSOLVED AND SORBED PHASES
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
              IF(iUnitTRNOP(7).GT.0)THEN
                IF(IUZFBND(J,I).GT.0) THEN
                  IF(QSTO(J,I,K).GT.0) THEN
                    RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)+QSTO(J,I,K)*
     &                                  DELR(J)*DELC(I)*DH(J,I,K)*
     &                                  COLD(J,I,K,ICOMP)*DTRANS
                  ELSE
                    RMASIO(118,2,ICOMP)=RMASIO(118,2,ICOMP)+QSTO(J,I,K)*
     &                                  DELR(J)*DELC(I)*DH(J,I,K)*
     &                                  COLD(J,I,K,ICOMP)*DTRANS
                  ENDIF
                ELSE
                  CTMP=COLD(J,I,K,ICOMP)
                  IF(QSTO(J,I,K).GT.0) THEN
                    RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)+QSTO(J,I,K)*
     &                                  CTMP*DELR(J)*DELC(I)*DH(J,I,K)
     &                                  *RETA(J,I,K,ICOMP)*DTRANS
                  ELSE
                    RMASIO(118,2,ICOMP)=RMASIO(118,2,ICOMP)+QSTO(J,I,K)*
     &                                  CTMP*DELR(J)*DELC(I)*DH(J,I,K)
     &                                  *RETA(J,I,K,ICOMP)*DTRANS 
                  ENDIF
                ENDIF
              ELSE
10              IF(IALTFM.GE.2) THEN
                  IF(IALTFM.GE.2.AND.IALTFM.LE.5) CTMP=COLD(J,I,K,ICOMP)
                  IF(IALTFM.EQ.3) THEN
                    RMULT=1.0
                  ELSE
                    RMULT=RETA(J,I,K,ICOMP)
                  ENDIF
                  IF(QSTO(J,I,K).GT.0) THEN
                    RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)+QSTO(J,I,K)*
     &                                  CTMP*DELR(J)*DELC(I)*DH(J,I,K)
     &                                  *RMULT*DTRANS 
                    IF(IALTFM.EQ.2) RMASIO(118,2,ICOMP)=
     &                              RMASIO(118,2,ICOMP)-QSTO(J,I,K)
     &                              *CTMP*DELR(J)*DELC(I)*
     &                              DH(J,I,K)*(RMULT-1.)*DTRANS 
                    IF(IALTFM.EQ.5) THEN
                      RMASIO(118,2,ICOMP)=RMASIO(118,2,ICOMP)-
     &                                    QSTO(J,I,K)*CTMP*DELR(J)*
     &                                    DELC(I)*DH(J,I,K)*(RMULT-1.)*
     &                                    DTRANS 
                      SORBMASS(J,I,K,ICOMP)=SORBMASS(J,I,K,ICOMP)
     &                                    +QSTO(J,I,K)*CTMP*DELR(J)*
     &                                    DELC(I)*DH(J,I,K)*(RMULT-1.)*
     &                                    DTRANS
                    ENDIF
                  ELSE
                    RMASIO(118,2,ICOMP)=RMASIO(118,2,ICOMP)+
     &                                  QSTO(J,I,K)*CTMP*DELR(J)*
     &                                  DELC(I)*DH(J,I,K)*RMULT*DTRANS
                    IF(IALTFM.EQ.2)
     &                RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)-
     &                                    QSTO(J,I,K)*CNEW(J,I,K,ICOMP)*
     &                                    DELR(J)*DELC(I)*DH(J,I,K)*
     &                                    (RMULT-1.)*DTRANS 
                    IF(IALTFM.EQ.5) THEN
                      VOL2=DELR(J)*DELC(I)*DH(J,I,K) +
     &                     DELR(J)*DELC(I)*DH(J,I,K)*
     &                     QSTO(J,I,K)/PRSITY(J,I,K)*(HT2-TIME2)
                      VOL1=DELR(J)*DELC(I)*DH(J,I,K) +
     &                     DELR(J)*DELC(I)*DH(J,I,K)*
     &                     QSTO(J,I,K)/PRSITY(J,I,K)*(HT2-TIME1)
                      VCELL=DELR(J)*DELC(I)*DZ(J,I,K)
                      VUNSAT1=MAX(0.0,VCELL-VOL1)
                      VUNSAT2=MAX(0.0,VCELL-VOL2)
                      IF(VUNSAT1.LE.1.0E-7) THEN
                        FRAC=0.
                      ELSE
                        FRAC=(VUNSAT1-VUNSAT2)/VUNSAT1
                      ENDIF
                      RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)+
     &                                    SORBMASS(J,I,K,ICOMP)*FRAC
                      SORBMASS(J,I,K,ICOMP)=SORBMASS(J,I,K,ICOMP)*
     &                                      (1.-FRAC)
                    ENDIF
                  ENDIF
                ELSE
                  CTMP=CNEW(J,I,K,ICOMP)
                  IF(QSTO(J,I,K).GT.0) THEN
                    RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)+
     &               QSTO(J,I,K)*CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
                  ELSE
                    RMASIO(118,2,ICOMP)=RMASIO(118,2,ICOMP)+
     &               QSTO(J,I,K)*CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
                IF(IDRY2.EQ.1) THEN
CC-----------STORAGE
                  CTMP=COLDFLW(J,I,K,ICOMP)
                  RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)+QSTO(J,I,K)*
     &                                CTMP*DTRANS*DELR(J)*DELC(I)*
     &                                DH(J,I,K)*RETA(J,I,K,ICOMP)
                  QC7(J,I,K,7)=-QSTO(J,I,K)*DH(J,I,K)*DELR(J)*DELC(I)*
     &                         RETA(J,I,K,ICOMP)*CTMP
                  QC7(J,I,K,8)=-QSTO(J,I,K)*DH(J,I,K)*DELR(J)*DELC(I)*
     &                         RETA(J,I,K,ICOMP)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--AREAL SINK/SOURCE TERMS
C--(RECHARGE)
   50 IF(.NOT.FRCH) GOTO 100
C
C--IF MF6-STYLE LINKER FILES BEING USED, RCH ENTERED AS LIST IN SS 
C--(NOT ENTERED AS AN ARRAY)
      IF(FRCH.AND.FMIFMT6) GOTO 100
C
      DO I=1,NROW
        DO J=1,NCOL
          K=IRCH(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            CTMP=CRCH(J,I,ICOMP)
            IF(RECH(J,I).LT.0) CTMP=CNEW(J,I,K,ICOMP)
            IF(RECH(J,I).GT.0) THEN
              RMASIO(7,1,ICOMP)=RMASIO(7,1,ICOMP)+RECH(J,I)*CTMP*DTRANS*
     &                          DELR(J)*DELC(I)*DH(J,I,K)
            ELSE
              RMASIO(7,2,ICOMP)=RMASIO(7,2,ICOMP)+RECH(J,I)*CTMP*DTRANS*
     &                          DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
C        
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              CTMP=CRCH(J,I,ICOMP)
              IF(RECH(J,I).LT.0) CTMP=CNEW(J,I,K,ICOMP)
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(RECH(J,I).LT.0) THEN
                RMASIO(7,2,ICOMP)=RMASIO(7,2,ICOMP)+RECH(J,I)*CTMP*
     &                            DTRANS*ABS(VOLAQU)
                QC7(J,I,K,9)=QC7(J,I,K,9)-RECH(J,I)*ABS(VOLAQU)
              ELSE
                RMASIO(7,1,ICOMP)=RMASIO(7,1,ICOMP)+RECH(J,I)*CTMP*
     &                            DTRANS*ABS(VOLAQU)
                QC7(J,I,K,7)=QC7(J,I,K,7)-RECH(J,I)*ABS(VOLAQU)*CTMP
                QC7(J,I,K,8)=QC7(J,I,K,8)-RECH(J,I)*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C--(EVAPOTRANSPIRATION)
  100 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 200
C
C--IF MF6-STYLE LINKER FILES BEING USED, EVT ENTERED AS LIST IN SS 
C--(NOT ENTERED AS AN ARRAY)
      IF((FEVT.OR.FETS).AND.FMIFMT6) GOTO 200
C
      DO I=1,NROW
        DO J=1,NCOL
          K=IEVT(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            CTMP=CEVT(J,I,ICOMP)
            IF(EVTR(J,I).LT.0.AND.(CTMP.LT.0 .or.
     &                             CTMP.GE.CNEW(J,I,K,ICOMP))) THEN
              CTMP=CNEW(J,I,K,ICOMP)
            ELSEIF(CTMP.LT.0) THEN        
              CTMP=0.
            ENDIF
            IF(EVTR(J,I).GT.0) THEN
              RMASIO(8,1,ICOMP)=RMASIO(8,1,ICOMP)+EVTR(J,I)*CTMP*DTRANS*
     &                          DELR(J)*DELC(I)*DH(J,I,K)
            ELSE
              RMASIO(8,2,ICOMP)=RMASIO(8,2,ICOMP)+EVTR(J,I)*CTMP*DTRANS*
     &                          DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
C
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              IF(CEVT(J,I,ICOMP)==0.) CYCLE
              CTMP=CEVT(J,I,ICOMP)
              IF(EVTR(J,I).LT.0.AND.(CTMP.LT.0 .or.
     &                         CTMP.GE.CNEW(J,I,K,ICOMP))) THEN
                CTMP=CNEW(J,I,K,ICOMP)
              ELSEIF(CTMP.LT.0) THEN        
                CTMP=0.
              ENDIF
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(EVTR(J,I).LT.0) THEN
                RMASIO(8,2,ICOMP)=RMASIO(8,2,ICOMP)+EVTR(J,I)*CTMP*
     &                            DTRANS*ABS(VOLAQU)
                QC7(J,I,K,9)=QC7(J,I,K,9)-EVTR(J,I)*ABS(VOLAQU)
              ELSE
                RMASIO(8,1,ICOMP)=RMASIO(8,1,ICOMP)+EVTR(J,I)*CTMP*
     &                            DTRANS*ABS(VOLAQU)
                QC7(J,I,K,7)=QC7(J,I,K,7)-EVTR(J,I)*ABS(VOLAQU)*CTMP
                QC7(J,I,K,8)=QC7(J,I,K,8)-EVTR(J,I)*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C--UZF TERMS
  200 IF(.NOT.FUZF) GOTO 300
C-----UZRECH
      DO I=1,NROW
        DO J=1,NCOL
          K=IUZRCH(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
          CTMP=CUZRCH(J,I,ICOMP)
          IF(UZRECH(J,I).LT.0) CTMP=CNEW(J,I,K,ICOMP)
            IF(UZRECH(J,I).GT.0) THEN
              RMASIO(53,1,ICOMP)=RMASIO(53,1,ICOMP)+UZRECH(J,I)*CTMP*
     &                           DTRANS
            ELSE
              RMASIO(53,2,ICOMP)=RMASIO(53,2,ICOMP)+UZRECH(J,I)*CTMP*
     &                           DTRANS
            ENDIF
          ELSE
            IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                CTMP=CUZRCH(J,I,ICOMP)
                IF(UZRECH(J,I).LT.0) CTMP=CNEW(J,I,K,ICOMP)
                IF(UZRECH(J,I).LT.0) THEN
                  RMASIO(53,2,ICOMP)=RMASIO(53,2,ICOMP)+UZRECH(J,I)*CTMP
     &                               *DTRANS 
                  QC7(J,I,K,9)=QC7(J,I,K,9)-UZRECH(J,I)
                ELSE
                  RMASIO(53,1,ICOMP)=RMASIO(53,1,ICOMP)+UZRECH(J,I)*CTMP
     &                               *DTRANS
                  QC7(J,I,K,7)=QC7(J,I,K,7)-UZRECH(J,I)*CTMP
                  QC7(J,I,K,8)=QC7(J,I,K,8)-UZRECH(J,I)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C----GWET
      DO I=1,NROW
        DO J=1,NCOL
          K=IGWET(J,I)
          IF(K.EQ.0) CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            CTMP=CGWET(J,I,ICOMP)
            IF(GWET(J,I).LT.0.AND.(CTMP.LT.0 .or.
     &                             CTMP.GE.CNEW(J,I,K,ICOMP))) THEN
              CTMP=CNEW(J,I,K,ICOMP)
            ELSEIF(CTMP.LT.0) THEN        
              CTMP=0.
            ENDIF
            IF(GWET(J,I).GT.0) THEN
             RMASIO(54,1,ICOMP)=RMASIO(54,1,ICOMP)+GWET(J,I)*CTMP*DTRANS
     &       *DELR(J)*DELC(I)*DH(J,I,K)
            ELSE
             RMASIO(54,2,ICOMP)=RMASIO(54,2,ICOMP)+GWET(J,I)*CTMP*DTRANS
     &       *DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
          ELSE
            IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                CTMP=CGWET(J,I,ICOMP)
                IF(GWET(J,I).LT.0.AND.(CTMP.LT.0 .or.
     &                           CTMP.GE.CNEW(J,I,K,ICOMP))) THEN
                  CTMP=CNEW(J,I,K,ICOMP)
                ELSEIF(CTMP.LT.0) THEN        
                  CTMP=0.
                ENDIF
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(GWET(J,I).LT.0) THEN
                  RMASIO(54,2,ICOMP)=RMASIO(54,2,ICOMP)+GWET(J,I)*CTMP
     &                               *DTRANS*ABS(VOLAQU)
                  QC7(J,I,K,9)=QC7(J,I,K,9)-GWET(J,I)*ABS(VOLAQU)
                ELSE
                  RMASIO(54,1,ICOMP)=RMASIO(54,1,ICOMP)+GWET(J,I)*CTMP
     &                               *DTRANS*ABS(VOLAQU)
                  QC7(J,I,K,7)=QC7(J,I,K,7)-GWET(J,I)*CTMP*ABS(VOLAQU)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-GWET(J,I)*ABS(VOLAQU)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C-----SEEPAGE
      IF(FSNKUZF) THEN
        DO II=1,NSNK2UZF
          IF(IUZCODESK(II).EQ.1) THEN !GW DISCHARGE
            N=INOD2SKUZ(II)
            CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
            GWQOUT=QSNK2UZF(II)
            CTMP=CNEW(J,I,K,ICOMP)
            IF(GWQOUT.LT.0) THEN                           
                RMASIO(53,2,ICOMP)=RMASIO(53,2,ICOMP)-ABS(GWQOUT)*
     &                             CTMP*DTRANS 
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C--POINT SINK/SOURCE TERMS
  300 DO NUM=1,NTSS
        K=SS(1,NUM)
        I=SS(2,NUM)
        J=SS(3,NUM)
        QSS=SS(5,NUM)
        IQ=SS(6,NUM)
        CTMP=SS(4,NUM)
        IF(NCOMP.GT.1) CTMP=SSMC(ICOMP,NUM)
C
C--SKIP IF THE WELL IS A PART OF TREATMENT SYSTEM
        IF(iUnitTRNOP(20).GT.0) THEN              
          IF(SS(8,NUM).GT.0) THEN
            IWN=NINT(REAL(SS(8,NUM)))
            IF(IWCTS(IWN).GT.0) CYCLE      
          ENDIF                                  
        ENDIF                                    
C
C--SKIP 1 LAK ENTRY IN SSM FILE; LAK CONC IS TRANSFERRED IN READGS
        IF(IQ.EQ.26.AND.K.EQ.0.AND.I.EQ.0) CYCLE
C
C--SKIP IF NOT ACTIVE CELL
        IF(.NOT.DRYON) THEN
          IF(ICBUND(J,I,K,ICOMP).LE.0.OR.IQ.LE.0) CYCLE
        ENDIF
C
C--RESET QSS FOR MASS-LOADING SOURCES (IQ=15)
        IF(IQ.EQ.15) THEN
          IF(DELR(J)*DELC(I)*DH(J,I,K).LT.1.E-8) THEN
            QSS=0.
          ELSE
            QSS=1./(DELR(J)*DELC(I)*DH(J,I,K))
          ENDIF
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
        IF(QSS.LT.0) THEN
          CTMP=CNEW(J,I,K,ICOMP)
          IF(ABS(CTMP-CINACT).LT.1.E-3) CTMP=0.
        ENDIF
C
        IF(ICBUND(J,I,K,ICOMP).GT.0.AND.(IQ.GT.0.AND.IQ.NE.8)) THEN
          IF(QSS.GT.0) THEN
            RMASIO(IQ,1,ICOMP)=RMASIO(IQ,1,ICOMP)+QSS*CTMP*DTRANS*
     &                         DELR(J)*DELC(I)*DH(J,I,K)
          ELSE
            RMASIO(IQ,2,ICOMP)=RMASIO(IQ,2,ICOMP)+QSS*CTMP*DTRANS*
     &                         DELR(J)*DELC(I)*DH(J,I,K)
          ENDIF
        ELSEIF(ICBUND(J,I,K,ICOMP).GT.0 .AND. IQ.EQ.8) THEN
          CTMP=SS(4,NUM)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(QSS.LT.0.AND.(CTMP.LT.0 .or.
     &                             CTMP.GE.CNEW(J,I,K,ICOMP))) THEN
              CTMP=CNEW(J,I,K,ICOMP)
            ELSEIF(CTMP.LT.0) THEN        
              CTMP=0.
            ENDIF
            IF(QSS.GT.0) THEN
              RMASIO(8,1,ICOMP)=RMASIO(8,1,ICOMP)+QSS*CTMP*DTRANS*
     &                          DELR(J)*DELC(I)*DH(J,I,K)
            ELSE
              RMASIO(8,2,ICOMP)=RMASIO(8,2,ICOMP)+QSS*CTMP*DTRANS*
     &                          DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
C
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              IF(QSS.LT.0.AND.(CTMP.LT.0 .or.
     &                         CTMP.GE.CNEW(J,I,K,ICOMP))) THEN
                CTMP=CNEW(J,I,K,ICOMP)
              ELSEIF(CTMP.LT.0) THEN        
                CTMP=0.
              ENDIF
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(QSS.LT.0) THEN
                RMASIO(8,2,ICOMP)=RMASIO(8,2,ICOMP)+QSS*CTMP*
     &                            DTRANS*ABS(VOLAQU)
                QC7(J,I,K,9)=QC7(J,I,K,9)-QSS*ABS(VOLAQU)
              ELSE
                RMASIO(8,1,ICOMP)=RMASIO(8,1,ICOMP)+QSS*CTMP*
     &                            DTRANS*ABS(VOLAQU)
                QC7(J,I,K,7)=QC7(J,I,K,7)-QSS*ABS(VOLAQU)*CTMP
                QC7(J,I,K,8)=QC7(J,I,K,8)-QSS*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ELSE
          IF(ICBUND(J,I,K,ICOMP).EQ.0.AND.IQ.GT.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(QSS.LT.0) THEN
                RMASIO(IQ,2,ICOMP)=RMASIO(IQ,2,ICOMP)+QSS*CTMP*DTRANS*
     &                             ABS(VOLAQU)
                QC7(J,I,K,9)=QC7(J,I,K,9)-QSS*ABS(VOLAQU)
              ELSE
                RMASIO(IQ,1,ICOMP)=RMASIO(IQ,1,ICOMP)+QSS*CTMP*DTRANS*
     &                             ABS(VOLAQU)
                QC7(J,I,K,7)=QC7(J,I,K,7)-QSS*ABS(VOLAQU)*CTMP
                QC7(J,I,K,8)=QC7(J,I,K,8)-QSS*ABS(VOLAQU)
              ENDIF        
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C--RETURN
  400 RETURN
      END
C
C
      SUBROUTINE SSM1OT(KPER,KSTP,NTRANS,TIME2)
C ******************************************************************
C THIS SUBROUTINE SAVES INFORMATION FOR MULTI-NODE WELLS.
C ******************************************************************
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
        IF(iGroup.le.0) CYCLE
        ctmp=ssg(4,iGroup)
        !iFlag=int(ssg(1,iGroup))
        iFlag = 0
        IF(ABS((-999.)-ssg(1,iGroup)) .LE. 1.0E-03) iFlag=-999
        IF(iFlag.ne.-999) THEN
          ssg(1,iGroup)=-999
          WRITE(IU,1004) kper,kstp,ntrans,time2,iGroup,k,i,j,ctmp
        ENDIF
      ENDDO
C
      IF(ISSGOUT.LE.0) WRITE(IU,1010) 
 1000 FORMAT(/1x,80('.'))
 1002 FORMAT(1x,'Stress  Time  Transport     Total         MNW   Layer',
     &          '  Row Column  Average',
     &      /1x,'Period  Step    Step     Elapsed Time    Group   [K] ',
     &          '  [I]  [J]     Conc.     ')     
 1004 FORMAT(1x, i4, 2x, i5, 3x, i5, 3x, g15.7, 2x, 4i6, 1x, g15.7)
 1010 FORMAT(1x,80('.')/)
C
 1200 RETURN
      END
C
C
      SUBROUTINE cgroup(ncol,nrow,nlay,ncomp,icomp,mxss,ntss,
     & ss,ssmc,ssg,icbund,cnew,delr,delc,dh)
c **********************************************************************
c this subroutine calculates the average concentration for a linked
c group sink/source such as a multi-node well
c **********************************************************************
c last modification: 02-15-2005
c
      IMPLICIT  NONE
      INTEGER   k,i,j,iGroup,num,IQ,icbund,icomp,ncomp,mxss,ntss,
     &          ncol,nrow,nlay
      REAL      ss,ssmc,ssg,cnew,delr,delc,dh,ctmp,qss,csink,
     &          QC_group,Q_group,Qnet_group,cavg
      DIMENSION ss(8,mxss),ssmc(ncomp,mxss),ssg(5,mxss),
     &          cnew(ncol,nrow,nlay,ncomp),delr(ncol),delc(nrow),
     &          dh(ncol,nrow,nlay),icbund(ncol,nrow,nlay,ncomp)
c
c--clear storage array
c
      DO iGroup=1,ntss
        DO i=1,5
          ssg(i,iGroup)=0.
        ENDDO
      ENDDO
c
c--get cumulative QC and Q (sinks only), and net Q (sinks/sources)
c
      DO num=1,ntss  
        k=ss(1,num)
        i=ss(2,num)
        j=ss(3,num)
        ctmp=ss(4,num)
        IF(ncomp.gt.1) ctmp=ssmc(icomp,num) 
        qss=ss(5,num)
        IQ=ss(6,num)
        iGroup=ss(7,num)
C
C--SKIP IS LAK CELL
        IF(IQ.EQ.26.AND.K.EQ.0.AND.I.EQ.0) CYCLE
c
c--skip if at an inactive cell        
        IF(icbund(j,i,k,icomp).le.0) CYCLE
c        
c--skip if not a linked group sink/source
        IF(iGroup.eq.0 .or. IQ.ne.27) CYCLE
c
c--get cell concentration
        csink=cnew(j,i,k,icomp)
c
c--get volumetric |Q|*C, |Q|, and Q
        IF(qss.lt.0) THEN
          QC_group=ABS(qss)*delr(j)*delc(i)*dh(j,i,k)*csink
          Q_group =ABS(qss)*delr(j)*delc(i)*dh(j,i,k)
        ELSE
          QC_group=0.
          Q_group =0.  
        ENDIF
        Qnet_group = qss*delr(j)*delc(i)*dh(j,i,k)
c
c--cumulate and store in ssg
        ssg(1,iGroup) = ssg(1,iGroup) + QC_group
        ssg(2,iGroup) = ssg(2,iGroup) + Q_group
        ssg(5,iGroup) = ssg(5,iGroup) + Qnet_group
c
c--get user-specified conc for any cell in the group
        ssg(3,iGroup) = MAX(ctmp,ssg(3,iGroup))
c
c--done
      ENDDO
c
c--get composite concentrations
c
      DO iGroup=1,ntss
        cavg = 0.
        QC_group   = ssg(1,iGroup)
        Q_group    = ssg(2,iGroup)
        Qnet_group = ssg(5,iGroup)
        ctmp       = ssg(3,iGroup)
        IF(Qnet_group.gt.0) THEN
          cavg=(QC_group+Qnet_group*ctmp)/(Q_group+Qnet_group)
        ELSEIF(Q_group.gt.0) THEN
          cavg =QC_group/Q_group
        ENDIF
        ssg(4,iGroup) = cavg
      ENDDO
c
c--normal return
c
      RETURN
      END
