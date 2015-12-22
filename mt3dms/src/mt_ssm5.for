C
      SUBROUTINE SSM5AL(INSSM,IOUT,ISSGOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & NCOMP,LCIRCH,LCRECH,LCCRCH,LCIEVT,LCEVTR,LCCEVT,MXSS,LCSS,
     & IVER,LCSSMC,LCSSG)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED IN THE SINK & SOURCE
C MIXING (SSM) PACKAGE.
C **********************************************************************
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   INSSM,IOUT,ISSGOUT,ISUM,ISUM2,NCOL,NROW,NLAY,NCOMP,
     &          LCIRCH,LCRECH,LCCRCH,LCIEVT,LCEVTR,LCCEVT,MXSS,LCSS,
     &          ISUMX,ISUMIX,NCR,ISOLD,ISOLD2,IVER,LCSSMC,LCSSG,
     &          IERR,IOSTAT
      CHARACTER LINE*200
      LOGICAL   FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,FIBS,
     &          FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
      COMMON /FC/FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,FIBS,
     &           FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
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
      ISOLD=ISUM
      ISOLD2=ISUM2
      NCR=NCOL*NROW
C
C--INTEGER ARRAYS
      LCIRCH=ISUM2
      IF(FRCH) ISUM2=ISUM2+NCR
      LCIEVT=ISUM2
      IF(FEVT.OR.FETS) ISUM2=ISUM2+NCR
C
C--REAL ARRAYS
      LCRECH=ISUM
      IF(FRCH) ISUM=ISUM+NCR
      LCCRCH=ISUM
      IF(FRCH) ISUM=ISUM+NCR * NCOMP
      LCEVTR=ISUM
      IF(FEVT.OR.FETS) ISUM=ISUM+NCR
      LCCEVT=ISUM
      IF(FEVT.OR.FETS) ISUM=ISUM+NCR * NCOMP
      LCSS=ISUM
      ISUM=ISUM + 7*MXSS
      LCSSMC=ISUM
      ISUM=ISUM+NCOMP*MXSS
      LCSSG=ISUM
      ISUM=ISUM + 5*MXSS
C
C--CHECK HOW MANY ELEMENTS OF ARRAYS X AND IX ARE USED
      ISUMX=ISUM-ISOLD
      ISUMIX=ISUM2-ISOLD2
      WRITE(IOUT,1090) ISUMX,ISUMIX
 1090 FORMAT(1X,I10,' ELEMENTS OF THE  X ARRAY USED BY THE SSM PACKAGE'
     & /1X,I10,' ELEMENTS OF THE IX ARRAY BY THE SSM PACKAGE'/)
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE SSM5RP(IN,IOUT,KPER,NCOL,NROW,NLAY,NCOMP,ICBUND,CNEW,
     & CRCH,CEVT,MXSS,NSS,SS,SSMC)
C ********************************************************************
C THIS SUBROUTINE READS CONCENTRATIONS OF SOURCES OR SINKS NEEDED BY
C THE SINK AND SOURCE MIXING (SSM) PACKAGE.
C ********************************************************************
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   IN,IOUT,KPER,NCOL,NROW,NLAY,NCOMP,ICBUND,
     &          MXSS,NSS,JJ,II,KK,NUM,IQ,INCRCH,INCEVT,NTMP,INDEX
      REAL      CRCH,CEVT,SS,SSMC,CSS,CNEW
      LOGICAL   FWEL,FDRN,FRIV,FGHB,FRCH,FEVT,FSTR,FRES,FFHB,FIBS,
     &          FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
      CHARACTER ANAME*24,TYPESS(-1:100)*15
      DIMENSION SS(7,MXSS),SSMC(NCOMP,MXSS),CRCH(NCOL,NROW,NCOMP),
     &          CEVT(NCOL,NROW,NCOMP),
     &          ICBUND(NCOL,NROW,NLAY,NCOMP),CNEW(NCOL,NROW,NLAY,NCOMP)
      COMMON /FC/FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,FIBS,
     &           FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF     
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
        CALL RARRAY(CRCH(1,1,INDEX),ANAME,NROW,NCOL,0,IN,IOUT)
      ENDDO
    2 FORMAT(/1X,'CONCENTRATION OF RECHARGE FLUXES',
     & ' WILL BE READ IN STRESS PERIOD',I3)
C
C--READ CONCENTRAION OF EVAPOTRANSPIRATION FLUX
   10 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 20
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
        WRITE(IOUT,11)
        GOTO 20
      ENDIF
   11 FORMAT(/1X,'CONCENTRATION OF E. T. FLUXES',
     & ' REUSED FROM LAST STRESS PERIOD')
C
      WRITE(IOUT,12) KPER
      ANAME='E. T. CONC. COMP. NO.'
      DO INDEX=1,NCOMP
        WRITE(ANAME(19:21),'(I3.2)') INDEX
        CALL RARRAY(CEVT(1,1,INDEX),ANAME,NROW,NCOL,0,IN,IOUT)
      ENDDO
   12 FORMAT(/1X,'CONCENTRATION OF E. T. FLUXES',
     & ' WILL BE READ IN STRESS PERIOD',I3)
C
   20 CONTINUE
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
          DO INDEX=1,NCOMP
            IF(SSMC(INDEX,NUM).GE.0) THEN
              CNEW(JJ,II,KK,INDEX)=SSMC(INDEX,NUM)
              ICBUND(JJ,II,KK,INDEX)=-ABS(ICBUND(JJ,II,KK,INDEX))
            ENDIF
          ENDDO
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
          IF(CSS.NE.0 .OR. ICBUND(JJ,II,KK,INDEX).LT.0)
     &     WRITE(IOUT,70) NUM,KK,II,JJ,CSS,TYPESS(IQ),INDEX
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
      SUBROUTINE SSM5FM(NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,DELR,DELC,
     & DH,IRCH,RECH,CRCH,IEVT,EVTR,CEVT,MXSS,NTSS,SS,SSMC,SSG,
     & QSTO,CNEW,ISS,A,RHS,NODES,UPDLHS,MIXELM)
C ******************************************************************
C THIS SUBROUTINE FORMULATES MATRIX COEFFICIENTS FOR THE SINK/
C SOURCE TERMS UNDER THE IMPLICIT FINITE-DIFFERENCE SCHEME.
C ******************************************************************
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,IRCH,IEVT,MXSS,
     &          NTSS,NUM,IQ,K,I,J,ISS,N,NODES,MIXELM,IGROUP,
     &          MHOST,KHOST,IHOST,JHOST
      REAL      CNEW,RECH,CRCH,EVTR,CEVT,SS,SSMC,SSG,
     &          CTMP,QSS,QCTMP,DELR,DELC,DH,QSTO,A,RHS
      LOGICAL   UPDLHS,FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,
     &          FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
      DIMENSION ICBUND(NCOL,NROW,NLAY,NCOMP),SS(7,MXSS),SSG(5,MXSS),
     &          SSMC(NCOMP,MXSS),RECH(NCOL,NROW),IRCH(NCOL,NROW),
     &          CRCH(NCOL,NROW,NCOMP),EVTR(NCOL,NROW),
     &          IEVT(NCOL,NROW),CEVT(NCOL,NROW,NCOMP),
     &          DELR(NCOL),DELC(NROW),CNEW(NCOL,NROW,NLAY,NCOMP),
     &          DH(NCOL,NROW,NLAY),QSTO(NCOL,NROW,NLAY),
     &          A(NODES),RHS(NODES)
      COMMON /FC/FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,FIBS,
     &           FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
C
C--DETERMINE AVERAGE CONCENTRATION FOR LINKED SINK/SOURCE GROUPS
      CALL CGROUP(NCOL,NROW,NLAY,NCOMP,ICOMP,MXSS,NTSS,
     & SS,SSMC,SSG,ICBUND,CNEW,DELR,DELC,DH)
C
C--FORMULATE [A] AND [RHS] MATRICES FOR EULERIAN SCHEMES
      IF(MIXELM.GT.0) GOTO 1000
C
C--TRANSIENT FLUID STORAGE TERM
      IF(ISS.EQ.0 .AND. UPDLHS) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
                N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
                A(N)=A(N)+QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)
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
C--(EVAPOTRANSPIRATION)
   10 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 20
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
C
C--(EVAPOTRANSPIRATION)
   30 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 40
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
C--POINT SINK/SOURCE TERMS
   40 DO NUM=1,NTSS
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
      SUBROUTINE SSM5BD(NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,DELR,DELC,
     & DH,IRCH,RECH,CRCH,IEVT,EVTR,CEVT,MXSS,NTSS,SS,SSMC,SSG,
     & QSTO,CNEW,RETA,DTRANS,ISS,RMASIO)
C ********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGETS ASSOCIATED WITH ALL SINK/
C SOURCE TERMS.
C ********************************************************************
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,IRCH,IEVT,MXSS,
     &          NTSS,NUM,IQ,K,I,J,ISS,IGROUP,MHOST,KHOST,IHOST,JHOST
      REAL      DTRANS,RECH,CRCH,EVTR,CEVT,SS,SSMC,SSG,CNEW,
     &          CTMP,QSS,RMASIO,DELR,DELC,DH,QSTO,RETA
      LOGICAL   FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,FIBS,
     &          FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
      DIMENSION ICBUND(NCOL,NROW,NLAY,NCOMP),SS(7,MXSS),SSG(5,MXSS),
     &          SSMC(NCOMP,MXSS),RECH(NCOL,NROW),IRCH(NCOL,NROW),
     &          CRCH(NCOL,NROW,NCOMP),EVTR(NCOL,NROW),
     &          IEVT(NCOL,NROW),CEVT(NCOL,NROW,NCOMP),
     &          CNEW(NCOL,NROW,NLAY,NCOMP),DELR(NCOL),DELC(NROW),
     &          DH(NCOL,NROW,NLAY),QSTO(NCOL,NROW,NLAY),
     &          RETA(NCOL,NROW,NLAY,NCOMP),RMASIO(122,2,NCOMP)
      COMMON /FC/FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,FIBS,
     &           FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
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
            CTMP=CNEW(J,I,K,ICOMP)
            IF(QSTO(J,I,K).GT.0) THEN
              RMASIO(118,1,ICOMP)=RMASIO(118,1,ICOMP)
     &         +QSTO(J,I,K)*CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
            ELSE
              RMASIO(118,2,ICOMP)=RMASIO(118,2,ICOMP)
     &         +QSTO(J,I,K)*CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--AREAL SINK/SOURCE TERMS
C--(RECHARGE)
   50 IF(.NOT.FRCH) GOTO 100
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
C
C--(EVAPOTRANSPIRATION)
  100 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 200
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
      SUBROUTINE SSM5OT(NCOL,NROW,NLAY,KPER,KSTP,NTRANS,NCOMP,ICOMP,
     & ICBUND,MXSS,NTSS,NSS,SS,SSG,PRTOUT,TIME2,IOUT,ISSGOUT)
C ******************************************************************
C THIS SUBROUTINE SAVES INFORMATION FOR MULTI-NODE WELLS.
C ******************************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,kper,kstp,ntrans,NCOMP,ICOMP,ICBUND,
     &          MXSS,NTSS,NSS,NUM,IQ,K,I,J,iGroup,IOUT,iFlag,
     &          ISSGOUT,IU
      REAL      SS,CTMP,TIME2,SSG
      LOGICAL   PRTOUT
      DIMENSION ICBUND(NCOL,NROW,NLAY,NCOMP),SS(7,MXSS),SSG(5,MXSS)
    
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
      real      ss,ssmc,ssg,cold,cnew,delr,delc,dh,ctmp,qss,csink,
     &          QC_group,Q_group,Qnet_group,cavg
      dimension ss(7,mxss),ssmc(ncomp,mxss),ssg(5,mxss),
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
        if(icomp.gt.1) ctmp=ssmc(icomp,num)
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