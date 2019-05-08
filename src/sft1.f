C
C
      SUBROUTINE SFT1AR(IN)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES SPACE FOR SFR VARIABLES
C***********************************************************************
      USE SFRVARS
      USE LAKVARS
      USE MT3DMS_MODULE, ONLY: INSFT,IOUT,NCOMP,iUnitTRNOP
      INTEGER IN
      LOGICAL OPND
C
      ALLOCATE(NSFINIT,MXSFBC,ICBCSF,IOUTOBS,IETSFR,ISFRBC)
      ALLOCATE(NSSSF)
      ISFRBC=0
      ALLOCATE(NSTRM,NSF2SF,MXSGMT,MXRCH)
      ALLOCATE(ISFSOLV,WIMP,WUPS,CCLOSESF,MXITERSF,CRNTSF,IPRTXMD)
      ALLOCATE(NOBSSF,NJASF)                                     
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INSFT
 1030 FORMAT(/1X,'SFT1 -- STREAM TRANSPORT PACKAGE,',
     &           ' VERSION 1, MAY 2016, INPUT READ FROM UNIT',I3)
C
C--READ NUMBER OF STREAMS
      READ(INSFT,*) NSFINIT,MXSFBC,ICBCSF,IOUTOBS,IETSFR
C
C--IF NSFINIT>0, DO RIGOROUS TRANSPORT ROUTING THROUGH SFR
C--IF NSFINIT<0, USE SFR ONLY AS GW BOUNDARY, ACTIVE FLAG
      IF(NSFINIT.LT.0) THEN
        NSFINIT=-NSFINIT
        ISFRBC=1
        WRITE(IOUT,*) ' SFR USED ONLY AS GW BOUNDARY CONDITION'
      ENDIF
C
      WRITE(IOUT,10) NSFINIT,MXSFBC
10    FORMAT(1X,'NUMBER OF STREAMS = ',I5,
     &      /1X,'MAXIMUM NUMBER OF STREAM BOUNDARY CONDITIONS = ',I5)
      IF(ICBCSF.GT.0) WRITE(IOUT,12)
C12    FORMAT(1X,'RCH-BY-RCH INFORMATION WILL BE PRINTED ON UNIT ',I5)
12    FORMAT(1X,'ICBCSF OPTION IS CURRENTLY UNAVAILABLE')
      IF(IOUTOBS.GT.0) THEN
        WRITE(IOUT,13) IOUTOBS
13      FORMAT(1X,'STREAM-FLOW OBSERVATION OUTPUT ',
     &            ' WILL BE SAVED IN UNIT:',I4)
        INQUIRE(UNIT=IOUTOBS,OPENED=OPND)
        IF(.NOT.OPND) THEN
          WRITE(IOUT,*) 'TO CREATE STREAM-FLOW OBSERVATION OUTPUT FILE',
     &                  'NAM FILE MUST CONTAIN UNIT NUMBER ',IOUTOBS
          WRITE(*,*) 'TO CREATE STREAM-FLOW OBSERVATION OUTPUT FILE, ',
     &               'NAM FILE MUST CONTAIN UNIT NUMBER ',IOUTOBS
          STOP
        ENDIF
      ENDIF
      IF(ISFTTR.EQ.0) THEN
        WRITE(IOUT,21)
      ELSE
        WRITE(IOUT,22)
      ENDIF
21    FORMAT(1X,'STEADY-STATE SFR FLOW SOLUTION IS USED; MASS WILL',
     &          ' BE GAINED/LOST')
22    FORMAT(1X,'TRANSIENT SFR FLOW SOLUTION IS USED; MASS WILL',
     &          ' BE CONSERVED')
C
      IF(IETSFR.EQ.0) THEN
        WRITE(IOUT,15)
      ELSE
        WRITE(IOUT,16)
      ENDIF
15    FORMAT(1X,'MASS DOES NOT EXIT VIA STREAM ET')
16    FORMAT(1X,'MASS IS ALLOWED TO EXIT VIA STREAM ET')
C
C--ALLOCATE INITIAL AND BOUNDARY CONDITION ARRAYS
      ALLOCATE(CNEWSF(NSFINIT,NCOMP),COLDSF(NSFINIT,NCOMP),
     &         COLDSF2(NSFINIT,NCOMP),CNEWSFTMP(NSFINIT,NCOMP),
     &         DISPSF(NSFINIT,NCOMP),IBNDSF(NSFINIT),
     &         BUFFSF(NSFINIT,NCOMP))
      ALLOCATE(ISFNBC(MXSFBC),ISFBCTYP(MXSFBC))
      ALLOCATE(CBCSF(MXSFBC,NCOMP))                           
      CBCSF=0.
C
C--ALLOCATE
        ALLOCATE(QPRECSF(NSFINIT),QRUNOFSF(NSFINIT),
     &           QETSF(NSFINIT),IEXIT(NSFINIT),QPRECSFO(NSFINIT),
     &           QRUNOFSFO(NSFINIT),QOUTSF(NSFINIT),QOUTSFO(NSFINIT),
     &           VOLSFO(NSFINIT),VOLSFN(NSFINIT),SFLEN(NSFINIT))
C
C--IF LAKE PACKAGE NOT ACTIVE, NLKINIT WILL BE UNDEFINED.  
C  ASSIGN VALUE OF 1 IF LAKE PACKAGE INACTIVE
      IF(iUnitTRNOP(18).EQ.0) THEN
        ALLOCATE(NLKINIT)
        NLKINIT=1
      ENDIF
      ALLOCATE(RMASSF(NLKINIT),VOUTSF(NLKINIT))
      RMASSF=0.
      IBNDSF=1
C
C--CUMULATIVE BUDGET TERMS
      ALLOCATE(CFLOINSF(NCOMP),CFLOOUTSF(NCOMP),CGW2SFR(NCOMP),
     &         CGWFROMSFR(NCOMP),CLAK2SFR(NCOMP),CLAKFROMSFR(NCOMP),
     &         CPRECSF(NCOMP),CRUNOFSF(NCOMP),CETSF(NCOMP),
     &         CSTORINSF(NCOMP),CSTOROTSF(NCOMP),CCCINSF(NCOMP),
     &         CCCOUTSF(NCOMP),CUZF2SFRGW(NCOMP),CLOSTMASS(NCOMP),
     &         CGAINEDMASS(NCOMP),CUZF2SFRINF(NCOMP))
      CFLOINSF=0.
      CFLOOUTSF=0.
      CGW2SFR=0.
      CUZF2SFRGW=0.
      CUZF2SFRINF=0.
      CGWFROMSFR=0.
      CLAK2SFR=0.
      CLAKFROMSFR=0.
      CPRECSF=0.
      CRUNOFSF=0.
      CETSF=0.
      CSTORINSF=0.
      CSTOROTSF=0.
      CCCINSF=0.
      CCCOUTSF=0.
      CLOSTMASS=0.
      CGAINEDMASS=0.
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE SFT1RP(KPER)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES READS LAK VARIABLES - INITIAL CONCS
C***********************************************************************
      USE SFRVARS
      USE MT3DMS_MODULE, ONLY: INSFT,IOUT,NCOMP
      CHARACTER ANAME*24
      INTEGER   KPER
C
C--PRINT A HEADER
      WRITE(IOUT,1000)
 1000 FORMAT(//1X,'STREAM INPUT PARAMETERS'/1X,23('-')/)
C
C--STREAM SOLVER SETTINGS
      READ(INSFT,*) ISFSOLV,WIMP,WUPS,CCLOSESF,MXITERSF,CRNTSF,IPRTXMD
      ISFSOLV=1
      WRITE(IOUT,20) ISFSOLV
20    FORMAT(' STREAM SOLVER OPTION = ',I5,
     &     /,'   1 = FINITE DIFFERENCE FORMULATION')
      WRITE(IOUT,22) WIMP
22    FORMAT(' STREAM SOLVER TIME WEIGHTING FACTOR = ',G12.5,
     &     /,'   0.0 = EXPLICIT SCHEME IS USED',
     &     /,'   0.5 = CRANK-NICOLSON SCHEME IS USED',
     &     /,'   1.0 = FULLY IMPLICIT SCHEME IS USED')
      WRITE(IOUT,23) WUPS
23    FORMAT(' STREAM SOLVER SPACE WEIGHTING FACTOR = ',G12.5,
     &     /,'   0.0 = CENTRAL-IN-SPACE WEIGHTING',
     &     /,'   1.0 = UPSTREAM WEIGHTING')
      WRITE(IOUT,24) CCLOSESF
24    FORMAT(' CLOSURE CRITERION FOR SFT SOLVER = ',G12.5)
      WRITE(IOUT,26) MXITERSF
26    FORMAT(' MAXIMUM NUMBER OF SFR ITERATIONS = ',I5)
      WRITE(IOUT,28) CRNTSF
28    FORMAT(' COURANT CONSTRAINT FOR SFR TIME-STEP = ',G12.5)
      WRITE(IOUT,30) IPRTXMD
30    FORMAT(' PRINT OPTION FOR xMD SOLVER = ',G12.5)
C
C--CALL RARRAY TO READ IN INITIAL CONCENTRATIONS
      DO INDEX=1,NCOMP
        ANAME='STRM INIT CONC COMP#    '
        WRITE(ANAME(22:24),'(I3.3)') INDEX
        CALL RARRAY(BUFFSF(:,INDEX),ANAME,1,NSFINIT,0,INSFT,IOUT)
      ENDDO
      COLDSF=BUFFSF
      CNEWSF=COLDSF
C
C--CALL RARRAY TO READ DISPERSION COEFFICIENTS (L2/T)
      DO INDEX=1,NCOMP
        ANAME='DISP COEF L2/T COMP#    '
        WRITE(ANAME(22:24),'(I3.3)') INDEX
        CALL RARRAY(DISPSF(:,INDEX),ANAME,1,NSFINIT,0,INSFT,IOUT)
      ENDDO
C
C--READ LIST OF STREAM GAGES
      READ(INSFT,*) NOBSSF
      ALLOCATE(ISFNOBS(NOBSSF))
      DO I=1,NOBSSF
        READ(INSFT,*) ISFNOBS(I) 
      ENDDO
      IF(IOUTOBS.GT.0) THEN
        WRITE(IOUTOBS,*) ' STREAM OBSERVATION OUTPUT'
        WRITE(IOUTOBS,*) 
     & '   TIME      SFR-NODE  SFR-CONCENTRATION    FLOWGW',
     & '          GW-CONC'
      ENDIF
      IF(NOBSSF.GT.0 .AND. IOUTOBS.LE.0) THEN
        WRITE(IOUT,*) '***STREAM-FLOW OBSERVATION WILL NOT BE OUTPUT***'
        WRITE(IOUT,*) 'IOUTOBS IS NON-POSITIVE'
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE SFT1SS(KPER)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES SFR BOUNDARY CONDITIONS
C***********************************************************************
      USE SFRVARS
      USE MT3DMS_MODULE, ONLY: INSFT,IOUT,NCOMP
      CHARACTER*10 BCTYPSF
      INTEGER      KPER
C
      IN=INSFT
C
C--PRINT A HEADER
      WRITE(IOUT,1000)
 1000 FORMAT(//1X,'STREAM BOUNDARY CONDITIONS'/1X,26('-')/)
C
C--READ AND ECHO POINT SINKS/SOURCES OF SPECIFIED CONCENTRATIONS
      READ(IN,'(I10)') NTMP
C
C--BASIC CHECKS ON NTMP
      IF(KPER.EQ.1.AND.NTMP.LT.0) THEN
        WRITE(IOUT,*) 'NTMP<0 NOT ALLOWED FOR FIRST STRESS PERIOD'
        WRITE(*,*) 'NTMP<0 NOT ALLOWED FOR FIRST STRESS PERIOD'
        STOP
      ENDIF
C
C--RESET ARRAYS
      IF(NTMP.GE.0) THEN
        ISFNBC=0
        ISFBCTYP=0
        CBCSF=0.
      ENDIF
C
C
      IF(NTMP.GT.MXSFBC) THEN
        WRITE(*,30)
        CALL USTOP(' ')
      ELSEIF(NTMP.LT.0) THEN
        WRITE(IOUT,40)
        RETURN
      ELSEIF(NTMP.GE.0) THEN
        WRITE(IOUT,50) NTMP,KPER
        NSSSF=NTMP
        IF(NTMP.EQ.0) RETURN
      ENDIF
C
C--READ BOUNDARY CONDITIONS
      IBNDSF=1
      WRITE(IOUT,60)
      DO NUM=1,NTMP
        READ(IN,*) ISFNBC(NUM),ISFBCTYP(NUM),
     &             (CBCSF(NUM,INDEX),INDEX=1,NCOMP)
C
        IF(ISFBCTYP(NUM).EQ.0) THEN
          BCTYPSF=' HEADWATER'
        ELSEIF(ISFBCTYP(NUM).EQ.1) THEN
          BCTYPSF='    PRECIP'
        ELSEIF(ISFBCTYP(NUM).EQ.2) THEN
          BCTYPSF='    RUNOFF'
        ELSEIF(ISFBCTYP(NUM).EQ.3) THEN
          BCTYPSF='CNST. CONC'
        ELSEIF(ISFBCTYP(NUM).EQ.4) THEN
          BCTYPSF='   PUMPING'
        ELSEIF(ISFBCTYP(NUM).EQ.5) THEN
          BCTYPSF='      EVAP'
        ENDIF
C
        WRITE(IOUT,70) ISFNBC(NUM),BCTYPSF,
     &                 (CBCSF(NUM,INDEX),INDEX=1,NCOMP)
C
        IF(ISFBCTYP(NUM).LT.0.OR.ISFBCTYP(NUM).GT.3) THEN
          WRITE(IOUT,*) 'INVALID STREAM BC-TYPE'
          WRITE(*,*) 'INVALID STREAM BC-TYPE'
          STOP
        ENDIF
      ENDDO
C
   30 FORMAT(/1X,'ERROR: MAXIMUM NUMBER OF STREAM SINKS/SOURCES',
     &           ' EXCEEDED'/1X,'INCREASE [MXSFBC] IN SFT INPUT FILE')
   40 FORMAT(/1X,'STREAM SINKS/SOURCES OF SPECIFIED CONCENTRATION',
     &           ' REUSED FROM LAST STRESS PERIOD')
   50 FORMAT(/1X,'NO. OF STREAM SINKS/SOURCES OF SPECIFIED',
     &           ' CONCENTRATIONS =',I5,' IN STRESS PERIOD',I3)
   60 FORMAT(/5X,' SFTN    BC-TYPE       CONC(1,NCOMP)')
70    FORMAT(5X,I5,1X,A10,3X,1000(1X,G15.7))
C
      RETURN
      END
C
C
      SUBROUTINE SFT1FMGW(ICOMP)
C***********************************************************************
C     THIS SUBROUTINE FORMULATES SFT PACKAGE
C***********************************************************************
      USE MIN_SAT, ONLY: QC7,DRYON
      USE SFRVARS
      USE LAKVARS, ONLY : CNEWLAK
      USE MT3DMS_MODULE, ONLY: IOUT,NCOMP,UPDLHS,CNEW,A,RHS,
     &                         DTRANS,NLAY,NROW,NCOL,ICBUND,NODES,
     &                         MIXELM
      IMPLICIT  NONE
      INTEGER N,K,I,J,NN
      INTEGER ICOMP
      REAL    Q
      DOUBLE PRECISION CONC
C
C--FILL COEFFICIENT MATRIX A - WITH GW TO SFR TERMS
      DO N=1,NSTRM
        K=ISFL(N)
        I=ISFR(N)
        J=ISFC(N)
        NN=(K-1)*NCOL*NROW+(I-1)*NCOL+J
        Q=0.
        Q=QSFGW(N)    !(-)VE MEANS GW TO SFR; (+)VE MEANS SFR TO GW
        IF(ICBUND(J,I,K,ICOMP).LE.0) THEN
          IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              IF(Q.LT.0.) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-Q
              ELSE
                CONC=CNEWSF(N,ICOMP)
                QC7(J,I,K,7)=QC7(J,I,K,7)-Q*CONC
                QC7(J,I,K,8)=QC7(J,I,K,8)-Q
              ENDIF
            ENDIF
          ENDIF
        ELSE
          IF(Q.LT.0.) THEN
C.......CONSIDER ONLY FLOW INTO STREAM
            IF(UPDLHS) A(NN)=A(NN)+Q
C
C--FILL RHS WITH SFR TO GW TERMS USING CALCULATED SFR CONCS
          ELSE
C.......CONSIDER ONLY FLOW OUT OF STREAM
            CONC=CNEWSF(N,ICOMP)
            RHS(NN)=RHS(NN)-Q*CONC
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
C
C
      SUBROUTINE SFT1FM(ICOMP)
C***********************************************************************
C     THIS SUBROUTINE ASSEMBLES AND SOLVES MATRIX FOR SFR TRANSPORT
C***********************************************************************
      USE MIN_SAT,       ONLY: QC7,DRYON
      USE UZTVARS,       ONLY: CUZINF 
      USE MT3DMS_MODULE, ONLY: IOUT,NCOMP,CNEW,
     &                         DTRANS,NLAY,NROW,NCOL,ICBUND,NODES,
     &                         iSSTrans,iUnitTRNOP
      USE SFRVARS
      USE LAKVARS,       ONLY : CNEWLAK
      USE PKG2PKG
      USE XMDMODULE
      IMPLICIT NONE
      INTEGER ICOMP
      REAL    DELT,DELTMIN
      INTEGER K,I,J,II,JJ,N,NN,IS,IR,NC,ICNT,ISIN,IRIN,III,IFROM,ITO
      REAL    Q,COEFO,COEFN
      INTEGER numactive,KKITER,ITER,N_ITER,KITERSF,NSUBSTEPS,KSFSTP
      DOUBLE PRECISION ADV1,ADV2,ADV3,CONC,VOLN,VOLO,DV,VOLMIN,VOL
      INTEGER ICON,INOFLOW,IBC,NLK,NUZ
C
      DELT=DTRANS
      DELTMIN=DTRANS
C
      VOLMIN=1.0D-6
      RHSSF=0.
      AMATSF=0.
      COEFN=0.
      CONC=0.0D0
C
C.....INFLOW BOUNDARY CONDITIONS: PRECIP AND RUNOFF
      DO I=1,NSSSF
        N=ISFNBC(I)
        CONC=CBCSF(I,ICOMP)
        IF(ISFBCTYP(I).EQ.0) THEN
          DO II=1,NSF2SF
            CALL GETFLOWDIR(INOD1SF,INOD2SF,QN2NSF,NSF2SF,II,IFROM,ITO)
            IF(IFROM.EQ.-999.AND.N.EQ.ITO) THEN
              Q=ABS(QN2NSF(II))
              RHSSF(N)=RHSSF(N)-Q*CONC
              GOTO 105
            ENDIF
          ENDDO
105       CONTINUE
        ELSEIF(ISFBCTYP(I).EQ.1) THEN
          Q=QPRECSF(N)
            RHSSF(N)=RHSSF(N)-Q*CONC
        ELSEIF(ISFBCTYP(I).EQ.2) THEN
          Q=QRUNOFSF(N)
            RHSSF(N)=RHSSF(N)-Q*CONC
        ENDIF
      ENDDO
C
      ICNT=0
      DO N=1,NSTRM
        K=ISFL(N)
        I=ISFR(N)
        J=ISFC(N)
        II=IASF(N)
C
C.......VOLUME/TIME
        IF(iSSTrans.EQ.0) THEN
          CALL TIMEINTERP(VOLSFN,VOLSFO,VOLSFO,NSTRM,N,VOLN,VOLO,DV,1)
          IF(ISFTTR.EQ.0) THEN
            VOLN=VOLN/DELT
            IF(VOLN.LT.VOLMIN) VOLN=VOLMIN
            RHSSF(N)=RHSSF(N)-VOLN*COLDSF(N,ICOMP)
            AMATSF(II)=AMATSF(II)-VOLN
          ELSE
            VOLO=VOLO/DELT
            IF(VOLO.LT.VOLMIN) VOLO=VOLMIN
            VOLN=VOLN/DELT
            IF(VOLN.LT.VOLMIN) VOLN=VOLMIN
            RHSSF(N)=RHSSF(N)-VOLO*COLDSF(N,ICOMP)
            AMATSF(II)=AMATSF(II)-VOLN
          ENDIF
        ENDIF
C
C.......GW TO SFR
        Q=QSFGW(N)
        IF(Q.LT.0.0) THEN
          IF(ICBUND(J,I,K,ICOMP).NE.0) THEN
            RHSSF(N)=RHSSF(N)+Q*CNEW(J,I,K,ICOMP)
          ENDIF
        ELSE
C
C.......SFR TO GW
          AMATSF(II)=AMATSF(II)-Q 
        ENDIF
C
C.......FLOW OUT FROM EXIT
        IF(IEXIT(N).EQ.1) THEN
          Q=QOUTSF(N)
          AMATSF(II)=AMATSF(II)-Q 
        ENDIF
C
C.......EVAP
        IF(IETSFR.EQ.1) THEN
          Q=QETSF(N)
          AMATSF(II)=AMATSF(II)-Q 
        ENDIF
      ENDDO
C
C.......LAKE
      DO ICNT=1,NSFR2LAK
        N=INOD1SFLK(ICNT)
        NLK=INOD2SFLK(ICNT)
        Q=QSFR2LAK(ICNT)
        II=IASF(N)
        IF(Q.GT.0.) THEN
          AMATSF(II)=AMATSF(II)-Q 
        ELSE
          Q=ABS(Q)
          CONC=CNEWLAK(NLK,ICOMP)
          RHSSF(N)=RHSSF(N)-Q*CONC
        ENDIF
      ENDDO
C
C.....UZF
      IF(iUnitTRNOP(7).GT.0) THEN
        DO ICON=1,NSFR2UZF
          Q=QSFR2UZF(ICON)
          IF(Q.GT.0.) THEN
            AMATSF(II)=AMATSF(II)-Q !*WIMP
          ELSE
            Q=ABS(Q)
            N=INOD1SFUZ(ICON)
            NUZ=INOD2SFUZ(ICON)
            CALL NODE2KIJ(NUZ,NLAY,NROW,NCOL,K,I,J)
            IF(ICBUND(J,I,K,ICOMP).EQ.0) CYCLE
            IF(IUZCODESF(ICON).EQ.1) THEN
              CONC=CNEW(J,I,K,ICOMP)
            ELSEIF(IUZCODESF(ICON).EQ.2) THEN
              CONC=CUZINF(J,I,ICOMP)
            ELSEIF(IUZCODESF(ICON).EQ.3) THEN
              CONC=CUZINF(J,I,ICOMP)
            ELSE
              WRITE(IOUT,*) 'CHECK FTL FILE - IUZCODESF(ICON) INVALID'
              WRITE(*,*) 'CHECK FTL FILE - IUZCODESF(ICON) INVALID'
              STOP
            ENDIF
            RHSSF(N)=RHSSF(N)-Q*CONC
          ENDIF
        ENDDO
      ENDIF
C
C-----NODE-TO-NODE FLOW-----------------------------------------------
      DO NC=1,NSF2SF
        CALL GETFLOWDIR(INOD1SF,INOD2SF,QN2NSF,NSF2SF,NC,IFROM,ITO)
        N=ITO
        NN=IFROM
        IF(N.GT.0.AND.NN.GT.0) THEN
C...........INFLOW FROM STREAM
          Q=ABS(QN2NSF(NC))
          DO II=IASF(N)+1,IASF(N+1)-1
            IF(NN.EQ.JASF(II)) GOTO 110
          ENDDO
100       WRITE(*,*) 'ERROR IN JASF MATRIX'
          STOP
110       CONTINUE
          III=IASF(N)
          Q=Q/(SFLEN(NN)+SFLEN(N))
          ADV1=Q*(WUPS*SFLEN(NN)+SFLEN(N))
          ADV2=Q*(1.0D0-WUPS)*SFLEN(NN)
          AMATSF(II)=AMATSF(II)+ADV1*WIMP
          RHSSF(N)=RHSSF(N)-(1.0D0-WIMP)*ADV1*COLDSF(NN,ICOMP)
          AMATSF(III)=AMATSF(III)+ADV2*WIMP
          RHSSF(N)=RHSSF(N)-(1.0D0-WIMP)*ADV2*COLDSF(N,ICOMP)
C
          IF(IDSPFLG(NC).NE.0) THEN
C--DISPERSION
            !i=N=>AMATSF(III) and i-1=NN=>AMATSF(II) terms
C...........DISPERSION TERMS i-1,i
            COEFN=(DISPSF(N,ICOMP)*SFLEN(NN)
     &            +DISPSF(NN,ICOMP)*SFLEN(N))*SFNAREA(NC)/
     &            (SFLEN(NN)+SFLEN(N))
            COEFO=(DISPSF(N,ICOMP)*SFLEN(NN)
     &            +DISPSF(NN,ICOMP)*SFLEN(N))*SFOAREA(NC)/
     &            (SFLEN(NN)+SFLEN(N))
            III=IASF(N)
            AMATSF(III)=AMATSF(III) 
     &                  -COEFN*2.0D0*WIMP/(SFLEN(NN)+SFLEN(N))
            AMATSF(II)=AMATSF(II) 
     &                 +COEFN*2.0D0*WIMP/(SFLEN(NN)+SFLEN(N))
            RHSSF(N)=RHSSF(N)+COEFN*2.0D0*(1.0D0-WIMP)*
     &               COLDSF(N,ICOMP)/(SFLEN(NN)+SFLEN(N))
     &               -COEFN*2.0D0*(1.0D0-WIMP)*COLDSF(NN,ICOMP)/
     &               (SFLEN(NN)+SFLEN(N))
          ENDIF
C
C...........LOOK FOR i-1 ROW IN THE MATRIX
            DO II=IASF(NN)+1,IASF(NN+1)-1
              IF(N.EQ.JASF(II)) GOTO 210
            ENDDO
200         WRITE(*,*) 'ERROR IN JASF MATRIX'
            STOP
210         CONTINUE
C
C.............ADVECTION TERMS ON i,i+1
            III=IASF(NN)
            ADV2=Q*(WUPS*SFLEN(NN)+SFLEN(N))
            ADV3=Q*(1.0D0-WUPS)*SFLEN(NN)
            AMATSF(III)=AMATSF(III)-ADV2*WIMP
            RHSSF(NN)=RHSSF(NN)+(1.0D0-WIMP)*ADV2*COLDSF(NN,ICOMP)
            AMATSF(II)=AMATSF(II)-ADV3*WIMP
            RHSSF(NN)=RHSSF(NN)+(1.0D0-WIMP)*ADV3*COLDSF(N,ICOMP)
C
            !i=NN=>AMATSF(III) and i+1=N=>AMATSF(II) terms
            IF(IDSPFLG(NC).NE.0) THEN
C--DISPERSION
C...........DISPERSION TERMS i,i+1
            III=IASF(NN)
            AMATSF(III)=AMATSF(III) 
     &                  -COEFN*2.0D0*WIMP/(SFLEN(NN)+SFLEN(N))
            AMATSF(II)=AMATSF(II) 
     &                 +COEFN*2.0D0*WIMP/(SFLEN(NN)+SFLEN(N))
            RHSSF(NN)=RHSSF(NN)+COEFN*2.0D0*(1.0D0-WIMP)*
     &                COLDSF(NN,ICOMP)/(SFLEN(NN)+SFLEN(N))
     &                -COEFN*2.0D0*(1.0D0-WIMP)*COLDSF(N,ICOMP)/
     &                (SFLEN(NN)+SFLEN(N))
          ENDIF
        ENDIF
      ENDDO
C
C--CONSTANT CONCENTRATION BOUNDARY
      DO N=1,NSTRM
        IF(IBNDSF(N).EQ.-1) THEN
          DO I=1,NSSSF
            IBC=ISFNBC(I)
            IF(IBC.EQ.N.AND.ISFBCTYP(I).EQ.3) THEN
              CONC=CBCSF(I,ICOMP)
              EXIT
            ENDIF
          ENDDO
          IF(I.GT.NSSSF) THEN
            WRITE(IOUT,*) 'CONSTANT HEAD BOUNDARY ERROR IN SFT FILE'
            WRITE(*,*) 'CONSTANT HEAD BOUNDARY ERROR IN SFT FILE'
            STOP
          ENDIF
          !SET OFF-DIAGONAL TO ZEROES
          DO II=IASF(N)+1,IASF(N+1)-1
            AMATSF(II)=0.0D0
          ENDDO
          !DIAGONAL
          II=IASF(N)
          AMATSF(II)=-1.0D0
          RHSSF(N)=-CONC
        ENDIF
      ENDDO
C
C--HANDLE NO-FLOW CELLS
      DO N=1,NSTRM
        INOFLOW=1
        DO II=IASF(N),IASF(N+1)-1
          IF(ABS(AMATSF(II)).GT.1.0E-10) INOFLOW=0
        ENDDO
        IF(ABS(RHSSF(N)).GT.1.0E-10) INOFLOW=0
        IF(INOFLOW.EQ.1) THEN
          II=IASF(N)
          AMATSF(II)=-1.0D0
          RHSSF(N)=-CNEWSF(N,ICOMP)
        ENDIF
      ENDDO
C
      RETURN
      END
C
C
      SUBROUTINE SFT1AD(KSTP,KPER,N)
C***********************************************************************
C     RESET STREAM CONCENTRATIONS
C***********************************************************************
      USE SFRVARS
      INTEGER KSTP,KPER,N,NN
      DOUBLE PRECISION DZERO
      DZERO=0.0D0
C
C--SET INITIAL CONC = 0.0 IF VOLUME IS CLOSE TO ZERO
      IF(KSTP.EQ.1.AND.KPER.EQ.1.AND.N.EQ.1) THEN
        DO NN=1,NSTRM
          VOL=VOLSFN(NN)
          IF(ABS(VOL-DZERO).LE.1.0D-3) THEN
            CNEWSF(NN,:)=0.0D0
          ENDIF
        ENDDO
      ENDIF
C
C--RESET STREAM CONCENTRATION
      COLDSF=CNEWSF
      COLDSF2=CNEWSF
C
      RETURN
      END
C
C
      SUBROUTINE SFT1AD2(N)
C***********************************************************************
C     SAVE OLD STREAM FLOW PARAMETERS
C***********************************************************************
      USE SFRVARS
      INTEGER N
C
C--SAVE OLD FLOW PARAMETERS
      QPRECSFO=QPRECSF
      QRUNOFSFO=QRUNOFSF
      QOUTSFO=QOUTSF
      SFOAREA=SFNAREA
      VOLSFO=VOLSFN
C
      RETURN
      END
C
C
      SUBROUTINE SFT1BD(ICOMP,KPER,KSTP,DTRANS,NTRANS)
C***********************************************************************
C     THIS SUBROUTINE CALCULATES BUDGETS FOR STREAMS
C     THIS SUBROUTINE CALCULATES GROUNDWATER BUDGETS RELATED TO STREAMS
C     THIS SUBROUTINE WRITES STREAM CONCENTRATIONS AT OBSERVATION LOCATIONS
C***********************************************************************
      USE MIN_SAT, ONLY: QC7,DRYON
      USE LAKVARS
      USE SFRVARS
      USE UZTVARS,       ONLY: CUZINF 
      USE PKG2PKG
      USE MT3DMS_MODULE, ONLY: IOUT,NCOMP,UPDLHS,CNEW,TIME2,PRTOUT,
     &                         NLAY,NROW,NCOL,ICBUND,NODES,
     &                         MIXELM,INLKT,RMASIO,iUnitTRNOP,
     &                         iSSTrans
      IMPLICIT  NONE
      INTEGER IS,IR,ICON
      INTEGER ICOMP
      INTEGER K,I,J,N,NUM,II,ICCNODE
      INTEGER KPER,KSTP,NTRANS
      INTEGER ISIN,IRIN,ICNT,NC,NN,III,IFROM,ITO,NLK,NUZ
      REAL COEFN,COEFO,DTRANS,QX
      REAL Q,VO,CO,VOL,QC,Q1,Q2,DELV,QDIFF,ADV1,ADV2,ADV3
      REAL GW2SFR,GWFROMSFR,LAKFROMSFR,LAK2SFR,PRECSF,RUNOFSF,WDRLSF,
     &     ETSF,TOTINSF,TOTOUTSF,CTOTINSF,CTOTOUTSF,DIFF,CDIFF,PERC,
     &     CPERC,STORINSF,STOROTSF,TOTMASOLD,TOTMASNEW,STORDIFF,CCINSF,
     &     CCOUTSF,UZF2SFRGW,LOSTMASS,GAINEDMASS,UZF2SFRINF
      REAL FLOINSF,FLOOUTSF
      REAL QL
      DOUBLE PRECISION CONC,VOLN,VOLO,DV,VOLMIN
C
C--ZERO OUT TERMS
      CONC=0.
      Q=0.
      IF(iUnitTRNOP(18).GT.0) THEN
        RMASLAK=0.
        VOUTLAK=0.
      ENDIF
C
      VOLMIN=1.0D-6
      FLOINSF=0.
      FLOOUTSF=0.
      GW2SFR=0.
      UZF2SFRGW=0.
      UZF2SFRINF=0.
      GWFROMSFR=0.
      LAKFROMSFR=0.
      LAK2SFR=0.
      PRECSF=0.
      RUNOFSF=0.
      WDRLSF=0.
      ETSF=0.
      STORINSF=0.
      STOROTSF=0.
      LOSTMASS=0.
      GAINEDMASS=0.
      CCINSF=0.
      CCOUTSF=0.
      TOTINSF=0.
      TOTOUTSF=0.
      CTOTINSF=0.
      CTOTOUTSF=0.
      Q1=0.
      Q2=0.
      QX=0.
      COEFN=0.
      DELV=0.
C
C--WRITE HEADER TO ICBCSF FILE
      IF(KPER.EQ.1 .AND. KSTP.EQ.1.AND.NTRANS.EQ.1.AND.NOBSSF.GT.0)THEN
      ENDIF
C
      IF(ISFRBC.NE.1) THEN
C
C-- CALCULATE INFLOW, STORAGE, AND OUTFLOW TERMS
C
C.....INFLOW BOUNDARY CONDITIONS: PRECIP AND RUNOFF
        DO I=1,NSSSF
          N=ISFNBC(I)
          CONC=CBCSF(I,ICOMP)
C.......NO BOUNDARY ALLOWED ON CONST. CONC
          IF(IBNDSF(N).EQ.-1) THEN
            CYCLE
          ENDIF
C        
          IF(ISFBCTYP(I).EQ.3) THEN
            !BCTYPSF='CNST. CONC', SKIP IF CONST. CONC
            CONTINUE  
          ELSEIF(ISFBCTYP(I).EQ.0) THEN !BCTYPSF=' HEADWATER'    
            DO II=1,NSF2SF
              CALL GETFLOWDIR(INOD1SF,INOD2SF,QN2NSF,NSF2SF,II,IFROM,
     &                        ITO)
              IF(IFROM.EQ.-999.AND.N.EQ.ITO) THEN
                Q=ABS(QN2NSF(II))
                FLOINSF=FLOINSF+Q*CONC*DTRANS
              ENDIF
            ENDDO
          ELSEIF(ISFBCTYP(I).EQ.1) THEN !BCTYPSF='    PRECIP'
            Q=QPRECSF(N)
            PRECSF=PRECSF+Q*CONC*DTRANS
          ELSEIF(ISFBCTYP(I).EQ.2) THEN !BCTYPSF='    RUNOFF'
            Q=QRUNOFSF(N)
            RUNOFSF=RUNOFSF+Q*CONC*DTRANS
          ENDIF
        ENDDO
C
C--CALCULATE Q1 FOR BOUNDARY CONDITIONS
        DO N=1,NSTRM
          Q=QPRECSF(N)
          Q1=Q1+Q*DTRANS
          Q=QRUNOFSF(N)
          Q1=Q1+Q*DTRANS
        ENDDO
C      
        DO N=1,NSTRM
          K=ISFL(N)
          I=ISFR(N)
          J=ISFC(N)
C
C.......VOLUME
        IF(iSSTrans.EQ.0) THEN
          CALL TIMEINTERP(VOLSFN,VOLSFO,VOLSFO,NSTRM,N,VOLN,VOLO,DV,1)
          IF(VOLO.LT.VOLMIN) VOLO=VOLMIN
          IF(VOLN.LT.VOLMIN) VOLN=VOLMIN
          DELV=DELV+VOLN-VOLO
          STORDIFF=VOLN*CNEWSF(N,ICOMP)-VOLO*COLDSF(N,ICOMP)
          IF(STORDIFF.LT.0) THEN
            STORINSF=STORINSF-STORDIFF
          ELSE
            STOROTSF=STOROTSF+STORDIFF
          ENDIF
C.......CALCULATE MASS CREATED/DESTROYED IF ISFTTR.EQ.0
          IF(ISFTTR.EQ.0) THEN
            IF((VOLN-VOLO).LT.0.0) THEN
              LOSTMASS=LOSTMASS+ABS((VOLN-VOLO))*COLDSF(N,ICOMP)
            ELSE
              GAINEDMASS=GAINEDMASS+ABS((VOLN-VOLO))*COLDSF(N,ICOMP)
            ENDIF
          ENDIF
        ENDIF
C
C.......FLOW OUT FROM EXIT
          IF(IEXIT(N).EQ.1) THEN
            Q=QOUTSF(N)
            Q2=Q2+ABS(Q)*DTRANS
            IF(IBNDSF(N).EQ.-1) THEN
              CCOUTSF=CCOUTSF+Q*CNEWSF(N,ICOMP)*DTRANS
            ELSE
              FLOOUTSF=FLOOUTSF+Q*CNEWSF(N,ICOMP)*DTRANS
            ENDIF
          ENDIF
C
C.......EVAP
          IF(IETSFR.EQ.1) THEN
            Q=QETSF(N)
            ETSF=ETSF+QETSF(N)*CNEWSF(N,ICOMP)*DTRANS
            Q2=Q2+ABS(Q)*DTRANS
          ELSE
            Q=QETSF(N)
            Q2=Q2+ABS(Q)*DTRANS
          ENDIF
        ENDDO
C
C.......ADVECTION, DISPERSION
        DO NC=1,NSF2SF
          CALL GETFLOWDIR(INOD1SF,INOD2SF,QN2NSF,NSF2SF,NC,IFROM,ITO)
          N=ITO
          NN=IFROM
C
C.......INFLOW FROM HEADWATER
          IF(N.GT.0.AND.NN.LT.0) THEN
            Q=ABS(QN2NSF(NC))
            Q1=Q1+Q*DTRANS
C
C.......INFLOW FROM UPSTREAM REACHES
          ELSEIF(N.GT.0.AND.NN.GT.0) THEN
C...........INFLOW FROM STREAM
            Q=ABS(QN2NSF(NC))
            DO II=IASF(N)+1,IASF(N+1)-1
              IF(NN.EQ.JASF(II)) GOTO 110
            ENDDO
100         WRITE(*,*) 'ERROR IN JASF MATRIX'
            STOP
110         CONTINUE
C...........INFLOW FROM STREAM
            IF(IBNDSF(NN).EQ.-1) THEN
              QX=Q/(SFLEN(NN)+SFLEN(N))
              ADV1=QX*(WUPS*SFLEN(NN)+SFLEN(N))
              ADV2=QX*(1.0D0-WUPS)*SFLEN(NN)
              CCINSF=CCINSF+ADV1*WIMP*CNEWSF(NN,ICOMP)*DTRANS
     &               +ADV1*(1.0D0-WIMP)*COLDSF(NN,ICOMP)*DTRANS
     &               +ADV2*WIMP*CNEWSF(N,ICOMP)*DTRANS
     &               +ADV2*(1.0D0-WIMP)*COLDSF(N,ICOMP)*DTRANS
            ENDIF
C
C--DISPERSION
            IF(IDSPFLG(NC).NE.0) THEN
          !i=N and i-1=NN terms
C...........DISPERSION TERMS i-1,i
              COEFN=(DISPSF(N,ICOMP)*SFLEN(NN)
     &              +DISPSF(NN,ICOMP)*SFLEN(N))*SFNAREA(NC)/
     &              (SFLEN(NN)+SFLEN(N))
              COEFO=(DISPSF(N,ICOMP)*SFLEN(NN)
     &              +DISPSF(NN,ICOMP)*SFLEN(N))*SFOAREA(NC)/
     &              (SFLEN(NN)+SFLEN(N))
              III=IASF(N)
              IF(IBNDSF(NN).EQ.-1) THEN
                CCINSF=CCINSF+COEFN*2.0D0*DTRANS*(WIMP*CNEWSF(NN,ICOMP)
     &                 +(1.0D0-WIMP)*COLDSF(NN,ICOMP))
     &                 /(SFLEN(NN)+SFLEN(N))
                CCINSF=CCINSF+COEFN*2.0D0*DTRANS*(-WIMP*CNEWSF(N,ICOMP)
     &                 -(1.0D0-WIMP)*COLDSF(N,ICOMP))
     &                 /(SFLEN(NN)+SFLEN(N))
              ENDIF
            ENDIF
C
C...........LOOK FOR i-1 ROW IN THE MATRIX
            DO II=IASF(NN)+1,IASF(NN+1)-1
              IF(N.EQ.JASF(II)) GOTO 210
            ENDDO
200         WRITE(*,*) 'ERROR IN JASF MATRIX'
            STOP
210         CONTINUE
C
C.............ADVECTION TERMS ON i,i+1
            IF(IBNDSF(N).EQ.-1) THEN
              ADV2=QX*(WUPS*SFLEN(NN)+SFLEN(N))
              ADV3=QX*(1.0D0-WUPS)*SFLEN(NN)
              CCOUTSF=CCOUTSF+ADV2*WIMP*CNEWSF(NN,ICOMP)*DTRANS
     &                +(1.0D0-WIMP)*ADV2*COLDSF(NN,ICOMP)*DTRANS
     &                +ADV3*WIMP*CNEWSF(N,ICOMP)*DTRANS
     &                +(1.0D0-WIMP)*ADV3*COLDSF(N,ICOMP)*DTRANS
            ENDIF
C         
            IF(IDSPFLG(NC).NE.0) THEN
            !i=NN and i+1=N terms
C...........DISPERSION TERMS i,i+1
              III=IASF(NN)
              IF(IBNDSF(N).EQ.-1) THEN
                CCOUTSF=CCOUTSF+COEFN*2.0D0*DTRANS*
     &                  (WIMP*CNEWSF(NN,ICOMP)+(1.0D0-WIMP)*
     &                  COLDSF(NN,ICOMP))/(SFLEN(NN)+SFLEN(N))
                CCOUTSF=CCOUTSF+COEFN*2.0D0*DTRANS*
     &                  (-WIMP*CNEWSF(N,ICOMP)-(1.0D0-WIMP)*
     &                  COLDSF(N,ICOMP))/(SFLEN(NN)+SFLEN(N))
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C.......INFLOW/OUTFLOW GW
      DO N=1,NSTRM
        K=ISFL(N)
        I=ISFR(N)
        J=ISFC(N)
        Q=QSFGW(N)
        IF(ICBUND(J,I,K,ICOMP).LE.0) THEN
          IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              IF(Q.LT.0.) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-Q
              ELSE
                Q2=Q2+ABS(Q)*DTRANS
                CONC=CNEWSF(N,ICOMP)
                IF(IBNDSF(N).EQ.-1) THEN
                  CONTINUE
                ELSE
                  GWFROMSFR=GWFROMSFR+Q*CONC*DTRANS
                ENDIF
                RMASIO(30,1,ICOMP)=RMASIO(30,1,ICOMP)+Q*CONC*DTRANS
                QC7(J,I,K,7)=QC7(J,I,K,7)-Q*CONC
                QC7(J,I,K,8)=QC7(J,I,K,8)-Q
              ENDIF
            ENDIF
          ENDIF
        ELSE
          IF(Q.LT.0.0) THEN
            Q1=Q1+ABS(Q)*DTRANS
            CONC=CNEW(J,I,K,ICOMP)
            IF(IBNDSF(N).EQ.-1) THEN
              CONTINUE
            ELSE
              GW2SFR=GW2SFR-Q*CONC*DTRANS
            ENDIF
            RMASIO(30,2,ICOMP)=RMASIO(30,2,ICOMP)+Q*CONC*DTRANS
          ELSE
            Q2=Q2+ABS(Q)*DTRANS
            CONC=CNEWSF(N,ICOMP)
            IF(IBNDSF(N).EQ.-1) THEN
              CONTINUE
            ELSE
              GWFROMSFR=GWFROMSFR+Q*CONC*DTRANS
            ENDIF
            RMASIO(30,1,ICOMP)=RMASIO(30,1,ICOMP)+Q*CONC*DTRANS
          ENDIF
        ENDIF
      ENDDO
C
C.....LAK
      IF(ISFRBC.NE.1) THEN
        DO ICNT=1,NSFR2LAK
          N=INOD1SFLK(ICNT)
          NLK=INOD2SFLK(ICNT)
          Q=QSFR2LAK(ICNT)
          IF(Q.GT.0.) THEN
            Q2=Q2+ABS(Q)*DTRANS
            LAKFROMSFR=LAKFROMSFR+Q*CNEWSF(N,ICOMP)*DTRANS
          ELSE
            Q=ABS(Q)
            Q1=Q1+ABS(Q)*DTRANS
            CONC=CNEWLAK(NLK,ICOMP)
            IF(IBNDSF(N).EQ.-1) THEN
            ELSE
              LAK2SFR=LAK2SFR+Q*CONC*DTRANS
            ENDIF
          ENDIF
        ENDDO
C
C.....UZF
        IF(iUnitTRNOP(7).GT.0) THEN
          DO ICON=1,NSFR2UZF
            Q=QSFR2UZF(ICON)
            IF(Q.GT.0.) THEN
              CONTINUE
            ELSE
              Q=ABS(Q)
              N=INOD1SFUZ(ICON)
              NUZ=INOD2SFUZ(ICON)
              CALL NODE2KIJ(NUZ,NLAY,NROW,NCOL,K,I,J)
              IF(ICBUND(J,I,K,ICOMP).EQ.0) CYCLE
              IF(IUZCODESF(ICON).EQ.1) THEN
                CONC=CNEW(J,I,K,ICOMP)
              ELSEIF(IUZCODESF(ICON).EQ.2) THEN
                CONC=CUZINF(J,I,ICOMP)
              ELSEIF(IUZCODESF(ICON).EQ.3) THEN
                CONC=CUZINF(J,I,ICOMP)
              ELSE
                WRITE(IOUT,*) 'CHECK FTL FILE - IUZCODESF(ICON) INVALID'
                WRITE(*,*) 'CHECK FTL FILE - IUZCODESF(ICON) INVALID'
                STOP
              ENDIF
              Q1=Q1+ABS(Q)*DTRANS
              IF(IBNDSF(N).EQ.-1) THEN
                CONTINUE
              ELSE
                IF(IUZCODESF(ICON).EQ.1) THEN
                  UZF2SFRGW=UZF2SFRGW+Q*CONC*DTRANS
                ELSEIF(IUZCODESF(ICON).EQ.2 .OR.
     1                 IUZCODESF(ICON).EQ.3) THEN
                  UZF2SFRINF=UZF2SFRINF+Q*CONC*DTRANS
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
C
C--CUMULATIVE TERMS
        CFLOINSF(ICOMP)=   CFLOINSF(ICOMP)+FLOINSF
        CFLOOUTSF(ICOMP)=  CFLOOUTSF(ICOMP)+FLOOUTSF
        CGW2SFR(ICOMP)=    CGW2SFR(ICOMP)+GW2SFR
        CUZF2SFRGW(ICOMP)=   CUZF2SFRGW(ICOMP)+UZF2SFRGW
        CUZF2SFRINF(ICOMP)=   CUZF2SFRINF(ICOMP)+UZF2SFRINF
        CGWFROMSFR(ICOMP)= CGWFROMSFR(ICOMP)+GWFROMSFR
        CLAKFROMSFR(ICOMP)=CLAKFROMSFR(ICOMP)+LAKFROMSFR
        CLAK2SFR(ICOMP)=   CLAK2SFR(ICOMP)+LAK2SFR
        CPRECSF(ICOMP)=    CPRECSF(ICOMP)+PRECSF
        CRUNOFSF(ICOMP)=   CRUNOFSF(ICOMP)+RUNOFSF
        CETSF(ICOMP)=      CETSF(ICOMP)+ETSF
        CSTORINSF(ICOMP)=  CSTORINSF(ICOMP)+STORINSF
        CSTOROTSF(ICOMP)=  CSTOROTSF(ICOMP)+STOROTSF
        CCCOUTSF(ICOMP)=   CCCOUTSF(ICOMP)+CCOUTSF
        CCCINSF(ICOMP)=    CCCINSF(ICOMP)+CCINSF
        CLOSTMASS(ICOMP)=  CLOSTMASS(ICOMP)+LOSTMASS
        CGAINEDMASS(ICOMP)=CGAINEDMASS(ICOMP)+GAINEDMASS
C
C--CALCULATE TOTAL
        TOTINSF=GW2SFR+LAK2SFR+PRECSF+RUNOFSF+STORINSF+FLOINSF+CCINSF+
     &          UZF2SFRGW+GAINEDMASS+UZF2SFRINF
        TOTOUTSF=GWFROMSFR+LAKFROMSFR+ETSF+STOROTSF+FLOOUTSF+CCOUTSF+
     &           LOSTMASS
        CTOTINSF=CGW2SFR(ICOMP)+CLAK2SFR(ICOMP)+CPRECSF(ICOMP)+
     &           CRUNOFSF(ICOMP)+CSTORINSF(ICOMP)+CFLOINSF(ICOMP)+
     &           CCCINSF(ICOMP)+CUZF2SFRGW(ICOMP)+CGAINEDMASS(ICOMP)+
     &           CUZF2SFRINF(ICOMP)
        CTOTOUTSF=CGWFROMSFR(ICOMP)+CLAKFROMSFR(ICOMP)+CETSF(ICOMP)+
     &            CSTOROTSF(ICOMP)+CFLOOUTSF(ICOMP)+CCCOUTSF(ICOMP)+
     &            CLOSTMASS(ICOMP)
C
        DIFF=TOTINSF-TOTOUTSF
        CDIFF=CTOTINSF-CTOTOUTSF
        IF(ABS(TOTINSF+TOTOUTSF).LE.1.0E-10) TOTINSF=1.0E-10
        PERC=DIFF*100/((TOTINSF+TOTOUTSF)/2.0E0)
        IF(ABS(CTOTINSF+CTOTOUTSF).LE.1.0E-10) CTOTINSF=1.0E-10
        CPERC=CDIFF*100/((CTOTINSF+CTOTOUTSF)/2.0E0)
C
C--FLOW BALANCE TERM
        QDIFF=Q1-Q2-DELV
C
C--WRITE SFR MASS BALANCE TO OUTPUT FILE
        IF(PRTOUT) THEN
          WRITE(IOUT,10) NTRANS,KSTP,KPER,ICOMP
          WRITE(IOUT,20) 
          WRITE(IOUT,30) CSTORINSF(ICOMP),STORINSF
          WRITE(IOUT,35) CFLOINSF(ICOMP),FLOINSF
          WRITE(IOUT,40) CGW2SFR(ICOMP),GW2SFR
          IF(iUnitTRNOP(7).GT.0) THEN
            WRITE(IOUT,41) CUZF2SFRGW(ICOMP),UZF2SFRGW
            WRITE(IOUT,42) CUZF2SFRINF(ICOMP),UZF2SFRINF
          ENDIF
          IF(iUnitTRNOP(18).GT.0) WRITE(IOUT,45) CLAK2SFR(ICOMP),LAK2SFR
          WRITE(IOUT,50) CPRECSF(ICOMP),PRECSF
          WRITE(IOUT,55) CRUNOFSF(ICOMP),RUNOFSF
          WRITE(IOUT,56) CCCINSF(ICOMP),CCINSF
          IF(ISFTTR.EQ.0) WRITE(IOUT,57) CGAINEDMASS(ICOMP),GAINEDMASS
          WRITE(IOUT,60)
          WRITE(IOUT,65) CTOTINSF,TOTINSF
          WRITE(IOUT,70) CSTOROTSF(ICOMP),STOROTSF
          WRITE(IOUT,75) CFLOOUTSF(ICOMP),FLOOUTSF
          WRITE(IOUT,80) CGWFROMSFR(ICOMP),GWFROMSFR
          IF(iUnitTRNOP(18).GT.0) 
     &    WRITE(IOUT,85) CLAKFROMSFR(ICOMP),LAKFROMSFR
          WRITE(IOUT,56) CCCOUTSF(ICOMP),CCOUTSF
          IF(IETSFR.GT.0) WRITE(IOUT,90) CETSF(ICOMP),ETSF
          IF(ISFTTR.EQ.0) WRITE(IOUT,87) CLOSTMASS(ICOMP),LOSTMASS
          WRITE(IOUT,60)
          WRITE(IOUT,95) CTOTOUTSF,TOTOUTSF
          WRITE(IOUT,97) CDIFF,DIFF
          WRITE(IOUT,98) CPERC,PERC
          WRITE(IOUT,99) QDIFF
        ENDIF
10      FORMAT(//21X,'STREAM MASS BUDGETS AT END OF TRANSPORT STEP',
     &     I5,', TIME STEP',I5,', STRESS PERIOD',I5,' FOR COMPONENT',I4,
     &          /21X,103('-'))
20      FORMAT(/33X,7X,1X,'CUMULATIVE MASS [M]',
     &           8X,13X,15X,' MASS FOR THIS TIME STEP [M]',
     &         /41X,19('-'),36X,14('-'))
30      FORMAT(16X,'      STREAM DEPLETION =',G15.7E3,
     &         16X,'      STREAM DEPLETION =',G15.7E3)
35      FORMAT(16X,'      INFLOW TO STREAM =',G15.7E3,
     &         16X,'      INFLOW TO STREAM =',G15.7E3)
40      FORMAT(16X,'          GW TO STREAM =',G15.7E3,
     &         16X,'          GW TO STREAM =',G15.7E3)
41      FORMAT(16X,'      UZF GW DISCHARGE =',G15.7E3,
     &         16X,'      UZF GW DISCHARGE =',G15.7E3)
42      FORMAT(16X,' UZF REJECTED RECHARGE =',G15.7E3,
     &         16X,' UZF REJECTED RECHARGE =',G15.7E3)
45      FORMAT(16X,'         LAK TO STREAM =',G15.7E3,
     &         16X,'         LAK TO STREAM =',G15.7E3)
50      FORMAT(16X,'         PRECIPITATION =',G15.7E3,
     &         16X,'         PRECIPITATION =',G15.7E3)
55      FORMAT(16X,'                RUNOFF =',G15.7E3,
     &         16X,'                RUNOFF =',G15.7E3)
56      FORMAT(16X,'CONSTANT CONCENTRATION =',G15.7E3,
     &         16X,'CONSTANT CONCENTRATION =',G15.7E3)
57      FORMAT(16X,'             MASS GAIN =',G15.7E3,
     &         16X,'             MASS GAIN =',G15.7E3)
60      FORMAT(41X,19('-'),36X,14('-'))
65      FORMAT(16X,'              TOTAL IN =',G15.7E3,
     &         16X,'              TOTAL IN =',G15.7E3)
70      FORMAT(/16X,'   STREAM ACCUMULATION =',G15.7E3,
     &         16X,'   STREAM ACCUMULATION =',G15.7E3)
75      FORMAT(16X,'        STREAM OUTFLOW =',G15.7E3,
     &         16X,'        STREAM OUTFLOW =',G15.7E3)
80      FORMAT(16X,'          STREAM TO GW =',G15.7E3,
     &         16X,'          STREAM TO GW =',G15.7E3)
85      FORMAT(16X,'         STREAM TO LAK =',G15.7E3,
     &         16X,'         STREAM TO LAK =',G15.7E3)
90      FORMAT(16X,'           EVAPORATION =',G15.7E3,
     &         16X,'           EVAPORATION =',G15.7E3)
87      FORMAT(16X,'             MASS LOSS =',G15.7E3,
     &         16X,'             MASS LOSS =',G15.7E3)
95      FORMAT(16X,'             TOTAL OUT =',G15.7E3,
     &         16X,'             TOTAL OUT =',G15.7E3)
97      FORMAT(/16X,'        NET (IN - OUT) =',G15.7E3,
     &          16X,'        NET (IN - OUT) =',G15.7E3)
98      FORMAT(16X,' DISCREPANCY (PERCENT) =',G15.7E3,
     &         16X,' DISCREPANCY (PERCENT) =',G15.7E3)
99      FORMAT(46X,'FLOW ERR (QIN-QOUT-DV) =',G15.7E3,' [L3/T]',/)
C       
      ENDIF !ISFRBC
C
C--WRITE OBSERVATIONS
      DO II=1,NOBSSF
        N=ISFNOBS(II)
        CONC=CNEWSF(N,ICOMP)
C
        K=ISFL(N)
        I=ISFR(N)
        J=ISFC(N)
        Q=QSFGW(N)
C
        WRITE(IOUTOBS,220) TIME2,N,CONC,Q,CNEW(J,I,K,ICOMP)
      ENDDO
220   FORMAT(1X,G15.7E3,2X,I5,100(5X,G15.7E3))
C
      RETURN
      END
C
C
      SUBROUTINE TIMEINTERP(VAR2,VAR1,DVAR,NVAR,N,VARTN,VARTO,DV,IOPT)
C
C  LINEAR INTERPOLATION FOR BEGINNING AND END OF TRANSPORT TIME-STEP
C  VAR2     - VARIABLE AT THE END OF FLOW TIME STEP - L3
C  VAR1     - VARIABLE AT THE BEGINNING OF FLOW TIME STEP - L3
C  DVAR     - RATE OF CHANGE - L3/T
C  VARTN    - VARIABLE AT THE END OF TRANSPORT TIME STEP - L3
C  VARTO    - VARIABLE AT THE BEGINNING OF TRANSPORT TIME STEP -L3
C  DV       - DV - L3/T
C  IOPT     - CALCULATION OPTION
C               - 1 MEANS VAR1 AND VAR2 ARE GIVEN; VARTN,VARTO,DV ARE CALCULATED
C               - 2 MEANS VAR1 AND DVAR ARE GIVEN; VARTN,VARTO ARE CALCULATED
C
      USE MT3DMS_MODULE, ONLY: HT1,HT2,TIME1,TIME2
      DOUBLE PRECISION VAR1(NVAR),VAR2(NVAR),DVAR(NVAR)
      DOUBLE PRECISION VARTN,VARTO,DV
      INTEGER          IOPT
C
      IF(IOPT.EQ.1) THEN
        DV=(VAR2(N)-VAR1(N))/(HT2-HT1)
        VARTO=VAR1(N)+DV*(TIME1-HT1)
        VARTN=VAR1(N)+DV*(TIME2-HT1)
      ELSEIF(IOPT.EQ.2) THEN
        DV=DVAR(N)
        VARTO=VAR1(N)+DV*(TIME1-HT1)
        VARTN=VAR1(N)+DV*(TIME2-HT1)
      ENDIF
C
      RETURN
      END
