      MODULE SFRVARS
        INTEGER NSTRM,MXSFBC,NSSSF
        INTEGER NSFINIT,ICBCSF,IETSFR,ISFSOLV
        INTEGER MXITERSF
        REAL WIMP,CCLOSESF
        INTEGER, ALLOCATABLE :: ISFL(:),ISFR(:),ISFC(:),ISEG(:),IRCH(:),
     1  NIN(:)
        REAL, ALLOCATABLE :: SFLEN(:),SFNAREA(:),SFOAREA(:)
        REAL, ALLOCATABLE :: QPRECSF(:),QRUNOFSF(:)
        REAL, ALLOCATABLE :: QPRECSFO(:),QRUNOFSFO(:)
        REAL, ALLOCATABLE :: QSFGW(:),QOUTSF(:),QETSF(:),QOUTSFO(:)
C
        REAL, ALLOCATABLE :: CNEWSF(:,:),COLDSF(:,:),COLDSF2(:,:)
        REAL, ALLOCATABLE :: CNEWSFTMP(:,:)
        REAL, ALLOCATABLE :: CBCSF(:,:)
        INTEGER, ALLOCATABLE :: ISEGBC(:),IRCHBC(:),ISFBCTYP(:)
        REAL, ALLOCATABLE :: RMASSF(:),VOUTSF(:)
C
        INTEGER MXSGMT,MXRCH
        INTEGER, ALLOCATABLE :: ISTRM(:,:) !ISTRM(IRCH,ISEG)
C
C.......OBSERVATION VARIABLES
        INTEGER NOBSSF,IOUTOBS
        INTEGER, ALLOCATABLE :: ISOBS(:),IROBS(:)
C
C.......SIZE NINTOT
        INTEGER NINTOT
        INTEGER, ALLOCATABLE :: IDXNIN(:) !IDXNIN(NSTRM+1)
        REAL, ALLOCATABLE :: QINSF(:),QINSFO(:)
        INTEGER, ALLOCATABLE :: INSEG(:),INRCH(:),IDSPFLG(:)
C
C.......SOLVER VARIABLES
        INTEGER, ALLOCATABLE :: IASF(:),JASF(:)
        REAL, ALLOCATABLE :: AMATSF(:),RHSSF(:)
        INTEGER NJASF
        REAL CRNTSF
      END MODULE SFRVARS
C
C
      SUBROUTINE SFT5AL(INSFT,IOUT,NCOMP)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES SPACE FOR SFR VARIABLES
C***********************************************************************
      USE SFRVARS
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INSFT
 1030 FORMAT(1X,'SFT -- STREAM TRANSPORT PACKAGE,',
     & ' VERSION 1, AUGUST 2012, INPUT READ FROM UNIT',I3)
C
C--READ NUMBER OF STREAMS
      READ(INSFT,*) NSFINIT,MXSFBC,ICBCSF,IETSFR
      WRITE(IOUT,10) NSFINIT,MXSFBC
10    FORMAT(1X,'NUMBER OF STREAMS = ',I5,
     &      /1X,'MAXIMUM NUMBER OF STREAM BOUNDARY CONDITIONS = ',I5)
      IF(ICBCSF.GT.0) WRITE(IOUT,12) ICBCSF
12    FORMAT(1X,'RCH-BY-RCH INFORMATION WILL BE PRINTED ON UNIT ',I5)
      IF(IETSFR.EQ.0) THEN
        WRITE(IOUT,14)
      ELSE
        WRITE(IOUT,16)
      ENDIF
14    FORMAT(1X,'MASS DOES NOT EXIT VIA STREAM ET')
16    FORMAT(1X,'MASS IS ALLOWED TO EXIT VIA STREAM ET')
C
C--ALLOCATE INITIAL AND BOUNDARY CONDITION ARRAYS
      ALLOCATE(CNEWSF(NSFINIT,NCOMP),COLDSF(NSFINIT,NCOMP),
     1  COLDSF2(NSFINIT,NCOMP),CNEWSFTMP(NSFINIT,NCOMP))
      ALLOCATE(ISEGBC(MXSFBC),IRCHBC(MXSFBC),ISFBCTYP(MXSFBC))
      ALLOCATE(CBCSF(MXSFBC,NCOMP))
      CBCSF=0.
      ALLOCATE(RMASSF(NLKINIT),VOUTSF(NLKINIT))
      RMASSF=0.
C
C--CUMULATIVE BUDGET TERMS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE SFT5RP(INSFT,IOUT,NCOMP)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES READS LAK VARIABLES - INITIAL CONCS
C***********************************************************************
      USE SFRVARS
      CHARACTER ANAME*24
C
C--PRINT A HEADER
      WRITE(IOUT,1000)
 1000 FORMAT(//1X,'STREAM INPUT PARAMETERS'/1X,23('-')/)
C
C--STREAM SOLVER SETTINGS
      READ(INSFT,*) ISFSOLV,WIMP,CCLOSESF,MXITERSF,CRNTSF
      WRITE(IOUT,20) ISFSOLV
20    FORMAT(' STREAM SOLVER OPTION = ',I5,
     1     /,'   1 = MASS BALANCE APPROACH',
     1     /,'   2 = FINITE DIFFERENCE CENTRAL DIFFERENCING')
      WRITE(IOUT,22) WIMP
22    FORMAT(' STREAM SOLVER TIME WEIGHTING FACTOR = ',G12.5,
     1     /,'   0.0 = EXPLICIT SCHEME IS USED',
     1     /,'   0.5 = CRANK-NICOLSON SCHEME IS USED',
     1     /,'   1.0 = FULLY IMPLICIT SCHEME IS USED')
      WRITE(IOUT,24) CCLOSESF
24    FORMAT(' CLOSURE CRITERION FOR SFT SOLVER = ',G12.5)
      WRITE(IOUT,26) MXITERSF
26    FORMAT(' MAXIMUM NUMBER OF SFR ITERATIONS = ',I5)
      WRITE(IOUT,28) CRNTSF
28    FORMAT(' COURANT CONSTRAINT FOR SFR TIME-STEP = ',I5)
C
C--CALL RARRAY TO READ IN CELL WIDTH ALONG ROWS
      DO INDEX=1,NCOMP
        ANAME='STRM INIT CONC COMP#    '
        WRITE(ANAME(22:24),'(I3.3)') INDEX
        CALL RARRAY(COLDSF(:,INDEX),ANAME,1,NSFINIT,0,INSFT,IOUT)
      ENDDO
      CNEWSF=COLDSF
C
C--READ LIST OF STREAM GAGES
      READ(INSFT,*) NOBSSF
      ALLOCATE(ISOBS(NOBSSF),IROBS(NOBSSF))
      DO I=1,NOBSSF
        READ(INSFT,*) ISOBS(I),IROBS(I)
      ENDDO
      IOUTOBS=123
      WRITE(IOUTOBS,*) ' TIME SEG RCH CONC '
C
      RETURN
      END
C
C
      SUBROUTINE SFT5SS(INSFT,IOUT,NCOMP,KPER)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES SFR BOUNDARY CONDITIONS
C***********************************************************************
      USE SFRVARS
      CHARACTER*10 BCTYPSF
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
      IF(NTMP.EQ.0) THEN
        RETURN
      ENDIF
C
C--RESET ARRAYS
      IF(NTMP.GE.0) THEN
        ISEGBC=0
        IRCHBC=0
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
      WRITE(IOUT,60)
      DO NUM=1,NTMP
          READ(IN,*) ISEGBC(NUM),IRCHBC(NUM),ISFBCTYP(NUM),
     1      (CBCSF(NUM,INDEX),INDEX=1,NCOMP)
C
          IF(ISFBCTYP(NUM).EQ.0) THEN
            BCTYPSF=' HEADWATER'
          ELSEIF(ISFBCTYP(NUM).EQ.1) THEN
            BCTYPSF='    PRECIP'
          ELSEIF(ISFBCTYP(NUM).EQ.2) THEN
            BCTYPSF='    RUNOFF'
          ELSEIF(ISFBCTYP(NUM).EQ.3) THEN
            BCTYPSF='   PUMPING'
          ELSEIF(ISFBCTYP(NUM).EQ.4) THEN
            BCTYPSF='      EVAP'
          ENDIF
C
C          IF(IETSFR.EQ.0.AND.ISFBCTYP(NUM).EQ.4) THEN
C            WRITE(IOUT,*) 'ISFBCTYP=4 IS NOT VALID WHEN IETSFR=0'
C            WRITE(*,*) 'ISFBCTYP=4 IS NOT VALID WHEN IETSFR=0'
C            STOP
C          ENDIF
C
          WRITE(IOUT,70) ISEGBC(NUM),IRCHBC(NUM),BCTYPSF,
     1      (CBCSF(NUM,INDEX),INDEX=1,NCOMP)
C
C          IF(ILKBC(NUM).GT.NLKINIT) THEN
C            WRITE(IOUT,*) 'INVALID LAKE NUMBER'
C            WRITE(*,*) 'INVALID LAKE NUMBER'
C            STOP
C          ENDIF
          IF(ISFBCTYP(NUM).LT.0.OR.ISFBCTYP(NUM).GT.2) THEN
            WRITE(IOUT,*) 'INVALID STREAM BC-TYPE'
            WRITE(*,*) 'INVALID STREAM BC-TYPE'
            STOP
          ENDIF
      ENDDO
C
   30 FORMAT(/1X,'ERROR: MAXIMUM NUMBER OF STREAM SINKS/SOURCES',
     & ' EXCEEDED'/1X,'INCREASE [MXSFBC] IN SFT INPUT FILE')
   40 FORMAT(/1X,'STREAM SINKS/SOURCES OF SPECIFIED CONCENTRATION',
     & ' REUSED FROM LAST STRESS PERIOD')
   50 FORMAT(/1X,'NO. OF STREAM SINKS/SOURCES OF SPECIFIED',
     & ' CONCONCENTRATIONS =',I5,' IN STRESS PERIOD',I3)
   60 FORMAT(/5X,' SEG  RCH     BC-TYPE       CONC(1,NCOMP)')
70    FORMAT(5X,2I5,1X,A10,3X,1000(1X,G15.7))
C
      RETURN
      END
C
C
      SUBROUTINE SFT5FM(IOUT,NCOMP,ICOMP,UPDLHS,CNEW,A,RHS,DTRANS,
     1  NLAY,NROW,NCOL,ICBUND,NODES,MIXELM)
C***********************************************************************
C     THIS SUBROUTINE FORMULATES SFT PACKAGE
C***********************************************************************
      USE SFRVARS
      USE LAKVARS, ONLY : CNEWLAK
      IMPLICIT  NONE
      INTEGER IOUT,NCOMP,ICOMP,ICBUND,NODES,MIXELM,NLAY,NROW,NCOL
      LOGICAL UPDLHS
      INTEGER N,K,I,J,NN
      REAL Q
      REAL CNEW,A,RHS,DTRANS,CONC
      DIMENSION CNEW(NCOL,NROW,NLAY,NCOMP),A(NODES),RHS(NODES)
C
C--FILL COEFFICIENT MATRIX A - WITH GW TO SFR TERMS
      DO N=1,NSTRM
        K=ISFL(N)     !LAYER
        I=ISFR(N)     !ROW
        J=ISFC(N)     !COLUMN
        Q=QSFGW(N)    !(-)VE MEANS GW TO SFR; (+)VE MEANS SFR TO GW
C.......CONSIDER ONLY FLOW INTO STREAM
        IF(Q.LT.0.) THEN
C          CONC=CNEW(J,I,K,ICOMP)
          NN=(K-1)*NCOL*NROW+(I-1)*NCOL+J
          IF(UPDLHS) A(NN)=A(NN)+Q
        ENDIF
      ENDDO
C
C--SOLVE FOR CONC IN SFR
      IF(ISFSOLV.EQ.1) THEN
        
      ELSEIF(ISFSOLV.GT.1) THEN
        CALL SFTSOLV(IOUT,NCOMP,ICOMP,CNEW,DTRANS,
     1  NLAY,NROW,NCOL,ICBUND,NODES)
      ENDIF
C
C--FILL RHS WITH SFR TO GW TERMS USING CALCULATED SFR CONCS
      DO N=1,NSTRM
        K=ISFL(N)     !LAYER
        I=ISFR(N)     !ROW
        J=ISFC(N)     !COLUMN
        Q=0.
        Q=QSFGW(N)    !(-)VE MEANS GW TO SFR; (+)VE MEANS SFR TO GW
        CONC=CNEWSF(N,ICOMP)
C.......CONSIDER ONLY FLOW OUT OF STREAM
        IF(Q.GT.0.) THEN
C          CONC=CNEW(J,I,K,ICOMP)
          NN=(K-1)*NCOL*NROW+(I-1)*NCOL+J
          RHS(NN)=RHS(NN)-Q*CONC
        ENDIF
      ENDDO
C
      RETURN
      END
C
C
      SUBROUTINE SFTSOLV(IOUT,NCOMP,ICOMP,CNEW,DTRANS,
     1  NLAY,NROW,NCOL,ICBUND,NODES)
C***********************************************************************
C     THIS SUBROUTINE ASSEMBLES AND SOLVES MATRIX FOR SFR TRANSPORT
C***********************************************************************
      USE SFRVARS
      USE LAKVARS, ONLY : CNEWLAK
      USE XMDMODULE
      IMPLICIT NONE
      INTEGER IOUT,NCOMP,ICOMP,NLAY,NROW,NCOL,ICBUND,NODES
      REAL CNEW,DTRANS,DELT,DELTMIN
      DIMENSION CNEW(NCOL,NROW,NLAY,NCOMP)
      INTEGER K,I,J,II,N,NN,IS,IR,NC,ICNT,ISIN,IRIN
      REAL CONC,Q,VOL
      INTEGER numactive,KKITER,ITER,N_ITER,JJ,KITERSF,NSUBSTEPS,KSFSTP
      REAL BIGDIFF,BIGDIFF2,DIFF
      INTEGER ICNVG
C
      DELT=DTRANS
      DELTMIN=DTRANS
C
C--DETERMINE DELTSF TO HONOR COURANT CONSTRAINT
CCC      DO I=1,NSTRM
CCC        IF(QOUTSF(I).GE.1.E-3) THEN
CCC          DELT=CRNTSF*SFLEN(I)*SFNAREA(I)/QOUTSF(I)
CCC          IF(DELT.LT.DELTMIN) DELTMIN=DELT
CCC        ENDIF
CCC      ENDDO
C
CCC      IF(DELTMIN.LE.1.E-5) THEN
CCC        WRITE(*,*) 'DELTMIN IS LESS THAN 1.E-5'
CCC        STOP
CCC      ENDIF
C
CCC      NSUBSTEPS=INT(DTRANS/DELTMIN)
CCC      NSUBSTEPS=NSUBSTEPS+1
CCC      DELT=DTRANS/REAL(NSUBSTEPS)
C
CCC      DO KSFSTP=1,NSUBSTEPS
CCC      COLDSF2(:,ICOMP)=CNEWSF(:,ICOMP)
C
      DO KITERSF=1,MXITERSF
      RHSSF=0.
      AMATSF=0.
C
C--FILL RHSSF---------------------------------------------------------
C
C.....INFLOW BOUNDARY CONDITIONS: PRECIP AND RUNOFF
      DO I=1,NSSSF
        IS=ISEGBC(I)
        IR=IRCHBC(I)
        CONC=CBCSF(I,ICOMP)
        N=ISTRM(IR,IS)
        IF(ISFBCTYP(I).EQ.0) THEN
          !BCTYPSF=' HEADWATER'
          DO II=IDXNIN(N),IDXNIN(N+1)-1
            ISIN=INSEG(II)
            IRIN=INRCH(II)
            IF(ISIN.LT.0.AND.IRIN.LT.0 .AND.
     1         IS.EQ.-ISIN.AND.IR.EQ.-IRIN) THEN
                Q=QINSF(II)
                IF(ISFSOLV.EQ.2) THEN
                  RHSSF(N)=RHSSF(N)-Q*CONC
                ELSE
                ENDIF
                GOTO 105
            ENDIF
          ENDDO
104       WRITE (*,*) 'INVLID SFR BC-TYPE',IS,IR
          STOP
105       CONTINUE
        ELSEIF(ISFBCTYP(I).EQ.1) THEN
          !BCTYPSF='    PRECIP'
          Q=QPRECSF(N)
          IF(ISFSOLV.EQ.2) THEN
            RHSSF(N)=RHSSF(N)-Q*CONC
          ELSE
          ENDIF
        ELSEIF(ISFBCTYP(I).EQ.2) THEN
          !BCTYPSF='    RUNOFF'
          Q=QRUNOFSF(N)
          IF(ISFSOLV.EQ.2) THEN
            RHSSF(N)=RHSSF(N)-Q*CONC
          ELSE
          ENDIF
        ENDIF
      ENDDO
C
      ICNT=0
      DO N=1,NSTRM
        K=ISFL(N)     !LAYER
        I=ISFR(N)     !ROW
        J=ISFC(N)     !COLUMN
C
C.......VOLUME/TIME: ASSUME VOLUME DOES NOT CHANGE
        VOL=SFLEN(N)*SFNAREA(N)
        VOL=VOL/DELT
        IF(ISFSOLV.EQ.2) THEN
          RHSSF(N)=RHSSF(N)-VOL*COLDSF(N,ICOMP)
        ELSE
          RHSSF(N)=RHSSF(N)+COLDSF(N,ICOMP)/DELT
        ENDIF
C
C.......INFLOW/OUTFLOW GW
        Q=QSFGW(N)
        IF(Q.LT.0.0) THEN
          IF(ISFSOLV.EQ.2) THEN
            RHSSF(N)=RHSSF(N)+Q*CNEW(J,I,K,ICOMP)
          ELSE
          ENDIF
        ENDIF
C
C.......CHECK INFLOW FROM LAKE
        DO NC=1,NIN(N)
          ICNT=ICNT+1
          IS=INSEG(ICNT)
          IR=INRCH(ICNT)
          IF(IS.GT.0.AND.IR.EQ.0) THEN
            Q=QINSF(ICNT)
            CONC=CNEWLAK(IS,ICOMP)
            IF(ISFSOLV.EQ.2) THEN
              RHSSF(N)=RHSSF(N)-Q*CONC
            ELSE
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C--FILL RHSSF COMPLETE------------------------------------------------
C
C--FILL AMATSF--------------------------------------------------------
      ICNT=0
      DO N=1,NSTRM
        II=IASF(N)
C
C.......VOLUME/TIME: ASSUME VOLUME DOES NOT CHANGE
        VOL=SFLEN(N)*SFNAREA(N)
        VOL=VOL/DELT
        IF(ISFSOLV.EQ.2) THEN
          AMATSF(II)=AMATSF(II)-VOL
        ELSE
          AMATSF(II)=AMATSF(II)+1.0D0/DELT
        ENDIF
C
C.......TOTAL FLOW OUT INCLUDING ET
        IF(IETSFR.EQ.0) THEN
          Q=QOUTSF(N)-QETSF(N)
          IF(QETSF(N).GE.QOUTSF(N)) Q=0.
        ELSE
          Q=QOUTSF(N)
        ENDIF
        IF(ISFSOLV.EQ.2) THEN
          AMATSF(II)=AMATSF(II)-Q
        ELSE
        ENDIF
C
C.......INFLOW/OUTFLOW GW
        Q=QSFGW(N)
        IF(Q.GE.0.0) THEN
          IF(ISFSOLV.EQ.2) THEN
            AMATSF(II)=AMATSF(II)-Q
          ELSE
          ENDIF
        ENDIF
C
C.......INFLOW FROM UPSTREAM REACHES, LAKES, FIRST REACH (IS=0,IR=0)
        DO NC=1,NIN(N)
          ICNT=ICNT+1
          IS=INSEG(ICNT)
          IR=INRCH(ICNT)
          IF(IS.GT.0.AND.IR.GT.0) THEN
            NN=ISTRM(IR,IS)
            Q=QINSF(ICNT)
            DO II=IASF(N)+1,IASF(N+1)-1
              IF(NN.EQ.JASF(II)) GOTO 110
            ENDDO
100         WRITE(*,*) 'ERROR IN JASF MATRIX'
            STOP
110         CONTINUE
            IF(ISFSOLV.EQ.2) THEN
              AMATSF(II)=AMATSF(II)+Q
            ELSE
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C--FILL AMATSF COMPLETE-----------------------------------------------
C
C-----CALL XMD SOLVER-------------------------------------------------
      numactive=NSTRM
      IF (IDROPTOL.EQ.0 .or.  KITERSF.gt.1) THEN
        call xmdnfctr(AMATSF, RHSSF, IASF, JASF, NJASF, numactive, ierr)
      ELSE
        call xmdprecd(AMATSF, RHSSF, epsrn, IASF, JASF, NJASF,
     [                       numactive,level, ierr)
      ENDIF
      iter = Mxiterxmd
      CNEWSFTMP=CNEWSF
      call xmdsolv(AMATSF, RHSSF, CNEWSF, hclosexmd, rrctol, IASF, 
     [                 JASF,NJASF,numactive, north, iter, iacl, ierr)
      n_iter = iter
C     
      IF(IDSCALE.EQ.1)THEN
c  ----------------
c       recover {x} because diagonal scaling subroutine (xmddgscl) is used
        do jj = 1, Numactive
          CNEWSF(jj,ICOMP) = CNEWSF(jj,ICOMP) * dgscal(jj)
        enddo                  
      ENDIF
C--------- XMD SOLVER-------------------------------------------------
C
C-----CHECK FOR CONVERGENCE
      ICNVG=0
      BIGDIFF=0.
      BIGDIFF2=0.
      DO N=1,NSTRM
        DIFF=CNEWSF(N,ICOMP)-CNEWSFTMP(N,ICOMP)
        IF(ABS(DIFF).GT.BIGDIFF) THEN
          BIGDIFF=ABS(DIFF)
          BIGDIFF2=DIFF
        ENDIF
      ENDDO
      IF(BIGDIFF.LT.CCLOSESF) ICNVG=1
      IF(ICNVG.EQ.1) EXIT
C
      ENDDO !KITERSF
C
CCC      ENDDO !KSFSTP
C
      RETURN
      END
C
C
      SUBROUTINE FILLIASFJASF()
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES AND FILLS IA AND JA MATRICES
C***********************************************************************
      USE SFRVARS
      IMPLICIT NONE
      INTEGER N,NC,K,I,J,IS,IR,ICNT,NN,II
C
      IF(ALLOCATED(IASF)) DEALLOCATE(IASF)
      IF(ALLOCATED(RHSSF)) DEALLOCATE(RHSSF)
      IF(ALLOCATED(JASF)) DEALLOCATE(JASF)
      IF(ALLOCATED(AMATSF)) DEALLOCATE(AMATSF)
C
      ALLOCATE(IASF(NSTRM+1),RHSSF(NSTRM))
      IASF=0
      RHSSF=0.
C
C--COUNT CONNECTIONS AT EACH NODE N
      ICNT=0
      NJASF=0
      DO N=1,NSTRM
        IASF(N)=IASF(N)+1
        DO NC=1,NIN(N)
          ICNT=ICNT+1
          IS=INSEG(ICNT)
          IR=INRCH(ICNT)
          IF(IS.GT.0.AND.IR.GT.0) THEN
            IASF(N)=IASF(N)+1
            NN=ISTRM(IR,IS)
            IASF(NN)=IASF(NN)+1
          ENDIF
        ENDDO
      ENDDO
C
C-------CUMULATIVE OF CONNECTIONS PER ROW IN IASF
C--REFERENCE: SORAB'S CODE
      DO II=2,NSTRM+1
        IASF(II) = IASF(II) + IASF(II-1)
      ENDDO
C-------IASF(N+1) IS CUMULATIVE_IASF(N) + 1
      DO II=NSTRM+1,2,-1
        IASF(II) = IASF(II-1) + 1
      ENDDO
      IASF(1) = 1
C--ALLOCATE JASF AND AMATSF WITH SIZE NJASF
      NJASF = IASF(NSTRM+1) - 1
      ALLOCATE(JASF(NJASF),AMATSF(NJASF))
      JASF=0
      AMATSF=0.
C
C--POPUATE JA
C-------DIAGONAL IN FIRST LOCATION
      DO N=1,NSTRM
C-------ONCE JA WORKS FINE REMOVE THIS BLOCK
        IF(JASF(IASF(N)).NE.0) THEN
          WRITE(*,*) 'ERROR IN JASF'
          READ(*,*)
          STOP
        ENDIF
C-------------------------------------------
        JASF(IASF(N)) = N
      ENDDO
C
      ICNT=0
      DO N=1,NSTRM
        DO NC=1,NIN(N)
          ICNT=ICNT+1
          IS=INSEG(ICNT)
          IR=INRCH(ICNT)
          IF(IS.GT.0.AND.IR.GT.0) THEN
            NN=ISTRM(IR,IS)
            CALL FINDJASF(N,NN,IASF,JASF,NSTRM,NJASF)
          ENDIF
        ENDDO
      ENDDO

C
      RETURN
      END
C
C
      SUBROUTINE FINDJASF(N,NN,IASF,JASF,NSTRM,NJASF)
C
      INTEGER IASF(NSTRM+1),JASF(NJASF)
      INTEGER N,NN,I1,I2
C
      I1=IASF(N)
      I2=IASF(N+1)-1
      ICHECK=0
      DO I=I1,I2
        IF(JASF(I).EQ.0) THEN
          JASF(I)=NN
          ICHECK=1
          EXIT
        ENDIF
      ENDDO
      IF(ICHECK.EQ.0) THEN
        WRITE(*,*) 'ERROR IN FINDJASF',N
        READ(*,*)
        STOP
      ENDIF
C
      I1=IASF(NN)
      I2=IASF(NN+1)-1
      ICHECK=0
      DO I=I1,I2
        IF(JASF(I).EQ.0) THEN
          JASF(I)=N
          ICHECK=1
          EXIT
        ENDIF
      ENDDO
      IF(ICHECK.EQ.0) THEN
        WRITE(*,*) 'ERROR IN FINDJASF',NN
        READ(*,*)
        STOP
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE SFT5AD(N)
C***********************************************************************
C     RESET STREAM CONCENTRATIONS
C***********************************************************************
      USE SFRVARS
      INTEGER N
C
C--RESET STREAM CONCENTRATION
      COLDSF=CNEWSF
      COLDSF2=CNEWSF
C
      RETURN
      END
C
C
      SUBROUTINE SFT5AD2(N)
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
      QINSFO=QINSF
C
      RETURN
      END
C
C
      SUBROUTINE SFT5BD(IOUT,NCOMP,ICOMP,CNEW,DTRANS,
     1  NLAY,NROW,NCOL,ICBUND,KPER,KSTP,NTRANS,TIME2,PRTOUT,INSFT,
     1  RMASIO)
C***********************************************************************
C     THIS SUBROUTINE CALCULATES BUDGETS FOR STREAMS
C     THIS SUBROUTINE CALCULATES GROUNDWATER BUDGETS RELATED TO STREAMS
C     THIS SUBROUTINE WRITES STREAM CONCENTRATIONS AT OBSERVATION LOCATIONS
C***********************************************************************
      USE LAKVARS
      USE SFRVARS
      IMPLICIT  NONE
      INTEGER IS,IR
      INTEGER IOUT,NCOMP,ICOMP,ICBUND,MIXELM,NLAY,NROW,NCOL,INSFT
      INTEGER K,I,J,N,NUM,II
      INTEGER KPER,KSTP,NTRANS
      REAL CNEW,DTRANS,TIME2
      REAL CONC,Q,VO,CO,VOL,QC,Q1,Q2,DELV,QDIFF
      REAL GW2LAK,GWFROMLAK,SFR2LAK,SFRFROMLAK,PRECLK,RUNOFLK,WDRLLK,
     1  ETLK,TOTINLK,TOTOUTLK,CTOTINLK,CTOTOUTLK,DIFF,CDIFF,PERC,CPERC,
     1  STORINLK,STOROTLK,TOTMASOLD,TOTMASNEW,STORDIFF
      LOGICAL PRTOUT
      DIMENSION CNEW(NCOL,NROW,NLAY,NCOMP)
      REAL RMASIO(122,2,NCOMP)
C
C--WRITE OBSERVATIONS
      DO I=1,NOBSSF
        IS=ISOBS(I)
        IR=IROBS(I)
        N=ISTRM(IR,IS)
        CONC=CNEWSF(N,ICOMP)
        WRITE(IOUTOBS,*) TIME2,IS,IR,CONC
      ENDDO
C
C--RETURN
      RETURN
      END
