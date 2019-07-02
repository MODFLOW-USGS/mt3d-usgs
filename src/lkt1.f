C
      SUBROUTINE LKT1AR(IN)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES SPACE FOR LAK VARIABLES
C***********************************************************************
      USE LAKVARS
      USE MT3DMS_MODULE, ONLY: INLKT,IOUT,NCOMP
      INTEGER IN
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INLKT
 1030 FORMAT(/1X,'LKT1 -- LAKE TRANSPORT PACKAGE,',
     &           ' VERSION 1, MAY 2016, INPUT READ FROM UNIT',I3)
C
C--ALLOCATE VARIABLES USED IN FMI
      ALLOCATE(NLAKES,LKNODE,NSFRLAK,NSSLK)
      NSSLK=0
C--READ NUMBER OF LAKES
      ALLOCATE(NLKINIT,MXLKBC,ICBCLK,IETLAK)
      READ(INLKT,*) NLKINIT,MXLKBC,ICBCLK,IETLAK
      WRITE(IOUT,10) NLKINIT,MXLKBC
10    FORMAT(1X,'NUMBER OF LAKES = ',I5,
     &      /1X,'MAXIMUM NUMBER OF LAKE BOUNDARY CONDITIONS = ',I5)
      IF(ICBCLK.GT.0) WRITE(IOUT,12) ICBCLK
12    FORMAT(1X,'LAKE-BY-LAKE INFORMATION WILL BE PRINTED ON UNIT ',I5)
      IF(IETLAK.EQ.0) THEN
        WRITE(IOUT,14)
      ELSE
        WRITE(IOUT,16)
      ENDIF
14    FORMAT(1X,'MASS DOES NOT EXIT VIA LAKE ET')
16    FORMAT(1X,'MASS IS ALLOWED TO EXIT VIA LAKE ET')
C
C--ALLOCATE INITIAL AND BOUNDARY CONDITION ARRAYS
      ALLOCATE(CNEWLAK(NLKINIT,NCOMP),COLDLAK(NLKINIT,NCOMP),
     &         BUFFLAK(NLKINIT,NCOMP))
      ALLOCATE(ILKBC(MXLKBC),ILKBCTYP(MXLKBC))
      ALLOCATE(CBCLK(MXLKBC,NCOMP))
      CBCLK=0.
      ALLOCATE(RMASLAK(NLKINIT),VOUTLAK(NLKINIT))
      RMASLAK=0.
      ALLOCATE(QPRECLAK(NLKINIT),QRUNOFLAK(NLKINIT),QWDRLLAK(NLKINIT),
     &         QETLAK(NLKINIT),VOLOLAK(NLKINIT),VOLNLAK(NLKINIT),
     &         DELVOLLAK(NLKINIT))
      QPRECLAK=0.
      QRUNOFLAK=0.
      QWDRLLAK=0.
      QETLAK=0.
      VOLOLAK=0.
      VOLNLAK=0.
      DELVOLLAK=0.
C
C--CUMULATIVE BUDGET TERMS
      ALLOCATE(CGW2LAK(NCOMP),CGWFROMLAK(NCOMP),CSFR2LAK(NCOMP),
     &         CSFRFROMLAK(NCOMP),CPRECLK(NCOMP),CRUNOFLK(NCOMP),
     &         CWDRLLK(NCOMP),CETLK(NCOMP),CSTORINLK(NCOMP),
     &         CSTOROTLK(NCOMP),CUZF2LAK(NCOMP))
      CGW2LAK=0.
      CUZF2LAK=0.
      CGWFROMLAK=0.
      CSFR2LAK=0.
      CSFRFROMLAK=0.
      CPRECLK=0.
      CRUNOFLK=0.
      CWDRLLK=0.
      CETLK=0.
      CSTORINLK=0.
      CSTOROTLK=0.
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE LKT1RP(KPER)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES READS LAK VARIABLES - INITIAL CONCS
C***********************************************************************
      USE LAKVARS
      USE MT3DMS_MODULE, ONLY: INLKT,IOUT,NCOMP
      CHARACTER ANAME*24
      INTEGER   KPER
C
C--PRINT A HEADER
      WRITE(IOUT,1000)
 1000 FORMAT(//1X,'LAKE INPUT PARAMETERS'/1X,21('-')/)
C
C--CALL RARRAY TO READ IN CELL WIDTH ALONG ROWS
      DO INDEX=1,NCOMP
        ANAME='LAKE INIT CONC COMP#    '
        WRITE(ANAME(22:24),'(I3.3)') INDEX
        CALL RARRAY(BUFFLAK(:,INDEX),ANAME,1,NLKINIT,0,INLKT,IOUT)
      ENDDO
      COLDLAK=BUFFLAK
      CNEWLAK=COLDLAK
C
      RETURN
      END
C
C
      SUBROUTINE LKT1SS(KPER)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES LAK BOUNDARY CONDITIONS
C***********************************************************************
      USE LAKVARS
      USE MT3DMS_MODULE, ONLY: INLKT,IOUT,NCOMP
      CHARACTER*10 BCTYPLK
C
      IN=INLKT
C
C--PRINT A HEADER
      WRITE(IOUT,1000)
 1000 FORMAT(//1X,'LAKE BOUNDARY CONDITIONS'/1X,24('-')/)
C
C--READ AND ECHO POINT SINKS/SOURCES OF SPECIFIED CONCENTRATIONS
      READ(IN,'(I10)') NTMP
C
C--BASIC CHECKS ON NTMP
      IF(KPER.EQ.1.AND.NTMP.LT.0) THEN
        WRITE(IOUT,*) 'NTMP<0 NOT ALLOWED FOR FIRST STRESS PERIOD'
        WRITE(*,*)    'NTMP<0 NOT ALLOWED FOR FIRST STRESS PERIOD'
        STOP
      ENDIF
      IF(NTMP.EQ.0) THEN
        RETURN
      ENDIF
C
C--RESET ARRAYS
      IF(NTMP.GE.0) THEN
        ILKBC=0
        ILKBCTYP=0
        CBCLK=0.
      ENDIF
C
C
      IF(NTMP.GT.MXLKBC) THEN
        WRITE(*,30)
        CALL USTOP(' ')
      ELSEIF(NTMP.LT.0) THEN
        WRITE(IOUT,40)
        RETURN
      ELSEIF(NTMP.GE.0) THEN
        WRITE(IOUT,50) NTMP,KPER
        NSSLK=NTMP
        IF(NTMP.EQ.0) RETURN
      ENDIF
C
C--READ BOUNDARY CONDITIONS
      WRITE(IOUT,60)
      DO NUM=1,NTMP
        READ(IN,*) ILKBC(NUM),ILKBCTYP(NUM),
     &             (CBCLK(NUM,INDEX),INDEX=1,NCOMP)
C
        IF(ILKBCTYP(NUM).EQ.1) THEN
          BCTYPLK='    PRECIP'
        ELSEIF(ILKBCTYP(NUM).EQ.2) THEN
          BCTYPLK='    RUNOFF'
        ELSEIF(ILKBCTYP(NUM).EQ.3) THEN
          BCTYPLK='   PUMPING'
        ELSEIF(ILKBCTYP(NUM).EQ.4) THEN
          BCTYPLK='      EVAP'
        ENDIF
C
        WRITE(IOUT,70) ILKBC(NUM),BCTYPLK,
     &    (CBCLK(NUM,INDEX),INDEX=1,NCOMP)
C
        IF(ILKBC(NUM).GT.NLKINIT) THEN
          WRITE(IOUT,*) 'INVALID LAKE NUMBER'
          WRITE(*,*)    'INVALID LAKE NUMBER'
          STOP
        ENDIF
        IF(ILKBCTYP(NUM).LT.1.OR.ILKBCTYP(NUM).GT.2) THEN
          WRITE(IOUT,*) 'INVALID LAKE BC-TYPE'
          WRITE(*,*)    'INVALID LAKE BC-TYPE'
          STOP
        ENDIF
      ENDDO
C
   30 FORMAT(/1X,'ERROR: MAXIMUM NUMBER OF LAKE SINKS/SOURCES',
     &           ' EXCEEDED'/1X,'INCREASE [MXLKBC] IN LKT INPUT FILE')
   40 FORMAT(/1X,'LAKE SINKS/SOURCES OF SPECIFIED CONCENTRATION',
     &           ' REUSED FROM LAST STRESS PERIOD')
   50 FORMAT(/1X,'NO. OF LAKE SINKS/SOURCES OF SPECIFIED',
     &           ' CONCENTRATIONS =',I5,' IN STRESS PERIOD',I3)
   60 FORMAT(/5X,' LAKE    BC-TYPE       CONC(1,NCOMP)')
70    FORMAT( 5X,I5,1X,A10,3X,1000(1X,G15.7))
C
      RETURN
      END
C
C
      SUBROUTINE LKT1FM(ICOMP)
C***********************************************************************
C     THIS SUBROUTINE FORMULATES LKT PACKAGE
C***********************************************************************
      USE MIN_SAT,       ONLY: QC7,DRYON
      USE LAKVARS
      USE SFRVARS,       ONLY: CNEWSF
      USE UZTVARS,       ONLY: CUZINF 
      USE MT3DMS_MODULE, ONLY: IOUT,NCOMP,UPDLHS,CNEW,A,RHS,DTRANS,
     &                         NLAY,NROW,NCOL,ICBUND,NODES,MIXELM,
     &  iUnitTRNOP,iSSTrans
      USE PKG2PKG
      IMPLICIT  NONE
      INTEGER ICOMP
      INTEGER K,I,J,N,NUM,ISEG,IREACH,II,ICON,ICNT,NSF,NUZ
      REAL    CONC,Q,CO
      DOUBLE PRECISION VO,VN,DV
C
C--LAK TRANSPORT ONLY AVAILABLE WITH TVD OR FD SCHEMES
      IF(MIXELM.GT.0) THEN
        WRITE(IOUT,*) 'LAK TRANSPORT ONLY WORKS WITH MIXELM<=0'
        WRITE(*,*)    'LAK TRANSPORT ONLY WORKS WITH MIXELM<=0'
        STOP
      ENDIF
C
C--ZERO OUT TERMS
      CONC=0.
      Q=0.
      RMASLAK=0.
      VOUTLAK=0.
C
C--ALL INFLOW TERMS-------------------------------------------------
C
C--GW TO LAK FLOW
      DO NUM=1,LKNODE
        N=LAKNUMGW(NUM) !LAKE NUMBER
        K=LAKL(NUM)     !LAYER
        I=LAKR(NUM)     !ROW
        J=LAKC(NUM)     !COLUMN
        Q=0.
        Q=QLAKGW(NUM)   !(-)VE MEANS GW TO LAK; (+)VE MEANS LAK TO GW
        CONC=CNEW(J,I,K,ICOMP)
C.......CONSIDER ONLY FLOW INTO LAKE
        IF(ICBUND(J,I,K,ICOMP).LE.0) THEN
          IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
               IF(Q.LT.0.) THEN
                 QC7(J,I,K,9)=QC7(J,I,K,9)-Q
               ENDIF
            ENDIF
          ENDIF          
        ELSE
          IF(Q.LT.0.) THEN
            RMASLAK(N)=RMASLAK(N)+CONC*ABS(Q)
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(UPDLHS) A(N)=A(N)+Q
          ENDIF
        ENDIF
      ENDDO
C
C--INFLOW FROM BCs
      DO NUM=1,NSSLK
        N=ILKBC(NUM)
        CONC=CBCLK(NUM,ICOMP)
        Q=0.
C.......PRECIP AND RUNOFF ARE INFLOW BOUNDARIES
        IF(ILKBCTYP(NUM).EQ.1) THEN
          !BCTYPLK='    PRECIP'
          Q=QPRECLAK(N)
        ELSEIF(ILKBCTYP(NUM).EQ.2) THEN
          !BCTYPLK='    RUNOFF'
          Q=QRUNOFLAK(N)
        ENDIF
        RMASLAK(N)=RMASLAK(N)+CONC*Q
      ENDDO
C
C--INFLOW FROM SFR
      DO ICNT=1,NSFR2LAK
        NSF=INOD1SFLK(ICNT)
        N=INOD2SFLK(ICNT)
        Q=QSFR2LAK(ICNT)
        CONC=(CNEWSF(NSF,ICOMP))
        IF(Q.GT.0.) THEN
          RMASLAK(N)=RMASLAK(N)+CONC*ABS(Q)
        ENDIF
      ENDDO
C
C--UZF TO LAK FLOW
      IF(iUnitTRNOP(7).GT.0) THEN
        DO ICON=1,NLAK2UZF
          N=INOD1LKUZ(ICON)
          NUZ=INOD2LKUZ(ICON)
          CALL NODE2KIJ(NUZ,NLAY,NROW,NCOL,K,I,J)
          Q=QLAK2UZF(ICON)
          Q=ABS(Q)
          IF(IUZCODELK(ICON).EQ.1) THEN
            CONC=CNEW(J,I,K,ICOMP)
            RMASLAK(N)=RMASLAK(N)+CONC*ABS(Q)
          ELSEIF(IUZCODELK(ICON).EQ.2) THEN
            CONC=CUZINF(J,I,ICOMP)
            RMASLAK(N)=RMASLAK(N)+CONC*ABS(Q)
          ELSEIF(IUZCODELK(ICON).EQ.3) THEN
            CONC=CUZINF(J,I,ICOMP)
            RMASLAK(N)=RMASLAK(N)+CONC*ABS(Q)
          ELSE
            WRITE(IOUT,*) 'CHECK FTL FILE - IUZCODELK(ICON) INVALID'
            WRITE(*,*) 'CHECK FTL FILE - IUZCODELK(ICON) INVALID'
            STOP
          ENDIF
        ENDDO
      ENDIF
C
C--MULTIPLY MASS TERM WITH TRANSPORT TIME-STEP
      RMASLAK=RMASLAK*DTRANS
C
C--ADD OLD MASS IN THE LAKE
      IF(iSSTrans.EQ.0) THEN
        DO N=1,NLAKES
          CALL TIMEINTERP(VOLNLAK,VOLOLAK,DELVOLLAK,NLAKES,N,VN,VO,DV,2)
          CO=COLDLAK(N,ICOMP)
          RMASLAK(N)=RMASLAK(N)+VO*CO
        ENDDO
      ENDIF
C-------------------------------------------------------------------
C
C--CALCULATE LAKE CONCENTRATION-------------------------------------
C
C--CALCULATE TOTAL VOLUME FOR CONC CALCULATION
C--IT IS ASSUMED THAT ET IS TAKEN OUT FIRST SO THAT LAKE AND OUT CONC IS SAME
C
C--LAK TO GW FLOW
      DO NUM=1,LKNODE
        N=LAKNUMGW(NUM) !LAKE NUMBER
        K=LAKL(NUM)     !LAYER
        I=LAKR(NUM)     !ROW
        J=LAKC(NUM)     !COLUMN
        Q=0.
        Q=QLAKGW(NUM)   !(-)VE MEANS GW TO LAK; (+)VE MEANS LAK TO GW
C.......CONSIDER ONLY FLOW OUT OF LAKE
        IF(Q.GT.0.) THEN
          VOUTLAK(N)=VOUTLAK(N)+Q
        ENDIF
      ENDDO
C
C--OUTFLOW TO BOUNDARY CONDITIONS (ONLY WITHDRAWAL)
      DO N=1,NLAKES
        Q=0.
        Q=QWDRLLAK(N)
        VOUTLAK(N)=VOUTLAK(N)+Q
        IF(IETLAK.NE.0) THEN
          Q=QETLAK(N)
          VOUTLAK(N)=VOUTLAK(N)+Q
        ENDIF
      ENDDO
C
C--FLOW TO STREAMS
      DO ICNT=1,NSFR2LAK
        NSF=INOD1SFLK(ICNT)
        N=INOD2SFLK(ICNT)
        Q=0.
        Q=QSFR2LAK(ICNT)
        IF(Q.LT.0.) THEN
          VOUTLAK(N)=VOUTLAK(N)+ABS(Q)
        ENDIF
      ENDDO
C
C-----LAKE VOLUME CHANGE
      IF(iSSTrans.EQ.0) THEN
        DO N=1,NLAKES
          CALL TIMEINTERP(VOLNLAK,VOLOLAK,DELVOLLAK,NLAKES,N,VN,VO,DV,2)
          VOUTLAK(N)=VOUTLAK(N)*DTRANS+VO+DV*DTRANS
        ENDDO
      ENDIF
C
C--CALCULATE LAKE AND OUT CONCENTRATION
      DO N=1,NLAKES
        IF(VOUTLAK(N).LE.1E-6) THEN
          CNEWLAK(N,ICOMP)=0.
        ELSE
          CNEWLAK(N,ICOMP)=RMASLAK(N)/VOUTLAK(N)
        ENDIF
      ENDDO
C-------------------------------------------------------------------
C
C--ALL OUTFLOW TERMS------------------------------------------------
C
C--LAK TO GW FLOW
      DO NUM=1,LKNODE
        N=LAKNUMGW(NUM) !LAKE NUMBER
        K=LAKL(NUM)     ! LAYER
        I=LAKR(NUM)     !ROW
        J=LAKC(NUM)     !COLUMN
        Q=0.
        Q=QLAKGW(NUM)   !(-)VE MEANS GW TO LAK; (+)VE MEANS LAK TO GW
        CONC=CNEWLAK(N,ICOMP)
C.......CONSIDER ONLY FLOW OUT OF LAKE
        IF(ICBUND(J,I,K,ICOMP).LE.0) THEN
          IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              IF(Q.GT.0.) THEN
                QC7(J,I,K,7)=QC7(J,I,K,7)-Q*CONC
                QC7(J,I,K,8)=QC7(J,I,K,8)-Q
              ENDIF
            ENDIF
          ENDIF          
        ELSE
          IF(Q.GT.0.) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            RHS(N)=RHS(N)-Q*CONC
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
C
C
      SUBROUTINE LKT1BD(ICOMP,KPER,KSTP,DTRANS,NTRANS)
C***********************************************************************
C     THIS SUBROUTINE CALCULATES BUDGETS FOR LAKE
C     THIS SUBROUTINE CALCULATES GROUNDWATER BUDGETS RELATED TO LAKES
C***********************************************************************
      USE MIN_SAT, ONLY: QC7,DRYON
      USE LAKVARS
      USE SFRVARS, ONLY : CNEWSF
      USE UZTVARS,       ONLY: CUZINF !UZQ,NCON,NCONLK,IROUTE,
      USE PKG2PKG
      USE MT3DMS_MODULE, ONLY: IOUT,NCOMP,UPDLHS,CNEW,TIME2,A,RHS,
     &                         NLAY,NROW,NCOL,ICBUND,NODES,MIXELM,
     &                         PRTOUT,INSFT,RMASIO,INUZT,iUnitTRNOP,
     &                         iSSTrans
!     &  MXUZCON,IROUTE,UZQ,NCON,CUZINF,NCONLK
      IMPLICIT  NONE
      INTEGER ICOMP
      INTEGER K,I,J,N,NUM,ISEG,IREACH,II,ICON,ICNT,NSF,NUZ
      INTEGER KPER,KSTP,NTRANS
      REAL    CONC,Q,CO,VOL,QC,Q1,Q2,DELV,QDIFF,DTRANS
      REAL    GW2LAK,GWFROMLAK,SFR2LAK,SFRFROMLAK,PRECLK,RUNOFLK,WDRLLK,
     &        ETLK,TOTINLK,TOTOUTLK,CTOTINLK,CTOTOUTLK,DIFF,CDIFF,PERC,
     &        CPERC,STORINLK,STOROTLK,TOTMASOLD,TOTMASNEW,STORDIFF,
     &        UZF2LAK
      DOUBLE PRECISION VO,VN,DV
C
C--ZERO OUT TERMS
      CONC=0.
      Q=0.
      RMASLAK=0.
      VOUTLAK=0.
C
      GW2LAK=0.
      UZF2LAK=0.
      GWFROMLAK=0.
      SFR2LAK=0.
      SFRFROMLAK=0.
      PRECLK=0.
      RUNOFLK=0.
      WDRLLK=0.
      ETLK=0.
      STORINLK=0.
      STOROTLK=0.
      Q1=0.
      Q2=0.
C
C--WRITE HEADER TO ICBCLK FILE
      IF(KPER.EQ.1 .AND. KSTP.EQ.1.AND.NTRANS.EQ.1.AND.ICBCLK.GT.0)THEN
        WRITE(ICBCLK,*) ' LAKE-BY-LAKE BUDGET SUMMARY FOR ALL LAKES'
        WRITE(ICBCLK,5)
5       FORMAT('    STRESS      TSTP',
     &         ' TRAN-STEP       TIME          LAKE       VOLUME  ',
     &         ' CONCENTRATION       MASS        ICOMP  ')
      ENDIF
C
C--ALL INFLOW TERMS-------------------------------------------------
C
C--GW TO LAK FLOW
      DO NUM=1,LKNODE
        N=LAKNUMGW(NUM) !LAKE NUMBER
        K=LAKL(NUM)     !LAYER
        I=LAKR(NUM)     !ROW
        J=LAKC(NUM)     !COLUMN
        Q=0.
        Q=QLAKGW(NUM)   !(-)VE MEANS GW TO LAK; (+)VE MEANS LAK TO GW
        CONC=CNEW(J,I,K,ICOMP)
C.......CONSIDER ONLY FLOW INTO LAKE
        IF(ICBUND(J,I,K,ICOMP).LE.0) THEN
          IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              IF(Q.LT.0.) THEN
                Q1=Q1+ABS(Q)
                QC7(J,I,K,9)=QC7(J,I,K,9)-Q
              ENDIF
            ENDIF
          ENDIF          
        ELSE
          IF(Q.LT.0.) THEN
            RMASLAK(N)=RMASLAK(N)+CONC*ABS(Q)
            GW2LAK=GW2LAK+CONC*ABS(Q)
            Q1=Q1+ABS(Q)
            RMASIO(26,2,ICOMP)=RMASIO(26,2,ICOMP)+Q*CONC*DTRANS
          ENDIF
        ENDIF
      ENDDO
C
C--INFLOW FROM BCs FOR FLOW CALCULATION
      DO N=1,NLAKES
        Q=QPRECLAK(N)
        Q1=Q1+Q
        Q=QRUNOFLAK(N)
        Q1=Q1+Q
      ENDDO
C
C--INFLOW FROM BCs
      DO NUM=1,NSSLK
        N=ILKBC(NUM)
        CONC=CBCLK(NUM,ICOMP)
        Q=0.
C.......PRECIP AND RUNOFF ARE INFLOW BOUNDARIES
        IF(ILKBCTYP(NUM).EQ.1) THEN !BCTYPLK='    PRECIP'
          Q=QPRECLAK(N)
          PRECLK=PRECLK+CONC*Q
        ELSEIF(ILKBCTYP(NUM).EQ.2) THEN !BCTYPLK='    RUNOFF'
          Q=QRUNOFLAK(N)
          RUNOFLK=RUNOFLK+CONC*Q
        ENDIF
        RMASLAK(N)=RMASLAK(N)+CONC*Q
      ENDDO
C
C--INFLOW FROM SFR
      DO ICNT=1,NSFR2LAK
        NSF=INOD1SFLK(ICNT)
        N=INOD2SFLK(ICNT)
        Q=0.
        Q=QSFR2LAK(ICNT)
        CONC=(CNEWSF(NSF,ICOMP))
        IF(Q.GT.0.) THEN
          RMASLAK(N)=RMASLAK(N)+CONC*ABS(Q)
          SFR2LAK=SFR2LAK+CONC*ABS(Q)
          Q1=Q1+ABS(Q)
        ENDIF
      ENDDO
C
C--UZF TO LAK FLOW
      IF(iUnitTRNOP(7).GT.0) THEN
        DO ICON=1,NLAK2UZF
          N=INOD1LKUZ(ICON)
          NUZ=INOD2LKUZ(ICON)
          CALL NODE2KIJ(NUZ,NLAY,NROW,NCOL,K,I,J)
          Q=QLAK2UZF(ICON)
          Q=ABS(Q)
          IF(IUZCODELK(ICON).EQ.1) THEN
            CONC=CNEW(J,I,K,ICOMP)
          ELSEIF(IUZCODELK(ICON).EQ.2) THEN
            CONC=CUZINF(J,I,ICOMP)
          ENDIF
          RMASLAK(N)=RMASLAK(N)+CONC*ABS(Q)
          UZF2LAK=UZF2LAK+CONC*ABS(Q)
          Q1=Q1+ABS(Q)
        ENDDO
      ENDIF
C
C--MULTIPLY MASS TERM WITH TRANSPORT TIME-STEP
      RMASLAK=RMASLAK*DTRANS
C
C--ADD OLD MASS IN THE LAKE
      IF(iSSTrans.EQ.0) THEN
      DO N=1,NLAKES
        CALL TIMEINTERP(VOLNLAK,VOLOLAK,DELVOLLAK,NLAKES,N,VN,VO,DV,2)
        CO=COLDLAK(N,ICOMP)
        RMASLAK(N)=RMASLAK(N)+VO*CO
      ENDDO
      ENDIF
C-------------------------------------------------------------------
C
C--CALCULATE LAKE CONCENTRATION-------------------------------------
C
C--CALCULATE TOTAL VOLUME FOR CONC CALCULATION
C--IT IS ASSUMED THAT ET IS TAKEN OUT FIRST SO THAT LAKE AND OUT CONC IS SAME
C
C--LAK TO GW FLOW
      DO NUM=1,LKNODE
        N=LAKNUMGW(NUM) !LAKE NUMBER
        K=LAKL(NUM)     ! LAYER
        I=LAKR(NUM)     !ROW
        J=LAKC(NUM)     !COLUMN
        Q=0.
        Q=QLAKGW(NUM)   !(-)VE MEANS GW TO LAK; (+)VE MEANS LAK TO GW
C.......CONSIDER ONLY FLOW OUT OF LAKE
        IF(Q.GT.0.) THEN
          VOUTLAK(N)=VOUTLAK(N)+Q
          Q2=Q2+Q
        ENDIF
      ENDDO
C
C--OUTFLOW TO BOUNDARY CONDITIONS (ONLY WITHDRAWAL)
      DO N=1,NLAKES
        Q=0.
        Q=QWDRLLAK(N)
        VOUTLAK(N)=VOUTLAK(N)+Q
        Q2=Q2+Q
        IF(IETLAK.NE.0) THEN
          Q=QETLAK(N)
          VOUTLAK(N)=VOUTLAK(N)+Q
        ENDIF
        Q=QETLAK(N)
        Q2=Q2+Q
      ENDDO
C
C--FLOW TO STREAMS
      DO ICNT=1,NSFR2LAK
        NSF=INOD1SFLK(ICNT)
        N=INOD2SFLK(ICNT)
        Q=0.
        Q=QSFR2LAK(ICNT)
        IF(Q.LT.0.) THEN
          VOUTLAK(N)=VOUTLAK(N)+ABS(Q)
          Q2=Q2+ABS(Q)
        ENDIF
      ENDDO
C
C-----LAKE VOLUME CHANGE
      IF(iSSTrans.EQ.0) THEN
      DO N=1,NLAKES
        CALL TIMEINTERP(VOLNLAK,VOLOLAK,DELVOLLAK,NLAKES,N,VN,VO,DV,2)
        VOUTLAK(N)=VOUTLAK(N)*DTRANS+VO+DV*DTRANS
      ENDDO
      ENDIF
C
C--CALCULATE LAKE AND OUT CONCENTRATION
      DO N=1,NLAKES
        IF(VOUTLAK(N).LE.1E-6) THEN
          CNEWLAK(N,ICOMP)=0.
        ELSE
          CNEWLAK(N,ICOMP)=RMASLAK(N)/VOUTLAK(N)
        ENDIF
      ENDDO
C-------------------------------------------------------------------
C
C--ALL OUTFLOW TERMS------------------------------------------------
C
C--LAK TO GW FLOW
      DO NUM=1,LKNODE
        N=LAKNUMGW(NUM) !LAKE NUMBER
        K=LAKL(NUM)     !LAYER
        I=LAKR(NUM)     !ROW
        J=LAKC(NUM)     !COLUMN
        Q=0.
        Q=QLAKGW(NUM)   !(-)VE MEANS GW TO LAK; (+)VE MEANS LAK TO GW
        CONC=CNEWLAK(N,ICOMP)
C.......CONSIDER ONLY FLOW OUT OF LAKE
        IF(ICBUND(J,I,K,ICOMP).LE.0) THEN
          IF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              IF(Q.GT.0.) THEN
                GWFROMLAK=GWFROMLAK+Q*CONC
                RMASIO(26,1,ICOMP)=RMASIO(26,1,ICOMP)+Q*CONC*DTRANS
                QC7(J,I,K,7)=QC7(J,I,K,7)-Q*CONC
                QC7(J,I,K,8)=QC7(J,I,K,8)-Q
              ENDIF
            ENDIF
          ENDIF          
        ELSE
          IF(Q.GT.0.) THEN
            GWFROMLAK=GWFROMLAK+Q*CONC
            RMASIO(26,1,ICOMP)=RMASIO(26,1,ICOMP)+Q*CONC*DTRANS
          ENDIF
        ENDIF
      ENDDO
C
C--LAKE TO SFR
      DO ICNT=1,NSFR2LAK
        NSF=INOD1SFLK(ICNT)
        N=INOD2SFLK(ICNT)
        Q=0.
        Q=QSFR2LAK(ICNT)
        CONC=CNEWLAK(N,ICOMP)
        IF(Q.LT.0.) THEN
          SFRFROMLAK=SFRFROMLAK+CONC*ABS(Q)
        ENDIF
      ENDDO
C
C--OUTFLOW TO BOUNDARY CONDITIONS
      DO N=1,NLAKES
        Q=0.
        CONC=CNEWLAK(N,ICOMP)
        Q=QWDRLLAK(N)
        WDRLLK=WDRLLK+Q*CONC
        IF(IETLAK.NE.0) THEN
          Q=QETLAK(N)
          ETLK=ETLK+Q*CONC
        ENDIF
      ENDDO
C
C--CALCULATE CHANGE IN LAKE MASS STORAGE
      IF(iSSTrans.EQ.0) THEN
      DO N=1,NLAKES
        CALL TIMEINTERP(VOLNLAK,VOLOLAK,DELVOLLAK,NLAKES,N,VN,VO,DV,2)
        TOTMASOLD=VO*COLDLAK(N,ICOMP)
        TOTMASNEW=VN*CNEWLAK(N,ICOMP)
        STORDIFF=TOTMASNEW-TOTMASOLD
        IF(STORDIFF.LT.0) THEN
          STORINLK=STORINLK-STORDIFF
        ELSE
          STOROTLK=STOROTLK+STORDIFF
        ENDIF
      ENDDO
      ENDIF
C-------------------------------------------------------------------
C
C--WRITE TO ICBCLK FILE FOR TIME=0.0
      IF(KPER.EQ.1.AND.KSTP.EQ.1.AND.NTRANS.EQ.1) THEN
      IF(ICBCLK.GT.0) THEN
        DO N=1,NLAKES
          CALL TIMEINTERP(VOLNLAK,VOLOLAK,DELVOLLAK,NLAKES,N,VN,VO,DV,2)
          VOL=VO
          QC=VOL*COLDLAK(N,ICOMP)
          WRITE(ICBCLK,7) KPER,KSTP,NTRANS,0.0,N,VOL,COLDLAK(N,ICOMP),
     &                    QC,ICOMP
        ENDDO
      ENDIF
      ENDIF
C
C--WRITE TO ICBCLK FILE
      IF(ICBCLK.GT.0) THEN
        DO N=1,NLAKES
          CALL TIMEINTERP(VOLNLAK,VOLOLAK,DELVOLLAK,NLAKES,N,VN,VO,DV,2)
          VOL=VO+DV*DTRANS
          QC=VOL*CNEWLAK(N,ICOMP)
          WRITE(ICBCLK,7) KPER,KSTP,NTRANS,TIME2,N,VOL,CNEWLAK(N,ICOMP),
     &                    QC,ICOMP
        ENDDO
      ENDIF
7     FORMAT(3I10,1X,G14.7,I10,3(1X,G14.7),I10)
C
C--COMPUTE MASS FROM RATES
      GW2LAK=GW2LAK*DTRANS
      UZF2LAK=UZF2LAK*DTRANS
      GWFROMLAK=GWFROMLAK*DTRANS
      SFR2LAK=SFR2LAK*DTRANS
      SFRFROMLAK=SFRFROMLAK*DTRANS
      PRECLK=PRECLK*DTRANS
      RUNOFLK=RUNOFLK*DTRANS
      WDRLLK=WDRLLK*DTRANS
      ETLK=ETLK*DTRANS
C
C--CUMULATIVE TERMS
      CGW2LAK(ICOMP)=CGW2LAK(ICOMP)+GW2LAK
      CUZF2LAK(ICOMP)=CUZF2LAK(ICOMP)+UZF2LAK
      CGWFROMLAK(ICOMP)=CGWFROMLAK(ICOMP)+GWFROMLAK
      CSFR2LAK(ICOMP)=CSFR2LAK(ICOMP)+SFR2LAK
      CSFRFROMLAK(ICOMP)=CSFRFROMLAK(ICOMP)+SFRFROMLAK
      CPRECLK(ICOMP)=CPRECLK(ICOMP)+PRECLK
      CRUNOFLK(ICOMP)=CRUNOFLK(ICOMP)+RUNOFLK
      CWDRLLK(ICOMP)=CWDRLLK(ICOMP)+WDRLLK
      CETLK(ICOMP)=CETLK(ICOMP)+ETLK
      CSTORINLK(ICOMP)=CSTORINLK(ICOMP)+STORINLK
      CSTOROTLK(ICOMP)=CSTOROTLK(ICOMP)+STOROTLK
C
C--CALCULATE TOTAL
      TOTINLK=GW2LAK+SFR2LAK+PRECLK+RUNOFLK+STORINLK+UZF2LAK
      TOTOUTLK=GWFROMLAK+SFRFROMLAK+WDRLLK+ETLK+STOROTLK
      CTOTINLK=CGW2LAK(ICOMP)+CSFR2LAK(ICOMP)+CPRECLK(ICOMP)+
     &         CRUNOFLK(ICOMP)+CSTORINLK(ICOMP)+CUZF2LAK(ICOMP)
      CTOTOUTLK=CGWFROMLAK(ICOMP)+CSFRFROMLAK(ICOMP)+
     &          CWDRLLK(ICOMP)+CETLK(ICOMP)+CSTOROTLK(ICOMP)
C
      DIFF=TOTINLK-TOTOUTLK
      CDIFF=CTOTINLK-CTOTOUTLK
      IF(ABS(TOTINLK+TOTOUTLK).LE.1.0E-10) TOTINLK=1.0E-10
      PERC=DIFF*100/((TOTINLK+TOTOUTLK)/2.0E0)
      IF(ABS(CTOTINLK+CTOTOUTLK).LE.1.0E-10) CTOTINLK=1.0E-10
      CPERC=CDIFF*100/((CTOTINLK+CTOTOUTLK)/2.0E0)
C
C--FLOW BALANCE TERM
      DELV=0.
      IF(iSSTrans.EQ.0) THEN
      DO N=1,NLAKES
        DELV=DELV+DELVOLLAK(N)
      ENDDO
      ENDIF
      QDIFF=Q1-Q2-DELV
C
C--WRITE LAKE MASS BALANCE TO OUTPUT FILE
      IF(PRTOUT) THEN
        WRITE(IOUT,10) NTRANS,KSTP,KPER,ICOMP
        WRITE(IOUT,20) 
        WRITE(IOUT,29) CSTORINLK(ICOMP),STORINLK
        WRITE(IOUT,30) CGW2LAK(ICOMP),GW2LAK
        IF(iUnitTRNOP(7).GT.0) WRITE(IOUT,31) CUZF2LAK(ICOMP),UZF2LAK
        IF(iUnitTRNOP(19).GT.0) WRITE(IOUT,35) CSFR2LAK(ICOMP),SFR2LAK
        WRITE(IOUT,40) CPRECLK(ICOMP),PRECLK
        WRITE(IOUT,41) CRUNOFLK(ICOMP),RUNOFLK
        WRITE(IOUT,43)
        WRITE(IOUT,45) CTOTINLK,TOTINLK
        WRITE(IOUT,49) CSTOROTLK(ICOMP),STOROTLK
        WRITE(IOUT,50) CGWFROMLAK(ICOMP),GWFROMLAK
        IF(iUnitTRNOP(19).GT.0) WRITE(IOUT,55) CSFRFROMLAK(ICOMP),
     1  SFRFROMLAK
        WRITE(IOUT,60) CWDRLLK(ICOMP),WDRLLK
        IF(IETLAK.NE.0) WRITE(IOUT,61) CETLK(ICOMP),ETLK
        WRITE(IOUT,43)
        WRITE(IOUT,65) CTOTOUTLK,TOTOUTLK
        WRITE(IOUT,70) CDIFF,DIFF
        WRITE(IOUT,75) CPERC,PERC
        WRITE(IOUT,80) QDIFF
      ENDIF
10    FORMAT(//21X,'LAKE MASS BUDGETS AT END OF TRANSPORT STEP',
     &         I5,', TIME STEP',I5,', STRESS PERIOD',I5,' FOR ',
     &            'COMPONENT',I4,
     &       /21X,101('-'))
20    FORMAT(/33X,7X,1X,'CUMULATIVE MASS [M]',
     &         8X,13X,15X,' MASS FOR THIS TIME STEP [M]',
     &       /41X,19('-'),36X,14('-'))
29    FORMAT(16X,'        LAKE DEPLETION =',G15.7,
     &       16X,'        LAKE DEPLETION =',G15.7)
30    FORMAT(16X,'            GW TO LAKE =',G15.7,
     &       16X,'            GW TO LAKE =',G15.7)
31    FORMAT(16X,'           UZF TO LAKE =',G15.7,
     &       16X,'           UZF TO LAKE =',G15.7)
35    FORMAT(16X,'        STREAM TO LAKE =',G15.7,
     &       16X,'        STREAM TO LAKE =',G15.7)
40    FORMAT(16X,'         PRECIPITATION =',G15.7,
     &       16X,'         PRECIPITATION =',G15.7)
41    FORMAT(16X,'                RUNOFF =',G15.7,
     &       16X,'                RUNOFF =',G15.7)
43    FORMAT(41X,19('-'),36X,14('-'))
45    FORMAT(16X,'              TOTAL IN =',G15.7,
     &       16X,'              TOTAL IN =',G15.7)
49    FORMAT(/16X,'     LAKE ACCUMULATION =',G15.7,
     &        16X,'     LAKE ACCUMULATION =',G15.7)
50    FORMAT(16X,'            LAKE TO GW =',G15.7,
     &       16X,'            LAKE TO GW =',G15.7)
55    FORMAT(16X,'        LAKE TO STREAM =',G15.7,
     &       16X,'        LAKE TO STREAM =',G15.7)
60    FORMAT(16X,'            WITHDRAWAL =',G15.7,
     &       16X,'            WITHDRAWAL =',G15.7)
61    FORMAT(16X,'           EVAPORATION =',G15.7,
     &       16X,'           EVAPORATION =',G15.7)
65    FORMAT(16X,'             TOTAL OUT =',G15.7,
     &       16X,'             TOTAL OUT =',G15.7)
70    FORMAT(/16X,'        NET (IN - OUT) =',G15.7,
     &        16X,'        NET (IN - OUT) =',G15.7)
75    FORMAT(16X,' DISCREPANCY (PERCENT) =',G15.7,
     &       16X,' DISCREPANCY (PERCENT) =',G15.7)
80    FORMAT(46X,'FLOW ERR (QIN-QOUT-DV) =',G15.7,' [L3/T]',/)
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE LKT1AD(N)
C***********************************************************************
C     RESET LAKE CONCENTRATIONS AND VOLUMES
C***********************************************************************
      USE LAKVARS
      INTEGER N
C
C--RESET LAKE CONCENTRATION
      COLDLAK=CNEWLAK
C
      RETURN
      END
C
C
      SUBROUTINE LKT1AD2(N)
C***********************************************************************
C     RESET LAKE CONCENTRATIONS AND VOLUMES
C***********************************************************************
      USE LAKVARS
      INTEGER N
C
C--RESET LAKE VOLUME
      VOLOLAK=VOLNLAK
C
      RETURN
      END
C
C
