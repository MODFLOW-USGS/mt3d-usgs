C
      SUBROUTINE RCT5AL(INRCT,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,NCOMP,
     & ISOTHM,IREACT,IRCTOP,IGETSC,LCRHOB,LCPRSITY2,LCRETA2,LCFRAC,
     & LCSP1,LCSP2,LCRC1,LCRC2)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED BY THE CHEMICAL 
C REACTION (RCT) PACKAGE.
C **********************************************************************
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   INRCT,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,NCOMP,
     &          ISOTHM,IREACT,IGETSC,LCRHOB,LCPRSITY2,LCFRAC,
     &          LCSP1,LCSP2,LCRC1,LCRC2,LCRETA2,
     &          NODES,ISUMX,ISUMIX,ISOLD,ISOLD2,IRCTOP,IERR
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1000) INRCT
 1000 FORMAT(1X,'RCT5 -- CHEMICAL REACTION PACKAGE,',
     & ' VERSION 5, FEBRUARY 2010, INPUT READ FROM UNIT',I3)
C
C--READ AND ECHO SORPTION ISOTHERM TYPE AND FLAG IREACT
      READ(INRCT,'(4I10)',ERR=100,IOSTAT=IERR)
     & ISOTHM,IREACT,IRCTOP,IGETSC
  100 IF(IERR.NE.0) THEN
        IRCTOP=1
        IGETSC=0
        BACKSPACE (INRCT)
        READ(INRCT,'(2I10)') ISOTHM,IREACT
      ENDIF
C
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
      ELSE
        WRITE(IOUT,1028)
      ENDIF
      IF(IREACT.EQ.0) THEN
        WRITE(IOUT,1030)
      ELSEIF(ireact.eq.1) THEN
        WRITE(IOUT,1032)
      ELSEIF(ireact.eq.100) THEN
        WRITE(IOUT,1034)
      ENDIF
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
 1034 FORMAT(1X,'ZEROTH-ORDER DECAY OR PRODUCTION IS SIMULATED')     
C
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
C--ALLOCATE SPACE FOR ARRAYS
      ISOLD=ISUM
      ISOLD2=ISUM2
      NODES=NCOL*NROW*NLAY
C
C--REAL ARRAYS
      LCRHOB=ISUM
      IF(ISOTHM.NE.0) ISUM=ISUM+NODES
      LCPRSITY2=ISUM
      IF(ISOTHM.EQ.5.OR.ISOTHM.EQ.6) ISUM=ISUM+NODES
      LCFRAC=ISUM
      IF(ISOTHM.EQ.5.OR.ISOTHM.EQ.6) ISUM=ISUM+NODES
      LCRETA2=ISUM
      IF(ISOTHM.EQ.5.OR.ISOTHM.EQ.6) ISUM=ISUM+NODES * NCOMP
      LCSP1=ISUM
      IF(ISOTHM.NE.0) ISUM=ISUM+NODES * NCOMP
      LCSP2=ISUM
      IF(ISOTHM.NE.0) ISUM=ISUM+NODES * NCOMP
      LCRC1=ISUM
      IF(IREACT.NE.0) ISUM=ISUM+NODES * NCOMP
      LCRC2=ISUM
      IF(IREACT.NE.0) ISUM=ISUM+NODES * NCOMP
C
C--CHECK WHETHER ARRAYS X AND IX ARE DIMENSIONED LARGE ENOUGH
      ISUMX=ISUM-ISOLD
      ISUMIX=ISUM2-ISOLD2
      WRITE(IOUT,1090) ISUMX,ISUMIX
 1090 FORMAT(1X,I10,' ELEMENTS OF THE  X ARRAY USED BY THE RCT PACKAGE'
     & /1X,I10,' ELEMENTS OF THE IX ARRAY USED BY THE RCT PACKAGE'/)
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE RCT5RP(IN,IOUT,NCOL,NROW,NLAY,NCOMP,ICBUND,COLD,
     & PRSITY,ISOTHM,IREACT,IRCTOP,IGETSC,RHOB,SP1,SP2,SRCONC,
     & RC1,RC2,RETA,BUFF,PRSITY2,RETA2,FRAC,RFMIN,IFMTRF,DTRCT)
C **********************************************************************
C THIS SUBROUTINE READS AND PREPARES INPUT DATA NEEDED BY THE CHEMICAL
C REACTION (RCT) PACKAGE.
C***********************************************************************
C last modified: 10-01-2005
C
      IMPLICIT  NONE
      INTEGER   IN,IOUT,NCOL,NROW,NLAY,ICBUND,ISOTHM,IREACT,IFMTRF,
     &          J,I,K,JR,IR,KR,IRCTOP,NCOMP,IGETSC,INDEX
      REAL      COLD,PRSITY,RHOB,SP1,SP2,SRCONC,RC1,RC2,RETA,RFMIN,
     &          DTRCT,TR,BUFF,PRSITY2,FRAC,TINY,EPSILON,TOTPOR,RETA2
      CHARACTER ANAME*24
      DIMENSION ICBUND(NCOL,NROW,NLAY,NCOMP),COLD(NCOL,NROW,NLAY,NCOMP),
     &          PRSITY(NCOL,NROW,NLAY),RETA(NCOL,NROW,NLAY,NCOMP),
     &          RHOB(NCOL,NROW,NLAY),PRSITY2(NCOL,NROW,NLAY),
     &          FRAC(NCOL,NROW,NLAY),SRCONC(NCOL,NROW,NLAY,NCOMP),
     &          SP1(NCOL,NROW,NLAY,NCOMP),SP2(NCOL,NROW,NLAY,NCOMP),
     &          RC1(NCOL,NROW,NLAY,NCOMP),RC2(NCOL,NROW,NLAY,NCOMP),
     &          RETA2(NCOL,NROW,NLAY,NCOMP),BUFF(NCOL*NROW*NLAY)
      PARAMETER (TINY=1.E-30,EPSILON=0.5E-6)
C
C--PRINT A HEADER
      WRITE(IOUT,1000)
 1000 FORMAT(//1X,'SORPTION AND 1ST/0TH ORDER REACTION PARAMETERS',
     & /1X,46('-')/)
C
C--CALL RARRAY TO READ IN SORPTION PARAMETERS IF SORPTION SIMULATED
      IF(ISOTHM.LE.0) GOTO 2000
C
      IF(ISOTHM.EQ.5) GOTO 111
      ANAME='BULK DENSITY (RHOB)    '
      IF(IRCTOP.EQ.2) THEN
        DO K=1,NLAY
          CALL RARRAY(RHOB(1,1,K),ANAME,NROW,NCOL,K,IN,IOUT)
        ENDDO
      ELSEIF(IRCTOP.EQ.1) THEN
        CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              RHOB(J,I,K)=BUFF(K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
  111 CONTINUE
C
      IF(ISOTHM.NE.5.AND.ISOTHM.NE.6) GOTO 222
      ANAME='IMMOBILE DOMAIN POROSITY'
      IF(IRCTOP.EQ.2) THEN
        DO K=1,NLAY
          CALL RARRAY(PRSITY2(1,1,K),ANAME,NROW,NCOL,K,IN,IOUT)
        ENDDO
      ELSEIF(IRCTOP.EQ.1) THEN
        CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              PRSITY2(J,I,K)=BUFF(K)
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
            CALL RARRAY(SRCONC(1,1,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                SRCONC(J,I,K,INDEX)=BUFF(K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO
  333 CONTINUE
C
      DO INDEX=1,NCOMP
        ANAME='1ST SORP. COEF. COMP. NO'
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        IF(IRCTOP.EQ.2) THEN
          DO K=1,NLAY
            CALL RARRAY(SP1(1,1,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                SP1(J,I,K,INDEX)=BUFF(K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
      DO INDEX=1,NCOMP
        ANAME='2ND SORP. COEF. COMP. NO'
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        IF(IRCTOP.EQ.2) THEN
          DO K=1,NLAY
            CALL RARRAY(SP2(1,1,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)
            DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                SP2(J,I,K,INDEX)=BUFF(K)
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
      IF(IREACT.ne.1.and.IREACT.ne.100) GOTO 3000
C
      DO INDEX=1,NCOMP
        ANAME='SOLUTE RXN RATE: COMP NO'
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        IF(IRCTOP.EQ.2) THEN
          DO K=1,NLAY
            CALL RARRAY(RC1(1,1,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                RC1(J,I,K,INDEX)=BUFF(K)
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
            CALL RARRAY(RC2(1,1,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT)
          ENDDO
        ELSEIF(IRCTOP.EQ.1) THEN
          CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                RC2(J,I,K,INDEX)=BUFF(K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
 3000 CONTINUE
C
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
          CALL SRCT5R(NCOL,NROW,NLAY,ICBUND(1,1,1,INDEX),PRSITY,
     &     COLD(1,1,1,INDEX),RETA(1,1,1,INDEX),RFMIN,RHOB,
     &     SP1(1,1,1,INDEX),SP2(1,1,1,INDEX),RC1(1,1,1,INDEX),
     &     RC2(1,1,1,INDEX),PRSITY2,RETA2(1,1,1,INDEX),FRAC,
     &     SRCONC(1,1,1,INDEX),ISOTHM,IREACT,TR)
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
          CALL RPRINT(RETA(1,1,K,INDEX),
     &     ANAME,0,1,1,NCOL,NROW,K,IFMTRF,IOUT)
        ENDDO
      ENDDO
C
C--RETURN
 5000 RETURN
      END
C
C
      SUBROUTINE SRCT5R(NCOL,NROW,NLAY,ICBUND,PRSITY,COLD,RETA,RFMIN,
     & RHOB,SP1,SP2,RC1,RC2,PRSITY2,RETA2,FRAC,SRCONC,
     & ISOTHM,IREACT,DTRANS)
C ********************************************************************
C THIS SUBROUTINE CALCULATES RETARDATION FACTOR AND CONCENTRATION
C OF SORBED (UNIT: MASS/MASS) FOR SINGLE-DOMAIN MODEL OR
C IMMOBILE-LIQUID PHASE (UNIT: MASS/VOLUME) FOR DUAL-DOMAIN MODEL.
C ********************************************************************
C last modified: 10-01-2005
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,ICBUND,ISOTHM,IREACT,J,I,K
      REAL      PRSITY,COLD,RETA,RFMIN,RHOB,SP1,SP2,RC1,RC2,
     &          PRSITY2,FRAC,SRCONC,DTRANS,TINY,
     &          RETA2,TERM1,RC1TMP,RC2TMP
      DIMENSION PRSITY(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),
     &          COLD(NCOL,NROW,NLAY),RETA(NCOL,NROW,NLAY),
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
              SRCONC(J,I,K)=SP1(J,I,K)*COLD(J,I,K)
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
              IF(COLD(J,I,K).LE.0) THEN
                RETA(J,I,K)=1.
                SRCONC(J,I,K)=0.
              ELSE
                RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*
     &           SP1(J,I,K)*SP2(J,I,K)*COLD(J,I,K)**(SP2(J,I,K)-1.)
                SRCONC(J,I,K)=SP1(J,I,K)*COLD(J,I,K)**SP2(J,I,K)
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
              IF(COLD(J,I,K).LT.0) THEN
                RETA(J,I,K)=1.
                SRCONC(J,I,K)=0.
              ELSE
                RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*
     &           SP1(J,I,K)*SP2(J,I,K)/(1.+SP1(J,I,K)*COLD(J,I,K))**2
                SRCONC(J,I,K)=SP1(J,I,K)*SP2(J,I,K)*COLD(J,I,K)
     &           /(1.+SP1(J,I,K)*COLD(J,I,K))
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
              IF(IREACT.eq.1.or.IREACT.eq.100) RC2TMP=RC2(J,I,K)
C--if with no reaction or with first-order reaction               
              if(ireact.eq.0.or.ireact.eq.1) then  
                SRCONC(J,I,K)=(SP2(J,I,K)*COLD(J,I,K)+
     &           RHOB(J,I,K)/DTRANS*SRCONC(J,I,K))/
     &           (RHOB(J,I,K)/DTRANS+SP2(J,I,K)/SP1(J,I,K)
     &           +RC2TMP*RHOB(J,I,K))
C--if with zeroth-order reaction      
              elseif(ireact.eq.100) then 
                SRCONC(J,I,K)=(SP2(J,I,K)*COLD(J,I,K)+
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
              RETA(J,I,K)=1.+FRAC(J,I,K)*RHOB(J,I,K)
     &                       *SP1(J,I,K)/PRSITY(J,I,K)
              RFMIN=MIN(RFMIN,RETA(J,I,K))
              RETA2(J,I,K)=1.0
              IF(PRSITY2(J,I,K).GT.TINY)
     &         RETA2(J,I,K)=1.+(1.-FRAC(J,I,K))
     &         *RHOB(J,I,K)*SP1(J,I,K)/PRSITY2(J,I,K)
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
                SRCONC(J,I,K)=(SP2(J,I,K)*COLD(J,I,K)
     &           +PRSITY2(J,I,K)*RETA2(J,I,K)/DTRANS*SRCONC(J,I,K))
     &           /TERM1
C--if with zeroth-order reaction     
              elseif(ireact.eq.100) then 
                TERM1=PRSITY2(J,I,K)*RETA2(J,I,K)/DTRANS+SP2(J,I,K)
                SRCONC(J,I,K)=(SP2(J,I,K)*COLD(J,I,K)
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
      SUBROUTINE RCT5FM(NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,PRSITY,
     & DELR,DELC,DH,ISOTHM,IREACT,RHOB,SP1,SP2,SRCONC,RC1,RC2,
     & PRSITY2,RETA2,FRAC,A,RHS,NODES,UPDLHS,DTRANS)
C *******************************************************************
C THIS SUBROUTINE FORMULATES THE COEFFICIENT MATRIX [A] AND THE 
C RIGHT-HAND-SIDE MATRIX [RHS] FOR SORPTION AND 1ST/0TH ORDER 
C REACTION TERMS USING THE IMPLICIT FINITE-DIFFERENCE SCHEME.
C *******************************************************************
C last modified: 10-01-2005
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,ISOTHM,IREACT,
     &          K,I,J,N,NODES
      REAL      PRSITY,RHOB,SP1,SP2,RC1,RC2,PRSITY2,FRAC,DTRANS,
     &          SRCONC,DELR,DELC,DH,A,RHS,RETA2,TERM1,TINY,
     &          RC1TMP,RC2TMP
      LOGICAL   UPDLHS
      DIMENSION ICBUND(NODES,NCOMP),PRSITY(NODES),
     &          RHOB(NODES),SP1(NODES,NCOMP),SP2(NODES,NCOMP),
     &          RC1(NODES,NCOMP),RC2(NODES,NCOMP),SRCONC(NODES,NCOMP),
     &          DELR(NCOL),DELC(NROW),DH(NODES),A(NODES),RHS(NODES),
     &          PRSITY2(NODES),RETA2(NODES,NCOMP),FRAC(NODES)
      PARAMETER (TINY=1.E-30)
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
              IF(IREACT.eq.1.or.IREACT.eq.100) RC2TMP=RC2(N,ICOMP)
C--if with no reaction or with first-order reaction             
              if(ireact.eq.0.or.ireact.eq.1) then
                IF(UPDLHS) A(N)=A(N)-SP2(N,ICOMP)*DELR(J)*DELC(I)
     &           *DH(N)*(1.-SP2(N,ICOMP)/SP1(N,ICOMP)
     &           /(RHOB(N)/DTRANS+SP2(N,ICOMP)/SP1(N,ICOMP)
     &           +RC2TMP*RHOB(N)))
                RHS(N)=RHS(N)-SP2(N,ICOMP)/SP1(N,ICOMP)*DELR(J)*DELC(I)
     &           *DH(N)*RHOB(N)*SRCONC(N,ICOMP)/DTRANS
     &           /(RHOB(N)/DTRANS+SP2(N,ICOMP)/SP1(N,ICOMP)
     &           +RC2TMP*RHOB(N))
C--if with zeroth-order reaction     
              elseif(ireact.eq.100) then
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
                IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*FRAC(N)*
     &           RHOB(N)*DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP)
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
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE RCT5BD(NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,PRSITY,
     & DELR,DELC,DH,DTRANS,ISOTHM,IREACT,RHOB,SP1,SP2,SRCONC,RC1,
     & RC2,PRSITY2,RETA2,FRAC,CNEW,RETA,RFMIN,RMASIO)
C **********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGET ASSOCIATED WITH REACTIONS.
C **********************************************************************
C last modified: 10-01-2005
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,ISOTHM,IREACT,K,I,J
      REAL      PRSITY,DTRANS,RHOB,SP1,SP2,RC1,RC2,RETA,RFMIN,RETA2,
     &          PRSITY2,FRAC,CNEW,RMASIO,DCRCT,SRCONC,DELR,DELC,DH,
     &          CMML,CMMS,CIML,CIMS,VOLUME,DCRCT2
      DIMENSION ICBUND(NCOL,NROW,NLAY,NCOMP),PRSITY(NCOL,NROW,NLAY),
     &          RHOB(NCOL,NROW,NLAY),RETA(NCOL,NROW,NLAY,NCOMP),
     &          PRSITY2(NCOL,NROW,NLAY),FRAC(NCOL,NROW,NLAY),
     &          SP1(NCOL,NROW,NLAY,NCOMP),SP2(NCOL,NROW,NLAY,NCOMP),
     &          RC1(NCOL,NROW,NLAY,NCOMP),RC2(NCOL,NROW,NLAY,NCOMP),
     &          CNEW(NCOL,NROW,NLAY,NCOMP),SRCONC(NCOL,NROW,NLAY,NCOMP),
     &          RETA2(NCOL,NROW,NLAY,NCOMP),DELR(NCOL),DELC(NROW),
     &          DH(NCOL,NROW,NLAY),RMASIO(122,2,NCOMP)
C
C--UPDATE RETARDATION FACTOR AND SORBED/IMMOBILE-PHASE CONCENTRATION
C
      IF(ISOTHM.GT.0) THEN
        CALL SRCT5R(NCOL,NROW,NLAY,ICBUND(1,1,1,ICOMP),PRSITY,
     &   CNEW(1,1,1,ICOMP),RETA(1,1,1,ICOMP),RFMIN,RHOB,
     &   SP1(1,1,1,ICOMP),SP2(1,1,1,ICOMP),RC1(1,1,1,ICOMP),
     &   RC2(1,1,1,ICOMP),PRSITY2,RETA2(1,1,1,ICOMP),FRAC,
     &   SRCONC(1,1,1,ICOMP),ISOTHM,IREACT,DTRANS)
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
C--CALCULATE SOLUTE MASS CHANGE
              DCRCT=-SP2(J,I,K,ICOMP)*(CNEW(J,I,K,ICOMP)
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
C--CALCULATE CHANGE IN CONCENTRATION OF MOBILE-LIQUID PHASE
              DCRCT=-SP2(J,I,K,ICOMP)*(CNEW(J,I,K,ICOMP)
     &         -SRCONC(J,I,K,ICOMP))*DTRANS
     &         *DELR(J)*DELC(I)*DH(J,I,K)
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
      IF(IREACT.ne.1.and.IREACT.ne.100) goto 9999
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
C--SKIP IF CONCENTRATION IS NOT POSITIVE
            IF(CNEW(J,I,K,ICOMP).LE.0) CYCLE         
C
C--DISSOLVED PHASE
            IF(ireact.eq.1) THEN
              DCRCT=-RC1(J,I,K,ICOMP)*CNEW(J,I,K,ICOMP)
     &            *DTRANS*DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)
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
C--UPDATE SORBED MASS STORAGE CHANGE FOR NONEQUILIBRIUM SORPTION
            IF(ISOTHM.EQ.4.AND.DCRCT2.GT.0) THEN
              RMASIO(120,2,ICOMP)=RMASIO(120,2,ICOMP)-DCRCT2
            ELSEIF(ISOTHM.EQ.4.AND.DCRCT2.LT.0) THEN
              RMASIO(120,1,ICOMP)=RMASIO(120,1,ICOMP)-DCRCT2
            ENDIF
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
C--SKIP IF CONCENTRATION IS NOT POSITIVE
            IF(CNEW(J,I,K,ICOMP).LE.0) CYCLE          
C
C--compute mass loss/gain in each cell for all 4 phases: 
C--mobile liquid, mobile sorbed, immobile liquid, immobile sorbed
            VOLUME=DELR(J)*DELC(I)*DH(J,I,K)
            CMML=CNEW(J,I,K,ICOMP)*PRSITY(J,I,K)*VOLUME
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
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE RCT5CF(NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,PRSITY,
     & COLD,RETA,RFMIN,RHOB,SP1,SP2,RC1,RC2,PRSITY2,RETA2,FRAC,
     & SRCONC,ISOTHM,IREACT,DTRANS)
C ********************************************************************
C THIS SUBROUTINE UPDATES NONLINEAR REACTION COEFFICIENTS.
C ********************************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,ISOTHM,IREACT
      REAL      PRSITY,COLD,RETA,RFMIN,RHOB,SP1,SP2,SRCONC,DTRANS,
     &          RC1,RC2,PRSITY2,RETA2,FRAC
      DIMENSION PRSITY(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY,NCOMP),
     &          COLD(NCOL,NROW,NLAY,NCOMP),RETA(NCOL,NROW,NLAY,NCOMP),
     &          RHOB(NCOL,NROW,NLAY),SRCONC(NCOL,NROW,NLAY,NCOMP),
     &          SP1(NCOL,NROW,NLAY,NCOMP),SP2(NCOL,NROW,NLAY,NCOMP),
     &          RC1(NCOL,NROW,NLAY,NCOMP),RC2(NCOL,NROW,NLAY,NCOMP),
     &          PRSITY2(NCOL,NROW,NLAY),FRAC(NCOL,NROW,NLAY),
     &          RETA2(NCOL,NROW,NLAY,NCOMP)
C
      IF(ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
        CALL SRCT5R(NCOL,NROW,NLAY,ICBUND(1,1,1,ICOMP),PRSITY,
     &   COLD(1,1,1,ICOMP),RETA(1,1,1,ICOMP),RFMIN,RHOB,
     &   SP1(1,1,1,ICOMP),SP2(1,1,1,ICOMP),RC1(1,1,1,ICOMP),
     &   RC2(1,1,1,ICOMP),PRSITY2,RETA2(1,1,1,ICOMP),FRAC,
     &   SRCONC(1,1,1,ICOMP),ISOTHM,IREACT,DTRANS)
      ENDIF
C
C--RETURN
      RETURN
      END