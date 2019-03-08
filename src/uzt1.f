C
      SUBROUTINE UZT1AR(INUZT)
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES SPACE FOR UZT VARIABLES
C***********************************************************************
      USE UZTVARS
      USE MT3DMS_MODULE, ONLY: IOUT,NCOMP,NCOL,NROW,DZ,DH,ICBUND,PRSITY,
     &                         NLAY,NCOL,NROW,iUnitTRNOP
C
      IMPLICIT NONE
      INTEGER         INUZT,IET,I,J,K,IERR
      CHARACTER       LINE*180,ANAME*24,BNAME*24
      LOGICAL         IUZFBND_CHK
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INUZT
 1030 FORMAT(/1X,'UZT1 -- UNSATURATED-ZONE TRANSPORT PACKAGE,',
     &           ' VERSION 1, MAY 2016, INPUT READ FROM UNIT',I3)
C
C--ALLOCATE VARIABLES USED IN FMI
      ALLOCATE(ICBCUZ,IETFLG,IUZFOPTG)
      IETFLG=.FALSE.
      IUZFOPTG=1
C--READ HEADER LINE (RECORD #1)
      ALLOCATE(IUZFBND(NCOL,NROW))
 9    READ(iUnitTRNOP(7),'(A)',IOSTAT=IERR) LINE
      IF(LINE(1:1).EQ.'#') THEN 
        GOTO 9
      ELSE
        BACKSPACE(iUnitTRNOP(7))
      ENDIF
C
C--READ RECORD #2, FIRST NON-COMMENT LINE IN UZT
      READ(INUZT,*) ICBCUZ,IET
      IF(IET.EQ.1) THEN
        IETFLG=.TRUE.
      ELSE
        IETFLG=.FALSE.
      ENDIF
      IF(IETFLG) THEN
        WRITE(IOUT,'(A)') 'ET IS BEING SIMULATED, READ UZET AND ',
     &                    'GWET TERMS'
      ELSE
        WRITE(IOUT,'(A)') 'ET IS NOT BEING SIMULATED'
      ENDIF
C
C--ALLOCATE INITIAL AND BOUNDARY CONDITION ARRAYS
      ALLOCATE(IUZFOPT(NCOL,NROW))            
      ALLOCATE(IUZFBND(NCOL,NROW))            
      ALLOCATE(UZFLX(NCOL,NROW,NLAY))         
      ALLOCATE(UZQSTO(NCOL,NROW,NLAY))        
      ALLOCATE(FINFIL(NCOL,NROW))             
      ALLOCATE(CUZINF(NCOL,NROW,NCOMP))       
      ALLOCATE(UZET(NCOL,NROW,NLAY))          
      ALLOCATE(CUZET(NCOL,NROW,NLAY,NCOMP))   
      ALLOCATE(GWET(NCOL,NROW))          
      ALLOCATE(IGWET(NCOL,NROW))
      ALLOCATE(CGWET(NCOL,NROW,NCOMP))
C
      ALLOCATE(SATOLD(NCOL,NROW,NLAY)) 
      ALLOCATE(SATNEW(NCOL,NROW,NLAY)) 
      ALLOCATE(WC(NCOL,NROW,NLAY))     
      ALLOCATE(THETAW(NCOL,NROW,NLAY))
      ALLOCATE(SDH(NCOL,NROW,NLAY))    
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            THETAW(J,I,K)=PRSITYSAV(J,I,K)
            SATOLD(J,I,K)=1.0
            SATNEW(J,I,K)=1.0
          ENDDO
        ENDDO
      ENDDO
C
C--INITIALIZE IUZFBND ARRAY
      DO I=1,NROW
        DO J=1,NCOL
          IUZFBND(J,I)=0
        ENDDO
      ENDDO
C
C--READ THE IUZFBND ARRAY (RECORD #3)                         
      BNAME=' AREAL EXTENT OF UZ FLOW'
      CALL IARRAY(IUZFBND,BNAME,NROW,NCOL,0,iUnitTRNOP(7),IOUT)
C--Adjust IUZFOPTG (IUZFOPT-Global) according to what is stored in 
C--the IUZFBND array                                               
      IF(IUZFOPTG.GT.0) THEN                                     
        IUZFBND_CHK = .FALSE.                                    
        DO I=1,NROW                                              
          DO J=1,NCOL                                            
            IF(IUZFBND(J,I).GT.0) THEN                      
              IUZFBND_CHK = .TRUE.                               
            ENDIF                                                
          ENDDO                                                  
        ENDDO                                                    
C--If the check on IUZFBND called IUZFBND_CHK remains false,     
C--then the value of IUZFOPT can be set equal to zero, since all 
C--cells are effectively acting as though IUZFOPT=0              
        IF(IUZFBND_CHK) THEN                                
          IUZFOPTG = 0                                           
        ENDIF                                                    
      ELSE                                                       
        IETFLG=.FALSE.                                           
      ENDIF
C
C--CALL RARRAY TO READ IN STARTING WATER CONTENT (RECORD #4)
C      ANAME='           WATER CONTENT'
C      DO K=1,NLAY
C       CALL RARRAY(WC(1:NCOL,1:NROW,K),ANAME,NROW,NCOL,K,INUZT,IOUT)
C      ENDDO
C--CALL RARRAY TO READ IN STARTING SATURATED THICKNESS (RECORD #5)
C      ANAME='STARTING SATURATED THICKNESS'
C      DO K=1,NLAY
C       CALL RARRAY(SDH(1:NCOL,1:NROW,K),ANAME,NROW,NCOL,K,INUZT,IOUT)
C      ENDDO
C
C--CALCULATE SATURATION AND STORE IN SATOLD
C      IF(iUnitTRNOP(7).GT.0) THEN
C        DO K=1,NLAY
C          DO I=1,NROW
C            DO J=1,NCOL
C              IF(ICBUND(J,I,K,1).GT.0) THEN
C                DH(J,I,K)=SDH(J,I,K)
C                SATOLD(J,I,K)=((DZ(J,I,K)-DH(J,I,K))/DZ(J,I,K))*
C     &                          WC(J,I,K)/PRSITY(J,I,K)+
C     &                          DH(J,I,K)/DZ(J,I,K)*1
C                THETAW(J,I,K)=SATOLD(J,I,K)*PRSITY(J,I,K)  
C              ENDIF
C            ENDDO
C          ENDDO
C        ENDDO
C      ENDIF
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE UZT1RP(KPER)
C***********************************************************************
C     THIS SUBROUTINE READS UZT VARIABLES - INITIAL CONCS
C***********************************************************************
      USE UZTVARS
      USE MT3DMS_MODULE, ONLY: INUZT,IOUT,NCOMP,NLAY,NROW,NCOL
C
      IMPLICIT NONE
      CHARACTER ANAME*24
      INTEGER   KPER,INDEX,KK,II,JJ,INCUZINF,INCUZET,INCGWET
C
C--PRINT A HEADER
      WRITE(IOUT,1)
 1    FORMAT(//1X,'UNSATURATED-ZONE INPUT PARAMETERS'/1X,23('-')/)
C
C--READ CONCENTRATION OF INFILTRATING FLUX (CUZINF)
C  READ FLAG INDICATING HOW TO READ APPLD AMT CONC.
      READ(INUZT,'(I10)') INCUZINF                                     
C                                                                   
C--IF INCUZINF<0, CONC.REUSED FROM LAST STRESS PERIOD               
      IF(INCUZINF.LT.0) THEN                                        
        WRITE(IOUT,3)                                               
        GOTO 14                                                     
      ENDIF                                                         
    3 FORMAT(/1X,'CONCENTRATION OF APPLIED WATER',                  
     &           ' REUSED FROM LAST STRESS PERIOD')
C                                                                   
C--IF INCUINF>=0, READ AN ARRAY CONTAINING CONC. OF APPL.           
C--WATER [CIUZNF]                                                   
      WRITE(IOUT,4) KPER                                            
      ANAME='APPLD CONC. COMP.NO.'                                  
      DO INDEX=1,NCOMP                                              
        WRITE(ANAME(19:21),'(I3.2)') INDEX                          
        CALL RARRAY(CUZINF(:,:,INDEX),ANAME,NROW,NCOL,              
     &              0,INUZT,IOUT)                                      
      ENDDO                                                         
    4 FORMAT(/1X,'CONCENTRATION OF APPLIED WATER',                  
     &           ' WILL BE READ IN STRESS PERIOD',I3) 
C
C--READ CONCENTRATION OF ET FLUX WHEN UZF ET IS BEING               
C--SIMULATED.                                                       
C--CUZET & CGWET should be kept              
C--separate and 'specifiable' for maximum code flexibility.           
   14 IF(.NOT.IETFLG) GOTO 17                                       
C                                                                   
      IF(KPER.EQ.1) THEN                                            
        DO INDEX=1,NCOMP                                            
          DO KK=1,NLAY                                              
            DO II=1,NROW                                            
              DO JJ=1,NCOL                                          
                CUZET(JJ,II,KK,INDEX)=-1.E-30                       
              ENDDO                                                 
            ENDDO                                                   
          ENDDO                                                     
        ENDDO                                                       
      ENDIF                                                         
      READ(INUZT,'(I10)') INCUZET                                      
      IF(INCUZET.LT.0) THEN                                         
        WRITE(IOUT,15)                                              
        GOTO 17                                                     
      ENDIF                                                         
   15 FORMAT(/1X,'CONCENTRATION OF UZET FLUXES',                    
     &           ' REUSED FROM LAST STRESS PERIOD')
C                                                                   
      WRITE(IOUT,16) KPER                                           
      ANAME='UZET. CONC. COMP. NO.'                                 
      DO INDEX=1,NCOMP                                              
        WRITE(ANAME(19:21),'(I3.2)') INDEX                          
C--BECAUSE UZET CAN BE WITHDRAWN FROM MULTIPLE LAYERS,              
C--AN 'RARRAY_UZ' FUNCTION MAY NEED TO BE ADDED.  BUT FOR NOW I'LL  
C--ASSUME THAT CONSTANT VALUES WILL BE UTILIZED RELIEVING THE NEED  
C--TO ADDRESS THIS RIGHT NOW.                                       
        CALL RARRAY(CUZET(:,:,:,INDEX),ANAME,NROW,NCOL,             
     &              NLAY,INUZT,IOUT)                                   
C--AFTER READING IN TOP LAYERS CUZET, COPY IT TO THE REMAINING      
C--LAYERS                                                           
        DO KK=2,NLAY                                                
          DO II=1,NROW                                              
            DO JJ=1,NCOL                                            
              CUZET(JJ,II,KK,INDEX)=CUZET(JJ,II,1,INDEX)            
            ENDDO                                                   
          ENDDO                                                   
        ENDDO                                                     
      ENDDO                                                       
   16 FORMAT(/1X,'CONCENTRATION OF UZET FLUXES',                  
     &           ' WILL BE READ IN STRESS PERIOD',I3)
C                                                                 
C--READ CONCENTRATION OF GWET FLUX WHEN UZF ET IS BEING           
C--SIMULATED.                                                     
   17 IF(.NOT.IETFLG) GOTO 20                                     
C                                                                 
      IF(KPER.EQ.1) THEN                                          
        DO INDEX=1,NCOMP                                          
          DO II=1,NROW                                          
            DO JJ=1,NCOL                                        
              CGWET(JJ,II,INDEX)=-1.E-30                     
            ENDDO                                               
          ENDDO                                                 
        ENDDO                                                     
      ENDIF                                                       
      READ(INUZT,'(I10)') INCGWET                                    
      IF(INCGWET.LT.0) THEN                                       
        WRITE(IOUT,18)                                            
        GOTO 20                                                   
      ENDIF                                                       
   18 FORMAT(/1X,'CONCENTRATION OF GWET FLUXES',                  
     &           ' REUSED FROM LAST STRESS PERIOD')  
C                                                                 
      WRITE(IOUT,19) KPER                                         
      ANAME='GWET. CONC. COMP. NO.'                               
      DO INDEX=1,NCOMP                                            
        WRITE(ANAME(19:21),'(I3.2)') INDEX                        
        CALL RARRAY(CGWET(:,:,INDEX),ANAME,NROW,NCOL,           
     &              1,INUZT,IOUT)                                 
      ENDDO                                                       
   19 FORMAT(/1X,'CONCENTRATION OF GWET FLUXES',                  
     &           ' WILL BE READ IN STRESS PERIOD',I3)
C
C--RETURN
   20 RETURN
      END
C
C
      SUBROUTINE UZT1FM(ICOMP)
C ******************************************************************
C THIS SUBROUTINE FORMULATES MATRIX COEFFICIENTS FOR THE UZT SINK/
C SOURCE TERMS UNDER THE IMPLICIT FINITE-DIFFERENCE SCHEME.
C ******************************************************************
C
      USE UZTVARS
      USE PKG2PKG
      USE MIN_SAT,       ONLY: QC7,DRYON
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,DELR,
     &                         DELC,DH,CNEW,A,RHS,NODES,UPDLHS,MIXELM,
     &                         RETA,COLD,IALTFM,
     &                         CINACT,DELT,DTRANS,iUnitTRNOP,iSSTrans
C
      IMPLICIT NONE
      INTEGER  I,J,K,II,N,ICOMP
      REAL     GWQOUT,CLOSEZERO,VOLAQU
      CLOSEZERO=1E-10
C
C--FORMULATE [A] AND [RHS] MATRICES FOR EULERIAN SCHEMES
      IF(MIXELM.GT.0) GOTO 1000
C
C--(INFILTRATED)                                                  
      DO I=1,NROW                                                 
        DO J=1,NCOL                                               
          K=ABS(IUZFBND(J,I))
          IF(K.EQ.0) CYCLE       !ROW/COL COMBO INACTIVE IN FLOW MODEL
          IF(FINFIL(J,I).LT.CLOSEZERO.AND.FINFIL(J,I).GT.CLOSEZERO)CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                       
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(FINFIL(J,I).LT.0) THEN
              IF(UPDLHS) A(N)=A(N)+FINFIL(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
            ELSE
              RHS(N)=RHS(N)-FINFIL(J,I)*CUZINF(J,I,ICOMP)*        
     &               DELR(J)*DELC(I)*DH(J,I,K)             
            ENDIF                                                 
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(FINFIL(J,I).LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-FINFIL(J,I)*ABS(VOLAQU)
              ELSE
                QC7(J,I,K,7)=QC7(J,I,K,7)-
     &                       FINFIL(J,I)*ABS(VOLAQU)*CUZINF(J,I,ICOMP)
                QC7(J,I,K,8)=QC7(J,I,K,8)-FINFIL(J,I)*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF                                                   
        ENDDO                                                     
      ENDDO                                                       
C                                                                 
C--(SURFACE LEAKANCE - HANDLE FOR SINK ON THE GW SYSTEM) 
      IF(NSNK2UZF+NLAK2UZF+NSFR2UZF.LT.1) GOTO 12
C-----SNK
      DO II=1,NSNK2UZF
        IF(IUZCODESK(II).EQ.1) THEN !GW DISCHARGE
          N=INOD2SKUZ(II)
          CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
          GWQOUT=QSNK2UZF(II)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWQOUT.LT.0) THEN     !GWQOUT recorded as neg. val in FTL
              IF(UPDLHS) A(N)=A(N)-ABS(GWQOUT) 
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(GWQOUT.LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-GWQOUT !*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C-----LAK
      DO II=1,NLAK2UZF
        IF(IUZCODELK(II).EQ.1) THEN !GW DISCHARGE
          N=INOD2LKUZ(II)
          CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
          GWQOUT=QLAK2UZF(II)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWQOUT.LT.0) THEN                           
              IF(UPDLHS) A(N)=A(N)-ABS(GWQOUT) 
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(GWQOUT.LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-GWQOUT !*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C-----SFR
      DO II=1,NSFR2UZF
        IF(IUZCODESF(II).EQ.1) THEN !GW DISCHARGE
          N=INOD2SFUZ(II)
          CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
          GWQOUT=QSFR2UZF(II)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWQOUT.LT.0) THEN                           
              IF(UPDLHS) A(N)=A(N)-ABS(GWQOUT) 
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(GWQOUT.LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-GWQOUT !*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C                                                                   
C--(UZET)                                                           
   12 IF(.NOT.IETFLG) GOTO 20                                       
      DO K=1,NLAY                                                   
        DO I=1,NROW                                                 
          DO J=1,NCOL                                               
            IF(UZET(J,I,K).EQ.0) CYCLE                              
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                       
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                        
              IF(UZET(J,I,K).LT.0.AND.(CUZET(J,I,K,ICOMP).LT.0 .OR. 
     &         CUZET(J,I,K,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN       
                IF(UPDLHS) A(N)=A(N)+UZET(J,I,K)*                   
     &                     DELR(J)*DELC(I)*DH(J,I,K)                
              ELSEIF(CUZET(J,I,K,ICOMP).GT.0) THEN                  
                RHS(N)=RHS(N)-UZET(J,I,K)*CUZET(J,I,K,ICOMP)*       
     &                 DELR(J)*DELC(I)*DH(J,I,K)                
              ENDIF                                                 
            ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(UZET(J,I,K).LT.0.AND.(CUZET(J,I,K,ICOMP).LT.0 .OR. 
     &                    CUZET(J,I,K,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-UZET(J,I,K)*ABS(VOLAQU)
                ELSEIF(CGWET(J,I,ICOMP).GT.0) THEN
                  QC7(J,I,K,7)=QC7(J,I,K,7)-UZET(J,I,K)*ABS(VOLAQU)
     &                                     *CUZET(J,I,K,ICOMP)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-UZET(J,I,K)*ABS(VOLAQU)
                ENDIF
              ENDIF
            ENDIF
          ENDDO                                                     
        ENDDO                                                       
      ENDDO                                                         
C                                                                   
C--(GWET)                                                                                                             
      DO I=1,NROW                                                 
        DO J=1,NCOL                                               
          K=IGWET(J,I)
          IF(K.EQ.0) CYCLE
          IF(GWET(J,I).EQ.0) CYCLE                              
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                       
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                        
            IF(GWET(J,I).LT.0.AND.(CGWET(J,I,ICOMP).LT.0 .OR. 
     &                 CGWET(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
              IF(UPDLHS) A(N)=A(N)+GWET(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
            ELSEIF(CGWET(J,I,ICOMP).GT.0) THEN                  
              RHS(N)=RHS(N)-GWET(J,I)*CGWET(J,I,ICOMP)*       
     &               DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF                                                 
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(GWET(J,I).LT.0.AND.(CGWET(J,I,ICOMP).LT.0 .OR. 
     &                     CGWET(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-GWET(J,I)*ABS(VOLAQU)
                ELSEIF(CGWET(J,I,ICOMP).GT.0) THEN
                  QC7(J,I,K,7)=QC7(J,I,K,7)-GWET(J,I)*ABS(VOLAQU)
     &            *CGWET(J,I,ICOMP)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-GWET(J,I)*ABS(VOLAQU)
                ENDIF
              ENDIF
          ENDIF                                                   
        ENDDO                                                     
      ENDDO                                                       
C
C--DONE WITH EULERIAN SCHEMES
   20 GOTO 2000
C
C--FORMULATE [A] AND [RHS] MATRICES FOR EULERIAN-LAGRANGIAN SCHEMES
 1000 CONTINUE
C     
C--(INFILTRATED)
      DO I=1,NROW
        DO J=1,NCOL
          K=ABS(IUZFBND(J,I))
          IF(K.EQ.0) CYCLE         !ROW/COL COMBO INACTIVE IN FLOW MODEL
          IF(FINFIL(J,I).LT.CLOSEZERO.AND.FINFIL(J,I).GT.CLOSEZERO)CYCLE
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
            IF(FINFIL(J,I).LT.0) THEN
              IF(UPDLHS) A(N)=A(N)+FINFIL(J,I)*DELR(J)*DELC(I)*DH(J,I,K)
            ELSE
              RHS(N)=RHS(N)-FINFIL(J,I)*CUZINF(J,I,ICOMP)*
     &               DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF                                                
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(FINFIL(J,I).LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-FINFIL(J,I)*ABS(VOLAQU)
              ELSE
                QC7(J,I,K,7)=QC7(J,I,K,7)-
     &                       FINFIL(J,I)*ABS(VOLAQU)*CUZINF(J,I,ICOMP)
                QC7(J,I,K,8)=QC7(J,I,K,8)-FINFIL(J,I)*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF                                                  
        ENDDO                                                      
      ENDDO                                                        
C                                                                  
C--(SURFACE LEAKANCE)                                              
      IF(NSNK2UZF+NLAK2UZF+NSFR2UZF.LT.1) GOTO 14
C-----SNK
      DO II=1,NSNK2UZF
        IF(IUZCODESK(II).EQ.1) THEN !GW DISCHARGE
          N=INOD2SKUZ(II)
          CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
          GWQOUT=QSNK2UZF(II)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWQOUT.LT.0) THEN                           
              IF(UPDLHS) A(N)=A(N)-ABS(GWQOUT) 
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(GWQOUT.LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-GWQOUT !*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C-----LAK
      DO II=1,NLAK2UZF
        IF(IUZCODELK(II).EQ.1) THEN !GW DISCHARGE
          N=INOD2LKUZ(II)
          CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
          GWQOUT=QLAK2UZF(II)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWQOUT.LT.0) THEN                           
              IF(UPDLHS) A(N)=A(N)-ABS(GWQOUT)
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(GWQOUT.LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-GWQOUT !*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C-----SFR
      DO II=1,NSFR2UZF
        IF(IUZCODESF(II).EQ.1) THEN !GW DISCHARGE
          N=INOD2SFUZ(II)
          CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
          GWQOUT=QSFR2UZF(II)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWQOUT.LT.0) THEN                           
              IF(UPDLHS) A(N)=A(N)-ABS(GWQOUT)
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(GWQOUT.LT.0) THEN
                QC7(J,I,K,9)=QC7(J,I,K,9)-GWQOUT !*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C--(UZET)                                                          
   14 IF(.NOT.IETFLG) GOTO 2000                      
      DO K=1,NLAY                                                  
        DO I=1,NROW                                                
          DO J=1,NCOL                                              
            IF(UZET(J,I,K).EQ.0) CYCLE                             
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                      
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                       
              IF(UZET(J,I,K).LT.0.AND.(CUZET(J,I,K,ICOMP).LT.0 .OR.
     &                 CUZET(J,I,K,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                CYCLE                                              
              ELSEIF(CUZET(J,I,K,ICOMP).GE.0) THEN                 
                IF(UPDLHS) A(N)=A(N)-UZET(J,I,K)
     &                          *DELR(J)*DELC(I)*DH(J,I,K)
                RHS(N)=RHS(N)-UZET(J,I,K)*CUZET(J,I,K,ICOMP)       
     &                 *DELR(J)*DELC(I)*DH(J,I,K)              
              ENDIF                                                
            ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(UZET(J,I,K).LT.0.AND.(CUZET(J,I,K,ICOMP).LT.0 .OR. 
     &                    CUZET(J,I,K,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-UZET(J,I,K)*ABS(VOLAQU)
                ELSEIF(CGWET(J,I,ICOMP).GT.0) THEN
                  QC7(J,I,K,7)=QC7(J,I,K,7)-UZET(J,I,K)*ABS(VOLAQU)
     &                                     *CUZET(J,I,K,ICOMP)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-UZET(J,I,K)*ABS(VOLAQU)
                ENDIF
              ENDIF
            ENDIF                                                  
          ENDDO                                                    
        ENDDO                                                      
      ENDDO                                                        
C--(GWET)                                                          
      DO I=1,NROW                                                
        DO J=1,NCOL                                              
          K=IGWET(J,I)
          IF(K.EQ.0) CYCLE
          IF(GWET(J,I).EQ.0) CYCLE                             
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN                      
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                       
            IF(GWET(J,I).LT.0.AND.(CGWET(J,I,ICOMP).LT.0 .OR.
     &                CGWET(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
              CYCLE                                              
            ELSEIF(CGWET(J,I,ICOMP).GE.0) THEN                 
              IF(UPDLHS) A(N)=A(N)-GWET(J,I)                   
     &                        *DELR(J)*DELC(I)*DH(J,I,K)
              RHS(N)=RHS(N)-GWET(J,I)*CGWET(J,I,ICOMP)       
     &               *DELR(J)*DELC(I)*DH(J,I,K)                
            ENDIF                                                
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(GWET(J,I).LT.0.AND.(CGWET(J,I,ICOMP).LT.0 .OR. 
     &                     CGWET(J,I,ICOMP).GE.CNEW(J,I,K,ICOMP))) THEN
                  QC7(J,I,K,9)=QC7(J,I,K,9)-GWET(J,I)*ABS(VOLAQU)
                ELSEIF(CGWET(J,I,ICOMP).GT.0) THEN
                  QC7(J,I,K,7)=QC7(J,I,K,7)-GWET(J,I)*ABS(VOLAQU)
     &            *CGWET(J,I,ICOMP)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-GWET(J,I)*ABS(VOLAQU)
                ENDIF
              ENDIF
          ENDIF                                                  
        ENDDO                                                    
      ENDDO                                                      
C--DONE WITH EULERIAN-LAGRANGIAN SCHEMES
 2000 CONTINUE
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE UZT1BD(ICOMP,DTRANS)
C ********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGETS ASSOCIATED WITH UZT SINK/
C SOURCE TERMS.
C ********************************************************************
      USE UZTVARS
      USE PKG2PKG
      USE MIN_SAT,       ONLY:QC7,DRYON
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,DELR,
     &                         DELC,DH,CNEW,A,RHS,NODES,UPDLHS,MIXELM,
     &                         RETA,COLD,IALTFM,RMASIO,IOUT,
     &                         CINACT,DELT,iUnitTRNOP
C
      IMPLICIT NONE
      INTEGER  I,J,K,II,N,ICOMP
      REAL     GWQOUT,CLOSEZERO,CTMP,DTRANS,VOLAQU
      CLOSEZERO=1E-10
C
C                                                                  
C--(INFILTRATED)                                                   
      DO I=1,NROW                                                  
        DO J=1,NCOL                                                
          K=ABS(IUZFBND(J,I))
          IF(K.EQ.0) CYCLE         !ROW/COL COMBO INACTIVE IN FLOW MODEL
          CTMP=CUZINF(J,I,ICOMP)
          IF(FINFIL(J,I).LT.0) CTMP=CNEW(J,I,K,ICOMP)              
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(FINFIL(J,I).GT.0) THEN                                
              RMASIO(53,1,ICOMP)=RMASIO(53,1,ICOMP)+FINFIL(J,I)*
     &                           CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
            ELSE                                                     
              RMASIO(53,2,ICOMP)=RMASIO(53,2,ICOMP)+FINFIL(J,I)*     
     &                           CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF                                                    
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(FINFIL(J,I).LT.0) THEN
                RMASIO(53,2,ICOMP)=RMASIO(53,2,ICOMP)+FINFIL(J,I)*     
     &                             CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
                QC7(J,I,K,9)=QC7(J,I,K,9)-FINFIL(J,I)*ABS(VOLAQU)
              ELSE
                RMASIO(53,1,ICOMP)=RMASIO(53,1,ICOMP)+FINFIL(J,I)*
     &                             CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
                QC7(J,I,K,7)=QC7(J,I,K,7)-
     &                       FINFIL(J,I)*ABS(VOLAQU)*CUZINF(J,I,ICOMP)
                QC7(J,I,K,8)=QC7(J,I,K,8)-FINFIL(J,I)*ABS(VOLAQU)
              ENDIF
            ENDIF
          ENDIF
        ENDDO                                                      
      ENDDO                                                        
C                                                                  
C--(SURFACE LEAKANCE)                                              
      IF(NSNK2UZF+NLAK2UZF+NSFR2UZF.LT.1) GOTO 110
C-----SNK
      DO II=1,NSNK2UZF
        IF(IUZCODESK(II).EQ.1) THEN !GW DISCHARGE
          N=INOD2SKUZ(II)
          CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
          IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
          GWQOUT=QSNK2UZF(II)
          CTMP=CNEW(J,I,K,ICOMP)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWQOUT.LT.0) THEN                           
                RMASIO(53,2,ICOMP)=RMASIO(53,2,ICOMP)-ABS(GWQOUT)*
     &                             CTMP*DTRANS
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(GWQOUT.LT.0) THEN
                RMASIO(53,2,ICOMP)=RMASIO(53,2,ICOMP)+
     &                             GWQOUT*CTMP*DTRANS
                QC7(J,I,K,9)=QC7(J,I,K,9)-GWQOUT
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C-----LAK
      DO II=1,NLAK2UZF
        IF(IUZCODELK(II).EQ.1) THEN !GW DISCHARGE
          N=INOD2LKUZ(II)
          CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
          GWQOUT=QLAK2UZF(II)
          CTMP=CNEW(J,I,K,ICOMP)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWQOUT.LT.0) THEN                           
                RMASIO(26,2,ICOMP)=RMASIO(26,2,ICOMP)-ABS(GWQOUT)*
     &                             CTMP*DTRANS 
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(GWQOUT.LT.0) THEN
                RMASIO(26,2,ICOMP)=RMASIO(26,2,ICOMP)+
     &                             GWQOUT*CTMP*DTRANS
                QC7(J,I,K,9)=QC7(J,I,K,9)-GWQOUT
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C-----SFR
      DO II=1,NSFR2UZF
        IF(IUZCODESF(II).EQ.1) THEN !GW DISCHARGE
          N=INOD2SFUZ(II)
          CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
          GWQOUT=QSFR2UZF(II)
          CTMP=CNEW(J,I,K,ICOMP)
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWQOUT.LT.0) THEN                           
                RMASIO(30,2,ICOMP)=RMASIO(30,2,ICOMP)-ABS(GWQOUT)*
     &                             CTMP*DTRANS
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
            IF(DRYON) THEN
              VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
              IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
              IF(GWQOUT.LT.0) THEN
                RMASIO(30,2,ICOMP)=RMASIO(30,2,ICOMP)+
     &                             GWQOUT*CTMP*DTRANS
                QC7(J,I,K,9)=QC7(J,I,K,9)-GWQOUT
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C                                                                   
C--(UZET)                                                           
  110 IF(.NOT.IETFLG) GOTO 200
      DO K=1,NLAY                                                   
        DO I=1,NROW                                                 
          DO J=1,NCOL                                               
            CTMP=CUZET(J,I,K,ICOMP)                                 
            IF(UZET(J,I,K).LT.0.AND.(CTMP.LT.0 .OR.                 
     &                             CTMP.GE.CNEW(J,I,K,ICOMP))) THEN 
              CTMP=CNEW(J,I,K,ICOMP)                                
            ELSEIF(CTMP.LT.0) THEN                                  
              CTMP=0.                                               
            ENDIF                                                   
            IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
              IF(UZET(J,I,K).GT.0) THEN                               
                RMASIO(54,1,ICOMP)=RMASIO(54,1,ICOMP)+UZET(J,I,K)*    
     &                           CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
              ELSE                                                    
                RMASIO(54,2,ICOMP)=RMASIO(54,2,ICOMP)+UZET(J,I,K)*    
     &                           CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
              ENDIF
            ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(UZET(J,I,K).GT.0) THEN                               
                  RMASIO(54,1,ICOMP)=RMASIO(54,1,ICOMP)+UZET(J,I,K)*    
     &            CTMP*DTRANS*ABS(VOLAQU)
                  QC7(J,I,K,7)=QC7(J,I,K,7)-UZET(J,I,K)*ABS(VOLAQU)
     &            *CTMP
                  QC7(J,I,K,8)=QC7(J,I,K,8)-UZET(J,I,K)*ABS(VOLAQU)
                ELSE                                                    
                  RMASIO(54,2,ICOMP)=RMASIO(54,2,ICOMP)+UZET(J,I,K)*    
     &            CTMP*DTRANS*ABS(VOLAQU)
                  QC7(J,I,K,9)=QC7(J,I,K,9)-UZET(J,I,K)*ABS(VOLAQU)
                ENDIF
              ENDIF
            ENDIF
          ENDDO                                                     
        ENDDO                                                       
      ENDDO                                                         
C                                                                   
C--(GWET)                                                                                                             
      DO I=1,NROW                                                 
        DO J=1,NCOL                                               
          K=IGWET(J,I)
          IF(K.EQ.0) CYCLE
          CTMP=CGWET(J,I,ICOMP)                                 
          IF(GWET(J,I).LT.0.AND.(CTMP.LT.0 .OR.                 
     &                           CTMP.GE.CNEW(J,I,K,ICOMP))) THEN 
            CTMP=CNEW(J,I,K,ICOMP)                                
          ELSEIF(CTMP.LT.0) THEN                                  
            CTMP=0.                                               
          ENDIF                                                   
          IF(ICBUND(J,I,K,ICOMP).GT.0) THEN
            IF(GWET(J,I).GT.0) THEN                               
              RMASIO(54,1,ICOMP)=RMASIO(54,1,ICOMP)+GWET(J,I)*    
     &          CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)               
            ELSE                                                    
              RMASIO(54,2,ICOMP)=RMASIO(54,2,ICOMP)+GWET(J,I)*    
     &          CTMP*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)               
            ENDIF
          ELSEIF(ICBUND(J,I,K,ICOMP).EQ.0) THEN
              IF(DRYON) THEN
                VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
                IF(ABS(VOLAQU).LE.1.E-8) VOLAQU=1.E-8
                IF(GWET(J,I).GT.0) THEN                               
                  RMASIO(54,1,ICOMP)=RMASIO(54,1,ICOMP)+GWET(J,I)*    
     &            CTMP*DTRANS*ABS(VOLAQU)
                  QC7(J,I,K,7)=QC7(J,I,K,7)-GWET(J,I)*ABS(VOLAQU)
     &            *CGWET(J,I,ICOMP)
                  QC7(J,I,K,8)=QC7(J,I,K,8)-GWET(J,I)*ABS(VOLAQU)
                ELSE                                                    
                  RMASIO(54,2,ICOMP)=RMASIO(54,2,ICOMP)+GWET(J,I)*    
     &            CTMP*DTRANS*ABS(VOLAQU)
                  QC7(J,I,K,9)=QC7(J,I,K,9)-GWET(J,I)*ABS(VOLAQU)
                ENDIF
              ENDIF
          ENDIF
        ENDDO                                                     
      ENDDO                                                       
C
C--RETURN
200   RETURN
      END
C
      SUBROUTINE UZT1AD(HT1,HT2,TIME1,TIME2)
C ********************************************************************
C THIS SUBROUTINE UPDATES WATER CONTENT AT EVERY TRANSPORT TIME-STEP
C ********************************************************************
C
      USE UZTVARS,       ONLY: IUZFBND,SATOLD,PRSITYSAV,SATNEW,THETAW
      USE MT3DMS_MODULE, ONLY: NLAY,NCOL,NROW,PRSITY
C
      INTEGER K,I,J
      REAL HT1,HT2,TIME1,TIME2
      REAL SAT
C
      DO I=1,NROW
        DO J=1,NCOL
          IF(IUZFBND(J,I).LE.0) THEN
            DO K=1,NLAY
              THETAW(J,I,K)=PRSITYSAV(J,I,K)
            ENDDO
          ELSE
            DO K=1,NLAY
              SAT=((SATNEW(J,I,K)-SATOLD(J,I,K))/(HT2-HT1))*(TIME2-HT1)
     1            +SATOLD(J,I,K)
              THETAW(J,I,K)=SAT*PRSITYSAV(J,I,K)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
C--RETURN
      RETURN
      END
C
      SUBROUTINE THETA2AD(HT,TIME)
C ********************************************************************
C THIS SUBROUTINE UPDATES WATER CONTENT AT EVERY TRANSPORT TIME-STEP
C ********************************************************************
C
      USE MT3DMS_MODULE, ONLY: NLAY,NCOL,NROW,PRSITY,THETAW2,ICBUND,
     1                         DELR,DELC,DH,QSTO,DZ
C
      INTEGER K,I,J
      REAL HT,TIME
      REAL SAT
C
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            N=(K-1)*NCOL*NROW + (I-1)*NCOL + J
            IF(ICBUND(J,I,K,1).LE.0) CYCLE
            VOL=DELR(J)*DELC(I)*DH(J,I,K)+DELR(J)*DELC(I)*DH(J,I,K)
     &          *QSTO(J,I,K)/PRSITY(J,I,K)*(HT-TIME)
            VCELL=DELR(J)*DELC(I)*DZ(J,I,K)
            VOL=MIN(VOL,VCELL)
            THETAW2(J,I,K)=VOL*PRSITY(J,I,K)/VCELL
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
      RETURN
      END





