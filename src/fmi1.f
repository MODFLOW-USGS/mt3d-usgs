C
      SUBROUTINE FMI1AR()
C **********************************************************************
C THIS SUBROUTINE CHECKS FLOW-TRANSPORT LINK FILE AND ALLOCATES SPACE
C FOR ARRAYS THAT MAY BE NEEDED BY FLOW MODEL-INTERFACE (FMI) PACKAGE.
C **********************************************************************
C last modified: 10-01-2014
C
      USE MT3DMS_MODULE, ONLY: INFTL,IOUT,MXTRNOP,iUnitTRNOP,NPERFL,ISS,
     &                         IVER,IFTLFMT,
     &         NPERFL,ISS,IVER,FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &         FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,
     &         FSWT,FSFR,FUZF,NPCKGTXT,FLAKFLOWS,FMNWFLOWS,FSFRFLOWS,
     &         FUZFFLOWS,FSWR,FSWRFLOWS,FSFRLAK,FSFRUZF,FLAKUZF,FSNKUZF
      USE SFRVARS, ONLY: ISFTTR
      IMPLICIT  NONE
      INTEGER   
     &          MTWEL,MTDRN,MTRCH,MTEVT,MTRIV,MTGHB,MTCHD,
     &          MTSTR,MTFHB,MTRES,MTTLK,MTIBS,MTLAK,
     &          MTDRT,MTETS,MTMNW,MTSWT,MTSFR,MTUZF,IERR,IPCKG
      CHARACTER VERSION*11
      CHARACTER*20 TEXT1
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INFTL
 1030 FORMAT(1X,'FMI1 -- FLOW MODEL INTERFACE PACKAGE,',
     & ' VERSION 1, MAY 2016, INPUT READ FROM UNIT',I3)
C
C--ALLOCATE
      ALLOCATE(NPERFL,ISS,IVER,FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &         FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,
     &         FSWT,FSFR,FUZF,NPCKGTXT,FLAKFLOWS,FMNWFLOWS,FSFRFLOWS,
     &         FUZFFLOWS,FSWR,FSWRFLOWS,FSFRLAK,FSFRUZF,FLAKUZF,FSNKUZF)
C
C--INITIALIZE
      ISS=1
      NPERFL=0
      IVER=2
      VERSION=' '
      FWEL=.FALSE.
      FDRN=.FALSE.
      FRCH=.FALSE.
      FEVT=.FALSE.
      FRIV=.FALSE.
      FGHB=.FALSE.
      FSTR=.FALSE.
      FRES=.FALSE.
      FFHB=.FALSE.
      FIBS=.FALSE.
      FTLK=.FALSE.
      FLAK=.FALSE.
      FMNW=.FALSE.
      FDRT=.FALSE.
      FETS=.FALSE.
      FSWT=.FALSE.
      FSFR=.FALSE.
      FUZF=.FALSE.
      FLAKFLOWS=.FALSE.
      FMNWFLOWS=.FALSE.
      FSFRFLOWS=.FALSE.
      FUZFFLOWS=.FALSE.
      FSWR=.FALSE.
      FSWRFLOWS=.FALSE.
      FSFRLAK=.FALSE.
      FSFRUZF=.FALSE.
      FLAKUZF=.FALSE.
      FSNKUZF=.FALSE.
      MTSTR=0
      MTRES=0
      MTFHB=0
      MTDRT=0
      MTETS=0
      MTTLK=0
      MTIBS=0
      MTLAK=0
      MTMNW=0
      MTSWT=0
      MTSFR=0
      MTUZF=0
C
C--READ HEADER OF FLOW-TRANSPORT LINK FILE
      IF(IFTLFMT.EQ.0) THEN
        READ(INFTL,ERR=100,IOSTAT=IERR) VERSION,MTWEL,MTDRN,MTRCH,
     &   MTEVT,MTRIV,MTGHB,MTCHD,ISS,NPERFL
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INFTL,*,ERR=100,IOSTAT=IERR) VERSION,MTWEL,MTDRN,MTRCH,
     &   MTEVT,MTRIV,MTGHB,MTCHD,ISS,NPERFL
      ENDIF
C
  100 IF((VERSION(1:4).NE.'MT3D'.AND.VERSION(1:4).NE.'MTGS').OR.
     1   IERR.NE.0) THEN
        GOTO 500
      ELSEIF(VERSION(1:11).EQ.'MT3D4.00.00') THEN
        REWIND(INFTL)
        IF(IFTLFMT.EQ.0) THEN
          READ(INFTL) VERSION,MTWEL,MTDRN,MTRCH,MTEVT,            !5
     &     MTRIV,MTGHB,MTCHD,ISS,NPERFL,                          !10
     &     MTSTR,MTRES,MTFHB,MTDRT,MTETS,MTTLK,MTIBS,MTLAK,MTMNW, !19
     &     MTSWT,MTSFR,MTUZF                                      !22
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INFTL,*) VERSION,MTWEL,MTDRN,MTRCH,MTEVT,
     &     MTRIV,MTGHB,MTCHD,ISS,NPERFL,
     &     MTSTR,MTRES,MTFHB,MTDRT,MTETS,MTTLK,MTIBS,MTLAK,MTMNW,
     &     MTSWT,MTSFR,MTUZF
        ENDIF
      ELSEIF(VERSION(1:11).EQ.'MTGS1.00.00') THEN
        IF(IFTLFMT.EQ.0) THEN
          READ(INFTL) NPCKGTXT
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INFTL,*) NPCKGTXT
        ENDIF
        DO IPCKG=1,NPCKGTXT
          IF(IFTLFMT.EQ.0) THEN
            READ(INFTL) TEXT1
          ELSEIF(IFTLFMT.EQ.1) THEN
            READ(INFTL,*) TEXT1
          ENDIF
C
          IF(TEXT1.EQ.'                 STR') FSTR=.TRUE.
          IF(TEXT1.EQ.'                 RES') FRES=.TRUE.
          IF(TEXT1.EQ.'                 FHB') FFHB=.TRUE.
          IF(TEXT1.EQ.'                 DRT') FDRT=.TRUE.
          IF(TEXT1.EQ.'                 ETS') FETS=.TRUE.
          IF(TEXT1.EQ.'                 IBS') FIBS=.TRUE.
          IF(TEXT1.EQ.'                 TLK') FTLK=.TRUE.
          IF(TEXT1.EQ.'                 LAK') FLAK=.TRUE. 
          IF(TEXT1.EQ.'           LAK FLOWS') FLAKFLOWS=.TRUE.
          IF(TEXT1.EQ.'                 MNW') FMNW=.TRUE.
          IF(TEXT1.EQ.'           MNW FLOWS') FMNWFLOWS=.TRUE. !NOT SUPPORTED YET
          IF(TEXT1.EQ.'                 SWT') FSWT=.TRUE.
          IF(TEXT1.EQ.'                 SFR') FSFR=.TRUE.
          IF(TEXT1.EQ.'        SFR FLOWS SS' .OR.
     1       TEXT1.EQ.'        SFR FLOWS TR') THEN
            FSFRFLOWS=.TRUE.
            IF(TEXT1.EQ.'        SFR FLOWS SS') ISFTTR=0
            IF(TEXT1.EQ.'        SFR FLOWS TR') ISFTTR=1
          ENDIF
          IF(TEXT1.EQ.'                 UZF') FUZF=.TRUE.
          IF(TEXT1.EQ.'           UZF FLOWS') FUZFFLOWS=.TRUE.
          IF(TEXT1.EQ.'                 SWR') FSWR=.TRUE.
          IF(TEXT1.EQ.'           SWR FLOWS') FSWRFLOWS=.TRUE. !NOT SUPPORTED YET
          IF(TEXT1.EQ.'     CONNECT SFR LAK') FSFRLAK=.TRUE.
          IF(TEXT1.EQ.'     CONNECT SFR UZF') FSFRUZF=.TRUE.
          IF(TEXT1.EQ.'     CONNECT LAK UZF') FLAKUZF=.TRUE.
          IF(TEXT1.EQ.'     CONNECT SNK UZF') FSNKUZF=.TRUE.
        ENDDO
      ENDIF
C
C--DETERMINE WHICH FLOW COMPONENTS USED IN FLOW MODEL
      IF(MTWEL.GT.0) FWEL=.TRUE.
      IF(MTDRN.GT.0) FDRN=.TRUE.
      IF(MTRCH.GT.0) FRCH=.TRUE.
      IF(MTEVT.GT.0) FEVT=.TRUE.
      IF(MTRIV.GT.0) FRIV=.TRUE.
      IF(MTGHB.GT.0) FGHB=.TRUE.
      IF(MTSTR.GT.0) FSTR=.TRUE.
      IF(MTRES.GT.0) FRES=.TRUE.
      IF(MTFHB.GT.0) FFHB=.TRUE.
      IF(MTIBS.GT.0) FIBS=.TRUE.
      IF(MTTLK.GT.0) FTLK=.TRUE.
      IF(MTLAK.GT.0) FLAK=.TRUE.
      IF(MTMNW.GT.0) FMNW=.TRUE.
      IF(MTDRT.GT.0) FDRT=.TRUE.
      IF(MTETS.GT.0) FETS=.TRUE.
      IF(MTSWT.GT.0) FSWT=.TRUE.
      IF(MTSFR.GT.0) FSFR=.TRUE.
      IF(MTUZF.GT.0) FUZF=.TRUE.
C
C--DETERMINE IF THE SSM PACKAGE IS REQUIRED
  200 IF(iUnitTRNOP(3).EQ.0) THEN
        IF(FWEL.OR.FDRN.OR.FRCH.OR.FEVT.OR.FRIV.OR.FGHB.OR.
     &   FSTR.OR.FRES.OR.FFHB.OR.FIBS.OR.FTLK.OR.FLAK.OR.FMNW.OR.
     &   FDRT.OR.FETS.OR.FSWT.OR.FSFR.OR.FUZF) THEN
          WRITE(*,300)
          CALL USTOP(' ')
        ELSEIF(MTCHD.GT.0) THEN
          WRITE(*,302)
          CALL USTOP(' ')
        ELSEIF(ISS.EQ.0) THEN
          WRITE(*,304)
          CALL USTOP(' ')
        ENDIF
      ENDIF
  300 FORMAT(/1X,'ERROR: THE SSM PACKAGE MUST BE USED',
     & ' IN THE CURRENT SIMULATION',
     & /1X,'BECAUSE THE FLOW MODEL INCLUDES A SINK/SOURCE PACKAGE.')
  302 FORMAT(/1X,'ERROR: THE SSM PACKAGE MUST BE USED',
     & ' IN THE CURRENT SIMULATION',
     & /1X,'BECAUSE THE FLOW MODEL CONTAINS CONSTANT-HEAD CELLS.')
  304 FORMAT(/1X,'ERROR: THE SSM PACKAGE MUST BE USED',
     & ' IN THE CURRENT SIMULATION',
     & /1X,'BECAUSE THE FLOW MODEL IS TRANSIENT.')
C
C--PRINT KEY INFORMATION OF THE FLOW MODEL
      IF(ISS.EQ.0) THEN
        WRITE(IOUT,310)
      ELSE
        WRITE(IOUT,320)
      ENDIF
      IF(MTCHD.GT.0) WRITE(IOUT,330)
      WRITE(IOUT,'(1X)')
  310 FORMAT(1X,'FLOW MODEL IS TRANSIENT')
  320 FORMAT(1X,'FLOW MODEL IS STEADY-STATE')
  330 FORMAT(1X,'FLOW MODEL CONTAINS CONSTANT-HEAD CELLS')
C
C--CHECK IF PACKAGES ARE ON
      IF(FUZFFLOWS) THEN
        IF(iUnitTRNOP(7).EQ.0) THEN
          WRITE(IOUT,*) ' UZT FILE IS REQUIRED'
          STOP
        ENDIF
      ELSE
        IF(iUnitTRNOP(7).GT.0) THEN
          WRITE(IOUT,*) ' UZT FILE WILL NOT BE USED'
          iUnitTRNOP(7)=0
        ENDIF
      ENDIF
      IF(FLAKFLOWS) THEN
        IF(iUnitTRNOP(18).EQ.0) THEN
          WRITE(IOUT,*) ' LKT FILE IS REQUIRED'
          STOP
        ENDIF
      ELSE
        IF(iUnitTRNOP(18).GT.0) THEN
          WRITE(IOUT,*) ' LKT FILE WILL NOT BE USED'
          iUnitTRNOP(18)=0
        ENDIF
      ENDIF
      IF(FSFRFLOWS) THEN
        IF(iUnitTRNOP(19).EQ.0) THEN
          WRITE(IOUT,*) ' SFT FILE IS REQUIRED'
          STOP
        ENDIF
      ELSE
        IF(iUnitTRNOP(19).GT.0) THEN
          WRITE(IOUT,*) ' SFT FILE WILL NOT BE USED'
          iUnitTRNOP(19)=0
        ENDIF
      ENDIF
C
C--DONE, RETURN
      GOTO 1000
C
C--ERROR READING THE FLOW-TRANSPORT LINK FILE
  500 WRITE(*,600)
      WRITE(IOUT,600)
      CALL USTOP(' ')
  600 FORMAT(/1X,'Error Reading Flow-Transport Link File',
     & ' Possibly Caused by:',
     & /1X,'1. Incompatible Styles of Unformatted Files',
     & ' Used by MODFLOW and MT3DMS;'
     & /1X,'2. Unformatted Flow-Transport Link File Saved by',
     & ' Verison 1 of LinkMT3D',
     & /1X,'   Package Which Is No Longer Supported by MT3DMS.')
C
 1000 RETURN
      END
C
C
      SUBROUTINE FMI1RP1(KPER,KSTP)
C **********************************************************************
C THIS SUBROUTINE READS SATURATED CELL THICKNESS, FLUXES ACROSS CELL
C INTERFACES, AND FLOW RATE TO OR FROM TRANSIENT STORAGE
C FROM AN UNFORMATTED FILE SAVED BY THE FLOW MODEL, AND PREPARES THEM
C IN THE FORMS NEEDED BY THE TRANSPORT MODEL.
C **********************************************************************
C last modified: 10-01-2014
C
      USE MIN_SAT  
      USE UZTVARS,       ONLY: WC,UZFLX,UZQSTO,SATOLD,SATNEW,
     &                         PRSITYSAV,IUZFOPT,IUZFOPTG,IUZFBND
      USE MT3DMS_MODULE, ONLY: INFTL,IOUT,NCOL,NROW,NLAY,NCOMP,
     &                         FPRT,LAYCON,ICBUND,HORIGN,DH,PRSITY,
     &                         DELR,DELC,DZ,XBC,YBC,ZBC,QSTO,COLD,CNEW,
     &                         RETA,QX,QY,QZ,DTRACK,DTRACK2,THKMIN,ISS,
     &                         IVER,iUnitTRNOP,PRSITY,FUZF,FUZFFLOWS,
     &                         NOCREWET  
C
      IMPLICIT  NONE
      INTEGER   INUF,J,I,K,KPER,KSTP,
     &          JTRACK,ITRACK,KTRACK,INDEX,
     &          IC,IR
      REAL      WW,WTBL,THKSAT,TK,CTMP,CREWET,THKMIN0
      CHARACTER TEXT*16
C
      JTRACK=0
      ITRACK=0
      KTRACK=0
C
      INUF=INFTL
CC
CC--READ UNSAT ZONE WATER CONTENT (UNITLESS)
C      IF(FUZF) THEN 
C        IF(IUZFOPTG.EQ.0) THEN
C          TEXT='WATER CONTENT   '
C          CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
C     &                WC,FPRT)                                
C        ENDIF
C                                                             
CC--READ UPPER-FACE FLUX TERMS                                 
C        TEXT='UZ FLUX         '                               
C        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,  
C     &              UZFLX,FPRT)                               
CC                                                             
CC--READ UNSATURATED ZONE STORAGE TERM (UNIT: L**3/T)          
C        TEXT='UZQSTO          '                               
C        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,  
C     &              UZQSTO,FPRT)                              
C!C                                                            
C!C--READ SURFACE LEAKANCE TERM (UNIT: L**3/T)                 
C!        TEXT='GWQOUT          '                              
C!        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT, 
C!     &              SURFLK,FPRT)                             
C      ENDIF                                                   
C
C--READ SATURATED THICKNESS (UNIT: L).
      IF(IVER.EQ.2) THEN
        TEXT='THKSAT'
      ELSEIF(IVER.EQ.1) THEN
        TEXT='HEAD'
      ENDIF
      CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,DH,FPRT)
      DO K=1,NLAY                                                 
        DO I=1,NROW                                               
          DO J=1,NCOL                                             
            IF(ICBUND(J,I,K,1).NE.0 .AND. DH(J,I,K).GT.1e25) THEN 
              DH(J,I,K)=0                                         
            ENDIF                                                 
          ENDDO                                                   
        ENDDO                                                     
      ENDDO                                                       
C
C--READ RIGHT-FACE FLOW TERMS IF MORE THAN ONE COLUMN (UNIT: L**3/T).
      IF(NCOL.LT.2) GOTO 100
      TEXT='QXX'
      CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,QX,FPRT)
C
C--READ FRONT-FACE FLOW TERMS IF MORE THAN ONE ROW (UNIT: L**3/T).
  100 IF(NROW.LT.2) GOTO 110
      TEXT='QYY'
      CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,QY,FPRT)
C
C--READ LOWER-FACE FLOW TERMS IF MORE THAN ONE LAYER (UNIT: L**3/T).
  110 IF(NLAY.LT.2) GOTO 120
      TEXT='QZZ'
      CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,QZ,FPRT)
C
C--READ STORAGE TERM (UNIT: L**3/T).
  120 TEXT='STO'
      IF(IVER.EQ.2.AND.ISS.EQ.0) THEN
        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,QSTO,FPRT)
      ENDIF
CC
CC--ONLY PERFORM THE NEXT BIT OF CODE IF UZF IS ACTIVE IN THE
CC--CURRENT CELL                                      
C      IF(FUZF) THEN
C      IF(IUZFOPTG.EQ.0) THEN                    
CC--IF NOT THE FIRST TIME STEP, COPY SATNEW TO SATOLD 
C        IF(KPER.NE.1 .OR. KSTP.NE.1) THEN            
C          DO K=1,NLAY                                
C            DO I=1,NROW                              
C              DO J=1,NCOL                            
C                IF(IUZFBND(J,I).GT.0) THEN           
C                  SATOLD(J,I,K)=SATNEW(J,I,K)        
C                ENDIF                                
C              ENDDO                                  
C            ENDDO                                    
C          ENDDO                                      
C        ENDIF                                        
CC                                                    
CC--COMPUTE SATURATION                                
C        PRSITY=>PRSITYSAV                            
C        DO K=1,NLAY                                  
C          DO I=1,NROW                                
C            DO J=1,NCOL                              
C              IF(DH(J,I,K).GE.DZ(J,I,K)) THEN        
C                SATNEW(J,I,K)=1                      
C              ELSEIF(DH(J,I,K).LT.DZ(J,I,K).AND.     
C     &        .NOT.ICBUND(J,I,K,1).EQ.0) THEN        
C                IF(INT(DH(J,I,K)).EQ.-111) THEN      
C                  SATNEW(J,I,K)=1                    
C                ELSE                                 
C                  SATNEW(J,I,K)=((DZ(J,I,K)-DH(J,I,K))/DZ(J,I,K))*
C     &                          WC(J,I,K)/PRSITY(J,I,K)+          
C     &                          DH(J,I,K)/DZ(J,I,K)*1             
C                ENDIF                                             
C              ENDIF                                               
C            ENDDO                                                 
C          ENDDO                                                   
C        ENDDO                                                     
CC                                                                 
CC--MELD UZFLX AND QZZ ARRAY SO THAT UZFLX DOESN'T NEED TO BE DEALT
CC--WITH LATER IN THE CODE, IT'LL INSTEAD BE IMPLICIT IN QZ        
C        DO K=1,(NLAY-1)                                           
C          DO I=1,NROW                                             
C            DO J=1,NCOL                                           
C              IF(ICBUND(J,I,K,1).NE.0) THEN                       
CC--THE NEXT LINE THAT CHECKS FOR -111 IS DUE TO CONFINED LAYERS,  
CC  ENSURES QZ ISN'T SET TO UZFLX IN THE EVENT DH = -111           
C                IF(INT(DH(J,I,K)).EQ.-111) DH(J,I,K)=DZ(J,I,K)    
C                IF(DH(J,I,K).LT.1E-5 .AND. .NOT. 
C     &              UZFLX(J,I,K+1).LE.0.0) THEN                        
C                  QZ(J,I,K)=UZFLX(J,I,K+1)                        
C                ENDIF                                             
C              ENDIF                                               
C            ENDDO                                                 
C          ENDDO                                                   
C        ENDDO                                                     
CC                                                                 
CC--PRSITY IS USED BELOW AND THEREFORE NEEDS TO BE UPDATED HERE    
CC--FOR BOTH THE SATURATED AND UNSATURATED CASE                    
C        DO K=1,NLAY                                               
C          DO I=1,NROW                                             
C            DO J=1,NCOL
C              IF(IUZFBND(J,I).EQ.0)THEN
C                WC(J,I,K)=PRSITY(J,I,K)
C              ELSE
C                WC(J,I,K)=SATNEW(J,I,K)*PRSITY(J,I,K)               
C              ENDIF
C            ENDDO                                                 
C          ENDDO                                                   
C        ENDDO                                                     
C        PRSITYSAV=>PRSITY                                         
C        PRSITY=>WC                                                
C      ENDIF                                                       
C      ENDIF                                                       
C
C--SET ICBUND=0 IF CELL IS DRY OR INACTIVE (INDICATED BY FLAG 1.E30)
C--AND REACTIVATE DRY CELL IF REWET AND ASSIGN CONC AT REWET CELL
C--WITH UZF TURNED ON THE GRID BECOMES FIXED.  THE USER PROVIDEDED
C--ICBUND ARRAY SHOULD REMAIN UNTOUCHED                           
      IF(.NOT.FUZFFLOWS.OR.IUZFOPTG.EQ.0) THEN                         
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(FUZFFLOWS) THEN
                IF(IUZFBND(J,I).GT.0) CYCLE
              ENDIF
              IF(ABS(DH(J,I,K)-1.E30).LT.1.E-5) THEN
                ICBUND(J,I,K,1)=0
              ELSEIF(ICBUND(J,I,K,1).EQ.0.AND.PRSITY(J,I,K).GT.0) THEN
                ICBUND(J,I,K,1)=30000
                DO INDEX=1,NCOMP
                  IF(NOCREWET.EQ.1) THEN
                  ELSE
                  CTMP=CREWET(NCOL,NROW,NLAY,CNEW(:,:,:,INDEX),
     &                        ICBUND,XBC,YBC,ZBC,J,I,K)
                  CTMP=(COLD(J,I,K,INDEX)*(RETA(J,I,K,INDEX)-1.0)+CTMP)
     &                /RETA(J,I,K,INDEX)
                  ENDIF
                  IF(NOCREWET.EQ.1) CTMP=0.
                  CNEW(J,I,K,INDEX)=CTMP
                  IF(MUTDRY.EQ.0) THEN
                    WRITE(IOUT,122) K,I,J,INDEX,CNEW(J,I,K,INDEX)
                  ENDIF 
                ENDDO
              ENDIF
            ENDDO
          ENDDO
        ENDDO
  122   FORMAT(/1X,'DRY CELL REACTIVATED AT K =',I4,',   I=',I4,
     &   ',   J=',I4/1X,'FOR SPECIES ',I3.3,
     &   ' WITH STARTING CONCENTRATION =',G13.5)
      ENDIF 
C
C--SET SATURATED THICKNESS [DH] TO LAYER THICKNESS [DZ]
C--FOR CONFINED LAYERS IF THE FLOW-TRANSPORT LINK FILE
C--IS SAVED BY LKMT PACKAGE VERSION 2 OR LATER
      IF(IVER.EQ.2) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,1).EQ.0) CYCLE
              IF(LAYCON(K).EQ.0.OR.INT(DH(J,I,K)).EQ.-111) THEN
                DH(J,I,K)=DZ(J,I,K)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
C--CALCULATE SATURATED THICKNESS FROM INPUT ARRAYS [HTOP] AND [DZ]
C--IF THE FLOW-TRANSPORT LINK FILE IS SAVED BY LKMT PACKAGE VER 1
      ELSEIF(IVER.EQ.1) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,1).EQ.0) CYCLE
              IF(LAYCON(K).EQ.0) THEN
                THKSAT=DZ(J,I,K)
              ELSE
                WTBL=HORIGN-DH(J,I,K)
                THKSAT=ZBC(J,I,K)+0.5*DZ(J,I,K)-WTBL
                THKSAT=MIN(THKSAT,DZ(J,I,K))
              ENDIF
              DH(J,I,K)=THKSAT
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--SET CELLS TO INACTIVE IF SATURATED THICKNESS < OR = 0, OR
C--IF SATURATED THICKNESS IS BELOW USER-SPECIFIED MINIMUM [THKMIN].
C--WITH UZF TURNED ON THE GRID BECOMES FIXED.  THE USER PROVIDEDED
C--ICBUND ARRAY SHOULD REMAIN UNTOUCHED                           
      IF(DRYON) THEN                                    
        NICBND2=0                                                 
        ICBND2=0                                                  
      ENDIF                                                       
      IF(.NOT.FUZFFLOWS.OR.IUZFOPTG.EQ.0) THEN                          
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(FUZFFLOWS) THEN
                IF(IUZFBND(J,I).GT.0) CYCLE
              ENDIF
              IF(ICBUND(J,I,K,1).EQ.0) CYCLE
              IF(DH(J,I,K).LE.0) THEN
                IF(DRYON) THEN          
                  ICBND2(J,I,K)=1                 
                  NICBND2=NICBND2+1               
                ENDIF                             
                IF(MUTDRY.EQ.0) THEN              
                  WRITE(IOUT,355) DH(J,I,K),K,I,J
                ENDIF
                ICBUND(J,I,K,1)=0
              ELSEIF(THKMIN.GT.0) THEN
                THKMIN0=THKMIN*DZ(J,I,K)
                IF(IABSMIN.EQ.1) THKMIN0=THKMIN
                IF(DH(J,I,K).LT.THKMIN0) THEN
                  IF(DRYON) THEN     
                    ICBND2(J,I,K)=1            
                    NICBND2=NICBND2+1          
                  ENDIF                        
                  IF(MUTDRY.EQ.0) THEN         
                    WRITE(IOUT,365) DH(J,I,K),THKMIN0,K,I,J
                  ENDIF   
                  ICBUND(J,I,K,1)=0
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
  355   FORMAT(/1X,'WARNING: SATURATED THICKNESS =',G13.5,
     &   ' NOT ALLOWED IN TRANSPORT MODEL'
     &   /10X,'AT ACTIVE CELL K =',I4,',   I=',I4,',   J=',I4,
     &   ';  RESET AS INACTIVE')
  365   FORMAT(/1X,'WARNING: SATURATED THICKNESS =',G13.5,
     &   ' BELOW SPECIFIED MINIMUM =',G13.5,
     &   /10X,'AT ACTIVE CELL K =',I4,',   I=',I4,',   J=',I4,
     &   ';  RESET AS INACTIVE')
      ENDIF
C
C--DETERMINE MAXIMUM TIME INCREMENT DURING WHICH ANY PARTICLE
C--CANNOT MOVE MORE THAN ONE CELL IN ANY DIRECTION.
      DTRACK=1.E30
C
      IF(NCOL.LT.2) GOTO 410
      DO K=1,NLAY
        DO I=1,NROW
          DO J=2,NCOL
C
            IF(ICBUND(J,I,K,1).NE.0) THEN
              TK=0.5*(QX(J-1,I,K)+QX(J,I,K))
              IF(TK.EQ.0) CYCLE
              TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
              IF(ABS(TK).LT.DTRACK) THEN
                DTRACK=ABS(TK)
                JTRACK=J
                ITRACK=I
                KTRACK=K
              ENDIF
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
  410 IF(NROW.LT.2) GOTO 420
      DO K=1,NLAY
        DO J=1,NCOL
          DO I=2,NROW
C
            IF(ICBUND(J,I,K,1).NE.0) THEN
              TK=0.5*(QY(J,I-1,K)+QY(J,I,K))
              IF(TK.EQ.0) CYCLE
              TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
              IF(ABS(TK).LT.DTRACK) THEN
                DTRACK=ABS(TK)
                JTRACK=J
                ITRACK=I
                KTRACK=K
              ENDIF
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
  420 IF(NLAY.LT.2) GOTO 430
      IF(FUZFFLOWS) GOTO 430  ! THIS CALCULATION IS REPEATED IN RP2 WHEN FUZFFLOWS>0
      DO J=1,NCOL
        DO I=1,NROW
          DO K=2,NLAY
C
            IF(ICBUND(J,I,K,1).NE.0) THEN
              TK=0.5*(QZ(J,I,K-1)+QZ(J,I,K))
              IF(TK.EQ.0) CYCLE
              TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
              IF(ABS(TK).LT.DTRACK) THEN
                DTRACK=ABS(TK)
                JTRACK=J
                ITRACK=I
                KTRACK=K
              ENDIF
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
C--PRINT INFORMATION ON DTRACK
  430 WRITE(IOUT,500) DTRACK,KTRACK,ITRACK,JTRACK
  500 FORMAT(/1X,'MAXIMUM STEPSIZE DURING WHICH ANY PARTICLE CANNOT',
     & ' MOVE MORE THAN ONE CELL'/1X,'=',G11.4,
     & '(WHEN MIN. R.F.=1)  AT K=',I4,', I=',I4,
     & ', J=',I4)
C
C--DETERMINE STABILITY CRITERION ASSOCIATED WITH EXPLICIT FINITE
C--DIFFERENCE SOLUTION OF THE ADVECTION TERM
      DTRACK2=1.E30
C
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,1).EQ.0) CYCLE
            TK=0.
            IF(J.GT.1) TK=TK+MAX(ABS(QX(J-1,I,K)),ABS(QX(J,I,K)))
            IF(I.GT.1) TK=TK+MAX(ABS(QY(J,I-1,K)),ABS(QY(J,I,K)))
            IF(.NOT.FUZFFLOWS) THEN
              IF(K.GT.1) TK=TK+MAX(ABS(QZ(J,I,K-1)),ABS(QZ(J,I,K)))
            ENDIF
            IF(TK.EQ.0) CYCLE
            TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
            IF(TK.LT.DTRACK2) THEN
              DTRACK2=TK
              JTRACK=J
              ITRACK=I
              KTRACK=K
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--PRINT INFORMATION ON DTRACK2
      WRITE(IOUT,550) DTRACK2,KTRACK,ITRACK,JTRACK
  550 FORMAT(/1X,'MAXIMUM STEPSIZE WHICH MEETS STABILITY CRITERION',
     & ' OF THE ADVECTION TERM'/1X,
     & '(FOR PURE FINITE-DIFFERENCE OPTION, MIXELM=0) '/1X,'=',G11.4,
     & '(WHEN MIN. R.F.=1)  AT K=',I4,', I=',I4,', J=',I4)
C
C--DIVIDE VOLUMETRIC QX, QY AND QZ BY AREAS
C--TO GET SPECIFIC DISCHAGES ACROSS EACH CELL INTERFACE
      IF(NCOL.LT.2) GOTO 910
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL-1
            WW=DELR(J+1)/(DELR(J+1)+DELR(J))
            THKSAT=DH(J,I,K)*WW+DH(J+1,I,K)*(1.-WW)
            IF(DOMINSAT) THEN                       
              IF(DH(J,I,K).LE.0. .OR. DH(J+1,I,K).LE.0.) THEN 
                IF(DH(J,I,K).GT.0.) THEN                      
                  THKSAT=DH(J,I,K)*WW                         
                ELSEIF(DH(J+1,I,K).GT.0.) THEN                
                  THKSAT=DH(J+1,I,K)*WW                       
                ELSE                                          
                  THKSAT=1.0E30                               
                ENDIF                                         
              ENDIF                                           
              THKSAT=ABS(DH(J,I,K))*WW+ABS(DH(J+1,I,K))*(1.-WW)
            ENDIF                                              
            IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
              IF(DOMINSAT) THEN           
                QX(J,I,K)=QX(J,I,K)/(DELC(I)*THKSAT)
                IF(THKSAT.EQ.0.0) QX(J,I,K)=0.
              ELSE                                  
                QX(J,I,K)=0
                IF(J.GT.1) QX(J-1,I,K)=0.
              ENDIF
            ELSE
              QX(J,I,K)=QX(J,I,K)/(DELC(I)*THKSAT)
            ENDIF
          ENDDO
          IF(.NOT.DOMINSAT) THEN 
            IF(ICBUND(NCOL,I,K,1).EQ.0) QX(NCOL-1,I,K)=0.
          ENDIF
        ENDDO
      ENDDO
C
  910 IF(NROW.LT.2) GOTO 920
      DO K=1,NLAY
        DO J=1,NCOL
          DO I=1,NROW-1
            WW=DELC(I+1)/(DELC(I+1)+DELC(I))
            THKSAT=DH(J,I,K)*WW+DH(J,I+1,K)*(1.-WW)
            IF(DOMINSAT) THEN 
              IF(DH(J,I,K).LE.0. .OR. DH(J,I+1,K).LE.0.) THEN
                IF(DH(J,I,K).GT.0.) THEN      
                  THKSAT=DH(J,I,K)*WW         
                ELSEIF(DH(J,I+1,K).GT.0.) THEN
                  THKSAT=DH(J,I+1,K)*WW       
                ELSE                          
                  THKSAT=1.0E30               
                ENDIF                         
              ENDIF                           
              THKSAT=ABS(DH(J,I,K))*WW+ABS(DH(J,I+1,K))*(1.-WW)
            ENDIF                                              
            IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
              IF(DOMINSAT) THEN           
                QY(J,I,K)=QY(J,I,K)/(DELR(J)*THKSAT)
                IF(THKSAT.EQ.0.0) QY(J,I,K)=0.
              ELSE                                  
                QY(J,I,K)=0
                IF(I.GT.1) QY(J,I-1,K)=0.
              ENDIF
            ELSE
              QY(J,I,K)=QY(J,I,K)/(DELR(J)*THKSAT)
            ENDIF
          ENDDO
          IF(.NOT.DOMINSAT) THEN
            IF(ICBUND(J,NROW,K,1).EQ.0) QY(J,NROW-1,K)=0.
          ENDIF
        ENDDO
      ENDDO
C
  920 IF(NLAY.LT.2) GOTO 990
      IF(FUZFFLOWS) GOTO 990
      DO J=1,NCOL
        DO I=1,NROW
          DO K=1,NLAY
            THKSAT=DH(J,I,K)
            IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
              IF(DOMINSAT) THEN             
                QZ(J,I,K)=QZ(J,I,K)/(DELR(J)*DELC(I)) 
              ELSE                                    
                QZ(J,I,K)=0
                IF(K.GT.1) QZ(J,I,K-1)=0.
              ENDIF
            ELSE
              QZ(J,I,K)=QZ(J,I,K)/(DELR(J)*DELC(I))
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--DIVIDE STORAGE BY CELL VOLUME TO GET DIMENSION (1/TIME)
  990 CONTINUE
      IF(FUZFFLOWS) GOTO 991
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            THKSAT=DH(J,I,K)  !WHEN UZT ACTIVE, DH IS DZ
            IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
              IF(DOMINSAT) THEN  
                QSTO(J,I,K)=QSTO(J,I,K)/(THKSAT*DELR(J)*DELC(I))
                IF(THKSAT.EQ.0.0) QSTO(J,I,K)=0.
              ELSE                                              
                QSTO(J,I,K)=0
              ENDIF 
            ELSE
              IF(iUnitTRNOP(7).GT.0) THEN
                QSTO(J,I,K)=(QSTO(J,I,K)+UZQSTO(J,I,K))/ 
     &                       (THKSAT*DELR(J)*DELC(I))    
                IF(THKSAT.EQ.0.0) QSTO(J,I,K)=0.
              ELSE                                       
                QSTO(J,I,K)=QSTO(J,I,K)/(THKSAT*DELR(J)*DELC(I))
              ENDIF                                             
            ENDIF
            IF(ABS(THKSAT-0.).LT.1.0E-5) QSTO(J,I,K)=0.
          ENDDO
        ENDDO
      ENDDO
C
C--SYNCHRONIZE ICBUND CONDITIONS OF ALL SPECIES
 991  IF(NCOMP.EQ.1) GOTO 999
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            DO INDEX=2,NCOMP
              IF(ICBUND(J,I,K,INDEX).GE.0) THEN
                ICBUND(J,I,K,INDEX)=IABS(ICBUND(J,I,K,1))
              ELSEIF(ICBUND(J,I,K,1).EQ.0) THEN
                ICBUND(J,I,K,INDEX)=0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
  999 RETURN
      END
C
C
      SUBROUTINE FMI1RP2(KPER,KSTP)
C **********************************************************************
C THIS SUBROUTINE READS THE LOCATIONS AND FLOW RATES OF SINKS & SOURCES
C FROM AN UNFORMATTED FILE SAVED BY THE FLOW MODEL, AND PREPARES THEM
C IN THE FORMS NEEDED BY THE TRANSPORT MODEL.
C **********************************************************************
C last modified: 02-20-2010
C
      USE MIN_SAT, ONLY: DRYON,DOMINSAT
      USE UZTVARS, ONLY: UZET,GWET,IETFLG,FINFIL,UZFLX,SATNEW,
     &                   IUZFBND,IUZRCH,UZRECH,IGWET,IUZFOPTG,WC,UZQSTO,
     &                   SATOLD,PRSITYSAV
      USE SFRVARS, ONLY: NSFINIT,NSTRM,ISFL,ISFR,ISFC,QSFGW,SFNAREA,
     &                   QPRECSF,QRUNOFSF,QETSF,IBNDSF,NSSSF,ISFNBC,
     &                   ISFBCTYP,IEXIT,QPRECSFO,QRUNOFSFO,QOUTSF,
     &                   QOUTSFO,SFOAREA,NSF2SF,INOD1SF,INOD2SF,IDSPFLG,
     &                   QN2NSF,VOLSFO,VOLSFN,SFLEN,ISFTTR
      USE LAKVARS, ONLY: QPRECLAK,QRUNOFLAK,QWDRLLAK,
     &                   QETLAK,VOLOLAK,VOLNLAK,DELVOLLAK,
     &                   LAKNUMGW,NLAKES,LKNODE,LAKL,LAKR,LAKC,
     &                   QLAKGW,NLKINIT
      USE PKG2PKG
      USE MT3DMS_MODULE, ONLY: INFTL,IOUT,NCOL,NROW,NLAY,NCOMP,FPRT,
     &                         LAYCON,ICBUND,DH,PRSITY,DELR,DELC,IRCH,
     &                         RECH,IEVT,EVTR,MXSS,NSS,NTSS,SS,BUFF,
     &                         DTSSM,IVER,DTRACK,DTRACK2,
     &                         FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &                         FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,
     &                         FETS,FSWT,FSFR,FUZF,ISS,NPERFL,
     &                         CNEW,SSMC,KSSZERO,
     &                         iUnitTRNOP,NPCKGTXT,FLAKFLOWS,FMNWFLOWS,
     &                         FSFRFLOWS,FUZFFLOWS,FSWR,FSWRFLOWS,
     &                         FSFRLAK,FSFRUZF,FLAKUZF,FSNKUZF,
     &                         DZ,QZ,PRSITY,QSTO
      USE INTERFACE1
C
      IMPLICIT  NONE
C
      INTEGER   INUF,J,I,K,JTRACK,ITRACK,KTRACK,
     &          NUM,KPER,KSTP,IQ,KSSM,ISSM,JSSM,
     &          JJ,II,KK,JM1,JP1,IM1,IP1,KM1,KP1,INDEX,IFL,N,IFROM,ITO
      INTEGER   INCTS,KOLD  
      REAL      VOLAQU,TM,THKSAT,TK
      CHARACTER TEXT*16
      INTEGER   NFLOWTYPE,NRCHCON,NNGW
      REAL,         ALLOCATABLE      :: PKGFLOWS(:,:),QAREA(:),
     1                                  QN2N(:)
      CHARACTER*20, ALLOCATABLE      :: CFLOWTYPE(:)
      INTEGER,      ALLOCATABLE      :: INOD1(:),INOD2(:),IDISP(:)
      INTEGER,      ALLOCATABLE      :: ICID(:)
      REAL,         ALLOCATABLE      :: QAUX(:)
      INTEGER,      ALLOCATABLE      :: IP2PFLG(:)
      CHARACTER*20  FLWTYP
C
      INUF=INFTL
C
C--RESET TOTAL NUMBER OF POINT SINKS/SOURCES AND CLEAR FLOW RATES
      NTSS=NSS
      SS(8,:)=0.
      DO NUM=1,NTSS
        SS(5,NUM)=0.
      ENDDO
C
C--RESET [ICBUND] VALUE AT ACTIVE CELLS TO UNITY
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,1).GT.0) ICBUND(J,I,K,1)=1
          ENDDO
        ENDDO
      ENDDO
C
C--READ CONSTANT-HEAD FLOW TERM (UNIT: L**3/T).
      TEXT='CNH'
      IQ=1
      CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
C
C--READ WELL FLOW TERM (L**3/T) IF WELL OPTION USED IN FLOW MODEL.
      IF(FWEL) THEN
        TEXT='WEL'
        IQ=2
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ DRAIN FLOW TERM (L**3/T) IF DRAIN OPTION USED IN FLOW MODEL.
      IF(FDRN) THEN
        TEXT='DRN'
        IQ=3
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ RECHARGE FLOW TERM (L**3/T)
C--IF RECHARGE OPTION USED IN FLOW MODEL
      IF(FRCH) THEN
        TEXT='RCH'
        CALL READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   RECH,IRCH,FPRT)
      ENDIF
C
C--READ ET FLOW TERM (L**3/T) IF EVT OPTION USED IN FLOW MODEL
      IF(FEVT) THEN
        TEXT='EVT'
        CALL READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   EVTR,IEVT,FPRT)
      ENDIF
C
C--READ RIVER FLOW TERM (L**3/T) IF RIVER OPTION USED IN FLOW MODEL.
      IF(FRIV) THEN
        TEXT='RIV'
        IQ=4
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ GERENAL HEAD DEPENDENT BOUNDARY FLOW TERM (L**3/T)
C--IF GHB OPTION IS USED IN FLOW MODEL.
      IF(FGHB) THEN
        TEXT='GHB'
        IQ=5
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ STREAMFLOW-ROUTING FLOW TERM (L**3/T)
C--IF STR OPTION IS USED IN FLOW MODEL.
      IF(FSTR) THEN
        TEXT='STR'
        IQ=21
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ RESERVOIR FLOW TERM (L**3/T)
C--IF RES OPTION IS USED IN FLOW MODEL.
      IF(FRES) THEN
        TEXT='RES'
        IQ=22
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ SPECIFIED FLOW AND HEAD BOUNDARY FLOW TERM (L**3/T)
C--IF FHB OPTION IS USED IN FLOW MODEL.
      IF(FFHB) THEN
        TEXT='FHB'
        IQ=23
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ DRAIN-RETURN FLOW TERM (L**3/T)
C--IF DRT OPTION IS USED IN FLOW MODEL.
      IF(FDRT) THEN
        TEXT='DRT'
        IQ=28
        CALL READGS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ ET FLOW TERM (L**3/T) IF SEGMENTED ET USED IN FLOW MODEL
      IF(FETS) THEN
        TEXT='ETS'
        IQ=29
        CALL READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   EVTR,IEVT,FPRT)
      ENDIF
C
C--READ MULTI-NODE WELL FLOW TERM (L**3/T)
C--IF MNW OPTION IS USED IN FLOW MODEL.
      IF(FMNW) THEN
        TEXT='MNW'
        IQ=27
        CALL READGS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ UZF FLOW TERMS
      IF(FUZF) THEN                                          
        IQ=31
        TEXT='UZF RECHARGE'
C        ALLOCATE(UZRECH(NCOL,NROW),IUZRCH(NCOL,NROW))
        CALL READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &              UZRECH,IUZRCH,FPRT)
C
        TEXT='GW-ET'
C        ALLOCATE(GWET(NCOL,NROW),IGWET(NCOL,NROW))
        CALL READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &              GWET,IGWET,FPRT)
      ELSEIF(FUZFFLOWS) THEN
        IQ=31
        IF(IUZFOPTG.EQ.0) THEN
C
C-------READ UNSAT ZONE WATER CONTENT (UNITLESS)
          TEXT='WATER CONTENT   '
          CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &                WC,FPRT)                                
        ENDIF
C                                                             
C-------READ UPPER-FACE FLUX TERMS                                 
        TEXT='UZ FLUX         '
        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,  
     &              UZFLX,FPRT)                               
C                                                             
C-------READ UNSATURATED ZONE STORAGE TERM (UNIT: L**3/T)          
        TEXT='UZQSTO          '                               
        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,  
     &              UZQSTO,FPRT)                              
!C                                                            
!C-------READ SURFACE LEAKANCE TERM (UNIT: L**3/T)                 
!        TEXT='GWQOUT          '                              
!        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT, 
!     &              SURFLK,FPRT)                             
C
C-------READ UZ-ET FLOW TERM (L**3/T) IF IETFLG>0 IN UZF PACKAGE   
C-------NOTE THAT EITHER THE ET PACKAGE OR THE UZF PACKAGE, BUT NOT
C-------BOTH WILL BE IN USE                                        
C                                                             
        IF(IETFLG) THEN
          TEXT='UZ-ET'
          CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &                UZET,FPRT)
        ENDIF                   
C                             
C-------Read 'GW-ET' flow term (L**3/T) if IETFLG>0 in UZF packge 
        IF(IETFLG) THEN
          TEXT='GW-ET'
C          ALLOCATE(GWET(NCOL,NROW),IGWET(NCOL,NROW))
          CALL READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &      GWET,IGWET,FPRT)
C          TEXT='GW-ET' 
C          CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
C     &                GWET,FPRT)
        ENDIF                   
C
C-------MODIFY ARRAYS
C
        IF(IUZFOPTG.EQ.0) THEN                    
C-------IF NOT THE FIRST TIME STEP, COPY SATNEW TO SATOLD 
          IF(KPER.NE.1 .OR. KSTP.NE.1) THEN            
            DO K=1,NLAY                                
              DO I=1,NROW                              
                DO J=1,NCOL                            
                  IF(IUZFBND(J,I).GT.0) THEN           
                    SATOLD(J,I,K)=SATNEW(J,I,K)        
                  ENDIF                                
                ENDDO                                  
              ENDDO                                    
            ENDDO                                      
          ENDIF                                        
C                                                    
C-------COMPUTE SATURATION                                
          PRSITY=>PRSITYSAV                            
          DO K=1,NLAY                                  
            DO I=1,NROW                                
              DO J=1,NCOL                              
                IF(DH(J,I,K).GE.DZ(J,I,K)) THEN        
                  SATNEW(J,I,K)=1                      
                ELSEIF(DH(J,I,K).LT.DZ(J,I,K).AND.     
     &          .NOT.ICBUND(J,I,K,1).EQ.0) THEN        
                  IF(INT(DH(J,I,K)).EQ.-111) THEN      
                    SATNEW(J,I,K)=1                    
                  ELSE                                 
                    SATNEW(J,I,K)=((DZ(J,I,K)-DH(J,I,K))/DZ(J,I,K))*
     &                            WC(J,I,K)/PRSITY(J,I,K)+          
     &                            DH(J,I,K)/DZ(J,I,K)*1             
                  ENDIF                                             
                ENDIF                                               
              ENDDO                                                 
            ENDDO                                                   
          ENDDO                                                     
C                                                                 
C-------MELD UZFLX AND QZZ ARRAY SO THAT UZFLX DOESN'T NEED TO BE DEALT
C-------WITH LATER IN THE CODE, IT'LL INSTEAD BE IMPLICIT IN QZ        
          DO K=1,(NLAY-1)                                           
            DO I=1,NROW                                             
              DO J=1,NCOL                                           
                IF(ICBUND(J,I,K,1).NE.0) THEN                       
C-------THE NEXT LINE THAT CHECKS FOR -111 IS DUE TO CONFINED LAYERS,  
C-------ENSURES QZ ISN'T SET TO UZFLX IN THE EVENT DH = -111           
                  IF(INT(DH(J,I,K)).EQ.-111) DH(J,I,K)=DZ(J,I,K)    
                  IF(DH(J,I,K).LT.1E-5 .AND. .NOT. 
     &                UZFLX(J,I,K+1).LE.0.0) THEN                        
                    QZ(J,I,K)=UZFLX(J,I,K+1)                        
                  ENDIF                                             
                ENDIF                                               
              ENDDO                                                 
            ENDDO                                                   
          ENDDO                                                     
C                                                                 
C-------PRSITY IS USED BELOW AND THEREFORE NEEDS TO BE UPDATED HERE    
C-------FOR BOTH THE SATURATED AND UNSATURATED CASE                    
          DO K=1,NLAY                                               
            DO I=1,NROW                                             
              DO J=1,NCOL
                IF(IUZFBND(J,I).EQ.0)THEN
                  WC(J,I,K)=PRSITY(J,I,K)
                ELSE
                  WC(J,I,K)=SATNEW(J,I,K)*PRSITY(J,I,K)               
                ENDIF
              ENDDO                                                 
            ENDDO                                                   
          ENDDO                                                     
          PRSITYSAV=>PRSITY                                         
          PRSITY=>WC                                                
        ENDIF
C
C--SET DH EQUAL TO DZ FOR THE CASE WHEN THE UZF PACKAGE IS ACTIVE 
        IF(IVER.EQ.2) THEN
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                IF(ICBUND(J,I,K,1).EQ.0) CYCLE 
                IF(iUnitTRNOP(7).GT.0.AND. IUZFOPTG.EQ.0) THEN
                  IF(IUZFBND(J,I).GT.0) THEN                        
                    DH(J,I,K)=DZ(J,I,K)                             
                  ENDIF                                             
                ELSEIF(iUnitTRNOP(7).GT.0.AND.IUZFOPTG.EQ.1) THEN   
                  !don't need to do anything in this case           
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDIF
C
C-------DIVIDE STORAGE BY CELL VOLUME TO GET DIMENSION (1/TIME)
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              THKSAT=DH(J,I,K)  ! WHEN UZT ACTIVE, DH IS DZ
              IF(iUnitTRNOP(7).GT.0) THEN
                QSTO(J,I,K)=QSTO(J,I,K)+(UZQSTO(J,I,K))/ 
     &                       (THKSAT*DELR(J)*DELC(I))    
              ENDIF
              IF(ABS(THKSAT-0.).LT.1.0E-5) QSTO(J,I,K)=0.
            ENDDO
          ENDDO
        ENDDO
C
C-------PULL INFILTRATED VALUES FROM UZFLX ARRAY IF FUZF OPTION USED
        DO I=1,NROW                                            
          DO J=1,NCOL                                          
            IF(ABS(IUZFBND(J,I)).GT.0) THEN                    
              FINFIL(J,I)=UZFLX(J,I,ABS(IUZFBND(J,I)))         
            ELSE                                               
              FINFIL(J,I)=UZFLX(J,I,1)                         
            ENDIF
          ENDDO
        ENDDO  
C
C--NOW THAT UZFLX HAS BEEN WOVEN INTO QZ, REDO CALCULATION OF 
C--MAXIMUM TIME INCREMENT DURING WHICH ANY PARTICLE
C--CANNOT MOVE MORE THAN ONE CELL IN THE Z-DIRECTION.
        IF(NLAY.LT.2) GOTO 440
        DO J=1,NCOL
          DO I=1,NROW
            DO K=2,NLAY
C       
              IF(ICBUND(J,I,K,1).NE.0) THEN
                TK=0.5*(QZ(J,I,K-1)+QZ(J,I,K))
                IF(TK.EQ.0) CYCLE
                TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
                IF(ABS(TK).LT.DTRACK) THEN
                  DTRACK=ABS(TK)
                  JTRACK=J
                  ITRACK=I
                  KTRACK=K
                ENDIF
              ENDIF
C       
            ENDDO
          ENDDO
        ENDDO
C
C--PRINT INFORMATION ON DTRACK
  440   WRITE(IOUT,500) DTRACK,KTRACK,ITRACK,JTRACK
  500   FORMAT(/1X,'MAXIMUM STEPSIZE DURING WHICH ANY PARTICLE CANNOT',
     &   ' MOVE MORE THAN ONE CELL IN Z-DIRECTION'/1X,'=',G11.4,
     &   '(WHEN MIN. R.F.=1) AT K=',I4,', I=',I4,', J=',I4)
C
C--DETERMINE STABILITY CRITERION ASSOCIATED WITH EXPLICIT FINITE
C--DIFFERENCE SOLUTION OF THE ADVECTION TERM
C
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,1).EQ.0) CYCLE
              TK=0.
              IF(K.GT.1) TK=TK+MAX(ABS(QZ(J,I,K-1)),ABS(QZ(J,I,K)))
              IF(TK.EQ.0) CYCLE
              TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
              IF(TK.LT.DTRACK2) THEN
                DTRACK2=TK
                JTRACK=J
                ITRACK=I
                KTRACK=K
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
C--PRINT INFORMATION ON DTRACK2
        WRITE(IOUT,550) DTRACK2,KTRACK,ITRACK,JTRACK
  550   FORMAT(/1X,'MAXIMUM STEPSIZE WHICH MEETS STABILITY CRITERION',
     &   ' OF THE ADVECTION TERM'/1X,
     &   '(FOR PURE FINITE-DIFFERENCE OPTION, MIXELM=0) '/1X,'=',G11.4,
     &   '(WHEN MIN. R.F.=1)  AT K=',I4,', I=',I4,', J=',I4)
C
        IF(NLAY.LT.2) GOTO 990
        DO J=1,NCOL
          DO I=1,NROW
            DO K=1,NLAY
              THKSAT=DH(J,I,K)
              IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
                IF(DOMINSAT) THEN
                  QZ(J,I,K)=QZ(J,I,K)/(DELR(J)*DELC(I))
                ELSE
                  QZ(J,I,K)=0
                  IF(K.GT.1) QZ(J,I,K-1)=0.
                ENDIF
              ELSE
                QZ(J,I,K)=QZ(J,I,K)/(DELR(J)*DELC(I))
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
C--DIVIDE STORAGE BY CELL VOLUME TO GET DIMENSION (1/TIME)
  990   CONTINUE
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              THKSAT=DH(J,I,K)
              IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
                QSTO(J,I,K)=0
              ELSE
                QSTO(J,I,K)=QSTO(J,I,K)/(THKSAT*DELR(J)*DELC(I))
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF                   
C                                                            
C--READ LAK FLOW TERMS                                       
C--VOLUMES (L**3); ALL OTHER TERMS(L**3/T)                   
C--IF LAK OPTION IS USED IN FLOW MODEL.                      
      IF(FLAK) THEN                                          
        TEXT='LAK'                                           
        IQ=26
        CALL READGS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ELSEIF(FLAKFLOWS) THEN
        TEXT='LAK FLOWS'                                  
        IQ=26
C        CALL READLAK(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
C     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT,NCOMP)         
C        ALLOCATE(NLAKES,LKNODE,NLKINIT)
        CALL READFLOWS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   ICBUND,FPRT,NCOMP,LKNODE,1,LAKL,LAKR,LAKC,QLAKGW,ICID,QAUX,
     &   NLKINIT,NLAKES,NFLOWTYPE,PKGFLOWS,CFLOWTYPE,
     &   NRCHCON,INOD1,INOD2,IDISP,QAREA,QN2N)
C
C-------ALLOCATE AND INITIALIZE LKT ARRAYS
        ALLOCATE(LAKNUMGW(LKNODE))
        LAKNUMGW=ICID
C        ALLOCATE(QPRECLAK(NLAKES),QRUNOFLAK(NLAKES),QWDRLLAK(NLAKES),
C     &  QETLAK(NLAKES),VOLOLAK(NLAKES),VOLNLAK(NLAKES),
C     &  DELVOLLAK(NLAKES))
C        QPRECLAK=0.
C        QRUNOFLAK=0.
C        QWDRLLAK=0.
C        QETLAK=0.
C
C-------MAP TO SFT ARRAYS
        DO IFL=1,NFLOWTYPE
          FLWTYP=CFLOWTYPE(IFL)
          SELECT CASE (FLWTYP)
            CASE("PRECIP              ") !L3/T
              QPRECLAK(:)=PKGFLOWS(IFL,:)
            CASE("RUNOFF              ") !L3/T
              QRUNOFLAK(:)=PKGFLOWS(IFL,:)
            CASE("WITHDRAW             ") !L3/T
              QWDRLLAK(:)=PKGFLOWS(IFL,:)
            CASE("EVAP                ") !L3/T
              QETLAK(:)=PKGFLOWS(IFL,:)
            CASE("VOLUME              ") !L3 - OLD VOLUME
              VOLOLAK(:)=PKGFLOWS(IFL,:)
            CASE("DELVOL              ") !L3/T
              DELVOLLAK(:)=PKGFLOWS(IFL,:)
            CASE DEFAULT
              WRITE(*,*) 'INCORRECT CFLOWTYPE IN FTL FILE'
              WRITE(IOUT,*) 'INCORRECT CFLOWTYPE IN FTL FILE'
              STOP
          END SELECT
        ENDDO
C
C-------CHECK NRCHCON=0 - NO LAKE-TO-LAKE CONNECTIONS
        IF(NRCHCON.GT.0) THEN
          WRITE(*,*) 'INVALID LAK CONNECTIONS IN FTL FILE',NRCHCON
          WRITE(IOUT,*) 
     &    'INVALID LAK CONNECTIONS IN FTL FILE',NRCHCON
          CALL USTOP(' ')
        ENDIF
C
C-------INITIALIZE OLD=NEW FOR THE FIRST TIME
        IF(KPER.EQ.1.AND.KSTP.EQ.1) THEN
          VOLNLAK=VOLOLAK
        ENDIF
C
C-------DEALLOCATE TEMP ARRAYS
        CALL DEALOCTEMPARR(PKGFLOWS,QAREA,QN2N,CFLOWTYPE,INOD1,
     &  INOD2,IDISP,ICID,QAUX,IP2PFLG)
      ENDIF
C
C--READ SFR FLOW TERMS                              
C--LENGTH (L); AREA (L**2), ALL OTHER TERMS(L**3/T) 
C--IF SFR OPTION IS USED IN FLOW MODEL.             
      IF(FSFR) THEN !SFR ACTS AS A BOUNDARY CONDITION ONLY
        TEXT='SFR'                                  
        IQ=30                                       
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ELSEIF(FSFRFLOWS) THEN
        IF(ISFTTR.EQ.0) THEN
          TEXT='SFR FLOWS SS'                                  
        ELSE
          TEXT='SFR FLOWS TR'                                  
        ENDIF
        IQ=30                                       
        ALLOCATE(NSTRM)
        CALL READFLOWS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   ICBUND,FPRT,NCOMP,NNGW,0,ISFL,ISFR,ISFC,QSFGW,ICID,QAUX,
     &   NSFINIT,NSTRM,NFLOWTYPE,PKGFLOWS,CFLOWTYPE,
     &   NRCHCON,INOD1,INOD2,IDISP,QAREA,QN2N)
C
C-------CHECK NNGW=NSTRM
        IF(NNGW.NE.NSTRM) THEN
          WRITE(*,*) 'INVALID NUMBER OF GW CONNECTIONS IN FTL FILE',NNGW
          WRITE(IOUT,*) 
     &    'INVALID NUMBER OF GW CONNECTIONS IN FTL FILE',NNGW
          CALL USTOP(' ')
        ENDIF
C
C-------ALLOCATE AND INITIALIZE SFT ARRAYS
        NSF2SF=NRCHCON
C        ALLOCATE(QPRECSF(NSTRM),QRUNOFSF(NSTRM),
C     &  QETSF(NSTRM),IEXIT(NSTRM),QPRECSFO(NSTRM),QRUNOFSFO(NSTRM),
C     &  QOUTSF(NSTRM),QOUTSFO(NSTRM),VOLSFO(NSTRM),VOLSFN(NSTRM),
C     &  SFLEN(NSTRM))
        QPRECSF=0.
        QRUNOFSF=0.
        QETSF=0.
        IEXIT=0
        QOUTSF=0.
        ALLOCATE(SFNAREA(NSF2SF),SFOAREA(NSF2SF),IDSPFLG(NSF2SF),
     &  INOD1SF(NSF2SF),INOD2SF(NSF2SF),QN2NSF(NSF2SF))
        INOD1SF=INOD1
        INOD2SF=INOD2
        IDSPFLG=IDISP
        SFNAREA=QAREA
        QN2NSF=QN2N
C
C-------MAP TO SFT ARRAYS
        DO IFL=1,NFLOWTYPE
          FLWTYP=CFLOWTYPE(IFL)
          SELECT CASE (FLWTYP)
            CASE("PRECIP              ") !L3/T
              QPRECSF(:)=PKGFLOWS(IFL,:)
            CASE("RUNOFF              ") !L3/T
              QRUNOFSF(:)=PKGFLOWS(IFL,:)
            CASE("EVAP                ") !L3/T
              QETSF(:)=PKGFLOWS(IFL,:)
            CASE("VOLUME              ") !L3 - NEW VOLUME
              VOLSFN(:)=PKGFLOWS(IFL,:)
            CASE("RCHLEN              ") !L
              SFLEN(:)=PKGFLOWS(IFL,:)
            CASE DEFAULT
              WRITE(*,*) 'INCORRECT CFLOWTYPE IN FTL FILE'
              WRITE(IOUT,*) 'INCORRECT CFLOWTYPE IN FTL FILE'
              STOP
          END SELECT
        ENDDO
C
C-------IDENTIFY CONSTANT CONCENTRATION NODES AND SET IBNDSF ARRAY
        IBNDSF=1
        DO I=1,NSSSF
          N=ISFNBC(I)
          IF(ISFBCTYP(I).EQ.3) IBNDSF(N)=-1
        ENDDO
C
C-------POPULATE SFT ARRAYS
        DO N=1,NSF2SF
        IFROM=INOD1SF(N)
          CALL GETFLOWDIR(INOD1SF,INOD2SF,QN2NSF,NSF2SF,N,IFROM,ITO)
C-------IDENTIFY EXIT CELLS AND CALCULATE TOTAL OUTFLOW
          IF(ITO.EQ.-999) THEN
            IEXIT(IFROM)=1
            QOUTSF(IFROM)=QOUTSF(IFROM)+ABS(QN2NSF(N))
          ENDIF
        ENDDO
C
C-------INITIALIZE OLD=NEW FOR THE FIRST TIME
        IF(KPER.EQ.1.AND.KSTP.EQ.1) THEN
          QPRECSFO=QPRECSF
          QRUNOFSFO=QRUNOFSF
          QOUTSFO=QOUTSF
          SFOAREA=SFNAREA
          VOLSFO=VOLSFN
        ENDIF
C
C-------DEALLOCATE TEMP ARRAYS
        CALL DEALOCTEMPARR(PKGFLOWS,QAREA,QN2N,CFLOWTYPE,INOD1,
     &  INOD2,IDISP,ICID,QAUX,IP2PFLG)
      ENDIF                                                  
C                                                            
C--READ UZF -> SFR & UZF -> LAK FLOWS (L**3/T)               
C--IF UZF AND SFR OR UZF AND LAKE OR BOTH ARE USED IN THE FLOW MODEL
C      IF((iUnitTRNOP(7).GT.0.AND.FSFR).OR.
C     &   (iUnitTRNOP(7).GT.0.AND.FLAK)) THEN                   
C        TEXT='UZF CONNECTIONS'                                 
C        IQ=32                                                  
C        CALL READUZFCONNECT(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,
C     &   TEXT,NCOMP)                                           
C      ENDIF                                                    
C
C--READ PACKAGE TO PACKAGE FLOWS
C
C--SFR-LAK
      ALLOCATE(NSFR2LAK)
      NSFR2LAK=0
      IF(FSFRLAK) THEN
        TEXT='CONNECT SFR LAK'
        CALL PKG2PKGFLOW(INUF,IOUT,KSTP,KPER,TEXT,FPRT,
     1  NSFR2LAK,INOD1SFLK,INOD2SFLK,QSFR2LAK,IP2PFLG)
C
        CALL DEALOCTEMPARR(PKGFLOWS,QAREA,QN2N,CFLOWTYPE,INOD1,
     &  INOD2,IDISP,ICID,QAUX,IP2PFLG)
      ENDIF
C
C--SFR-UZF
      ALLOCATE(NSFR2UZF)
      NSFR2UZF=0
      IF(FSFRUZF) THEN
        TEXT='CONNECT SFR UZF'
        CALL PKG2PKGFLOW(INUF,IOUT,KSTP,KPER,TEXT,FPRT,
     1  NSFR2UZF,INOD1SFUZ,INOD2SFUZ,QSFR2UZF,IP2PFLG)
C
        IF(NSFR2UZF.GT.0) THEN
          ALLOCATE(IUZCODESF(NSFR2UZF))
          IUZCODESF=IP2PFLG
        ENDIF
C
        CALL DEALOCTEMPARR(PKGFLOWS,QAREA,QN2N,CFLOWTYPE,INOD1,
     &  INOD2,IDISP,ICID,QAUX,IP2PFLG)
      ENDIF
C
C--LAK-UZF
      ALLOCATE(NLAK2UZF)
      NLAK2UZF=0
      IF(FLAKUZF) THEN
        TEXT='CONNECT LAK UZF'
        CALL PKG2PKGFLOW(INUF,IOUT,KSTP,KPER,TEXT,FPRT,
     1  NLAK2UZF,INOD1LKUZ,INOD2LKUZ,QLAK2UZF,IP2PFLG)
C
        IF(NLAK2UZF.GT.0) THEN
          ALLOCATE(IUZCODELK(NLAK2UZF))
          IUZCODELK=IP2PFLG
        ENDIF
C
        CALL DEALOCTEMPARR(PKGFLOWS,QAREA,QN2N,CFLOWTYPE,INOD1,
     &  INOD2,IDISP,ICID,QAUX,IP2PFLG)
      ENDIF
C
C--UZF-SNK
      ALLOCATE(NSNK2UZF)
      NSNK2UZF=0
      IF(FSNKUZF) THEN
        TEXT='CONNECT SNK UZF'
        CALL PKG2PKGFLOW(INUF,IOUT,KSTP,KPER,TEXT,FPRT,
     1  NSNK2UZF,INOD1SKUZ,INOD2SKUZ,QSNK2UZF,IP2PFLG)
C
        IF(NSNK2UZF.GT.0) THEN
          ALLOCATE(IUZCODESK(NSNK2UZF))
          IUZCODESK=IP2PFLG
        ENDIF
C
        CALL DEALOCTEMPARR(PKGFLOWS,QAREA,QN2N,CFLOWTYPE,INOD1,
     &  INOD2,IDISP,ICID,QAUX,IP2PFLG)
      ENDIF
C
C--CHECK IF MAXIMUM NUMBER OF POINT SINKS/SOURCES EXCEEDED.
C--IF SO STOP
      WRITE(IOUT,801) NTSS
  801 FORMAT(//1X,'TOTAL NUMBER OF POINT SOURCES/SINKS PRESENT',
     & ' IN THE FLOW MODEL =',I6)
      IF(NTSS.GT.MXSS) THEN
        WRITE(*,802) MXSS
  802   FORMAT(/1X,'ERROR: MAXIMUM NUMBER OF SINKS/SOURCES ALLOWED',
     &   ' [MXSS] =',I6
     &   /1X,'INCREASE VALUE OF [MXSS] IN [SSM] PACKAGE INPUT FILE')
        CALL USTOP(' ')
      ENDIF
C
C-------SET UP FOR CONSTANT CONC BOUNDARY VARYING AS PER IRCH ARRAY
      DO NUM=1,NTSS                                                
        IF(KSSZERO(NUM).EQ.1) THEN                                 
          I=SS(2,NUM)                                              
          J=SS(3,NUM)                                              
          KOLD=SS(1,NUM)                                           
          K=IRCH(J,I)                                              
          SS(1,NUM)=K                                              
          DO INDEX=1,NCOMP                                         
C-----------RESET ICBUND TO A POSITIVE NUMBER                      
            IF(KOLD.GE.1) THEN                                     
              ICBUND(J,I,KOLD,INDEX)=ABS(ICBUND(J,I,KOLD,INDEX))   
            ENDIF                                                  
C-----------SET ICBUND TO NEGATIVE FOR CONSTANT CONCENTRATION BOUNDARY 
            IF(SSMC(INDEX,NUM).GE.0) THEN                  
              CNEW(J,I,K,INDEX)=SSMC(INDEX,NUM)            
              ICBUND(J,I,K,INDEX)=-ABS(ICBUND(J,I,K,INDEX))
            ENDIF                                          
C                                                          
          ENDDO                                            
        ENDIF                                              
      ENDDO                                                
C                                                          
C--IDENTIFY CELLS IN THE VICINITY OF POINT SINKS OR SOURCES
      DO NUM=1,NTSS
        K=SS(1,NUM)
        I=SS(2,NUM)
        J=SS(3,NUM)
        KM1=MAX(K-1,1)
        KP1=MIN(K+1,NLAY)
        DO KK=KM1,KP1
          IM1=MAX(I-1,1)
          IP1=MIN(I+1,NROW)
          DO II=IM1,IP1
            JM1=MAX(J-1,1)
            JP1=MIN(J+1,NCOL)
            DO JJ=JM1,JP1
              IF(ICBUND(JJ,II,KK,1).EQ.1) ICBUND(JJ,II,KK,1)=1000
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--DIVIDE RECH, EVTR, Q_SS BY AQUIFER VOLUME
C--TO GET FLUXES OF SINKS/SOURCES PER UNIT AQUIFER VOLUME.
C--ALSO DETERMINE STEPSIZE WHICH MEETS STABILITY CRITERION
C--FOR SOLVING THE SINK/SOURCE TERM WITH EXPLICIT SCHEME.
      DTSSM=1.E30
      KSSM=0       
      ISSM=0
      JSSM=0
C
      IF(.NOT.FRCH) GOTO 950
      DO I=1,NROW
        DO J=1,NCOL
          K=IRCH(J,I)
          IF(K.EQ.0) CYCLE
          VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
          IF(ABS(VOLAQU).LE.1.E-5) VOLAQU=1.E-5
          IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN
            IF(DRYON) THEN
              RECH(J,I)=RECH(J,I)/ABS(VOLAQU) !MAINTAIN CORRECT SIGN
            ELSE
              RECH(J,I)=0.
            ENDIF
          ELSE
            RECH(J,I)=RECH(J,I)/VOLAQU
          ENDIF
C          IF(ABS(VOLAQU-0.).LT.1.0E-5) RECH(J,I)=0.
          IF(RECH(J,I).LE.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE
          TM=PRSITY(J,I,K)/RECH(J,I)
          IF(ABS(TM).LT.DTSSM) THEN
            DTSSM=ABS(TM)
            KSSM=K
            ISSM=I
            JSSM=J
          ENDIF
        ENDDO
      ENDDO
C
C--DIVIDE INFILTRATED VOL BY AQUIFER VOLUME TO GET PER UNIT AQ. VOL
  950 IF(.NOT.FUZFFLOWS) GOTO 951
      DO I=1,NROW                           
        DO J=1,NCOL                         
          IF(FINFIL(J,I).EQ.0) CYCLE        
          K=ABS(IUZFBND(J,I))
          VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)            
          IF(ABS(VOLAQU).LE.1.E-5) VOLAQU=1.E-5
          IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN
            FINFIL(J,I)=0.                            
          ELSE                                        
            FINFIL(J,I)=FINFIL(J,I)/VOLAQU            
          ENDIF                                       
          IF(FINFIL(J,I).LE.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE
          TM=PRSITY(J,I,K)/FINFIL(J,I)
          IF(ABS(TM).LT.DTSSM) THEN   
            DTSSM=ABS(TM)             
            KSSM=K                    
            ISSM=I                    
            JSSM=J                    
          ENDIF                       
        ENDDO                         
      ENDDO                           
C
  951 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 955
      DO I=1,NROW
        DO J=1,NCOL
          K=IEVT(J,I)
          IF(K.EQ.0) CYCLE
          VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
          IF(ABS(VOLAQU).LE.1.E-5) VOLAQU=1.E-5
          IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN
            IF(DRYON) THEN
              EVTR(J,I)=EVTR(J,I)/ABS(VOLAQU) !MAINTAIN CORRECT SIGN
            ELSE
              EVTR(J,I)=0
            ENDIF
          ELSE
            EVTR(J,I)=EVTR(J,I)/VOLAQU
          ENDIF
C          IF(ABS(VOLAQU-0.).LT.1.0E-5) EVTR(J,I)=0.
          IF(EVTR(J,I).EQ.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE
            TM=PRSITY(J,I,K)/EVTR(J,I)
            IF(ABS(TM).LT.DTSSM) THEN
              DTSSM=ABS(TM)
              KSSM=K
              ISSM=I
              JSSM=J
            ENDIF
        ENDDO
      ENDDO
C
C--PERFORM LOOP ONCE FOR UZET AND AGAIN FOR GWET
C--(UZET)                                       
  955 IF(.NOT.(iUnitTRNOP(7).GT.0)) GOTO 960
      IF(.NOT.IETFLG) GOTO 960
      DO K=1,NLAY             
        DO I=1,NROW           
          DO J=1,NCOL         
            VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
            IF(ABS(VOLAQU).LE.1.E-5) VOLAQU=1.E-5
            IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN
              UZET(J,I,K)=0                             
            ELSE                                        
              UZET(J,I,K)=UZET(J,I,K)/VOLAQU            
            ENDIF                                       
            IF(UZET(J,I,K).EQ.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE
            TM=PRSITY(J,I,K)/UZET(J,I,K)
            IF(ABS(TM).LT.DTSSM) THEN   
              DTSSM=ABS(TM)             
              KSSM=K                    
              ISSM=I                    
              JSSM=J                    
            ENDIF                       
          ENDDO                         
        ENDDO                           
      ENDDO                             
C--(GWET)                               
      IF(.NOT.IETFLG) GOTO 960          
C      DO K=1,NLAY                       
        DO I=1,NROW                     
          DO J=1,NCOL                   
            K=IGWET(J,I)
            VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)             
            IF(ABS(VOLAQU).LE.1.E-5) VOLAQU=1.E-5
            IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN 
              GWET(J,I)=0                              
            ELSE                                         
              GWET(J,I)=GWET(J,I)/VOLAQU             
            ENDIF                                        
            IF(GWET(J,I).EQ.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE
            TM=PRSITY(J,I,K)/GWET(J,I)
            IF(ABS(TM).LT.DTSSM) THEN   
              DTSSM=ABS(TM)             
              KSSM=K                    
              ISSM=I                    
              JSSM=J                    
            ENDIF                       
          ENDDO                         
        ENDDO                           
C      ENDDO                             
C
  960 IF(NTSS.LE.0) GOTO 970
      DO NUM=1,NTSS
        K=SS(1,NUM)
        I=SS(2,NUM)
        J=SS(3,NUM)
        IQ=SS(6,NUM)
        IF(IQ.EQ.26.AND.K.EQ.0.AND.I.EQ.0) CYCLE
        VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
        IF(ABS(VOLAQU).LE.1.E-5) VOLAQU=1.E-5
        IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN
          IF(DRYON) THEN
            SS(5,NUM)=SS(5,NUM)/ABS(VOLAQU) !MAINTAIN CORRECT SIGN
          ELSE
            SS(5,NUM)=0
          ENDIF
        ELSE
          SS(5,NUM)=SS(5,NUM)/VOLAQU
        ENDIF
C        IF(ABS(VOLAQU-0.).LT.1.0E-5) SS(5,NUM)=0.
        IF(SS(5,NUM).LE.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE
        TM=PRSITY(J,I,K)/SS(5,NUM)
        IF(ABS(TM).LT.DTSSM) THEN
          DTSSM=ABS(TM)
          KSSM=K
          ISSM=I
          JSSM=J
        ENDIF
      ENDDO
C
C--PRINT INFORMATION ON DTSSM
  970 WRITE(IOUT,1000) DTSSM,KSSM,ISSM,JSSM
 1000 FORMAT(/1X,'MAXIMUM STEPSIZE WHICH MEETS STABILITY CRITERION',
     & ' OF THE SINK & SOURCE TERM'/1X,'=',G11.4,
     & '(WHEN MIN. R.F.=1)  AT K=',I4,', I=',I4,
     & ', J=',I4)
C
C--SYNCHRONIZE ICBUND CONDITIONS OF ALL SPECIES
      IF(NCOMP.EQ.1) GOTO 1999
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            DO INDEX=2,NCOMP
              IF(ICBUND(J,I,K,INDEX).GE.0) THEN
                ICBUND(J,I,K,INDEX)=IABS(ICBUND(J,I,K,1))
              ELSEIF(ICBUND(J,I,K,1).EQ.0) THEN
                ICBUND(J,I,K,INDEX)=0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
 1999 RETURN
      END
C
C
      SUBROUTINE READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,FPRT)
C *****************************************************************
C THIS SUBROUTINE READS HEADS AND VOLUMETRIC FLUXES ACROSS CELL
C INTERFACES FROM AN UNFORMATTED FILE SAVED BY THE FLOW MODEL.
C *****************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
C
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,IPRTFM,K,I,J,
     &          KKSTP,KKPER,NC,NR,NL
      REAL      BUFF
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL
      ENDIF
C
C--CHECK INTERFACE
C      WRITE(*,'(13hReadHQ REQD: ,a10,6hREAD: ,a10)') TEXT,LABEL
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        CALL USTOP(' ')
      ENDIF
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH CELL IN THE GRID
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) (((BUFF(J,I,K),J=1,NCOL),I=1,NROW),K=1,NLAY)
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) (((BUFF(J,I,K),J=1,NCOL),I=1,NROW),K=1,NLAY)
      ENDIF
C
C--PRINT OUT INPUT FOR CHECKING IF REQUESTED
      IF(FPRT.NE.'Y'.AND.FPRT.NE.'y') RETURN
      IPRTFM=1
      DO K=1,NLAY
        WRITE(IOUT,50) K
        CALL RPRINT(BUFF(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,0,IPRTFM,IOUT)
      ENDDO
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE.'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
   10 FORMAT(/44X,'LAYER LOCATION OF ',A16,'FLOW TERM'
     & /44X,43('-'))
   50 FORMAT(/61X,'LAYER ',I3)
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,LOCLAY,FPRT)
C *****************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF DIFFUSIVE
C SINK/SOURCE TERMS (RECHARGE AND EVAPOTRANSPIRATION) FROM AN
C UNFORMATTED FILE SAVED BY THE FLOW MODEL.
C *****************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
C
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,IPRTFM,I,J,
     &          KKSTP,KKPER,NC,NR,NL,LOCLAY
      REAL      BUFF
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW),LOCLAY(NCOL,NROW)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL
      ENDIF
C
C--CHECK INTERFACE
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        CALL USTOP(' ')
      ENDIF
C
C--READ LAYER LOCATION IF FLOW TERM IS RECHARGE OR E.T.
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) ((LOCLAY(J,I),J=1,NCOL),I=1,NROW)
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) ((LOCLAY(J,I),J=1,NCOL),I=1,NROW)
      ENDIF
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH CELL IN THE GRID
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) ((BUFF(J,I),J=1,NCOL),I=1,NROW)
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) ((BUFF(J,I),J=1,NCOL),I=1,NROW)
      ENDIF
C
C--PRINT OUT INPUT FOR CHECKING IF REQUESTED
      IF(FPRT.NE.'Y'.AND.FPRT.NE.'y') RETURN
      IPRTFM=1
      CALL RPRINT(BUFF(1,1),TEXT,
     & 0,KSTP,KPER,NCOL,NROW,0,IPRTFM,IOUT)
      IPRTFM=3
      WRITE(IOUT,10)
      CALL IPRINT(LOCLAY(1,1),TEXT,0,KSTP,KPER,NCOL,NROW,
     & 0,IPRTFM,IOUT)
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE.'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
   10 FORMAT(/60X,'LAYER INDEX')
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
C *********************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF POINT SINK/SOURCE
C FLOW TERMS FROM AN UNFORMATTED FILE SAVED BY THE FLOW MODEL.
C *********************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT,ICTSPKG
C
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,NUM,N,MXSS,NTSS,NSS,ICBUND,IQ,ID,
     &          KKK,III,JJJ,ITEMP
      REAL      BUFF,SS,QSS,QSTEMP
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),SS(8,MXSS)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NUM
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NUM
      ENDIF
C
C--CHECK INTERFACE
C      WRITE(*,'(13hReadPS REQD: ,a10,6hREAD: ,a10)') TEXT,LABEL
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        CALL USTOP(' ')
      ENDIF
C
C--RETURN IF NUM=0
      IF(NUM.LE.0) RETURN
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH POINT SINK OR SOURCE
      DO N=1,NUM
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) K,I,J,QSTEMP
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) K,I,J,QSTEMP
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &              WRITE(IOUT,50) K,I,J,QSTEMP        
C
C--IF ALREADY DEFINED AS A SOURCE OF USER-SPECIFIED CONCENTRATION,
C--STORE FLOW RATE QSTEMP                              
        DO ITEMP=1,NSS
          KKK=SS(1,ITEMP)
          III=SS(2,ITEMP)
          JJJ=SS(3,ITEMP)
          QSS=SS(5,ITEMP)
          ID =SS(6,ITEMP)       
          IF(KKK.NE.K.OR.III.NE.I.OR.JJJ.NE.J.OR.ID.NE.IQ) CYCLE
          IF(ABS(QSS).GT.0) CYCLE                   
          SS(5,ITEMP)=QSTEMP
          SS(7,ITEMP)=0
          IF(LABEL.EQ.'WEL             '.AND.ICTSPKG.EQ.1)SS(8,ITEMP)=N
C
C--MARK CELLS NEAR THE SINK/SOURCE                   
          IF(QSTEMP.LT.0 .AND. ICBUND(J,I,K).GT.0) THEN                
            ICBUND(J,I,K)=1000+IQ
          ELSEIF(ICBUND(J,I,K).GT.0 ) THEN
            ICBUND(J,I,K)=1020+IQ
          ENDIF
          GOTO 100
        ENDDO
C       
C--OTHERWISE, ADD TO THE SS ARRAY
        NTSS=NTSS+1
        IF(NTSS.GT.MXSS) CYCLE
        SS(1,NTSS)=K
        SS(2,NTSS)=I
        SS(3,NTSS)=J
        SS(4,NTSS)=0.
        SS(5,NTSS)=QSTEMP
        SS(6,NTSS)=IQ
        SS(7,NTSS)=0.
        IF(LABEL.EQ.'WEL             '.AND.ICTSPKG.EQ.1) SS(8,NTSS)=N
        IF(QSTEMP.LT.0 .AND. ICBUND(J,I,K).GT.0) THEN                   
          ICBUND(J,I,K)=1000+IQ
        ELSEIF(ICBUND(J,I,K).GT.0) THEN
          ICBUND(J,I,K)=1020+IQ
        ENDIF
  100   CONTINUE
      ENDDO  
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
   50 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7)
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE READGS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
C *********************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF SINK/SOURCE GROUPS
C THAT ARE CONNECTED FROM THE FLOW-TRANSPORT LINK FILE
C *********************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT,ICTSPKG
C
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,NUM,N,MXSS,NTSS,NSS,ICBUND,IQ,ID,
     &          KKK,III,JJJ,ITEMP,IGROUP
      REAL      BUFF,SS,QSS,QSTEMP,QSW
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),SS(8,MXSS)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NUM
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NUM
      ENDIF
C
C--CHECK INTERFACE
C      WRITE(*,'(13hReadGS REQD: ,a10,6hREAD: ,a10)') TEXT,LABEL
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        CALL USTOP(' ')
      ENDIF
C
C--RETURN IF NUM=0
      IF(NUM.LE.0) RETURN
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH POINT SINK OR SOURCE
      DO N=1,NUM
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) K,I,J,QSTEMP,IGROUP,QSW
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) K,I,J,QSTEMP,IGROUP,QSW
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,50) K,I,J,QSTEMP,IGROUP,QSW
C
C--IF ALREADY DEFINED AS A SOURCE OF USER-SPECIFIED CONCENTRATION,
C--STORE FLOW RATE QSTEMP                               
        DO ITEMP=1,NSS
          KKK=SS(1,ITEMP)
          III=SS(2,ITEMP)
          JJJ=SS(3,ITEMP)
          QSS=SS(5,ITEMP)
          ID =SS(6,ITEMP)       
          IF(KKK.NE.K.OR.III.NE.I.OR.JJJ.NE.J.OR.ID.NE.IQ) CYCLE
          IF(ABS(QSS).GT.0) CYCLE                      
          SS(5,ITEMP)=QSTEMP
          SS(7,ITEMP)=IGROUP
          IF(LABEL.EQ.'MNW             '.AND.ICTSPKG.EQ.0) SS(8,ITEMP)=N
C
C--MAKR CELLS NEAR THE SINK/SOURCE                   
          IF(QSTEMP.LT.0 .AND. ICBUND(J,I,K).GT.0 ) THEN
            ICBUND(J,I,K)=1000+IQ
          ELSEIF(ICBUND(J,I,K).GT.0) THEN              
            ICBUND(J,I,K)=1020+IQ   
          ENDIF
          GOTO 100
        ENDDO
C       
C--OTHERWISE, ADD TO THE SS ARRAY
        NTSS=NTSS+1
        IF(NTSS.GT.MXSS) CYCLE
        SS(1,NTSS)=K
        SS(2,NTSS)=I
        SS(3,NTSS)=J
        IF(LABEL.EQ.'LAK             ') THEN
          DO ITEMP=1,NSS
            JJJ=SS(3,ITEMP) !LAKE NUMBER
            ID =SS(6,ITEMP)       
            IF(IGROUP.EQ.JJJ.AND.ID.EQ.IQ) THEN
              SS(4,NTSS)=SS(4,ITEMP)
              EXIT
            ENDIF
          ENDDO
        ELSE
          SS(4,NTSS)=0.
        ENDIF
        SS(5,NTSS)=QSTEMP
        SS(6,NTSS)=IQ
        SS(7,NTSS)=IGROUP
        IF(LABEL.EQ.'MNW             '.AND.ICTSPKG.EQ.0) SS(8,NTSS)=N
        IF(QSTEMP.LT.0 .AND. ICBUND(J,I,K).GT.0) THEN                 
          ICBUND(J,I,K)=1000+IQ
        ELSEIF(ICBUND(J,I,K).GT.0) THEN                       
          ICBUND(J,I,K)=1020+IQ
        ENDIF
  100   CONTINUE
      ENDDO  
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
   50 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7,
     & ' SS CODE',I5,5X,'EXTERNAL FLOW',G15.7)  
C
C--RETURN
      RETURN
      END
C
C
      FUNCTION CREWET(NCOL,NROW,NLAY,CNEW,ICBUND,XBC,YBC,ZBC,
     & JJ,II,KK)
C *****************************************************************
C THIS FUNCTION OBTAINS CONCENTRATION AT A REWET CELL (JJ,II,KK)
C FROM CONCENTRATIONS AT NEIGHBORING NODES WITH INVERSE DISTANCE
C (POWER 2) WEIGHTING .
C *****************************************************************
C last modified: 02-15-2005
C
      USE MIN_SAT  
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,ICBUND,JJ,II,KK
      REAL      XBC,YBC,ZBC,CTMP,CNEW,CREWET,D2,D2SUM
      DIMENSION ICBUND(NCOL,NROW,NLAY),CNEW(NCOL,NROW,NLAY),
     &          XBC(NCOL),YBC(NROW),ZBC(NCOL,NROW,NLAY)
C
C--INITIALIZE
      D2SUM=0
      CTMP=0
C
C--ACCUMULATE CONCENTRATIONS AT NEIGHBORING NODELS
C--IN THE LAYER DIRECTION
      IF(NLAY.EQ.1) GOTO 10
      IF(KK-1.GT.0) THEN
        IF(ICBUND(JJ,II,KK-1).NE.0) THEN
          D2=(ZBC(JJ,II,KK)-ZBC(JJ,II,KK-1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ,II,KK-1)/D2
          ELSE
            CTMP=CNEW(JJ,II,KK-1)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
      IF(KK+1.LE.NLAY) THEN
        IF(ICBUND(JJ,II,KK+1).NE.0) THEN
          D2=(ZBC(JJ,II,KK)-ZBC(JJ,II,KK+1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ,II,KK+1)/D2
          ELSE
            CTMP=CNEW(JJ,II,KK+1)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
C
C--IN THE ROW DIRECTION
   10 IF(NROW.EQ.1) GOTO 20
      IF(II-1.GT.0) THEN
        IF(ICBUND(JJ,II-1,KK).NE.0) THEN
          D2=(YBC(II)-YBC(II-1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ,II-1,KK)/D2
          ELSE
            CTMP=CNEW(JJ,II-1,KK)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
      IF(II+1.LE.NROW) THEN
        IF(ICBUND(JJ,II+1,KK).NE.0) THEN
          D2=(YBC(II)-YBC(II+1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ,II+1,KK)/D2
          ELSE
            CTMP=CNEW(JJ,II+1,KK)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
C
C--IN THE COLUMN DIRECTION
   20 IF(NCOL.EQ.1) GOTO 30
      IF(JJ-1.GT.0) THEN
        IF(ICBUND(JJ-1,II,KK).NE.0) THEN
          D2=(XBC(JJ)-XBC(JJ-1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ-1,II,KK)/D2
          ELSE
            CTMP=CNEW(JJ-1,II,KK)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
      IF(JJ+1.LE.NCOL) THEN
        IF(ICBUND(JJ+1,II,KK).NE.0) THEN
          D2=(XBC(JJ)-XBC(JJ+1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ+1,II,KK)/D2
          ELSE
            CTMP=CNEW(JJ+1,II,KK)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
C
C--OBTAIN WEIGHTED CONCENTRATION
   30 IF(D2SUM.EQ.0) THEN
        IF(.NOT.DRYON) THEN
          ICBUND(JJ,II,KK)=0
        ENDIF
      ELSE
        CTMP=CTMP/D2SUM
      ENDIF
C
C--ASSIGN WEIGHTED CONCENTRATION TO CREWET
  100 CREWET=CTMP
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE READSFR(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT,NCOMP)
C *********************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF SFR PAKCAGE
C *********************************************************************
C last modified: 08-10-2012
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT 
      USE SFRVARS
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,NUM,N,MXSS,NTSS,NSS,ICBUND,IQ,ID, !IFTLFMT,
     &          KKK,III,JJJ,ITEMP,IGROUP,NCOMP,ICNT,IS,IR,NN
      REAL      BUFF,SS,QSS,QSTEMP,QSW
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),SS(8,MXSS)
C      INTEGER   ISEG,IREACH
C      COMMON /FTL/IFTLFMT
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
C        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NSTRM,NINTOT,MXSGMT,MXRCH
      ELSEIF(IFTLFMT.EQ.1) THEN
C        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NSTRM,NINTOT,MXSGMT,
C     1    MXRCH
      ENDIF
C
C--DEALLOCATE AND ALLOCATE ARRAYS FOR STORING FLOW TERMS
cvsb      IF(ALLOCATED(ISFL)) DEALLOCATE(ISFL)
cvsb      IF(ALLOCATED(ISFR)) DEALLOCATE(ISFR)
cvsb      IF(ALLOCATED(ISFC)) DEALLOCATE(ISFC)
cvsb      IF(ALLOCATED(ISEG)) DEALLOCATE(ISEG)
cvsb      IF(ALLOCATED(IREACH)) DEALLOCATE(IREACH)
cvsb      IF(ALLOCATED(SFLEN)) DEALLOCATE(SFLEN)
cvsb      IF(ALLOCATED(SFNAREA)) DEALLOCATE(SFNAREA)
C      IF(ALLOCATED(SFOAREA)) DEALLOCATE(SFOAREA)
cvsb      IF(ALLOCATED(QPRECSF)) DEALLOCATE(QPRECSF)
cvsb      IF(ALLOCATED(QRUNOFSF)) DEALLOCATE(QRUNOFSF)
C      IF(ALLOCATED(QPRECSFO)) DEALLOCATE(QPRECSFO)
C      IF(ALLOCATED(QRUNOFSFO)) DEALLOCATE(QRUNOFSFO)
cvsb      IF(ALLOCATED(QSFGW)) DEALLOCATE(QSFGW)
cvsb      IF(ALLOCATED(QOUTSF)) DEALLOCATE(QOUTSF)
C      IF(ALLOCATED(QOUTSFO)) DEALLOCATE(QOUTSFO)
cvsb      IF(ALLOCATED(QETSF)) DEALLOCATE(QETSF)
cvsb      IF(ALLOCATED(NIN)) DEALLOCATE(NIN)
C.....NINTOT SIZED ARRAYS
cvsb      IF(ALLOCATED(QINSF)) DEALLOCATE(QINSF)
C      IF(ALLOCATED(QINSFO)) DEALLOCATE(QINSFO)
cvsb      IF(ALLOCATED(INSEG)) DEALLOCATE(INSEG)
cvsb      IF(ALLOCATED(INRCH)) DEALLOCATE(INRCH)
cvsb      IF(ALLOCATED(IDSPFLG)) DEALLOCATE(IDSPFLG)
cvsb      IF(ALLOCATED(IDXNIN)) DEALLOCATE(IDXNIN)
C.....INDEXING TO GET ISTRM FROM SEG AND RCH NUMBERS
cvsb      IF(ALLOCATED(ISTRM)) DEALLOCATE(ISTRM)
C
      ALLOCATE(ISFL(NSTRM),ISFR(NSTRM),ISFC(NSTRM),ISEG(NSTRM),
     1  SFLEN(NSTRM),SFNAREA(NSTRM),IREACH(NSTRM), !SFOAREA(NSTRM),
     1  QPRECSF(NSTRM),QRUNOFSF(NSTRM),
!     1  QPRECSFO(NSTRM),QRUNOFSFO(NSTRM),
C     1  QSFGW(NSTRM),QOUTSF(NSTRM),QETSF(NSTRM),NIN(NSTRM),
     1  IEXIT(NSTRM)) !,QOUTSFO(NSTRM))
C      ALLOCATE(QINSF(NINTOT),INSEG(NINTOT),INRCH(NINTOT),
C     1  IDSPFLG(NINTOT),IDXNIN(NSTRM+1)) !,QINSFO(NINTOT))
C      ALLOCATE(ISTRM(MXRCH,MXSGMT))
C      ISTRM=0
C      IDXNIN=0
      IEXIT=1
C
      IF(KKPER.EQ.1.AND.KKSTP.EQ.1) THEN
        ALLOCATE(SFOAREA(NSTRM),QPRECSFO(NSTRM),QRUNOFSFO(NSTRM),
     1    QOUTSFO(NSTRM))
C        ALLOCATE(QINSFO(NINTOT))
      ENDIF
C
C--CHECK INTERFACE
C      WRITE(*,'(14hReadSFR REQD: ,a10,6hREAD: ,a10)') TEXT,LABEL
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        WRITE(IOUT,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        WRITE(IOUT,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        WRITE(IOUT,2) NC,NR,NL
        CALL USTOP(' ')
      ELSEIF(NSTRM.NE.NSFINIT) THEN
        WRITE(*,5) NSTRM
        WRITE(IOUT,5) NSTRM
        CALL USTOP(' ')
      ENDIF
C
C--RETURN IF NSTRM=0
      IF(NSTRM.EQ.0) RETURN
C
      IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,10) 
10    FORMAT(1X,' GW-L GW-R GW-C SEG# RCH# ',
     &  '   LENGTH       AREA         FLOW-GW      FLOWOUT      RUNOFF',
     &  '        PRECIP       EVAP     IN-NODES',/,
     &  1X,5(1X,4('-')),7(1X,12('-')),1X,8('-'))
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH LAKE INFLOWS AND OUTFLOWS
      ICNT=0
      DO N=1,NSTRM

        IF(N.EQ.4)THEN
        CONTINUE
        ENDIF

        IF(IFTLFMT.EQ.0) THEN
C          READ(INUF) ISFL(N),ISFR(N),ISFC(N),ISEG(N),
C     1      IREACH(N),SFLEN(N),SFNAREA(N),QSFGW(N),QOUTSF(N),
C     1      QRUNOFSF(N),QPRECSF(N),QETSF(N),NIN(N)
C          IF(NIN(N).GT.0) IDXNIN(N)=ICNT+1
C          DO I=1,NIN(N)
C            ICNT=ICNT+1
C            READ(INUF) INSEG(ICNT),INRCH(ICNT),QINSF(ICNT),
C     1        IDSPFLG(ICNT)
C          ENDDO
        ELSEIF(IFTLFMT.EQ.1) THEN
C          READ(INUF,*) ISFL(N),ISFR(N),ISFC(N),ISEG(N),
C     1      IREACH(N),SFLEN(N),SFNAREA(N),QSFGW(N),QOUTSF(N),
C     1      QRUNOFSF(N),QPRECSF(N),QETSF(N),NIN(N)
C          IF(NIN(N).GT.0) IDXNIN(N)=ICNT+1
C          DO I=1,NIN(N)
C            ICNT=ICNT+1
C            READ(INUF,*) INSEG(ICNT),INRCH(ICNT),QINSF(ICNT),
C     1        IDSPFLG(ICNT)
C          ENDDO
        ENDIF
C.......STORE N
C        ISTRM(IREACH(N),ISEG(N))=N
      ENDDO  
C      IDXNIN(NSTRM+1)=ICNT+1
CC
CC--CHECK IF NINTOT MATCHES THE SUM OF NIN(1:NSTRM)
C      IF(NINTOT.NE.ICNT) THEN
C        WRITE(*,*) 'MISMATCH IN FTL FILE NINTOT<>ICNT'
C        WRITE(IOUT,*) 'MISMATCH IN FTL FILE NINTOT<>ICNT'
C        CALL USTOP(' ')
C      ENDIF
CC
CC--INITIALIZE OLD=NEW FOR THE FIRST TIME
C      IF(KKPER.EQ.1.AND.KKSTP.EQ.1) THEN
C        QPRECSFO=QPRECSF
C        QRUNOFSFO=QRUNOFSF
C        QOUTSFO=QOUTSF
C        QINSFO=QINSF
C        SFOAREA=SFNAREA
C      ENDIF
CC
CC--WRITE TO OUTPUT FILE
C      ICNT=0
C      DO N=1,NSTRM
C        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') THEN
C          WRITE(IOUT,51) ISFL(N),ISFR(N),ISFC(N),ISEG(N),
C     1      IREACH(N),SFLEN(N),SFNAREA(N),QSFGW(N),QOUTSF(N),
C     1      QRUNOFSF(N),QPRECSF(N),QETSF(N),NIN(N)
C          DO I=1,NIN(N)
C            ICNT=ICNT+1
C            WRITE(IOUT,52) INSEG(ICNT),INRCH(ICNT),QINSF(ICNT),
C     1        IDSPFLG(ICNT)
C          ENDDO
C        ENDIF
C      ENDDO  
CC
CC--IDENTIFY NODES THAT DO NOT FEED WATER TO DOWNSTREAM NODES
C      ICNT=0
C      DO N=1,NSTRM
C        DO NC=1,NIN(N)
C          ICNT=ICNT+1
C          IS=INSEG(ICNT)
C          IR=INRCH(ICNT)
C          IF(IS.GT.0.AND.IR.GT.0) THEN
CC...........INFLOW FROM STREAM
C            NN=ISTRM(IR,IS)
C            IEXIT(NN)=0
C          ENDIF
C        ENDDO
C      ENDDO
CC
CC--IDENTIFY CONSTANT CONCENTRATION NODES AND SET IBNDSF ARRAY
C      IBNDSF=1
C      DO I=1,NSSSF
C        IS=ISEGBC(I)
C        IR=IRCHBC(I)
C        N=ISTRM(IR,IS)
C        IF(ISFBCTYP(I).EQ.3) IBNDSF(N)=-1
C      ENDDO
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
    5 FORMAT(1X,'ERROR: INVALID NUMBER OF STREAM REACHES',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STREAM REACHES IN FLOW-TRANSPORT LINK FILE =',I5)
   50 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7,
     & ' SS CODE',I5,5X,'EXTERNAL FLOW',G15.7)  
51    FORMAT(1X,5I5,7(1X,G12.5),1X,I5)
52    FORMAT('SEG,RCH,QIN,IDISP: ',1X,2I5,1(1X,G12.5),I5)
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE READLAK(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT,NCOMP)
C *********************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF LAK PAKCAGE
C *********************************************************************
C last modified: 08-10-2012
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
      USE LAKVARS
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,NUM,N,MXSS,NTSS,NSS,ICBUND,IQ,ID,
     &          KKK,III,JJJ,ITEMP,IGROUP,NCOMP
      REAL      BUFF,SS,QSS,QSTEMP,QSW
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),SS(8,MXSS)
C      COMMON /FTL/IFTLFMT
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NLAKES,LKNODE,NSFRLAK
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NLAKES,LKNODE,NSFRLAK
      ENDIF
C
C--DEALLOCATE AND ALLOCATE ARRAYS FOR STORING FLOW TERMS
      CALL MEMDEALLOCATE6()
      ALLOCATE(LAKL(LKNODE),LAKR(LKNODE),LAKC(LKNODE),LAKNUMGW(LKNODE))
      ALLOCATE(LAKRCH(NSFRLAK),LAKSEG(NSFRLAK),LAKNUMSFR(LKNODE))
      ALLOCATE(QPRECLAK(NLAKES),QRUNOFLAK(NLAKES),QWDRLLAK(NLAKES))
      ALLOCATE(QLAKGW(LKNODE),QLAKSFR(NSFRLAK),QETLAK(NLAKES))
      ALLOCATE(VOLNLAK(NLAKES),VOLOLAK(NLAKES))
C
C--CHECK INTERFACE
C      WRITE(*,'(14hReadLAK REQD: ,a10,6hREAD: ,a10)') TEXT,LABEL
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        WRITE(IOUT,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        WRITE(IOUT,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        WRITE(IOUT,2) NC,NR,NL
        CALL USTOP(' ')
      ELSEIF(NLAKES.NE.NLKINIT) THEN
        WRITE(*,5) NLAKES
        WRITE(IOUT,5) NLAKES
        CALL USTOP(' ')
      ENDIF
C
C--RETURN IF NLAKES,LKNODE,NSFRLAK=0
      IF(NLAKES.EQ.0 .AND. LKNODE.EQ.0 .AND. NSFRLAK.EQ.0) RETURN
C
      IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,10) 
10    FORMAT(1X,'LAKE#    VOLUME_OLD     DEL_V        PRECIP ',
     &  '          ET        RUNOFF    WITHDRAWAL',/,
     &  1X,5('-'),6(1X,12('-')))
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH LAKE INFLOWS AND OUTFLOWS
      DO N=1,NLAKES
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) I,VOLOLAK(N),DELVOLLAK(N),QPRECLAK(N),QETLAK(N),
     1    QRUNOFLAK(N),QWDRLLAK(N)
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) I,VOLOLAK(N),DELVOLLAK(N),QPRECLAK(N),QETLAK(N),
     1    QRUNOFLAK(N),QWDRLLAK(N)
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,51) I,VOLOLAK(N),DELVOLLAK(N),QPRECLAK(N),QETLAK(N),
     1    QRUNOFLAK(N),QWDRLLAK(N)
      ENDDO  
C
C
      IF(LKNODE.GT.0) THEN
      IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,12) 
12    FORMAT(/1X,'LAKE# LAYER  ROW COLUMN GW-FLOW',/,
     &  4(1X,5('-')),1(1X,12('-')))
      ENDIF
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH LAKE-GW CONNECTION
      DO N=1,LKNODE
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) LAKNUMGW(N),LAKL(N),LAKR(N),LAKC(N),QLAKGW(N)
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) LAKNUMGW(N),LAKL(N),LAKR(N),LAKC(N),QLAKGW(N)
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,52) LAKNUMGW(N),LAKL(N),LAKR(N),LAKC(N),QLAKGW(N)
      ENDDO  
C
C
      IF(NSFRLAK.GT.0) THEN
      IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,14) 
14    FORMAT(/1X,'LAKE# SEGMENT REACH SFR-FLOW',/,
     &  3(1X,5('-')),1(1X,12('-')))
      ENDIF
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH LAKE-SFR CONNECTION
      DO N=1,NSFRLAK
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) LAKNUMSFR(N),LAKSEG(N),LAKRCH(N),QLAKSFR(N)
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) LAKNUMSFR(N),LAKSEG(N),LAKRCH(N),QLAKSFR(N)
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,53) LAKNUMSFR(N),LAKSEG(N),LAKRCH(N),QLAKSFR(N)
      ENDDO  
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
    5 FORMAT(1X,'ERROR: INVALID NUMBER OF LAKES',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF LAKES IN FLOW-TRANSPORT LINK FILE =',I5)
   50 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7,
     & ' SS CODE',I5,5X,'EXTERNAL FLOW',G15.7)  
51    FORMAT(1X,I5,6(1X,G12.5))
52    FORMAT(1X,4I5,1(1X,G12.5))
53    FORMAT(1X,3I5,1(1X,G12.5))
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE READUZFCONNECT(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &                          NCOMP)
C *********************************************************************
C THIS SUBROUTINE READS UZF->SFR & UZF->LAK CONNECTIONS
C *********************************************************************
C last modified: 08-15-2013
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
C      USE UZTVARS,       ONLY: !UZQ,NCON,NCONLK,NCONSF,MXUZCON,IROUTE,
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,NUM,N,MXSS,NTSS,NSS,ICBUND,IQ,ID,
     &          KK,II,JJ,ITEMP,IGROUP,NCOMP
      INTEGER   ISTSG,NREACH,ILAK,LK
      REAL      Q,LENFRAC
      !CHARACTER LABEL*16,TEXT*16
      CHARACTER LABEL16*16,LABEL*4,TEXT*4,BUFFER*100
C      COMMON /FTL/IFTLFMT
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        !READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NCON
C        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL16,NCON
      ELSEIF(IFTLFMT.EQ.1) THEN
        !READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NCON
C        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL16,NCON
      ENDIF
C      WRITE(*,'(14hReadUZF REQD: ,a10,6hREAD: ,a10)') TEXT,LABEL
C
C--CLEAN AND INITIALIZE TO ZERO
C      DO II=1,MXUZCON
C        DO JJ=1,7
C          IROUTE(JJ,II)=0
C        ENDDO
C        UZQ(II)=0.
C      ENDDO
C      NCONLK=0
C      NCONSF=0
C
C--READ CONNECTIONS INFORMATION
C      DO I=1,NCON
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) LABEL,TEXT
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,'(A100)') BUFFER
          READ(BUFFER,2) LABEL,TEXT
  2       FORMAT(2X,A4,2X,A4)
          !BACKSPACE(INUF)
        ENDIF
      !BACKSPACE (INUF)
C
C--LOOP THROUGH EACH CONNECTION
      !DO I=1,NCON
C
C--IF UZF -> SFR, READ 9 VALUES
        ! IF(LABEL.EQ.'SFR') THEN
        IF(LABEL.EQ.'SFR ') THEN
          IF(IFTLFMT.EQ.0) THEN
            !READ(INUF) LABEL,TEXT,KK,II,JJ,ISTSG,NREACH,Q
            READ(INUF) KK,II,JJ,ISTSG,NREACH,Q
          ELSEIF(IFTLFMT.EQ.1) THEN
            !READ(INUF,*)  LABEL,TEXT,KK,II,JJ,ISTSG,NREACH,Q
C            READ(INUF,2)  KK,II,JJ,ISTSG,NREACH,Q
            READ(BUFFER,3) LABEL,TEXT,KK,II,JJ,ISTSG,NREACH,Q
   3        FORMAT(2X,A4,2X,A4,5I6,F20.10)
          ENDIF
C          IROUTE(1,I)=1  !1:SFR, 2:LAK, 3:SNK
C          IROUTE(2,I)=KK
C          IROUTE(3,I)=II
C          IROUTE(4,I)=JJ
C          IROUTE(5,I)=ISTSG
C          IROUTE(6,I)=NREACH
C          IF(TEXT.EQ.'GRW ') THEN
C            IROUTE(7,I)=1    !1:GRW, 2:EXC, 3:REJ
C          ELSEIF(TEXT.EQ.'EXC ') THEN
C            IROUTE(7,I)=2    !1:GRW, 2:EXC, 3:REJ
C          ELSEIF(TEXT.EQ.'REJ ') THEN
C            IROUTE(7,I)=2    !1:GRW, 2:EXC, 3:REJ
C          ENDIF
C          UZQ(I)=Q
C          NCONSF=NCONSF+1
C
C--IF UZF -> LAK, READ 7 VALUES
        !ELSEIF(LABEL.EQ.'LAK') THEN
        ELSEIF(LABEL.EQ.'LAK ') THEN
          IF(IFTLFMT.EQ.0) THEN
            !READ(INUF) LABEL,TEXT,KK,II,JJ,ILAK,Q
            READ(INUF) KK,II,JJ,ILAK,Q
          ELSEIF(IFTLFMT.EQ.1) THEN
            READ(INUF,*) LABEL,TEXT,KK,II,JJ,ILAK,Q
          ENDIF
C          IROUTE(1,I)=2  !1:SFR, 2:LAK, 3:SNK
C          IROUTE(2,I)=KK
C          IROUTE(3,I)=II
C          IROUTE(4,I)=JJ
C          IROUTE(5,I)=ILAK
C          !IROUTE(6,I) ALREADY EQUALS ZERO
C          IF(TEXT.EQ.'GRW ') THEN
C            IROUTE(7,I)=1    !1:GRW, 2:EXC, 3:REJ
C          ELSEIF(TEXT.EQ.'EXC ') THEN
C            IROUTE(7,I)=2    !1:GRW, 2:EXC, 3:REJ
C          ELSEIF(TEXT.EQ.'REJ ') THEN
C            IROUTE(7,I)=2    !1:GRW, 2:EXC, 3:REJ
C          ENDIF
C          UZQ(I)=Q
C          NCONLK=NCONLK+1
C
C--IF UZF -> SNK, READ 6 VALUES
        ! ELSEIF(LABEL.EQ.'SNK') THEN
        ELSEIF(LABEL.EQ.'SNK ') THEN
          IF(IFTLFMT.EQ.0) THEN
            !READ(INUF) LABEL,TEXT,KK,II,JJ,Q
            READ(INUF) KK,II,JJ
            IF(TEXT.EQ.'GRW ') THEN
              READ(INUF) Q
            ELSEIF(TEXT.EQ.'EXC ') THEN
              READ(INUF) Q
            ELSEIF(TEXT.EQ.'REJ ') THEN
              READ(INUF) LK,Q
            ENDIF
          ELSEIF(IFTLFMT.EQ.1) THEN
            IF(TEXT.EQ.'GRW ') THEN
              READ(INUF,*) LABEL,TEXT,KK,II,JJ,Q
            ELSEIF(TEXT.EQ.'EXC ') THEN
              READ(INUF,*) LABEL,TEXT,KK,II,JJ,Q
            ELSEIF(TEXT.EQ.'REJ ') THEN
              READ(INUF,*) LABEL,TEXT,KK,II,JJ,LK,Q
            ENDIF
          ENDIF
C          IROUTE(1,I)=3  !1:SFR, 2:LAK, 3:SNK
C          IROUTE(2,I)=KK
C          IROUTE(3,I)=II
C          IROUTE(4,I)=JJ
C          !IROUTE(5,I) ALREADY EQUALS ZERO
C          !IROUTE(6,I) ALREADY EQUALS ZERO
C          IF(TEXT.EQ.'GRW ') THEN
C            IROUTE(7,I)=1    !1:GRW, 2:EXC, 3:REJ
C          ELSEIF(TEXT.EQ.'EXC ') THEN
C            IROUTE(7,I)=2    !1:GRW, 2:EXC, 3:REJ
C          ELSEIF(TEXT.EQ.'REJ ') THEN
C            IROUTE(7,I)=2    !1:GRW, 2:EXC, 3:REJ
C          ENDIF
C          UZQ(I)=Q
C          NCONLK=NCONLK+1
        ENDIF
C        
C      ENDDO
C
      WRITE(*,*) I,LABEL,TEXT
      RETURN
      END
C
C
      SUBROUTINE READFLOWS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   ICBUND,FPRT,NCOMP,NNGW,IPSGS,INFL,INFR,INFC,QNGW,ICID,QAUX,
     &   NNINIT,NNODES,NFLOWTYPE,PKGFLOWS,CFLOWTYPE,
     &   NRCHCON,INOD1,INOD2,IDISP,QAREA,QN2N)
C *********************************************************************
C THIS SUBROUTINE READS TWO SECTIONS
C 1. GW EXCHANGE FLOWS
C 2. INTERNAL FLOWS WITHIN A PACKAGE AND INFLOW (HEADWATER) AND 
C    OUTFLOW (EXIT BOUNDARY)
C *********************************************************************
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,IFL,IPSGS
      INTEGER   NUM,N,ICBUND,IQ,ID,
     &          KKK,III,JJJ,ITEMP,NCOMP,ICNT,IS,IR,NN,NNINIT
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION ICBUND(NCOL,NROW,NLAY)
      INTEGER NSFINIT,NNODES,NFLOWTYPE,NRCHCON,NNGW
      INTEGER, DIMENSION(:), POINTER :: INFL,INFR,INFC
      REAL,    DIMENSION(:), POINTER :: QNGW
      REAL,         ALLOCATABLE      :: PKGFLOWS(:,:),QAREA(:),
     1  QN2N(:)
      CHARACTER*20, ALLOCATABLE      :: CFLOWTYPE(:)
      INTEGER,      ALLOCATABLE      :: INOD1(:),INOD2(:),IDISP(:)
      INTEGER,      ALLOCATABLE      :: ICID(:)
      REAL,         ALLOCATABLE      :: QAUX(:)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NNGW
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NNGW
      ENDIF
C
C--CHECK INTERFACE
C      WRITE(*,'(14hReadSFR REQD: ,a10,6hREAD: ,a10)') TEXT,LABEL
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        WRITE(IOUT,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        WRITE(IOUT,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        WRITE(IOUT,2) NC,NR,NL
        CALL USTOP(' ')
      ENDIF
C
C--GROUNDWATER FLOW 
      IF(NNGW.GT.0) THEN
C
C--ALLOCATE ARRAYS
      ALLOCATE(INFL(NNGW),INFR(NNGW),INFC(NNGW),QNGW(NNGW))
C
C--READ GW EXCHANGE TERMS
      IF(IPSGS.EQ.0) THEN
        DO N=1,NNGW
          IF(IFTLFMT.EQ.0) THEN
            READ(INUF) INFL(N),INFR(N),INFC(N),QNGW(N)
          ELSEIF(IFTLFMT.EQ.1) THEN
            READ(INUF,*) INFL(N),INFR(N),INFC(N),QNGW(N)
          ENDIF
          IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &              WRITE(IOUT,50) INFL(N),INFR(N),INFC(N),QNGW(N)
        ENDDO  
      ELSEIF(IPSGS.EQ.1) THEN
        ALLOCATE(ICID(NNGW),QAUX(NNGW))
        DO N=1,NNGW
          IF(IFTLFMT.EQ.0) THEN
            READ(INUF) INFL(N),INFR(N),INFC(N),QNGW(N),ICID(N),QAUX(N)
          ELSEIF(IFTLFMT.EQ.1) THEN
            READ(INUF,*) INFL(N),INFR(N),INFC(N),QNGW(N),ICID(N),QAUX(N)
          ENDIF
          IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &    WRITE(IOUT,55) INFL(N),INFR(N),INFC(N),QNGW(N),ICID(N),QAUX(N)
        ENDDO  
      ELSE
        WRITE(*,*) 'INVALID IPSGS',IPSGS
        WRITE(IOUT,*) 'INVALID IPSGS',IPSGS
        CALL USTOP(' ')
      ENDIF
C
      ENDIF !IF(NNGW.GT.0)
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,LABEL,NNODES,NFLOWTYPE,NRCHCON
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,LABEL,NNODES,NFLOWTYPE,NRCHCON
      ENDIF
C
C--PERFORM CHECKS
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        WRITE(IOUT,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        WRITE(IOUT,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NNODES.NE.NNINIT) THEN
        WRITE(*,5) NNODES
        WRITE(IOUT,5) NNODES
        CALL USTOP(' ')
      ENDIF
C
C--READ THIS BLOCK FOR NODAL FLOWS (BOUNDARY CONDITIONS OR CELL SPECIFIC FLOWS)
      IF(NFLOWTYPE.GT.0) THEN
C
C--ALLOCATE
        ALLOCATE(CFLOWTYPE(NFLOWTYPE),PKGFLOWS(NFLOWTYPE,NNODES))
C
C--READ CFLOWTYPE IDENTIFIER TEXT
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) (CFLOWTYPE(IFL),IFL=1,NFLOWTYPE)
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) (CFLOWTYPE(IFL),IFL=1,NFLOWTYPE)
        ENDIF
C
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &  WRITE(IOUT,'(/A10,50A20)') 
     &  '  NODE    ',(CFLOWTYPE(IFL),IFL=1,NFLOWTYPE)
C
C--READ BOUNDARY AND OTHER FLOW TERMS
        DO N=1,NNODES
          IF(IFTLFMT.EQ.0) THEN
            READ(INUF) (PKGFLOWS(IFL,N),IFL=1,NFLOWTYPE)
          ELSEIF(IFTLFMT.EQ.1) THEN
            READ(INUF,*) (PKGFLOWS(IFL,N),IFL=1,NFLOWTYPE)
          ENDIF
          IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &      WRITE(IOUT,51) N,(PKGFLOWS(IFL,N),IFL=1,NFLOWTYPE)
        ENDDO  
      ENDIF
C
C--READ THIS BLOCK FOR NODE-TO-NODE FLOWS
      IF(NRCHCON.GT.0) THEN
C
C--ALLOCATE
        ALLOCATE(INOD1(NRCHCON),INOD2(NRCHCON),IDISP(NRCHCON),
     1  QAREA(NRCHCON),QN2N(NRCHCON))
C
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &  WRITE(IOUT,'(/3A5,A15,A15)') 
     &  ' NOD1',' NOD2',' IDIV','      FLOW RATE','      AREA'
C
C--READ NODE-TO-NODE INFO
        DO N=1,NRCHCON
          IF(IFTLFMT.EQ.0) THEN
            READ(INUF) INOD1(N),INOD2(N),IDISP(N),
     1                 QN2N(N),QAREA(N)
          ELSEIF(IFTLFMT.EQ.1) THEN
            READ(INUF,*) INOD1(N),INOD2(N),IDISP(N),
     1                 QN2N(N),QAREA(N)
          ENDIF
          IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &      WRITE(IOUT,52) INOD1(N),INOD2(N),IDISP(N),
     1                 QN2N(N),QAREA(N)
        ENDDO  
      ENDIF
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
    5 FORMAT(1X,'ERROR: INVALID NUMBER OF NODES',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF NODES IN FLOW-TRANSPORT LINK FILE =',I5)
   50 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7)
   55 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7,
     & ' CELL ID',I5,5X,'AUXILIARY FLOW',G15.7)  
   51 FORMAT(I10,100(1X,G19.7))
   52 FORMAT(3I5,1X,G14.7,1X,G14.7)
C
      RETURN
      END
C
C
      SUBROUTINE PKG2PKGFLOW(INUF,IOUT,KSTP,KPER,TEXT,FPRT,
     1  NPKG2PKG,INOD1,INOD2,QN1N2,IP2PFLG)
C *********************************************************************
C THIS SUBROUTINE READS FLOWS BETWEEN TWO DIFFERENT PACKAGES: PKG1 PKG2
C FLOW IS POSITIVE IF FLOW IS FROM PKG1 TO PKG2
C *********************************************************************
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,IOUT,KKSTP,KKPER,NPKG2PKG
      INTEGER   NUM,N,ICBUND,IQ,ID,ITEMP,NCOMP
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      INTEGER, DIMENSION(:), POINTER :: INOD1,INOD2
      REAL,    DIMENSION(:), POINTER :: QN1N2
      INTEGER,      ALLOCATABLE      :: IP2PFLG(:)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,LABEL,NPKG2PKG
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,LABEL,NPKG2PKG
      ENDIF
C
C--CHECK INTERFACE
C      WRITE(*,'(14hReadSFR REQD: ,a10,6hREAD: ,a10)') TEXT,LABEL
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        WRITE(IOUT,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        WRITE(IOUT,3) KKPER,KKSTP
        CALL USTOP(' ')
      ENDIF
C
C--RETURN
      IF(NPKG2PKG.EQ.0) RETURN
C
C--ALLOCATE
      ALLOCATE(INOD1(NPKG2PKG),INOD2(NPKG2PKG),QN1N2(NPKG2PKG),
     &  IP2PFLG(NPKG2PKG))
C
C--READ FLOW RATES
      DO N=1,NPKG2PKG
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) INOD1(N),INOD2(N),QN1N2(N),IP2PFLG(N)
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) INOD1(N),INOD2(N),QN1N2(N),IP2PFLG(N)
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &    WRITE(IOUT,50) INOD1(N),INOD2(N),QN1N2(N),IP2PFLG(N)
      ENDDO
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
    5 FORMAT(1X,'ERROR: INVALID NUMBER OF NODES',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF NODES IN FLOW-TRANSPORT LINK FILE =',I5)
   50 FORMAT(1X,'NODE1',I5,5X,'NODE1',I5,5X,'RATE',G15.7,5X,'FLAG',I4)
C
      RETURN
      END
C
C
      SUBROUTINE GETFLOWDIR(INOD1,INOD2,QN2N,NDIM,N,IFROM,ITO)
C
C DETERMINE FLOW DIRECTION
C
      INTEGER INOD1(NDIM),INOD2(NDIM)
      INTEGER IFROM,ITO
      REAL QN2N(NDIM)
C
      IF(QN2N(N).GT.0.) THEN
        IFROM=INOD1(N)
        ITO=INOD2(N)
      ELSEIF(QN2N(N).LT.0.) THEN
        IFROM=INOD2(N)
        ITO=INOD1(N)
      ELSE
        IFROM=INOD1(N)
        ITO=INOD2(N)
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE DEALOCTEMPARR(PKGFLOWS,QAREA,QN2N,CFLOWTYPE,INOD1,
     &  INOD2,IDISP,ICID,QAUX,IP2PFLG)
C
      REAL,         ALLOCATABLE      :: PKGFLOWS(:,:),QAREA(:),
     1  QN2N(:)
      CHARACTER*20, ALLOCATABLE      :: CFLOWTYPE(:)
      INTEGER,      ALLOCATABLE      :: INOD1(:),INOD2(:),IDISP(:)
      INTEGER,      ALLOCATABLE      :: ICID(:)
      REAL,         ALLOCATABLE      :: QAUX(:)
      INTEGER,      ALLOCATABLE      :: IP2PFLG(:)
C
       IF(ALLOCATED(CFLOWTYPE))     DEALLOCATE(CFLOWTYPE)
       IF(ALLOCATED(PKGFLOWS))      DEALLOCATE(PKGFLOWS)
       IF(ALLOCATED(QAREA))         DEALLOCATE(QAREA)
       IF(ALLOCATED(QN2N))          DEALLOCATE(QN2N)
       IF(ALLOCATED(INOD1))         DEALLOCATE(INOD1)
       IF(ALLOCATED(INOD2))         DEALLOCATE(INOD2)
       IF(ALLOCATED(IDISP))         DEALLOCATE(IDISP)
       IF(ALLOCATED(ICID))          DEALLOCATE(ICID)
       IF(ALLOCATED(QAUX))          DEALLOCATE(QAUX)
       IF(ALLOCATED(IP2PFLG))       DEALLOCATE(IP2PFLG)
C
      RETURN
      END
