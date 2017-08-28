C
      SUBROUTINE RCT1AR(IN)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED BY THE CHEMICAL 
C REACTION (RCT) PACKAGE.
C **********************************************************************
C
      USE MT3DMS_MODULE, ONLY: INRCT,IOUT,NCOL,NROW,NLAY,NCOMP,ICBUND,
     &                         COLD,PRSITY,BUFF,SRCONC,RETA,RHOB,
     &                         PRSITY2,RETA2,ISOTHM,RFMIN,DTRCT,
     &                         IREACT,IRCTOP,IGETSC,IFMTRF,FRAC,SP1,SP2,
     &                         RC1,RC2,
     &                         SAVUCN,IUCN2
      USE MIN_SAT, ONLY: ICIMDRY
C
      USE RCTMOD                         
C
      IMPLICIT  NONE
      INTEGER   IN,J,I,K,JR,IR,KR,INDEX,IERR
      INTEGER   NODES         
      REAL      TR,TINY,EPSILON,TOTPOR
      CHARACTER ANAME*24,FLNAME*50,FINDEX*30
      PARAMETER (TINY=1.E-30,EPSILON=0.5E-6)
      LOGICAL   EXISTED      
C
      INRCT=IN
C
C--ALLOCATE AND INITIALIZE
      ALLOCATE(IREACT,IRCTOP,IGETSC,IFESLD,ISORBIMONLY,ISP1IM,
     &         rec_FileName,NSPECIAL,NEA,NED,NSTORE,Ad_methane_name,
     &         IUMETH,RVAL,NSOLID)
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
 1000 FORMAT(/1X,'RCT1 -- CHEMICAL REACTION PACKAGE,',
     &           ' VERSION 1, MAY 2016, INPUT READ FROM UNIT',I3)
C
C--READ AND ECHO SORPTION ISOTHERM TYPE AND FLAG IREACT
      READ(INRCT,'(5I10)',ERR=100,IOSTAT=IERR)
     &     ISOTHM,IREACT,IRCTOP,IGETSC,IREACTION 
  100 IF(IERR.NE.0) THEN
        IRCTOP=1
        IGETSC=0
        BACKSPACE (INRCT)
        READ(INRCT,'(2I10)') ISOTHM,IREACT
      ENDIF
C
      ISORBIMONLY=.FALSE.                    
      ISP1IM=0                               
      IF(ISOTHM.EQ.-6) THEN                  
        ISORBIMONLY=.TRUE.                   
        ISP1IM=1                             
        ISOTHM=6                             
      ENDIF                                  
C                                            
      IF(IREACT.EQ.90.OR.IREACT.EQ.91) THEN  
        IREACTION=1                          
        IREACT=IREACT-90                     
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
        IF(ISP1IM.GE.1) WRITE(IOUT,1042)
      ELSE
        WRITE(IOUT,1028)
      ENDIF
      IF(IREACT.EQ.0) THEN
        WRITE(IOUT,1030)
      ELSEIF(ireact.eq.1) THEN
        WRITE(IOUT,1032)
      ELSEIF(ireact.eq.2) THEN
        WRITE(IOUT,1033)      
      ELSEIF(ireact.eq.3) THEN
        WRITE(IOUT,1035)      
      ELSEIF(ireact.eq.100) THEN
        WRITE(IOUT,1034)
      ENDIF
C                                                               
C-----DUAL-DOMAIN NOT ALLOWED WITH MONOD KINETIC DISSOLVED      
      IF(IREACT.EQ.2.AND.(ISOTHM.EQ.5.OR.ISOTHM.EQ.6)) THEN     
        WRITE(IOUT,*) 'IREACT=2 NOT ALLOWED WITH DUAL-DOMAIN'   
        WRITE(*,*)    'IREACT=2 NOT ALLOWED WITH DUAL-DOMAIN'      
        READ(*,*)                                               
        STOP                                                    
      ENDIF                                                     
C                                                               
C-----DUAL-DOMAIN NOT ALLOWED WITH MONOD KINETIC DISSOLVED      
      IF(IREACT.EQ.3.AND.(ISOTHM.EQ.5.OR.ISOTHM.EQ.6)) THEN     
        WRITE(IOUT,*) 'IREACT=3 NOT ALLOWED WITH DUAL-DOMAIN'   
        WRITE(*,*)    'IREACT=3 NOT ALLOWED WITH DUAL-DOMAIN'      
        READ(*,*)                                               
        STOP                                                    
      ENDIF                                                     
C                                                               
C-----NEED MULTIPLE SPECIED WITH IREACT=3                       
      IF(IREACT.EQ.3.AND.NCOMP.LT.2) THEN                       
        WRITE(IOUT,*) 'IREACT=3 NOT ALLOWED WITH SINGLE SPECIES'
        WRITE(*,*)    'IREACT=3 NOT ALLOWED WITH SINGLE SPECIES'   
        READ(*,*)                                               
        STOP                                                    
      ENDIF                                                     
C                                                               
      IF(IREACTION.EQ.1) THEN                                   
        WRITE(IOUT,1036)                                        
      ELSEIF(IREACTION.EQ.2) THEN                               
        WRITE(IOUT,1038)                                        
      ELSEIF(IREACTION.EQ.0) THEN                               
      ELSE                                                      
        WRITE(IOUT,*) 'IREACTION MUST BE 0, 1, OR 2'            
        WRITE(*,*)    'IREACTION MUST BE 0, 1, OR 2'               
        READ(*,*)                                               
        STOP                                                    
      ENDIF                                                     
C
 1022 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [LINEAR]')
 1024 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [FREUNDLICH]')
 1026 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [LANGMUIR]')
 1027 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [NON-EQUILIBRIUM]')
 2027 FORMAT(1X,'DUAL DOMAIN MASS TRANSFER IS SIMULATED')
 3027 FORMAT(1X,'DUAL DOMAIN MASS TRANSFER WITH SORPTION IS SIMULATED')
 1028 FORMAT(1X,'NO SORPTION [OR DUAL-DOMAIN MODEL] IS SIMULATED')
 1030 FORMAT(1X,'NO FIRST-ORDER RATE REACTION IS SIMULATED')
 1032 FORMAT(1X,'FIRST-ORDER IRREVERSIBLE REACTION',
     &          ' [RADIOACTIVE DECAY OR BIODEGRADATION] IS SIMULATED')
 1033 FORMAT(1X,'MONOD KINETIC REACTION IS SIMULATED')
 1035 FORMAT(1X,'FIRST-ORDER CHAIN REACTION IS SIMULATED')
 1034 FORMAT(1X,'ZEROTH-ORDER DECAY OR PRODUCTION IS SIMULATED')
C
 1036 FORMAT(1X,'REACTION BETWEEN 1 EA AND 1 ED IS SIMULATED')
 1038 FORMAT(1X,'MULTI-SPECIES KINETIC REACTION MODULE IS SIMULATED')
 1042 FORMAT(1X,'SEPARATE Kd FOR TWO DOMAINS')
      IF(IRCTOP.LE.1) THEN
        IRCTOP=1
        WRITE(*,1050)
      ELSEIF(IRCTOP.GE.2) THEN
        IRCTOP=2
        WRITE(IOUT,1052)
      ENDIF
 1050 FORMAT(/1X,'WARNING: INPUT FILE FOR VER 1 OF [RCT] PACKAGE',
     &           ' DETECTED;'/1X,'REACTION COEFFICIENTS ASSIGNED',
     &           ' ONE VALUE PER LAYER'/)
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
     &          ' ASSIGNED BY DEFAULT')
 1061 FORMAT(1X,'ERROR: INITIAL SORBED CONCENTRATION FOR',
     &          ' EQUILIBRIUM-CONTROLLED SORPTION CANNOT BE SPECIFIED;',
     &      /1X,'INPUT VALUE FOR [IGETSC] MUST BE SET TO ZERO')
 1062 FORMAT(1X,'INITIAL SORBED/IMMOBILE PHASE CONCENTRATION',
     &          ' READ FROM INPUT FILE')
C
C--ALLOCATE IF IREACTION=1                          
      IF(IREACTION.EQ.1) THEN                       
        ALLOCATE(IED,IEA,FEDEA)
        ALLOCATE (CRCT(NCOL,NROW,NLAY,2),STAT=IERR) 
        IF(IERR.NE.0) THEN                          
          WRITE(*,106)                              
  106     FORMAT(1X,'RCT: MEMORY ALLOCATION ERROR') 
          CALL USTOP(' ')                           
        ENDIF                                       
      ENDIF                                         
C                                                   
C--ALLOCATE RC3 IS IREACT=2                         
      IF(IREACT.EQ.2) THEN                          
        ALLOCATE(RC3(NCOL,NROW,NLAY,NCOMP),STAT=IERR)
        IF(IERR.NE.0) THEN                          
          WRITE(*,107)                              
  107     FORMAT(1X,'RC3: MEMORY ALLOCATION ERROR') 
          CALL USTOP(' ')                           
        ENDIF                                       
      ENDIF                                         
C                                                   
C--ALLOCATE YLD IS IREACT=3                         
      IF(IREACT.EQ.3) THEN                          
        ALLOCATE(YLD(NCOMP-1),STAT=IERR)            
        IF(IERR.NE.0) THEN                          
          WRITE(*,108)                              
  108     FORMAT(1X,'YLD: MEMORY ALLOCATION ERROR') 
          CALL USTOP(' ')                           
        ENDIF                                       
      ENDIF                                         
C                                                   
C--ALLOCATE SP1IM                                   
      IF(ISP1IM.GE.1) THEN                             
        ALLOCATE(SP1IM(NCOL,NROW,NLAY,NCOMP),STAT=IERR)
        IF(IERR.NE.0) THEN                             
          WRITE(*,109)                                 
  109     FORMAT(1X,'SP1IM MEMORY ALLOCATION ERROR')   
          CALL USTOP(' ')                              
        ENDIF                                          
      ENDIF                                            
C                                                      
      IF(ISOTHM.NE.5.AND.ISOTHM.NE.6) THEN
        IF(ICIMDRY.GE.2) THEN
          ICIMDRY=0
          WRITE(IOUT,*) ' ICIMDRY ONLY ACTIVE WITH DUAL-DOMAIN'
          WRITE(IOUT,*) ' ICIMDRY RESET TO 0'
        ENDIF
      ENDIF
C
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
     &          'IN UNFORMATTED FILES [MT3DnnnS.UCN]'/1X,' FOR EACH ',
     &          'SPECIES ON UNITS ',I3,' AND ABOVE, ',
     &          'IF SORPTION/MASS TRANSFER SIMULATED')
C
C--PRINT A HEADER
      WRITE(IOUT,2001)
 2001 FORMAT(//1X,'SORPTION AND 1ST/0TH ORDER REACTION PARAMETERS',
     &        /1X,46('-')/)
C
C--CALL RARRAY TO READ IN SORPTION PARAMETERS IF SORPTION SIMULATED
      IF(ISOTHM.LE.0 .AND. IREACTION.NE.2) GOTO 2000 
C
      IF(ISOTHM.EQ.5 .AND. IREACTION.NE.2) GOTO 111
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
      IF(ISOTHM.LE.0 .AND. IREACTION.EQ.2) GOTO 2000
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
C
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
C-----IMMOBILE DOMAIN SORPTION COEFFICIENT                             
      IF(ISP1IM.GE.1) THEN                                             
        DO INDEX=1,NCOMP                                               
          ANAME='IMM SORP. COEF. COMP. NO'                             
          WRITE(ANAME(22:24),'(I3.2)') INDEX                           
          IF(IRCTOP.EQ.2) THEN                                         
            DO K=1,NLAY                                                
              CALL RARRAY(SP1IM(:,:,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT)
            ENDDO                                                      
          ELSEIF(IRCTOP.EQ.1) THEN                                     
            CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)                   
            DO K=1,NLAY                                                
              DO I=1,NROW                                              
                DO J=1,NCOL                                            
                  SP1IM(J,I,K,INDEX)=BUFF(1,1,K)                       
                ENDDO                                                  
              ENDDO                                                    
            ENDDO                                                      
          ENDIF                                                        
        ENDDO                                                          
      ENDIF                                                            
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
                IF(ISP1IM.GE.1) SP1IM(J,I,K,INDEX)=0.0
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
      IF(IREACT.ne.1.and.IREACT.ne.100.and.IREACT.ne.2.and.
     &   IREACT.ne.3) GOTO 3000                            
C
      DO INDEX=1,NCOMP
        ANAME='SOLUTE RXN RATE: COMP NO'
        IF(IREACT.EQ.2) ANAME='Mt * Umax      : COMP NO' 
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
      IF(IREACT.EQ.2) THEN                   
        DO INDEX=1,NCOMP                     
          ANAME='HALF-SAT. CONST: COMP NO'   
          WRITE(ANAME(22:24),'(I3.2)') INDEX 
          IF(IRCTOP.EQ.2) THEN               
            DO K=1,NLAY                      
              CALL RARRAY(RC3(:,:,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT)
            ENDDO                                   
          ELSEIF(IRCTOP.EQ.1) THEN                  
            CALL RARRAY(BUFF,ANAME,1,NLAY,0,IN,IOUT)
            DO K=1,NLAY                             
              DO I=1,NROW                           
                DO J=1,NCOL                         
                  RC3(J,I,K,INDEX)=BUFF(1,1,K)      
                ENDDO                               
              ENDDO                                 
            ENDDO                                   
          ENDIF                                     
        ENDDO                                       
      ENDIF                                         
C                                                   
      IF(IREACT.EQ.3) THEN                          
        DO INDEX=1,NCOMP-1                          
          READ(IN,*) YLD(INDEX)                     
          WRITE(IOUT,'(18X,2(A,I3),A,G13.6)')       
     &               'YIELD COEFFICIENT BETWEEN SPECIES ',INDEX,
     &               ' AND ',INDEX+1,' = ', YLD(INDEX)          
        ENDDO                                          
      ENDIF                                            
C                                                      
 3000 CONTINUE
C
C-----READ REACTION RELATED DATA             
      IF(IREACTION.EQ.1) THEN                
        READ(IN,'(2I10,F10.0)') IED,IEA,FEDEA
        WRITE(IOUT,104) IED,IEA,FEDEA        
104     FORMAT(1X,'SIMULATED REACTION: ED + FEDEA*EA --> PRODUCT'
     &        /1X,'ELECTRON DONOR COMPONENT (IED)         = ',I3
     &        /1X,'ELECTRON ACCEPTOR COMPONENT (IEA)      = ',I3
     &        /1X,'STOICHIOMETRIC RATIO (FEDEA)           = ',G12.4)
C.......CHECK FOR POSSIBLE ERRORS                              
        IF(IED.GT.NCOMP .OR. IEA.GT.NCOMP) THEN                
          WRITE(*,*) 'IEA OR IED GREATER THAN NCOMP'           
          WRITE(IOUT,*) 'IEA OR IED GREATER THAN NCOMP'        
          CALL USTOP(' ')                                      
        ENDIF                                                  
        IF(IED.EQ.IEA) THEN                                    
          WRITE(*,*) 'IEA AND IED CANNOT BE THE SAME NUMBER'   
          WRITE(IOUT,*) 'IEA AND IED CANNOT BE THE SAME NUMBER'
          CALL USTOP(' ')                                      
        ENDIF                                                  
      ELSEIF(IREACTION.EQ.2) THEN                              
C-------READ REACTION FILE NAME                                
        rec_FileName=''                                        
        READ(IN,'(A1000)') rec_FileName                        
        INQUIRE(FILE=rec_FileName,EXIST=EXISTED)               
        IF(.NOT.EXISTED) THEN                                  
          WRITE(*,*) 'FILE ',TRIM(rec_FileName),' DOES NOT EXIST'
          CALL USTOP('  ')        
        ELSE                      
          NODES=NLAY*NROW*NCOL    
          CALL REACTION_PRE(NODES)
        ENDIF                     
      ENDIF                       
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
C--CALL [SRCT1R] TO CALCULATE RETARDATION FACTORS FOR BOTH DOMAINS
C--AND SORBED CONCENTRATION (SINGLE-DOMAIN MODEL)
C--OR IMMOBILE-LIQUID PHASE CONCENTRATION (DUAL-DOMAIN MODEL)
      IF(ISOTHM.GT.0) THEN
        RFMIN=1.E30
        TR=0.
        DO INDEX=1,NCOMP
          CALL SRCT1R(NCOL,NROW,NLAY,ICBUND(:,:,:,INDEX),PRSITY,
     &               COLD(:,:,:,INDEX),RETA(:,:,:,INDEX),RFMIN,RHOB,
     &               SP1(:,:,:,INDEX),SP2(:,:,:,INDEX),RC1(:,:,:,INDEX),
     &               RC2(:,:,:,INDEX),PRSITY2,RETA2(:,:,:,INDEX),FRAC,
     &               SRCONC(:,:,:,INDEX),ISOTHM,IREACT,TR,INDEX)
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
     &           ' OF THE REACTION TERM'/1X,'=',G11.4,
     &           ' AT K=',I4,', I=',I4,', J=',I4)
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
      SUBROUTINE SRCT1R(NCOL,NROW,NLAY,ICBUND,PRSITY,CNEW,RETA,RFMIN,
     & RHOB,SP1,SP2,RC1,RC2,PRSITY2,RETA2,FRAC,SRCONC,
     & ISOTHM,IREACT,DTRANS,ICOMP)
C ********************************************************************
C THIS SUBROUTINE CALCULATES RETARDATION FACTOR AND CONCENTRATION
C OF SORBED (UNIT: MASS/MASS) FOR SINGLE-DOMAIN MODEL OR
C IMMOBILE-LIQUID PHASE (UNIT: MASS/VOLUME) FOR DUAL-DOMAIN MODEL.
C
C ***NOTE: THIS ROUTINE IS CALLED FROM A NUMBER OF FUNCTIONS, TWO
C          OF WHICH ARE RCT1CF1 AND RCT1CF2.  WHEN CALLED FROM THE
c          LATTER, 'PRISTY' ACTS AS A SURROGATE FOR 'THETAW'.
C ********************************************************************
C
      USE RCTMOD
      USE UZTVARS,       ONLY: THETAW
      USE MT3DMS_MODULE, ONLY: iUnitTRNOP,IALTFM,THETAW2,iSSTrans
C                     
      IMPLICIT     NONE
      INTEGER      NCOL,NROW,NLAY,ICBUND,ISOTHM,IREACT,J,I,K,
     &             ICOMP
      REAL         PRSITY,CNEW,RETA,RFMIN,RHOB,SP1,SP2,RC1,RC2,
     &             PRSITY2,FRAC,SRCONC,DTRANS,TINY,
     &             RETA2,TERM1,RC1TMP,RC2TMP
      DIMENSION    PRSITY(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),
     &             CNEW(NCOL,NROW,NLAY),RETA(NCOL,NROW,NLAY),
     &             RHOB(NCOL,NROW,NLAY),SRCONC(NCOL,NROW,NLAY),
     &             SP1(NCOL,NROW,NLAY),SP2(NCOL,NROW,NLAY),
     &             RC1(NCOL,NROW,NLAY),RC2(NCOL,NROW,NLAY),
     &             PRSITY2(NCOL,NROW,NLAY),FRAC(NCOL,NROW,NLAY),
     &             RETA2(NCOL,NROW,NLAY)
      PARAMETER    (TINY=1.E-30)
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
              IF(iUnitTRNOP(7).GT.0) THEN
                IF(THETAW(J,I,K).NE.PRSITY(J,I,K)) THEN
                  RETA(J,I,K)=1.+RHOB(J,I,K)/THETAW(J,I,K)*SP1(J,I,K)
                ELSE
                  RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*SP1(J,I,K)
                ENDIF
              ELSE
                RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*SP1(J,I,K)
                IF(IALTFM.EQ.3)
     1            RETA(J,I,K)=1.+RHOB(J,I,K)/THETAW2(J,I,K)*SP1(J,I,K)
              ENDIF            
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
     &                      SP1(J,I,K)*SP2(J,I,K)*
     &                      CNEW(J,I,K)**(SP2(J,I,K)-1.)
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
     &                      SP1(J,I,K)*SP2(J,I,K)/(1.+SP1(J,I,K)*
     &                      CNEW(J,I,K))**2
                SRCONC(J,I,K)=SP1(J,I,K)*SP2(J,I,K)*CNEW(J,I,K)/
     &                        (1.+SP1(J,I,K)*CNEW(J,I,K))
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
              IF(IREACT.eq.1.or.IREACT.eq.100.or.IREACT.eq.2.or.
     1          IREACT.eq.3)                                    
     1          RC2TMP=RC2(J,I,K)                                
C--IF with no reaction or with first-order reaction               
              IF(ireact.eq.0.or.ireact.eq.1.or.ireact.eq.2.or.
     1           ireact.eq.3) THEN                            
                SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K)+RHOB(J,I,K)/
     &                        DTRANS*SRCONC(J,I,K))/(RHOB(J,I,K)/
     &                        DTRANS+SP2(J,I,K)/SP1(J,I,K)+
     &                        RC2TMP*RHOB(J,I,K))
C--IF with zeroth-order reaction      
              ELSEIF(ireact.eq.100) THEN 
                SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K)+RHOB(J,I,K)/
     &                        DTRANS*SRCONC(J,I,K)-RC2TMP*RHOB(J,I,K))/
     &                        (RHOB(J,I,K)/DTRANS+SP2(J,I,K)/SP1(J,I,K))
              END IF
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
                RETA(J,I,K)=1.+FRAC(J,I,K)*RHOB(J,I,K)*
     &                      SP1(J,I,K)/PRSITY(J,I,K)
                RFMIN=MIN(RFMIN,RETA(J,I,K))
                RETA2(J,I,K)=1.0
              IF(PRSITY2(J,I,K).GT.TINY) THEN
                IF(ISP1IM.EQ.0) THEN         
                  RETA2(J,I,K)=1.+(1.-FRAC(J,I,K))*RHOB(J,I,K)*
     &                         SP1(J,I,K)/PRSITY2(J,I,K)
                ELSE                              
                  RETA2(J,I,K)=1.+(1.-FRAC(J,I,K))*RHOB(J,I,K)*
     &                         SP1IM(J,I,K,ICOMP)/PRSITY2(J,I,K)
                ENDIF                                             
              ENDIF                                               
              IF(DTRANS.LT.TINY) CYCLE
              RC1TMP=0.
              RC2TMP=0.
              IF(IREACT.eq.1.or.IREACT.eq.100) THEN
                RC1TMP=RC1(J,I,K)
                RC2TMP=RC2(J,I,K)
              ENDIF    
C--IF with no reaction or with first-order reaction            
              IF(ireact.eq.0.or.ireact.eq.1) THEN
                IF(iSSTrans.EQ.1) THEN
                  TERM1=SP2(J,I,K)+
     &                RC1TMP*PRSITY2(J,I,K)+RC2TMP*PRSITY2(J,I,K)*
     &                (RETA2(J,I,K)-1.)
                  SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K))/TERM1
                ELSE
                  TERM1=PRSITY2(J,I,K)*RETA2(J,I,K)/DTRANS+SP2(J,I,K)+
     &                RC1TMP*PRSITY2(J,I,K)+RC2TMP*PRSITY2(J,I,K)*
     &                (RETA2(J,I,K)-1.)
                  SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K)+PRSITY2(J,I,K)*
     &                RETA2(J,I,K)/DTRANS*SRCONC(J,I,K))/TERM1
                ENDIF
C--IF with zeroth-order reaction     
              ELSEIF(ireact.eq.100) THEN 
                IF(iSSTrans.EQ.1) THEN
                  TERM1=SP2(J,I,K)
                      SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K)-
     &                RC1TMP*PRSITY2(J,I,K)-RC2TMP*(1.-FRAC(J,I,K))*
     &                RHOB(J,I,K))/TERM1
                ELSE
                  TERM1=PRSITY2(J,I,K)*RETA2(J,I,K)/DTRANS+SP2(J,I,K)
                      SRCONC(J,I,K)=(SP2(J,I,K)*CNEW(J,I,K)-
     &                RC1TMP*PRSITY2(J,I,K)-RC2TMP*(1.-FRAC(J,I,K))*
     &                RHOB(J,I,K)+PRSITY2(J,I,K)*RETA2(J,I,K)/DTRANS*
     &                SRCONC(J,I,K))/TERM1
                ENDIF
              END IF         
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
      SUBROUTINE RCT1FM(ICOMP,ICBUND,PRSITY,DH,RHOB,SP1,SP2,SRCONC,
     &                  RC1,RC2,PRSITY2,RETA2,FRAC,DTRANS,
     &                  COLD,CNEW)
C *******************************************************************
C THIS SUBROUTINE FORMULATES THE COEFFICIENT MATRIX [A] AND THE 
C RIGHT-HAND-SIDE MATRIX [RHS] FOR SORPTION AND 1ST/0TH ORDER 
C REACTION TERMS USING THE IMPLICIT FINITE-DIFFERENCE SCHEME.
C *******************************************************************
C
      USE RCTMOD
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,DELR,DELC,NODES,
     &                         UPDLHS,ISOTHM,IREACT,A,RHS,MCOMP,iSSTrans
C
      IMPLICIT  NONE
      INTEGER   ICOMP,ICBUND,K,I,J,N,III
      REAL      PRSITY,RHOB,SP1,SP2,RC1,RC2,PRSITY2,FRAC,DTRANS,
     &          SRCONC,DH,RETA2,TERM1,TINY,
     &          RC1TMP,RC2TMP,
     &          COLD,CNEW
      DIMENSION ICBUND(NODES,NCOMP),PRSITY(NODES),
     &          RHOB(NODES),SP1(NODES,NCOMP),SP2(NODES,NCOMP),
     &          RC1(NODES,NCOMP),RC2(NODES,NCOMP),SRCONC(NODES,NCOMP),
     &          DH(NODES),PRSITY2(NODES),RETA2(NODES,NCOMP),FRAC(NODES),
     &          CNEW(NODES,NCOMP),COLD(NODES,NCOMP)
      PARAMETER (TINY=1.E-30)
      INTEGER   MM
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
              IF(IREACT.eq.1.or.IREACT.eq.100.or.IREACT.eq.2.or.
     &           IREACT.eq.3)                                   
     &           RC2TMP=RC2(N,ICOMP)                            
C--IF with no reaction or with first-order reaction             
              IF(IREACT.EQ.0.OR.IREACT.EQ.1.OR.IREACT.EQ.2.OR.  
     &           IREACT.EQ.3) THEN                              
                IF(UPDLHS) A(N)=A(N)-SP2(N,ICOMP)*DELR(J)*DELC(I)*DH(N)*
     &                          (1.-SP2(N,ICOMP)/SP1(N,ICOMP)/
     &                         (RHOB(N)/DTRANS+SP2(N,ICOMP)/SP1(N,ICOMP)
     &                         +RC2TMP*RHOB(N)))
                RHS(N)=RHS(N)-SP2(N,ICOMP)/SP1(N,ICOMP)*DELR(J)*DELC(I)*
     &                 DH(N)*RHOB(N)*SRCONC(N,ICOMP)/DTRANS/
     &                 (RHOB(N)/DTRANS+SP2(N,ICOMP)/SP1(N,ICOMP)+
     &                 RC2TMP*RHOB(N))
                IF(ireact.eq.3.AND.ICOMP.GT.1)           
     &            RHS(N)=RHS(N)-YLD(ICOMP-1)*RC2(N,ICOMP-1)*
     &                   DELR(J)*DELC(I)*DH(N)*RHOB(N)*SRCONC(N,ICOMP-1)
C--IF with zeroth-order reaction     
              ELSEIF(IREACT.EQ.100) THEN
                IF(UPDLHS) A(N)=A(N)-SP2(N,ICOMP)*DELR(J)*DELC(I)*
     &                          DH(N)*(1.-SP2(N,ICOMP)/SP1(N,ICOMP)
     &                          /(RHOB(N)/DTRANS+SP2(N,ICOMP)/
     &                          SP1(N,ICOMP)))
                RHS(N)=RHS(N)+(SP2(N,ICOMP)/SP1(N,ICOMP)*
     &                 DELR(J)*DELC(I)*DH(N)*RHOB(N)*
     &                 (RC2TMP-SRCONC(N,ICOMP)/DTRANS))/
     &                 (RHOB(N)/DTRANS+SP2(N,ICOMP)/SP1(N,ICOMP))
              END IF   
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
C--IF with no reaction or with first-order reaction
              IF(ireact.eq.0.or.ireact.eq.1) THEN
                IF(iSSTrans.EQ.1) THEN
                  TERM1=SP2(N,ICOMP)+
     &                RC1TMP*PRSITY2(N)+
     &                RC2TMP*PRSITY2(N)*(RETA2(N,ICOMP)-1.)
                ELSE
                  TERM1=PRSITY2(N)*RETA2(N,ICOMP)/DTRANS+SP2(N,ICOMP)+
     &                RC1TMP*PRSITY2(N)+
     &                RC2TMP*PRSITY2(N)*(RETA2(N,ICOMP)-1.)
                ENDIF
                IF(UPDLHS) A(N)=A(N)-SP2(N,ICOMP)*DELR(J)*DELC(I)*DH(N)*
     &                          (1.-SP2(N,ICOMP)/TERM1)
                IF(iSSTrans.EQ.0) THEN
                  RHS(N)=RHS(N)-SP2(N,ICOMP)*PRSITY2(N)*RETA2(N,ICOMP)*
     &                 DELR(J)*DELC(I)*DH(N)*SRCONC(N,ICOMP)/
     &                 (DTRANS*TERM1)
                ENDIF
C--IF with zeroth-order reaction      
              ELSEIF(ireact.eq.100) THEN
                IF(iSSTrans.EQ.1) THEN
                  TERM1=SP2(N,ICOMP)
                ELSE
                  TERM1=PRSITY2(N)*RETA2(N,ICOMP)/DTRANS+SP2(N,ICOMP)
                ENDIF
                IF(UPDLHS) A(N)=A(N)-SP2(N,ICOMP)*DELR(J)*DELC(I)*
     &                          DH(N)*(1.-SP2(N,ICOMP)/TERM1)
                IF(iSSTrans.EQ.0) THEN
                  RHS(N)=RHS(N)-SP2(N,ICOMP)*DELR(J)*DELC(I)*DH(N)*
     &                 (-RC1TMP*PRSITY2(N)-RC2TMP*(1.-FRAC(N))*RHOB(N)+
     &                 PRSITY2(N)*RETA2(N,ICOMP)*SRCONC(N,ICOMP)/DTRANS)
     &                 /TERM1             
                ENDIF
              END IF
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
              IF(UPDLHS) A(N)=A(N)-RC1(N,ICOMP)*PRSITY(N)*
     &                        DELR(J)*DELC(I)*DH(N)
C
C--SORBED PHASE FOR EQUILIBRIUM-CONTROLLED ISOTHERMS
              IF(ISOTHM.EQ.1) THEN
                IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*RHOB(N)*
     &                          DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP)
              ELSEIF(ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
                RHS(N)=RHS(N)+RC2(N,ICOMP)*DELR(J)*DELC(I)*DH(N)*
     &                 RHOB(N)*SRCONC(N,ICOMP)
              ELSEIF(ISOTHM.EQ.6) THEN
                IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*FRAC(N)*     
     &                        RHOB(N)*DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP)
              ENDIF                 
            ENDDO                   
          ENDDO                     
        ENDDO                       
      ENDIF                         
C                                   
C--MONOD KINETIC REACTION           
C                                   
      IF(IREACT.EQ.2) THEN          
        DO K=1,NLAY                 
          DO I=1,NROW               
            DO J=1,NCOL             
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
C                                           
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
              IF(ICBUND(N,ICOMP).LE.0) CYCLE      
C                                                 
C--DISSOLVED PHASE                                
              IF(UPDLHS) A(N)=A(N)-RC1(N,ICOMP)*PRSITY(N)*
     &                        DELR(J)*DELC(I)*DH(N)/
     &                        (RC3(J,I,K,ICOMP)+COLD(N,ICOMP))
            ENDDO                                
          ENDDO                                  
        ENDDO                                    
      ENDIF                                      
C                                                
C--CONTRIBUTIONS TO [A] AND [RHS] FROM 1ST ORDER KINETIC REACTION 
C  WITH CHAIN DECAY
C                                                         
      IF(IREACT.EQ.3) THEN                                
        DO K=1,NLAY                                       
          DO I=1,NROW                                     
            DO J=1,NCOL                                   
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J              
C                                                         
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL        
              IF(ICBUND(N,ICOMP).LE.0) CYCLE              
C                                                         
C--DISSOLVED PHASE                                        
              IF(UPDLHS) A(N)=A(N)-RC1(N,ICOMP)*PRSITY(N)*
     &                        DELR(J)*DELC(I)*DH(N)                    
              IF(ICOMP.GT.1) RHS(N)=RHS(N)-YLD(ICOMP-1)*RC1(N,ICOMP-1)*
     &                              CNEW(N,ICOMP-1)*PRSITY(N)*
     &                              DELR(J)*DELC(I)*DH(N)
C                                                                 
C--SORBED PHASE FOR EQUILIBRIUM-CONTROLLED ISOTHERMS              
              IF(ISOTHM.EQ.1) THEN                                
                IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*RHOB(N)*         
     &                          DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP)
                IF(ICOMP.GT.1)RHS(N)=RHS(N)-YLD(ICOMP-1)*RC2(N,ICOMP-1)*
     &                               RHOB(N)*CNEW(N,ICOMP-1)*DELR(J)*
     &                               DELC(I)*DH(N)*SP1(N,ICOMP-1)
              ELSEIF(ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN           
                RHS(N)=RHS(N)+RC2(N,ICOMP)*DELR(J)*DELC(I)*DH(N)*
     &                 RHOB(N)*SRCONC(N,ICOMP)                       
                IF(ICOMP.GT.1)RHS(N)=RHS(N)-YLD(ICOMP-1)*RC2(N,ICOMP-1)*
     &                               DELR(J)*DELC(I)*DH(N)*
     &                               RHOB(N)*SRCONC(N,ICOMP-1)
              ELSEIF(ISOTHM.EQ.6) THEN                          
                IF(UPDLHS) A(N)=A(N)-RC2(N,ICOMP)*FRAC(N)*RHOB(N)*
     &                          DELR(J)*DELC(I)*DH(N)*SP1(N,ICOMP)
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
              RHS(N)=RHS(N)+RC1(N,ICOMP)*PRSITY(N)*DELR(J)*DELC(I)*DH(N)
C
C--SORBED PHASE FOR EQUILIBRIUM-CONTROLLED ISOTHERMS
              IF(ISOTHM.EQ.1.OR.ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
                RHS(N)=RHS(N)+RC2(N,ICOMP)*RHOB(N)*DELR(J)*DELC(I)*DH(N)
              ELSEIF(ISOTHM.EQ.6) THEN
                RHS(N)=RHS(N)+RC2(N,ICOMP)*FRAC(N)*RHOB(N)*
     &                 DELR(J)*DELC(I)*DH(N)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF                 
C
C--REACTION: ED + FEAED*EA --> PRODUCT                    
      IF(IREACTION.EQ.1) THEN                             
      IF(ICOMP.EQ.IED.OR.ICOMP.EQ.IEA) THEN               
        DO K=1,NLAY                                       
          DO I=1,NROW                                     
            DO J=1,NCOL                                   
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J              
C                                                         
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL        
              IF(ICBUND(N,ICOMP).LE.0) CYCLE              
C                                                         
              IF(ICOMP.EQ.IED) THEN                       
                IF(COLD(N,IED).GE.COLD(N,IEA)/FEDEA) THEN 
cvsb                  RHS(N)=RHS(N)+(COLD(N,IEA)/FEDEA)   
cvsb     &              *PRSITY(N)*DELR(J)*DELC(I)*DH(N)  
                ELSE                                      
cvsb                  RHS(N)=RHS(N)+(COLD(N,IED))         
cvsb     &              *PRSITY(N)*DELR(J)*DELC(I)*DH(N)  
                ENDIF                                     
              ELSE                                        
                IF(COLD(N,IEA).GE.COLD(N,IED)*FEDEA) THEN 
cvsb                  RHS(N)=RHS(N)+(COLD(N,IED)*FEDEA)   
cvsb     &              *PRSITY(N)*DELR(J)*DELC(I)*DH(N)  
                ELSE                                      
cvsb                  RHS(N)=RHS(N)+(COLD(N,IEA))         
cvsb     &              *PRSITY(N)*DELR(J)*DELC(I)*DH(N)  
                ENDIF                                     
              ENDIF                                       
            ENDDO                                         
          ENDDO                                           
        ENDDO                                             
      ENDIF                                               
C                                                         
      ELSEIF(IREACTION.EQ.2) THEN                         
        IF(ICOMP.EQ.NCOMP.AND.IFESLD.EQ.1) RETURN         
        DO K=1,NLAY                                       
          DO I=1,NROW                                     
            DO J=1,NCOL                                   
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                
C                                                           
              DCDT_S(N,ICOMP)=0.                            
              DCDT_SYLD(N,ICOMP)=0.                            
              IF(ICOMP.GT.NED) DCDT_FE(N,ICOMP-NED,1:NED)=0.
              IF(ICOMP.GT.1) THEN
                DO III=1,ICOMP-1
                  DCDT(III)=DCDT_S(N,III)
                  DCDTYLD(III)=DCDT_SYLD(N,III)
                ENDDO
              ENDIF
              DCDT(ICOMP)=0.                                
              DCDTYLD(ICOMP)=0.                                
              DEA_ED_DT(1:NED)=0.                           
              RVAL=0.                                       
C                                                           
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL          
              IF(ICBUND(N,ICOMP).LE.0)  CYCLE               
C                                                           
C--DISSOLVED PHASE                                          
              IF(UPDLHS) THEN                               
C	          ASSIGN CONCENTRATIONS                       
                RCOLD=COLD(N,1:NCOMP)                       
                DO MM=1,NCOMP                               
                  IF(RCOLD(MM)<0.0) RCOLD(MM)=0.0           
                ENDDO                                       
C                                                           
	          IF(SPECIAL(ICOMP).EQ."SOLID".AND.IFESLD.GT.0) THEN  
	            MAXEC(ICOMP)=COLD(N,NCOMP)/PRSITY(N)*RHOB(N)      
	          ENDIF                                               
C                                                                   
		      CALL reaction_sub(ICOMP,1)                        
C                                                                   
                DCDT_S(N,ICOMP)=DCDT(ICOMP)                         
                DCDT_SYLD(N,ICOMP)=DCDTYLD(ICOMP)
C                                                                   
                IF(ICOMP.LE.NED) THEN                               
                  RHS(N)=RHS(N)-DCDT(ICOMP)*PRSITY(N)*
     &                   DELR(J)*DELC(I)*DH(N)
                  RHS(N)=RHS(N)-DCDTYLD(ICOMP)*PRSITY(N)*DELR(J)*DELC(I)
     &                   *DH(N)
                ELSE                                                
                  DO MM=1,NED                                       
			      DCDT_FE(N,ICOMP-NED,MM)=DEA_ED_DT(MM)       
			      RHS(N)=RHS(N)-DEA_ED_DT(MM)*PRSITY(N)*DELR(J)*DELC(I)
     &                     *DH(N)
                  ENDDO
                ENDIF
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
      SUBROUTINE RCT1BD(ICOMP,DTRANS)
C **********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGET ASSOCIATED WITH REACTIONS.
C **********************************************************************
C
      USE MIN_SAT, ONLY: VAQSAT,ICIMDRY
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,PRSITY,
     &                         DELR,DELC,DH,ISOTHM,IREACT,RHOB,SP1,SP2,
     &                         SRCONC,RC1,RC2,PRSITY2,RETA2,FRAC,CNEW,
     &                         RETA,RFMIN,RMASIO,
     &                         COLD,iSSTrans
      USE RCTMOD                    
C
      IMPLICIT  NONE
      INTEGER   ICOMP,K,I,J
      REAL      DTRANS,DCRCT,CMML,CMMS,CIML,CIMS,VOLUME,DCRCT2
      REAL      CTMP,DCRCTX,DCRCTX2 
      INTEGER   M,N                 
C
      DCRCT=0.
      DCRCT2=0.
C
C--UPDATE RETARDATION FACTOR AND SORBED/IMMOBILE-PHASE CONCENTRATION
C
      IF(ISOTHM.GT.0) THEN
C                                                               
C--DETERMINE WHICH CONC TO USE IF IREACTION=1                   
        IF(IREACTION.EQ.1) THEN                                   
          IF(ICOMP.EQ.IED) THEN                                   
            CALL SRCT1R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY,
     &               CRCT(:,:,:,1),RETA(:,:,:,ICOMP),RFMIN,RHOB,
     &               SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),
     &               RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,
     &               SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)
          ELSEIF(ICOMP.EQ.IEA) THEN                               
            CALL SRCT1R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY,
     &               CRCT(:,:,:,2),RETA(:,:,:,ICOMP),RFMIN,RHOB,        
     &               SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),
     &               RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,  
     &               SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)    
          ELSE                                                    
            CALL SRCT1R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY,
     &               CNEW(:,:,:,ICOMP),RETA(:,:,:,ICOMP),RFMIN,RHOB,    
     &               SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),
     &               RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,  
     &               SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)    
          ENDIF                                                   
        ELSE                                                      
          CALL SRCT1R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY,
     &               CNEW(:,:,:,ICOMP),RETA(:,:,:,ICOMP),RFMIN,RHOB,
     &               SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),
     &               RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,
     &               SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)
        ENDIF                                                     
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
C--DETERMINE WHICH CONC TO USE IF IREACTION=1                
              IF(IREACTION.EQ.1) THEN                        
                IF(ICOMP.EQ.IED) THEN                        
                  CTMP=CRCT(J,I,K,1)                         
                ELSEIF(ICOMP.EQ.IEA) THEN                    
                  CTMP=CRCT(J,I,K,2)                         
                ELSE                                         
                  CTMP=CNEW(J,I,K,ICOMP)                     
                ENDIF                                        
              ELSE                                           
                CTMP=CNEW(J,I,K,ICOMP)                       
              ENDIF                                          
C                                                            
C--CALCULATE SOLUTE MASS CHANGE
              DCRCT=-SP2(J,I,K,ICOMP)*(CTMP-SRCONC(J,I,K,ICOMP)/
     &               SP1(J,I,K,ICOMP))*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
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
C--DETERMINE WHICH CONC TO USE IF IREACTION=1               
              IF(IREACTION.EQ.1) THEN                       
                IF(ICOMP.EQ.IED) THEN                       
                  CTMP=CRCT(J,I,K,1)                        
                ELSEIF(ICOMP.EQ.IEA) THEN                   
                  CTMP=CRCT(J,I,K,2)                        
                ELSE                                        
                  CTMP=CNEW(J,I,K,ICOMP)                    
                ENDIF                                       
              ELSE                                          
                CTMP=CNEW(J,I,K,ICOMP)                      
              ENDIF                                         
C                                                           
C--CALCULATE CHANGE IN CONCENTRATION OF MOBILE-LIQUID PHASE
              IF(iSSTrans.EQ.1) THEN
              DCRCT=0.
              ELSE
              DCRCT=-SP2(J,I,K,ICOMP)*(CTMP-SRCONC(J,I,K,ICOMP))*DTRANS*
     &               DELR(J)*DELC(I)*DH(J,I,K)
              ENDIF
C
C--SAVE LATEST SATURATED VOLUME OF AQUIFER TO HANDLE IMMOBILE DOMAIN IN DRY CELLS
              IF(ICIMDRY.GE.2) VAQSAT(J,I,K)=DELR(J)*DELC(I)*DH(J,I,K)
C
C--RECORD MASS STORAGE CHANGE IN IMMOBILE DOMAIN
              IF(DCRCT.LT.0) THEN
                RMASIO(121,2,ICOMP)=RMASIO(121,2,ICOMP)+DCRCT/
     &                              RETA2(J,I,K,ICOMP)
                RMASIO(122,2,ICOMP)=RMASIO(122,2,ICOMP)+DCRCT*
     &                              (RETA2(J,I,K,ICOMP)-1.)/
     &                              RETA2(J,I,K,ICOMP)
              ELSE
                RMASIO(121,1,ICOMP)=RMASIO(121,1,ICOMP)+DCRCT/
     &                              RETA2(J,I,K,ICOMP)
                RMASIO(122,1,ICOMP)=RMASIO(122,1,ICOMP)+DCRCT*
     &                              (RETA2(J,I,K,ICOMP)-1.)/
     &                              RETA2(J,I,K,ICOMP)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--CALCULATE MASS BUDGETS FOR
C--1st/0th ORDER IRREVERSIBLE REACTION
C
      IF(IREACT.ne.1.and.IREACT.ne.100.and.IREACT.ne.2.and.IREACT.ne.3)
     &  GOTO 9999                                                      
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
C--DETERMINE WHICH CONC TO USE IF IREACTION=1
            IF(IREACTION.EQ.1) THEN          
              IF(ICOMP.EQ.IED) THEN          
                CTMP=CRCT(J,I,K,1)           
              ELSEIF(ICOMP.EQ.IEA) THEN      
                CTMP=CRCT(J,I,K,2)           
              ELSE                           
                CTMP=CNEW(J,I,K,ICOMP)       
              ENDIF                          
            ELSE                             
              CTMP=CNEW(J,I,K,ICOMP)         
            ENDIF                            
C                                            
C--SKIP IF CONCENTRATION IS NOT POSITIVE
            IF(CTMP.LE.0) CYCLE
C
C--DISSOLVED PHASE
            IF(ireact.eq.1) THEN
              DCRCT=-RC1(J,I,K,ICOMP)*CTMP
     &              *DTRANS*DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)
            ELSEIF(ireact.eq.2) THEN
              DCRCT=-RC1(J,I,K,ICOMP)*CTMP*DTRANS*DELR(J)*DELC(I)*
     &               DH(J,I,K)*PRSITY(J,I,K)/(RC3(J,I,K,ICOMP)+CTMP)    
            ELSEIF(ireact.eq.3) THEN                             
              DCRCT=-RC1(J,I,K,ICOMP)*CTMP*DTRANS*DELR(J)*DELC(I)*
     &               DH(J,I,K)*PRSITY(J,I,K)
              IF(ICOMP.GT.1) THEN                   
                DCRCTX=YLD(ICOMP-1)*RC1(J,I,K,ICOMP-1)*
     &                 CNEW(J,I,K,ICOMP-1)*DTRANS*DELR(J)*DELC(I)*
     &                 DH(J,I,K)*PRSITY(J,I,K)
              ELSE                                               
                DCRCTX=0.                                        
              ENDIF                                              
            ELSEIF(ireact.eq.100) THEN
              DCRCT=-RC1(J,I,K,ICOMP)*DTRANS*DELR(J)*DELC(I)*
     &               DH(J,I,K)*PRSITY(J,I,K)
            ENDIF
C--SORBED PHASE
            DCRCT2=0.
            IF(ISOTHM.GT.0.and.ireact.eq.1) THEN
              DCRCT2=-RC2(J,I,K,ICOMP)*RHOB(J,I,K)*SRCONC(J,I,K,ICOMP)*
     &               DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
            ELSEIF(ISOTHM.GT.0.and.ireact.eq.3) THEN         
              DCRCT2=-RC2(J,I,K,ICOMP)*RHOB(J,I,K)*
     &              SRCONC(J,I,K,ICOMP)*DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
              IF(ICOMP.GT.1) THEN                            
                DCRCTX2=YLD(ICOMP-1)*RC2(J,I,K,ICOMP-1)*RHOB(J,I,K)*
     &                  SRCONC(J,I,K,ICOMP-1)*DTRANS*DELR(J)*DELC(I)*
     &                  DH(J,I,K)
              ELSE                                           
                DCRCTX2=0.                                   
              ENDIF                                          
            ELSEIF(ISOTHM.GT.0.and.ireact.eq.100) THEN
              DCRCT2=-RC2(J,I,K,ICOMP)*RHOB(J,I,K)*
     &               DTRANS*DELR(J)*DELC(I)*DH(J,I,K)
            ENDIF            
C
C--CALCULATE MASS LOSS/GAIN DUE TO 1st/0th ORDER REACTION
            IF(DCRCT+DCRCT2.LT.0) THEN
              RMASIO(9,2,ICOMP)=RMASIO(9,2,ICOMP)+DCRCT+DCRCT2
            ELSE
              RMASIO(9,1,ICOMP)=RMASIO(9,1,ICOMP)+DCRCT+DCRCT2
            ENDIF
C
C--CALCULATE MASS LOSS/GAIN DUE TO PARENT'S DECAY                 
            IF(ireact.eq.3.AND.ICOMP.GT.1) THEN                   
              IF(DCRCTX+DCRCTX2.LT.0) THEN                        
                RMASIO(9,2,ICOMP)=RMASIO(9,2,ICOMP)+DCRCTX+DCRCTX2
              ELSE                                                
                RMASIO(9,1,ICOMP)=RMASIO(9,1,ICOMP)+DCRCTX+DCRCTX2
              ENDIF                                               
            ENDIF                                                 
C                                                                 
C--UPDATE SORBED MASS STORAGE CHANGE FOR NONEQUILIBRIUM SORPTION
            IF(ISOTHM.EQ.4.AND.DCRCT2.GT.0) THEN
              RMASIO(120,2,ICOMP)=RMASIO(120,2,ICOMP)-DCRCT2
            ELSEIF(ISOTHM.EQ.4.AND.DCRCT2.LT.0) THEN
              RMASIO(120,1,ICOMP)=RMASIO(120,1,ICOMP)-DCRCT2
            ENDIF
C
C--UPDATE SORBED MASS STORAGE CHANGE FOR NONEQUILIBRIUM SORPTION - PARENT'S DECAY !# LINE 1415 RCT
            IF(IREACT.EQ.3.AND.ICOMP.GT.1) THEN                
              IF(ISOTHM.EQ.4.AND.DCRCTX2.GT.0) THEN            
                RMASIO(120,2,ICOMP)=RMASIO(120,2,ICOMP)-DCRCTX2
              ELSEIF(ISOTHM.EQ.4.AND.DCRCT2.LT.0) THEN         
                RMASIO(120,1,ICOMP)=RMASIO(120,1,ICOMP)-DCRCTX2
              ENDIF                                            
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
C--DETERMINE WHICH CONC TO USE IF IREACTION=1
            IF(IREACTION.EQ.1) THEN    
              IF(ICOMP.EQ.IED) THEN    
                CTMP=CRCT(J,I,K,1)     
              ELSEIF(ICOMP.EQ.IEA) THEN
                CTMP=CRCT(J,I,K,2)     
              ELSE                     
                CTMP=CNEW(J,I,K,ICOMP) 
              ENDIF                    
            ELSE                       
              CTMP=CNEW(J,I,K,ICOMP)   
            ENDIF                      
C                                      
C--SKIP IF CONCENTRATION IS NOT POSITIVE
            IF(CTMP.LE.0) CYCLE
C
C--compute mass loss/gain in each cell for all 4 phases: 
C--mobile liquid, mobile sorbed, immobile liquid, immobile sorbed
            VOLUME=DELR(J)*DELC(I)*DH(J,I,K)
            CMML=CTMP*PRSITY(J,I,K)*VOLUME 
            CMMS=(RETA(J,I,K,ICOMP)-1.)*CMML
            CIML=PRSITY2(J,I,K)*SRCONC(J,I,K,ICOMP)*VOLUME
            CIMS=(RETA2(J,I,K,ICOMP)-1.)*CIML            
C--for 1st-order reaction            
            IF(ireact.eq.1) THEN
              CMML=-RC1(J,I,K,ICOMP)*CMML*DTRANS
              CMMS=-RC2(J,I,K,ICOMP)*CMMS*DTRANS
              CIML=-RC1(J,I,K,ICOMP)*CIML*DTRANS
              CIMS=-RC2(J,I,K,ICOMP)*CIMS*DTRANS
C--for zero-order reaction
            ELSEIF(ireact.eq.100) THEN              
              CMML=-RC1(J,I,K,ICOMP)*VOLUME*PRSITY(J,I,K)*DTRANS
              CMMS=-RC2(J,I,K,ICOMP)*VOLUME*RHOB(J,I,K)*DTRANS
     &             *FRAC(J,I,K)
              CIML=-RC1(J,I,K,ICOMP)*VOLUME*PRSITY2(J,I,K)*DTRANS
              CIMS=-RC2(J,I,K,ICOMP)*VOLUME*RHOB(J,I,K)*DTRANS
     &             *(1.-FRAC(J,I,K))
            END IF            
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
            IF(iSSTrans.EQ.0) THEN
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
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
 9999 CONTINUE
C
C--REACTION: ED + FEAED*EA --> PRODUCT      
      IF(IREACTION.EQ.1) THEN               
        IF(ICOMP.EQ.IED.OR.ICOMP.EQ.IEA) THEN 
          DO K=1,NLAY                         
            DO I=1,NROW                       
              DO J=1,NCOL                     
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
                IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
C       
                IF(ICOMP.EQ.IED) THEN                         
                  RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)-      
     &                (CRCT(J,I,K,1)-CNEW(J,I,K,IED))         
     &                *PRSITY(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)
     &                *RETA(J,I,K,IED)
                ELSE                                              
                  RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)-          
     &                (CRCT(J,I,K,2)-CNEW(J,I,K,IEA))             
     &                *PRSITY(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)    
     &                *RETA(J,I,K,IEA)
                ENDIF                                               
              ENDDO                                                 
            ENDDO                                                   
          ENDDO                                                     
        ENDIF                                                       
      ELSEIF(IREACTION.EQ.2) THEN                                 
        DO K=1,NLAY                                                 
          DO I=1,NROW                                               
            DO J=1,NCOL                                             
              N=(K-1)*NCOL*NROW+(I-1)*NCOL+J                        
              IF(ICBUND(J,I,K,ICOMP).LE.0.AND.COLD(J,I,K,ICOMP)<=0)CYCLE
              IF(ICOMP<=NED) THEN                                   
                DCRCT=0.0                                           
                DCRCT2=0.0                                           
                IF(COLD(J,I,K,ICOMP)>0.) THEN                       
                  DCRCT=DCDT_S(N,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)*
     &                  PRSITY(J,I,K)*DTRANS           
                  DCRCT2=DCDT_SYLD(N,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)*
     &                   PRSITY(J,I,K)*DTRANS           
                ELSEIF(COLD(J,I,K,ICOMP)<=0.) THEN                  
                  DCRCT=0.0                                         
                  DCRCT2=DCDT_SYLD(N,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)*
     &                   PRSITY(J,I,K)*DTRANS           
                ELSE                                                
                  DCRCT=DCDT_S(N,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)*
     &                  PRSITY(J,I,K)*DTRANS           
                  DCRCT2=DCDT_SYLD(N,ICOMP)*DELR(J)*DELC(I)*DH(J,I,K)*
     &                   PRSITY(J,I,K)*DTRANS           
                ENDIF                                               
              ELSEIF(ICOMP>NED.AND.ICOMP<=NED+NEA) THEN              
                DCRCT=0.0                                           
                DCRCT2=0.0                                           
                DO M=1,NED                                          
                  IF(COLD(J,I,K,M)>0.) THEN                          
                    DCRCT=DCRCT+DCDT_FE(N,ICOMP-NED,M)*DELR(J)*DELC(I)*
     &                    PRSITY(J,I,K)*DH(J,I,K)*DTRANS
                  ELSEIF(COLD(J,I,K,M)<=0.) THEN                     
                    DCRCT=DCRCT+0.0                                   
                  ELSE                                              
                    DCRCT=DCRCT+DCDT_FE(N,ICOMP-NED,M)*DELR(J)*DELC(I)*
     &                    PRSITY(J,I,K)*DH(J,I,K)*DTRANS       
                  ENDIF                                             
                ENDDO                                               
              ELSEIF(ICOMP==NCOMP.and.IFESLD>0) THEN                 
                DCRCT=0.0
                DO M=1,NED                                         
                  IF(COLD(J,I,K,M)>0) THEN                          
                    DCRCT=DCRCT-DCDT_FE(N,NSOLID-NED,M)*DELR(J)*DELC(I)*
     &                    PRSITY(J,I,K)*DH(J,I,K)*DTRANS 
                  ELSEIF(COLD(J,I,K,M)<=0) THEN                     
                    DCRCT=DCRCT+0.0                                  
                  ELSE                                             
                    DCRCT=DCRCT-DCDT_FE(N,NSOLID-NED,M)*DELR(J)*DELC(I)*
     &                    PRSITY(J,I,K)*DH(J,I,K)*DTRANS 
                  ENDIF                                            
                ENDDO                                              
              ENDIF                                                
              IF(DCRCT<0.)THEN                              
                RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)+DCRCT
              ELSE                                         
                RMASIO(13,1,ICOMP)=RMASIO(13,1,ICOMP)+DCRCT
              ENDIF                                        
              IF(DCRCT2<0.)THEN                              
                RMASIO(13,2,ICOMP)=RMASIO(13,2,ICOMP)+DCRCT2
              ELSE                                         
                RMASIO(13,1,ICOMP)=RMASIO(13,1,ICOMP)+DCRCT2
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
      SUBROUTINE RCT1CF1(ICOMP,DTRANS)
C ********************************************************************
C THIS SUBROUTINE UPDATES NONLINEAR REACTION COEFFICIENTS.
C ********************************************************************
C
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,PRSITY,
     &                         RETA,RFMIN,RHOB,SP1,SP2,RC1,RC2,PRSITY2,
     &                         RETA2,FRAC,SRCONC,ISOTHM,IREACT,
     &                         CNEW
C
      IMPLICIT  NONE
      INTEGER   ICOMP
      REAL      DTRANS
C
      IF(ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
        CALL SRCT1R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),PRSITY,
     &              CNEW(:,:,:,ICOMP),RETA(:,:,:,ICOMP),RFMIN,RHOB,
     &              SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),
     &              RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,
     &              SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)
      ENDIF
C
C--RETURN
      RETURN
      END
C
      SUBROUTINE RCT1CF2(ICOMP,DTRANS)
C ********************************************************************
C THIS SUBROUTINE UPDATES NONLINEAR REACTION COEFFICIENTS.
C ********************************************************************
C
      USE UZTVARS,       ONLY: THETAW
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,PRSITY,
     &                         RETA,RFMIN,RHOB,SP1,SP2,RC1,RC2,PRSITY2,
     &                         RETA2,FRAC,SRCONC,ISOTHM,IREACT,
     &                         CNEW
C
      IMPLICIT  NONE
      INTEGER   ICOMP
      REAL      DTRANS
C
      IF(ISOTHM.EQ.1.OR.ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
        CALL SRCT1R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),THETAW,
     &              CNEW(:,:,:,ICOMP),RETA(:,:,:,ICOMP),RFMIN,RHOB,
     &              SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),
     &              RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,
     &              SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)
      ENDIF
C
C--RETURN
      RETURN
      END
C
      SUBROUTINE RCT1CF3(ICOMP,DTRANS)
C ********************************************************************
C THIS SUBROUTINE UPDATES NONLINEAR REACTION COEFFICIENTS.
C ********************************************************************
C
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,PRSITY,
     &                         RETA,RFMIN,RHOB,SP1,SP2,RC1,RC2,PRSITY2,
     &                         RETA2,FRAC,SRCONC,ISOTHM,IREACT,
     &                         CNEW,THETAW2
C
      IMPLICIT  NONE
      INTEGER   ICOMP
      REAL      DTRANS
C
      IF(ISOTHM.EQ.1.OR.ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
        CALL SRCT1R(NCOL,NROW,NLAY,ICBUND(:,:,:,ICOMP),THETAW2,
     &              CNEW(:,:,:,ICOMP),RETA(:,:,:,ICOMP),RFMIN,RHOB,
     &              SP1(:,:,:,ICOMP),SP2(:,:,:,ICOMP),RC1(:,:,:,ICOMP),
     &              RC2(:,:,:,ICOMP),PRSITY2,RETA2(:,:,:,ICOMP),FRAC,
     &              SRCONC(:,:,:,ICOMP),ISOTHM,IREACT,DTRANS,ICOMP)
      ENDIF
C
C--RETURN
      RETURN
      END
C
      SUBROUTINE FLASHREACT(ICOMP)
C ********************************************************************
C THIS SUBROUTINE CALCULATES FLASH CONCENTRATIONS AFTER APPLYING 
C REACTION: ED + FEAED*EA --> PRODUCT
C ********************************************************************
      USE MT3DMS_MODULE, ONLY: NCOMP,NLAY,NROW,NCOL,ICBUND,CNEW,RETA,
     &                         CADV
      USE RCTMOD
      IMPLICIT NONE
      INTEGER ICOMP,K,I,J,N
      REAL CED,CEA
      REAL C1,C2
C
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
C--SKIP IF INACTIVE OR CONSTANT CONCENTRATION CELL
            IF(ICBUND(J,I,K,1).LE.0) CYCLE
            C1=CNEW(J,I,K,IED)
            C2=CNEW(J,I,K,IEA)*RETA(J,I,K,IEA)/(FEDEA*RETA(J,I,K,IED))
            IF(C1.GE.C2) THEN
              CED=C1-C2
            ELSE
              CED=0.0
            ENDIF
            C1=CNEW(J,I,K,IED)*RETA(J,I,K,IED)*FEDEA/RETA(J,I,K,IEA)
            C2=CNEW(J,I,K,IEA)
            IF(C2.GE.C1) THEN
              CEA=C2-C1
            ELSE
              CEA=0.0
            ENDIF
            CRCT(J,I,K,1)=CNEW(J,I,K,IED)
            CRCT(J,I,K,2)=CNEW(J,I,K,IEA)
            CNEW(J,I,K,IED)=CED
            CNEW(J,I,K,IEA)=CEA
            CADV(J,I,K,IED)=CED
            CADV(J,I,K,IEA)=CEA
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END
C
      SUBROUTINE REACTION_PRE(NODES)
C ********************************************************************
C
C THIS SUBROUTINE READS THE REACTION FILE rec_FileName
C
C ********************************************************************
      USE MT3DMS_MODULE, ONLY: IOUT,MCOMP,NCOMP,NCOL,NROW,NLAY,SAVUCN
      USE RCTMOD
      CHARACTER*100 LINE
      INTEGER LLOC,ITYP1,ITYP2,ISTART,ISTOP
      INTEGER NODES
      REAL R
      LOGICAL OPND
      CHARACTER FINDEX*30
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
     &           ' FROM FILE: ',TRIM(rec_FileName),' ON UNIT NUMBER',
     &           INUNIT
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
     &       /1X,'NUMBER OF ELECTRON ACCEPTERS = ',I3,
     &       /1X,'NUMBER OF SPECIAL COMPONENTS = ',I3)
C
      WRITE(IOUT,*)
      WRITE(IOUT,'(2(A,I2))') 'ELECTRON DONORS:    SPECIES ',1,' - ',NED
      WRITE(IOUT,'(2(A,I2))') 'ELECTRON ACCEPTERS: SPECIES ',NED+1,
     &                        ' - ',NEA+NED
C
      ALLOCATE(RCOLD(NCOMP),RCNEW(NCOMP),SPECIAL(NCOMP))
      ALLOCATE(MAXEC(NCOMP),SWITCH(NCOMP),INHIB(NCOMP),
     &         DECAY(NED,NCOMP-NED))
      ALLOCATE(YIELDC(NED,NCOMP),DEA_ED_DT(NED),DCDT(NCOMP),
     &         DCDTYLD(NCOMP))
C
      ALLOCATE(MASS_NEG(NCOMP),CON_NEG(NCOMP))
      MASS_NEG=0.0
      CON_NEG=0.0
C
      IF(NSPECIAL.GE.1) THEN
        WRITE(IOUT,110)
110     FORMAT(/1x,'Species no.',5x,'Case code' , 5x, 'Max EFC',
     &         /1x,'-----------',5x,'---------' , 5x, '-------')
      ENDIF
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
            WRITE(*,*)    'INVALID SPECIES NO./KEYWORD (SOLID)'
            WRITE(*,*)    'KEYWORD SOLID ONLY APPLICABLE WITH EAs'
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
     &                         ' SOLID-PHASE SPECIES, IFESLD RESET TO 0'
        ELSEIF(NSOLID.EQ.0) THEN
          IFESLD=0
          WRITE(IOUT,'(/,2A)') 'NO SPECIES SET TO ''SOLID''',
     &                         ', IFESLD RESET TO 0'
        ELSE
          WRITE(IOUT,1040) NSOLID,NCOMP
        ENDIF
      ENDIF
1040  FORMAT(/1X,'SOLID PHASE FOR SPECIES ',I3,
     &           ' IS SIMULATED AS IMMOBILE SPECIES ',I3)
C
      WRITE(IOUT,'(/,2A)') 'EA#  SPECIES#  HALF SATURATION CONSTANT',
     &                     ' INHIBITION CONSTANT'
      WRITE(IOUT,'(2A)') '---  --------  ------------------------',
     &                   ' -------------------'
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
     &           'ED(1:NED)'
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
160       FORMAT(1X,'SAVE THE ADDITIONAL METHANE MASS ',
     &              'IN UNFORMATTED FILE [MT3D_Ad_methane.UCN]')
        ENDIF
      ENDIF
C
      ALLOCATE(DCDT_FE(NODES,NCOMP-NED,NED),DCDT_S(NODES,NCOMP),
     &         DCDT_SYLD(NODES,NCOMP))
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
	SUBROUTINE reaction_sub(icomp, cflag)
C
	! Implements general form of Lu et al (1999) Eq. 19-24
	USE RCTMOD
	implicit none
	integer     m,n,k, icomp
	integer     cflag
C
      m=icomp
      rval=0.
      IF (m.le.NED) THEN
	    DO n=1,NEA
		  ! first term: reaction rate times effective EA availability
	      IF(special(n+ned).eq.'MAXEC') THEN
              RVAL=0.
              IF(maxEC(n+ned).GE.rcold(n+ned)) THEN
	          rval = decay(m,n)*((maxEC(n+ned)-rcold(n+ned)) /
     &	             (switch(n) + (maxEC(n+ned)-rcold(n+ned))))
              ENDIF
            ELSEIF(special(n+ned).eq.'SOLID') THEN             !--JZ for solid phase iron
              IF(maxEC(n+ned)<=0.) CYCLE
              rval=decay(m,n) * maxEC(n+ned) /(switch(n) + maxEC(n+ned))
            ELSEIF(special(n+ned).eq.'STORE') THEN
              rval=decay(m,n) * (rcold(m) / (switch(n) + rcold(m)))  !For methane
            ELSE
              rval=decay(m,n)*(rcold(n+ned)/(switch(n) + rcold(n+ned)))!for other EAs
		  END IF
		  ! second term: inhibition by higher-sequence EAs
	      IF(n.gt.1) THEN
	        DO k=1,n-1
	          IF(special(k+ned).eq.'MAXEC') THEN
	            rval = rval * inhib(k) / (inhib(k) +
     &                     (maxEC(k+ned)-rcold(k+ned)))
                ELSEIF(special(k+ned).eq.'SOLID') THEN
                  IF(maxEC(k+ned)<=0.) CYCLE                 !--JZ for solid phase iron
	            rval = rval * inhib(k) / (inhib(k) +maxEC(k+ned))
                ELSE
                  rval = rval * inhib(k) / (inhib(k) + rcold(k+ned))
                END IF
	        END DO
	      END IF
		  ! update electron donor(s)
            IF (cflag==0) THEN
              dcdt(m) = dcdt(m) + rval *RCOLD(m)         !Call by the standalone module 
            ELSEIF(cflag==1) THEN
              dcdt(m) = dcdt(m) + rval  *RCOLD(m)        !call by MT3DMS
            END IF
	    END DO
C
          ! yield from a higher ED (added by MTONKIN in V12)
          IF(m.gt.1.and.m.le.ned) THEN
            DO k=1,m-1
              dcdtYLD(m)=dcdtYLD(m)-(yieldc(k,m))*dcdt(k) !*RCOLD(K)- COLD ALTERADY IN DCDT TERM
            END DO
          END IF
        END IF
C
	  ! ** USE the average of (COLD+CNEW) to calculate consumption? **
C
	  IF(m.gt.NED) THEN
		DO n=1,NED
		  ! first term: reaction rate times effect EA availability
	      IF(special(m).eq.'MAXEC') THEN
              RVAL=0.
              IF(maxEC(m).GE.rcold(m)) THEN
                rval = decay(n,m-ned) * ((maxEC(m)-rcold(m)) /
     & 	             (switch(m-ned) + (maxEC(m)-rcold(m))))          !for iron
              ENDIF
            ELSEIF(special(m).eq.'SOLID') THEN
              IF(maxEC(m)<=0.) CYCLE
              rval = decay(n,m-ned)*maxEC(m)/(switch(m-ned)+maxEC(m))  !for iron 
     	      ELSEIF(special(m).eq.'STORE') THEN
	        rval = decay(n,m-ned)*(rcold(n)/(switch(m-ned)+rcold(n))) !for methane
            ELSEIF(RCOLD(m)>0.0) THEN
	        rval = decay(n,m-ned)*(rcold(m)/(switch(m-ned)+rcold(m))) !for other EAs
	      ELSE 
	        rval=0.0    
	      END IF
C
		  ! second term: inhibition by higher-sequence EAs
	      IF(m.gt.ned+1) THEN
	        DO k=1,m-ned-1
	          IF(special(k+ned).eq.'MAXEC') THEN
                  rval = rval * inhib(k) / (inhib(k) +
     &	                   (maxEC(k+ned)-rcold(k+ned)))
                ELSEIF(special(k+ned).eq.'SOLID') THEN
                  IF(maxEC(k+ned)<=0.) CYCLE
                  rval = rval * inhib(k) / (inhib(k) +maxEC(k+ned))
                ELSE
		        rval = rval * inhib(k) / (inhib(k) + rcold(k+ned))
                END IF
	        END DO
	      END IF
	      ! update electron acceptor(s)
            IF(cflag==0) THEN
              dcdt(m) = dcdt(m) + rval*yieldc(n,m) *RCOLD(n)   !Call by the standalone module
            ELSEIF(cflag==1) THEN
              dea_Ed_DT(n)=rval*yieldc(n,m) *RCOLD(n)
              dcdt(m) = dcdt(m) + rval*yieldc(n,m) *RCOLD(n)   !Call by mt3dms,DONT MULTIPLY BY RCOLD WHEN CALLED BY MT3D AS THIS IS DONE WITHIN MT3D
            END IF  
          END DO                                               !USE yieldc(n,m-ned) WHEN COMPILING WITH MT3D, and yieldc(n,m) WHEN COMPILING ALONE
	  END IF
C
	!endDO ! ** This mimics the MT3D main NCOMP loop **
C
	RETURN
	END SUBROUTINE reaction_sub
C
C
      SUBROUTINE Stor_Add_Methane(cMethane, ICOMP, DTRANS) 
!this SUBROUTINE is to check the concentration of methane, IF it is over the maximum
!EFC, THEN the additional mass of methane will be stored into a array, and the 
!concentration of methane will be assigned to equal the maximum EFC. 
      USE RCTMOD
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
            IF((cMethane(J,I,K)-MaxEC(ICOMP))>0.) THEN
               MassStor(J,I,K)=MassStor(J,I,K)+(cMethane(J,I,K)-
     &                         MaxEC(ICOMP))*PRSITY(J,I,K)*DELR(K)*  
     &                         DELC(I)*DH(J,I,K)*DTRANS*0.5*DTRANS 
                               cMethane(J,I,K)=MaxEC(ICOMP)
            ENDIF                                                   
          ENDDO                                                       
        ENDDO
      ENDDO      
C
      END SUBROUTINE
C
C
      SUBROUTINE KINETIC_SOLID(ICOMP,DTRANS)
      USE RCTMOD
      USE MT3DMS_MODULE, ONLY: DELR,DELC,PRSITY,DH,NROW,NLAY,NCOL,CNEW,
     1                         ICBUND,COLD,RHOB
      INTEGER ICOMP
      REAL DTRANS
C
      DO IEDEA=1,NED+NEA                         
        IF(SPECIAL(IEDEA)=="SOLID") THEN         
          DO K=1,NLAY                            
            DO I=1,NROW                          
              DO J=1,NCOL                        
                NN=(K-1)*NCOL*NROW+(I-1)*NCOL+J  
                IF(ICBUND(J,I,K,1).LE.0) CYCLE   
                MAXEC(IEDEA)=COLD(J,I,K,ICOMP)*RHOB(J,I,K)/PRSITY(J,I,K)
                DO II=1,NED                            
                  MAXEC(IEDEA)=MAXEC(IEDEA)-
     &                         DCDT_FE(NN,IEDEA-NED,II)*DTRANS
                ENDDO                          
                CNEW(J,I,K,ICOMP)=MAXEC(IEDEA)*
     &                            PRSITY(J,I,K)/RHOB(J,I,K)
              ENDDO                                   
            ENDDO                                     
          ENDDO                                       
        ENDIF                                         
      ENDDO                                           
C
      RETURN
      END
C
C
      SUBROUTINE DTS(ICOMP)
      USE RCTMOD
      USE MT3DMS_MODULE, ONLY: DELR,DELC,PRSITY,DH,NROW,NLAY,NCOL,CNEW,
     &                         ICBUND,COLD,RHOB
      INTEGER ICOMP
      REAL DTRANS
C
C--THIS SUBROUTINE CALCULATES A STABLE TIME-STEP SIZE
C
      DO K=1,NLAY    
        DO I=1,NROW  
          DO J=1,NCOL
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE 
            IF(CNEW(J,I,K,ICOMP).GT.MAXEC(ICOMP)) THEN 
              CNEW=COLD
            ENDIF
          ENDDO
        ENDDO  
      ENDDO
      RETURN
      END
