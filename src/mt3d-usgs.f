C 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C                                                                      %
C                             MT3D-USGS                                %
C      a modular three-dimensional multi-species transport model       %
C    for simulation of advection, dispersion and chemical reactions    %
C                of contaminants in groundwater systems                %
C                                                                      %
C                  For Technical Information Contact                   %
C                                                                      %
C                           Vivek Bedekar                              %
C                    S.S. Papadopulos & Associates                     %
C                                                                      %
C                      Bethesda, MD 20814, USA                         %
C                      Email: vivekb@sspa.com                          %
C                   Web site: http://www.sspa.com/                     %
C                                                                      %
C                                 or                                   %
C                                                                      %
C                             Eric Morway                              %
C                       U.S. Geological Survey                         %
C                                                                      %
C                    Carson City, NV 89701, USA                        %
C                     Email: emorway@usgs.gov                          %
C   Web site: https://www.usgs.gov/ or http://water.usgs.gov/ogw/      %
C                                                                      %
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C MT3D-USGS is based on MT3DMS v5.3 originally developed by Chunmiao 
C Zheng at S.S. Papadopulos & Associates, Inc. and documented for
C the United States Environmental Protection Agency.  MT3D-USGS is
C written by authors at S.S. Papadopulos & Associates, Inc. and the 
C U.S. Geological Survey with the iterative solver routine by Tsun-Zee 
C Mai.  Funding for MT3D-USGS development is provided, in part, by
C U.S. Geological Survey's Groundwater Resources Program.
C
C Please refer to the USGS Water Resources Software User Rights Notice 
C
C    http://water.usgs.gov/software/help/notice/
C
C for complete use, copyright, and distribution information. This 
C software has been approved for release by the U.S. Geological Survey 
C (USGS). Although the software has been subjected to rigorous review, 
C the USGS reserves the right to update the software as needed pursuant 
C to further analysis and review. No warranty, expressed or implied, is 
C made by the USGS or the U.S. Government as to the functionality of 
C the software and related material nor shall the fact of release 
C constitute any such warranty. Furthermore, the software is released 
C on condition that neither the USGS nor the U.S. Government shall be 
C held liable for any damages resulting from its authorized or 
C unauthorized use.
C
C=======================================================================
C Version history: 09-30-2016 (1.00)
C                  02-28-2019 (1.0.1)
C
C  =====================================================================                                        
C
      USE RCTMOD                                                 
      USE MIN_SAT                                                
      USE SFRVARS
      USE LAKVARS
      USE UZTVARS,       ONLY: PRSITYSAV,SATOLD
      USE MT3DMS_MODULE, ONLY: MXTRNOP,MXSTP,
     &                         FPRT,INBTN,ICBUND,CADV,COLD,RETA,PRSITY,
     &                         DZ,DH,QX,QY,QZ,A,RHOB,SP1,SP2,SRCONC,
     &                         RC1,RC2,PRSITY2,RETA2,FRAC,CNEW,IOUT,
     &                         iUnitTRNOP,IMPSOL,ISPD,MIXELM,NPER,NSTP,
     &                         TSLNGH,ISS,NPERFL,MXSTRN,NCOMP,MCOMP,
     &                         ISOTHM,UPDLHS,MXITER,FMNW,HT1,HT2,DTRANS,
     &                         DELT,TIME1,TIME2,NPS,
     &                         MEMDEALLOCATE,
     &                         INTSO,INLKT,INSFT,
     &                         IWCTS,IALTFM,NOCREWET,        
     &                         NODES,SAVUCN,NLAY,NROW,NCOL,COLDFLW,
     &                         IDRY2,FLAM1,FLAM2,
     &                         FMIFMT6
      USE FMI1MF6, ONLY: FMI1MF6AR, FMI1MF6RP1A, FMI1MF6RP2A
      USE DSSL
C
      IMPLICIT  NONE
      INTEGER iNameFile,KPER,KSTP,N,ICOMP,ICNVG,ITO,ITP,IFLEN
      CHARACTER FLNAME*5000
      CHARACTER COMLIN*2000               
      INTEGER II,NN,I,J,K,OperFlag,IEDEA 
      REAL DT00                          
      REAL START_TIME,TOTAL_TIME,
     &     END_TIME
      LOGICAL existed
      CHARACTER,PARAMETER :: VID*18='[Ver 1.00.00]'
C
C--ALLOCATE LOGICALS
      ALLOCATE(DOMINSAT,DRYON)
      DOMINSAT=.FALSE.        
      DRYON=.FALSE.           
C
C--Initialize variables 
      ALLOCATE(IREACTION,IALTFM,NOCREWET,ICIMDRY,IDRY2,ISFTTR)
!      IWCTS=1
      IREACTION=0
      IALTFM=5 !NEW STORAGE FORMULATION
      NOCREWET=0
      ICIMDRY=0
      IDRY2=0
C
C--Get CPU time at the start of simulation
      Call CPU_TIME(start_time)
C
C--WRITE AN IDENTIFIER TO SCREEN
      WRITE(*,101) VID
  101 FORMAT(
     &      /1X,'MT3D-USGS - Modular 3D Multi-Species Transport Model ',
     &  A14,/1X,'and based on MT3DMS. MT3D-USGS developed in ',
     &          'cooperation by ',/1X,'S.S. Papadopulos & Associates ',
     &          'and the U.S. Geological Survey',/)
C
C--INITIALIZE CHARACTER VARIABLES
      FLNAME=' '
C
C--The following statement should be uncommented in order to use
C--GETCL to retrieve a command line argument.  The call to GETCL may
C--be commented out for compilers that do not support it.
      !CALL GETCL(FLNAME)
      CALL GETARG(1,COMLIN)
C                                          
      IF(COMLIN.NE.' ') THEN               
        flname=COMLIN                      
      ELSE                                 
C--Get Name of NAME File from Screen
        IF(FLNAME.EQ.' ') THEN
          WRITE(*,102)
  102     FORMAT(1x,'Enter Name of the MT3DMS NAME File: ')
          READ(*,'(a)') flname
        ENDIF
      ENDIF
C
C-Open files using the Name File method as in MODFLOW-2000      
      iflen=INDEX(flname,' ')-1
      INQUIRE(file=flname(1:iflen),exist=existed)
      IF(.not.existed) THEN
        flname=flname(1:iflen)//'.nam'
        INQUIRE(file=flname(1:iflen+4),exist=existed)
        IF(.not.existed) THEN
          WRITE(*,103) flname(1:iflen),flname(1:iflen+4)
          CALL ustop(' ')
        ENDIF
      ENDIF
  103 FORMAT(1x,'STOP. Specified Name file does not exist: ',
     &        a,' or ',a)
      WRITE(*,104) TRIM(FLNAME)
  104 FORMAT(1x,'Using NAME File: ',a)
      iNameFile=99
      OPEN(iNameFile,file=flname,status='old')
      CALL BTN1OPEN(iNameFile)
      CLOSE (iNameFile)
C
C--WRITE PROGRAM TITLE TO OUTPUT FILE
      WRITE(IOUT,11)
   11 FORMAT(/30X,71('+')/30X,'+',69X,'+'
     &       /30X,'+',30X,'MT3D-USGS',30X,'+'
     &       /30X,'+',13X,'A Modular 3D Multi-Species Transport Model ',
     &                13X,'+'
     &       /30X,'+', 4X,'For Simulation of Advection, Dispersion and',
     &                ' Chemical Reactions',3X,'+'
     &       /30X,'+',16X,'of Contaminants in Groundwater Systems',15X,
     &            '+',
     &       /30X,'+',69X,'+'/30X,71('+')/)
C
C--DEFINE PROBLEM DIMENSION AND SIMULATION OPTIONS
      CALL BTN1AR(INBTN)
C
C--ALLOCATE STORAGE SPACE FOR DATA ARRAYS
      IF(FMIFMT6) THEN
        CALL FMI1MF6AR()
      ELSE
        CALL FMI1MF5AR()
      ENDIF
      IF(iUnitTRNOP(1).GT.0) CALL ADV1AR(iUnitTRNOP(1))
      IF(iUnitTRNOP(2).GT.0) CALL DSP1AR(iUnitTRNOP(2))
      IF(iUnitTRNOP(3).GT.0) CALL SSM1AR(iUnitTRNOP(3))
      IF(iUnitTRNOP(7).GT.0) CALL UZT1AR(iUnitTRNOP(7))
      IF(iUnitTRNOP(4).GT.0) CALL RCT1AR(iUnitTRNOP(4))
      IF(iUnitTRNOP(5).GT.0) CALL GCG1AR(iUnitTRNOP(5))
      IF(iUnitTRNOP(20).GT.0) CALL CTS1AR(iUnitTRNOP(20))
      IF(iUnitTRNOP(11).GT.0) CALL TOB1AR(iUnitTRNOP(11))
      IF(iUnitTRNOP(13).GT.0) CALL HSS1AR(iUnitTRNOP(13))
      IF(iUnitTRNOP(18).GT.0) CALL LKT1AR(iUnitTRNOP(18))
      IF(iUnitTRNOP(19).GT.0) CALL SFT1AR(iUnitTRNOP(19))
C
C--INITIALIZE VARIABLES.      
      IF(iUnitTRNOP(5).EQ.0) THEN
        WRITE(*,107) 
  107   FORMAT(1X,'STOP. GCG SOLVER PACKAGE MUST BE ACTIVATED')
        CALL USTOP(' ')
      ENDIF      
      IMPSOL=1
      ISPD=1
      IF(MIXELM.EQ.0) ISPD=0
      IF(iUnitTRNOP(18).GT.0) CALL LKT1RP(KPER)
      IF(iUnitTRNOP(19).GT.0) CALL SFT1RP(KPER)
C
C--FOR EACH STRESS PERIOD***********************************************
      HT1=0.
      HT2=0.
      DTRANS=0.
      NPS=1
      DO KPER=1,NPER
C
C--WRITE AN IDENTIFYING MESSAGE
        WRITE(*,50) KPER
        WRITE(IOUT,51) KPER
        WRITE(IOUT,'(1X)')
   50   FORMAT(/1X,'STRESS PERIOD NO.',I5)
   51   FORMAT(//35X,62('+')/55X,'STRESS PERIOD NO.',I5.3/35X,62('+'))
C
C--GET STRESS TIMING INFORMATION
        CALL BTN1ST(KPER)
C
C--READ AND PREPARE INPUT INFORMATION WHICH IS CONSTANT
C--WITHIN EACH STRESS PERIOD
        IF(iUnitTRNOP(3).GT.0)  CALL SSM1RP(KPER)
        IF(iUnitTRNOP(20).GT.0) CALL CTS1RP(KPER)
        IF(iUnitTRNOP(7).GT.0)  CALL UZT1RP(KPER)
C--READ LAK AND SFR BOUNDARY CONDITIONS
        IF(iUnitTRNOP(18).GT.0) CALL LKT1SS(KPER)
        IF(iUnitTRNOP(19).GT.0) CALL SFT1SS(KPER)
C
C--FOR EACH FLOW TIME STEP----------------------------------------------
        DO KSTP=1,NSTP
          DELT=TSLNGH(KSTP)
          HT1=HT2
          HT2=HT2+DELT    
C
C--WRITE AN IDENTIFYING MESSAGE
          WRITE(*,60) KSTP,HT1,HT2
          WRITE(IOUT,61) KSTP,HT1,HT2
          WRITE(IOUT,'(1X)')
   60     FORMAT(/1X,'TIME STEP NO.',I5
     &           /1X,'FROM TIME =',G13.5,' TO ',G13.5/)
   61     FORMAT(//42X,48('=')/57X,'TIME STEP NO.',I5.3/42X,48('=')
     &          //1X,'FROM TIME =',G13.5,' TO ',G13.5)
C
C--READ AND PROCESS SATURATED THICKNESS, VELOCITY COMPONENTS
C--ACROSS CELL INTERFACES, AND SINK/SOURCE INFORMATION
C--(NOTE THAT THESE ITEMS ARE READ ONLY ONCE IF FLOW MODEL
C--IS STEADY-STATE AND HAS SINGLE STRESS PERIOD)
          IF(KPER*KSTP.GT.1.AND.ISS.NE.0.AND.NPERFL.EQ.1) GOTO 70
C
          IF(KPER*KSTP.EQ.1) THEN
            IF(IDRY2.EQ.1) THEN
              DO ICOMP=1,NCOMP
                DO K=1,NLAY
                  DO I=1,NROW
                    DO J=1,NCOL
                      COLDFLW(J,I,K,ICOMP)=CNEW(J,I,K,ICOMP)
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
          ENDIF
C
          IF(KPER*KSTP.GT.1) THEN
            IF(iUnitTRNOP(19).GT.0) CALL SFT1AD2(N)
          ENDIF
C
          IF(.NOT.FMIFMT6) THEN
            CALL FMI1MF5RP1A(KPER,KSTP)
          ELSEIF(FMIFMT6) THEN
            CALL FMI1MF6RP1A(KPER,KSTP)
          ENDIF
          CALL FMI1RP1B(KPER,KSTP)
          
          IF(iUnitTRNOP(3).GT.0) THEN
            IF(.NOT.FMIFMT6) THEN
              CALL FMI1MF5RP2A(KPER,KSTP)
            ELSEIF(FMIFMT6) THEN
              CALL FMI1MF6RP2A(KPER,KSTP)
            ENDIF
            CALL FMI1RP2B(KPER,KSTP)
          ENDIF
C
          IF(DRYON) CALL ADVQC1RP(KPER,KSTP)
C                                           
          IF(iUnitTRNOP(19).GT.0) THEN      
            CALL FILLIASFJASF()
            !IF(KPER*KSTP.EQ.1) CALL XMD7AR()
            CALL XMD7AR()
          ENDIF                             
C
C--CALCULATE COEFFICIENTS THAT VARY WITH FLOW-MODEL TIME STEP
          IF(iUnitTRNOP(2).GT.0) CALL DSP1CF(KSTP,KPER)
C
   70     CONTINUE
C
C--FOR EACH TRANSPORT STEP..............................................
          TIME2=HT1
          DO N=1,MXSTRN
C
C--ADVANCE ONE TRANSPORT STEP
            CALL BTN1AD(N,TIME1,TIME2,HT2,DELT,KSTP,KPER,DTRANS,NPS,HT1)
C--UPDATE CONCENTRATIONS OF LAKE VOLUMES                    
            IF(iUnitTRNOP(18).GT.0) CALL LKT1AD(N)          
            IF(iUnitTRNOP(19).GT.0) CALL SFT1AD(KSTP,KPER,N)
            IF(iUnitTRNOP(7).GT.0) CALL UZT1AD(HT1,HT2,TIME1,TIME2)
            IF(IALTFM.EQ.3) CALL THETA2AD(HT2,TIME2)
C
C--FOR EACH COMPONENT......
            DO ICOMP=1,NCOMP
C
C--TAKE CARE OF Fe3+                                   
              IF(IREACTION.EQ.2) THEN                  
                IF(ICOMP==NCOMP.AND.IFESLD>0)GOTO 1001 
              ENDIF                                    
C
C--SOLVE TRANSPORT TERMS WITH EXPLICIT SCHEMES
              IF(MIXELM.EQ.0) GOTO 1500
C
C--FORMULATE AND SOLVE
              CALL BTN1SV(ICOMP)
              IF(iUnitTRNOP(1).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL ADV1SV(ICOMP,DTRANS)
C     
 1500         CONTINUE
C
C--SOLVE TRANSPORT TERMS WITH IMPLICIT SCHEMES
              IF(DTRANS.EQ.0) THEN
                ICNVG=1
                GOTO 110
              ENDIF
C
C--ALWAYS UPDATE MATRIX IF NONLINEAR SORPTION OR MULTICOMPONENT
              IF(iUnitTRNOP(4).GT.0.AND.ISOTHM.GT.1) UPDLHS=.TRUE.
              IF(NCOMP.GT.1) UPDLHS=.TRUE.
              IF(IALTFM.GE.2) UPDLHS=.TRUE.
              IF(iUnitTRNOP(20).GT.0 .OR.
     &           iUnitTRNOP(18).GT.0 .OR.
     &           iUnitTRNOP(19).GT.0) UPDLHS=.TRUE.
C
C--FOR EACH OUTER ITERATION...
              DO ITO=1,MXITER
C
C--UPDATE COEFFICIENTS THAT VARY WITH ITERATIONS
                IF(iUnitTRNOP(4).GT.0) THEN
                  IF(iUnitTRNOP(7).EQ.0) THEN
                    IF(IALTFM.EQ.3) THEN
                      CALL RCT1CF3(ICOMP,DTRANS)
                    ELSE
                      CALL RCT1CF1(ICOMP,DTRANS)
                    ENDIF
                  ELSE
                    CALL RCT1CF2(ICOMP,DTRANS)
                  ENDIF
                ENDIF
C
C--FORMULATE MATRIX COEFFICIENTS
                CALL BTN1FM(ICOMP,ICBUND,CADV,COLD,RETA,PRSITY,DH,
     &                      DTRANS,HT2,TIME2)
                IF(iUnitTRNOP(1).GT.0.AND.MIXELM.EQ.0 
     &             .AND. ICOMP.LE.MCOMP)
     &           CALL ADV1FM(ICOMP,ICBUND,DH,QX,QY,QZ,A)
                IF(iUnitTRNOP(2).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL DSP1FM(ICOMP,ICBUND,A,CNEW)
                IF(iUnitTRNOP(3).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL SSM1FM(ICOMP,HT2,TIME1,TIME2)
                IF(iUnitTRNOP(7).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL UZT1FM(ICOMP)
                IF(iUnitTRNOP(13).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL HSS1FM(ICOMP,ICBUND,time1,time2,DTRANS)
                IF(iUnitTRNOP(20).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL CTS1FM(ICOMP)                        
                IF(iUnitTRNOP(18).GT.0)                    
     &           CALL LKT1FM(ICOMP)                        
                IF(iUnitTRNOP(19).GT.0) !OR SWR OR MNW2 ETC
     &           CALL GNT1FM(ICOMP)                        
                IF(iUnitTRNOP(4).GT.0) 
     &           CALL RCT1FM(ICOMP,ICBUND,PRSITY,DH,RHOB,SP1,SP2,SRCONC,
     &                  RC1,RC2,PRSITY2,RETA2,FRAC,DTRANS,
     &                  COLD,CNEW,ITO,FLAM1,FLAM2,RETA)
                IF(iUnitTRNOP(1).GT.0.AND.MIXELM.LE.0       
     &           .AND. ICOMP.LE.MCOMP .AND. DRYON)
     &           CALL ADVQC1FM(ICOMP)                       
                IF(iUnitTRNOP(5).GT.0)
     &            CALL GCG1AP(IOUT,ITO,ITP,ICNVG,N,KSTP,KPER,TIME2,
     &                        HT2,ICBUND(:,:,:,ICOMP),CNEW(:,:,:,ICOMP))
C
                IF(IREACTION.EQ.2) THEN
                  IF(ICOMP.LE.NED+NEA) THEN 
                    IF(SPECIAL(ICOMP)=="MAXEC") THEN
                      !CALL DTS(ICOMP)
                    ENDIF
                  ENDIF
                ENDIF
C
C--IF CONVERGED, GO TO NEXT OUTER ITERATION
                IF(ICNVG.EQ.1) GOTO 110
C
C--END OF OUTER ITERATION LOOP
              ENDDO
  110         CONTINUE
C
C-------------TAKE CARE OF Fe2+                        
 1001         IF(IREACTION.EQ.2) THEN                  
                IF(ICOMP.EQ.NCOMP.AND.IFESLD.GT.0) THEN
                  CALL KINETIC_SOLID(ICOMP,DTRANS)
                ENDIF                                  
              ENDIF                                    
C                                                      
C--END OF COMPONENT LOOP
            ENDDO
C
C--APPLY ED/EA REACTION AS A FLASH CALCULATION 
            IF(IREACTION.EQ.1) THEN            
              CALL FLASHREACT(ICOMP)           
            ENDIF                              
C                                              
C--CALCULATE MASS BUDGETS AND SAVE RESULTS FOR ALL COMPONENTS
            DO ICOMP=1,NCOMP
C
C--CALCULATE MASS BUDGETS FOR IMPLICIT SCHEMES
C
              IF(iUnitTRNOP(1).GT.0.AND.MIXELM.EQ.0 
     &           .AND. ICOMP.LE.MCOMP)
     &         CALL ADV1BD(ICOMP,DTRANS,N,KPER,KSTP)
              IF(iUnitTRNOP(2).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL DSP1BD(ICOMP,DTRANS)
              IF(iUnitTRNOP(3).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL SSM1BD(ICOMP,DTRANS,HT2,TIME1,TIME2)
              IF(iUnitTRNOP(7).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL UZT1BD(ICOMP,DTRANS)
              IF(iUnitTRNOP(13).GT.0 .AND. ICOMP.LE.MCOMP) 
     &         CALL HSS1BD(ICOMP,ICBUND,50,time1,time2,DTRANS)     
              IF(iUnitTRNOP(20).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL CTS1BD(KSTP,KPER,ICOMP,DTRANS,N)     
              IF(iUnitTRNOP(18).GT.0)                    
     1         CALL LKT1BD(ICOMP,KPER,KSTP,DTRANS,N)    
              IF(iUnitTRNOP(19).GT.0)                    
     1         CALL SFT1BD(ICOMP,KPER,KSTP,DTRANS,N)    
              IF(iUnitTRNOP(4).GT.0) 
     &         CALL RCT1BD(ICOMP,DTRANS)
              IF(iUnitTRNOP(1).GT.0.AND.MIXELM.LE.0         
     &           .AND. ICOMP.LE.MCOMP .AND. DRYON)
     &         CALL ADVQC1BD(ICOMP)      
C
C--CALCULATE GLOBAL MASS BUDGETS AND CHECK MASS BALANCE
              CALL BTN1BD(ICOMP,DTRANS,TIME2,HT2)
C
C--STORE ADDITIONAL MASS AND RESET CONC TO MAX EXPRESSED FIELD CAPACITY
              IF(IREACTION.EQ.2) THEN                       
                IF(ICOMP<=NED+NEA)THEN                      
                  IF(SPECIAL(ICOMP)=="STORE")THEN           
                    CALL Stor_Add_Methane(CNEW(:,:,:,ICOMP),
     &                                    ICOMP,DTRANS)     
                  ENDIF                                     
                ENDIF                                       
              ENDIF                                         
C
C--SAVE OUTPUTS
              CALL BTN1OT(KPER,KSTP,N,ICOMP,TIME2)   
              IF(FMNW) CALL SSM1OT(KPER,KSTP,N,TIME2)
              IF(iUnitTRNOP(11).GT.0) CALL TOB1OT(KPER,KSTP,N,
     &                                            TIME1,TIME2)
C              
            ENDDO !done with budget and output
C
            IF(TIME2.GE.HT2) GOTO 900
            IF(ICNVG.EQ.0) THEN
              WRITE(*,808) 
  808         FORMAT(1X,'STOP. GCG SOLVER FAILED TO CONVERGE.')
              CALL USTOP(' ')
            ENDIF
C
C--END OF TRANSPORT STEP LOOP
          ENDDO
C
          IF(TIME2.LT.HT2) THEN 
            WRITE(IOUT,810) MXSTRN
  810       FORMAT(/1X,'NUMBER OF TRANSPORT STEPS EXCEEDS',
     &                 ' SPECIFIED MAXIMUM (MXSTRN) =',I10)
            CALL USTOP(' ')
          ENDIF
  900     CONTINUE
C
C--SAVE CNEW AS COLDFLW
          IF(IDRY2.EQ.1) CALL BTNFLW1AD
C
C--END OF FLOW TIME STEP LOOP
        ENDDO
C
C--END OF STRESS PERIOD LOOP
      ENDDO
C
C--PROGRAM COMPLETED
      WRITE(IOUT,1200)
      WRITE(IOUT,1225)
      WRITE(IOUT,1200)
 1200 FORMAT(1X,' ----- ')
 1225 FORMAT(1X,'| M T |'
     &      /1X,'| 3 D | END OF MODEL OUTPUT')
C
C--CLOSE FILES                          
      IF(IREACTION.EQ.2) THEN           
        IF(NSTORE.NE.0 .and. SAVUCN)THEN
          CLOSE(IUMETH)                 
        ENDIF                           
      ENDIF                             
C--DEALLOCATE MEMORY
      CALL MEMDEALLOCATE()
      CALL MEMDEALLOCATE2()
      CALL MEMDEALLOCATE4()
      CALL MEMDEALLOCATE5()
      CALL MEMDEALLOCATE_DSSL()
C
C--Get CPU time at the end of simulation
C--and print out total elapsed time in seconds
      Call CPU_TIME(end_time)
      total_time = end_time - start_time
      WRITE(*,2010) int(total_time/60.),mod(total_time,60.)
 2010 FORMAT(/1X,'Program completed.   ',
     &           'Total CPU time:',i5.3,' minutes ',f6.3,' seconds')
C
      STOP
      END
