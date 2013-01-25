C 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C                                                                      %
C                               MT3DMS                                 %
C      a modular three-dimensional multi-species transport model       %
C    for simulation of advection, dispersion and chemical reactions    %
C                of contaminants in groundwater systems                %
C                                                                      %
C                  For Technical Information Contact                   %
C                           Chunmiao Zheng                             %
C                  Department of Geological Sciences                   %
C                        University of Alabama                         %
C                      Tuscaloosa, AL 35487, USA                       %
C                        Email: czheng@ua.edu                          %
C              Web site: http://hydro.geo.ua.edu/mt3d                  %
C                                                                      %
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C MT3DMS is based on MT3D originally developed by Chunmiao Zheng
C at S.S. Papadopulos & Associates, Inc. and documented for
C the United States Environmental Protection Agency.
C MT3DMS is written by Chunmiao Zheng and P. Patrick Wang
C with the iterative solver routine by Tsun-Zee Mai.
C Funding for MT3DMS development is provided, in part, by
C U.S. Army Corps of Engineers, Research and Development Center.
C
C Copyright, 1998-2010, The University of Alabama. All rights reserved.
C
C This program is provided without any warranty.
C No author or distributor accepts any responsibility
C to anyone for the consequences of using it
C or for whether it serves any particular purpose.
C The program may be copied, modified and redistributed,
C but ONLY under the condition that the above copyright notice
C and this notice remain intact.
C
C=======================================================================
C Version history: 06-23-1998 (3.00.A)
C                  05-10-1999 (3.00.B)
C                  11-15-1999 (3.50.A)
C                  08-15-2000 (3.50.B)
C                  08-12-2001 (4.00)
C                  05-27-2003 (4.50)
C                  02-15-2005 (5.00)   
C                  10-25-2005 (5.10)
C                  10-30-2006 (5.20)
C                  02-20-2010 (5.30)
C
C  =====================================================================
C
C
C--There appears to be a bug when PERCEL (ADV input file) is set    !edm
C--to something that causes sub-time stepping.  Mass is not         !edm
C--conserved in this scenario and may need to be addressed before   !edm
C--code is distributed.                                             !edm
C
      USE MT3DMS_MODULE, ONLY: MXTRNOP,MXSTP,
     &                         FPRT,INBTN,ICBUND,CADV,COLD,RETA,PRSITY,
     &                         DZ,DH,QX,QY,QZ,A,RHOB,SP1,SP2,SRCONC,
     &                         RC1,RC2,PRSITY2,RETA2,FRAC,CNEW,IOUT,
     &                         iUnitTRNOP,IMPSOL,ISPD,MIXELM,NPER,NSTP,
     &                         TSLNGH,ISS,NPERFL,MXSTRN,NCOMP,MCOMP,
     &                         ISOTHM,UPDLHS,MXITER,FMNW,HT1,HT2,DTRANS,
     &                         DELT,TIME1,TIME2,NPS,
     &                         MEMDEALLOCATE,
     &                         PRSITYSAV,SATOLD                     !edm
C
      IMPLICIT  NONE
      INTEGER iNameFile,KPER,KSTP,N,ICOMP,ICNVG,ITO,ITP,IFLEN
      CHARACTER FLNAME*50
      REAL START_TIME,TOTAL_TIME,
     &     END_TIME
      LOGICAL existed
      CHARACTER,PARAMETER :: VID*30='[Version 5.40-beta 07/27/2010]'
C
C--Get CPU time at the start of simulation
      Call CPU_TIME(start_time)
C
C--WRITE AN IDENTIFIER TO SCREEN
      WRITE(*,101) VID
  101 FORMAT(1X,'MT3DMS - Modular 3-D Multi-Species Transport Model ',
     & A14/1X,'Developed at University of Alabama',
     & ' for U.S. Department of Defense'/)
C
C--INITIALIZE CHARACTER VARIABLES
      FLNAME=' '
C
C--The following statement should be uncommented in order to use
C--GETCL to retrieve a command line argument.  The call to GETCL may
C--be commented out for compilers that do not support it.
      !CALL GETCL(FLNAME)
      CALL GETARG(1,FLNAME)
C
C--Get Name of NAME File from Screen
      IF(FLNAME.EQ.' ') THEN
        write(*,102)
  102   format(1x,'Enter Name of the MT3DMS NAME File: ')
        read(*,'(a)') flname
      ENDIF
C
C-Open files using the Name File method as in MODFLOW-2000      
      iflen=index(flname,' ')-1
      inquire(file=flname(1:iflen),exist=existed)
      if(.not.existed) then
        flname=flname(1:iflen)//'.nam'
        inquire(file=flname(1:iflen+4),exist=existed)
        if(.not.existed) then
          write(*,103) flname(1:iflen),flname(1:iflen+4)
          call ustop(' ')
        endif
      endif
  103 format(1x,'STOP. Specified Name file does not exist: ',
     & a,' or ',a)
      WRITE(*,104) FLNAME
  104 FORMAT(1x,'Using NAME File: ',a)
      iNameFile=99
      OPEN(iNameFile,file=flname,status='old')
      CALL BTN5OPEN(iNameFile)
      CLOSE (iNameFile)
C
C--WRITE PROGRAM TITLE TO OUTPUT FILE
      WRITE(IOUT,11)
   11 FORMAT(/30X,71('+')/30X,'+',69X,'+'
     &  /30X,'+',28X,'   MT3DMS',32X,'+'
     &  /30X,'+',13X,'A Modular 3D Multi-Species Transport Model ',
     &           13X,'+'
     &  /30X,'+', 4X,'For Simulation of Advection, Dispersion and',
     &           ' Chemical Reactions',3X,'+'
     &  /30X,'+',16X,'of Contaminants in Groundwater Systems',15X,'+'
     &  /30X,'+',69X,'+'/30X,71('+')/)
C
C--DEFINE PROBLEM DIMENSION AND SIMULATION OPTIONS
      CALL BTN5AR(INBTN)
C
C--ALLOCATE STORAGE SPACE FOR DATA ARRAYS
      CALL FMI5AR()
      IF(iUnitTRNOP(1).GT.0) CALL ADV5AR(iUnitTRNOP(1))
      IF(iUnitTRNOP(2).GT.0) CALL DSP5AR(iUnitTRNOP(2))
      IF(iUnitTRNOP(3).GT.0) CALL SSM5AR(iUnitTRNOP(3))
      IF(iUnitTRNOP(4).GT.0) CALL RCT5AR(iUnitTRNOP(4))
      IF(iUnitTRNOP(5).GT.0) CALL GCG5AR(iUnitTRNOP(5))
      IF(iUnitTRNOP(11).GT.0) CALL TOB5AR(iUnitTRNOP(11))     
      IF(iUnitTRNOP(13).GT.0) CALL HSS5AR(iUnitTRNOP(13))
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
C
C--FOR EACH STRESS PERIOD***********************************************
      HT1=0.
      HT2=0.
      DTRANS=0.
      NPS=1
      DO KPER=1,NPER
C
C--WRITE AN INDENTIFYING MESSAGE
        WRITE(*,50) KPER
        WRITE(IOUT,51) KPER
        WRITE(IOUT,'(1X)')
   50   FORMAT(/1X,'STRESS PERIOD NO.',I5)
   51   FORMAT(//35X,62('+')/55X,'STRESS PERIOD NO.',I5.3/35X,62('+'))
C
C--GET STRESS TIMING INFORMATION
        CALL BTN5ST()
C
C--READ AND PREPARE INPUT INFORMATION WHICH IS CONSTANT
C--WITHIN EACH STRESS PERIOD
        IF(iUnitTRNOP(3).GT.0) CALL SSM5RP(KPER)
C
C--FOR EACH FLOW TIME STEP----------------------------------------------
        DO KSTP=1,NSTP
          DELT=TSLNGH(KSTP)
          HT1=HT2
          HT2=HT2+DELT
C
C--WRITE AN INDENTIFYING MESSAGE
          WRITE(*,60) KSTP,HT1,HT2
          WRITE(IOUT,61) KSTP,HT1,HT2
          WRITE(IOUT,'(1X)')
   60     FORMAT(/1X,'TIME STEP NO.',I5
     &     /1X,'FROM TIME =',G13.5,' TO ',G13.5/)
   61     FORMAT(//42X,48('=')/57X,'TIME STEP NO.',I5.3/42X,48('=')
     &     //1X,'FROM TIME =',G13.5,' TO ',G13.5)
C
C--READ AND PROCESS SATURATED THICKNESS, VELOCITY COMPONENTS
C--ACROSS CELL INTERFACES, AND SINK/SOURCE INFORMATION
C--(NOTE THAT THESE ITEMS ARE READ ONLY ONCE IF FLOW MODEL
C--IS STEADY-STATE AND HAS SINGLE STRESS PERIOD)
          IF(KPER*KSTP.GT.1.AND.ISS.NE.0.AND.NPERFL.EQ.1) GOTO 70
C
          CALL FMI5RP1(KPER,KSTP)
          IF(iUnitTRNOP(3).GT.0) CALL FMI5RP2(KPER,KSTP)
C
C--CALCULATE COEFFICIENTS THAT VARY WITH FLOW-MODEL TIME STEP
          IF(iUnitTRNOP(2).GT.0) CALL DSP5CF(KSTP,KPER)
C
   70     CONTINUE
C
C--FOR EACH TRANSPORT STEP..............................................
          TIME2=HT1
          DO N=1,MXSTRN
C
C--ADVANCE ONE TRANSPORT STEP
            CALL BTN5AD(N,TIME1,TIME2,HT2,DELT,KSTP,KPER,DTRANS,NPS)
C
C--FOR EACH COMPONENT......
            DO ICOMP=1,NCOMP
C
C--SOLVE TRANSPORT TERMS WITH EXPLICIT SCHEMES
              IF(MIXELM.EQ.0) GOTO 1500
C
C--FORMULATE AND SOLVE
              CALL BTN5SV(ICOMP)
              IF(iUnitTRNOP(1).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL ADV5SV(ICOMP,DTRANS)
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
C
C--FOR EACH OUTER ITERATION...
              DO ITO=1,MXITER
C
C--UPDATE COEFFICIENTS THAT VARY WITH ITERATIONS
                IF(iUnitTRNOP(4).GT.0.AND.ISOTHM.GT.1)
     &           CALL RCT5CF(ICOMP,DTRANS)
C
C--FORMULATE MATRIX COEFFICIENTS
                CALL BTN5FM(ICOMP,ICBUND,CADV,COLD,RETA,PRSITY,DH,
     &                      DTRANS,PRSITYSAV,SATOLD)                !edm
                IF(iUnitTRNOP(1).GT.0.AND.MIXELM.EQ.0 
     &           .AND. ICOMP.LE.MCOMP)
     &           CALL ADV5FM(ICOMP,ICBUND,DH,QX,QY,QZ,A)
                IF(iUnitTRNOP(2).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL DSP5FM(ICOMP,ICBUND,A,COLD)
                IF(iUnitTRNOP(3).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL SSM5FM(ICOMP)
                IF(iUnitTRNOP(13).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL HSS5FM(ICOMP,ICBUND,time1,time2)
                IF(iUnitTRNOP(4).GT.0) 
     &           CALL RCT5FM(ICOMP,ICBUND,PRSITY,DH,RHOB,SP1,SP2,SRCONC,
     &                  RC1,RC2,PRSITY2,RETA2,FRAC,DTRANS)
                IF(iUnitTRNOP(5).GT.0) 
     &           CALL GCG5AP(IOUT,ITO,ITP,ICNVG,N,KSTP,KPER,TIME2,
     &                       HT2,ICBUND(:,:,:,ICOMP),CNEW(:,:,:,ICOMP))
C
C--IF CONVERGED, GO TO NEXT OUTER ITERATION
                  IF(ICNVG.EQ.1) GOTO 110
C
C--END OF OUTER ITERATION LOOP
              ENDDO
  110         CONTINUE
C
C--END OF COMPONENT LOOP
            ENDDO
C
C--CALCULATE MASS BUDGETS AND SAVE RESULTS FOR ALL COMPONENTS
            DO ICOMP=1,NCOMP
C
C--CALCULATE MASS BUDGETS FOR IMPLICIT SCHEMES
C
              IF(iUnitTRNOP(1).GT.0.AND.MIXELM.EQ.0 
     &         .AND. ICOMP.LE.MCOMP)
     &         CALL ADV5BD(ICOMP,DTRANS)
              IF(iUnitTRNOP(2).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL DSP5BD(ICOMP,DTRANS)
              IF(iUnitTRNOP(3).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL SSM5BD(ICOMP,DTRANS)
              IF(iUnitTRNOP(13).GT.0 .AND. ICOMP.LE.MCOMP) 
     &         CALL HSS5BD(ICOMP,ICBUND,50,time1,time2,DTRANS)     
              IF(iUnitTRNOP(4).GT.0) 
     &         CALL RCT5BD(ICOMP,DTRANS)
C
C--CALCULATE GLOBAL MASS BUDGETS AND CHECK MASS BALANCE
              CALL BTN5BD(ICOMP,DTRANS)
C
C--SAVE OUTPUTS
              CALL BTN5OT(KPER,KSTP,N,ICOMP,TIME2)   
              IF(FMNW) CALL SSM5OT(KPER,KSTP,N,TIME2)
              IF(iUnitTRNOP(11).GT.0) CALL TOB5OT(KPER,KSTP,N,
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
     &       ' SPECIFIED MAXIMUM (MXSTRN) =',I10)
            CALL USTOP(' ')
          ENDIF
  900     CONTINUE
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
C--DEALLOCATE MEMORY
      CALL MEMDEALLOCATE()
C
C--Get CPU time at the end of simulation
C--and print out total elapsed time in seconds
      Call CPU_TIME(end_time)
      total_time = end_time - start_time
      Write(*,2010) int(total_time/60.),mod(total_time,60.)
C      Write(IOUT,2010) int(total_time/60.),mod(total_time,60.)
 2010 FORMAT(/1X,'Program completed.   ',
     & 'Total CPU time:',i5.3,' minutes ',f6.3,' seconds')
C
      STOP
      END