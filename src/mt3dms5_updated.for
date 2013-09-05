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
      USE RCTMOD                                                    !# LINE 57 MAIN
      USE MIN_SAT                                                   !# LINE 58 MAIN
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
     &                         INRTR,INTSO,INRTR,INLKT,INSFT,       !# V
     &                         IWCTS,IREACTION,IALTFM,NOCREWET,     !# V
     &                         NODES,SAVUCN,NLAY,NROW,NCOL,COLDFLW          !# Amended
C
      IMPLICIT  NONE
      INTEGER iNameFile,KPER,KSTP,N,ICOMP,ICNVG,ITO,ITP,IFLEN
      CHARACTER FLNAME*50
      CHARACTER COMLIN*200                              !# LINE 115 MAIN
      CHARACTER CMST*3                                  !# LINE 116 MAIN
      CHARACTER CDRY*3                                  !# LINE 116 MAIN
      INTEGER II,NN,I,J,K,OperFlag,IEDEA                !# LINE 117 MAIN (and amended)
      REAL DT00                                         !# LINE 119 MAIN
      REAL START_TIME,TOTAL_TIME,
     &     END_TIME
      LOGICAL existed
      CHARACTER,PARAMETER :: VID*30='[Version 5.40-beta 07/27/2010]'
C
C--ALLOCATE LOGICALS
      ALLOCATE(DOMINSAT,DRYON)                                 !# NEW
      DOMINSAT=.FALSE.                                         !# LINE 174 MAIN
      DRYON=.FALSE.                                            !# LINE 175 MAIN
C
C--Initialize variables 
      ALLOCATE(IREACTION,IALTFM,NOCREWET)
      IWCTS=1
      IREACTION=0
      IALTFM=0
      NOCREWET=0
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
      CALL GETARG(1,COMLIN)                                    !# LINE 170 MAIN
cvsb                                                           !# LINE 171 MAIN
      CALL GETARG(2,CMST)                                      !# LINE 172 MAIN
      CALL GETARG(3,CDRY)                                      !# LINE 173 MAIN
      IF(CMST.EQ.'MST'.OR.CMST.EQ.'mst') DOMINSAT=.TRUE.       !# LINE 176 MAIN
      IF(CDRY.EQ.'DRY'.OR.CDRY.EQ.'dry') DRYON=.TRUE.          !# LINE 177 MAIN
      IF(DOMINSAT.EQ..FALSE.) DRYON=.FALSE.                    !# LINE 178 MAIN
c      CALL GETCL(FLNAME)                                      !# LINE 179 MAIN
C                                                              !# LINE 180 MAIN
      IF(COMLIN.NE.' ') THEN                                   !# LINE 181 MAIN
        flname=COMLIN                                          !# LINE 182 MAIN
      ELSE                                                     !# LINE 183 MAIN
C--Get Name of NAME File from Screen
        IF(FLNAME.EQ.' ') THEN
          write(*,102)
  102     format(1x,'Enter Name of the MT3DMS NAME File: ')
          read(*,'(a)') flname
        ENDIF
      ENDIF                                                    !# NEW
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
      IF(iUnitTRNOP(6).GT.0) CALL CTS5AR(iUnitTRNOP(6))        !# LINE 263 MAIN
      IF(iUnitTRNOP(7).GT.0) CALL UZT5AR(iUnitTRNOP(7))
      IF(iUnitTRNOP(11).GT.0) CALL TOB5AR(iUnitTRNOP(11))     
      IF(iUnitTRNOP(13).GT.0) CALL HSS5AR(iUnitTRNOP(13))
      IF(iUnitTRNOP(18).GT.0) CALL LKT5AR(iUnitTRNOP(18))      !# LINE 280 MAIN
      IF(iUnitTRNOP(19).GT.0) CALL SFT5AR(iUnitTRNOP(19))      !# LINE 282 MAIN
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
      IF(iUnitTRNOP(18).GT.0) CALL LKT5RP(KPER)
      IF(iUnitTRNOP(19).GT.0) CALL SFT5RP(KPER)
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
        CALL BTN5ST(KPER)                                      !# Amended (LINE 850 BTN)
C
C--READ AND PREPARE INPUT INFORMATION WHICH IS CONSTANT
C--WITHIN EACH STRESS PERIOD
        IF(iUnitTRNOP(3).GT.0) CALL SSM5RP(KPER)
        IF(iUnitTRNOP(6).GT.0) CALL CTS5RP(KPER)               !# LINE 418 MAIN
        IF(iUnitTRNOP(7).GT.0) CALL UZT5RP(KPER)
C--READ LAK AND SFR BOUNDARY CONDITIONS                        !# LINE 427 MAIN
        IF(iUnitTRNOP(18).GT.0) CALL LKT5SS(KPER)              !# LINE 429 MAIN
        IF(iUnitTRNOP(19).GT.0) CALL SFT5SS(KPER)              !# LINE 431 MAIN
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
          IF(iUnitTRNOP(19).GT.0) CALL SFT5AD2(N)              !# LINE 454 MAIN
C
          CALL FMI5RP1(KPER,KSTP)
          IF(iUnitTRNOP(3).GT.0) CALL FMI5RP2(KPER,KSTP)
C
          IF(DRYON) CALL ADVQC7RP(KPER,KSTP)                   !# LINE 467 MAIN
          IF(IALTFM.EQ.1) COLDFLW=COLD
C                                                              !# LINE 467 MAIN
          IF(iUnitTRNOP(19).GT.0.AND.ISFSOLV.GT.0) THEN        !# LINE 468 MAIN
            CALL FILLIASFJASF()                                !# LINE 469 MAIN
            IF(KPER*KSTP.EQ.1) CALL XMD7AR()                   !# LINE 470 MAIN
          ENDIF                                                !# LINE 471 MAIN
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
C--UPDATE CONCENTRATIONS OF LAKE VOLUMES                       !# LINE 506 MAIN
            IF(iUnitTRNOP(18).GT.0) CALL LKT5AD(N)             !# LINE 507 MAIN
            IF(iUnitTRNOP(19).GT.0) CALL SFT5AD(N)             !# LINE 508 MAIN
C
C
C--FOR EACH COMPONENT......
            DO ICOMP=1,NCOMP
C
C--TAKE CARE OF Fe2+                                           !# LINE 513 MAIN
              IF(IREACTION.EQ.2) THEN                          !# LINE 514 MAIN
                IF(ICOMP==NCOMP.AND.IFESLD>0)GOTO 1001         !# LINE 515 MAIN
              ENDIF                                            !# LINE 516 MAIN
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
              IF(IALTFM.EQ.2) UPDLHS=.TRUE.
              IF(iUnitTRNOP(6).GT.0 .OR.                       !# LINE 545 MAIN
     1           iUnitTRNOP(18).GT.0 .OR.                      !# LINE 546 MAIN
     1           iUnitTRNOP(19).GT.0) UPDLHS=.TRUE.            !# LINE 547 MAIN
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
     &                      DTRANS,PRSITYSAV,SATOLD,HT2,TIME2)                !edm
                IF(iUnitTRNOP(1).GT.0.AND.MIXELM.EQ.0 
     &           .AND. ICOMP.LE.MCOMP)
     &           CALL ADV5FM(ICOMP,ICBUND,DH,QX,QY,QZ,A)
                IF(iUnitTRNOP(2).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL DSP5FM(ICOMP,ICBUND,A,CNEW)
                IF(iUnitTRNOP(3).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL SSM5FM(ICOMP)
                IF(iUnitTRNOP(7).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL UZT5FM(ICOMP)
                IF(iUnitTRNOP(13).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL HSS5FM(ICOMP,ICBUND,time1,time2)
                IF(iUnitTRNOP(6).GT.0 .AND. ICOMP.LE.MCOMP)    !# LINE 596 MAIN
     &           CALL CTS5FM(ICOMP)                            !# LINE 597 MAIN
                IF(iUnitTRNOP(1).GT.0.AND.MIXELM.EQ.0          !# LINE 609 MAIN
     &           .AND. ICOMP.LE.MCOMP .AND. DRYON.EQ..TRUE.)   !# LINE 610 MAIN
     &           CALL ADVQC7FM(ICOMP)                          !# LINE 611 MAIN
                IF(iUnitTRNOP(4).GT.0) 
     &           CALL RCT5FM(ICOMP,ICBUND,PRSITY,DH,RHOB,SP1,SP2,SRCONC,
     &                  RC1,RC2,PRSITY2,RETA2,FRAC,DTRANS,
     &                  COLD,CNEW)                             !# LINE 620 MAIN
                IF(iUnitTRNOP(18).GT.0)                        !# LINE 622 MAIN
     &           CALL LKT5FM(ICOMP)                            !# LINE 623 MAIN
                IF(iUnitTRNOP(19).GT.0) !OR SWR OR MNW2 ETC..                        !# LINE 627 MAIN
     1           CALL SWF5FM(ICOMP)                            !# LINE 628 MAIN
                IF(iUnitTRNOP(5).GT.0)
     &            CALL GCG5AP(IOUT,ITO,ITP,ICNVG,N,KSTP,KPER,TIME2,
     &                        HT2,ICBUND(:,:,:,ICOMP),CNEW(:,:,:,ICOMP))
                IF(IREACTION.EQ.2.AND.ICOMP.LE.NED+NEA) THEN !# LINE 645 MAIN
                  IF(SPECIAL(ICOMP)=="MAXEC") THEN           !# LINE 646 MAIN
                    DO K=1,NLAY                              !# Modified
                      DO I=1,NROW                            !# Modified
                        DO J=1,NCOL                          !# Modified
                          IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE !# Modified
                          IF(CNEW(J,I,K,ICOMP).GT.MAXEC(ICOMP)) THEN !# Modified
                            CNEW=COLD                        !# Modified
                            !N=N-1                             !# Added by Vivek? (See email 2-22-13)
                            !CYCLE TRANSPORT TIME-STEP LOOP    !# Added by Vivek? (See email 2-22-13)
CEDM                  DO NN=1,NODES                            !# LINE 647 MAIN
CEDM                    IF(IX(LCIB+NN-1)<=0)CYCLE              !# LINE 648 MAIN
CEDM                    IF(X(LCCNEW+(ICOMP-1)*NODES+NN-1).GT.  !# LINE 649 MAIN
CEDM 1                     MAXEC(ICOMP))THEN                   !# LINE 650 MAIN
                          !concentration is over the maximum EFC !# LINE 651 MAIN
CVSB                          OperFlag=2                       !# LINE 652 MAIN 
                          !time1=time1-DTRANS                  !# LINE 653 MAIN
CVSB                          IF(PRTOUT)NPS=NPS-1              !# LINE 654 MAIN
CVSB                          TIME2=TIME1                      !# LINE 655 MAIN
                          !2-DTRANS                            !# LINE 656 MAIN
CVSB                          CALL TimeStep_adjust(OperFlag,   !# LINE 657 MAIN
CVSB     &                    X(LCCOLD+(ICOMP-1)*NODES+NN-1),  !# LINE 658 MAIN
CVSB     &                    X(LCCNEW+(ICOMP-1)*NODES+NN-1),MAXEC(ICOMP),  !# LINE 659 MAIN
CVSB     &                    DTRANS,ICOMP)                    !# LINE 660 MAIN
CVSB                          IF(DTRANS<DT0)THEN               !# LINE 661 MAIN
CVSB                            DT0=DTRANS                     !# LINE 662 MAIN
CVSB                          ENDIF                            !# LINE 663 MAIN
CEDM                      X(LCCNEW:LCCNEW+NCOMP*NODES-1)=      !# LINE 664 MAIN
CEDM &                    X(LCCOLD:LCCOLD+NCOMP*NODES-1)       !# LINE 665 MAIN
CEDM                      DO III=0,NCOMP*NODES-1               !# LINE 666 MAIN
CEDM                        X(LCCNEW+III)=X(LCCOLD+III)        !# LINE 667 MAIN
CEDM                      ENDDO                                !# LINE 668 MAIN
CVSB                          N=N-1                            !# LINE 669 MAIN
CVSB                          cycle mstrans_loop               !# LINE 670 MAIN
CEDM                    ELSEIF(X(LCCNEW+(ICOMP-1)*NODES+NN-1)- !# LINE 671 MAIN
CEDM &                        MAXEC(ICOMP)<1.E-6)THEN          !# LINE 672 MAIN
CVSB                          ! restore the default time step for mass transport  !# LINE 673 MAIN
CVSB                          IF(DT0<DT00)DT0=DT00             !# LINE 674 MAIN
CEDM                    ENDIF                                  !# LINE 675 MAIN
CEDM                  ENDDO                                    !# LINE 676 MAIN
CEDM                ENDIF                                      !# LINE 677 MAIN
CEDM              ENDIF                                        !# LINE 678 MAIN
                          ENDIF
                        ENDDO
                      ENDDO  
                    ENDDO
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
C-------------TAKE CARE OF Fe2+                                    !# LINE 687 MAIN
 1001         IF(IREACTION.EQ.2) THEN                              !# LINE 688 MAIN
                IF(ICOMP.EQ.NCOMP.AND.IFESLD.GT.0) THEN            !# Modified
                  DO IEDEA=1,NED+NEA                               !# Modified
                    IF(SPECIAL(IEDEA)=="SOLID") THEN               !# Modified
                      DO K=1,NLAY                                  !# Modified
                        DO I=1,NROW                                !# Modified
                          DO J=1,NCOL                              !# Modified
                            NN=(K-1)*NCOL*NROW+(I-1)*NCOL+J        !# Modified
                            IF(ICBUND(J,I,K,1).LE.0) CYCLE         !# Modified
                            MAXEC(IEDEA)=COLD(J,I,K,ICOMP)*        !# Modified
     &                        RHOB(J,I,K)/PRSITY(J,I,K)            !# Modified
                            DO II=1,NED                            !# Modified
                              MAXEC(IEDEA)=MAXEC(IEDEA)-           !# Modified
     &                          DCDT_FE(NN,IEDEA-NED,II)*          !# Modified
     &                          DTRANS*COLD(J,I,K,II)              !# Modified
                            ENDDO                                  !# Modified
                            CNEW(J,I,K,ICOMP)=MAXEC(IEDEA)*        !# Modified
     &                          PRSITY(J,I,K)/RHOB(J,I,K)          !# Modified
                            IF(CNEW(J,I,K,ICOMP).LT.0.)            !# Modified
     &                          CNEW(J,I,K,ICOMP)=0.0              !# Modified
                          ENDDO                                    !# Modified
                        ENDDO                                      !# Modified
                      ENDDO                                        !# Modified
                    ENDIF                                          !# Modified
                  ENDDO                                            !# Modified
                ENDIF                                              !# Modified
              ENDIF                                                !# Modified
CEDM            IF(ICOMP==NCOMP.AND.IFESLD>0)THEN                  !# LINE 689 MAIN
CEDM              DO I=1,NED+NEA                                   !# LINE 690 MAIN
CEDM                IF(SPECIAL(I)=="SOLID")THEN                    !# LINE 691 MAIN
CEDM                    DO NN=1,NODES                              !# LINE 692 MAIN
CEDM                      IF(IX(LCIB+NN-1)<=0)CYCLE                !# LINE 693 MAIN
CEDM                      MAXEC(I)=X(LCCOLD+(NCOMP-1)*NODES+NN-1)* !# LINE 694 MAIN
CEDM &                       X(LCRHOB+NN-1)/X(LCPR+NN-1)           !# LINE 695 MAIN
CEDM                      DO II=1,NED                              !# LINE 696 MAIN
CEDM                       MAXEC(I)=MAXEC(I)-DCDT_FE(NN,I-NED,II)* !# LINE 697 MAIN
CEDM &                       DTRANS*X(LCCOLD+(II-1)*NODES+NN-1)    !# LINE 698 MAIN
CEDM                      ENDDO                                    !# LINE 699 MAIN
CEDM                      X(LCCNEW+(ICOMP-1)*NODES+NN-1)=MAXEC(I)  !# LINE 700 MAIN
CEDM &                     *X(LCPR+NN-1)/X(LCRHOB+NN-1)            !# LINE 701 MAIN
CEDM                      IF(X(LCCNEW+(ICOMP-1)*NODES+NN-1)<0.)    !# LINE 702 MAIN
CEDM &                        X(LCCNEW+(ICOMP-1)*NODES+NN-1)=0.0   !# LINE 703 MAIN
CEDM                    ENDDO                                      !# LINE 704 MAIN
CEDM                ENDIF                                          !# LINE 705 MAIN
CEDM              ENDDO                                            !# LINE 706 MAIN
CEDM            ENDIF                                              !# LINE 707 MAIN
CEDM          ENDIF                                                !# LINE 708 MAIN
C                                                                  !# LINE 709 MAIN
C--END OF COMPONENT LOOP
            ENDDO
C
C--APPLY ED/EA REACTION AS A FLASH CALCULATION                 !# LINE 713 MAIN
            IF(IREACTION.EQ.1) THEN                            !# LINE 714 MAIN
              CALL FLASHREACT(ICOMP)                           !# LINE 715 MAIN (Modified)
            ENDIF                                              !# LINE 716 MAIN
C                                                              !# LINE 717 MAIN
C--CALCULATE MASS BUDGETS AND SAVE RESULTS FOR ALL COMPONENTS
            DO ICOMP=1,NCOMP
C
C--CALCULATE MASS BUDGETS FOR IMPLICIT SCHEMES
C
              IF(iUnitTRNOP(1).GT.0.AND.MIXELM.EQ.0 
     &         .AND. ICOMP.LE.MCOMP)
     &         CALL ADV5BD(ICOMP,DTRANS,N,KPER,KSTP)           !# Amended (LINE 728 MAIN)
              IF(iUnitTRNOP(2).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL DSP5BD(ICOMP,DTRANS)
              IF(iUnitTRNOP(3).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL SSM5BD(ICOMP,DTRANS)
              IF(iUnitTRNOP(7).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL UZT5BD(ICOMP,DTRANS)
              IF(iUnitTRNOP(13).GT.0 .AND. ICOMP.LE.MCOMP) 
     &         CALL HSS5BD(ICOMP,ICBUND,50,time1,time2,DTRANS)     
              IF(iUnitTRNOP(6).GT.0 .AND. ICOMP.LE.MCOMP)      !# LINE 745 MAIN
     &         CALL CTS5BD(KSTP,KPER,ICOMP,DTRANS,N)           !# LINE 746 MAIN
              IF(iUnitTRNOP(1).GT.0.AND.MIXELM.EQ.0            !# LINE 761 MAIN
     &           .AND. ICOMP.LE.MCOMP .AND. DRYON.EQ..TRUE.)   !# LINE 762 MAIN
     &           CALL ADVQC7BD(ICOMP,DTRANS)                   !# LINE 763 MAIN
C
              IF(iUnitTRNOP(4).GT.0) 
     &         CALL RCT5BD(ICOMP,DTRANS)
C
              IF(iUnitTRNOP(18).GT.0)                          !# LINE 775 MAIN
     1          CALL LKT5BD(ICOMP,KPER,KSTP,DTRANS,N)          !# LINE 776 MAIN
              IF(iUnitTRNOP(19).GT.0)                          !# LINE 780 MAIN
     1          CALL SFT5BD(ICOMP,KPER,KSTP,DTRANS,N)          !# LINE 781 MAIN
C
C--CALCULATE GLOBAL MASS BUDGETS AND CHECK MASS BALANCE
              CALL BTN5BD(ICOMP,DTRANS,TIME2,HT2)
C
C--STORE ADDITIONAL MASS AND RESET CONC TO MAX EXPRESSED FIELD CAPACITY !# LINE 792 MAIN
              IF(IREACTION.EQ.2) THEN                          !# LINE 793 MAIN
                IF(ICOMP<=NED+NEA)THEN                         !# LINE 794 MAIN
                  IF(SPECIAL(ICOMP)=="STORE")THEN              !# LINE 795 MAIN
                    CALL Stor_Add_Methane(CNEW(:,:,:,ICOMP),   !# LINE 796 MAIN
     &                                    ICOMP,DTRANS)        !# LINE 796 MAIN
                  ENDIF                                        !# LINE 799 MAIN
                ENDIF                                          !# LINE 800 MAIN
              ENDIF                                            !# LINE 801 MAIN
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
C--CLOSE FILES                                                 !# LINE 858 MAIN
      IF(IREACTION.EQ.2) THEN                                  !# LINE 859 MAIN
        IF(NSTORE.NE.0 .and. SAVUCN)THEN                       !# LINE 860 MAIN
          CLOSE(IUMETH)                                        !# LINE 861 MAIN
        ENDIF                                                  !# LINE 862 MAIN
      ENDIF                                                    !# LINE 863 MAIN
C--DEALLOCATE MEMORY
      CALL MEMDEALLOCATE()
      CALL MEMDEALLOCATE2()
      CALL MEMDEALLOCATE4()
      CALL MEMDEALLOCATE5()
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