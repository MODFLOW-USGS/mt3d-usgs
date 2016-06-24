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
C--SET MAXIMUM ARRAY DIMENSIONS
C--MXTRNOP: MAXIMUM NUMBER OF TRANSPORT OPTIONS (PACKAGES)
C--MXPRS:   MAXIMUM NUMBER OF TIMES AT WHICH RESULTS ARE SAVED
C--MXSTP:   MAXIMUM NUMBER OF TIME STEPS IN FLOW MODEL
C--MXOBS:   MAXIMUM NUMBER OF OBSERVATION POINTS
C--MXCOMP:  MAXIMUM NUMBER OF CHEMICAL COMPONENTS
C  =====================================================================
C
      IMPLICIT  NONE
      CHARACTER,PARAMETER :: VID*14='[Version 5.30]'
      INTEGER,PARAMETER :: MXTRNOP=50,MXCOMP=100,
     &                     MXPRS=1000,MXSTP=1000,MXOBS=200     
      INTEGER   IX,ISUMX,ISUMIX,ISUM,ISUM2,NCOL,NROW,NLAY,NCOMP,MCOMP,
     &          INBTN,INADV,INDSP,INSSM,INRCT,INGCG,INTOB,INHSS,INFTL,
     &          IOUT,ICNF,IUCN,IUCN2,IOBS,IMAS,ICBM,ISSGOUT,
     &          LCLAYC,LCDELR,LCDELC,LCDZ,LCPR,LCXBC,LCYBC,LCZBC,LCQX,
     &          LCQY,LCQZ,LCDH,LCIB,LCCOLD,LCCNEW,LCCADV,LCRETA,LCBUFF,
     &          MIXELM,MXPART,LCXP,LCYP,LCZP,LCCNPT,LCCHEK,
     &          NCOUNT,NPINS,NRC,LCAL,LCTRPT,LCTRPV,LCDM,LCDXX,LCDXY,
     &          LCDXZ,LCDYX,LCDYY,LCDYZ,LCDZX,LCDZY,LCDZZ,LCSSMC,
     &          LCIRCH,LCRECH,LCCRCH,LCIEVT,LCEVTR,LCCEVT,MXSS,LCSS,
     &          LCSSG,ISOTHM,IREACT,LCRHOB,LCPRSITY2,LCFRAC,LCRETA2,
     &          LCSP1,LCSP2,LCRC1,LCRC2,INTERP,
     &          ISEED,ITRACK,NPL,NPH,NPMIN,NPMAX,NPLANE,NLSINK,NPSINK,
     &          NPRS,NOBS,LOCOBS,NSS,KSTP,KPER,NTSS,I,N,NPS,
     &          IFMTCN,IFMTNP,IFMTRF,IFMTDP,MXSTRN,
     &          NPER,NSTP,ISTAT,LCQSTO,LCHTOP,LCCWGT,LCSR,
     &          LCINDX,LCINDY,LCINDZ,ISS,IVER,NPROBS,NPRMAS,IRCTOP,
     &          MXITER,IPRGCG,NADVFD,ITP,NODES,ICNVG,ITER1,ITO,
     &          ISOLVE,LCA,LCQ,LCWK,LCCNCG,LCLRCH,LCRHS,
     &          IMPSOL,NCRS,ISPD,IGETSC,L,INDEX,ICOMP,NPERFL,IERR
      INTEGER   iNameFile,iFLen,IC,iFTLfmt,iUnitTRNOP,
     &          MaxConcObs,MaxFluxObs,MaxFluxCells,inSaveObs,
     &          LCMLAYER,LCCOBS,LCPRLAYER,LCTEMP,LCFLUXGROUP,
     &          LCGROUPDATA,InConcObs,nConcObs,iOutCobs,iConcLOG,
     &          iConcINTP,inFluxObs,nFluxGroup,nFluxObs,iOutFlux,
     &          iSSTrans,MaxHSSSource,MaxHSSCells,MaxHSSStep,
     &          LCHSSData,LCHSSLoc,iHSSLoc,nHSSSource,iRunHSSM
      REAL      X,TIMPRS,TSLNGH,PERCEL,HORIGN,XMAX,YMAX,ZMAX,CINACT,
     &          TMASIO,RMASIO,DCEPS,SRMULT,WD,DCHMOC,HT1,HT2,TIME1,
     &          TIME2,DT0,DELT,DTRACK,DTDISP,DTRANS,THKMIN,
     &          DTSSM,DTRCT,DTRACK2,RFMIN,TMASS,ACCL,CCLOSE,
     &          TTSMULT,TTSMAX,TMASIN,TMASOT,ERROR,ERROR2,
     &          start_time,end_time,total_time,CScale,FScale,
     &          faclength,factime,facmass
      LOGICAL   UNIDX,UNIDY,UNIDZ,SAVUCN,SAVCBM,CHKMAS,
     &          FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,PRTOUT,UPDLHS,EXISTED,
     &          FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,
     &          FSWT,FSFR,FUZF
      CHARACTER FLNAME*50,FINDEX*30,TUNIT*4,LUNIT*4,MUNIT*4,FPRT*1,
     &          LINE*80,NameTRNOP*4,cobsnam*12,fobsnam*12,HSSNAM*12
      DIMENSION X(:),IX(:),cobsnam(:),fobsnam(:),HSSNAM(:),
     &          TIMPRS(MXPRS),TSLNGH(MXSTP),LOCOBS(3,MXOBS),
     &          NCOUNT(MXCOMP),NPINS(MXCOMP),NRC(MXCOMP),
     &          TMASIO(122,2,MXCOMP),RMASIO(122,2,MXCOMP),
     &          TMASS(4,3,MXCOMP),TMASIN(MXCOMP),TMASOT(MXCOMP),
     &          ERROR(MXCOMP),ERROR2(MXCOMP),
     &          NameTRNOP(MXTRNOP),iUnitTRNOP(MXTRNOP)
      ALLOCATABLE :: X,IX,cobsnam,fobsnam,HSSNAM      
      COMMON   /PD/HORIGN,XMAX,YMAX,ZMAX,UNIDX,UNIDY,UNIDZ
      COMMON   /FC/FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,
     &             FIBS,FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
      COMMON   /OC/IFMTCN,IFMTNP,IFMTRF,IFMTDP,SAVUCN,
     &             SAVCBM,CHKMAS,NPRMAS
      COMMON   /AD/PERCEL,ITRACK,WD,ISEED,DCEPS,NPLANE,NPL,NPH,
     &             NPMIN,NPMAX,SRMULT,INTERP,NLSINK,NPSINK,DCHMOC
      COMMON   /GCGIDX/L(19)
      COMMON   /FTL/iFTLfmt            
      DATA NameTRNOP/'ADV ', 'DSP ', 'SSM ', 'RCT ', 'GCG ',
     &               '    ', '    ', '    ', '    ', '    ',               
     &               'TOB ', '    ', 'HSS ', '    ', '    ',
     &               '    ', '    ', '    ', '    ', '    ',
     &            30*'    '/
      DATA INBTN/1/,  INFTL/10/, IOUT/16/,
     &     INADV/2/,  INDSP/3/,  INSSM/4/,  INRCT/8/,  INGCG/9/,  
     &     INTOB/12/, INHSS/13/, ICNF/17/, 
     &     IUCN/200/, IUCN2/300/,IOBS/400/, IMAS/600/, ICBM/800/
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
      FPRT=' '
C
C--The following statement should be uncommented in order to use
C--GETCL to retrieve a command line argument.  The call to GETCL may
C--be commented out for compilers that do not support it.
      CALL GETCL(FLNAME)
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
      CALL BTN5OPEN(iNameFile,IOUT,INBTN,INADV,INDSP,INSSM,INRCT,
     & INGCG,INTOB,INHSS,INFTL,FPRT,MXTRNOP,iUnitTRNOP,NameTRNOP)
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
      CALL BTN5DF(INBTN,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,NPER,
     & NCOMP,MCOMP,MXTRNOP,iUnitTRNOP,NameTRNOP,
     & TUNIT,LUNIT,MUNIT,NODES,MXCOMP,iNameFile)
      IF(FPRT.EQ.' ') FPRT='N'
C
C--ALLOCATE STORAGE SPACE FOR DATA ARRAYS
      CALL BTN5AL(INBTN,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,NCOMP,
     & LCLAYC,LCDELR,LCDELC,LCHTOP,LCDZ,LCPR,LCXBC,LCYBC,LCZBC,
     & LCQX,LCQY,LCQZ,LCQSTO,LCDH,LCIB,LCCOLD,LCCNEW,LCCWGT,
     & LCCADV,LCRETA,LCSR,LCBUFF,ISOTHM,LCRHOB,LCPRSITY2,LCRETA2)
      CALL FMI5AL(INFTL,IOUT,MXTRNOP,iUnitTRNOP,NPERFL,ISS,IVER)
      IF(iUnitTRNOP(1).GT.0) 
     & CALL ADV5AL(iUnitTRNOP(1),IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & MCOMP,MIXELM,MXPART,PERCEL,NADVFD,LCXP,LCYP,LCZP,
     & LCINDX,LCINDY,LCINDZ,LCCNPT,LCCHEK)
      IF(iUnitTRNOP(2).GT.0) 
     & CALL DSP5AL(iUnitTRNOP(2),IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & MCOMP,LCAL,LCTRPT,LCTRPV,LCDM,LCDXX,LCDXY,LCDXZ,
     & LCDYX,LCDYY,LCDYZ,LCDZX,LCDZY,LCDZZ)
      IF(iUnitTRNOP(3).GT.0) 
     & CALL SSM5AL(iUnitTRNOP(3),IOUT,ISSGOUT,ISUM,ISUM2,
     & NCOL,NROW,NLAY,NCOMP,LCIRCH,LCRECH,LCCRCH,LCIEVT,LCEVTR,
     & LCCEVT,MXSS,LCSS,IVER,LCSSMC,LCSSG)
      IF(iUnitTRNOP(4).GT.0) 
     & CALL RCT5AL(iUnitTRNOP(4),IOUT,ISUM,ISUM2,
     & NCOL,NROW,NLAY,NCOMP,ISOTHM,IREACT,IRCTOP,IGETSC,LCRHOB,
     & LCPRSITY2,LCRETA2,LCFRAC,LCSP1,LCSP2,LCRC1,LCRC2)
      IF(iUnitTRNOP(5).GT.0) 
     & CALL GCG5AL(iUnitTRNOP(5),IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & MXITER,ITER1,NCRS,ISOLVE,LCA,LCQ,LCWK,LCCNCG,LCLRCH,LCRHS)          
      IF(iUnitTRNOP(11).GT.0) THEN        
        CALL TOB5AL(iUnitTRNOP(11),IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     &   MaxConcObs,MaxFluxObs,MaxFluxCells,LCMLAYER,LCCOBS,
     &   LCPRLAYER,LCTEMP,LCFLUXGROUP,LCGROUPDATA)     
        ALLOCATE (cobsnam(MaxConcObs),fobsnam(MaxFluxObs),stat=ierr)
        IF(IERR.NE.0) THEN  
          WRITE(*,105)
  105     FORMAT(1X,'ERROR ALLOCATING MEMORY FOR COBSNAM/FOBSNAM')
          CALL USTOP(' ')
        ENDIF  
      ENDIF     
      IF(iUnitTRNOP(13).GT.0) THEN
        CALL HSS5AL(iUnitTRNOP(13),IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     &   MaxHSSSource,MaxHSSCells,MaxHSSStep,
     &   LCHSSData,LCHSSLoc,iRunHSSM)
        ALLOCATE (HSSNAM(MaxHSSSource),stat=ierr)
        IF(IERR.NE.0) THEN
          WRITE(*,205)
  205     FORMAT(1X,'ERROR ALLOCATING MEMORY FOR HSSNAM')
          CALL USTOP(' ')
        ENDIF
      ENDIF                              
C
C--CHECK WHETHER ARRAYS X AND IX ARE DIMENSIONED LARGE ENOUGH.
C--IF NOT STOP
      ISUMX=ISUM                        
      ISUMIX=ISUM2         
      WRITE(IOUT,20) ISUMX,ISUMIX
   20 FORMAT(1X,42('.')/1X,'ELEMENTS OF THE  X ARRAY USED =',I10,
     & /1X,'ELEMENTS OF THE IX ARRAY USED =',I10,
     & /1X,42('.')/)
C
      ALLOCATE (X(0:ISUMX),IX(0:ISUMIX),STAT=IERR)
      IF(IERR.NE.0) THEN
        WRITE(*,106) 
  106   FORMAT(1X,'STOP. NOT ENOUGH MEMORY')
        CALL USTOP(' ')
      ENDIF
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
C--INITILIZE ARRAYS.
      DO I=1,ISUMX
        X(I)=0.
      ENDDO
      DO I=1,ISUMIX
        IX(I)=0
      ENDDO
      DO IC=1,NCOMP
        DO I=1,122
          TMASIO(I,1,IC)=0.
          TMASIO(I,2,IC)=0.
        ENDDO
        DO I=1,4
          TMASS(I,1,IC)=0.
          TMASS(I,2,IC)=0.
          TMASS(I,3,IC)=0.
        ENDDO
      ENDDO
C
C--READ AND PREPARE INPUT DATA RELEVANT TO
C--THE ENTIRE SIMULATION
      CALL BTN5RP(INBTN,IOUT,IUCN,IUCN2,IOBS,IMAS,ICNF,ICBM,
     & NCOL,NROW,NLAY,NCOMP,ISOTHM,IX(LCLAYC),X(LCDELR),X(LCDELC),
     & X(LCHTOP),X(LCDZ),X(LCPR),IX(LCIB),X(LCCOLD),X(LCCNEW),
     & X(LCCADV),CINACT,THKMIN,X(LCXBC),X(LCYBC),X(LCZBC),
     & X(LCRETA),RFMIN,X(LCBUFF),MXPRS,NPRS,TIMPRS,
     & MXOBS,NOBS,NPROBS,LOCOBS,TUNIT,LUNIT,MUNIT)
      IF(iUnitTRNOP(1).GT.0) 
     & CALL ADV5RP(iUnitTRNOP(1),IOUT,NCOL,NROW,NLAY,
     & MCOMP,MIXELM,MXPART,NADVFD,NCOUNT)
      IF(iUnitTRNOP(2).GT.0) 
     & CALL DSP5RP(iUnitTRNOP(2),IOUT,NCOL,NROW,NLAY,MCOMP,
     & X(LCBUFF),X(LCAL),X(LCTRPT),X(LCTRPV),X(LCDM))
      IF(iUnitTRNOP(4).GT.0) 
     & CALL RCT5RP(iUnitTRNOP(4),IOUT,NCOL,NROW,NLAY,
     & NCOMP,IX(LCIB),X(LCCOLD),X(LCPR),ISOTHM,IREACT,IRCTOP,IGETSC,
     & X(LCRHOB),X(LCSP1),X(LCSP2),X(LCSR),X(LCRC1),X(LCRC2),X(LCRETA),
     & X(LCBUFF),X(LCPRSITY2),X(LCRETA2),X(LCFRAC),RFMIN,IFMTRF,DTRCT)
      IF(iUnitTRNOP(5).GT.0) 
     & CALL GCG5RP(iUnitTRNOP(5),IOUT,MXITER,
     & ITER1,ISOLVE,ACCL,CCLOSE,IPRGCG)     
      IF(iUnitTRNOP(11).GT.0) 
     & CALL TOB5RP(iUnitTRNOP(11),IOUT,NCOL,NROW,NLAY,
     & NCOMP,MaxConcObs,MaxFluxObs,MaxFluxCells,inConcObs,nConcObs,
     & CScale,iOutCobs,iConcLOG,iConcINTP,COBSNAM,
     & X(LCCOBS),IX(LCMLAYER),X(LCPRLAYER),X(LCTEMP),
     & inFluxObs,nFluxGroup,nFluxObs,FScale,
     & iOutFlux,inSaveObs,FOBSNAM,X(LCFLUXGROUP),X(LCGROUPDATA))
      IF(iUnitTRNOP(13).GT.0) 
     & CALL HSS5RP(iUnitTRNOP(13),IOUT,NCOL,NROW,NLAY,NCOMP,
     & IX(LCIB),X(LCDELR),X(LCDELC),X(LCXBC),X(LCYBC),MaxHSSSource,
     & MaxHSSCells,MaxHSSStep,nHSSSource,faclength,factime,facmass,
     & X(LCHSSData),IX(LCHSSLoc),HSSNAM,iRunHSSM)               
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
        CALL BTN5ST(INBTN,IOUT,NSTP,MXSTP,TSLNGH,DT0,MXSTRN,TTSMULT,
     &   TTSMAX,TUNIT,iSSTrans)
C
C--READ AND PREPARE INPUT INFORMATION WHICH IS CONSTANT
C--WITHIN EACH STRESS PERIOD
        IF(iUnitTRNOP(3).GT.0) 
     &   CALL SSM5RP(iUnitTRNOP(3),IOUT,KPER,
     &   NCOL,NROW,NLAY,NCOMP,IX(LCIB),X(LCCNEW),X(LCCRCH),
     &   X(LCCEVT),MXSS,NSS,X(LCSS),X(LCSSMC))
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
          CALL FMI5RP1(INFTL,IOUT,KPER,KSTP,NCOL,NROW,NLAY,
     &     NCOMP,FPRT,IX(LCLAYC),IX(LCIB),HORIGN,X(LCDH),X(LCPR),
     &     X(LCDELR),X(LCDELC),X(LCDZ),X(LCXBC),X(LCYBC),X(LCZBC),
     &     X(LCQSTO),X(LCCOLD),X(LCCNEW),X(LCRETA),X(LCQX),
     &     X(LCQY),X(LCQZ),DTRACK,DTRACK2,THKMIN,ISS,IVER)
          IF(iUnitTRNOP(3).GT.0) 
     &     CALL FMI5RP2(INFTL,IOUT,KPER,KSTP,NCOL,NROW,NLAY,
     &     NCOMP,FPRT,IX(LCLAYC),IX(LCIB),X(LCDH),X(LCPR),X(LCDELR),
     &     X(LCDELC),IX(LCIRCH),X(LCRECH),IX(LCIEVT),X(LCEVTR),
     &     MXSS,NSS,NTSS,X(LCSS),X(LCBUFF),DTSSM)
C
C--CALCULATE COEFFICIENTS THAT VARY WITH FLOW-MODEL TIME STEP
          IF(iUnitTRNOP(2).GT.0) 
     &     CALL DSP5CF(IOUT,KSTP,KPER,NCOL,NROW,NLAY,MCOMP,
     &     IX(LCIB),X(LCPR),X(LCDELR),X(LCDELC),X(LCDH),
     &     X(LCQX),X(LCQY),X(LCQZ),X(LCAL),X(LCTRPT),X(LCTRPV),
     &     X(LCDM),DTDISP,X(LCDXX),X(LCDXY),X(LCDXZ),X(LCDYX),
     &     X(LCDYY),X(LCDYZ),X(LCDZX),X(LCDZY),X(LCDZZ),IFMTDP)
C
   70     CONTINUE
C
C--FOR EACH TRANSPORT STEP..............................................
          TIME2=HT1
          DO N=1,MXSTRN
C
C--ADVANCE ONE TRANSPORT STEP
            CALL BTN5AD(IOUT,N,MXTRNOP,iUnitTRNOP,iSSTrans,
     &       TIME1,TIME2,HT2,DELT,
     &       KSTP,NSTP,MXPRS,TIMPRS,DT0,MXSTRN,MIXELM,DTRACK,DTRACK2,
     &       PERCEL,DTDISP,DTSSM,DTRCT,RFMIN,NPRS,NPS,DTRANS,PRTOUT,
     &       NCOL,NROW,NLAY,NCOMP,IX(LCIB),X(LCCNEW),X(LCCOLD),
     &       CINACT,UPDLHS,IMPSOL,TTSMULT,TTSMAX,KPER,X(LCDELR),
     &       X(LCDELC),X(LCDH),X(LCPR),X(LCSR),X(LCRHOB),X(LCRETA),
     &       X(LCPRSITY2),X(LCRETA2),ISOTHM,TMASIO,RMASIO,TMASS)
C
C--FOR EACH COMPONENT......
            DO ICOMP=1,NCOMP
C
C--SOLVE TRANSPORT TERMS WITH EXPLICIT SCHEMES
              IF(MIXELM.EQ.0) GOTO 1500
C
C--FORMULATE AND SOLVE
              CALL BTN5SV(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
     &         X(LCCNEW),X(LCCWGT),CINACT,RMASIO)
              IF(iUnitTRNOP(1).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL ADV5SV(IOUT,NCOL,NROW,NLAY,MCOMP,ICOMP,
     &         MIXELM,MXPART,NCOUNT,NPINS,NRC,IX(LCCHEK),
     &         X(LCXP),X(LCYP),X(LCZP),IX(LCINDX),IX(LCINDY),
     &         IX(LCINDZ),X(LCCNPT),IX(LCIB),X(LCDELR),X(LCDELC),
     &         X(LCDZ),X(LCXBC),X(LCYBC),X(LCZBC),X(LCDH),
     &         X(LCPR),X(LCQX),X(LCQY),X(LCQZ),X(LCRETA),
     &         X(LCCOLD),X(LCCWGT),X(LCCNEW),X(LCCADV),
     &         X(LCBUFF),DTRANS,IMPSOL,NADVFD,RMASIO)
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
     &           CALL RCT5CF(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
     &           X(LCPR),X(LCCNEW),X(LCRETA),RFMIN,X(LCRHOB),X(LCSP1),
     &           X(LCSP2),X(LCRC1),X(LCRC2),X(LCPRSITY2),X(LCRETA2),
     &           X(LCFRAC),X(LCSR),ISOTHM,IREACT,DTRANS)
C
C--FORMULATE MATRIX COEFFICIENTS
                CALL BTN5FM(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
     &           X(LCCADV),X(LCCOLD),X(LCRETA),X(LCPR),X(LCDELR),
     &           X(LCDELC),X(LCDH),DTRANS,
     &           X(LCA),X(LCRHS),NODES,UPDLHS,NCRS,MIXELM,iSSTrans)
                IF(iUnitTRNOP(1).GT.0.AND.MIXELM.EQ.0 
     &           .AND. ICOMP.LE.MCOMP)
     &           CALL ADV5FM(NCOL,NROW,NLAY,MCOMP,ICOMP,IX(LCIB),
     &           X(LCDELR),X(LCDELC),X(LCDH),X(LCQX),X(LCQY),X(LCQZ),
     &           NADVFD,NODES,X(LCA),UPDLHS)
                IF(iUnitTRNOP(2).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL DSP5FM(NCOL,NROW,NLAY,MCOMP,ICOMP,IX(LCIB),
     &           X(LCDELR),X(LCDELC),X(LCDH),X(LCDXX),X(LCDXY),
     &           X(LCDXZ),X(LCDYX),X(LCDYY),X(LCDYZ),X(LCDZX),
     &           X(LCDZY),X(LCDZZ),X(LCA),NODES,UPDLHS,X(LCCNEW),
     &           X(LCRHS),NCRS)
                IF(iUnitTRNOP(3).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL SSM5FM(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
     &           X(LCDELR),X(LCDELC),X(LCDH),IX(LCIRCH),X(LCRECH),
     &           X(LCCRCH),IX(LCIEVT),X(LCEVTR),X(LCCEVT),MXSS,NTSS,
     &           X(LCSS),X(LCSSMC),X(LCSSG),X(LCQSTO),X(LCCNEW),ISS,
     &           X(LCA),X(LCRHS),NODES,UPDLHS,MIXELM)
                IF(iUnitTRNOP(13).GT.0 .AND. ICOMP.LE.MCOMP)
     &           CALL HSS5FM(NCOL,NROW,NLAY,NCOMP,ICOMP,MIXELM,UPDLHS,
     &           MaxHSSSource,MaxHSSStep,MaxHSSCells,nHSSSource,
     &           TIME1,TIME2,IX(LCIB),X(LCA),X(LCRHS),NODES,
     &           X(LCHSSData),IX(LCHSSLoc))
                IF(iUnitTRNOP(4).GT.0) 
     &           CALL RCT5FM(NCOL,NROW,NLAY,NCOMP,ICOMP,
     &           IX(LCIB),X(LCPR),X(LCDELR),X(LCDELC),X(LCDH),ISOTHM,
     &           IREACT,X(LCRHOB),X(LCSP1),X(LCSP2),X(LCSR),X(LCRC1),
     &           X(LCRC2),X(LCPRSITY2),X(LCRETA2),X(LCFRAC),X(LCA),
     &           X(LCRHS),NODES,UPDLHS,DTRANS)
                IF(iUnitTRNOP(5).GT.0) 
     &           CALL GCG5AP(IOUT,MXITER,ITER1,ITO,ITP,ISOLVE,ACCL,
     &           CCLOSE,ICNVG,X(LCCNCG),IX(LCLRCH),NCOL,NROW,NLAY,
     &           NODES,N,KSTP,KPER,TIME2,HT2,UPDLHS,IPRGCG,
     &           IX(LCIB+(ICOMP-1)*NODES),CINACT,X(LCA),
     &           X(LCCNEW+(ICOMP-1)*NODES),X(LCRHS),X(LCQ),X(LCWK),
     &           NCRS,ISPD)
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
     &         CALL ADV5BD(IOUT,NCOL,NROW,NLAY,MCOMP,ICOMP,NADVFD,
     &         IX(LCIB),X(LCDELR),X(LCDELC),X(LCDH),X(LCQX),X(LCQY),
     &         X(LCQZ),X(LCCNEW),DTRANS,RMASIO)
              IF(iUnitTRNOP(2).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL DSP5BD(NCOL,NROW,NLAY,MCOMP,ICOMP,IX(LCIB),
     &         X(LCDELR),X(LCDELC),X(LCDH),X(LCDXX),X(LCDXY),X(LCDXZ),
     &         X(LCDYX),X(LCDYY),X(LCDYZ),X(LCDZX),X(LCDZY),X(LCDZZ),
     &         X(LCCNEW),X(LCBUFF),DTRANS,RMASIO)
              IF(iUnitTRNOP(3).GT.0 .AND. ICOMP.LE.MCOMP)
     &         CALL SSM5BD(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
     &         X(LCDELR),X(LCDELC),X(LCDH),IX(LCIRCH),X(LCRECH),
     &         X(LCCRCH),IX(LCIEVT),X(LCEVTR),X(LCCEVT),MXSS,NTSS,
     &         X(LCSS),X(LCSSMC),X(LCSSG),X(LCQSTO),X(LCCNEW),X(LCRETA),
     &         DTRANS,ISS,RMASIO)
              IF(iUnitTRNOP(13).GT.0 .AND. ICOMP.LE.MCOMP) 
     &         CALL HSS5BD(NCOL,NROW,NLAY,NCOMP,ICOMP,NODES,IX(LCIB),
     &         MaxHSSSource,MaxHSSStep,MaxHSSCells,nHSSSource,50,
     &         TIME1,TIME2,X(LCHSSData),IX(LCHSSLoc),RMASIO,DTRANS)     
              IF(iUnitTRNOP(4).GT.0) 
     &         CALL RCT5BD(NCOL,NROW,NLAY,NCOMP,ICOMP,
     &         IX(LCIB),X(LCPR),X(LCDELR),X(LCDELC),X(LCDH),DTRANS,
     &         ISOTHM,IREACT,X(LCRHOB),X(LCSP1),X(LCSP2),X(LCSR),
     &         X(LCRC1),X(LCRC2),X(LCPRSITY2),X(LCRETA2),X(LCFRAC),
     &         X(LCCNEW),X(LCRETA),RFMIN,RMASIO)
C
C--CALCULATE GLOBAL MASS BUDGETS AND CHECK MASS BALANCE
              CALL BTN5BD(KPER,KSTP,N,NCOL,NROW,NLAY,NCOMP,ICOMP,
     &         ISS,iSSTrans,IX(LCIB),X(LCDELR),X(LCDELC),X(LCDH),
     &         X(LCPR),X(LCRETA),X(LCCNEW),X(LCCOLD),X(LCRHOB),
     &         X(LCSR),X(LCPRSITY2),X(LCRETA2),ISOTHM,
     &         DTRANS,TMASIN,TMASOT,ERROR,ERROR2,TMASIO,RMASIO,TMASS)
C
C--SAVE OUTPUTS
              CALL BTN5OT(NCOL,NROW,NLAY,KPER,KSTP,N,NCOMP,ICOMP,IOUT,
     &         IOBS,IUCN,IUCN2,IMAS,ICBM,MXOBS,NOBS,NPROBS,LOCOBS,
     &         IX(LCIB),TIME2,X(LCCNEW),MIXELM,NCOUNT,NPINS,NRC,
     &         IX(LCCHEK),ISOTHM,X(LCRETA),X(LCSR),TMASIN,TMASOT,
     &         ERROR,ERROR2,MXTRNOP,iUnitTRNOP,TUNIT,MUNIT,PRTOUT,
     &         TMASIO,RMASIO,TMASS)   
              IF(FMNW) CALL SSM5OT(NCOL,NROW,NLAY,KPER,KSTP,N,NCOMP,
     &         ICOMP,IX(LCIB),MXSS,NTSS,NSS,X(LCSS),X(LCSSG),PRTOUT,
     &         TIME2,IOUT,ISSGOUT)              
              IF(iUnitTRNOP(11).GT.0) THEN 
                if(inConcOBS.GT.0) then
                  call ConcObs(inConcObs,iout,ncol,nrow,nlay,ncomp,
     &             kper,kstp,n,time1,time2,X(LCCNEW),cinact,
     &             IX(LCIB),X(LCdelr),X(LCdelc),
     &             X(LCxbc),X(LCybc),nConcObs,X(LCCOBS),cobsnam,
     &             IX(LCMLAYER),X(LCPRLAYER),X(LCTEMP),inSaveObs,
     &             iOutCobs,iConcLOG,iConcINTP)
                endif   
                if(inFluxObs.GT.0) then
                  call MassFluxObs(inFluxObs,iout,ncol,nrow,nlay,ncomp,
     &             MaxFluxCells,nFLuxGroup,nFLuxObs,kper,kstp,n,
     &             time1,time2,X(LCCNEW),IX(LCIB),mxss,ntss,X(LCSS),
     &             X(LCSSMC),X(LCdelr),X(LCdelc),X(LCdh),IX(LCIRCH),
     &             X(LCRECH),X(LCCRCH),IX(LCIEVT),X(LCEVTR),X(LCCEVT),
     &             X(LCFluxGroup),X(LCGroupData),
     &             fobsnam,x(LCTEMP),inSaveObs,iOutFlux)
                endif
              ENDIF    
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
C--DEALLOCATE MEMORY
      DEALLOCATE (X,IX)
      if(iUnitTRNOP(11).GT.0) DEALLOCATE (COBSNAM,FOBSNAM)
      if(iUnitTRNOP(13).GT.0) DEALLOCATE (HSSNAM)
C
C--PROGRAM COMPLETED
      WRITE(IOUT,1200)
      WRITE(IOUT,1225)
      WRITE(IOUT,1200)
 1200 FORMAT(1X,' ----- ')
 1225 FORMAT(1X,'| M T |'
     &      /1X,'| 3 D | END OF MODEL OUTPUT')
C
C--Get CPU time at the end of simulation
C--and print out total elapsed time in seconds
      Call CPU_TIME(end_time)
      total_time = end_time - start_time
      Write(*,2010) int(total_time/60.),mod(total_time,60.)
 2010 FORMAT(/1X,'Program completed.   ',
     & 'Total CPU time:',i5.3,' minutes ',f6.3,' seconds')
C
      STOP
      END