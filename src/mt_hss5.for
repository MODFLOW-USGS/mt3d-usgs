C
      SUBROUTINE HSS1AR(IN)
C ********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED BY THE HSSM-MT3DMS
C INTERFACE (HSS) PACKAGE.
C ********************************************************************
C Last modified: 02-20-2010
C
      USE DSSL
      USE MT3DMS_MODULE, ONLY: INHSS,IOUT,NCOL,NROW,NLAY,NCOMP,ICBUND,
     &                         DELR,DELC,XBC,YBC,
     &                         MaxHSSSource,MaxHSSCells,MaxHSSStep,
     &                         nHSSSource,iRunHSSM,faclength,factime,
     &                         facmass,iHSSLoc,HSSData,HSSNAM,
     &                         IHSSGEN,P 
C
      IMPLICIT  NONE
      INTEGER   IN,LLOC,ISTART,ISTOP,ITMP,inam1,inam2
      REAL      R
      REAL      YMAX
      CHARACTER LINE*200
C      integer,parameter :: nPoint=51, nSubGrid=25
      INTEGER   nPoint,nSubGrid,IPNTPOLY
      INTEGER   N,IU,
     &          IFLEN,inHSSFile,i,j,k,
     &          it,iHSSComp,num,
     &          jSource,iSource,kSource,iStep,NStep,nr,inode
      REAL      time,radius_lnapl,
     &          sourcemassflux,r_distance,
     &          area_cell,area_source,area_total,degree
      CHARACTER SourceName*12,HSSFileName*200
C      DIMENSION p(2,nPoint)                
      DOUBLE PRECISION PI                   
      PARAMETER (PI=3.141592653589793238D0) 
C     REAL, ALLOCATABLE :: P(:,:)           
C
      INHSS=IN
C
C--ALLOCATE
      ALLOCATE(MaxHSSSource,MaxHSSCells,MaxHSSStep,nHSSSource,iRunHSSM,
     &         faclength,factime,facmass,IHSSGEN,MAXDSSL,IHSSOUT)
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INHSS
 1030 FORMAT(1X,'HSS1 -- MT3DMS-HSSM INTERFACE PACAKGE,',
     & ' VERSION 1, OCTOBER 2014, INPUT READ FROM UNIT ',I5)
C
C--READ INPUT LINE AS A TEXT STRING
    2 READ(INHSS,'(A)') LINE
      IF(LINE.EQ.' ') GOTO 2
      IF(LINE(1:1).EQ.'#') THEN
        WRITE(IOUT,'(A)') LINE
        GOTO 2
      ENDIF      
C
C--DECODE THE INPUT VARIABLES
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,INHSS)
      MaxHSSSource=ITMP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,INHSS)
      MaxHSSCells=ITMP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,INHSS)
      MaxHSSStep=ITMP
      CALL URWORD(LINE,LLOC,inam1,inam2, 1,ITMP,R,IOUT,INHSS)  
      IF(LINE(inam1:inam2).EQ.'RUNHSSM') THEN
        iRunHSSM=1
      ELSE
        iRunHSSM=0
      ENDIF
      IHSSGEN=0 
      ITMP=0    
      CALL URWORD(LINE,LLOC,inam1,inam2, 1,ITMP,R,IOUT,INHSS)
      IF(LINE(inam1:inam2).EQ.'POLYGON') THEN
        IHSSGEN=1                            
      ELSEIF(LINE(inam1:inam2).EQ.'IRREGULAR') THEN 
        IHSSGEN=2
      ENDIF      
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,INHSS)
      MAXDSSL=ITMP
C
C--ECHO INPUT VARIABLES
      WRITE(IOUT,9) MaxHSSSource,MaxHSSCells,MaxHSSStep
    9 FORMAT(1X,'MAXIMUM NO. OF HSS SOURCES =',I4,
     &      /1X,'MAXIMUM NO. OF MODEL CELLS FOR A HSS SOURCE =',I4,
     &      /1X,'MAXIMUM NO. OF TIME STEPS DEFINING A HSS SOURCE =',I4)
      IF(IHSSGEN.EQ.0) THEN                                  
        WRITE(IOUT,*) 'SOURCE SHAPE IS AN APPROXIMATE CIRCLE'
      ELSEIF(IHSSGEN.EQ.1) THEN                              
        WRITE(IOUT,*) 'SOURCE SHAPE IS A REGULAR POLYGON'    
      ELSEIF(IHSSGEN.EQ.2) THEN                              
        WRITE(IOUT,*) 'SOURCE SHAPE IS AN IRREGULAR POLYGON' 
      ELSE                                                   
        WRITE(IOUT,*) 'ERROR IN HSS PACKAGE: IHSSGEN'        
        WRITE(*,*) 'ERROR IN HSS PACKAGE: IHSSGEN'           
        READ(*,*)                                            
        STOP                                                 
      ENDIF                                                  
C                                                            
C--ALLOCATE AND INITIALIZE
      ALLOCATE(iHSSLoc(MaxHSSCells,MaxHSSStep,MaxHSSSource))
      ALLOCATE(HSSData(4+MaxHSSCells,MaxHSSStep,MaxHSSSource))
      ALLOCATE(HSSNAM(MaxHSSSource))
      IF(MAXDSSL.GT.0) THEN
        ALLOCATE(IDSSL(MaxHSSSource),IDSSLCOMP(MaxHSSSource))
        ALLOCATE(THKDSS(MaxHSSCells,MaxHSSSource),
     1  TSDSS(MaxHSSCells,MaxHSSSource),TEDSS(MaxHSSCells,MaxHSSSource))
        IDSSL=0
      ENDIF
      iHSSLoc=0
      HSSData=0.
C
C--READ INPUT DATA
      read(inhss,*)  faclength,factime,facmass
      write(iout,10) faclength,factime,facmass
      read(inhss,*)  nHSSSource
      write(iout,12) nHSSSource
   10 format(//1x,'INPUT DATA FOR HSS LNAPL SOURCES',
     & /1x,       '--------------------------------',
     &//1x,'LNTH UNIT CONVERSION FACTOR FROM HSSM TO MT3D-USGS =',G12.4,
     & /1x,'TIME UNIT CONVERSION FACTOR FROM HSSM TO MT3D-USGS =',G12.4,
     & /1x,'MASS UNIT CONVERSION FACTOR FROM HSSM TO MT3D-USGS =',G12.4)
   12 format(/1x,'TOTAL NUMBER OF HSS SOURCES IN THIS SIMULATION =',I4)
C   
      if(nHSSSource.gt.MaxHSSSource) then
        call ustop ('[MaxHSSSource] exceeded!')
      endif   
C   
      IF(IHSSGEN.EQ.2) THEN 
        YMAX=0.             
        DO I=1,NROW         
          YMAX=YMAX+DELC(I) 
        ENDDO               
      ENDIF                 
C
      DO n=1,nHSSSource     !go over each HSS source
C
C--READ INPUT LINE AS A TEXT STRING
C
        READ(INHSS,'(A)') LINE
C--DECODE THE HSS SOURCE DEFINITION FILE NAME
        LLOC=1
        CALL URWORD(LINE,LLOC,INAM1,INAM2,0,IU,R,IOUT,INHSS)
        IFLEN=INAM2-INAM1+1
        HSSFileName(1:IFLEN)=LINE(INAM1:INAM2)
C
C--DECODE SOURCE DEFINITION FILE INPUT UNIT
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INHSS)
        inHSSFile=IU
C--DSSL OUTPUT FILE UNIT
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INHSS)
        IHSSOUT=IU
C
C--DECODE THE DISSOLUTION PACKAGE OPTION
        IF(MAXDSSL.GT.0) THEN
          READ(LINE(ISTOP+1:200),*,ERR=13) IDSSL(N),IDSSLCOMP(N)
        ENDIF
13      CONTINUE
C
C--ECHO INPUT DATA
        write(iout,20) n,HSSFileName(1:IFLEN),inHSSFile,IHSSOUT
   20   format(/1x,'HSS Source No. ',I4.4,
     &         /1x,'Source Definition File Name: ',a,
     &         /1x,'Source Definition File Read from Unit: ',i4,
     &         /1x,'Output will be wrtten on Unit: ',i4)
        IF(MAXDSSL.GT.0) THEN
          IF(IDSSL(N).GT.0) WRITE(IOUT,21) IDSSL(N),IDSSLCOMP(N)
        ENDIF
21    FORMAT(1X,'DISSOLUTION FORMULATION IS INVOKED FOR FOLLOWING ',I4,
     &           ' CELLS, ASSOCIATED WITH SPECIES ',I4)
C
C--GET SOURCE LOCATION FOR DISSOLUTION OPTION
        IF(MAXDSSL.GT.0) THEN
        IF(IDSSL(N).GT.0) THEN
          IF(IDSSL(N).GT.MaxHSSCells) THEN
            WRITE(*,*) 'IDSSL EXCEEDS MaxHSSCells',N
            WRITE(IOUT,*) 'IDSSL EXCEEDS MaxHSSCells',N
            STOP
          ENDIF
          DO NUM=1,IDSSL(N)
            READ(inhss,*)  ksource,isource,jsource,THKDSS(NUM,N),
     &      TSDSS(NUM,N),TEDSS(NUM,N)
            iHSSLoc(num,1,n)=
     &        (ksource-1)*ncol*nrow+(isource-1)*ncol+jsource
            write(iout,22) ksource,isource,jsource,THKDSS(NUM,N),
     &      TSDSS(NUM,N),TEDSS(NUM,N)
          ENDDO
C--READ DSSL INPUT FILE
          OPEN(inHSSFile,file=HSSFileName(1:IFLEN),STATUS='OLD')
          CALL DSSL1AR(inHSSFile,N,MaxHSSStep,nHSSSource,IOUT)
          CLOSE(inHSSFile)
          CYCLE
   22     format(1x,'[Layer,Row,Column,SrcThk,Tstrt,Tend]:',
     1           3i5,3(1PG12.4)) !,'; Species:',i3)
        ENDIF
        ELSE
C
C--GET HSS SOURCE LOCATION AND NAME
        IF(IHSSGEN.EQ.1) THEN                                        
          READ(inhss,*)  ksource,isource,jsource,iHSSComp,sourcename,
     &    nPoint,nSubGrid                                            
        ELSEIF(IHSSGEN.EQ.2) THEN                                    
          READ(inhss,*)  ksource,iHSSComp,sourcename,                
     &    nPoint,nSubGrid                                            
        ELSE                                                         
          nPoint=51                                                  
          nSubGrid=25                                                
          READ(inhss,*)  ksource,isource,jsource,iHSSComp,sourcename 
        ENDIF                                                        
        ENDIF
        ALLOCATE (P(2,NPOINT))                                       
C 
        HSSNAM(n)=sourcename
C                              
        IPNTPOLY=0             
        IF(nSubGrid.LT.0) THEN 
          nSubGrid=-nSubGrid   
          IPNTPOLY=1           
        ENDIF                  
C                              
        IF(IHSSGEN.EQ.1) THEN  
        write(iout,31) ksource,isource,jsource,iHSSComp,sourcename,
     &    nPoint,nSubGrid                                   
   31   format(1x,'[Layer,Row,Column]:',3i5,'; Species:',i3,
     &        /1x,'Source Name: ',A,' Poly Points: ',I3,' Subgrids',I3)
        ELSEIF(IHSSGEN.EQ.2) THEN                   
        write(iout,32) ksource,iHSSComp,sourcename, 
     &    nPoint,nSubGrid                           
   32   format(1x,'[Layer]:',i5,'; Species:',i3,    
     &        /1x,'Source Name: ',A,' Poly Points: ',I3,' Subgrids',I3)
        ELSE
          write(iout,30) ksource,isource,jsource,iHSSComp,sourcename
   30     format(1x,'[Layer,Row,Column]:',3i5,'; Species:',i3,
     &          /1x,'Source Name: ',A)                 
        ENDIF                         
C                                     
C-----READ POLYGON VERTICES 1:nPoint  
        IF(IHSSGEN.EQ.2) THEN         
          WRITE(iout,*) 'X Y MODEL COORDINATES (Y REVERSED)' 
          DO nr=1,nPoint                 
            READ(inhss,*) p(1,nr),p(2,nr)
C                                        
C-----------REVERSE Y                    
            p(2,nr)=YMAX-p(2,nr)         
C                                        
            WRITE(iout,*) p(1,nr),p(2,nr)
          ENDDO                          
        ENDIF                            
C
C--Run external HSSMKO DLL if necessary (uncomment to activate)
        IF(iRunHSSM.eq.1) then     
!hss      WRITE(*,33) HSSFileName(1:IFLEN)  !HSSFileName is c*200
!hss      CALL HSSM(1,0,HSSFileName(1:IFLEN-4))  
        ENDIF         
   33   format(/'***Running HSSMKO ',
     &          'to generate source definition file: ',a/)
C                                                                             
C--READ HSSM INPUT FILE
        OPEN(inHSSFile,file=HSSFileName(1:IFLEN),STATUS='OLD')
        write(iout,40)
   40   FORMAT(1x,'   Time      LNAPL_Plume_Radius',
     &   '   Mass_Loading_Rate')
C
        it=0
   50   read(inHSSFile,*,end=100) time,radius_lnapl,sourcemassflux
        write(iout,60) time,radius_lnapl,sourcemassflux
   60   format(1x,g12.4,3x,g15.7,4x,g15.7)     
        it=it+1
        if(it.gt.MaxHSSStep) then
          call ustop ('[MaxHSSStep] exceeded!')          
        endif
        HSSData(1,it,n)=time * factime
        HSSData(2,it,n)=radius_lnapl * faclength
        HSSData(3,it,n)=sourcemassflux * facmass / factime
        HSSData(4,it,n)=iHSSComp
        goto 50
  100   close (inHSSFile)
C
C--DISTRIBUTE LNAPL SOURCE into MULTIPLE CELLS at MULTIPLE times
C--(based on area weighting)
        NStep=it
        write(iout,110)
  110   format(1x,'Source Allocation Statistics',
     &        /1x,'   Time        Layer  Row  Col',
     &        '   Redistributed Rate')
c
        DO iStep=1,NStep
c          
c compute source area
          radius_lnapl=HSSData(2,iStep,n)
          if(radius_lnapl.le.0) then
            area_source=-999.
          else
            IF(IHSSGEN.EQ.1) THEN                          
              area_source=radius_lnapl*radius_lnapl*nPoint*
     &        sin(2.0*PI/nPoint)/2.0                       
            ELSEIF(IHSSGEN.EQ.2) THEN                      
              area_source=0.                               
              DO nr=1,nPoint                               
                IF(nr.EQ.nPoint) THEN                      
                  area_source=area_source+                 
     &            p(1,nr)*p(2,1)-p(2,nr)*p(1,1)            
                ELSE                                       
                  area_source=area_source+                 
     &            p(1,nr)*p(2,nr+1)-p(2,nr)*p(1,nr+1)      
                ENDIF                                      
              ENDDO                                        
              area_source=ABS(area_source/2.0)             
            ELSE                                           
              area_source=PI*radius_lnapl**2               
            ENDIF                                          
          ENDIF                                            
c
c distribute to starting source cell
          IF(IHSSGEN.NE.2) THEN
            num=1
            iHSSLoc(num,iStep,n)=
     &        (ksource-1)*ncol*nrow+(isource-1)*ncol+jsource
            area_cell=delr(jsource)*delc(isource)
            if(area_source .lt. area_cell) then
              HSSData(4+num,iStep,n)=HSSData(3,iStep,n)
              write(iout,120) HSSData(1,iStep,n),
     &         ksource,isource,jsource,HSSData(4+num,iStep,n)
              cycle
            endif
c
c distribute to multiple cells            
            do nr=1,nPoint     !discretize source perimeter into polygon
              degree=(nr-1)*2.*PI/nPoint                    
              p(1,nr)=xbc(jsource)+radius_lnapl*cos(degree)            
              p(2,nr)=ybc(isource)+radius_lnapl*sin(degree)
            ENDDO
          ENDIF
c                            
          num=0
          area_total=0	    
          do i=1,nrow
            do j=1,ncol
              IF(I.EQ.16.AND.J.EQ.15) THEN
                CONTINUE                  
              ENDIF                       
              IF(IHSSGEN.NE.2) THEN       
                R=
     &          sqrt((xbc(j)-xbc(jsource))**2+(ybc(i)-ybc(isource))**2)
                R=R-0.5*sqrt(delr(j)**2+delc(i)**2)
                if(R.gt.1.5*radius_lnapl) cycle  !1.5 is a safety factor
              ENDIF 
              call GetArea(ncol,nrow,nPoint,p,nSubGrid,delr,xbc,
     &         delc,ybc,j,i,area_cell,IPNTPOLY)    
c            
              if(area_cell.le.0) cycle
              num=num+1              
              if(num.gt.MaxHSSCells) then
                call ustop('[MaxHSSCells] exceeded!')                 
              endif              
              iHSSLOC(num,iStep,n)=(ksource-1)*ncol*nrow+(i-1)*ncol+j
              HSSData(4+num,iStep,n)=area_cell  
              area_total=area_total+area_cell
            enddo
          enddo
c
          do itmp=1,num
            inode=iHSSLoc(itmp,iStep,n)
            if(inode.le.0) cycle
            i = mod((inode-1),ncol*nrow)/ncol + 1
            j = mod((inode-1),ncol) + 1
            area_cell=HSSData(4+itmp,iStep,n)
            R=area_cell/area_total
            HSSData(4+itmp,iStep,n)=R*HSSData(3,iStep,n)
            write(iout,120) HSSData(1,iStep,n),ksource,i,j,
     &       HSSData(4+itmp,iStep,n)
          enddo
c
        ENDDO
  120   FORMAT(1x,g12.4,3i6,4x,g15.7)
        DEALLOCATE (p) 
C        
      ENDDO         !done with all HSS sources
C
C--normal return
 1000 CONTINUE
      RETURN
      END
C
C  
      Subroutine HSS1FM(ICOMP,ICBUND,time1,time2,DTRANS)
C **********************************************************************
C THIS SUBROUTINE FORMULATES MATRIX COEFFICIENTS FOR THE HSS SOURCE
C TERM UNDER THE IMPLICIT FINITE-DIFFERENCE METHOD.
C **********************************************************************          
C last modified: 02-20-2010
C
      USE DSSL
      USE MIN_SAT, ONLY : QC7
      USE MT3DMS_MODULE, ONLY:NCOL,NROW,NLAY,NCOMP,MIXELM,UPDLHS,
     &                        MaxHSSSource,MaxHSSStep,MaxHSSCells,
     &                        nHSSSource,HSSData,iHSSLoc,
     &                        A,RHS,NODES,IDRY2,DELR,DELC,DH,PRSITY,
     &                        COLD,CNEW
C
      IMPLICIT  NONE
      INTEGER   ICOMP,ICBUND,is,it,icell,IGRID,
     &          N,iStep,iHSSComp
      INTEGER K,I,J,NUM
      REAL      ctmp,ctmp1,ctmp2,tstart,tend,time1,time2,DTRANS,RSOL,
     &          FDSS
      DIMENSION ICBUND(NODES,NCOMP)
C
C--FORMULATE [RHS] MATRIX FOR EULERIAN & EULERIAN-LAGRANGIAN SCHEMES     
      DO is=1,nHSSSource     
        IF(MAXDSSL.EQ.0) THEN
C        IF(IDSSL(is).EQ.0) THEN
c
        iHSSComp=int(HSSData(4,1,is))
        if(iHSSComp.ne.ICOMP) cycle               
c        
  666   DO icell=1,MaxHSSCells                   
          N=iHSSLoc(icell,1,is)
          IF(N.EQ.0) CYCLE
c          
          CALL TVSource(NCOL,NROW,NLAY,iStep,iCell,is,
     &     MaxHSSSource,MaxHSSStep,MaxHSSCells,time1,time2,
     &     HSSData,CTMP)     
          IF(ICBUND(N,ICOMP).le.0) THEN
            IF(ICBUND(N,ICOMP).eq.0) THEN
              IF(IDRY2.EQ.1) THEN
                CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
                QC7(J,I,K,7)=QC7(J,I,K,7)-CTMP
              ENDIF
            ENDIF
          ELSE
c
c--add contribution to [RHS] if ctmp is positive
          IF(CTMP.LT.0) THEN
            CALL USTOP('Error in HSS Source Definition File!')
          ELSE
            RHS(N)=RHS(N)-CTMP
          ENDIF             
C
          ENDIF
        ENDDO         
        ELSE
C
C--DISSOLUTION FORMULATION
          iHSSComp=IDSSLCOMP(is)
          if(iHSSComp.ne.ICOMP) cycle               
          DO NUM=1,IDSSL(is)
            N=iHSSLoc(num,1,is)
            CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
C...........CHECK TIME PERIOD OF SOURCE RELEASE
            TSTART=TSDSS(NUM,IS)
            TEND=TEDSS(NUM,IS)
            IF(TSTART.GE.time2 .OR. TEND.LE.time1) CYCLE
C
            IF(ICBUND(N,ICOMP).le.0) CYCLE
            FDSS=DH(J,I,K)/THKDSS(NUM,IS)
            FDSS=MIN(FDSS,1.0)
            CALL DSSL1FM(DTRANS,RSOL,IS,NUM,ICOMP,FDSS)
C            RHS(N)=RHS(N)-RSOL*(DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K))
            RHS(N)=RHS(N)-RSOL*
     1        (DELR(J)*DELC(I)*THKDSS(NUM,IS)*PORDSS(IS))
          ENDDO
        ENDIF
C
      ENDDO        
C
C--RETURN
      RETURN
      END
C
C  
      Subroutine HSS1BD(ICOMP,ICBUND,IQ,time1,time2,DTRANS)
C **********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGETS ASSOCIATED WITH THE HSS
C SOURCE TERM.
C **********************************************************************          
C last modified: 02-20-2010
C
      USE DSSL
      USE MIN_SAT, ONLY: QC7
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,NODES,RMASIO,
     &                         MaxHSSSource,MaxHSSCells,MaxHSSStep,
     &                         nHSSSource,iRunHSSM,faclength,factime,
     &                         facmass,iHSSLoc,HSSData,HSSNAM,IDRY2,
     &                         DELR,DELC,DH,PRSITY,
     &                        COLD,CNEW
C
      IMPLICIT  NONE      
      INTEGER   ICOMP,ICBUND,is,it,icell,N,iStep,IGRID,
     &          iHSSComp,IQ    !IQ is iSSType for HSS source
      INTEGER K,I,J,NUM,IPHS,II
      REAL      DTRANS,ctmp,ctmp1,ctmp2,tstart,tend,time1,time2,RSOL,
     &          FDSS
      DIMENSION ICBUND(NODES,NCOMP)
C
C--LOOP over all HSS_LNAPL sources
        DO is=1,nHSSSource               
          IF(MAXDSSL.EQ.0) THEN
C          IF(IDSSL(is).EQ.0) THEN
c	  
          iHSSComp=int(HSSData(4,1,is))
	        if(iHSSComp.ne.ICOMP) cycle            
c
  666     DO icell=1,MaxHSSCells                
c  
            N=iHSSLoc(icell,1,is)
            IF(N.EQ.0) CYCLE 
C            IF(ICBUND(N,icomp).le.0) cycle
c            
            CALL TVSource(NCOL,NROW,NLAY,iStep,iCell,is,
     &       MaxHSSSource,MaxHSSStep,MaxHSSCells,time1,time2,
     &       HSSData,CTMP)            
            IF(ICBUND(N,ICOMP).le.0) THEN
              IF(ICBUND(N,ICOMP).eq.0) THEN
                IF(IDRY2.EQ.1) THEN
                  CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
                  QC7(J,I,K,7)=QC7(J,I,K,7)-CTMP
                  RMASIO(IQ,1,ICOMP)=RMASIO(IQ,1,ICOMP)+CTMP*DTRANS     
                ENDIF
              ENDIF
            ELSE
c
            IF(CTMP.GT.0) THEN
              RMASIO(IQ,1,ICOMP)=RMASIO(IQ,1,ICOMP)+CTMP*DTRANS     
            ELSE
              RMASIO(IQ,2,ICOMP)=RMASIO(IQ,2,ICOMP)+CTMP*DTRANS    
            ENDIF                     
            ENDIF
          ENDDO
C
          ELSE
C
C--DISSOLUTION FORMULATION
          iHSSComp=IDSSLCOMP(is)
          if(iHSSComp.ne.ICOMP) cycle               
          DO NUM=1,IDSSL(is)
            N=iHSSLoc(num,1,is)
            CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
C...........CHECK TIME PERIOD OF SOURCE RELEASE
            TSTART=TSDSS(NUM,IS)
            TEND=TEDSS(NUM,IS)
            IF(TSTART.GE.time2 .OR. TEND.LE.time1) CYCLE
C
            IF(ICBUND(N,ICOMP).GT.0) THEN
              FDSS=DH(J,I,K)/THKDSS(NUM,IS)
              FDSS=MIN(FDSS,1.0)
              CALL DSSL1BD(DTRANS,RSOL,IS,NUM,time2,ICOMP,FDSS)
C              RMASIO(IQ,1,ICOMP)=RMASIO(IQ,1,ICOMP)+
C     1        RSOL*DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)*DTRANS
              RMASIO(IQ,1,ICOMP)=RMASIO(IQ,1,ICOMP)+
     1        RSOL*DTRANS*
     1          DELR(J)*DELC(I)*THKDSS(NUM,IS)*PORDSS(IS)
              SRCMASS(NUM,IS)=SRCMASS(NUM,IS)-RSOL*DTRANS*
     1          DELR(J)*DELC(I)*THKDSS(NUM,IS)*PORDSS(IS)
            ENDIF
C
C--WRITE MINERAL, CATION, AND ANION CONC
            IF(IHSSOUT.GT.0) 
     &      WRITE(IHSSOUT,10) time2,IS,K,I,J,
     &      ((CRESNEW(IPHS,II,NUM,IS),IPHS=1,NSLDPHS(IS)),
     &      II=1,2),SRCMASS(NUM,IS),
     &      (CRESNEW(IPHS,4,NUM,IS),IPHS=1,NSLDPHS(IS))
          ENDDO
          CRESOLD(:,:,:,IS)=CRESNEW(:,:,:,IS)
          CSOLOLD(:,IS)=CSOLNEW(:,IS)
          ENDIF
        ENDDO  
10    FORMAT(1X,1PG13.5,1X,4(I4,1X),100(1PG13.5,2X))
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE TVSource(NCOL,NROW,NLAY,iStep,iCell,iSource,
     & MaxHSSSource,MaxHSSStep,MaxHSSCells,time1,time2,HSSData,CTMP)
C **********************************************************************
C THIS SUBROUTINE CALCULATES AN AVERAGE MASS LOADING RATE BETWEEN TIME
C INTERVAL [Time1, Time2] FROM AN ARBITRARILY DEFINED SOURCE SERIES
C **********************************************************************          
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,isource,it,icell,
     &          iStep,MaxHSSSource,iTime1,iTime2,
     &          MaxHSSCells,MaxHSSStep,nHSSSource,iHSSComp
      REAL      ctmp,ctmp1,ctmp2,tstart,tend,time1,time2,HSSData,
     &          cmtmp,tmtmp,cTime1,cTime2,cstart,cend,ttmp     
      DIMENSION HSSData(4+MaxHSSCells,MaxHSSStep,MaxHSSSource)
c
c--get starting and ending indices of transport step in source series    
      iTime2=1
      do it=MaxHSSStep,1,-1
        ttmp=HSSData(1,it,iSource)
        if(ttmp.gt.0 .and. time2.ge.ttmp ) then
          iTime2=it
          exit
        endif
      enddo     
c             
      iTime1=1      
      do it=iTime2,1,-1
        ttmp=HSSData(1,it,iSource)
        if(ttmp.gt.0. and. time1.ge.ttmp) then
          iTime1=it
          exit
        endif
      enddo               
c          
c--get interpolated conc at beginning and ending of transport step         
      cTime1=0.
      if(iTime1.lt.MaxHSSStep) then                
	      cstart=HSSData(4+icell,iTime1,  isource)
        cend=  HSSData(4+icell,iTime1+1,isource)
        tstart=HSSData(1,iTime1,  isource)
        tend  =HSSData(1,iTime1+1,isource)       
        if(tend.ne.tstart.and.time1.ge.tstart.and.time1.le.tend) then
          cTime1=((cend-cstart)/(tend-tstart))*(time1-tstart)+cstart
        endif
      endif  
c      
      cTime2=0.
      if(iTime2.lt.MaxHSSStep) then                     
	      cstart=HSSData(4+icell,iTime2,  isource)
        cend=  HSSData(4+icell,iTime2+1,isource)
        tstart=HSSData(1,iTime2,  isource)
        tend  =HSSData(1,iTime2+1,isource)       
  	    if(tend.ne.tstart.and.time2.ge.tstart.and.time2.le.tend) then
          cTime2=((cend-cstart)/(tend-tstart))*(time2-tstart)+cstart
        endif
      endif      
c       
c--integrate time-averaged mass loading rate over source series
      cmtmp=0.
      tmtmp=0.
      ctmp=0.      
      do it=iTime1,iTime2 
        tstart=HSSData(1,it,  isource)
        tend  =HSSData(1,it+1,isource)
        cstart=HSSData(4+icell,it,  isource)
        cend=  HSSData(4+icell,it+1,isource)
        if(tend.lt.tstart) then
          tend=tstart
          cend=cstart
        endif
        if(time2.lt.tstart .or.time1.gt.tend) then
          cycle
        endif
c        
        if(time1.gt.tstart.and.time1.le.tend) then
          tstart=time1
          cstart=cTime1         
        endif
        if(time2.gt.tstart.and.time2.le.tend) then    
          tend=time2
          cend=cTime2         
        endif           
        cmtmp=cmtmp+0.5*(cstart+cend)*(tend-tstart)
        tmtmp=tmtmp+(tend-tstart)                                   
      enddo
      if(time2.ne.time1) ctmp=cmtmp/(time2-time1)      
C
C--RETURN
      RETURN
      END
C
C
      subroutine GetArea(ncol,nrow,nPoint,p,nSubGrid,
     & delr,xbc,delc,ybc,j,i,area,IPNTPOLY) 
c **********************************************************************
c This subroutine calculates the portion of a finite-difference cell 
C that is intersected by a polygon defined in array P of dimension 
C [nPoint]. The finite-difference cell is discritized into a subgrid
C of dimension [nSubgrid] x [nSubgrid].
c **********************************************************************    
c last modified: 01-10-2006
c
      implicit  none
      integer   ncol,nrow,nPoint,nSubgrid,j,i,nx,ny,nsub
      INTEGER   IPNTPOLY,INOUT
      real      delr,delc,xbc,ybc,area,pmin,pmax,p,subpoint,
     &          x0,y0,dx,dy
      logical   inside,l1,l2  
      dimension delr(ncol),delc(nrow),xbc(ncol),ybc(nrow), 
     &		p(2,npoint),subpoint(2),pmin(2),pmax(2)
c
      pmin(1)=0
      pmin(2)=0
      pmax(1)=xbc(ncol)+0.5*delr(ncol)
      pmax(2)=ybc(nrow)+0.5*delc(nrow)
c
      dx=delr(j)/nSubgrid
      dy=delc(i)/nSubgrid
      x0=xbc(j)-0.5*delr(j)-0.5*dx
      y0=ybc(i)-0.5*delc(i)-0.5*dy
      nsub=0
      do nx=1,nSubgrid
        subpoint(1)=x0+nx*dx
        do ny=1,nSubgrid
          subpoint(2)=y0+ny*dy
          IF(IPNTPOLY.EQ.1) THEN                               
            CALL PNPOLY(subpoint(1),subpoint(2),P,npoint,INOUT)
            L1=.FALSE.                                         
            IF(INOUT.GT.0)L1=.TRUE.                            
            L2=.TRUE.                                          
          ELSE                                                 
            l1=inside(npoint,p,subpoint,pmin)                  
            l2=inside(npoint,p,subpoint,pmax)                  
          ENDIF                                                
c          if(inside(npoint,p,subpoint,pmin) .and.             
c     &     inside(npoint,p,subpoint,pmax)) then               
          if(l1.and.l2) then
             nsub=nsub+1
	    endif
        enddo
      enddo
      if(nsub.gt.0) then
        area=float(nsub)/float(nSubgrid*nSubgrid)*
     &   delr(j)*delc(i)
      else
        area=0.
      endif
c
      return
      end
C
C
      LOGICAL FUNCTION INSIDE(NP,P,P1,P2)
C .........................................................
C This function checks whether a point P1 is inside
C a polygon defined by a [NP] number of points P.  If yes,
C the function returns a logical value .TRUE.  Otherwise,
C it returns .FAUSE.
C .........................................................
C last modified: 01-10-2006
C
      REAL P,P1,P2,PL1,PL2
      DIMENSION P(2,NP),P1(2),P2(2),PL1(2),PL2(2)
      LOGICAL CROSS
C
C--COUNT THE THE NUMBER OF INTERSECTION
C--BETWEEN THE LINE (P1,P2)
C--AND EACH LINE SEGMENT ALONG THE POLYGON
      NCOUNT=0
      NN=NP       
      DO 140 N=1,NP
      PL1(1)=P(1,N)
      PL1(2)=P(2,N)
      PL2(1)=P(1,N)
      PL2(2)=P(2,N)
      IF(.NOT.CROSS(PL1,PL2,P1,P2)) THEN
        PL2(1)=P(1,NN)
        PL2(2)=P(2,NN)
        NN=N
        IF(CROSS(PL1,PL2,P1,P2)) THEN
          NCOUNT=NCOUNT+1
        ENDIF
      ENDIF
  140 CONTINUE
C
C--IF THE INTERSECTION NUMBER IS ODD,
C--THEN THE DATA POINT IS WITHIN THE POLYGON
      IF(MOD(NCOUNT,2).NE.0) THEN
        INSIDE=.TRUE.
      ELSE
        INSIDE=.FALSE.
      ENDIF
  130 CONTINUE
  120 CONTINUE
C
C--PROGRAM COMPLETED
      RETURN
      END
C
C
      LOGICAL FUNCTION CROSS(PL1,PL2,PL3,PL4)
C...................................................
      REAL SAME           
      REAL PL1,PL2,PL3,PL4
      DIMENSION PL1(2),PL2(2),PL3(2),PL4(2)
C
      IF(SAME(PL1,PL2,PL3,PL4).lt.0. .AND.   
     & SAME(PL3,PL4,PL1,PL2).lt.0.) THEN     
        CROSS=.TRUE.
      ELSE
        CROSS=.FALSE.
      ENDIF
C
      RETURN
      END
C
C
      REAL FUNCTION SAME(PL1,PL2,P1,P2) 
C...................................................
      REAL PL1,PL2,P1,P2
      DIMENSION PL1(2),PL2(2),P1(2),P2(2)
C
      DX=PL2(1)-PL1(1)
      DY=PL2(2)-PL1(2)
      DX1=P1(1)-PL1(1)
      DY1=P1(2)-PL1(2)
      DX2=P2(1)-PL2(1)
      DY2=P2(2)-PL2(2)
      SAME=(DX*DY1-DY*DX1)*(DX*DY2-DY*DX2)
C
      RETURN
      END
C------------------------------------------------------------------------
C========FOLLOWING SUBROUTINE COPIED FROM 
C========http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
C========PNPOLY - Point Inclusion in Polygon Test
C========W. Randolph Franklin (WRF)
C        SUBROUTINE PNPOLY                                              
C                                                                       
C        PURPOSE                                                        
C           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON            
C                                                                       
C        USAGE                                                          
C           CALL PNPOLY (PX, PY, XX, YY, N, INOUT )                     
C                                                                       
C        DESCRIPTION OF THE PARAMETERS                                  
C           PX      - X-COORDINATE OF POINT IN QUESTION.                
C           PY      - Y-COORDINATE OF POINT IN QUESTION.                
C           XX      - N LONG VECTOR CONTAINING X-COORDINATES OF         
C                     VERTICES OF POLYGON.                              
C           YY      - N LONG VECTOR CONTAING Y-COORDINATES OF           
C                     VERTICES OF POLYGON.                              
C           N       - NUMBER OF VERTICES IN THE POLYGON.                
C           INOUT   - THE SIGNAL RETURNED:                              
C                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,        
C                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,     
C                      1 IF THE POINT IS INSIDE OF THE POLYGON.         
C                                                                       
C        REMARKS                                                        
C           THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.      
C           THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY           
C           OPTIONALLY BE INCREASED BY 1.                               
C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING      
C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX    
C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING   
C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.              
C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.         
C           THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM      
C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.   
C                                                                       
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  
C           NONE                                                        
C                                                                       
C        METHOD                                                         
C           A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT  
C           CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE        
C           POINT IS INSIDE OF THE POLYGON.                             
C                                                                       
C     ..................................................................
C                                                                       
      SUBROUTINE PNPOLY(PX,PY,P,N,INOUT)                            
      INTEGER N,INOUT
      REAL X(200),Y(200),P(2,N)                                    
      LOGICAL MX,MY,NX,NY                                               
      INTEGER O                                                         
C      OUTPUT UNIT FOR PRINTED MESSAGES                                 
      DATA O/6/                                                         
      MAXDIM=200                                                        
      IF(N.LE.MAXDIM)GO TO 6                                            
      WRITE(O,7)                                                        
   7  FORMAT('0WARNING:',I5,' TOO GREAT FOR THIS VERSION OF PNPOLY.     
     &          1 RESULTS INVALID') 
      RETURN                                                            
   6  DO 1 I=1,N                                                        
      X(I)=P(1,I)-PX
   1  Y(I)=P(2,I)-PY
      INOUT=-1   
      DO 2 I=1,N 
      J=1+MOD(I,N)                                                      
      MX=X(I).GE.0.0                                                    
      NX=X(J).GE.0.0                                                    
      MY=Y(I).GE.0.0                                                    
      NY=Y(J).GE.0.0                                                    
      IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX)) GO TO 2       
      IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX))) GO TO 3  
      INOUT=-INOUT                                                      
      GO TO 2                                                           
   3  IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))) 2,4,5                       
   4  INOUT=0                                                           
      RETURN                                                            
   5  INOUT=-INOUT                                                      
   2  CONTINUE                                                          
      RETURN                                                            
      END                                                               
C
C
      SUBROUTINE DSSL1AR(IN,N,MaxHSSStep,nHSSSource,IOUT)
C***********************************************************************
C ALLOCATE DISSOLUTION RELATED PARAMETERS
C***********************************************************************
      USE DSSL
      USE MT3DMS_MODULE, ONLY: iHSSLoc,NLAY,NROW,NCOL,DELR,DELC
      INTEGER IN,N,MaxHSSStep,nHSSSource
      CHARACTER*13, ALLOCATABLE :: TEXT(:),TEXT2(:),TEXT3(:)
      CHARACTER*13 DASH
C
      DASH='-------------'
C
      IF(.NOT.ASSOCIATED(NSLDPHS)) THEN
        ALLOCATE(NSLDPHS(nHSSSource),ISA(nHSSSource),
     &  CONVMOL(nHSSSource),IDSSLDOM(nHSSSource),ALDSSL(nHSSSource),
     &  PORDSS(nHSSSource),IDSSOPT(nHSSSource))
        ALLOCATE(SOLU(MaxHSSStep,nHSSSource),
     &  RNEUTRATE(MaxHSSStep,nHSSSource),SSA(MaxHSSStep,nHSSSource),
     &  WGHTMOL(MaxHSSStep,nHSSSource))
        ALLOCATE(POWR(MaxHSSStep,nHSSSource),
     &  POWR2(MaxHSSStep,nHSSSource))
        ALLOCATE(CRESNEW(MaxHSSStep,4,MAXDSSL,nHSSSource),
     &  CRESOLD(MaxHSSStep,4,MAXDSSL,nHSSSource),
     &  CRESINIT(MaxHSSStep,4,MAXDSSL,nHSSSource),
     &  CSOLNEW(MAXDSSL,nHSSSource),CSOLOLD(MAXDSSL,nHSSSource),
     &  SRCMASS(MAXDSSL,nHSSSource))
        ALLOCATE(ANION(MaxHSSStep,nHSSSource),
     &  CATION(MaxHSSStep,nHSSSource),ANION2(MaxHSSStep,nHSSSource),
     &  XB(MaxHSSStep,nHSSSource),XC(MaxHSSStep,nHSSSource))
        ALLOCATE(TEXT(MaxHSSStep),TEXT2(MaxHSSStep),TEXT3(MaxHSSStep))
        DO I=1,MaxHSSStep
          WRITE(TEXT(I),'(A8,I5)') 'SPECIES ',I
        ENDDO
        CRESOLD=0.
        CRESNEW=0.
        CSOLNEW=0.
        CSOLOLD=0.
        SRCMASS=0.
      ENDIF
C
C--READ DISSOLUTION PARAMETERS
      READ(IN,*) NSLDPHS(N),ISA(N),CONVMOL(N),IDSSLDOM(N),ALDSSL(N),
     1  PORDSS(N),IDSSOPT(N)
      NS=NSLDPHS(N)
      IF(NS.GT.MaxHSSStep) THEN
        WRITE(*,*) 'NSLDPHS EXCEEDS MaxHSSStep'
        WRITE(IOUT,*) 'NSLDPHS EXCEEDS MaxHSSStep'
        STOP
      ENDIF
      READ(IN,*) (CATION(I,N),I=1,NS)
      READ(IN,*) (ANION(I,N),I=1,NS)
      IF(IDSSOPT(N).EQ.1) READ(IN,*) (ANION2(I,N),I=1,NS)
      IF(IDSSOPT(N).EQ.1) READ(IN,*) (XB(I,N),I=1,NS)
      IF(IDSSOPT(N).EQ.1) READ(IN,*) (XC(I,N),I=1,NS)
      READ(IN,*) (WGHTMOL(I,N),I=1,NS)
      READ(IN,*) (SOLU(I,N),I=1,NS)
      READ(IN,*) (RNEUTRATE(I,N),I=1,NS)
      READ(IN,*) (SSA(I,N),I=1,NS)
      READ(IN,*) (POWR(I,N),I=1,NS)
      READ(IN,*) (POWR2(I,N),I=1,NS)
      DO II=1,3
        DO NUM=1,IDSSL(N)
          READ(IN,*) (CRESOLD(I,II,NUM,N),I=1,NS)
          NCELL=iHSSLoc(num,1,N)
          CALL NODE2KIJ(NCELL,NLAY,NROW,NCOL,KSRC,ISRC,JSRC)
          IF(II.EQ.3) THEN
            DO I=1,NS
              CSOLOLD(NUM,N)=CSOLOLD(NUM,N)+CRESOLD(I,II,NUM,N)*
     1          CONVMOL(N)
C.............CALCULATE INITIAL MASS
              SRCMASS(NUM,N)=SRCMASS(NUM,N)+CRESOLD(I,II,NUM,N)*
     1          CONVMOL(N)*
     1          DELR(JSRC)*DELC(ISRC)*THKDSS(NUM,N)*PORDSS(N)
            ENDDO
          ENDIF
          IF(II.EQ.1) THEN
            DO I=1,NS
C.............CALCULATE MASS THAT CAN POTENTIALLY EMANATE FROM ANIONS
              SRCMASS(NUM,N)=SRCMASS(NUM,N)+CRESOLD(I,II,NUM,N)*
     1          CONVMOL(N)*ANION(I,N)*
     1          DELR(JSRC)*DELC(ISRC)*THKDSS(NUM,N)*PORDSS(N)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
      IF(IDSSOPT(N).EQ.1) THEN
      DO II=4,4
        DO NUM=1,IDSSL(N)
          READ(IN,*) (CRESOLD(I,II,NUM,N),I=1,NS)
        ENDDO
      ENDDO
      ENDIF
C
      CRESINIT(:,:,:,N)=CRESOLD(:,:,:,N)
      CRESNEW(:,:,:,N)=CRESOLD(:,:,:,N)
      CSOLNEW(:,N)=CSOLOLD(:,N)
C
C--PERFORM CHECKS
      IF(IDSSLDOM(N).LT.1.OR.IDSSLDOM(N).GT.2) THEN
        WRITE(IOUT,*) 'IDSSLDOM(N) MUST BE 1 OR 2'
        WRITE(*,*) 'IDSSLDOM(N) MUST BE 1 OR 2'
        READ(*,*)
        STOP
      ENDIF
C
C--ECHO TO OUTPUT FILE
      WRITE(IOUT,12) NSLDPHS(N),ISA(N),CONVMOL(N),IDSSLDOM(N),ALDSSL(N)
      WRITE(IOUT,10) 'DISSOLUTION PARAMETERS',(TEXT(I),I=1,NS)
      WRITE(IOUT,11) (DASH,I=1,NS)
      WRITE(IOUT,14) '     NUMBER OF CATIONS',(CATION(I,N),I=1,NS)
      WRITE(IOUT,14) '      NUMBER OF ANIONS',(ANION(I,N),I=1,NS)
      WRITE(IOUT,14) 'MOLECULAR WGHT (g/mol)',(WGHTMOL(I,N),I=1,NS)
      WRITE(IOUT,14) 'EQUILIB CONST. (mol/l)',(SOLU(I,N),I=1,NS)
      WRITE(IOUT,14) '  NEUT RATE (mol/L2/T)',(RNEUTRATE(I,N),I=1,NS)
      WRITE(IOUT,14) 'SPEC SURFACE AREA (L2)',(SSA(I,N),I=1,NS)
      WRITE(IOUT,14) ' SURFACE AREA EXPONENT',(POWR(I,N),I=1,NS)
      WRITE(IOUT,14) '  EQUILIBRIUM EXPONENT',(POWR2(I,N),I=1,NS)
      WRITE(IOUT,*) 'INITIAL CONCENTRATION (mol/l):'
      DO NUM=1,IDSSL(N)
        NODE=iHSSLoc(num,1,n)
        CALL NODE2KIJ(NODE,NLAY,NROW,NCOL,K,I,J)
        WRITE(IOUT,15) 'MINERAL CONC [K,I,J] ',K,I,J,
     &  (CRESOLD(I,1,NUM,N),I=1,NS)
      ENDDO
      DO NUM=1,IDSSL(N)
        NODE=iHSSLoc(num,1,n)
        CALL NODE2KIJ(NODE,NLAY,NROW,NCOL,K,I,J)
        WRITE(IOUT,15) ' CATION CONC [K,I,J] ',K,I,J,
     &  (CRESOLD(I,2,NUM,N),I=1,NS)
      ENDDO
      DO NUM=1,IDSSL(N)
        NODE=iHSSLoc(num,1,n)
        CALL NODE2KIJ(NODE,NLAY,NROW,NCOL,K,I,J)
        WRITE(IOUT,15) '  ANION CONC [K,I,J] ',K,I,J,
     &  (CRESOLD(I,3,NUM,N),I=1,NS)
      ENDDO
      IF(IDSSOPT(N).EQ.1) THEN
      DO NUM=1,IDSSL(N)
        NODE=iHSSLoc(num,1,n)
        CALL NODE2KIJ(NODE,NLAY,NROW,NCOL,K,I,J)
        WRITE(IOUT,15) ' ANION2 CONC [K,I,J] ',K,I,J,
     &  (CRESOLD(I,4,NUM,N),I=1,NS)
      ENDDO
      ENDIF
10    FORMAT(/9X,A,7X,100(A,2X))
11    FORMAT(1X,30('-'),7X,100(A,2X))
12    FORMAT(/1X,'NUMBER OF SPECIES:                   ',I3,
     &       /1X,'OPTION FOR SURFACE AREA CALCULATION: ',I3,
     &       /1X,'MOLES TO MASS UNITS MULTIPLIER:      ',1PG13.5,
     &       /1X,'MASS FORMULATION OPTION (IDSSLDOM):  ',I3,
     &       /1X,' = 1 DIRECT IMPLEMENTATION INTO MOBILE DOMAIN',
     &       /1X,' = 2 MASS TRANSFER RATE',
     &       /1X,'MASS TRANSFER RATE COEFFICIENT:      ',1PG13.5,
     &       /1X,' (READ BUT USED ONLY IF IDSSLDOM=2)')
13    FORMAT(9X,A,7X,100(I13,2X))
14    FORMAT(9X,A,7X,100(1PG13.5,2X))
15    FORMAT(1X,A,'[',I4,',',I4,',',I4,']',100(1PG13.5,2X))
C
C--WRITE HEADER AND INITIAL CONDITIONS TO HSS OUTPUT FILE
      TEXT=''
      TEXT2=''
      TEXT3=''
      DO IPHS=1,NSLDPHS(N)
        WRITE(TEXT(IPHS),'(A9,I0)') 'MINERAL_C',IPHS
        WRITE(TEXT2(IPHS),'(A9,I0)') 'CATION_C',IPHS
        WRITE(TEXT3(IPHS),'(A9,I0)') 'ANION_C',IPHS
      ENDDO
      IF(IHSSOUT.GT.0) THEN
        WRITE(IHSSOUT,21) ' TIME','SRC#','LAY','ROW','COL',
     &  (TRIM(TEXT(I)),I=1,NS),(TRIM(TEXT2(I)),I=1,NS),
     &  'RESERVOIR_MASS'
C     &  (TRIM(TEXT3(I)),I=1,NS)
      DO NUM=1,IDSSL(N)
        NODE=iHSSLoc(NUM,1,n)
        CALL NODE2KIJ(NODE,NLAY,NROW,NCOL,K,I,J)
        WRITE(IHSSOUT,20) 0.0,N,K,I,J,
     &  ((CRESOLD(IPHS,II,NUM,N),IPHS=1,NSLDPHS(N)),
     &  II=1,2),SRCMASS(NUM,N)
      ENDDO
      ENDIF
20    FORMAT(1X,1PG13.5,1X,4(I4,1X),100(1PG13.5,2X))
21    FORMAT(1X,A13,1X,4(A5),100(A13,2X))
C
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE DSSL1FM(DT,RSOL,N,IC,ICOMP,FDSS)
C***********************************************************************
C FORMULATE FOR DISSOLUTION
C***********************************************************************
! SOLU            = equilibrium constant (moles/liter)
! RNEUTRATE       = neutral reaction rate (moles/m^2/day) --> use consistent time units with DT
! SSA             = specific surface area (m^2)
! WGHTMOL         = molecular weight (grams/mole)
! POWR            = exponent for a power function (-)
! DT              = time step length (day)
! CRESNEW(iphs,i) = reservoir concentration for species iphs
!                   index i=1 is the species concentration, eg. BaCrO4
!                   index i=2 is the cation concentration, eg. Ba++
!                   index i=3 is the anion concentration, eg. CrO4--
      USE DSSL
      USE MT3DMS_MODULE, ONLY: iHSSLoc,NLAY,NROW,NCOL,CNEW,COLD
      REAL DT,DC,RSOL
!
      NODE=iHSSLoc(IC,1,N)
      CALL NODE2KIJ(NODE,NLAY,NROW,NCOL,K,I,J)
      IF(IDSSLDOM(N).EQ.1) THEN
        CTOT=COLD(J,I,K,ICOMP)/CONVMOL(N)
      ELSEIF(IDSSLDOM(N).EQ.2) THEN
        CTOT=CSOLOLD(IC,N)/CONVMOL(N)
      ENDIF
      DC=0.
      DO IPHS=1,NSLDPHS(N)
        IF(IDSSOPT(N).EQ.1) THEN
          CRES2=0.
          CRES4=0.
          DO IPREV=1,IPHS
            CRES2=CRES2+CRESOLD(IPREV,2,IC,N)
            CRES4=CRES4+CRESOLD(IPREV,4,IC,N)
          ENDDO
          Q=CRES2*
     1      (CRES4**XB(IPHS,N))*
     1      (CTOT**XC(IPHS,N))
        ELSE
          Q=(CRESOLD(IPHS,2,IC,N)**CATION(IPHS,N))*
     1      (CTOT**ANION(IPHS,N))
        ENDIF
        SR=Q/SOLU(IPHS,N)
        RN=RNEUTRATE(IPHS,N)*(1.-(SR**POWR2(IPHS,N)))
        IF(RN.LT.0.) RN=0.
        IF(ISA(N).EQ.1) THEN
          SA=SSA(IPHS,N)*WGHTMOL(IPHS,N)*
     &    (CRESOLD(IPHS,1,IC,N)**POWR(IPHS,N))
        ELSE
          IF(CRESINIT(IPHS,1,IC,N).LE.1.0E-6) THEN
            SA=0.
          ELSE
           SA=SSA(IPHS,N)*
     &     ((CRESOLD(IPHS,1,IC,N)/CRESINIT(IPHS,1,IC,N))**POWR(IPHS,N))
          ENDIF
        ENDIF
        R=SA*RN
        DEL=R*DT
        DC=DC+MIN(CRESOLD(IPHS,1,IC,N),DEL)*ANION(IPHS,N)
      ENDDO
!
!-- moles/l CONVERT TO CONC UNITS
      IF(IDSSLDOM(N).EQ.1) THEN
        RSOL=DC*CONVMOL(N)*FDSS/DT
      ELSEIF(IDSSLDOM(N).EQ.2) THEN
        CSOLTMP=CSOLOLD(IC,N)+DC*CONVMOL(N)
        RSOL=ALDSSL(N)*(CSOLTMP-COLD(J,I,K,ICOMP))*FDSS
        IF(RSOL.LT.0.) RSOL=0.
      ENDIF
!
      RETURN
      END
!---------------------------------------------------------------
      SUBROUTINE DSSL1BD(DT,RSOL,N,IC,time2,ICOMP,FDSS)
C***********************************************************************
C CALCULATE BUDGET FOR DISSOLUTION
C***********************************************************************
! SOLU            = equilibrium constant (moles/liter)
! RNEUTRATE       = neutral reaction rate (moles/m^2/day) --> use consistent time units with DT
! SSA             = specific surface area (m^2)
! WGHTMOL         = molecular weight (grams/mole)
! POWR            = exponent for a power function (-)
! DT              = time step length (day)
! CRESNEW(iphs,i) = reservoir concentration for species iphs
!                   index i=1 is the species concentration, eg. BaCrO4
!                   index i=2 is the cation concentration, eg. Ba++
!                   index i=3 is the anion concentration, eg. CrO4--
      USE DSSL
      USE MT3DMS_MODULE, ONLY: iHSSLoc,NLAY,NROW,NCOL,CNEW,COLD
      REAL DT,DC,RSOL
!
      NODE=iHSSLoc(IC,1,N)
      CALL NODE2KIJ(NODE,NLAY,NROW,NCOL,K,I,J)
      IF(IDSSLDOM(N).EQ.1) THEN
        CTOT=COLD(J,I,K,ICOMP)/CONVMOL(N)
      ELSEIF(IDSSLDOM(N).EQ.2) THEN
        CTOT=CSOLOLD(IC,N)/CONVMOL(N)
      ENDIF
      DC=0.
      DO IPHS=1,NSLDPHS(N)
        IF(IDSSOPT(N).EQ.1) THEN
          CRES2=0.
          CRES4=0.
          DO IPREV=1,IPHS
            CRES2=CRES2+CRESOLD(IPREV,2,IC,N)
            CRES4=CRES4+CRESOLD(IPREV,4,IC,N)
          ENDDO
          Q=CRES2*
     1      (CRES4**XB(IPHS,N))*
     1      (CTOT**XC(IPHS,N))
        ELSE
          Q=(CRESOLD(IPHS,2,IC,N)**CATION(IPHS,N))*
     1      (CTOT**ANION(IPHS,N))
        ENDIF
        SR=Q/SOLU(IPHS,N)
        RN=RNEUTRATE(IPHS,N)*(1.-(SR**POWR2(IPHS,N)))
        IF(RN.LT.0.) RN=0.
        IF(ISA(N).EQ.1) THEN
          SA=SSA(IPHS,N)*WGHTMOL(IPHS,N)*
     &    (CRESOLD(IPHS,1,IC,N)**POWR(IPHS,N))
        ELSE
          IF(CRESINIT(IPHS,1,IC,N).LE.1.0E-6) THEN
            SA=0.
          ELSE
           SA=SSA(IPHS,N)*
     &     ((CRESOLD(IPHS,1,IC,N)/CRESINIT(IPHS,1,IC,N))**POWR(IPHS,N))
          ENDIF
        ENDIF
        R=SA*RN
        DEL=R*DT
        DC=DC+MIN(CRESOLD(IPHS,1,IC,N),DEL)*ANION(IPHS,N)
        DC2=MIN(CRESOLD(IPHS,1,IC,N),DEL)*CATION(IPHS,N)
        DC3=MIN(CRESOLD(IPHS,1,IC,N),DEL)*ANION(IPHS,N)
        DC4=MIN(CRESOLD(IPHS,1,IC,N),DEL)*ANION2(IPHS,N)
        CRESNEW(IPHS,1,IC,N)=CRESOLD(IPHS,1,IC,N)-
     &  MIN(CRESOLD(IPHS,1,IC,N),DEL)
        CRESNEW(IPHS,2,IC,N)=CRESOLD(IPHS,2,IC,N)+DC2
!        CRESNEW(IPHS,3,IC,N)=CRESOLD(IPHS,3,IC,N)+DC3
        CRESNEW(IPHS,4,IC,N)=CRESOLD(IPHS,4,IC,N)+DC4
      ENDDO
!
!-- moles/l CONVERT TO CONC UNITS
      IF(IDSSLDOM(N).EQ.1) THEN
        RSOL=DC*CONVMOL(N)*FDSS/DT
      ELSEIF(IDSSLDOM(N).EQ.2) THEN
        CSOLTMP=CSOLOLD(IC,N)+DC*CONVMOL(N)
        RSOL=ALDSSL(N)*(CSOLTMP-COLD(J,I,K,ICOMP))*FDSS
        IF(RSOL.LT.0.) RSOL=0.
        CSOLNEW(IC,N)=CSOLOLD(IC,N)+DC*CONVMOL(N)-RSOL*DT
      ENDIF
!
      RETURN
      END
!---------------------------------------------------------------
