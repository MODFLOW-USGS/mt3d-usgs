C
      SUBROUTINE HSS5AL(INHSS,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & MaxHSSSource,MaxHSSCells,MaxHSSStep,
     & LCHSSData,LCHSSLoc,iRunHSSM)
C ********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED BY THE HSSM-MT3DMS
C INTERFACE (HSS) PACKAGE.
C ********************************************************************
C Last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   INHSS,IOUT,ISUM,ISUM2,ISOLD,ISOLD2,ISUMX,ISUMIX,
     &          NCOL,NROW,NLAY,MaxHSSSource,MaxHSSCells,MaxHSSStep,
     &          LCHSSData,LCHSSLoc,LLOC,ISTART,ISTOP,ITMP,
     &          inam1,inam2,iRunHSSM
      REAL      R
      CHARACTER LINE*200
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INHSS
 1030 FORMAT(1X,'HSS5 -- MT3DMS-HSSM INTERFACE PACAKGE,',
     & ' VERSION 5, FEBRUARY 2010, INPUT READ FROM UNIT ',I5)
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
C
C--ECHO INPUT VARIABLES
      WRITE(IOUT,10) MaxHSSSource,MaxHSSCells,MaxHSSStep
   10 FORMAT(1X,'MAXIMUM NO. OF HSS SOURCES =',I4,
     &      /1X,'MAXIMUM NO. OF MODEL CELLS FOR A HSS SOURCE =',I4,
     &      /1X,'MAXIMUM NO. OF TIME STEPS DEFINING A HSS SOURCE =',I4)
C     
C--ALLOCATE SPACE FOR ARRAYS
      ISOLD=ISUM
      ISOLD2=ISUM2
C
C--INTEGER ARRAYS
      LCHSSLoc=ISUM2
      ISUM2=ISUM2+MaxHSSCells*MaxHSSStep*MaxHSSSource
C
C--REAL ARRAYS
      LCHSSData=ISUM
      ISUM=ISUM+(4+MaxHSSCells)*MaxHSSStep*MaxHSSSource
C
C--CHECK HOW MANY ELEMENTS OF THE X AND IX ARRAYS ARE USED
      ISUMX=ISUM-ISOLD
      ISUMIX=ISUM2-ISOLD2
      WRITE(IOUT,1090) ISUMX,ISUMIX
 1090 FORMAT(1X,I10,' ELEMENTS OF THE  X ARRAY USED BY THE HSS PACKAGE',
     &  /1X,I10,' ELEMENTS OF THE IX ARRAY USED BY THE HSS PACKAGE'/)
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE HSS5RP(INHSS,IOUT,NCOL,NROW,NLAY,NCOMP,ICBUND,
     & DELR,DELC,XBC,YBC,MaxHSSSource,MaxHSSCells,MaxHSSStep,nHSSSource,
     & faclength,factime,facmass,HSSData,iHSSLoc,HSSNAM,iRunHSSM)
C **********************************************************************
C This subroutine reads input data for the HSS package
C **********************************************************************
C last modified: 02-20-2010               
C
!hss  DLL_IMPORT HSSM            
      IMPLICIT  NONE
      integer,parameter :: nPoint=51, nSubGrid=25
      INTEGER   INHSS,IOUT,NCOL,NROW,NLAY,LLOC,INAM1,INAM2,N,IU,
     &          ISTART,ISTOP,IFLEN,nHSSSource,inHSSFile,i,j,k,
     &          MaxHSSSource,MaxHSSCells,MaxHSSStep,iHSSLoc,
     &          it,icbund,ncomp,iHSSComp,num,iRunHSSM,
     &          jSource,iSource,kSource,iStep,NStep,nr,itmp,inode
      REAL      HSSData,faclength,factime,facmass,time,radius_lnapl,
     &          sourcemassflux,r_distance,delr,delc,xbc,ybc,R,
     &          area_cell,area_source,area_total,degree,p
      CHARACTER HSSNAM*12,SourceName*12,LINE*200,HSSFileName*200               
      DIMENSION ICBUND(ncol,nrow,nlay,ncomp),DELR(ncol),DELC(nrow),
     &          iHSSLoc(MaxHSSCells,MaxHSSStep,MaxHSSSource),
     &          HSSData(4+MaxHSSCells,MaxHSSStep,MaxHSSSource),
     &          HSSNAM(MaxHSSSource),XBC(ncol),YBC(nrow),
     &          p(2,nPoint)         
C
C--READ INPUT DATA
      read(inhss,*)  faclength,factime,facmass
      write(iout,10) faclength,factime,facmass
      read(inhss,*)  nHSSSource
      write(iout,12) nHSSSource
   10 format(//1x,'INPUT DATA FOR HSS LNAPL SOURCES',
     & /1x,       '--------------------------------',
     &//1x,'LENGTH UNIT CONVERSION FACTOR FROM HSSM TO MT3DMS =',G12.4,
     & /1x,'TIME UNIT CONVERSION FACTOR FROM HSSM TO MT3DMS   =',G12.4,
     & /1x,'MASS UNIT CONVERSION FACTOR FROM HSSM TO MT3DMS   =',G12.4)
   12 format(/1x,'TOTAL NUMBER OF HSS SOURCES IN THIS SIMULATION =',I4)
C   
      if(nHSSSource.gt.MaxHSSSource) then
        call ustop ('[MaxHSSSource] exceeded!')
      endif   
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
C
C--ECHO INPUT DATA
        write(iout,20) n,HSSFileName(1:IFLEN),inHSSFile
   20   format(/1x,'HSS Source No. ',I4.4,
     &         /1x,'Source Definition File Name: ',a,
     &         /1x,'Source Definition File Read from Unit: ',i4)
C
C--GET HSS SOURCE LOCATION AND NAME
        READ(inhss,*)  ksource,isource,jsource,iHSSComp,sourcename
C 
        HSSNAM(n)=sourcename
        write(iout,30) ksource,isource,jsource,iHSSComp,sourcename
   30   format(1x,'[Layer,Row,Column]:',3i5,'; Species:',i3,
     &        /1x,'Source Name: ',A)                 
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
            area_source=3.14159*radius_lnapl**2
          endif      
c
c distribute to starting source cell
          num=1
          iHSSLoc(num,iStep,n)=
     &     (ksource-1)*ncol*nrow+(isource-1)*ncol+jsource
          area_cell=delr(jsource)*delc(isource)
          if(area_source .lt. area_cell) then
            HSSData(4+num,iStep,n)=HSSData(3,iStep,n)
            write(iout,120) HSSData(1,iStep,n),
     &       ksource,isource,jsource,HSSData(4+num,iStep,n)
            cycle
          endif
c
c distribute to multiple cells            
          do nr=1,nPoint     !discretize source perimeter into polygon
            degree=(nr-1)*2.*3.14159/nPoint
            p(1,nr)=xbc(jsource)+radius_lnapl*cos(degree)            
            p(2,nr)=ybc(isource)+radius_lnapl*sin(degree)
          enddo              
c                            
          num=0
          area_total=0	    
          do i=1,nrow
            do j=1,ncol
              R=sqrt((xbc(j)-xbc(jsource))**2+(ybc(i)-ybc(isource))**2)
              R=R-0.5*sqrt(delr(j)**2+delc(i)**2)
              if(R.gt.1.5*radius_lnapl) cycle  !1.5 is a safety factor             			 
              call GetArea(ncol,nrow,nPoint,p,nSubGrid,delr,xbc,
     &         delc,ybc,j,i,area_cell)
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
C        
      ENDDO         !done with all HSS sources
C
C--normal return
 1000 CONTINUE
      RETURN
      END
C
C  
      Subroutine HSS5FM(NCOL,NROW,NLAY,NCOMP,ICOMP,MIXELM,UPDLHS,
     & MaxHSSSource,MaxHSSStep,MaxHSSCells,nHSSSource,time1,time2,
     & ICBUND,A,RHS,NODES,HSSData,iHSSLoc)
C **********************************************************************
C THIS SUBROUTINE FORMULATES MATRIX COEFFICIENTS FOR THE HSS SOURCE
C TERM UNDER THE IMPLICIT FINITE-DIFFERENCE METHOD.
C **********************************************************************          
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,is,it,icell,iHSSLoc,
     &          N,NODES,MIXELM,iStep,MaxHSSSource,
     &          MaxHSSCells,MaxHSSStep,nHSSSource,iHSSComp
      REAL      A,RHS,ctmp,ctmp1,ctmp2,tstart,tend,time1,time2,HSSData
      LOGICAL   UPDLHS
      DIMENSION ICBUND(NODES,NCOMP),A(NODES),RHS(NODES),
     &          HSSData(4+MaxHSSCells,MaxHSSStep,MaxHSSSource),
     &          iHSSLoc(MaxHSSCells,MaxHSSStep,MaxHSSSource)
C
C--FORMULATE [RHS] MATRIX FOR EULERIAN & EULERIAN-LAGRANGIAN SCHEMES     
      DO is=1,nHSSSource     
c
        iHSSComp=int(HSSData(4,1,is))
        if(iHSSComp.ne.ICOMP) cycle               
c        
  666   DO icell=1,MaxHSSCells                   
          N=iHSSLoc(icell,1,is)         
          IF(ICBUND(N,ICOMP).le.0) cycle
c          
          CALL TVSource(NCOL,NROW,NLAY,iStep,iCell,is,
     &     MaxHSSSource,MaxHSSStep,MaxHSSCells,time1,time2,
     &     HSSData,CTMP)     
c
c--add contribution to [RHS] if ctmp is positive
          IF(CTMP.LT.0) THEN
            CALL USTOP('Error in HSS Source Definition File!')
          ELSE
            RHS(N)=RHS(N)-CTMP
          ENDIF             
        ENDDO         
C
      ENDDO        
C
C--RETURN
      RETURN
      END
C
C  
      Subroutine HSS5BD(NCOL,NROW,NLAY,NCOMP,ICOMP,NODES,ICBUND,
     & MaxHSSSource,MaxHSSStep,MaxHSSCells,nHSSSource,IQ,
     & time1,time2,HSSData,iHSSLoc,RMASIO,DTRANS)
C **********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGETS ASSOCIATED WITH THE HSS
C SOURCE TERM.
C **********************************************************************          
C last modified: 02-20-2010
C
      IMPLICIT  NONE      
      INTEGER   NCOL,NROW,NLAY,NCOMP,ICOMP,ICBUND,NODES,MaxHSSStep,
     &          MaxHSSSource,MaxHSSCells,is,it,icell,iHSSLoc,N,iStep,
     &          nHSSSource,iHSSComp,IQ    !IQ is iSSType for HSS source
      REAL      DTRANS,ctmp,ctmp1,ctmp2,tstart,tend,time1,time2,
     &          RMASIO,HSSData
      DIMENSION ICBUND(NODES,NCOMP),RMASIO(122,2,NCOMP),
     &          HSSData(4+MaxHSSCells,MaxHSSStep,MaxHSSSource),
     &          iHSSLoc(MaxHSSCells,MaxHSSStep,MaxHSSSource)           
C
C--LOOP over all HSS_LNAPL sources
        DO is=1,nHSSSource               
c	  
          iHSSComp=int(HSSData(4,1,is))
	        if(iHSSComp.ne.ICOMP) cycle            
c
  666     DO icell=1,MaxHSSCells                
c  
            N=iHSSLoc(icell,1,is)                         
            IF(ICBUND(N,icomp).le.0) cycle
c            
            CALL TVSource(NCOL,NROW,NLAY,iStep,iCell,is,
     &       MaxHSSSource,MaxHSSStep,MaxHSSCells,time1,time2,
     &       HSSData,CTMP)            
c
            IF(CTMP.GT.0) THEN
              RMASIO(IQ,1,ICOMP)=RMASIO(IQ,1,ICOMP)+CTMP*DTRANS     
            ELSE
              RMASIO(IQ,2,ICOMP)=RMASIO(IQ,2,ICOMP)+CTMP*DTRANS    
            ENDIF                     
          ENDDO
C
        ENDDO  
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
     & delr,xbc,delc,ybc,j,i,area)
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
      real      delr,delc,xbc,ybc,area,pmin,pmax,p,subpoint,
     &          x0,y0,dx,dy
      logical   inside
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
          if(inside(npoint,p,subpoint,pmin) .and.
     &     inside(npoint,p,subpoint,pmax)) then
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
      DIMENSION P(2,NP),P1(2),P2(2),PL1(2),PL2(2)
      LOGICAL CROSS
C
C--COUNT THE THE NUMBER OF INTERSECTION
C--BETWEEN THE LINE (P1,P2)
C--AND EACH LINE SEGMENT ALONG THE POLYGON
      NCOUNT=0
      NN=1
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
      FUNCTION SAME(PL1,PL2,P1,P2)
C...................................................
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