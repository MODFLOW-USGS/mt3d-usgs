C
      SUBROUTINE HSS5AR(IN)
C ********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED BY THE HSSM-MT3DMS
C INTERFACE (HSS) PACKAGE.
C ********************************************************************
C Last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: INHSS,IOUT,NCOL,NROW,NLAY,NCOMP,ICBUND,
     &                         DELR,DELC,XBC,YBC,
     &                         MaxHSSSource,MaxHSSCells,MaxHSSStep,
     &                         nHSSSource,iRunHSSM,faclength,factime,
     &                         facmass,iHSSLoc,HSSData,HSSNAM,
     &                         IHSSGEN,P                       !# LINE 4 HSS
C
      IMPLICIT  NONE
      INTEGER   IN,LLOC,ISTART,ISTOP,ITMP,inam1,inam2
      REAL      R
      REAL      YMAX                                           !# LINE 118 HSS
      CHARACTER LINE*200
C      integer,parameter :: nPoint=51, nSubGrid=25             !# Amended (LINE 109 HSS)
      INTEGER   nPoint,nSubGrid,IPNTPOLY                       !# LINE 110 HSS
      INTEGER   N,IU,
     &          IFLEN,inHSSFile,i,j,k,
     &          it,iHSSComp,num,
     &          jSource,iSource,kSource,iStep,NStep,nr,inode
      REAL      time,radius_lnapl,
     &          sourcemassflux,r_distance,
     &          area_cell,area_source,area_total,degree
      CHARACTER SourceName*12,HSSFileName*200
C      DIMENSION p(2,nPoint)                                   !# Amended (LINE 124 HSS)
      DOUBLE PRECISION PI                                      !# LINE 125 HSS
      PARAMETER (PI=3.141592653589793238D0)                    !# LINE 126 HSS
C     REAL, ALLOCATABLE :: P(:,:)                              !# LINE 127 HSS
C
      INHSS=IN
C
C--ALLOCATE
      ALLOCATE(MaxHSSSource,MaxHSSCells,MaxHSSStep,nHSSSource,iRunHSSM,
     &         faclength,factime,facmass,IHSSGEN)
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
      IHSSGEN=0                                                !# LINE 47 HSS
      ITMP=0                                                   !# LINE 48 HSS
      CALL URWORD(LINE,LLOC,inam1,inam2, 1,ITMP,R,IOUT,INHSS)  !# LINE 49 HSS
      IF(LINE(inam1:inam2).EQ.'POLYGON') THEN                  !# LINE 50 HSS
        IHSSGEN=1                                              !# LINE 51 HSS
      ELSEIF(LINE(inam1:inam2).EQ.'IRREGULAR') THEN            !# LINE 52 HSS
        IHSSGEN=2                                              !# LINE 53 HSS
      ENDIF                                                    !# LINE 54 HSS
C
C--ECHO INPUT VARIABLES
      WRITE(IOUT,9) MaxHSSSource,MaxHSSCells,MaxHSSStep
    9 FORMAT(1X,'MAXIMUM NO. OF HSS SOURCES =',I4,
     &      /1X,'MAXIMUM NO. OF MODEL CELLS FOR A HSS SOURCE =',I4,
     &      /1X,'MAXIMUM NO. OF TIME STEPS DEFINING A HSS SOURCE =',I4)
      IF(IHSSGEN.EQ.0) THEN                                    !# LINE 61 HSS
        WRITE(IOUT,*) 'SOURCE SHAPE IS AN APPROXIMATE CIRCLE'  !# LINE 62 HSS
      ELSEIF(IHSSGEN.EQ.1) THEN                                !# LINE 63 HSS
        WRITE(IOUT,*) 'SOURCE SHAPE IS A REGULAR POLYGON'      !# LINE 64 HSS
      ELSEIF(IHSSGEN.EQ.2) THEN                                !# LINE 65 HSS
        WRITE(IOUT,*) 'SOURCE SHAPE IS AN IRREGULAR POLYGON'   !# LINE 66 HSS
      ELSE                                                     !# LINE 67 HSS
        WRITE(IOUT,*) 'ERROR IN HSS PACKAGE: IHSSGEN'          !# LINE 68 HSS
        WRITE(*,*) 'ERROR IN HSS PACKAGE: IHSSGEN'             !# LINE 69 HSS
        READ(*,*)                                              !# LINE 70 HSS
        STOP                                                   !# LINE 71 HSS
      ENDIF                                                    !# LINE 72 HSS
C                                                              !# LINE 73 HSS
C--ALLOCATE AND INITIALIZE
      ALLOCATE(iHSSLoc(MaxHSSCells,MaxHSSStep,MaxHSSSource))
      ALLOCATE(HSSData(4+MaxHSSCells,MaxHSSStep,MaxHSSSource))
      ALLOCATE(HSSNAM(MaxHSSSource))
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
     &//1x,'LENGTH UNIT CONVERSION FACTOR FROM HSSM TO MT3DMS =',G12.4,
     & /1x,'TIME UNIT CONVERSION FACTOR FROM HSSM TO MT3DMS   =',G12.4,
     & /1x,'MASS UNIT CONVERSION FACTOR FROM HSSM TO MT3DMS   =',G12.4)
   12 format(/1x,'TOTAL NUMBER OF HSS SOURCES IN THIS SIMULATION =',I4)
C   
      if(nHSSSource.gt.MaxHSSSource) then
        call ustop ('[MaxHSSSource] exceeded!')
      endif   
C   
      IF(IHSSGEN.EQ.2) THEN                                    !# LINE 145 HSS
        YMAX=0.                                                !# LINE 146 HSS
        DO I=1,NROW                                            !# LINE 147 HSS
          YMAX=YMAX+DELC(I)                                    !# LINE 148 HSS
        ENDDO                                                  !# LINE 149 HSS
      ENDIF                                                    !# LINE 150 HSS
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
        IF(IHSSGEN.EQ.1) THEN                                         !# LINE 174 HSS
          READ(inhss,*)  ksource,isource,jsource,iHSSComp,sourcename, !# Amended (LINE 175 HSS)
     &    nPoint,nSubGrid                                             !# LINE 176 HSS
        ELSEIF(IHSSGEN.EQ.2) THEN                                     !# LINE 177 HSS
          READ(inhss,*)  ksource,iHSSComp,sourcename,                 !# LINE 178 HSS
     &    nPoint,nSubGrid                                             !# LINE 179 HSS
        ELSE                                                          !# LINE 180 HSS
          nPoint=51                                                   !# LINE 181 HSS
          nSubGrid=25                                                 !# LINE 182 HSS
          READ(inhss,*)  ksource,isource,jsource,iHSSComp,sourcename  !# LINE 183 HSS
        ENDIF                                                         !# LINE 184 HSS
        ALLOCATE (P(2,NPOINT))                                        !# LINE 185 HSS
C 
        HSSNAM(n)=sourcename
C                                                                     !# LINE 188 HSS
        IPNTPOLY=0                                                    !# LINE 189 HSS
        IF(nSubGrid.LT.0) THEN                                        !# LINE 190 HSS
          nSubGrid=-nSubGrid                                          !# LINE 191 HSS
          IPNTPOLY=1                                                  !# LINE 192 HSS
        ENDIF                                                         !# LINE 193 HSS
C                                                                     !# LINE 194 HSS
        IF(IHSSGEN.EQ.1) THEN                                         !# LINE 195 HSS
        write(iout,31) ksource,isource,jsource,iHSSComp,sourcename,   !# LINE 196 HSS
     &    nPoint,nSubGrid                                             !# LINE 197 HSS
   31   format(1x,'[Layer,Row,Column]:',3i5,'; Species:',i3,          !# LINE 198 HSS
     &        /1x,'Source Name: ',A,' Poly Points: ',I3,' Subgrids',I3) !# LINE 199 HSS
        ELSEIF(IHSSGEN.EQ.2) THEN                                     !# LINE 200 HSS
        write(iout,32) ksource,iHSSComp,sourcename,                   !# LINE 201 HSS
     &    nPoint,nSubGrid                                             !# LINE 202 HSS
   32   format(1x,'[Layer]:',i5,'; Species:',i3,                      !# LINE 203 HSS
     &        /1x,'Source Name: ',A,' Poly Points: ',I3,' Subgrids',I3) !# LINE 204 HSS
        ELSE                                                          !# LINE 205 HSS
          write(iout,30) ksource,isource,jsource,iHSSComp,sourcename
   30     format(1x,'[Layer,Row,Column]:',3i5,'; Species:',i3,
     &          /1x,'Source Name: ',A)                 
        ENDIF                                                         !# LINE 209 HSS
C                                                                     !# LINE 210 HSS
C-----READ POLYGON VERTICES 1:nPoint                                  !# LINE 211 HSS
        IF(IHSSGEN.EQ.2) THEN                                         !# LINE 212 HSS
          WRITE(iout,*) 'X Y MODEL COORDINATES (Y REVERSED)'          !# LINE 213 HSS
          DO nr=1,nPoint                                              !# LINE 214 HSS
            READ(inhss,*) p(1,nr),p(2,nr)                             !# LINE 215 HSS
C                                                                     !# LINE 216 HSS
C-----------REVERSE Y                                                 !# LINE 217 HSS
            p(2,nr)=YMAX-p(2,nr)                                      !# LINE 218 HSS
C                                                                     !# LINE 219 HSS
            WRITE(iout,*) p(1,nr),p(2,nr)                             !# LINE 220 HSS
          ENDDO                                                       !# LINE 221 HSS
        ENDIF                                                         !# LINE 222 HSS
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
            IF(IHSSGEN.EQ.1) THEN                              !# LINE 268 HSS
              area_source=radius_lnapl*radius_lnapl*nPoint*    !# LINE 269 HSS
     &        sin(2.0*PI/nPoint)/2.0                           !# LINE 270 HSS
            ELSEIF(IHSSGEN.EQ.2) THEN                          !# LINE 271 HSS
              area_source=0.                                   !# LINE 272 HSS
              DO nr=1,nPoint                                   !# LINE 273 HSS
                IF(nr.EQ.nPoint) THEN                          !# LINE 274 HSS
                  area_source=area_source+                     !# LINE 275 HSS
     &            p(1,nr)*p(2,1)-p(2,nr)*p(1,1)                !# LINE 276 HSS
                ELSE                                           !# LINE 277 HSS
                  area_source=area_source+                     !# LINE 278 HSS
     &            p(1,nr)*p(2,nr+1)-p(2,nr)*p(1,nr+1)          !# LINE 279 HSS
                ENDIF                                          !# LINE 280 HSS
              ENDDO                                            !# LINE 281 HSS
              area_source=ABS(area_source/2.0)                 !# LINE 282 HSS
            ELSE                                               !# LINE 283 HSS
              area_source=PI*radius_lnapl**2                   !# Amended (LINE 284 HSS)
            ENDIF                                              !# LINE 285 HSS
          ENDIF                                                !# LINE 286 HSS
c
c distribute to starting source cell
          IF(IHSSGEN.NE.2) THEN                                !# LINE 289 HSS
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
              degree=(nr-1)*2.*PI/nPoint                      !# Amended (LINE 289 HSS)
              p(1,nr)=xbc(jsource)+radius_lnapl*cos(degree)            
              p(2,nr)=ybc(isource)+radius_lnapl*sin(degree)
            ENDDO
          ENDIF                                               !# LINE 307 HSS
c                            
          num=0
          area_total=0	    
          do i=1,nrow
            do j=1,ncol
              IF(I.EQ.16.AND.J.EQ.15) THEN                    !# LINE 313 HSS
                CONTINUE                                      !# LINE 314 HSS
              ENDIF                                           !# LINE 315 HSS
              IF(IHSSGEN.NE.2) THEN                           !# LINE 316 HSS
                R=
     &          sqrt((xbc(j)-xbc(jsource))**2+(ybc(i)-ybc(isource))**2)
                R=R-0.5*sqrt(delr(j)**2+delc(i)**2)
                if(R.gt.1.5*radius_lnapl) cycle  !1.5 is a safety factor
              ENDIF                                           !# LINE 321 HSS
              call GetArea(ncol,nrow,nPoint,p,nSubGrid,delr,xbc,
     &         delc,ybc,j,i,area_cell,IPNTPOLY)               !# Amended (LINE 323 HSS)
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
        DEALLOCATE (p)                                         !# LINE 350 HSS
C        
      ENDDO         !done with all HSS sources
C
C--normal return
 1000 CONTINUE
      RETURN
      END
C
C  
      Subroutine HSS5FM(ICOMP,ICBUND,time1,time2)
C **********************************************************************
C THIS SUBROUTINE FORMULATES MATRIX COEFFICIENTS FOR THE HSS SOURCE
C TERM UNDER THE IMPLICIT FINITE-DIFFERENCE METHOD.
C **********************************************************************          
C last modified: 02-20-2010
C
      USE MIN_SAT, ONLY : QC7
      USE MT3DMS_MODULE, ONLY:NCOL,NROW,NLAY,NCOMP,MIXELM,UPDLHS,
     &                        MaxHSSSource,MaxHSSStep,MaxHSSCells,
     &                        nHSSSource,HSSData,iHSSLoc,
     &                        A,RHS,NODES,IDRY2
C
      IMPLICIT  NONE
      INTEGER   ICOMP,ICBUND,is,it,icell,IGRID,
     &          N,iStep,iHSSComp
      INTEGER K,I,J
      REAL      ctmp,ctmp1,ctmp2,tstart,tend,time1,time2
      DIMENSION ICBUND(NODES,NCOMP)
C
C--FORMULATE [RHS] MATRIX FOR EULERIAN & EULERIAN-LAGRANGIAN SCHEMES     
      DO is=1,nHSSSource     
c
        iHSSComp=int(HSSData(4,1,is))
        if(iHSSComp.ne.ICOMP) cycle               
c        
  666   DO icell=1,MaxHSSCells                   
          N=iHSSLoc(icell,1,is)
          IF(N.EQ.0) CYCLE                                     !# LINE 387 HSS
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
C
      ENDDO        
C
C--RETURN
      RETURN
      END
C
C  
      Subroutine HSS5BD(ICOMP,ICBUND,IQ,time1,time2,DTRANS)
C **********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGETS ASSOCIATED WITH THE HSS
C SOURCE TERM.
C **********************************************************************          
C last modified: 02-20-2010
C
      USE MIN_SAT, ONLY: QC7
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,NODES,RMASIO,
     &                         MaxHSSSource,MaxHSSCells,MaxHSSStep,
     &                         nHSSSource,iRunHSSM,faclength,factime,
     &                         facmass,iHSSLoc,HSSData,HSSNAM,IDRY2
C
      IMPLICIT  NONE      
      INTEGER   ICOMP,ICBUND,is,it,icell,N,iStep,IGRID,
     &          iHSSComp,IQ    !IQ is iSSType for HSS source
      INTEGER K,I,J
      REAL      DTRANS,ctmp,ctmp1,ctmp2,tstart,tend,time1,time2
      DIMENSION ICBUND(NODES,NCOMP)
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
            IF(N.EQ.0) CYCLE                                   !# LINE 437 HSS
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
     & delr,xbc,delc,ybc,j,i,area,IPNTPOLY)                    !# LINE 552 HSS
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
      INTEGER   IPNTPOLY,INOUT                                 !# LINE 568 HSS
      real      delr,delc,xbc,ybc,area,pmin,pmax,p,subpoint,
     &          x0,y0,dx,dy
      logical   inside,l1,l2                                   !# LINE 565 HSS
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
          IF(IPNTPOLY.EQ.1) THEN                                !# LINE 594 HSS 
            CALL PNPOLY(subpoint(1),subpoint(2),P,npoint,INOUT) !# LINE 595 HSS 
            L1=.FALSE.                                          !# LINE 596 HSS 
            IF(INOUT.GT.0)L1=.TRUE.                             !# LINE 597 HSS 
            L2=.TRUE.                                           !# LINE 598 HSS 
          ELSE                                                  !# LINE 599 HSS 
            l1=inside(npoint,p,subpoint,pmin)                   !# LINE 600 HSS 
            l2=inside(npoint,p,subpoint,pmax)                   !# LINE 601 HSS 
          ENDIF                                                 !# LINE 602 HSS 
c          if(inside(npoint,p,subpoint,pmin) .and.              !# Amended (LINE 603 HSS) 
c     &     inside(npoint,p,subpoint,pmax)) then                !# Amended (LINE 604 HSS) 
          if(l1.and.l2) then                                    !# LINE 605 HSS
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
      REAL P,P1,P2,PL1,PL2                                     !# LINE 630 HSS
      DIMENSION P(2,NP),P1(2),P2(2),PL1(2),PL2(2)
      LOGICAL CROSS
C
C--COUNT THE THE NUMBER OF INTERSECTION
C--BETWEEN THE LINE (P1,P2)
C--AND EACH LINE SEGMENT ALONG THE POLYGON
      NCOUNT=0
      NN=NP                                                    !# LINE 638 HSS
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
      REAL SAME                                                !# LINE 671 HSS
      REAL PL1,PL2,PL3,PL4                                     !# LINE 672 HSS
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
      REAL FUNCTION SAME(PL1,PL2,P1,P2)                        !# Amended 686 HSS
C...................................................
      REAL PL1,PL2,P1,P2                                       !# LINE 688 HSS
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
     &          1RESULTS INVALID') 
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

