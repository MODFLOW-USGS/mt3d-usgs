C
      SUBROUTINE TOB1AR(IN)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED BY THE TRANSPORT
C OBSERVATION (TOB) PACKAGE.
C **********************************************************************
C
      USE MT3DMS_MODULE, ONLY: INTOB,IOUT,NCOL,NROW,NLAY,NCOMP,
     &                         MaxConcObs,MaxFluxObs,MaxFluxCells,
     &                         inConcObs,nConcObs,iOutCobs,iConcLOG,
     &                         iConcINTP,inFluxObs,nFluxGroup,nFluxObs,
     &                         iOutFlux,inSaveObs,CScale,FScale,mLayer,
     &                         prLayer,COBSWEL,TEMP,FluxGroup,GroupData,
     &                         FOBSNAM,COBSNAM
C
      IMPLICIT  NONE
      INTEGER   IN,LLOC,IFLEN,INAM1,INAM2,ISTART,ISTOP,N,IU,kk,kp,
     &          ip,jp,nFluxTimeObs,nCells,iSSType,kc,ic,jc,icell,ig,
     &          icomp,it,ITMP,I
      REAL      TimeObs,Roff,Coff,weight,cobs,FluxObs,R,fraction
      CHARACTER LINE*200,FNAME*80,FTMP*80,NAMTMP*12,header*15,
     &          TYPESS(-1:100)*15
      INCLUDE 'openspec.inc'
C
      INTOB=IN
C
C--ALLOCATE
      ALLOCATE(MaxConcObs,MaxFluxObs,MaxFluxCells,inConcObs,nConcObs,
     &         iOutCobs,iConcLOG,iConcINTP,inFluxObs,nFluxGroup,
     &         nFluxObs,iOutFlux,inSaveObs,CScale,FScale)
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INTOB
 1030 FORMAT(/1X,'TOB1 -- TRANSPORT OBSERVATION PACKAGE,',
     & ' VERSION 1, MAY 2016, INPUT READ FROM UNIT',I3)
C
C--READ INPUT LINE AS A TEXT STRING
    1 READ(INTOB,'(A)') LINE
      IF(LINE.EQ.' ') GOTO 1
      IF(LINE(1:1).EQ.'#') THEN
        WRITE(IOUT,'(A)') LINE
        GOTO 1
      ENDIF      
C
C--DECODE INPUT LINE
      LLOC=1 
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,INTOB)
      MaxConcObs=ITMP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,INTOB)
      MaxFluxObs=ITMP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,INTOB)
      MaxFluxCells=ITMP  
C
C--PRINT MAXIMUM DIMENSIONS    
      WRITE(iout,7) MaxConcObs,MaxFluxObs,MaxFluxCells                
    7 FORMAT(1x,'MAXIMUM NUMBER OF CONC OBSERVATION WELLS =',i7,
     &      /1x,'MAXIMUM NUMBER OF MASS-FLUX OBSERVATIONS =',i7,  
     &      /1x,'MAXIMUM NUMBER OF CELLS IN A FLUX OBJECT =',i7)
C
C--SET MINIMUM DIMENSION AT ONE
      IF(MaxConcObs.lt.1) MaxConcObs=1
      IF(MaxFluxObs.lt.1) MaxFluxObs=1
      If(MaxFluxCells.lt.1) MaxFluxCells=1          
C     
      ALLOCATE(mLayer(nlay,MaxConcObs))
      ALLOCATE(prLayer(nlay,MaxConcObs))
      ALLOCATE(COBSWEL(9,MaxConcObs))
      ALLOCATE(COBSNAM(MaxConcObs))
      ALLOCATE(FluxGroup(3+2*MaxFluxCells,MaxFluxObs))
      ALLOCATE(GroupData(7,MaxFluxObs))
      ALLOCATE(FOBSNAM(MaxFluxObs))
      ALLOCATE(TEMP(max(MaxConcObs,MaxFluxObs),3))
C
C--INITIALIZE
      TYPESS(-1)='UNSUPPORTED    '
      TYPESS(1) ='CONSTANT HEAD  '
      TYPESS(2) ='WELL           '
      TYPESS(3) ='DRAIN          '
      TYPESS(4) ='RIVER          '
      TYPESS(5) ='HEAD DEP BOUND '
      TYPESS(7) ='RECHARGE       '
      TYPESS(8) ='E-T FLUX       '    
      TYPESS(15)='MASS LOADING   '
      TYPESS(21)='STREAM         '
      TYPESS(22)='RESERVOIR      '
      TYPESS(23)='SP FLW HD BOUND'
      TYPESS(24)='INTERBED STRG  '
      TYPESS(25)='TRANSIENT LEAK '
      TYPESS(26)='LAKE           '
      TYPESS(27)='MULTI-NODE WELL'
      TYPESS(28)='DRN W RET FLOW '
      TYPESS(29)='SEGMENTED ET   '
      TYPESS(50)='HSS MAS LOADING'
      TYPESS(51)='SUBSIDENCE-WT  '
      TYPESS(52)='STREAM FL ROUT.'
      TYPESS(53)='UNSAT ZONE FLOW'
C
C--READ INPUT LINE AS A TEXT STRING
    2 READ(INTOB,'(A)') LINE
      IF(LINE.EQ.' ') GOTO 2
      IF(LINE(1:1).EQ.'#') THEN
        WRITE(IOUT,'(A)') LINE
        GOTO 2
      ENDIF      
C
C--DECODE THE TOB OUTPUT FILE ROOT NAME
      LLOC=1
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INTOB)
      IFLEN=INAM2-INAM1+1
      FNAME(1:IFLEN)=LINE(INAM1:INAM2)
C
C--DECODE OUTPUT FILE UNITS      
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INTOB)
      inConcObs=IU
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INTOB)
      inFluxObs=IU  
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INTOB)
      inSaveObs=IU           
C
C--OPEN OUTPUT FILES      
      IF(inConcObs.gt.0) THEN
        ftmp=fname(1:iflen)//'.ocn'
        CALL OPENFL(inConcObs,0,ftmp,1,ftmp)          
        WRITE(inConcObs,10)
      ENDIF
      IF(inFluxObs.gt.0) THEN
        ftmp=fname(1:iflen)//'.mfx'
        CALL OPENFL(inFluxObs,0,ftmp,1,ftmp)            
        WRITE(inFluxObs,20)
      ENDIF
      IF(inSaveObs.gt.0) THEN
        header='MT3DMS_TOB_5.00'
        ftmp=fname(1:iflen)//'.pst'
        CALL OPENFL(-inSaveObs,0,ftmp,1,ftmp)        
        WRITE(inSaveObs) header
      ENDIF      
   10 FORMAT(/1x,'CONCENTRATION OBSERVATION OUTPUT FILE',
     &       /1X,37('='))   
   20 FORMAT(/1x,'MASS FLUX OBJECT OBSERVATION OUTPUT FILE',
     &       /1X,40('='))             
C
C--get input data for concentration observation wells
      IF(inConcObs.le.0) GOTO 1000
C      
      READ(intob,*) nConcObs,CScale,iOutCobs,iConcLOG,iConcINTP
      WRITE(iout,98) 
   98 FORMAT(//1x,'TRANSPORT OBSERVATION INPUT DATA'/1x,32('-'))
      WRITE(iout,100) nCOncObs,CScale,iOutCobs,iConcLOG,iConcINTP
  100 FORMAT(/1x,'NUMBER OF CONCENTRATION OBSERVATION WELLS: ',i5,
     &       /1x,'   MULTIPLIER FOR OBSERVED CONCENTRATIONS: ',g12.4,
     &       /1x,'   OPTION FOR CALCULATING RESIDUAL ERRORS: ',i5,
     &       /1x,'        OPTION FOR LOGARITHMIC CONVERSION: ',i5,
     &       /1x,'OPTION FOR INTERPOLATING CALCULATED CONC.: ',i5,
     &      //1x,'OBS_NAME     LAYER  ROW  COL. SPECIES  OBS_TIME',
     &        3x,'ROW_OFFSET',2x,'COL_OFFSET',4x,'WEIGHT',6x,'OBS_CONC',
     &       /1x,99('.'))
C      
      IF(nConcObs.gt.MaxConcObs) THEN
        WRITE(*,102)
  102   FORMAT(1x,'Number of conc. observation wells',
     &   ' exceeds specified maximum')
        CALL ustop(' ')
      ENDIF
C      
      DO n=1, nConcObs
C 
        READ(intob,*) NAMTMP,kp,ip,jp,icomp,
     &                TimeObs,Roff,Coff,weight,COBS
        WRITE(iout,120) NAMTMP,kp,ip,jp,icomp,
     &                  TimeObs,Roff,Coff,weight,COBS    
  120   FORMAT(1x,a12,3i5,2x,i5,3x,4g12.4,g14.5)
C
        COBSNAM(n)=NAMTMP   
        cobswel(1,n)=kp     
        cobswel(2,n)=ip
        cobswel(3,n)=jp     
        cobswel(4,n)=icomp  
        cobswel(5,n)=TimeObs
        cobswel(6,n)=Roff   
        cobswel(7,n)=Coff
        cobswel(8,n)=weight
        cobswel(9,n)=cobs * CScale  
C
        IF(iOutCobs.gt.0 .and. iConcLOG.gt.0) THEN
          IF(weight.gt.0 .and. Cobs.le.0) THEN
            WRITE(*,121)
  121       FORMAT(1x,'Observed conc. invalid for log conversion')
            CALL ustop(' ')
          ENDIF  
        ENDIF        
C
        IF(kp.lt.0) THEN
          READ(intob,*)   (mLayer(kk,n),prLayer(kk,n), kk=1,iABS(kp))
          WRITE(iout,122) (mLayer(kk,n),prLayer(kk,n), kk=1,iABS(kp))
        ENDIF
  122   FORMAT(1x,'  @Layer',i4,'  Proportion=',g12.4)
C 
      ENDDO
C
C--get input data for mass flux objects
 1000 IF(inFluxObs.le.0) GOTO 2000
C
      READ(intob,*) nFluxGroup,FScale,iOutFlux
      WRITE(iout,200) nFluxGroup,FScale,iOutFlux
  200 FORMAT(/1x,'           NUMBER OF MASS FLUX OBJECTS: ',i5,
     &       /1x,'   MULTIPLIER FOR OBSERVED MASS FLUXES: ',g12.4,
     &       /1x,'OPTION FOR CALCULATING RESIDUAL ERRORS: ',i5)
C      
      IF(nFluxGroup.gt.MaxFluxObs) THEN
        WRITE(*,202)
  202   FORMAT(1x,'Number of mass flux objects',
     &   ' exceeds specified maximum')
        CALL ustop(' ')        
      ENDIF 
C      
      nFluxObs=0
      DO ig=1, nFluxGroup
C
        READ(intob,*) nFluxTimeObs,nCells,iSSType
        WRITE(iout,220) ig,nFluxTimeObs,nCells,TYPESS(iSSType)
  220   FORMAT(/1x,'          MASS FLUX OBJECT NO.: ',i5,
     &         /1x,'   NUMBER OF OBSERVATION TIMES: ',i5,
     &         /1x,'NUMBER OF MODEL CELLS INCLUDED: ',i5,
     &         /1x,'      TYPE OF MASS FLUX OBJECT: ',a15,
     &        //1x,'OBS_NAME',4x,'SPECIES',2x,'OBS_TIME',
     &          5x,'WEIGHT',7x,'OBS_FLUX'/1x,55('.'))
C
        IF(nCells.gt.MaxFluxCells) THEN
          WRITE(*,222)
  222     FORMAT(1x,'Number of cells in a flux object',
     &              ' exceeds specified maximum')
          CALL ustop(' ')
        ENDIF  
        IF(iSSType.lt.0) THEN
          WRITE(*,224)
  224     FORMAT(1x,'iSSType code supported must be > 0')
          CALL ustop(' ')
        ENDIF
C                      
        FluxGroup(1,ig)=nFluxTimeObs
        FluxGroup(2,ig)=nCells       
        FluxGroup(3,ig)=iSSType
C       
        DO it=1, nFluxTimeObs
C         
          READ(intob,*) NAMTMP,icomp,TimeObs,weight,FluxObs
          WRITE(iout,240) NAMTMP,icomp,TimeObs,weight,FluxObs
  240     FORMAT(1x,a12,i5,3x,2g12.4,g14.5)
C
          nFluxObs = nFluxObs+1
C
          IF(nFluxObs.gt.MaxFluxObs) THEN
            WRITE(*,242)
  242       FORMAT(1x,'Number of total observations for ',
     &                'all mass flux objects exceeds specified maximum')
            CALL ustop(' ')
          ENDIF                   
C
          FOBSNAM(nFluxObs)=NAMTMP
          GroupData(1,nFluxObs)=ig     
          GroupData(2,nFluxObs)=icomp
          GroupData(3,nFluxObs)=TimeObs
          GroupData(4,nFluxObs)=weight 
          GroupData(5,nFluxObs)=FluxObs * FScale
C
        ENDDO
C
        DO icell=1,nCells
          READ(intob,*) kc, ic, jc, fraction
          WRITE(iout,260) kc, ic, jc, fraction
          FluxGroup(3+icell,ig)= (kc-1)*ncol*nrow + (ic-1)*ncol + jc
          FluxGroup(3+MaxFluxCells+icell,ig)=fraction
          IF(fraction.gt.1 .or. fraction.lt.0) THEN
            WRITE(*,244)
  244       FORMAT(1x,'contributing fraction for any cell',
     &                ' in a mass flux object must be between 0 and 1')
            CALL ustop(' ')
          ENDIF  
        ENDDO
  260   FORMAT(1x,'@Layer:',i4,'; Row:',i5,'; Column:',i5,
     &            '; Fraction:',f8.3)    
C
      ENDDO
C
C--normal return
 2000 CONTINUE
      RETURN     
      END
C
C  
      SUBROUTINE TOB1OT(KPER,KSTP,NTRANS,TIME1,TIME2)
C **********************************************************************
C This subroutine calls the sconcobs and smassfluxobs subroutines
C to compute observed residuals between observed and simulated 
C values
C **********************************************************************          
      USE MT3DMS_MODULE, ONLY: IOUT,inConcObs,inFluxObs
      IMPLICIT NONE
      INTEGER :: KPER,KSTP,NTRANS,IGRID
      REAL :: TIME1,TIME2
C
C--CONCENTRATION OBSERVATIONS
      IF(inConcOBS.GT.0) THEN
        CALL SConcObs(iout,kper,kstp,ntrans,time1,time2)
      ENDIF
C
C--MASS FLUX OBSERVATIONS
      IF(inFluxObs.GT.0) THEN
        CALL SMassFluxObs(iout,kper,kstp,ntrans,time1,time2)
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE SConcObs(iout,kper,kstp,ntrans,time1,time2)
C **********************************************************************
C This subroutine gets calculated concentration values at observation
C points and computes residual errors between calculated and observed
C if necessary
C **********************************************************************          
C
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,CNEW,ICBUND,DELR,
     &                         DELC,XBC,YBC,
     &                         CINACT,
     &                         nConcObs,cobswel,cobsnam,mLayer,prLayer,
     &                         temp,inSaveObs,iOutCobs,iConcLOG,
     &                         iConcINTP,INCONCOBS,TEMP
C
      IMPLICIT  NONE
      INTEGER   IOUT,ntrans,n,itmp,
     &          nobs_active,ktmp,kp,ip,jp,
     &          jlo,ilo,jhi,ihi,kstart,kend,ncal,layer,
     &          icomp,kstp,kper,iErrMsg,nobs_current
      REAL      time1,time2,errsum,abserrsum,TimeObs,Roff,Coff,weight,
     &          xx2,yy2,wlayer,cwgt,
     &          ccal,cobs,wx,wy,ctmp,error,
     &          AVE,ADEV,SDEV,VAR,CURT,R,PROB,Z,SKEW
      CHARACTER ErrMsg*24
C
C--print headers to conc obs output file
C
      WRITE(inConcObs,2) kper,kstp,ntrans,time2      
      IF(iOutCobs.gt.0) THEN
        IF(iConcLOG.eq.0) WRITE(inConcObs,4)
        IF(iConcLOG.gt.0) WRITE(inConcObs,6)
      ELSEIF(iOutCobs.eq.0) THEN
        WRITE(inConcObs,8)
      ENDIF  
    2 FORMAT(//1x,30('*'),
     &        /1x,'     STRESS PERIOD:    ',i5,
     &        /1x,'         TIME STEP:    ',i5,
     &        /1x,'    TRANSPORT STEP:    ',i5,
     &        /1x,'TOTAL ELAPSED TIME: ',g12.4, 
     &        /1x,30('*')/)           
    4 FORMAT(1x,'WELLID        X_GRID      Y_GRID     LAYER ROW COLUMN',
     &         ' SPECIES  CALCULATED   OBSERVED   WEIGHT    (CAL.-OBS.)'
     &      /1x,108('.'))  
    6 FORMAT(1x,'WELLID        X_GRID      Y_GRID     LAYER ROW COLUMN',
     &         ' SPECIES  CALCULATED   OBSERVED   WEIGHT  LogCAL-LogOBS'
     &      /1x,110('.'))
    8 FORMAT(1x,'WELLID        X_GRID      Y_GRID     LAYER ROW COLUMN',
     &          ' SPECIES  CALCULATED  '
     &      /1x,75('.'))           
C
C--reset accumulators
C
      nobs_active=0
      nobs_current=0
      errsum=0
      abserrsum=0
C
C--loop over all observation wells
C
      DO n=1,nConcObs
C     
        kp=     cobswel(1,n)
        ip=     cobswel(2,n)
        jp=     cobswel(3,n)
        icomp=  cobswel(4,n)
        TimeObs=cobswel(5,n)
        Roff=   cobswel(6,n)
        Coff=   cobswel(7,n)
        weight= cobswel(8,n)
        cobs=   cobswel(9,n)
C
        IF(TimeObs.lt.0) THEN
          itmp=-INT(TimeObs)
          IF(MOD(ntrans,itmp).ne.0) THEN
            CYCLE          !skip if not even multiple
          ELSE
            TimeObs=time2
          ENDIF  
        ELSEIF(TimeObs.le.time1 .or. TimeObs.gt.time2) THEN
          CYCLE            !skip if not at current time step
        ENDIF
C
        nobs_current=nobs_current+1
        xx2=xbc(jp)+Coff*delr(jp)
        yy2=ybc(ip)+Roff*delc(ip)
        jlo=max(1,jp-1)
        ilo=max(1,ip-1)
        IF(xx2.gt.xbc(jp)) jlo=jp
        IF(yy2.gt.ybc(ip)) ilo=ip
        jhi=min(jlo+1,ncol)
        ihi=min(ilo+1,nrow)
C
        IF(kp.gt.0) THEN
          kstart=1
          kend=1
        ELSE
          kstart=1
          kend=ABS(kp)
        ENDIF
C
        cwgt=0
        ncal=0
        DO ktmp=kstart,kend
C
          IF(kp.gt.0) THEN
            layer=kp
            wlayer=1.
          ELSEIF(kp.lt.0) THEN
            layer=mLayer(ktmp,n)
            wlayer=prLayer(ktmp,n)
          ENDIF
C
          IF(icbund(jp,ip,layer,iComp).eq.0) THEN
            wlayer=0
            ccal=cinact
          ELSE
            ncal=ncal+1
            ccal=cnew(jp,ip,layer,iComp)
C
            IF(iConcINTP.gt.0) THEN   !interpolate if requested
              IF(JLO.NE.JHI) THEN
                WX=(xx2-XBC(JLO))/(0.5*DELR(JHI)+0.5*DELR(JLO))
              ELSE
                WX=0
              ENDIF
              IF(ILO.NE.IHI) THEN
                WY=(yy2-YBC(ILO))/(0.5*DELC(IHI)+0.5*DELC(ILO))
              ELSE
                WY=0
              ENDIF
              ccal=0
              CTMP=cnew(JLO,ILO,layer,iComp)
              IF(ICBUND(JLO,ILO,layer,iComp).EQ.0) 
     &           CTMP=cnew(JP,IP,layer,iComp)
              ccal=ccal+(1.-WX)*(1.-WY)*CTMP
              CTMP=cnew(JLO,IHI,layer,iComp)
              IF(ICBUND(JLO,IHI,layer,iComp).EQ.0) 
     &           CTMP=cnew(JP,IP,layer,iComp)
              ccal=ccal+(1.-WX)*WY*CTMP
              CTMP=cnew(JHI,ILO,layer,iComp)
              IF(ICBUND(JHI,ILO,layer,iComp).EQ.0) 
     &           CTMP=cnew(JP,IP,layer,iComp)
              ccal=ccal+WX*(1.-WY)*CTMP
              CTMP=cnew(JHI,IHI,layer,iComp)
              IF(ICBUND(JHI,IHI,layer,iComp).EQ.0) 
     &           CTMP=cnew(JP,IP,layer,iComp)
              ccal=ccal+WX*WY*CTMP
            ENDIF   !end of interpolation block
C
          ENDIF
C
          cwgt = cwgt + ccal * wlayer
C
        ENDDO ! end of multi-layer weighting

        iErrMsg=0
        ErrMsg=' '
        IF(ncal.le.0) THEN              !obs at dry cell
          iErrMsg=1
          ErrMsg='obs well at a dry cell'
          cwgt=cinact
        ELSEIF(iOutCobs*iConcLOG.gt.0 .and. cwgt.le.0) THEN
          iErrMsg=2
          ErrMsg='invalid log conversion'                  
        ELSEIF(weight .lt. 0) THEN      !for well without obs conc.
          iErrMsg=-1
          ErrMsg='no observed conc given'
        ELSEIF(iOutCobs.gt.0) THEN      !active
          nobs_active=nobs_active+1
          IF(iConcLOG.le.0) THEN
            temp(nobs_active,1)=cobs
            temp(nobs_active,2)=cwgt
            error=(cwgt-cobs)*weight            
          ELSEIF(iConcLOG.gt.0) THEN
            temp(nobs_active,1)=log10(cobs)
            temp(nobs_active,2)=log10(cwgt)
            error=(log10(cwgt)-log10(cobs))*weight
          ENDIF
          errsum=errsum+error*error
          abserrsum=abserrsum+ABS(error)
          temp(nobs_active,3)=error          
        ENDIF
C
        IF(iOutCobs.gt.0 .and. iErrMsg.eq.0) THEN
          WRITE(inConcObs,30)
     &     cobsnam(n),xx2,yy2,kp,ip,jp,icomp,cwgt,cobs,weight,error
        ELSEIF(iOutCobs.gt.0 .and. iErrMsg.ne.0) THEN
          WRITE(inConcObs,32) cobsnam(n),xx2,yy2,kp,ip,jp,icomp,cwgt,
     &                        ErrMsg
        ELSEIF(iOutCobs.eq.0 .and. iErrMsg.eq.0) THEN
          WRITE(inConcObs,34) cobsnam(n),xx2,yy2,kp,ip,jp,icomp,cwgt
        ELSEIF(iOutCobs.eq.0 .and. iErrMsg.gt.0) THEN
          WRITE(inConcObs,36) cobsnam(n),xx2,yy2,kp,ip,jp,icomp,cwgt,
     &                        ErrMsg 
        ENDIF
        IF(inSaveObs.gt.0) THEN
          WRITE(inSaveObs) cobsnam(n),TimeObs,cwgt
        ENDIF  
C
      ENDDO !end of obs wel loop
C
   30 FORMAT(1x,a12,1p,2g12.4,3i5,3x,i4,2x,4g12.4)
   32 FORMAT(1x,a12,1p,2g12.4,3i5,3x,i4,2x,1g12.4,3x,a) 
   34 FORMAT(1x,a12,1p,2g12.4,3i5,3x,i4,2x,1g12.4)   
   36 FORMAT(1x,a12,1p,2g12.4,3i5,3x,i4,2x,1g12.4,3x,a)       
C
c--calculate statistics
      IF(iOutCobs.gt.0.and.nobs_active.gt.1) THEN
        CALL MOMENT(temp(:,3),nobs_active,AVE,ADEV,SDEV,VAR,SKEW,CURT)
        CALL PEARSN(temp(:,1),temp(:,2),nobs_active,R,PROB,Z)
      ELSE
        GOTO 1000
      ENDIF
C
c--print statastics
      WRITE(inConcObs,50) nobs_active
      WRITE(inConcObs,60) AVE
      WRITE(inConcObs,62) SDEV
      WRITE(inConcObs,64) abserrsum/nobs_active
      WRITE(inConcObs,66) SQRT(ERRSUM/nobs_active)     
      IF(nobs_active.gt.2) THEN
        WRITE(inConcObs,70) R
        WRITE(inConcObs,72) PROB       
      ENDIF
C
   50 FORMAT(/1x,'   NUMBER OF ACTIVE OBSERVATION POINTS = ',I5)
   60 FORMAT( 1x,'                 MEAN OF RESIDUALS (M) = ',G15.7)
   62 FORMAT( 1x,'STANDARD DEVIATION OF RESIDUALS (SDEV) = ',G15.7)
   64 FORMAT( 1x,'       MEAN OF ABSOLUTE RESIDUALS (MA) = ',G15.7)
   66 FORMAT( 1x,'     ROOT MEAN SQUARED RESIDUALS (RMS) = ',G15.7)
   70 FORMAT( 1x,'               CORRELATION COEFFICIENT = ',G15.7)
   72 FORMAT( 1x,'         PROBABILITY OF UN-CORRELATION = ',G15.7)
C
 1000 IF(nobs_current.le.0) WRITE(inConcObs,1080)
 1080 FORMAT(1x,'[No obs wells active at current transport step]')
C
C--normal return
      RETURN
      END
C
C
      SUBROUTINE SMassFluxObs(iout,kper,kstp,ntrans,time1,time2)
C **********************************************************************
C This subroutine gets calculated mass fluxes at user specified
C locations and computes residual errors between calculated and
C observed if necessary
C **********************************************************************
C
      USE MT3DMS_MODULE, ONLY: ncol,nrow,nlay,ncomp,CNEW,ICBUND,
     &                         delr,delc,dh,
     &                         FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &                         FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,
     &                         FETS,FSWT,FSFR,FUZF,
     &                         mxss,ntss,ss,ssmc,irch,rech,crch,
     &                         ievt,evtr,cevt,
     &                         INFLUXOBS,MaxFluxCells,nFLuxGroup,
     &                         nFLuxObs,FluxGroup,GroupData,fobsnam,
     &                         grouptmp=>TEMP,inSaveObs,iOutFlux
C
      IMPLICIT  NONE
      INTEGER   ntrans,iout,n,iGroup,nFluxTimeObs,
     &          nobs_active,icell,nCells,iSSType,it,inode,k,i,j,
     &          num,IQ,kp,ip,jp,icomp,kstp,kper,
     &          issLink,iErrMsg,itmp,nobs_current
      REAL      time1,time2,TimeObs,
     &          FluxObs,FluxCal,qss,csink,ctmp,error,
     &          QC,Q,weight,errsum,abserrsum,
     &          SKEW,AVE,ADEV,SDEV,VAR,CURT,R,PROB,Z,fraction
      CHARACTER ErrMsg*24
c
c--print headers to output file
c    
      WRITE(inFluxObs,2) kper,kstp,ntrans,time2
      IF(iOutFlux.gt.0) THEN
        WRITE(inFluxObs,4) 
      ELSEIF(iOutFlux.eq.0) THEN
        WRITE(inFluxObs,6)
      ENDIF
    2 FORMAT(//1x,30('*'),
     &        /1x,'     STRESS PERIOD:    ',i5,
     &        /1x,'         TIME STEP:    ',i5,
     &        /1x,'    TRANSPORT STEP:    ',i5,
     &        /1x,'TOTAL ELAPSED TIME: ',g12.4,     
     &        /1x,30('*')/)                
    4 FORMAT(1x,'  NO. NAME            TIME   SPECIES      ',
     &          'CALCULATED   OBSERVED   WEIGHT     (CAL.-OBS.)'/1x,
     &          88('.'))  
    6 FORMAT(1x,'  NO. NAME            TIME   SPECIES      ',
     &          'CALCULATED  '/1x,54('.'))   
C
c--reset accumulators
C
      nobs_active=0
      nobs_current=0
      errsum=0
      abserrsum=0            
C
c--clear temporary storage arrays
C
      DO n=1,nfluxobs       
        GroupTmp(n,1)=0.       
        GroupTmp(n,2)=0.       
        GroupTmp(n,3)=0.
      ENDDO
C
c--loop through all mass flux observations
C
      DO n=1,nfluxobs
C
        iGroup= GroupData(1,n)
        iComp=  GroupData(2,n)
        TimeObs=GroupData(3,n)         
        weight =GroupData(4,n)
        FluxObs=GroupData(5,n)
        GroupData(6,n)=0.
        GroupData(7,n)=0.
C
        IF(TimeObs.lt.0) THEN
          itmp=-INT(TimeObs)
          IF(MOD(ntrans,itmp).ne.0) THEN
            CYCLE                   !skip if not even multiple
          ELSE
            TimeObs=time2
          ENDIF
        ELSEIF(TimeObs.le.time1 .or. TimeObs.gt.time2) THEN
          CYCLE                     !skip if not at current time step
        ENDIF        
C
  100   nFluxTimeObs=FluxGroup(1,iGroup)
        nCells=FluxGroup(2,iGroup)
        iSSType=FluxGroup(3,iGroup)
C
c--loop through all cells in the current flux object
C
        DO icell=1,nCells
C
          inode=INT( FluxGroup(3+icell,iGroup) )
          kp = (inode-1) / (ncol*nrow) + 1
          ip = MOD((inode-1),ncol*nrow)/ncol + 1
          jp = MOD((inode-1),ncol) + 1
          fraction=FluxGroup(3+MaxFluxCells+icell,iGroup)
C
c--if recharge flux
C
          IF(iSSType.eq.7 .and. FRCH) THEN
            IF(kp.ne.irch(jp,ip)) CYCLE
            IF(icbund(jp,ip,kp,icomp).le.0) CYCLE
            ctmp=crch(jp,ip,icomp)
            qss=rech(jp,ip)
            IF(qss.lt.0) ctmp=cnew(jp,ip,kp,icomp)
c--get volumetric Q*C and Q                      
            QC=qss*delr(jp)*delc(ip)*dh(jp,ip,kp)*ctmp
            Q =qss*delr(jp)*delc(ip)*dh(jp,ip,kp)
c--cumulate in GroupData
            GroupData(6,n) = GroupData(6,n) + QC * fraction
            GroupData(7,n) = GroupData(7,n) + Q  * fraction 
            CYCLE
          ENDIF              
C
c--if evapotranspiration flux
C
          IF(iSSType.eq.8 .and. (FEVT.or.FETS) ) THEN                 
            IF(kp.ne.ievt(jp,ip)) CYCLE
            IF(icbund(jp,ip,kp,icomp).le.0) CYCLE
            ctmp=cevt(jp,ip,icomp)
            qss=evtr(jp,ip)
            IF(qss.lt.0 .and. (ctmp.lt.0. or. 
     &                 ctmp.ge.cnew(jp,ip,kp,icomp))) THEN
              ctmp=cnew(jp,ip,kp,icomp)
            ELSEIF(ctmp.lt.0) THEN
              ctmp=0.
            ENDIF  
c--get volumetric Q*C and Q            
            QC=qss*delr(jp)*delc(ip)*dh(jp,ip,kp)*ctmp
            Q =qss*delr(jp)*delc(ip)*dh(jp,ip,kp)            
c--cumulate in GroupData
            GroupData(6,n) = GroupData(6,n) + QC * fraction
            GroupData(7,n) = GroupData(7,n) + Q  * fraction 
            CYCLE
          ENDIF              
C
c--if point sinks/sources
C
          DO num=1,ntss
            k=ss(1,num)
            i=ss(2,num)
            j=ss(3,num)
            ctmp=ss(4,num)
            IF(icomp.gt.1) ctmp=ssmc(icomp,num)
            qss=ss(5,num)
            IF(qss.lt.0) ctmp=cnew(j,i,k,icomp)
            IQ=ss(6,num)
            issLink=ss(7,num)            
c--skip if not same ss type
            IF(iSSType.ne.IQ) CYCLE            
c--skip if not same cell
            IF(kp.ne.k .or. ip.ne.i .or. jp.ne.j) CYCLE    
c--skip if not an active cell
            IF(icbund(j,i,k,icomp).le.0) CYCLE         
c--skip if at a linked group sink/source   
            IF(issLink.gt.0) CYCLE                
c--get volumetric Q*C and Q            
            QC=qss*delr(j)*delc(i)*dh(j,i,k)*ctmp
            Q =qss*delr(j)*delc(i)*dh(j,i,k)            
c--cumulate in GroupData
            GroupData(6,n) = GroupData(6,n) + QC * fraction
            GroupData(7,n) = GroupData(7,n) + Q  * fraction            
          ENDDO      !end of point sink/source loop
C
        ENDDO   !end of the cell loop in the current flux object
C
        nobs_current=nobs_current+1
        iErrMsg=0
        ErrMsg=' '
        fluxcal=GroupData(6,n)
        IF(weight.lt.0) THEN
          iErrMsg=-1
          ErrMsg='no observed flux given'          
        ELSEIF(iOutFlux.gt.0) THEN
          nobs_active=nobs_active+1
          grouptmp(nobs_active,1)=fluxobs
          grouptmp(nobs_active,2)=fluxcal
          error=(fluxcal-fluxobs)*weight
          errsum=errsum+error*error
          abserrsum=abserrsum+ABS(error)
          grouptmp(nobs_active,3)=error                    
        ENDIF  
C
        IF(iOutFlux.gt.0 .and. iErrMsg.eq.0) THEN          
          WRITE(inFluxObs,30) iGroup,fobsnam(n),timeobs,
     &                        icomp,fluxcal,fluxobs,weight,error
        ELSEIF(iOutFlux.gt.0 .and. iErrMsg.ne.0) THEN           
          WRITE(inFluxObs,32) iGroup,fobsnam(n),timeobs,
     &                        icomp,fluxcal,ErrMsg
        ELSEIF(iOutFlux.eq.0) THEN
          WRITE(inFluxObs,34) iGroup,fobsnam(n),timeobs,
     &                        icomp,fluxcal
        ENDIF       
        IF(inSaveObs.gt.0) THEN
          WRITE(inSaveObs) fobsnam(n),TimeObs,fluxcal
        ENDIF
C       
      ENDDO  !end of mass flux object loop
C    
   30 FORMAT(1x,i4,2x,a12,1p,g12.4,i4,6x,4g12.4)
   32 FORMAT(1x,i4,2x,a12,1p,g12.4,i4,6x,1g12.4,3x,a)
   34 FORMAT(1x,i4,2x,a12,1p,g12.4,i4,6x,1g12.4)
c
c--calculate statistics
c
      IF(iOutFlux.gt.0.and.nobs_active.gt.1) THEN
        CALL MOMENT(grouptmp(:,3),nobs_active,AVE,ADEV,
     &              SDEV,VAR,SKEW,CURT)
        CALL PEARSN(grouptmp(:,1),grouptmp(:,2),
     &              nobs_active,R,PROB,Z)
      ELSE
        GOTO 1000
      ENDIF
c
c--print statastics
c
      WRITE(inFluxObs,50) nobs_active
      WRITE(inFluxObs,60) AVE
      WRITE(inFluxObs,62) SDEV
      WRITE(inFluxObs,64) abserrsum/nobs_active
      WRITE(inFluxObs,66) SQRT(ERRSUM/nobs_active)      
      IF(nobs_active.gt.2) THEN
        WRITE(inFluxObs,70) R
        WRITE(inFluxObs,72) PROB     
      ENDIF
   50 FORMAT(/1x,'   NUMBER OF ACTIVE OBSERVATION POINTS = ',I5)
   60 FORMAT( 1x,'                 MEAN OF RESIDUALS (M) = ',G15.7)
   62 FORMAT( 1x,'STANDARD DEVIATION OF RESIDUALS (SDEV) = ',G15.7)
   64 FORMAT( 1x,'       MEAN OF ABSOLUTE RESIDUALS (MA) = ',G15.7)
   66 FORMAT( 1x,'     ROOT MEAN SQUARED RESIDUALS (RMS) = ',G15.7)
   70 FORMAT( 1x,'               CORRELATION COEFFICIENT = ',G15.7)
   72 FORMAT( 1x,'         PROBABILITY OF UN-CORRELATION = ',G15.7)
C
 1000 IF(nobs_current.le.0) WRITE(inFluxObs,1080)
 1080 FORMAT(1x,'[No flux object active at current transport step]')
C
c--normal return
      RETURN
      END
C
C
      SUBROUTINE MOMENT(DATA,N,AVE,ADEV,SDEV,VAR,SKEW,CURT)
C *****************************************************************
C This subroutine computes mean, variance, skewness, and kurtosis 
C for an array of data points DATA(n).
C *****************************************************************
C modified from Press et al. (1992)
C      
      IMPLICIT NONE
      INTEGER n,j
      REAL adev,ave,curt,sdev,skew,var,data,p,s,ep
      DIMENSION DATA(n)
C      
      IF(n.le.1) THEN
        CALL ustop('N must be at least 2 in subroutine MOMENT'//
     &             ' used by TOB Package')
      ENDIF        
      s=0.
      DO j=1,n
        s=s+DATA(j)
      ENDDO
      ave=s/n
      adev=0.
      var=0.
      skew=0.
      curt=0.
      ep=0.      
      DO j=1,n
        s=DATA(j)-ave
        ep=ep+s
        adev=adev+ABS(s)
        p=s*s
        var=var+p
        p=p*s
        skew=skew+p
        p=p*s
        curt=curt+p
      ENDDO      
      adev=adev/n
      var=(var-ep**2/n)/(n-1)
      sdev=SQRT(var)
      IF(var.ne.0.)THEN
        skew=skew/(n*sdev**3)
        curt=curt/(n*var**2)-3.
      ELSE
CZ      pause 'no skew or kurtosis when zero variance in moment'
      ENDIF      
C          
      RETURN
      END
C
C
      SUBROUTINE PEARSN(X,Y,N,R,PROB,Z)
C *************************************************************
C modified from Press et al. (1992)
C     
      IMPLICIT NONE
      REAL,PARAMETER :: TINY=1.e-20
      INTEGER n,j
      REAL prob,r,z,x,y,ax,ay,df,sxx,sxy,syy,t,xt,yt,betai
      DIMENSION x(n),y(n)
C     
      IF(n.le.2) GOTO 1   !added by CZ
C      
      ax=0.
      ay=0.
      DO j=1,n
        ax=ax+x(j)
        ay=ay+y(j)
      ENDDO
      ax=ax/n
      ay=ay/n
      sxx=0.
      syy=0.
      sxy=0.      
      DO j=1,n
        xt=x(j)-ax
        yt=y(j)-ay
        sxx=sxx+xt**2
        syy=syy+yt**2
        sxy=sxy+xt*yt
      ENDDO
      r=sxy/(SQRT(sxx*syy) + TINY)   !TINY added by CZ
      z=0.5*LOG(((1.+r)+TINY)/((1.-r)+TINY))
      df=n-2     
      t=r*SQRT(df/(((1.-r)+TINY)*((1.+r)+TINY)))
      prob=betai(0.5*df,0.5,df/(df+t**2+TINY))  !TINY added by CZ
C     prob=erfcc(abs(z*sqrt(n-1.))/1.4142136)

    1 RETURN
      END
C
C
      FUNCTION BETAI(A,B,X)
C ********************************************************
C modified from Press et al. (1992)
C 
      IMPLICIT NONE     
      REAL betai,a,b,x,bt,betacf,gammln
C      
      IF(x.lt.0. .or. x.gt.1.) THEN
        CALL ustop('Bad argument x in subroutine BETAI'//
     &             ' used by TOB Package')
      ENDIF  
      IF(x.eq.0. .or. x.eq.1.) THEN
        bt=0.
      ELSE
        bt=EXP(gammln(a+b)-gammln(a)-gammln(b)
     &     +a*LOG(x)+b*LOG(1.-x))
      ENDIF
      IF(x.lt.(a+1.)/(a+b+2.)) THEN
        betai=bt*betacf(a,b,x)/a
        return
      ELSE
        betai=1.-bt*betacf(b,a,1.-x)/b
        return
      ENDIF
C      
      END
C
C
      FUNCTION BETACF(A,B,X)
C ********************************************************
C modified from Press et al. (1992)
C      
      IMPLICIT NONE
      INTEGER,PARAMETER :: MAXIT=100
      REAL,PARAMETER :: EPS=3.e-7,FPMIN=1.e-30
      INTEGER m,m2            
      REAL betacf,a,b,x,
     & aa,c,d,del,h,qab,qam,qap
C     
      qab=a+b
      qap=a+1.
      qam=a-1.
      c=1.
      d=1.-qab*x/qap
      IF(ABS(d).lt.FPMIN) d=FPMIN
      d=1./d
      h=d
      DO m=1,MAXIT
        m2=2*m
        aa=m*(b-m)*x/((qam+m2)*(a+m2))
        d=1.+aa*d
        IF(ABS(d).lt.FPMIN) d=FPMIN
        c=1.+aa/c
        IF(ABS(c).lt.FPMIN) c=FPMIN
        d=1./d
        h=h*d*c
        aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
        d=1.+aa*d
        IF(ABS(d).lt.FPMIN) d=FPMIN
        c=1.+aa/c
        IF(ABS(c).lt.FPMIN) c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        IF(ABS(del-1.).lt.EPS) GOTO 1
      ENDDO      
      CALL ustop('a or b too big, or MAXIT too small'//
     &           ' in subroutine BETACF used by TOB Package')
    1 betacf=h
C
      RETURN
      END
C
C
      FUNCTION GAMMLN(XX)
C *******************************************************************
C modified from Press et al. (1992)
C      
      IMPLICIT NONE
      INTEGER j
      REAL gammln,xx
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
C     
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*LOG(tmp)-tmp
      ser=1.000000000190015d0
      DO j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
      ENDDO
      gammln=tmp+LOG(stp*ser/x)
C      
      RETURN
      END