C
      SUBROUTINE TOB5AL(INTOB,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & MaxConcObs,MaxFluxObs,MaxFluxCells,
     & LCMLAYER,LCCOBS,LCPRLAYER,LCTEMP,LCFLUXGROUP,LCGROUPDATA)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED BY THE TRANSPORT
C OBSERVATION (TOB) PACKAGE.
C **********************************************************************
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   INTOB,IOUT,ISUM,ISUM2,ISOLD,ISOLD2,ISUMX,ISUMIX,
     &          NCOL,NROW,NLAY,MaxConcObs,MaxFluxObs,MaxFluxCells,
     &          LCMLAYER,LCCOBS,LCPRLAYER,LCTEMP,
     &          LCFLUXGROUP,LCGROUPDATA,LLOC,ISTART,ISTOP,ITMP
      REAL      R
      CHARACTER LINE*200
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INTOB
 1030 FORMAT(1X,'TOB5 -- TRANSPORT OBSERVATION PACKAGE,',
     & ' VERSION 5, FEBRUARY 2010, INPUT READ FROM UNIT',I3)
C
C--READ INPUT LINE AS A TEXT STRING
    2 READ(INTOB,'(A)') LINE
      IF(LINE.EQ.' ') GOTO 2
      IF(LINE(1:1).EQ.'#') THEN
        WRITE(IOUT,'(A)') LINE
        GOTO 2
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
      WRITE(iout,10) MaxConcObs,MaxFluxObs,MaxFluxCells                
   10 FORMAT(1x,'MAXIMUM NUMBER OF CONC OBSERVATION WELLS =',i7,
     &      /1x,'MAXIMUM NUMBER OF MASS-FLUX OBSERVATIONS =',i7,  
     &      /1x,'MAXIMUM NUMBER OF CELLS IN A FLUX OBJECT =',i7)
C
C--SET MINIMUM DIMENSION AT ONE
      IF(MaxConcObs.lt.1) MaxConcObs=1
      IF(MaxFluxObs.lt.1) MaxFluxObs=1
      If(MaxFluxCells.lt.1) MaxFluxCells=1          
C     
C--ALLOCATE SPACE FOR ARRAYS
      ISOLD=ISUM
      ISOLD2=ISUM2
C
C--INTEGER ARRAYS
      LCMLAYER=ISUM2
      ISUM2=ISUM2+NLAY*MaxConcObs
C
C--REAL ARRAYS
      LCCOBS=ISUM
      ISUM=ISUM+9*MaxConcObs
      LCPRLAYER=ISUM
      ISUM=ISUM+NLAY*MaxConcObs
      LCTEMP=ISUM
      ISUM=ISUM+3*max(MaxConcObs,MaxFluxObs)
      LCFluxGroup=ISUM
      ISUM=ISUM+(3+2*MaxFluxCells)*MaxFluxObs
      LCGroupData=ISUM
      ISUM=ISUM+7*MaxFluxObs
C
C--CHECK HOW MANY ELEMENTS OF THE X AND IX ARRAYS ARE USED
      ISUMX=ISUM-ISOLD
      ISUMIX=ISUM2-ISOLD2
      WRITE(IOUT,1090) ISUMX,ISUMIX
 1090 FORMAT(1X,I10,' ELEMENTS OF THE  X ARRAY USED BY THE TOB PACKAGE',
     &  /1X,I10,' ELEMENTS OF THE IX ARRAY USED BY THE TOB PACKAGE'/)
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE TOB5RP(INTOB,IOUT,NCOL,NROW,NLAY,NCOMP,MaxConcObs,
     & MaxFluxObs,MaxFluxCells,inConcObs,nConcObs,CScale,iOutCobs,
     & iConcLOG,iConcINTP,COBSNAM,COBSWEL,mLAYER,prLAYER,TEMP,
     & inFluxObs,nFluxGroup,nFluxObs,FScale,iOutFlux,inSaveObs,
     & FOBSNAM,FLUXGROUP,GROUPDATA)
C *********************************************************************
C THIS SUBROUTINE READS INPUT DATA for THE TOB PACKAGE.
C *********************************************************************
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   INTOB,IOUT,LLOC,IFLEN,INAM1,INAM2,ISTART,ISTOP,N,
     &          NCOL,NROW,NLAY,inConcObs,inFluxObs,inSaveObs,IU,
     &          nConcObs,MaxConcObs,iOutCobs,iConcLOG,iConcINTP,
     &          kk,kp,ip,jp,mLayer,MaxFluxObs,MaxFluxCells,
     &          nFluxGroup,iOutFlux,nFluxTimeObs,nCells,iSSType,
     &          nFluxObs,kc,ic,jc,icell,ig,icomp,ncomp,it
      REAL      CScale,COBSWEL,FScale,TimeObs,Roff,Coff,weight,
     &          temp,prLayer,cobs,FluxTimeObs,FluxObs,
     &          FluxGroup,GroupData,R,fraction
      CHARACTER LINE*200,FNAME*80,FTMP*80,NAMTMP*12,header*15,
     &          COBSNAM*12,FOBSNAM*12,TYPESS(-1:100)*15              
      DIMENSION mLayer(nlay,MaxConcObs),prLayer(nlay,MaxConcObs),
     &          COBSWEL(9,MaxConcObs),COBSNAM(MaxConcObs),
     &          FluxGroup(3+2*MaxFluxCells,MaxFluxObs),
     &          GroupData(7,MaxFluxObs),FOBSNAM(MaxFluxObs)
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
      if(inConcObs.gt.0) then
        ftmp=fname(1:iflen)//'.ocn'
        CALL OPENFL(inConcObs,0,ftmp,1,ftmp)          
        write(inConcObs,10)
      endif
      if(inFluxObs.gt.0) then
        ftmp=fname(1:iflen)//'.mfx'
        CALL OPENFL(inFluxObs,0,ftmp,1,ftmp)            
        write(inFluxObs,20)
      endif
      if(inSaveObs.gt.0) then
        header='MT3DMS_TOB_5.00'
        ftmp=fname(1:iflen)//'.pst'
        CALL OPENFL(-inSaveObs,0,ftmp,1,ftmp)        
        write(inSaveObs) header
      endif      
   10 format(/1x,'CONCENTRATION OBSERVATION OUTPUT FILE',
     &       /1X,37('='))   
   20 format(/1x,'MASS FLUX OBJECT OBSERVATION OUTPUT FILE',
     &       /1X,40('='))             
C
C--get input data for concentration observation wells
      if(inConcObs.le.0) goto 1000
C      
      read(intob,*) nConcObs,CScale,iOutCobs,iConcLOG,iConcINTP
      write(iout,98) 
   98 format(//1x,'TRANSPORT OBSERVATION INPUT DATA'/1x,32('-'))
      write(iout,100) nCOncObs,CScale,iOutCobs,iConcLOG,iConcINTP
  100 format(/1x,'NUMBER OF CONCENTRATION OBSERVATION WELLS: ',i5,
     &       /1x,'   MULTIPLIER FOR OBSERVED CONCENTRATIONS: ',g12.4,
     &       /1x,'   OPTION FOR CALCULATING RESIDUAL ERRORS: ',i5,
     &       /1x,'        OPTION FOR LOGARITHMIC CONVERSION: ',i5,
     &       /1x,'OPTION FOR INTERPOLATING CALCULATED CONC.: ',i5,
     &      //1x,'OBS_NAME     LAYER  ROW  COL. SPECIES  OBS_TIME',
     &        3x,'ROW_OFFSET',2x,'COL_OFFSET',4x,'WEIGHT',6x,'OBS_CONC',
     &       /1x,99('.'))
C      
      IF(nConcObs.gt.MaxConcObs) then
        write(*,102)
  102   format(1x,'Number of conc. observation wells',
     &   ' exceeds specified maximum')
        call ustop(' ')
      ENDIF
C      
      DO n=1, nConcObs
      
        read(intob,*) NAMTMP,kp,ip,jp,icomp,
     &                TimeObs,Roff,Coff,weight,COBS
        write(iout,120) NAMTMP,kp,ip,jp,icomp,
     &                  TimeObs,Roff,Coff,weight,COBS    
  120   format(1x,a12,3i5,2x,i5,3x,4g12.4,g14.5)
  
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

        IF(iOutCobs.gt.0 .and. iConcLOG.gt.0) then
          if(weight.gt.0 .and. Cobs.le.0) then
            write(*,121)
  121       format(1x,'Observed conc. invalid for log conversion')
            call ustop(' ')
          endif  
        ENDIF        
     
        if(kp.lt.0) then
          read(intob,*)   (mLayer(kk,n),prLayer(kk,n), kk=1,iabs(kp))          
          write(iout,122) (mLayer(kk,n),prLayer(kk,n), kk=1,iabs(kp))
        endif
  122   format(1x,'  @Layer',i4,'  Proportion=',g12.4)                    
        
      ENDDO
C
C--get input data for mass flux objects
 1000 if(inFluxObs.le.0) goto 2000
C
      read(intob,*) nFluxGroup,FScale,iOutFlux
      write(iout,200) nFluxGroup,FScale,iOutFlux
  200 format(/1x,'           NUMBER OF MASS FLUX OBJECTS: ',i5,
     &       /1x,'   MULTIPLIER FOR OBSERVED MASS FLUXES: ',g12.4,
     &       /1x,'OPTION FOR CALCULATING RESIDUAL ERRORS: ',i5)
C      
      if(nFluxGroup.gt.MaxFluxObs) then
        write(*,202)
  202   format(1x,'Number of mass flux objects',
     &   ' exceeds specified maximum')
        call ustop(' ')        
      endif 
C      
      nFluxObs=0
      DO ig=1, nFluxGroup
      
        read(intob,*) nFluxTimeObs,nCells,iSSType
        write(iout,220) ig,nFluxTimeObs,nCells,TYPESS(iSSType)
  220   format(/1x,'          MASS FLUX OBJECT NO.: ',i5,
     &         /1x,'   NUMBER OF OBSERVATION TIMES: ',i5,
     &         /1x,'NUMBER OF MODEL CELLS INCLUDED: ',i5,
     &         /1x,'      TYPE OF MASS FLUX OBJECT: ',a15,
     &        //1x,'OBS_NAME',4x,'SPECIES',2x,'OBS_TIME',
     &          5x,'WEIGHT',7x,'OBS_FLUX'/1x,55('.'))
        
        if(nCells.gt.MaxFluxCells) then
          write(*,222)
  222     format(1x,'Number of cells in a flux object',
     &     ' exceeds specified maximum')
          call ustop(' ')
        endif  
        if(iSSType.lt.0) then
          write(*,224)
  224     format(1x,'iSSType code supported must be > 0')
          call ustop(' ')
        endif
                      
        FluxGroup(1,ig)=nFluxTimeObs
        FluxGroup(2,ig)=nCells       
        FluxGroup(3,ig)=iSSType
        
        do it=1, nFluxTimeObs
          
          read(intob,*) NAMTMP,icomp,TimeObs,weight,FluxObs
          write(iout,240) NAMTMP,icomp,TimeObs,weight,FluxObs
  240     format(1x,a12,i5,3x,2g12.4,g14.5)
      
          nFluxObs = nFluxObs+1
          
          if(nFluxObs.gt.MaxFluxObs) then
            write(*,242)
  242       format(1x,'Number of total observations for',
     &       ' all mass flux objects exceeds specified maximum')
            call ustop(' ')
          endif                   
          
          FOBSNAM(nFluxObs)=NAMTMP
          GroupData(1,nFluxObs)=ig     
          GroupData(2,nFluxObs)=icomp
          GroupData(3,nFluxObs)=TimeObs
          GroupData(4,nFluxObs)=weight 
          GroupData(5,nFluxObs)=FluxObs * FScale
          
        enddo
                  
        do icell=1,nCells
          read(intob,*) kc, ic, jc, fraction
          write(iout,260) kc, ic, jc, fraction
          FluxGroup(3+icell,ig)= (kc-1)*ncol*nrow + (ic-1)*ncol + jc
          FluxGroup(3+MaxFluxCells+icell,ig)=fraction
          if(fraction.gt.1 .or. fraction.lt.0) then
            write(*,244)
  244       format(1x,'contributing fraction for any cell',
     &       ' in a mass flux object must be between 0 and 1')
            call ustop(' ')
          endif  
        enddo
  260   format(1x,'@Layer:',i4,'; Row:',i5,'; Column:',i5,
     &   '; Fraction:',f8.3)    
        
      ENDDO
C
C--normal return
 2000 CONTINUE
      RETURN     
      END
C
C  
      Subroutine ConcObs(inConcObs,iout,ncol,nrow,nlay,ncomp,kper,kstp,
     & ntrans,time1,time2,cnew,cinact,icbund,delr,delc,xbc,ybc,
     & nConcObs,cobswel,cobsnam,mLayer,prLayer,temp,inSaveObs,
     & iOutCobs,iConcLOG,iConcINTP)
C **********************************************************************
C This subroutine gets calculated concentration values at observation
C points and computes residual errors between calculated and observed
C if necessary
C **********************************************************************          
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   IOUT,NCOL,NROW,NLAY,ntrans,n,inConcObs,nConcObs,itmp,
     &          nobs_active,iOutCobs,iConcLOG,iConcINTP,ktmp,kp,ip,jp,
     &          jlo,ilo,jhi,ihi,kstart,kend,ncal,layer,mLayer,
     &          icbund,icomp,ncomp,kstp,kper,inSaveObs,iErrMsg,
     &          nobs_current
      REAL      time1,time2,errsum,abserrsum,TimeObs,Roff,Coff,weight,
     &          xx2,yy2,xbc,ybc,delr,delc,wlayer,cnew,cobswel,cwgt,
     &          ccal,cobs,prLayer,wx,wy,ctmp,temp,error,cinact,
     &          AVE,ADEV,SDEV,VAR,CURT,R,PROB,Z,SKEW
      CHARACTER cobsnam*12,ErrMsg*24
      DIMENSION cobswel(9,nConcObs),cobsnam(nConcObs),temp(nConcObs,3),
     &          mLayer(nlay,nConcObs),prLayer(nlay,nConcObs),
     &          cnew(ncol,nrow,nlay,ncomp),icbund(ncol,nrow,nlay,ncomp),
     &          delr(ncol),delc(nrow),xbc(ncol),ybc(nrow)
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
    2 format(//1x,30('*'),
     &        /1x,'     STRESS PERIOD:    ',i5,
     &        /1x,'         TIME STEP:    ',i5,
     &        /1x,'    TRANSPORT STEP:    ',i5,
     &        /1x,'TOTAL ELAPSED TIME: ',g12.4, 
     &        /1x,30('*')/)           
    4 format(1x,'WELLID        X_GRID      Y_GRID     LAYER ROW COLUMN',
     & ' SPECIES  CALCULATED   OBSERVED   WEIGHT    (CAL.-OBS.)'
     &      /1x,108('.'))  
    6 format(1x,'WELLID        X_GRID      Y_GRID     LAYER ROW COLUMN',
     & ' SPECIES  CALCULATED   OBSERVED   WEIGHT  LogCAL-LogOBS'
     &      /1x,110('.'))
    8 format(1x,'WELLID        X_GRID      Y_GRID     LAYER ROW COLUMN',
     & ' SPECIES  CALCULATED  '
     &      /1x,75('.'))           
    
C--reset accumulators

      nobs_active=0
      nobs_current=0
      errsum=0
      abserrsum=0

C--loop over all observation wells

      do n=1,nConcObs
      
        kp=     cobswel(1,n)
        ip=     cobswel(2,n)
        jp=     cobswel(3,n)
        icomp=  cobswel(4,n)
        TimeObs=cobswel(5,n)
        Roff=   cobswel(6,n)
        Coff=   cobswel(7,n)
        weight= cobswel(8,n)
        cobs=   cobswel(9,n)

        if(TimeObs.lt.0) then
          itmp=-int(TimeObs)
          if(mod(ntrans,itmp).ne.0) then
            cycle          !skip if not even multiple
          else
            TimeObs=time2
          endif  
        elseif(TimeObs.le.time1 .or. TimeObs.gt.time2) then
          cycle            !skip if not at current time step
        endif

        nobs_current=nobs_current+1
        xx2=xbc(jp)+Coff*delr(jp)
        yy2=ybc(ip)+Roff*delc(ip)
        jlo=max(1,jp-1)
        ilo=max(1,ip-1)
        if(xx2.gt.xbc(jp)) jlo=jp
        if(yy2.gt.ybc(ip)) ilo=ip
        jhi=min(jlo+1,ncol)
        ihi=min(ilo+1,nrow)

        if(kp.gt.0) then
          kstart=1
          kend=1
        else
          kstart=1
          kend=abs(kp)
        endif

        cwgt=0
        ncal=0
        do ktmp=kstart,kend

          if(kp.gt.0) then
            layer=kp
            wlayer=1.
          elseif(kp.lt.0) then
            layer=mLayer(ktmp,n)
            wlayer=prLayer(ktmp,n)
          endif

          if(icbund(jp,ip,layer,iComp).eq.0) then
            wlayer=0
            ccal=cinact
          else
            ncal=ncal+1
            ccal=cnew(jp,ip,layer,iComp)

            IF(iConcINTP.gt.0) then   !interpolate if requested
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
     &                     CTMP=cnew(JP,IP,layer,iComp)
              ccal=ccal+(1.-WX)*(1.-WY)*CTMP
              CTMP=cnew(JLO,IHI,layer,iComp)
              IF(ICBUND(JLO,IHI,layer,iComp).EQ.0) 
     &                     CTMP=cnew(JP,IP,layer,iComp)
              ccal=ccal+(1.-WX)*WY*CTMP
              CTMP=cnew(JHI,ILO,layer,iComp)
              IF(ICBUND(JHI,ILO,layer,iComp).EQ.0) 
     &                     CTMP=cnew(JP,IP,layer,iComp)
              ccal=ccal+WX*(1.-WY)*CTMP
              CTMP=cnew(JHI,IHI,layer,iComp)
              IF(ICBUND(JHI,IHI,layer,iComp).EQ.0) 
     &                     CTMP=cnew(JP,IP,layer,iComp)
              ccal=ccal+WX*WY*CTMP
            endif   !end of interpolation block

          endif

          cwgt = cwgt + ccal * wlayer

        enddo ! end of multi-layer weighting

        iErrMsg=0
        ErrMsg=' '
        if(ncal.le.0) then              !obs at dry cell
          iErrMsg=1
          ErrMsg='obs well at a dry cell'
          cwgt=cinact
        elseif(iOutCobs*iConcLOG.gt.0 .and. cwgt.le.0) then
          iErrMsg=2
          ErrMsg='invalid log conversion'                  
        elseif(weight .lt. 0) then      !for well without obs conc.
          iErrMsg=-1
          ErrMsg='no observed conc given'
        elseif(iOutCobs.gt.0) then      !active
          nobs_active=nobs_active+1
          if(iConcLOG.le.0) then
            temp(nobs_active,1)=cobs
            temp(nobs_active,2)=cwgt
            error=(cwgt-cobs)*weight            
          elseif(iConcLOG.gt.0) then
            temp(nobs_active,1)=log10(cobs)
            temp(nobs_active,2)=log10(cwgt)
            error=(log10(cwgt)-log10(cobs))*weight
          endif
          errsum=errsum+error*error
          abserrsum=abserrsum+abs(error)
          temp(nobs_active,3)=error          
        endif
        
        if(iOutCobs.gt.0 .and. iErrMsg.eq.0) then
          write(inConcObs,30)
     &     cobsnam(n),xx2,yy2,kp,ip,jp,icomp,cwgt,cobs,weight,error
        elseif(iOutCobs.gt.0 .and. iErrMsg.ne.0) then
          write(inConcObs,32)
     &     cobsnam(n),xx2,yy2,kp,ip,jp,icomp,cwgt,ErrMsg
        elseif(iOutCobs.eq.0 .and. iErrMsg.eq.0) then
          write(inConcObs,34)
     &     cobsnam(n),xx2,yy2,kp,ip,jp,icomp,cwgt
        elseif(iOutCobs.eq.0 .and. iErrMsg.gt.0) then
          write(inConcObs,36)
     &     cobsnam(n),xx2,yy2,kp,ip,jp,icomp,cwgt,ErrMsg     
        endif
        if(inSaveObs.gt.0) then
          write(inSaveObs) cobsnam(n),TimeObs,cwgt
        endif  

      enddo !end of obs wel loop
            
   30 format(1x,a12,1p,2g12.4,3i5,3x,i4,2x,4g12.4)
   32 format(1x,a12,1p,2g12.4,3i5,3x,i4,2x,1g12.4,3x,a) 
   34 format(1x,a12,1p,2g12.4,3i5,3x,i4,2x,1g12.4)   
   36 format(1x,a12,1p,2g12.4,3i5,3x,i4,2x,1g12.4,3x,a)       

c--calculate statistics
      if(iOutCobs.gt.0.and.nobs_active.gt.1) then
        CALL MOMENT(temp(1,3),nobs_active,AVE,ADEV,SDEV,VAR,SKEW,CURT)
        CALL PEARSN(temp(1,1),temp(1,2),nobs_active,R,PROB,Z)
      else
        goto 1000
      endif

c--print statastics
      write(inConcObs,50) nobs_active
      write(inConcObs,60) AVE
      write(inConcObs,62) SDEV
      write(inConcObs,64) abserrsum/nobs_active
      write(inConcObs,66) SQRT(ERRSUM/nobs_active)     
      if(nobs_active.gt.2) then
        write(inConcObs,70) R
        write(inConcObs,72) PROB       
      endif

   50 FORMAT(/1x,'   NUMBER OF ACTIVE OBSERVATION POINTS = ',I5)
   60 FORMAT( 1x,'                 MEAN OF RESIDUALS (M) = ',G15.7)
   62 FORMAT( 1x,'STANDARD DEVIATION OF RESIDUALS (SDEV) = ',G15.7)
   64 FORMAT( 1x,'       MEAN OF ABSOLUTE RESIDUALS (MA) = ',G15.7)
   66 FORMAT( 1x,'     ROOT MEAN SQUARED RESIDUALS (RMS) = ',G15.7)
   70 FORMAT( 1x,'               CORRELATION COEFFICIENT = ',G15.7)
   72 FORMAT( 1x,'         PROBABILITY OF UN-CORRELATION = ',G15.7)
c
 1000 IF(nobs_current.le.0) write(inConcObs,1080)
 1080 FORMAT(1x,'[No obs wells active at current transport step]')
C
C--normal return
      RETURN
      END
C
C
      Subroutine MassFluxObs(inFluxObs,iout,ncol,nrow,nlay,ncomp,
     & MaxFluxCells,nFLuxGroup,nFLuxObs,kper,kstp,ntrans,time1,time2,
     & cnew,icbund,mxss,ntss,ss,ssmc,delr,delc,dh,
     & irch,rech,crch,ievt,evtr,cevt,
     & FluxGroup,GroupData,fobsnam,grouptmp,inSaveObs,iOutFlux)
C **********************************************************************
C This subroutine gets calculated mass fluxes at user specified
C locations and computes residual errors between calculated and
C observed if necessary
C **********************************************************************
C last modified: 02-20-2010
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,ntrans,inFluxObs,iout,n,iGroup,
     &          nFluxGroup,nFluxTimeObs,MaxFluxCells,nFluxObs,
     &          nobs_active,icell,nCells,iSSType,it,inode,k,i,j,
     &          num,mxss,ntss,IQ,kp,ip,jp,icbund,icomp,ncomp,
     &          irch,ievt,kstp,kper,iOutFlux,
     &          inSaveObs,issLink,iErrMsg,itmp,nobs_current
      REAL      time1,time2,FluxGroup,GroupData,GroupTmp,TimeObs,
     &          FluxObs,FluxCal,ss,ssmc,qss,cnew,csink,ctmp,error,
     &          delr,delc,dh,QC,Q,weight,errsum,abserrsum,
     &          rech,crch,evtr,cevt,
     &          SKEW,AVE,ADEV,SDEV,VAR,CURT,R,PROB,Z,fraction
      CHARACTER fobsnam*12,ErrMsg*24
      LOGICAL   FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,FIBS,
     &          FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
      DIMENSION FluxGroup(3+2*MaxFluxCells,nFluxGroup),
     &          fobsnam(nFluxObs),GroupData(7,nFluxObs),
     &          GroupTmp(nFluxObs,3),cnew(ncol,nrow,nlay,ncomp),
     &          icbund(ncol,nrow,nlay,ncomp),ss(7,mxss),
     &          ssmc(ncomp,mxss),rech(ncol,nrow),evtr(ncol,nrow),
     &          crch(ncol,nrow,ncomp),cevt(ncol,nrow,ncomp),
     &          irch(ncol,nrow),ievt(ncol,nrow),
     &          delr(ncol),delc(nrow),dh(ncol,nrow,nlay)
      COMMON   /FC/FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,
     &             FIBS,FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF
c
c--print headers to output file
c    
      WRITE(inFluxObs,2) kper,kstp,ntrans,time2
      IF(iOutFlux.gt.0) THEN
        WRITE(inFluxObs,4) 
      ELSEIF(iOutFlux.eq.0) THEN
        WRITE(inFluxObs,6)
      ENDIF
    2 format(//1x,30('*'),
     &        /1x,'     STRESS PERIOD:    ',i5,
     &        /1x,'         TIME STEP:    ',i5,
     &        /1x,'    TRANSPORT STEP:    ',i5,
     &        /1x,'TOTAL ELAPSED TIME: ',g12.4,     
     &        /1x,30('*')/)                
    4 format(1x,'  NO. NAME            TIME   SPECIES      ',
     & 'CALCULATED   OBSERVED   WEIGHT     (CAL.-OBS.)'/1x,88('.'))  
    6 format(1x,'  NO. NAME            TIME   SPECIES      ',
     & 'CALCULATED  '/1x,54('.'))   
c
c--reset accumulators
c
      nobs_active=0
      nobs_current=0
      errsum=0
      abserrsum=0            
c
c--clear temporary storage arrays
c
      do n=1,nfluxobs       
        GroupTmp(n,1)=0.       
        GroupTmp(n,2)=0.       
        GroupTmp(n,3)=0.
      enddo
c
c--loop through all mass flux observations
c
      do n=1,nfluxobs

        iGroup= GroupData(1,n)
        iComp=  GroupData(2,n)
        TimeObs=GroupData(3,n)         
        weight =GroupData(4,n)
        FluxObs=GroupData(5,n)
        GroupData(6,n)=0.
        GroupData(7,n)=0.
               
        if(TimeObs.lt.0) then
          itmp=-int(TimeObs)
          if(mod(ntrans,itmp).ne.0) then
            cycle   !skip if not even multiple
          else
            TimeObs=time2
          endif
        elseif(TimeObs.le.time1 .or. TimeObs.gt.time2) then
          cycle                     !skip if not at current time step
        endif        

  100   nFluxTimeObs=FluxGroup(1,iGroup)
        nCells=FluxGroup(2,iGroup)
        iSSType=FluxGroup(3,iGroup)

c--loop through all cells in the current flux object
        
        do icell=1,nCells

          inode=int( FluxGroup(3+icell,iGroup) )
          kp = (inode-1) / (ncol*nrow) + 1
          ip = mod((inode-1),ncol*nrow)/ncol + 1
          jp = mod((inode-1),ncol) + 1
          fraction=FluxGroup(3+MaxFluxCells+icell,iGroup)
          
c--if recharge flux

          if(iSSType.eq.7 .and. FRCH) then
            if(kp.ne.irch(jp,ip)) cycle
            if(icbund(jp,ip,kp,icomp).le.0) cycle
            ctmp=crch(jp,ip,icomp)
            qss=rech(jp,ip)
            if(qss.lt.0) ctmp=cnew(jp,ip,kp,icomp)
c--get volumetric Q*C and Q                      
            QC=qss*delr(jp)*delc(ip)*dh(jp,ip,kp)*ctmp
            Q =qss*delr(jp)*delc(ip)*dh(jp,ip,kp)
c--cumulate in GroupData
            GroupData(6,n) = GroupData(6,n) + QC * fraction
            GroupData(7,n) = GroupData(7,n) + Q  * fraction                   
            cycle
          endif              
          
c--if evapotranspiration flux

          if(iSSType.eq.8 .and. (FEVT.or.FETS) ) then                 
            if(kp.ne.ievt(jp,ip)) cycle
            if(icbund(jp,ip,kp,icomp).le.0) cycle
            ctmp=cevt(jp,ip,icomp)
            qss=evtr(jp,ip)
            if(qss.lt.0 .and. (ctmp.lt.0. or. 
     &                 ctmp.ge.cnew(jp,ip,kp,icomp))) then
              ctmp=cnew(jp,ip,kp,icomp)
            elseif(ctmp.lt.0) then
              ctmp=0.
            endif  
c--get volumetric Q*C and Q            
            QC=qss*delr(jp)*delc(ip)*dh(jp,ip,kp)*ctmp
            Q =qss*delr(jp)*delc(ip)*dh(jp,ip,kp)            
c--cumulate in GroupData
            GroupData(6,n) = GroupData(6,n) + QC * fraction
            GroupData(7,n) = GroupData(7,n) + Q  * fraction                   
            cycle
          endif              

c--if point sinks/sources

          do num=1,ntss
            k=ss(1,num)
            i=ss(2,num)
            j=ss(3,num)
            ctmp=ss(4,num)
            if(icomp.gt.1) ctmp=ssmc(icomp,num)
            qss=ss(5,num)
            if(qss.lt.0) ctmp=cnew(j,i,k,icomp)
            IQ=ss(6,num)
            issLink=ss(7,num)            
c--skip if not same ss type
            if(iSSType.ne.IQ) cycle            
c--skip if not same cell
            if(kp.ne.k .or. ip.ne.i .or. jp.ne.j) cycle    
c--skip if not an active cell
            if(icbund(j,i,k,icomp).le.0) cycle         
c--skip if at a linked group sink/source   
            if(issLink.gt.0) cycle                
c--get volumetric Q*C and Q            
            QC=qss*delr(j)*delc(i)*dh(j,i,k)*ctmp
            Q =qss*delr(j)*delc(i)*dh(j,i,k)            
c--cumulate in GroupData
            GroupData(6,n) = GroupData(6,n) + QC * fraction
            GroupData(7,n) = GroupData(7,n) + Q  * fraction            
          enddo      !end of point sink/source loop
          
        enddo   !end of the cell loop in the current flux object
        
        nobs_current=nobs_current+1
        iErrMsg=0
        ErrMsg=' '
        fluxcal=GroupData(6,n)
        if(weight.lt.0) then
          iErrMsg=-1
          ErrMsg='no observed flux given'          
        elseif(iOutFlux.gt.0) then
          nobs_active=nobs_active+1
          grouptmp(nobs_active,1)=fluxobs
          grouptmp(nobs_active,2)=fluxcal
          error=(fluxcal-fluxobs)*weight
          errsum=errsum+error*error
          abserrsum=abserrsum+abs(error)
          grouptmp(nobs_active,3)=error                    
        endif  
        
        if(iOutFlux.gt.0 .and. iErrMsg.eq.0) then          
          write(inFluxObs,30) iGroup,fobsnam(n),timeobs,
     &     icomp,fluxcal,fluxobs,weight,error
        elseif(iOutFlux.gt.0 .and. iErrMsg.ne.0) then           
          write(inFluxObs,32) iGroup,fobsnam(n),timeobs,
     &     icomp,fluxcal,ErrMsg
        elseif(iOutFlux.eq.0) then
          write(inFluxObs,34) iGroup,fobsnam(n),timeobs,
     &     icomp,fluxcal
        endif       
        if(inSaveObs.gt.0) then
          write(inSaveObs) fobsnam(n),TimeObs,fluxcal
        endif
               
      enddo  !end of mass flux object loop
              
   30 format(1x,i4,2x,a12,1p,g12.4,i4,6x,4g12.4)
   32 format(1x,i4,2x,a12,1p,g12.4,i4,6x,1g12.4,3x,a)
   34 format(1x,i4,2x,a12,1p,g12.4,i4,6x,1g12.4)
c
c--calculate statistics
c
      if(iOutFlux.gt.0.and.nobs_active.gt.1) then
        CALL MOMENT(grouptmp(1,3),nobs_active,AVE,ADEV,
     &              SDEV,VAR,SKEW,CURT)
        CALL PEARSN(grouptmp(1,1),grouptmp(1,2),
     &              nobs_active,R,PROB,Z)
      else
        goto 1000
      endif
c
c--print statastics
c
      write(inFluxObs,50) nobs_active
      write(inFluxObs,60) AVE
      write(inFluxObs,62) SDEV
      write(inFluxObs,64) abserrsum/nobs_active
      write(inFluxObs,66) SQRT(ERRSUM/nobs_active)      
      if(nobs_active.gt.2) then
        write(inFluxObs,70) R
        write(inFluxObs,72) PROB     
      endif
   50 FORMAT(/1x,'   NUMBER OF ACTIVE OBSERVATION POINTS = ',I5)
   60 FORMAT( 1x,'                 MEAN OF RESIDUALS (M) = ',G15.7)
   62 FORMAT( 1x,'STANDARD DEVIATION OF RESIDUALS (SDEV) = ',G15.7)
   64 FORMAT( 1x,'       MEAN OF ABSOLUTE RESIDUALS (MA) = ',G15.7)
   66 FORMAT( 1x,'     ROOT MEAN SQUARED RESIDUALS (RMS) = ',G15.7)
   70 FORMAT( 1x,'               CORRELATION COEFFICIENT = ',G15.7)
   72 FORMAT( 1x,'         PROBABILITY OF UN-CORRELATION = ',G15.7)
c
 1000 IF(nobs_current.le.0) write(inFluxObs,1080)
 1080 FORMAT(1x,'[No flux object active at current transport step]')
c
c--normal return
      RETURN
      END
C
C
      SUBROUTINE MOMENT(DATA,N,AVE,ADEV,SDEV,VAR,SKEW,CURT)
C *****************************************************************
C This subroutine computes mean, variance, skewness, and kurtosis 
C for an array of data points data(n).
C *****************************************************************
C modified from Press et al. (1992)
C last modified: 02-15-2005
C      
      IMPLICIT NONE
      INTEGER n,j
      REAL adev,ave,curt,sdev,skew,var,data,p,s,ep
      DIMENSION data(n)
C      
      if(n.le.1) then
        call ustop ('N must be at least 2 in subroutine MOMENT',
     &   ' used by TOB Package')
      endif        
      s=0.
      do j=1,n
        s=s+data(j)
      enddo
      ave=s/n
      adev=0.
      var=0.
      skew=0.
      curt=0.
      ep=0.      
      do j=1,n
        s=data(j)-ave
        ep=ep+s
        adev=adev+abs(s)
        p=s*s
        var=var+p
        p=p*s
        skew=skew+p
        p=p*s
        curt=curt+p
      enddo      
      adev=adev/n
      var=(var-ep**2/n)/(n-1)
      sdev=sqrt(var)
      if(var.ne.0.)then
        skew=skew/(n*sdev**3)
        curt=curt/(n*var**2)-3.
      else
CZ      pause 'no skew or kurtosis when zero variance in moment'
      endif      
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
      if(n.le.2) goto 1   !added by CZ
C      
      ax=0.
      ay=0.
      do j=1,n
        ax=ax+x(j)
        ay=ay+y(j)
      enddo
      ax=ax/n
      ay=ay/n
      sxx=0.
      syy=0.
      sxy=0.      
      do j=1,n
        xt=x(j)-ax
        yt=y(j)-ay
        sxx=sxx+xt**2
        syy=syy+yt**2
        sxy=sxy+xt*yt
      enddo
      r=sxy/(sqrt(sxx*syy) + TINY)   !TINY added by CZ
      z=0.5*log(((1.+r)+TINY)/((1.-r)+TINY))
      df=n-2     
      t=r*sqrt(df/(((1.-r)+TINY)*((1.+r)+TINY)))
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
      if(x.lt.0. .or. x.gt.1.) then
        call ustop('Bad argument x in subroutine BETAI',
     &   ' used by TOB Package')
      endif  
      if(x.eq.0. .or. x.eq.1.) then
        bt=0.
      else
        bt=exp(gammln(a+b)-gammln(a)-gammln(b)
     &   +a*log(x)+b*log(1.-x))
      endif
      if(x.lt.(a+1.)/(a+b+2.)) then
        betai=bt*betacf(a,b,x)/a
        return
      else
        betai=1.-bt*betacf(b,a,1.-x)/b
        return
      endif
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
      if(abs(d).lt.FPMIN) d=FPMIN
      d=1./d
      h=d
      do m=1,MAXIT
        m2=2*m
        aa=m*(b-m)*x/((qam+m2)*(a+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN) d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN) c=FPMIN
        d=1./d
        h=h*d*c
        aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN) d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN) c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS) goto 1
      enddo      
      call ustop('a or b too big, or MAXIT too small',
     & ' in subroutine BETACF used by TOB Package')
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
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
      enddo
      gammln=tmp+log(stp*ser/x)
C      
      RETURN
      END