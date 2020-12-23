C
      SUBROUTINE DSP1AR(IN)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED IN THE DISPERSION
C (DSP) PACKAGE.
C **********************************************************************
C
      USE MT3DMS_MODULE, ONLY: INDSP,IOUT,NCOL,NROW,NLAY,MCOMP,BUFF,
     &                         ALPHAL,DMCOEF,TRPT,TRPV,DXX,DYY,
     &                         DZZ,DXY,DXZ,DYX,DYZ,DZX,DZY,INOCROSS,
     &                         UNSATHEAT,K_T_SAT,K_T_RESID,
     &                         RHO_FLUID,C_P_FLUID,THETA_R
C
      IMPLICIT  NONE
      INTEGER   IN,ICOMP,IMSD,J,I,K,
     &          IFLEN,LLOC,INAM1,INAM2,N
      REAL      R
      REAL, ALLOCATABLE :: BUFF2(:)
      CHARACTER ANAME*24,LINE*200,KEYWORD*200
C
      INDSP=IN
C
C--ALLOCATE DSP ARRAYS AND INITIALIZE TO ZERO
      ALLOCATE(ALPHAL(NCOL,NROW,NLAY))
      ALLOCATE(TRPT(NLAY))
      ALLOCATE(TRPV(NLAY))
      ALLOCATE(DMCOEF(NCOL,NROW,NLAY,MCOMP))
      ALLOCATE(DXX(NCOL,NROW,NLAY,MCOMP))
      ALLOCATE(DXY(NCOL,NROW,NLAY))
      ALLOCATE(DXZ(NCOL,NROW,NLAY))
      ALLOCATE(DYX(NCOL,NROW,NLAY))
      ALLOCATE(DYY(NCOL,NROW,NLAY,MCOMP))
      ALLOCATE(DYZ(NCOL,NROW,NLAY))
      ALLOCATE(DZX(NCOL,NROW,NLAY))
      ALLOCATE(DZY(NCOL,NROW,NLAY))
      ALLOCATE(DZZ(NCOL,NROW,NLAY,MCOMP))
      ALLOCATE(BUFF2(NLAY))
      ALLOCATE(K_T_SAT,K_T_RESID,RHO_FLUID,C_P_FLUID,THETA_R)
      ALPHAL=0.
      TRPT=0.
      TRPV=0.
      DMCOEF=0.
      DXX=0.
      DXY=0.
      DXZ=0.
      DYX=0.
      DYY=0.
      DYZ=0.
      DZX=0.
      DZY=0.
      DZZ=0.
      BUFF2=0.
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INDSP
 1030 FORMAT(/1X,'DSP1 -- DISPERSION PACKAGE,',
     &           ' VERSION 1, MAY 2016, INPUT READ FROM UNIT',I3)
C
C--PRINT A HEADER
      WRITE(IOUT,1000)
 1000 FORMAT(//1X,'DISPERSION INPUT PARAMETERS'/1X,27('-')/)
C
C--READ INPUT KEYWORDS AS A TEXT STRING
   10 READ(IN,'(A)',END=20) LINE  
      IF(LINE.EQ.' ') GOTO 10   
      IF(LINE(1:1).EQ.'#') THEN
        WRITE(IOUT,'(A)') LINE
        GOTO 10
      ELSEIF(LINE(1:1).EQ.'$') THEN
        IMSD=0
        UNSATHEAT=0
        GOTO 30
      ENDIF
C
C--NO KEYWORD LINE, REWIND INPUT FILE      
   20 BACKSPACE(IN) 
      IMSD=0
      GOTO 50     
C       
C--DECODE KEYWORD LINE
   30 CONTINUE 
      BACKSPACE(IN)
      CALL DSPKEYWORDS(IN,IMSD)
C
C--WRITE STATUS OF NOCROSS FLAG
      IF(INOCROSS.EQ.1) WRITE(IOUT,1010)
 1010 FORMAT(1X,'CROSS DISPERSION DEACTIVATED (NOCROSS)')
C
C--WRITE STATUS OF UNSATHEAT FLAG
      IF(UNSATHEAT.EQ.1) THEN
        ALLOCATE(K_T_SAT,K_T_RESID,RHO_FLUID,C_P_FLUID)
        WRITE(IOUT,1012)
 1012   FORMAT(1X,'SIMULATION OF UNSATURATED HEAT FLOW ACTIVE'
     &          /,'READING HEAT-RELATED TERMS')
        READ(IN,'(4F10.2,F10.3)') K_T_SAT,K_T_RESID,RHO_FLUID,C_P_FLUID,
     &                            THETA_R
        WRITE(IOUT,1014) K_T_SAT,K_T_RESID,RHO_FLUID,C_P_FLUID,THETA_R
 1014   FORMAT(1X,'K_T_SAT = ',F10.4,
     &         1/,'K_T_RESID = ',F10.4,
     &         1/,'RHO_FLUID = ',F10.4,
     &         1/,'C_P_FLUID = ',F10.4,
     &         1/,'THETA_R   = ',F10.4)
      ENDIF
C
C--CALL RARRAY TO READ LONGITUDINAL DISPERSIVITY ONE LAYER A TIME
   50 DO K=1,NLAY
        ANAME='LONG. DISPERSIVITY (AL)'
        CALL RARRAY(ALPHAL(:,:,K),ANAME,NROW,NCOL,K,IN,IOUT)
      ENDDO
C
C--CALL RARRAY TO READ RATIO OF HORIZONTAL TRANSVERSE
C--TO LONGITUDINAL DISPERSIVITY ONE VALUE PER LAYER
      ANAME='H. TRANS./LONG. DISP.'
      CALL RARRAY(TRPT(1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
C
C--CALL RARRAY TO READ RATIO OF VERTICAL TRANSVERSE TO
C--LONGITUDINAL DISPERSIVITY ONE VALUE PER LAYER
      ANAME='V. TRANS./LONG. DISP.'
      CALL RARRAY(TRPV(1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
C
C--CALL RARRAY TO READ EFFECTIVE MOLECULAR DIFFUSION COEFFICIENT
C--OPTION 1: ONE VALUE PER LAYER FOR ALL CHEMICAL COMPONENTS
      IF(IMSD.EQ.0) THEN  
        ANAME='DIFFUSION COEFFICIENT'   
        CALL RARRAY(BUFF2(1:NLAY),ANAME,1,NLAY,0,IN,IOUT)
        DO ICOMP=1,MCOMP
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                DMCOEF(J,I,K,ICOMP)=BUFF2(K)
              ENDDO
            ENDDO
          ENDDO  
        ENDDO                    
C--OPTION 2: ONE VALUE PER MODEL CELL PER CHEMICAL COMPONENT  
      ELSEIF(IMSD.EQ.1) THEN     
        ANAME='DIFFUSION COEFF. COMP ##'
        DO ICOMP=1,MCOMP
          WRITE(ANAME(23:24),'(I2.2)') ICOMP
          DO K=1,NLAY
            CALL RARRAY(DMCOEF(1:NCOL,1:NROW,K,ICOMP),ANAME,NROW,NCOL,K,
     &                  IN,IOUT)
          ENDDO
        ENDDO
      ENDIF
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE DSP1CF(KSTP,KPER)
C***********************************************************************
C THIS SUBROUTINE CALCULATES COMPONENTS OF THE HYDRODYNAMIC DISPERSION
C COEFFICIENT (Dij) AT CELL INTERFACES.
C NOTE: Dij IS CALCULATED USING DARCY FLUX COMPONENTS INSTEAD OF
C ====  LINEAR VELOCITY COMPONENTS.  TO CONVERT THIS APPARENT Dij TO
C       ACTUAL DISPERSION COEFFICIENT, IT IS DIVIDED BY POROSITY.
C***********************************************************************
C
      USE MT3DMS_MODULE, ONLY: IOUT,NCOL,NROW,NLAY,MCOMP,ICBUND,PRSITY,
     &                         DELR,DELC,DH,QX,QY,QZ,IFMTDP,DTDISP,ISS,
     &                         NPERFL,
     &                         ALPHAL,TRPT,TRPV,DMCOEF,DXX,DXY,DXZ,DYX,
     &                         DYY,DYZ,DZX,DZY,DZZ,
     &                         INOCROSS,UNSATHEAT,K_T_SAT,K_T_RESID,
     &                         RHO_FLUID,C_P_FLUID,THETA_R
      USE UZTVARS,       ONLY: PRSITYSAV
C
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,K,I,J,KM1,IM1,JM1,
     &          KP1,IP1,JP1,JD,ID,KD,ICOMP
      REAL      V,WW,PF,AL,AT,AV,DM,VX,VY,VZ,TD,AREA,THETA,THETA_S,W1,W2
      CHARACTER TEXT*16
C
C--INITIALIZE
      DTDISP=1.E30
      KD=0
      ID=0
      JD=0
C
C--IF SIMULATING UNSATURATED ZONE HEAT, UPDATE DMCOEF AS THIS IS A
C  SURROGATE FOR HEAT CONDUCTION AND IS DEPENDENT UPON MOISTURE CONTENT
      IF(UNSATHEAT) THEN
        DO ICOMP=1, MCOMP
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                IF(ICBUND(J,I,K,1).NE.0) THEN
                  THETA=PRSITY(J,I,K)
                  THETA_S=PRSITYSAV(J,I,K)
                  W1=((THETA-THETA_R)/(THETA_S-THETA_R))
                  W2=1-W1
                  DMCOEF(J,I,K,ICOMP)=(W1*K_T_SAT + W2*K_T_RESID)/
     &                                 (THETA*RHO_FLUID*C_P_FLUID)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--FOR EACH CHEMICAL COMPONENT WITH A DIFFERENT DIFFUSION COEFFICIENT     
      DO ICOMP=1,MCOMP           
C
C--FOR COEFFICIENTS ALONG THE X DIRECTION
C  ======================================
      IF(NCOL.LT.2) GOTO 100
C
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
            IF(ICBUND(J,I,K,1).EQ.0.OR.ICBUND(JP1,I,K,1).EQ.0) CYCLE
C
C--CALCULATE VALUES AT INTERFACES
            WW=DELR(JP1)/(DELR(J)+DELR(JP1))
            PF=1.
            AL=ALPHAL(J,I,K)*WW+ALPHAL(JP1,I,K)*(1.-WW)
            AT=AL*TRPT(K)
            AV=AL*TRPV(K)
            DM=(DMCOEF(J,I,K,ICOMP)*WW+DMCOEF(JP1,I,K,ICOMP)*(1.-WW))*
     &         (PRSITY(J,I,K)*WW+PRSITY(JP1,I,K)*(1.-WW))
            VX=QX(J,I,K)
            IF(NROW.GT.1) THEN
              VY=0.5*(QY(J,IM1,K)+QY(J,I,K))*WW+
     &           0.5*(QY(JP1,IM1,K)+QY(JP1,I,K))*(1.-WW)
            ELSE
              VY=0
            ENDIF
            IF(NLAY.GT.1) THEN
              VZ=0.5*(QZ(J,I,KM1)+QZ(J,I,K))*WW+
     &           0.5*(QZ(JP1,I,KM1)+QZ(JP1,I,K))*(1.-WW)
            ELSE
              VZ=0
            ENDIF
            V=SQRT(VX*VX+VY*VY+VZ*VZ)
C
C--CALCULATE DISPERSION COEFFICIENTS
            IF(V.EQ.0) THEN
              DXX(J,I,K,ICOMP)=DM
              IF(NROW.GT.1) DXY(J,I,K)=0
              IF(NLAY.GT.1) DXZ(J,I,K)=0
            ELSE
              DXX(J,I,K,ICOMP)=AL*VX*VX/V/
     &                         PF+AT*VY*VY/V/PF+AV*VZ*VZ/V/PF+DM
              IF(INOCROSS.EQ.1) THEN       
                IF(NROW.GT.1) DXY(J,I,K)=0.
                IF(NLAY.GT.1) DXZ(J,I,K)=0.
              ELSE                         
                IF(NROW.GT.1) DXY(J,I,K)=(AL-AT)*VX*VY/V/PF
                IF(NLAY.GT.1) DXZ(J,I,K)=(AL-AV)*VX*VZ/V/PF
              ENDIF 
            ENDIF            
          ENDDO
        ENDDO
      ENDDO
C
C--FOR COEFFICIENTS ALONG THE Y DIRECTION
C  ======================================
  100 IF(NROW.LT.2) GOTO 200
C
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO J=1,NCOL
          JP1=MIN(J+1,NCOL)
          JM1=MAX(1,J-1)
          DO I=1,NROW
            IP1=MIN(I+1,NROW)
            IM1=MAX(1,I-1)
            IF(ICBUND(J,I,K,1).EQ.0.OR.ICBUND(J,IP1,K,1).EQ.0) CYCLE
C
C--CALCULATE VALUES AT INTERFACES
            WW=DELC(IP1)/(DELC(I)+DELC(IP1))
            PF=1.
            AL=ALPHAL(J,I,K)*WW+ALPHAL(J,IP1,K)*(1.-WW)
            AT=AL*TRPT(K)
            AV=AL*TRPV(K)
            DM=(DMCOEF(J,I,K,ICOMP)*WW+DMCOEF(J,IP1,K,ICOMP)*(1.-WW))*
     &         (PRSITY(J,I,K)*WW+PRSITY(J,IP1,K)*(1.-WW))
            VY=QY(J,I,K)
            IF(NCOL.GT.1) THEN
              VX=0.5*(QX(J,I,K)+QX(JM1,I,K))*WW+
     &           0.5*(QX(J,IP1,K)+QX(JM1,IP1,K))*(1.-WW)
            ELSE
              VX=0
            ENDIF
            IF(NLAY.GT.1) THEN
              VZ=0.5*(QZ(J,I,K)+QZ(J,I,KM1))*WW+
     &           0.5*(QZ(J,IP1,K)+QZ(J,IP1,KM1))*(1.-WW)
            ELSE
              VZ=0
            ENDIF
            V=SQRT(VX*VX+VY*VY+VZ*VZ)
C
C--CALCULATE DISPERSION COEFFICIENTS
            IF(V.EQ.0) THEN
              DYY(J,I,K,ICOMP)=DM
              IF(NCOL.GT.1) DYX(J,I,K)=0
              IF(NLAY.GT.1) DYZ(J,I,K)=0
            ELSE
              DYY(J,I,K,ICOMP)=AL*VY*VY/V/PF+
     &                         AT*VX*VX/V/PF+AV*VZ*VZ/V/PF+DM
              IF(INOCROSS.EQ.1) THEN       
                IF(NCOL.GT.1) DYX(J,I,K)=0.
                IF(NLAY.GT.1) DYZ(J,I,K)=0.
              ELSE                         
                IF(NCOL.GT.1) DYX(J,I,K)=(AL-AT)*VY*VX/V/PF
                IF(NLAY.GT.1) DYZ(J,I,K)=(AL-AV)*VY*VZ/V/PF
              ENDIF 
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--FOR COEFFICIENTS ALONG THE Z DIRECTION
C  ======================================
  200 IF(NLAY.LT.2) GOTO 300
C
      DO I=1,NROW
        IP1=MIN(I+1,NROW)
        IM1=MAX(1,I-1)
        DO J=1,NCOL
          JP1=MIN(J+1,NCOL)
          JM1=MAX(1,J-1)
          DO K=1,NLAY
            KP1=MIN(K+1,NLAY)
            KM1=MAX(1,K-1)
            IF(ICBUND(J,I,K,1).EQ.0.OR.ICBUND(J,I,KP1,1).EQ.0) CYCLE
C
C--CALCULATE VALUES AT INTERFACES
            WW=DH(J,I,KP1)/(DH(J,I,K)+DH(J,I,KP1))
            PF=1.
            AL=ALPHAL(J,I,K)*WW+ALPHAL(J,I,KP1)*(1.-WW)
            AT=ALPHAL(J,I,K)*TRPT(K)*WW+
     &         ALPHAL(J,I,KP1)*TRPT(KP1)*(1.-WW)
            AV=ALPHAL(J,I,K)*TRPV(K)*WW+
     &         ALPHAL(J,I,KP1)*TRPV(KP1)*(1.-WW)
            DM=(DMCOEF(J,I,K,ICOMP)*WW+DMCOEF(J,I,KP1,ICOMP)*(1.-WW))*
     &         (PRSITY(J,I,K)*WW+PRSITY(J,I,KP1)*(1.-WW))
            VZ=QZ(J,I,K)
            IF(NCOL.GT.1) THEN
              VX=0.5*(QX(JM1,I,K)+QX(J,I,K))*WW+
     &           0.5*(QX(JM1,I,KP1)+QX(J,I,KP1))*(1.-WW)
            ELSE
              VX=0
            ENDIF
            IF(NROW.GT.1) THEN
              VY=0.5*(QY(J,IM1,K)+QY(J,I,K))*WW+
     &           0.5*(QY(J,IM1,KP1)+QY(J,I,KP1))*(1.-WW)
            ELSE
              VY=0
            ENDIF
            V=SQRT(VX*VX+VY*VY+VZ*VZ)
C
C--CALCULATE DISPERSION COEFFICIENTS
            IF(V.EQ.0) THEN
              DZZ(J,I,K,ICOMP)=DM
              IF(NCOL.GT.1) DZX(J,I,K)=0
              IF(NROW.GT.1) DZY(J,I,K)=0
            ELSE
              DZZ(J,I,K,ICOMP)=AL*VZ*VZ/V/PF+
     &                         AV*VX*VX/V/PF+AV*VY*VY/V/PF+DM
              IF(INOCROSS.EQ.1) THEN       
                IF(NCOL.GT.1) DZX(J,I,K)=0.
                IF(NROW.GT.1) DZY(J,I,K)=0.
              ELSE                         
                IF(NCOL.GT.1) DZX(J,I,K)=(AL-AV)*VZ*VX/V/PF
                IF(NROW.GT.1) DZY(J,I,K)=(AL-AV)*VZ*VY/V/PF
              ENDIF  
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
  300 CONTINUE
C
C--SET DISPERSION COEFFICIENTS TO ZERO IN INACTIVE CELLS
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
            IF(ICBUND(J,I,K,1).NE.0) CYCLE
C            
            IF(NCOL.GT.1) THEN
              DXX(J  ,I,K,ICOMP)=0.
              DXX(JM1,I,K,ICOMP)=0.
              IF(NROW.GT.1) THEN
                DXY(J,  IM1,K)=0.
                DXY(J,  I,  K)=0.
                DXY(J,  IP1,K)=0.
                DXY(JM1,IM1,K)=0.
                DXY(JM1,I,  K)=0.
                DXY(JM1,IP1,K)=0.
              ENDIF
              IF(NLAY.GT.1) THEN
                DXZ(J,  I,KM1)=0.
                DXZ(J,  I,K  )=0.
                DXZ(J,  I,KP1)=0.
                DXZ(JM1,I,KM1)=0.
                DXZ(JM1,I,K  )=0.
                DXZ(JM1,I,KP1)=0.
              ENDIF
            ENDIF
C
            IF(NROW.GT.1) THEN
              DYY(J,IM1,K,ICOMP)=0.
              DYY(J,  I,K,ICOMP)=0.
              IF(NCOL.GT.1) THEN
                DYX(JM1,I  ,K)=0.
                DYX(J  ,I  ,K)=0.
                DYX(JP1,I  ,K)=0.
                DYX(JM1,IM1,K)=0.
                DYX(J  ,IM1,K)=0.
                DYX(JP1,IM1,K)=0.
              ENDIF
              IF(NLAY.GT.1) THEN
                DYZ(J,I  ,KM1)=0.
                DYZ(J,I  ,K  )=0.
                DYZ(J,I  ,KP1)=0.
                DYZ(J,IM1,KM1)=0.
                DYZ(J,IM1,K  )=0.
                DYZ(J,IM1,KP1)=0.
              ENDIF
            ENDIF
C
            IF(NLAY.GT.1) THEN
              DZZ(J,I,K  ,ICOMP)=0.
              DZZ(J,I,KM1,ICOMP)=0.
              IF(NCOL.GT.1) THEN
                DZX(JM1,I,K)=0.
                DZX(J  ,I,K)=0.
                DZX(JP1,I,K)=0.
                DZX(JM1,I,KM1)=0.
                DZX(J  ,I,KM1)=0.
                DZX(JP1,I,KM1)=0.
              ENDIF
              IF(NROW.GT.1) THEN
                DZY(J,IM1,K  )=0.
                DZY(J,I  ,K  )=0.
                DZY(J,IP1,K  )=0.
                DZY(J,IM1,KM1)=0.
                DZY(J,I  ,KM1)=0.
                DZY(J,IP1,KM1)=0.
              ENDIF
            ENDIF
C            
          ENDDO
        ENDDO
      ENDDO
C
C--CALCULATE MAXIMUM TIME INCREMENT WHICH MEETS STABILITY CRITERION
C--FOR SOLVING THE EXPLICIT FINITE-DIFFERENCE DISPERSION EQUATIONS
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
C         
            IF(ICBUND(J,I,K,1).NE.0) THEN
              TD=0.
              IF(NCOL.GT.1.AND.J.LT.NCOL) THEN
                IF(ICBUND(J+1,I,K,1).NE.0)
     &            TD=TD+DXX(J,I,K,ICOMP)/(0.5*DELR(J)+0.5*DELR(J+1))**2
              ENDIF
              IF(NROW.GT.1.AND.I.LT.NROW) THEN
                IF(ICBUND(J,I+1,K,1).NE.0)
     &            TD=TD+DYY(J,I,K,ICOMP)/(0.5*DELC(I)+0.5*DELC(I+1))**2
              ENDIF
              IF(NLAY.GT.1.AND.K.LT.NLAY) THEN
                IF(ICBUND(J,I,K+1,1).NE.0) TD=TD+DZZ(J,I,K,ICOMP)/
     &                                (0.5*DH(J,I,K)+0.5*DH(J,I,K+1))**2
              ENDIF
              IF(TD.GT.0) THEN
                TD=0.5/TD*PRSITY(J,I,K)
                IF(TD.LT.DTDISP) THEN
                  DTDISP=TD
                  JD=J
                  ID=I
                  KD=K
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--PRINT OUT DISPERSION COEFFICIENT IF REQUESTED
      IF(IFMTDP.EQ.0) GOTO 980
C
      WRITE(IOUT,510) 
  510 FORMAT(/1X,'PRINTED DISPERSION COEFFICIENTS ARE APPARENT Dij',
     &      ' CALCULATED USING DARCY FLUX RATHER THAN SEEPAGE VELOCITY')
C
      IF(NCOL.LT.2) GOTO 920
      TEXT='Dxx^ Comp. # XXX'
      WRITE(TEXT(14:16),'(I3.3)') ICOMP
      DO K=1,NLAY
        CALL RPRINT(DXX(1:NCOL,1:NROW,K,ICOMP),TEXT,
     &              0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
      IF(NROW.LT.2.OR.ICOMP.GT.1) GOTO 910
      TEXT='Dxy^ Comp. ALL  '
      DO K=1,NLAY
        CALL RPRINT(DXY(1:NCOL,1:NROW,K),TEXT,
     &              0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  910 IF(NLAY.LT.2.OR.ICOMP.GT.1) GOTO 920
      TEXT='Dxz^ Comp. ALL  '
      DO K=1,NLAY
        CALL RPRINT(DXZ(1:NCOL,1:NROW,K),TEXT,
     &              0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  920 IF(NROW.LT.2) GOTO 950
      TEXT='Dyy^ Comp. # XXX'
      WRITE(TEXT(14:16),'(I3.3)') ICOMP
      DO K=1,NLAY
        CALL RPRINT(DYY(1:NCOL,1:NROW,K,ICOMP),TEXT,
     &              0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
      IF(NCOL.LT.2.OR.ICOMP.GT.1) GOTO 940
      TEXT='Dyx^ Comp. ALL  '
      DO K=1,NLAY
        CALL RPRINT(DYX(1:NCOL,1:NROW,K),TEXT,
     &              0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  940 IF(NLAY.LT.2.OR.ICOMP.GT.1) GOTO 950
      TEXT='Dyz^ Comp. ALL  '
      DO K=1,NLAY
        CALL RPRINT(DYZ(1:NCOL,1:NROW,K),TEXT,
     &              0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  950 IF(NLAY.LT.2) GOTO 980
      TEXT='Dzz^ Comp. # XXX'
      WRITE(TEXT(14:16),'(I3.3)') ICOMP      
      DO K=1,NLAY
        CALL RPRINT(DZZ(1:NCOL,1:NROW,K,ICOMP),TEXT,
     &              0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
      IF(NCOL.LT.2.OR.ICOMP.GT.1) GOTO 970
      TEXT='Dzx^ Comp. ALL  '
      DO K=1,NLAY
        CALL RPRINT(DZX(1:NCOL,1:NROW,K),TEXT,
     &              0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  970 IF(NROW.LT.2.OR.ICOMP.GT.1) GOTO 980
      TEXT='Dzy^ Comp. ALL  '
      DO K=1,NLAY
        CALL RPRINT(DZY(1:NCOL,1:NROW,K),TEXT,
     &              0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  980 CONTINUE
C
C--CONVERT DISPERSION COEFFICIENTS TO DISPERSION CONDUCTANCES
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
C
            IF(ICBUND(J,I,K,1).EQ.0) CYCLE
C
C--ALONG THE X-DIRECTION: DXX, DXY AND DXZ
            WW=DELR(JP1)/(DELR(J)+DELR(JP1))
            AREA=DELC(I)*(DH(J,I,K)*WW+DH(JP1,I,K)*(1.-WW))
            IF(NCOL.GT.1.AND.AREA.GT.0) THEN
              DXX(J,I,K,ICOMP)=AREA*DXX(J,I,K,ICOMP)/
     &                         (0.5*DELR(JP1)+0.5*DELR(J))
              IF(NROW.GT.1) THEN
                IF(ABS(DXY(J,I,K)-0.).GT.1.0E-6) THEN
                DXY(J,I,K)=AREA*DXY(J,I,K)/
     &                     (0.5*DELC(IM1)+DELC(I)+0.5*DELC(IP1))
                ENDIF
              ENDIF
              IF(NLAY.GT.1) THEN
                IF(ABS(DXZ(J,I,K)-0.).GT.1.0E-6) THEN
                DXZ(J,I,K)=AREA*DXZ(J,I,K)/((0.5*DH(J,I,KM1)+DH(J,I,K)+
     &                     0.5*DH(J,I,KP1))*WW+(0.5*DH(JP1,I,KM1)+
     &                     DH(JP1,I,K)+0.5*DH(JP1,I,KP1))*(1.-WW))
                ENDIF
              ENDIF
            ENDIF
C
C--ALONG THE Y-DIRECTION: DYX, DYY AND DYZ
            WW=DELC(IP1)/(DELC(I)+DELC(IP1))
            AREA=DELR(J)*(DH(J,I,K)*WW+DH(J,IP1,K)*(1.-WW))
            IF(NROW.GT.1.AND.AREA.GT.0) THEN
              DYY(J,I,K,ICOMP)=AREA*DYY(J,I,K,ICOMP)/
     &                         (0.5*DELC(IP1)+0.5*DELC(I))
              IF(NCOL.GT.1) THEN
                IF(ABS(DYX(J,I,K)-0.).GT.1.0E-6) THEN
                DYX(J,I,K)=AREA*DYX(J,I,K)/(0.5*DELR(JM1)+DELR(J)+0.5*
     &                     DELR(JP1))
                ENDIF
              ENDIF
              IF(NLAY.GT.1) THEN
                IF(ABS(DYZ(J,I,K)-0.).GT.1.0E-6) THEN
                DYZ(J,I,K)=AREA*DYZ(J,I,K)/((0.5*DH(J,I,KM1)+DH(J,I,K)+
     &                     0.5*DH(J,I,KP1))*WW+(0.5*DH(J,IP1,KM1)+
     &                     DH(J,IP1,K)+0.5*DH(J,IP1,KP1))*(1.-WW) )
                ENDIF
              ENDIF
            ENDIF
C
C--ALONG THE Z DIRECTION: DZX, DZY AND DZZ
            AREA=DELR(J)*DELC(I)
            IF(NLAY.GT.1.AND.AREA.GT.0) THEN
              DZZ(J,I,K,ICOMP)=AREA*DZZ(J,I,K,ICOMP)/
     &                         (0.5*DH(J,I,KP1)+0.5*DH(J,I,K))
              IF(NCOL.GT.1) THEN
                IF(ABS(DZX(J,I,K)-0.).GT.1.0E-6) THEN
                DZX(J,I,K)=AREA*DZX(J,I,K)/
     &                     (0.5*DELR(JM1)+DELR(J)+0.5*DELR(JP1))
                ENDIF
              ENDIF
              IF(NROW.GT.1) THEN
                IF(ABS(DZY(J,I,K)-0.).GT.1.0E-6) THEN
                DZY(J,I,K)=AREA*DZY(J,I,K)/
     &                     (0.5*DELC(IM1)+DELC(I)+0.5*DELC(IP1))
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C      
      ENDDO   !LOOP OVER ALL CHEMICAL COMPONENTS      
C
C--PRINT OUT INFORMATION ON DTDISP
      WRITE(IOUT,1500) DTDISP,KD,ID,JD
 1500 FORMAT(/1X,'MAXIMUM STEPSIZE WHICH MEETS STABILITY CRITERION',
     &           ' OF THE DISPERSION TERM'/1X,'=',G11.4,
     &           '(WHEN MIN. R.F.=1)  AT K=',I4,', I=',I4,
     &           ', J=',I4)            
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE DSP1FM(ICOMP,ICBUND,A,CNEW)
C **********************************************************************
C THIS SUBROUTINE FORMULATES THE COEFFICIENT MATRIX FOR THE DISPERSION
C TERM USING THE IMPLICIT FINITE-DIFFERENCE SCHEME.
C **********************************************************************
C
      USE UZTVARS,       ONLY: IUZFBND
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,MCOMP,DELR,DELC,DH,
     &                         DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     &                         UPDLHS,RHS,NCRS,L,NODES
C
      IMPLICIT  NONE
      INTEGER   K,I,J,KP1,KM1,IP1,IM1,JP1,JM1,ICBUND,
     &          N,II,INDEX,ICOMP
      REAL      WXP,WXM,WYP,WYM,WZP,WZM,CNEW,
     &          A,TEMP1,TEMP2,BNDTMP
      DIMENSION ICBUND(NODES,MCOMP),
     &          CNEW(NODES,MCOMP),A(NODES,*),
     &          TEMP1(7),TEMP2(19)
C
C--INITIALIZE
      WZP=0.
      WZM=0.
C
C--LOOP THROUGH EVERY FINITE DIFFERENCE CELL
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
C
C--CALCULATE THE CELL INDEX
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
C
C--SET TEMP. ARRAYS FOR PRINCIPAL AND CROSS TERMS
            DO II=1,7
              TEMP1(II)=0.
            ENDDO
            DO II=1,19
              TEMP2(II)=0.
            ENDDO
C
C--SKIP IF AT CONSTANT-CONCENTRATION OR INACTIVE CELL
            IF(ICBUND(N,ICOMP).LE.0) CYCLE
C
C--CALCULATE CELL INTERFACE WEIGHTING FACTORS
            WXP=DELR(JP1)/(DELR(J)+DELR(JP1))
            WXM=DELR(J)/(DELR(JM1)+DELR(J))
            IF(J.EQ.1) WXM=1.
            WYP=DELC(IP1)/(DELC(I)+DELC(IP1))
            WYM=DELC(I)/(DELC(IM1)+DELC(I))
            IF(I.EQ.1) WYM=1.
C
C...........TAKE CARE OF DRY CELLS    
            IF(DH(J,I,KP1).LE.0.) THEN
              WZP=0                   
            ELSE                      
              IF(DH(J,I,K).EQ.0.AND.DH(J,I,KP1).EQ.0) GOTO 10 
              WZP=DH(J,I,KP1)/(DH(J,I,K)+DH(J,I,KP1))
            ENDIF                     
C...........TAKE CARE OF DRY CELLS    
  10        IF(DH(J,I,KM1).LE.0.) THEN
              WZM=1                   
            ELSE                      
              IF(DH(J,I,KM1).EQ.0.AND.DH(J,I,K).EQ.0) GOTO 20 
              WZM=DH(J,I,K)/(DH(J,I,KM1)+DH(J,I,K))
            ENDIF
  20        IF(K.EQ.1) WZM=1.
C      
C--COEF. FOR (J,I,K)
            IF(J.GT.1) TEMP1(1)=TEMP1(1)-DXX(JM1,I,K,ICOMP)
            IF(I.GT.1) TEMP1(1)=TEMP1(1)-DYY(J,IM1,K,ICOMP)
            IF(K.GT.1) TEMP1(1)=TEMP1(1)-DZZ(J,I,KM1,ICOMP)
            IF(J.LT.NCOL) TEMP1(1)=TEMP1(1)-DXX(J,I,K,ICOMP)
            IF(I.LT.NROW) TEMP1(1)=TEMP1(1)-DYY(J,I,K,ICOMP)
            IF(K.LT.NLAY) TEMP1(1)=TEMP1(1)-DZZ(J,I,K,ICOMP)

C--BOUNDARY CONDITIONS
            BNDTMP=-DXY(J,I,K)*WXP+DXY(JM1,I,K)*(1.-WXM)
            IF(I.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(I.EQ.NROW) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DXZ(J,I,K)*WXP+DXZ(JM1,I,K)*(1.-WXM)
            IF(K.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(K.EQ.NLAY) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DYX(J,I,K)*WYP+DYX(J,IM1,K)*(1.-WYM)
            IF(J.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(J.EQ.NCOL) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DYZ(J,I,K)*WYP+DYZ(J,IM1,K)*(1.-WYM)
            IF(K.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(K.EQ.NLAY) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DZX(J,I,K)*WZP+DZX(J,I,KM1)*(1.-WZM)
            IF(J.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(J.EQ.NCOL) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DZY(J,I,K)*WZP+DZY(J,I,KM1)*(1.-WZM)
            IF(I.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(I.EQ.NROW) TEMP2(1)=TEMP2(1)-BNDTMP
C
C--COEF. FOR (J,I,K-1)
            IF(K.GT.1) THEN
              TEMP1(2)=DZZ(J,I,KM1,ICOMP)
              TEMP2(2)=-DXZ(J,I,K)*WXP+DXZ(JM1,I,K)*(1-WXM)
     &                 -DYZ(J,I,K)*WYP+DYZ(J,IM1,K)*(1-WYM)
C--BOUNDARY CONDITION
              IF(J.EQ.1)    TEMP2(2)=TEMP2(2)+DZX(J,I,KM1)*WZM
              IF(J.EQ.NCOL) TEMP2(2)=TEMP2(2)-DZX(J,I,KM1)*WZM
              IF(I.EQ.1)    TEMP2(2)=TEMP2(2)+DZY(J,I,KM1)*WZM
              IF(I.EQ.NROW) TEMP2(2)=TEMP2(2)-DZY(J,I,KM1)*WZM
            ENDIF
C      
C--COEF. FOR (J,I,K+1)
            IF(K.LT.NLAY) THEN
              TEMP1(3)=DZZ(J,I,K,ICOMP)
              TEMP2(3)=+DXZ(J,I,K)*WXP-DXZ(JM1,I,K)*(1-WXM)
     &                 +DYZ(J,I,K)*WYP-DYZ(J,IM1,K)*(1-WYM)
C--BOUNDARY CONDITION
              IF(J.EQ.1)    TEMP2(3)=TEMP2(3)-DZX(J,I,K)*(1-WZP)
              IF(J.EQ.NCOL) TEMP2(3)=TEMP2(3)+DZX(J,I,K)*(1-WZP)
              IF(I.EQ.1)    TEMP2(3)=TEMP2(3)-DZY(J,I,K)*(1-WZP)
              IF(I.EQ.NROW) TEMP2(3)=TEMP2(3)+DZY(J,I,K)*(1-WZP)
            ENDIF
C      
C--COEF. FOR (J,I-1,K)
            IF(I.GT.1) THEN
              TEMP1(4)=DYY(J,IM1,K,ICOMP)
              TEMP2(4)=-DXY(J,I,K)*WXP+DXY(JM1,I,K)*(1-WXM)
     &                -DZY(J,I,K)*WZP+DZY(J,I,KM1)*(1-WZM)
C--BOUNDARY CONDITION
              IF(J.EQ.1)    TEMP2(4)=TEMP2(4)+DYX(J,IM1,K)*WYM
              IF(J.EQ.NCOL) TEMP2(4)=TEMP2(4)-DYX(J,IM1,K)*WYM
              IF(K.EQ.1)    TEMP2(4)=TEMP2(4)+DYZ(J,IM1,K)*WYM
              IF(K.EQ.NLAY) TEMP2(4)=TEMP2(4)-DYZ(J,IM1,K)*WYM
            ENDIF
C      
C--COEF. FOR (J,I+1,K)
            IF(I.LT.NROW) THEN
              TEMP1(5)=DYY(J,I,K,ICOMP)
              TEMP2(5)=+DXY(J,I,K)*WXP-DXY(JM1,I,K)*(1-WXM)
     &                 +DZY(J,I,K)*WZP-DZY(J,I,KM1)*(1-WZM)
C--BOUNDARY CONDITION
              IF(J.EQ.1)    TEMP2(5)=TEMP2(5)-DYX(J,I,K)*(1-WYP)
              IF(J.EQ.NCOL) TEMP2(5)=TEMP2(5)+DYX(J,I,K)*(1-WYP)
              IF(K.EQ.1)    TEMP2(5)=TEMP2(5)-DYZ(J,I,K)*(1-WYP)
              IF(K.EQ.NLAY) TEMP2(5)=TEMP2(5)+DYZ(J,I,K)*(1-WYP)
            ENDIF
C      
C--COEF. FOR (J-1,I,K)
            IF(J.GT.1) THEN
              TEMP1(6)=DXX(JM1,I,K,ICOMP)
              TEMP2(6)=-DYX(J,I,K)*WYP+DYX(J,IM1,K)*(1-WYM)
     &                 -DZX(J,I,K)*WZP+DZX(J,I,KM1)*(1-WZM)
C--BOUNDARY CONDITION
         IF(I.EQ.1)    TEMP2(6)=TEMP2(6)+DXY(JM1,I,K)*WXM
         IF(I.EQ.NROW) TEMP2(6)=TEMP2(6)-DXY(JM1,I,K)*WXM
         IF(K.EQ.1)    TEMP2(6)=TEMP2(6)+DXZ(JM1,I,K)*WXM
         IF(K.EQ.NLAY) TEMP2(6)=TEMP2(6)-DXZ(JM1,I,K)*WXM
            ENDIF
C      
C--COEF. FOR (J+1,I,K)
            IF(J.LT.NCOL) THEN
              TEMP1(7)=DXX(J,I,K,ICOMP)
              TEMP2(7)=+DYX(J,I,K)*WYP-DYX(J,IM1,K)*(1-WYM)
     &                 +DZX(J,I,K)*WZP-DZX(J,I,KM1)*(1-WZM)
C--BOUNDARY CONDITION
         IF(I.EQ.1)    TEMP2(7)=TEMP2(7)-DXY(J,I,K)*(1-WXP)
         IF(I.EQ.NROW) TEMP2(7)=TEMP2(7)+DXY(J,I,K)*(1-WXP)
         IF(K.EQ.1)    TEMP2(7)=TEMP2(7)-DXZ(J,I,K)*(1-WXP)
         IF(K.EQ.NLAY) TEMP2(7)=TEMP2(7)+DXZ(J,I,K)*(1-WXP)
            ENDIF
C
C--COEF. FOR (J,I-1,K-1)
            IF(I.GT.1.AND.K.GT.1) 
     &         TEMP2(8)=DYZ(J,IM1,K)*WYM+DZY(J,I,KM1)*WZM
C      
C--COEF. FOR (J-1,I,K-1)
            IF(J.GT.1.AND.K.GT.1) 
     &         TEMP2(9)=DXZ(JM1,I,K)*WXM+DZX(J,I,KM1)*WZM
C      
C--COEF. FOR (J+1,I,K-1)
            IF(J.LT.NCOL.AND.K.GT.1) 
     &         TEMP2(10)=-DXZ(J,I,K)*(1-WXP)-DZX(J,I,KM1)*WZM
C      
C--COEF. FOR (J,I+1,K-1)
            IF(I.LT.NROW.AND.K.GT.1) 
     &         TEMP2(11)=-DYZ(J,I,K)*(1-WYP)-DZY(J,I,KM1)*WZM
C      
C--COEF. FOR (J,I-1,K+1)
            IF(I.GT.1.AND.K.LT.NLAY) 
     &         TEMP2(12)=-DYZ(J,IM1,K)*WYM-DZY(J,I,K)*(1-WZP)
C      
C--COEF. FOR (J-1,I,K+1)
            IF(J.GT.1.AND.K.LT.NLAY) 
     &         TEMP2(13)=-DXZ(JM1,I,K)*WXM-DZX(J,I,K)*(1-WZP)
C      
C--COEF. FOR (J+1,I,K+1)
            IF(J.LT.NCOL.AND.K.LT.NLAY) 
     &         TEMP2(14)=+DXZ(J,I,K)*(1-WXP)+DZX(J,I,K)*(1-WZP)
C      
C--COEF. FOR (J,I+1,K+1)
            IF(I.LT.NROW.AND.K.LT.NLAY) 
     &         TEMP2(15)=+DYZ(J,I,K)*(1-WYP)+DZY(J,I,K)*(1-WZP)
C      
C--COEF. FOR (J-1,I-1,K)
            IF(I.GT.1.AND.J.GT.1) 
     &         TEMP2(16)=+DXY(JM1,I,K)*WXM+DYX(J,IM1,K)*WYM
C      
C--COEF. FOR (J+1,I-1,K)
            IF(I.GT.1.AND.J.LT.NCOL) 
     &         TEMP2(17)=-DXY(J,I,K)*(1-WXP)-DYX(J,IM1,K)*WYM
C      
C--COEF. FOR (J-1,I+1,K)
            IF(I.LT.NROW.AND.J.GT.1)
     &         TEMP2(18)=-DXY(JM1,I,K)*WXM-DYX(J,I,K)*(1-WYP)
C      
C--COEF. FOR (J+1,I+1,K)
            IF(I.LT.NROW.AND.J.LT.NCOL) THEN
              TEMP2(19)=+DXY(J,I,K)*(1-WXP)+DYX(J,I,K)*(1-WYP)
            ENDIF
C
C--ASSIGN COEF. OF PRINCIPAL DIRECTIONS TO ARRAY [A] OR [RHS]
            DO II=1,7
               INDEX=N+L(II)
               IF(INDEX.GE.1.AND.INDEX.LE.NODES) THEN
C
C--UPDATE MATRIX A IF NEIGHBOR CELL IS ACTIVE
                 IF(ICBUND(INDEX,ICOMP).GT.0) THEN
                   IF(UPDLHS) A(N,II)=A(N,II)+TEMP1(II)
C
C--SHIFT COEF. TO THE RIGHT-HAND-SIDE, OTHERWISE
                 ELSE
                   RHS(N)=RHS(N)-TEMP1(II)*CNEW(INDEX,ICOMP)
                 ENDIF
               ENDIF
            ENDDO
C
C--ASSIGN COEF. OF CROSS TERMS TO ARRAY [A] OR [RHS]
            DO II=1,19
               INDEX=N+L(II)
               IF(INDEX.GE.1.AND.INDEX.LE.NODES) THEN
C
C--IF CROSS TERMS INCLUDED
                 IF(NCRS.GT.0.AND.ICBUND(INDEX,ICOMP).GT.0) THEN
                   IF(UPDLHS) A(N,II)=A(N,II)+TEMP2(II)
C
C--SHIFT CROSSING TERMS TO THE RIGHT-HAND-SIDE, OTHERWISE
                 ELSE
                   RHS(N)=RHS(N)-TEMP2(II)*CNEW(INDEX,ICOMP)
                 ENDIF
               ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE DSP1BD(ICOMP,DTRANS)
C **********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGET OF CONSTANT-CONCENTRATION NODES
C DUE TO DISPERSION.
C **********************************************************************
C
      USE MT3DMS_MODULE, ONLY:NCOL,NROW,NLAY,MCOMP,ICBUND,DELR,DELC,
     &                        DH,DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     &                        CNEW,BUFF,RMASIO
C
      IMPLICIT  NONE
      INTEGER   K,I,J,KP1,KM1,IP1,IM1,JP1,JM1,ICOMP
      REAL      DCFLUX,DTRANS,WXP,WXM,WYP,WYM,WZP,WZM
C
C--LOAD CNEW FOR COMPONENT [ICOMP] INTO BUFF
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=CNEW(J,I,K,ICOMP)
          ENDDO
        ENDDO
      ENDDO
C
C--LOOP THROUGH EVERY FINITE DIFFERENCE CELL
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
C
C--SKIP IF CELL IS NOT CONSTANT-CONCENTRATION
            IF(ICBUND(J,I,K,ICOMP).GE.0) CYCLE
C
C--CALCULATE INTERFACE WEIGHTING FACTORS
            WXP=DELR(JP1)/(DELR(J)+DELR(JP1))
            WXM=DELR(J)/(DELR(JM1)+DELR(J))
            WYP=DELC(IP1)/(DELC(I)+DELC(IP1))
            WYM=DELC(I)/(DELC(IM1)+DELC(I))
            WZP=DH(J,I,KP1)/(DH(J,I,K)+DH(J,I,KP1))
            WZM=DH(J,I,K)/(DH(J,I,KM1)+DH(J,I,K))
C
C--ACCUMULATE ALL COMPONENTS OF THE DISPERSIVE FLUX
            DCFLUX=0.
C
C--COMPONENTS ACROSS LEFT AND RIGHT FACES IN THE X-DIRECTION
            IF(NCOL.GT.1) THEN
              DCFLUX=DCFLUX+DXX(J,I,K,ICOMP)*(BUFF(JP1,I,K)-BUFF(J,I,K))
     &               -DXX(JM1,I,K,ICOMP)*(BUFF(J,I,K)-BUFF(JM1,I,K))
              IF(NROW.GT.1) THEN
                DCFLUX=DCFLUX+DXY(J,I,K)*(BUFF(JP1,IP1,K)*(1.-WXP)+
     &                 BUFF(J,IP1,K)*WXP-BUFF(JP1,IM1,K)*(1.-WXP)-
     &                 BUFF(J,IM1,K)*WXP)
                IF(J.GT.1) THEN
                  DCFLUX=DCFLUX-DXY(JM1,I,K)*(BUFF(J,IP1,K)*(1.-WXM)+
     &                   BUFF(JM1,IP1,K)*WXM-BUFF(J,IM1,K)*(1.-WXM)-
     &                   BUFF(JM1,IM1,K)*WXM)
                ENDIF
              ENDIF
              IF(NLAY.GT.1) THEN
                DCFLUX=DCFLUX+DXZ(J,I,K)*(BUFF(JP1,I,KP1)*(1.-WXP)+
     &                 BUFF(J,I,KP1)*WXP-BUFF(JP1,I,KM1)*(1.-WXP)-
     &                 BUFF(J,I,KM1)*WXP)
                IF(J.GT.1) THEN
                  DCFLUX=DCFLUX-DXZ(JM1,I,K)*(BUFF(J,I,KP1)*(1.-WXM)+
     &                   BUFF(JM1,I,KP1)*WXM-BUFF(J,I,KM1)*(1.-WXM)-
     &                   BUFF(JM1,I,KM1)*WXM)
                ENDIF
              ENDIF
            ENDIF
C
C--COMPONENTS ACROSS BACK AND FRONT FACES IN THE Y-DIRECTION
            IF(NROW.GT.1) THEN
              DCFLUX=DCFLUX+DYY(J,I,K,ICOMP)*(BUFF(J,IP1,K)-BUFF(J,I,K))
     &               -DYY(J,IM1,K,ICOMP)*(BUFF(J,I,K)-BUFF(J,IM1,K))
              IF(NCOL.GT.1) THEN
                DCFLUX=DCFLUX+DYX(J,I,K)*(BUFF(JP1,IP1,K)*(1.-WYP)+
     &                 BUFF(JP1,I,K)*WYP-BUFF(JM1,IP1,K)*(1.-WYP)-
     &                 BUFF(JM1,I,K)*WYP)
                IF(I.GT.1) THEN
                  DCFLUX=DCFLUX-DYX(J,IM1,K)*(BUFF(JP1,I,K)*(1.-WYM)+
     &                   BUFF(JP1,IM1,K)*WYM-BUFF(JM1,I,K)*(1.-WYM)-
     &                   BUFF(JM1,IM1,K)*WYM)
                ENDIF
              ENDIF
              IF(NLAY.GT.1) THEN
                DCFLUX=DCFLUX+DYZ(J,I,K)*(BUFF(J,IP1,KP1)*(1.-WYP)+
     &                 BUFF(J,I,KP1)*WYP-BUFF(J,IP1,KM1)*(1.-WYP)-
     &                 BUFF(J,I,KM1)*WYP)
                IF(I.GT.1) THEN
                  DCFLUX=DCFLUX-DYZ(J,IM1,K)*(BUFF(J,I,KP1)*(1.-WYM)+
     &                   BUFF(J,IM1,KP1)*WYM-BUFF(J,I,KM1)*(1.-WYM)-
     &                   BUFF(J,IM1,KM1)*WYM)
                ENDIF
              ENDIF
            ENDIF
C
C--COMPONENTS ACROSS UPPER AND LOWER FACES IN THE Z-DIRECTION
            IF(NLAY.GT.1) THEN
              DCFLUX=DCFLUX+DZZ(J,I,K,ICOMP)*(BUFF(J,I,KP1)-BUFF(J,I,K))
     &               -DZZ(J,I,KM1,ICOMP)*(BUFF(J,I,K)-BUFF(J,I,KM1))
              IF(NCOL.GT.1) THEN
                DCFLUX=DCFLUX+DZX(J,I,K)*(BUFF(JP1,I,KP1)*(1.-WZP)+
     &                 BUFF(JP1,I,K)*WZP-BUFF(JM1,I,KP1)*(1.-WZP)-
     &                 BUFF(JM1,I,K)*WZP)
                IF(K.GT.1) THEN
                  DCFLUX=DCFLUX-DZX(J,I,KM1)*(BUFF(JP1,I,K)*(1.-WZM)+
     &                   BUFF(JP1,I,KM1)*WZM-BUFF(JM1,I,K)*(1.-WZM)-
     &                   BUFF(JM1,I,KM1)*WZM)
                ENDIF
              ENDIF
              IF(NROW.GT.1) THEN
                DCFLUX=DCFLUX+DZY(J,I,K)*(BUFF(J,IP1,KP1)*(1.-WZP)+
     &                 BUFF(J,IP1,K)*WZP-BUFF(J,IM1,KP1)*(1.-WZP)-
     &                 BUFF(J,IM1,K)*WZP)
                IF(K.GT.1) THEN
                  DCFLUX=DCFLUX-DZY(J,I,KM1)*(BUFF(J,IP1,K)*(1.-WZM)+
     &                   BUFF(J,IP1,KM1)*WZM-BUFF(J,IM1,K)*(1.-WZM)-
     &                   BUFF(J,IM1,KM1)*WZM)
                ENDIF
              ENDIF
            ENDIF
C
C--ACCUMULATE MASS IN OR OUT.
            IF(DCFLUX.GT.0) THEN
              RMASIO(6,2,ICOMP)=RMASIO(6,2,ICOMP)-DCFLUX*DTRANS
            ELSE
              RMASIO(6,1,ICOMP)=RMASIO(6,1,ICOMP)-DCFLUX*DTRANS
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE DSPKEYWORDS(IN,IMSD)
C ********************************************************
C THIS SUBROUTINE READS BTN FILE AND IDENTIFIES KEYWORDS
C ********************************************************
      USE MT3DMS_MODULE, ONLY: IOUT,INOCROSS,UNSATHEAT
      USE MT3DUTIL
C
      IMPLICIT       NONE
      LOGICAL        KEYFOUND,KEYFOUND2
      CHARACTER*1000 LINE
      CHARACTER*30,  ALLOCATABLE :: KEYWORDS(:)
      INTEGER        IN,OUT,NKEYWORDS,LLOC,ISTART,ISTOP,I,IKEY,IMSD
      REAL           R
C
C SET NUMBER OF KEYWORDS AND KEYWORDS
      NKEYWORDS=3
      KEYFOUND=.FALSE.
      ALLOCATE(KEYWORDS(NKEYWORDS))
      KEYWORDS=''
      KEYWORDS(1)='MULTIDIFFUSION                '
      KEYWORDS(2)='NOCROSS                       '
      KEYWORDS(3)='UNSATHEAT                     '
C
C READ LINE WITH KEYWORDS
      LINE=''
      CALL URDCOM(IN,IOUT,LINE)
C
      LLOC=2
      DO
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        KEYFOUND2=.FALSE.
        DO IKEY=1,NKEYWORDS
          IF(LINE(ISTART:ISTOP).EQ.TRIM(KEYWORDS(IKEY))) THEN
            KEYFOUND=.TRUE.
            KEYFOUND2=.TRUE.
            EXIT
          ENDIF
        ENDDO
C
        IF(.NOT.KEYFOUND) THEN
          WRITE(IOUT,10) 
          CALL USTOP('ERROR: INVALID DSP PACKAGE INPUT KEYWORDS')
        ENDIF
C
        IF(.NOT.KEYFOUND2) THEN
          IF(LINE(ISTART:ISTOP).EQ.'') THEN
            RETURN
          ELSE
            WRITE(IOUT,10) 
            CALL USTOP('ERROR: INVALID DSP PACKAGE INPUT KEYWORDS')
          ENDIF
        ENDIF
C
        SELECT CASE(IKEY)
          CASE(1) !'MULTIDIFFUSION'
            IMSD=1
          CASE(2) !'NOCROSS'
            INOCROSS=1
          CASE(3) !'UNSATHEAT'
            UNSATHEAT=1
        END SELECT
      ENDDO
C
10    FORMAT(1X,'ERROR: INVALID DSP PACKAGE INPUT KEYWORDS')
C
      RETURN
      END SUBROUTINE
C
