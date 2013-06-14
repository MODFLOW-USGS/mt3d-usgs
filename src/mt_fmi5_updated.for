C
      SUBROUTINE FMI5AR()
C **********************************************************************
C THIS SUBROUTINE CHECKS FLOW-TRANSPORT LINK FILE AND ALLOCATES SPACE
C FOR ARRAYS THAT MAY BE NEEDED BY FLOW MODEL-INTERFACE (FMI) PACKAGE.
C **********************************************************************
C last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: INFTL,IOUT,MXTRNOP,iUnitTRNOP,NPERFL,ISS,
     &                         IVER,IFTLFMT,
     &         NPERFL,ISS,IVER,FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &         FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,
     &         FSWT,FSFR,FUZF
     
      IMPLICIT  NONE
      INTEGER   
     &          MTWEL,MTDRN,MTRCH,MTEVT,MTRIV,MTGHB,MTCHD,
     &          MTSTR,MTFHB,MTRES,MTTLK,MTIBS,MTLAK,
     &          MTDRT,MTETS,MTMNW,MTSWT,MTSFR,MTUZF,IERR
      CHARACTER VERSION*11
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INFTL
 1030 FORMAT(1X,'FMI5 -- FLOW MODEL INTERFACE PACKAGE,',
     & ' VERSION 5, FEBRUARY 2010, INPUT READ FROM UNIT',I3)
C
C--ALLOCATE
      ALLOCATE(NPERFL,ISS,IVER,FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &         FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,
     &         FSWT,FSFR,FUZF)
C
C--INITIALIZE
      ISS=1
      NPERFL=0
      IVER=2
      VERSION=' '
      FWEL=.FALSE.
      FDRN=.FALSE.
      FRCH=.FALSE.
      FEVT=.FALSE.
      FRIV=.FALSE.
      FGHB=.FALSE.
      FSTR=.FALSE.
      FRES=.FALSE.
      FFHB=.FALSE.
      FIBS=.FALSE.
      FTLK=.FALSE.
      FLAK=.FALSE.
      FMNW=.FALSE.
      FDRT=.FALSE.
      FETS=.FALSE.
      FSWT=.FALSE.
      FSFR=.FALSE.
      FUZF=.FALSE.
      MTSTR=0
      MTRES=0
      MTFHB=0
      MTDRT=0
      MTETS=0
      MTTLK=0
      MTIBS=0
      MTLAK=0
      MTMNW=0
      MTSWT=0
      MTSFR=0
      MTUZF=0
C
C--READ HEADER OF FLOW-TRANSPORT LINK FILE
      IF(IFTLFMT.EQ.0) THEN
        READ(INFTL,ERR=100,IOSTAT=IERR) VERSION,MTWEL,MTDRN,MTRCH,
     &   MTEVT,MTRIV,MTGHB,MTCHD,ISS,NPERFL
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INFTL,*,ERR=100,IOSTAT=IERR) VERSION,MTWEL,MTDRN,MTRCH,
     &   MTEVT,MTRIV,MTGHB,MTCHD,ISS,NPERFL
      ENDIF
C
  100 IF(VERSION(1:4).NE.'MT3D'.OR.IERR.NE.0) THEN
        GOTO 500
      ELSEIF(VERSION(1:11).EQ.'MT3D4.00.00') THEN
        REWIND(INFTL)
        IF(IFTLFMT.EQ.0) THEN
          READ(INFTL) VERSION,MTWEL,MTDRN,MTRCH,MTEVT,
     &     MTRIV,MTGHB,MTCHD,ISS,NPERFL,
     &     MTSTR,MTRES,MTFHB,MTDRT,MTETS,MTTLK,MTIBS,MTLAK,MTMNW,
     &     MTSWT,MTSFR,MTUZF
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INFTL,*) VERSION,MTWEL,MTDRN,MTRCH,MTEVT,
     &     MTRIV,MTGHB,MTCHD,ISS,NPERFL,
     &     MTSTR,MTRES,MTFHB,MTDRT,MTETS,MTTLK,MTIBS,MTLAK,MTMNW,
     &     MTSWT,MTSFR,MTUZF
        ENDIF
      ENDIF
C
C--DETERMINE WHICH FLOW COMPONENTS USED IN FLOW MODEL
      IF(MTWEL.GT.0) FWEL=.TRUE.
      IF(MTDRN.GT.0) FDRN=.TRUE.
      IF(MTRCH.GT.0) FRCH=.TRUE.
      IF(MTEVT.GT.0) FEVT=.TRUE.
      IF(MTRIV.GT.0) FRIV=.TRUE.
      IF(MTGHB.GT.0) FGHB=.TRUE.
      IF(MTSTR.GT.0) FSTR=.TRUE.
      IF(MTRES.GT.0) FRES=.TRUE.
      IF(MTFHB.GT.0) FFHB=.TRUE.
      IF(MTIBS.GT.0) FIBS=.TRUE.
      IF(MTTLK.GT.0) FTLK=.TRUE.
      IF(MTLAK.GT.0) FLAK=.TRUE.
      IF(MTMNW.GT.0) FMNW=.TRUE.
      IF(MTDRT.GT.0) FDRT=.TRUE.
      IF(MTETS.GT.0) FETS=.TRUE.
      IF(MTSWT.GT.0) FSWT=.TRUE.
      IF(MTSFR.GT.0) FSFR=.TRUE.
      IF(MTUZF.GT.0) FUZF=.TRUE.
C
C--DETERMINE IF THE SSM PACKAGE IS REQUIRED
  200 IF(iUnitTRNOP(3).EQ.0) THEN
        IF(FWEL.OR.FDRN.OR.FRCH.OR.FEVT.OR.FRIV.OR.FGHB.OR.
     &   FSTR.OR.FRES.OR.FFHB.OR.FIBS.OR.FTLK.OR.FLAK.OR.FMNW.OR.
     &   FDRT.OR.FETS.OR.FSWT.OR.FSFR.OR.FUZF) THEN
          WRITE(*,300)
          CALL USTOP(' ')
        ELSEIF(MTCHD.GT.0) THEN
          WRITE(*,302)
          CALL USTOP(' ')
        ELSEIF(ISS.EQ.0) THEN
          WRITE(*,304)
          CALL USTOP(' ')
        ENDIF
      ENDIF
  300 FORMAT(/1X,'ERROR: THE SSM PACKAGE MUST BE USED',
     & ' IN THE CURRENT SIMULATION',
     & /1X,'BECAUSE THE FLOW MODEL INCLUDES A SINK/SOURCE PACKAGE.')
  302 FORMAT(/1X,'ERROR: THE SSM PACKAGE MUST BE USED',
     & ' IN THE CURRENT SIMULATION',
     & /1X,'BECAUSE THE FLOW MODEL CONTAINS CONSTANT-HEAD CELLS.')
  304 FORMAT(/1X,'ERROR: THE SSM PACKAGE MUST BE USED',
     & ' IN THE CURRENT SIMULATION',
     & /1X,'BECAUSE THE FLOW MODEL IS TRANSIENT.')
C
C--PRINT KEY INFORMATION OF THE FLOW MODEL
      IF(ISS.EQ.0) THEN
        WRITE(IOUT,310)
      ELSE
        WRITE(IOUT,320)
      ENDIF
      IF(MTCHD.GT.0) WRITE(IOUT,330)
      WRITE(IOUT,'(1X)')
  310 FORMAT(1X,'FLOW MODEL IS TRANSIENT')
  320 FORMAT(1X,'FLOW MODEL IS STEADY-STATE')
  330 FORMAT(1X,'FLOW MODEL CONTAINS CONSTANT-HEAD CELLS')
C
C--DONE, RETURN
      GOTO 1000
C
C--ERROR READING THE FLOW-TRANSPORT LINK FILE
  500 WRITE(*,600)
      WRITE(IOUT,600)
      CALL USTOP(' ')
  600 FORMAT(/1X,'Error Reading Flow-Transport Link File',
     & ' Possibly Caused by:',
     & /1X,'1. Incompatible Styles of Unformatted Files',
     & ' Used by MODFLOW and MT3DMS;'
     & /1X,'2. Unformatted Flow-Transport Link File Saved by',
     & ' Verison 1 of LinkMT3D',
     & /1X,'   Package Which Is No Longer Supported by MT3DMS.')
C
 1000 RETURN
      END
C
C
      SUBROUTINE FMI5RP1(KPER,KSTP)
C **********************************************************************
C THIS SUBROUTINE READS SATURATED CELL THICKNESS, FLUXES ACROSS CELL
C INTERFACES, AND FLOW RATE TO OR FROM TRANSIENT STORAGE
C FROM AN UNFORMATTED FILE SAVED BY THE FLOW MODEL, AND PREPARES THEM
C IN THE FORMS NEEDED BY THE TRANSPORT MODEL.
C **********************************************************************
C last modified: 02-15-2005
C
      USE MIN_SAT                                                   !# LINE 168 FMI
      USE MT3DMS_MODULE, ONLY: INFTL,IOUT,NCOL,NROW,NLAY,NCOMP,
     &                         FPRT,LAYCON,ICBUND,HORIGN,DH,PRSITY,
     &                         DELR,DELC,DZ,XBC,YBC,ZBC,QSTO,COLD,CNEW,
     &                         RETA,QX,QY,QZ,DTRACK,DTRACK2,THKMIN,ISS,
     &                         IVER,
     &                         FUZF,WC,PRSITY,UZFLX,UZQSTO,SURFLK,  !edm
     &                         SATOLD,SATNEW,PRSITYSAV,IUZFOPT,     !edm
     &                         IUZFOPTG,IUZFBND,                    !edm
     &                         NOCREWET                             !# LINE 159 FMI
C
      IMPLICIT  NONE
      INTEGER   INUF,J,I,K,KPER,KSTP,
     &          JTRACK,ITRACK,KTRACK,INDEX,
     &          IC,IR                                               !edm
      REAL      WW,WTBL,THKSAT,TK,CTMP,CREWET,THKMIN0
      CHARACTER TEXT*16
C
      INUF=INFTL
C                                                                   !edm
C--READ UNSAT ZONE WATER CONTENT (UNITLESS)                         !edm
      IF(FUZF) THEN                                                 !edm
        IF(IUZFOPTG.GT.0) THEN                                      !edm
          TEXT='WATER CONTENT   '                                   !edm
          CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,      !edm
     &                WC,FPRT)                                      !edm
        ENDIF
C                                                                   !edm
C--READ UPPER-FACE FLUX TERMS                                       !edm
        TEXT='UZ FLUX         '                                     !edm
        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,        !edm
     &              UZFLX,FPRT)                                     !edm
C                                                                   !edm
C--READ UNSATURATED ZONE STORAGE TERM (UNIT: L**3/T)                !edm
        TEXT='UZQSTO          '                                     !edm
        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,        !edm
     &              UZQSTO,FPRT)                                    !edm
C                                                                   !edm
C--READ SURFACE LEAKANCE TERM (UNIT: L**3/T)                        !edm
        TEXT='GWQOUT          '                                     !edm
        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,        !edm
     &              SURFLK,FPRT)                                    !edm
      ENDIF                                                         !edm
C
C--READ SATURATED THICKNESS (UNIT: L).
      IF(IVER.EQ.2) THEN
        TEXT='THKSAT'
      ELSEIF(IVER.EQ.1) THEN
        TEXT='HEAD'
      ENDIF
      CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,DH,FPRT)
      DO K=1,NLAY                                                   !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            IF(ICBUND(J,I,K,1).NE.0 .AND. DH(J,I,K).GT.1e25) THEN   !edm
              DH(J,I,K)=0                                           !edm
            ENDIF                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C
C--READ RIGHT-FACE FLOW TERMS IF MORE THAN ONE COLUMN (UNIT: L**3/T).
      IF(NCOL.LT.2) GOTO 100
      TEXT='QXX'
      CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,QX,FPRT)
C
C--READ FRONT-FACE FLOW TERMS IF MORE THAN ONE ROW (UNIT: L**3/T).
  100 IF(NROW.LT.2) GOTO 110
      TEXT='QYY'
      CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,QY,FPRT)
C
C--READ LOWER-FACE FLOW TERMS IF MORE THAN ONE LAYER (UNIT: L**3/T).
  110 IF(NLAY.LT.2) GOTO 120
      TEXT='QZZ'
      CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,QZ,FPRT)
C
C--READ STORAGE TERM (UNIT: L**3/T).
  120 TEXT='STO'
      IF(IVER.EQ.2.AND.ISS.EQ.0) THEN
        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,QSTO,FPRT)
      ENDIF
C
C--ONLY PERFORM THE NEXT BIT OF CODE IF UZF IS ACTIVE IN THE        !edm
C--CURRENT CELL                                                     !edm
      IF(FUZF.AND. .NOT.IUZFOPTG.EQ.0) THEN                         !edm
C--IF NOT THE FIRST TIME STEP, COPY SATNEW TO SATOLD                !edm
        IF(KPER.NE.1 .OR. KSTP.NE.1) THEN                           !edm
          DO K=1,NLAY                                               !edm
            DO I=1,NROW                                             !edm
              DO J=1,NCOL                                           !edm
                IF(IUZFBND(J,I).GT.0) THEN                          !edm
                  SATOLD(J,I,K)=SATNEW(J,I,K)                       !edm
                ENDIF                                               !edm
              ENDDO                                                 !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
        ENDIF                                                       !edm
C                                                                   !edm
C--COMPUTE SATURATION                                               !edm
        PRSITY=>PRSITYSAV                                           !edm
        DO K=1,NLAY                                                 !edm
          DO I=1,NROW                                               !edm
            DO J=1,NCOL                                             !edm
              IF(DH(J,I,K).GE.DZ(J,I,K)) THEN                       !edm
                SATNEW(J,I,K)=1                                     !edm
              ELSEIF(DH(J,I,K).LT.DZ(J,I,K).AND.                    !edm
     &        .NOT.ICBUND(J,I,K,1).EQ.0) THEN                       !edm
                IF(INT(DH(J,I,K)).EQ.-111) THEN                     !edm 6/14/13
                  SATNEW(J,I,K)).EQ.-111) THEN                      !edm 6/14/13
                ELSE                                                !edm 6/14/13
                  SATNEW(J,I,K)=((DZ(J,I,K)-DH(J,I,K))/DZ(J,I,K))*  !edm
     &                          WC(J,I,K)/PRSITY(J,I,K)+            !edm
     &                          DH(J,I,K)/DZ(J,I,K)*1               !edm
                ENDIF                                               !edm 6/14/13
                SATNEW(J,I,K)=((DZ(J,I,K)-DH(J,I,K))/DZ(J,I,K))*    !edm

              ENDIF                                                 !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
C                                                                   !edm
C--MELD UZFLX AND QZZ ARRAY SO THAT UZFLX DOESN'T NEED TO BE DEALT  !edm
C--WITH LATER IN THE CODE, IT'LL INSTEAD BE IMPLICIT IN QZ          !edm
        DO K=1,(NLAY-1)                                             !edm
          DO I=1,NROW                                               !edm
            DO J=1,NCOL                                             !edm
              IF(ICBUND(J,I,K,1).NE.0) THEN                         !edm
C--THE NEXT LINE THAT CHECKS FOR -111 IS DUE TO CONFINED LAYERS,    !edm 6/14/13
C  ENSURES QZ ISN'T SET TO UZFLX IN THE EVENT DH = -111             !edm 6/14/13
                IF(INT(DH(J,I,K).EQ.-111) DH(J,I,K)=DZ(J,I,K)       !edm
                IF(DH(J,I,K).LT.1E-5) THEN                          !edm
                  QZ(J,I,K)=UZFLX(J,I,K+1)                          !edm
                ENDIF                                               !edm
              ENDIF                                                 !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
C                                                                   !edm
C--PRSITY IS USED BELOW AND THEREFORE NEEDS TO BE UPDATED HERE      !edm
C--FOR BOTH THE SATURATED AND UNSATURATED CASE                      !edm
        DO K=1,NLAY                                                 !edm
          DO I=1,NROW                                               !edm
            DO J=1,NCOL                                             !edm
              WC(J,I,K)=SATNEW(J,I,K)*PRSITY(J,I,K)                 !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
        PRSITYSAV=>PRSITY                                           !edm
        PRSITY=>WC                                                  !edm
      ENDIF                                                         !edm
C
C--SET ICBUND=0 IF CELL IS DRY OR INACTIVE (INDICATED BY FLAG 1.E30)
C--AND REACTIVATE DRY CELL IF REWET AND ASSIGN CONC AT REWET CELL
C--WITH UZF TURNED ON THE GRID BECOMES FIXED.  THE USER PROVIDEDED  !edm
C--ICBUND ARRAY SHOULD REMAIN UNTOUCHED                             !edm
      IF(.NOT.FUZF.OR.IUZFOPTG.EQ.0) THEN                           !edm
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ABS(DH(J,I,K)-1.E30).LT.1.E-5) THEN
                ICBUND(J,I,K,1)=0
              ELSEIF(ICBUND(J,I,K,1).EQ.0.AND.PRSITY(J,I,K).GT.0) THEN
                ICBUND(J,I,K,1)=30000
                DO INDEX=1,NCOMP
                  CTMP=CREWET(NCOL,NROW,NLAY,CNEW(:,:,:,INDEX),
     &                        ICBUND,XBC,YBC,ZBC,J,I,K)
                  CTMP=(COLD(J,I,K,INDEX)*(RETA(J,I,K,INDEX)-1.0)+CTMP)
     &                /RETA(J,I,K,INDEX)
                  IF(NOCREWET.EQ.1) CTMP=0.                         !# LINE 228 FMI
                  CNEW(J,I,K,INDEX)=CTMP
                  IF(MUTDRY.EQ.0) THEN                              !# LINE 230 FMI
                    WRITE(IOUT,122) K,I,J,INDEX,CNEW(J,I,K,INDEX)
                  ENDIF                                             !# LINE 230 FMI
                ENDDO
              ENDIF
            ENDDO
          ENDDO
        ENDDO
  122   FORMAT(/1X,'DRY CELL REACTIVATED AT K =',I4,',   I=',I4,
     &   ',   J=',I4/1X,'FOR SPECIES ',I3.3,
     &   ' WITH STARTING CONCENTRATION =',G13.5)
      ENDIF                                                         !edm
C
C--SET SATURATED THICKNESS [DH] TO LAYER THICKNESS [DZ]
C--FOR CONFINED LAYERS IF THE FLOW-TRANSPORT LINK FILE
C--IS SAVED BY LKMT PACKAGE VERSION 2 OR LATER
      IF(IVER.EQ.2) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,1).EQ.0) CYCLE
              IF(.NOT.FUZF) THEN                                    !edm
                IF(LAYCON(K).EQ.0.OR.INT(DH(J,I,K)).EQ.-111) THEN
                  DH(J,I,K)=DZ(J,I,K)
                ENDIF                                               !edm
              ELSEIF(FUZF.AND. .NOT.IUZFOPTG.EQ.0) THEN             !edm
C--SET DH EQUAL TO DZ FOR THE CASE WHEN THE UZF PACKAGE IS ACTIVE   !edm
                IF(IUZFBND(J,I).GT.0) THEN                          !edm
                  DH(J,I,K)=DZ(J,I,K)                               !edm
                ENDIF                                               !edm
              ELSEIF(FUZF.AND.IUZFOPTG.EQ.0) THEN                   !edm
                !don't need to do anything in this case             !edm
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
C--CALCULATE SATURATED THICKNESS FROM INPUT ARRAYS [HTOP] AND [DZ]
C--IF THE FLOW-TRANSPORT LINK FILE IS SAVED BY LKMT PACKAGE VER 1
      ELSEIF(IVER.EQ.1) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,1).EQ.0) CYCLE
              IF(LAYCON(K).EQ.0) THEN
                THKSAT=DZ(J,I,K)
              ELSE
                WTBL=HORIGN-DH(J,I,K)
                THKSAT=ZBC(J,I,K)+0.5*DZ(J,I,K)-WTBL
                THKSAT=MIN(THKSAT,DZ(J,I,K))
              ENDIF
              DH(J,I,K)=THKSAT
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C--SET CELLS TO INACTIVE IF SATURATED THICKNESS < OR = 0, OR
C--IF SATURATED THICKNESS IS BELOW USER-SPECIFIED MINIMUM [THKMIN].
C--WITH UZF TURNED ON THE GRID BECOMES FIXED.  THE USER PROVIDEDED  !edm
C--ICBUND ARRAY SHOULD REMAIN UNTOUCHED                             !edm
      IF(DRYON.EQ..TRUE.) THEN                                      !# LINE 279 FMI
        NICBND2=0                                                   !# LINE 280 FMI
        ICBND2=0                                                    !# LINE 281 FMI
      ENDIF                                                         !# LINE 282 FMI
      IF(.NOT.FUZF) THEN                                            !edm
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,1).EQ.0) CYCLE
              IF(DH(J,I,K).LE.0) THEN
                IF(DRYON.EQ..TRUE.) THEN                            !# LINE 288 FMI
                  ICBND2(J,I,K)=1                                   !# LINE 289 FMI
                  NICBND2=NICBND2+1                                 !# LINE 290 FMI
                ENDIF                                               !# LINE 291 FMI
                IF(MUTDRY.EQ.0) THEN                                !# LINE 292 FMI
                  WRITE(IOUT,355) DH(J,I,K),K,I,J
                ENDIF                                               !# LINE 294 FMI
                ICBUND(J,I,K,1)=0
              ELSEIF(THKMIN.GT.0) THEN
                THKMIN0=THKMIN*DZ(J,I,K)
                IF(IABSMIN.EQ.1) THKMIN0=THKMIN                     !# LINE 298 FMI
                IF(DH(J,I,K).LT.THKMIN0) THEN
                  IF(DRYON.EQ..TRUE.) THEN                          !# LINE 300 FMI
                    ICBND2(J,I,K)=1                                 !# LINE 301 FMI
                    NICBND2=NICBND2+1                               !# LINE 302 FMI
                  ENDIF                                             !# LINE 303 FMI
                  IF(MUTDRY.EQ.0) THEN                              !# LINE 304 FMI
                    WRITE(IOUT,365) DH(J,I,K),THKMIN0,K,I,J
                  ENDIF                                             !# LINE 306 FMI
                  ICBUND(J,I,K,1)=0
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
  355   FORMAT(/1X,'WARNING: SATURATED THICKNESS =',G13.5,
     &   ' NOT ALLOWED IN TRANSPORT MODEL'
     &   /10X,'AT ACTIVE CELL K =',I4,',   I=',I4,',   J=',I4,
     &   ';  RESET AS INACTIVE')
  365   FORMAT(/1X,'WARNING: SATURATED THICKNESS =',G13.5,
     &   ' BELOW SPECIFIED MINIMUM =',G13.5,
     &   /10X,'AT ACTIVE CELL K =',I4,',   I=',I4,',   J=',I4,
     &   ';  RESET AS INACTIVE')
      ENDIF                                                       !edm
C
C--DETERMINE MAXIMUM TIME INCREMENT DURING WHICH ANY PARTICLE
C--CANNOT MOVE MORE THAN ONE CELL IN ANY DIRECTION.
      DTRACK=1.E30
C
      IF(NCOL.LT.2) GOTO 410
      DO K=1,NLAY
        DO I=1,NROW
          DO J=2,NCOL
C
            IF(ICBUND(J,I,K,1).NE.0) THEN
              TK=0.5*(QX(J-1,I,K)+QX(J,I,K))
              IF(TK.EQ.0) CYCLE
              TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
              IF(ABS(TK).LT.DTRACK) THEN
                DTRACK=ABS(TK)
                JTRACK=J
                ITRACK=I
                KTRACK=K
              ENDIF
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
  410 IF(NROW.LT.2) GOTO 420
      DO K=1,NLAY
        DO J=1,NCOL
          DO I=2,NROW
C
            IF(ICBUND(J,I,K,1).NE.0) THEN
              TK=0.5*(QY(J,I-1,K)+QY(J,I,K))
              IF(TK.EQ.0) CYCLE
              TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
              IF(ABS(TK).LT.DTRACK) THEN
                DTRACK=ABS(TK)
                JTRACK=J
                ITRACK=I
                KTRACK=K
              ENDIF
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
  420 IF(NLAY.LT.2) GOTO 430
      DO J=1,NCOL
        DO I=1,NROW
          DO K=2,NLAY
C
            IF(ICBUND(J,I,K,1).NE.0) THEN
              TK=0.5*(QZ(J,I,K-1)+QZ(J,I,K))
              IF(TK.EQ.0) CYCLE
              TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
              IF(ABS(TK).LT.DTRACK) THEN
                DTRACK=ABS(TK)
                JTRACK=J
                ITRACK=I
                KTRACK=K
              ENDIF
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
C--PRINT INFORMATION ON DTRACK
  430 WRITE(IOUT,500) DTRACK,KTRACK,ITRACK,JTRACK
  500 FORMAT(/1X,'MAXIMUM STEPSIZE DURING WHICH ANY PARTICLE CANNOT',
     & ' MOVE MORE THAN ONE CELL'/1X,'=',G11.4,
     & '(WHEN MIN. R.F.=1)  AT K=',I4,', I=',I4,
     & ', J=',I4)
C
C--DETERMINE STABILITY CRITERION ASSOCIATED WITH EXPLICIT FINITE
C--DIFFERENCE SOLUTION OF THE ADVECTION TERM
      DTRACK2=1.E30
C
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,1).EQ.0) CYCLE
            TK=0.
            IF(J.GT.1) TK=TK+MAX(ABS(QX(J-1,I,K)),ABS(QX(J,I,K)))
            IF(I.GT.1) TK=TK+MAX(ABS(QY(J,I-1,K)),ABS(QY(J,I,K)))
            IF(K.GT.1) TK=TK+MAX(ABS(QZ(J,I,K-1)),ABS(QZ(J,I,K)))
            IF(TK.EQ.0) CYCLE
            TK=DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)/TK
            IF(TK.LT.DTRACK2) THEN
              DTRACK2=TK
              JTRACK=J
              ITRACK=I
              KTRACK=K
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--PRINT INFORMATION ON DTRACK2
      WRITE(IOUT,550) DTRACK2,KTRACK,ITRACK,JTRACK
  550 FORMAT(/1X,'MAXIMUM STEPSIZE WHICH MEETS STABILITY CRITERION',
     & ' OF THE ADVECTION TERM'/1X,
     & '(FOR PURE FINITE-DIFFERENCE OPTION, MIXELM=0) '/1X,'=',G11.4,
     & '(WHEN MIN. R.F.=1)  AT K=',I4,', I=',I4,', J=',I4)
C
C--DIVIDE VOLUMETRIC QX, QY AND QZ BY AREAS
C--TO GET SPECIFIC DISCHAGES ACROSS EACH CELL INTERFACE
      IF(NCOL.LT.2) GOTO 910
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL-1
            WW=DELR(J+1)/(DELR(J+1)+DELR(J))
            THKSAT=DH(J,I,K)*WW+DH(J+1,I,K)*(1.-WW)
            IF(DOMINSAT.EQ..TRUE.) THEN                            !# LINE 438 FMI
              IF(DH(J,I,K).LE.0. .OR. DH(J+1,I,K).LE.0.) THEN      !# LINE 439 FMI
                IF(DH(J,I,K).GT.0.) THEN                           !# LINE 440 FMI
                  THKSAT=DH(J,I,K)*WW                              !# LINE 441 FMI
                ELSEIF(DH(J+1,I,K).GT.0.) THEN                     !# LINE 442 FMI
                  THKSAT=DH(J+1,I,K)*WW                            !# LINE 443 FMI
                ELSE                                               !# LINE 444 FMI
                  THKSAT=1.0E30                                    !# LINE 445 FMI
                ENDIF                                              !# LINE 446 FMI
              ENDIF                                                !# LINE 447 FMI
              THKSAT=ABS(DH(J,I,K))*WW+ABS(DH(J+1,I,K))*(1.-WW)    !# LINE 448 FMI
            ENDIF                                                  !# LINE 449 FMI
            IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
              IF(DOMINSAT.EQ..TRUE.) THEN                          !# LINE 451 FMI
                QX(J,I,K)=QX(J,I,K)/(DELC(I)*THKSAT)               !# LINE 452 FMI
              ELSE                                                 !# LINE 453 FMI
                QX(J,I,K)=0
                IF(J.GT.1) QX(J-1,I,K)=0.
              ENDIF                                                !# LINE 456 FMI
            ELSE
              QX(J,I,K)=QX(J,I,K)/(DELC(I)*THKSAT)
            ENDIF
          ENDDO
          IF(DOMINSAT.EQ..FALSE.) THEN                             !# LINE 461 FMI
            IF(ICBUND(NCOL,I,K,1).EQ.0) QX(NCOL-1,I,K)=0.
          ENDIF                                                    !# LINE 463 FMI
        ENDDO
      ENDDO
C
  910 IF(NROW.LT.2) GOTO 920
      DO K=1,NLAY
        DO J=1,NCOL
          DO I=1,NROW-1
            WW=DELC(I+1)/(DELC(I+1)+DELC(I))
            THKSAT=DH(J,I,K)*WW+DH(J,I+1,K)*(1.-WW)
            IF(DOMINSAT.EQ..TRUE.) THEN                            !# LINE 473 FMI
              IF(DH(J,I,K).LE.0. .OR. DH(J,I+1,K).LE.0.) THEN      !# LINE 474 FMI
                IF(DH(J,I,K).GT.0.) THEN                           !# LINE 475 FMI
                  THKSAT=DH(J,I,K)*WW                              !# LINE 476 FMI
                ELSEIF(DH(J,I+1,K).GT.0.) THEN                     !# LINE 477 FMI
                  THKSAT=DH(J,I+1,K)*WW                            !# LINE 478 FMI
                ELSE                                               !# LINE 479 FMI
                  THKSAT=1.0E30                                    !# LINE 480 FMI
                ENDIF                                              !# LINE 481 FMI
              ENDIF                                                !# LINE 482 FMI
              THKSAT=ABS(DH(J,I,K))*WW+ABS(DH(J,I+1,K))*(1.-WW)    !# LINE 483 FMI
            ENDIF                                                  !# LINE 484 FMI
            IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
              IF(DOMINSAT.EQ..TRUE.) THEN                          !# LINE 486 FMI
                QY(J,I,K)=QY(J,I,K)/(DELR(J)*THKSAT)               !# LINE 487 FMI
              ELSE                                                 !# LINE 488 FMI
                QY(J,I,K)=0
                IF(I.GT.1) QY(J,I-1,K)=0.
              ENDIF                                                !# LINE 491 FMI
            ELSE
              QY(J,I,K)=QY(J,I,K)/(DELR(J)*THKSAT)
            ENDIF
          ENDDO
          IF(DOMINSAT.EQ..FALSE.) THEN                             !# LINE 496 FMI
            IF(ICBUND(J,NROW,K,1).EQ.0) QY(J,NROW-1,K)=0.
          ENDIF                                                    !# LINE 498 FMI
        ENDDO
      ENDDO
C
  920 IF(NLAY.LT.2) GOTO 990
      DO J=1,NCOL
        DO I=1,NROW
          DO K=1,NLAY
            THKSAT=DH(J,I,K)
            IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
              IF(DOMINSAT.EQ..TRUE.) THEN                          !# LINE 508 FMI
                QZ(J,I,K)=QZ(J,I,K)/(DELR(J)*DELC(I))              !# LINE 509 FMI
              ELSE                                                 !# LINE 510 FMI
                QZ(J,I,K)=0
                IF(K.GT.1) QZ(J,I,K-1)=0.
              ENDIF                                                !# LINE 513 FMI
            ELSE
              QZ(J,I,K)=QZ(J,I,K)/(DELR(J)*DELC(I))
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--DIVIDE STORAGE BY CELL VOLUME TO GET DIMENSION (1/TIME)
  990 CONTINUE
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            THKSAT=DH(J,I,K)  !WHEN FUZF=1, DH IS DZ
            IF(THKSAT.LE.0.OR.ICBUND(J,I,K,1).EQ.0) THEN
              IF(DOMINSAT.EQ..TRUE.) THEN                           !# LINE 528 FMI
                QSTO(J,I,K)=QSTO(J,I,K)/(THKSAT*DELR(J)*DELC(I))    !# LINE 529 FMI
              ELSE                                                  !# LINE 530 FMI
                QSTO(J,I,K)=0
              ENDIF                                                 !# LINE 532 FMI
            ELSE
              IF(FUZF) THEN
                QSTO(J,I,K)=(QSTO(J,I,K)+UZQSTO(J,I,K))/            !edm
     &                       (THKSAT*DELR(J)*DELC(I))               !edm
              ELSE                                                  !edm
                QSTO(J,I,K)=QSTO(J,I,K)/(THKSAT*DELR(J)*DELC(I))    !edm
              ENDIF                                                 !edm
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C--DIVIDE SURFACE LEAKANCE BY CELL VOL. TO GET DIMENSION (1/TIME)   !edm
C--UZFLX                                                            !edm
      IF(FUZF) THEN                                                 !edm
        DO J=1,NCOL                                                 !edm
          DO I=1,NROW                                               !edm
            DO K=1,NLAY                                             !edm
              THKSAT=DH(J,I,K)                                      !edm
              IF(ICBUND(J,I,K,1).EQ.0) THEN                         !edm
                SURFLK(J,I,K)=0                                     !edm
              ELSEIF(ICBUND(J,I,K,1).GT.0.AND. .NOT.THKSAT.EQ.0) THEN !edm
                SURFLK(J,I,K)=SURFLK(J,I,K)/(DELR(J)*DELC(I)*THKSAT)!edm
              ENDIF                                                 !edm
            ENDDO                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDIF                                                         !edm

C
C--SYNCHRONIZE ICBUND CONDITIONS OF ALL SPECIES
      IF(NCOMP.EQ.1) GOTO 999
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            DO INDEX=2,NCOMP
              IF(ICBUND(J,I,K,INDEX).GE.0) THEN
                ICBUND(J,I,K,INDEX)=IABS(ICBUND(J,I,K,1))
              ELSEIF(ICBUND(J,I,K,1).EQ.0) THEN
                ICBUND(J,I,K,INDEX)=0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
  999 RETURN
      END
C
C
      SUBROUTINE FMI5RP2(KPER,KSTP)
C **********************************************************************
C THIS SUBROUTINE READS THE LOCATIONS AND FLOW RATES OF SINKS & SOURCES
C FROM AN UNFORMATTED FILE SAVED BY THE FLOW MODEL, AND PREPARES THEM
C IN THE FORMS NEEDED BY THE TRANSPORT MODEL.
C **********************************************************************
C last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: INFTL,IOUT,NCOL,NROW,NLAY,NCOMP,FPRT,
     &                         LAYCON,ICBUND,DH,PRSITY,DELR,DELC,IRCH,
     &                         RECH,IEVT,EVTR,MXSS,NSS,NTSS,SS,BUFF,
     &                         DTSSM,
     &                         UZET,GWET,IETFLG,FINFIL,UZFLX,SATNEW,!edm
     &                         FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &                         FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,
     &                         FETS,FSWT,FSFR,FUZF,ISS,NPERFL,
     &                         CNEW,SSMC,KSSZERO                    !# LINE 563 FMI
C
      IMPLICIT  NONE
      INTEGER   INUF,J,I,K,
     &          NUM,KPER,KSTP,IQ,KSSM,ISSM,JSSM,
     &          JJ,II,KK,JM1,JP1,IM1,IP1,KM1,KP1,INDEX
      INTEGER   INCTS,KOLD                                          !# LINE 576 FMI
      REAL      VOLAQU,TM
      CHARACTER TEXT*16
C
      INUF=INFTL
C
C--RESET TOTAL NUMBER OF POINT SINKS/SOURCES AND CLEAR FLOW RATES
      NTSS=NSS
      SS(8,:)=0.                                                    !# LINE 593 FMI
      DO NUM=1,NTSS
        SS(5,NUM)=0.
      ENDDO
C
C--RESET [ICBUND] VALUE AT ACTIVE CELLS TO UNITY
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,1).GT.0) ICBUND(J,I,K,1)=1
          ENDDO
        ENDDO
      ENDDO
C
C--READ CONSTANT-HEAD FLOW TERM (UNIT: L**3/T).
      TEXT='CNH'
      IQ=1
      CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
C
C--READ WELL FLOW TERM (L**3/T) IF WELL OPTION USED IN FLOW MODEL.
      IF(FWEL) THEN
        TEXT='WEL'
        IQ=2
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ DRAIN FLOW TERM (L**3/T) IF DRAIN OPTION USED IN FLOW MODEL.
      IF(FDRN) THEN
        TEXT='DRN'
        IQ=3
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ RECHARGE FLOW TERM (L**3/T)
C--IF RECHARGE OPTION USED IN FLOW MODEL
      IF(FRCH) THEN
        TEXT='RCH'
        CALL READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   RECH,IRCH,FPRT)
      ENDIF
C
C--PULL INFILTRATED VALUES FROM UZFLX ARRAY IF FUZF OPTION USED     !edm
      IF(FUZF) THEN                                                 !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            FINFIL(J,I)=UZFLX(J,I,1)                                !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDIF                                                         !edm
C
C--READ ET FLOW TERM (L**3/T) IF EVT OPTION USED IN FLOW MODEL
      IF(FEVT) THEN
        TEXT='EVT'
        CALL READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   EVTR,IEVT,FPRT)
      ENDIF
C
C--READ ET FLOW TERM (L**3/T) IF SEGMENTED ET USED IN FLOW MODEL
      IF(FETS) THEN
        TEXT='ETS'
        CALL READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   EVTR,IEVT,FPRT)
      ENDIF
C--READ UZ-ET FLOW TERM (L**3/T) IF IETFLG>0 IN UZF PACKAGE         !edm
C--NOTE THAT EITHER THE ET PACKAGE OR THE UZF PACKAGE, BUT NOT      !edm
C--BOTH WILL BE IN USE                                              !edm
C                                                                   !edm
      IF(FUZF.AND.IETFLG) THEN                                      !edm
        TEXT='UZ-ET'                                                !edm
        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,        !edm
     &              UZET,FPRT)                                      !edm
      ENDIF                                                         !edm
C                                                                   !edm
C--Read 'GW-ET' flow term (L**3/T) if IETFLG>0 in UZF packge        !edm
C                                                                   !edm
      IF(FUZF.AND.IETFLG) THEN                                      !edm
        TEXT='GW-ET'                                                !edm
        CALL READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,        !edm
     &              GWET,FPRT)                                      !edm
      ENDIF                                                         !edm

C
C--READ RIVER FLOW TERM (L**3/T) IF RIVER OPTION USED IN FLOW MODEL.
      IF(FRIV) THEN
        TEXT='RIV'
        IQ=4
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ GERENAL HEAD DEPENDENT BOUNDARY FLOW TERM (L**3/T)
C--IF GHB OPTION IS USED IN FLOW MODEL.
      IF(FGHB) THEN
        TEXT='GHB'
        IQ=5
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ STREAMFLOW-ROUTING FLOW TERM (L**3/T)
C--IF STR OPTION IS USED IN FLOW MODEL.
      IF(FSTR) THEN
        TEXT='STR'
        IQ=21
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ RESERVOIR FLOW TERM (L**3/T)
C--IF RES OPTION IS USED IN FLOW MODEL.
      IF(FRES) THEN
        TEXT='RES'
        IQ=22
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ SPECIFIED FLOW AND HEAD BOUNDARY FLOW TERM (L**3/T)
C--IF FHB OPTION IS USED IN FLOW MODEL.
      IF(FFHB) THEN
        TEXT='FHB'
        IQ=23
        CALL READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ MULTI-NODE WELL FLOW TERM (L**3/T)
C--IF MNW OPTION IS USED IN FLOW MODEL.
      IF(FMNW) THEN
        TEXT='MNW'
        IQ=27
        CALL READGS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ DRAIN-RETURN FLOW TERM (L**3/T)
C--IF DRT OPTION IS USED IN FLOW MODEL.
      IF(FDRT) THEN
        TEXT='DRT'
        IQ=28
        CALL READGS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
      ENDIF
C
C--READ SFR FLOW TERMS                                         !# LINE 713 FMI
C--LENGTH (L); AREA (L**2), ALL OTHER TERMS(L**3/T)            !# LINE 714 FMI
C--IF LAK OPTION IS USED IN FLOW MODEL.                        !# LINE 715 FMI
      IF(FSFR) THEN                                            !# LINE 716 FMI
        TEXT='SFR'                                             !# LINE 717 FMI
        IQ=30                                                  !# LINE 718 FMI
        CALL READSFR(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,  !# LINE 719 FMI
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT,NCOMP)           !# LINE 720 FMI
      ENDIF                                                    !# LINE 721 FMI
C                                                              !# LINE 722 FMI
C--READ LAK FLOW TERMS                                         !# LINE 723 FMI
C--VOLUMES (L**3); ALL OTHER TERMS(L**3/T)                     !# LINE 724 FMI
C--IF LAK OPTION IS USED IN FLOW MODEL.                        !# LINE 725 FMI
      IF(FLAK) THEN                                            !# LINE 726 FMI
        TEXT='LAK'                                             !# LINE 727 FMI
        IQ=31                                                  !# LINE 728 FMI
        CALL READLAK(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,  !# LINE 729 FMI
     &   BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT,NCOMP)           !# LINE 730 FMI
      ENDIF                                                    !# LINE 731 FMI
C                                                              !# LINE 732 FMI
C--CHECK IF MAXIMUM NUMBER OF POINT SINKS/SOURCES EXCEEDED.
C--IF SO STOP
      WRITE(IOUT,801) NTSS
  801 FORMAT(//1X,'TOTAL NUMBER OF POINT SOURCES/SINKS PRESENT',
     & ' IN THE FLOW MODEL =',I6)
      IF(NTSS.GT.MXSS) THEN
        WRITE(*,802) MXSS
  802   FORMAT(/1X,'ERROR: MAXIMUM NUMBER OF SINKS/SOURCES ALLOWED',
     &   ' [MXSS] =',I6
     &   /1X,'INCREASE VALUE OF [MXSS] IN [SSM] PACKAGE INPUT FILE')
        CALL USTOP(' ')
      ENDIF
C
C-------SET UP FOR CONSTANT CONC BOUNDARY VARYING AS PER IRCH ARRAY !# LINE 746 FMI
      DO NUM=1,NTSS                                                 !# LINE 747 FMI
        IF(KSSZERO(NUM).EQ.1) THEN                                  !# LINE 748 FMI
          I=SS(2,NUM)                                               !# LINE 749 FMI
          J=SS(3,NUM)                                               !# LINE 750 FMI
          KOLD=SS(1,NUM)                                            !# LINE 751 FMI
          K=IRCH(J,I)                                               !# LINE 752 FMI
          SS(1,NUM)=K                                               !# LINE 753 FMI
          DO INDEX=1,NCOMP                                          !# LINE 754 FMI
C-----------RESET ICBUND TO A POSITIVE NUMBER                       !# LINE 755 FMI
            IF(KOLD.GE.1) THEN                                      !# LINE 756 FMI
              ICBUND(J,I,KOLD,INDEX)=ABS(ICBUND(J,I,KOLD,INDEX))    !# LINE 757 FMI
            ENDIF                                                   !# LINE 758 FMI
C-----------SET ICBUND TO NEGATIVE FOR CONSTANT CONCENTRATION BOUNDARY !# LINE 759 FMI
            IF(SSMC(INDEX,NUM).GE.0) THEN                           !# LINE 760 FMI
              CNEW(J,I,K,INDEX)=SSMC(INDEX,NUM)                     !# LINE 761 FMI
              ICBUND(J,I,K,INDEX)=-ABS(ICBUND(J,I,K,INDEX))         !# LINE 762 FMI
            ENDIF                                                   !# LINE 763 FMI
C                                                                   !# LINE 764 FMI
          ENDDO                                                     !# LINE 765 FMI
        ENDIF                                                       !# LINE 766 FMI
      ENDDO                                                         !# LINE 767 FMI
C                                                                   !# LINE 768 FMI
C--IDENTIFY CELLS IN THE VICINITY OF POINT SINKS OR SOURCES
      DO NUM=1,NTSS
        K=SS(1,NUM)
        I=SS(2,NUM)
        J=SS(3,NUM)
        KM1=MAX(K-1,1)
        KP1=MIN(K+1,NLAY)
        DO KK=KM1,KP1
          IM1=MAX(I-1,1)
          IP1=MIN(I+1,NROW)
          DO II=IM1,IP1
            JM1=MAX(J-1,1)
            JP1=MIN(J+1,NCOL)
            DO JJ=JM1,JP1
              IF(ICBUND(JJ,II,KK,1).EQ.1) ICBUND(JJ,II,KK,1)=1000
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--DIVIDE RECH, EVTR, Q_SS BY AQUIFER VOLUME
C--TO GET FLUXES OF SINKS/SOURCES PER UNIT AQUIFER VOLUME.
C--ALSO DETERMINE STEPSIZE WHICH MEETS STABILITY CRITERION
C--FOR SOLVING THE SINK/SOURCE TERM WITH EXPLICIT SCHEME.
      DTSSM=1.E30
      KSSM=0       
      ISSM=0
      JSSM=0
C
      IF(.NOT.FRCH) GOTO 950
      DO I=1,NROW
        DO J=1,NCOL
          K=IRCH(J,I)
          IF(K.EQ.0) CYCLE
          VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
          IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN
            RECH(J,I)=0.
          ELSE
            RECH(J,I)=RECH(J,I)/VOLAQU
          ENDIF
          IF(RECH(J,I).LE.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE
            TM=PRSITY(J,I,K)/RECH(J,I)
            IF(ABS(TM).LT.DTSSM) THEN
              DTSSM=ABS(TM)
              KSSM=K
              ISSM=I
              JSSM=J
            ENDIF
        ENDDO
      ENDDO
C
C--DIVIDE INFILTRATED VOL BY AQUIFER VOLUME TO GET PER UNIT AQ. VOL !edm
  950 IF(.NOT.FUZF) GOTO 951                                        !edm
      DO I=1,NROW                                                   !edm
        DO J=1,NCOL                                                 !edm
          IF(FINFIL(J,I).EQ.0) CYCLE                                !edm
          VOLAQU=DELR(J)*DELC(I)*DH(J,I,1)                          !edm
          IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN              !edm
            FINFIL(J,I)=0.                                          !edm
          ELSE                                                      !edm
            FINFIL(J,I)=FINFIL(J,I)/VOLAQU                          !edm
          ENDIF                                                     !edm
          IF(FINFIL(J,I).LE.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE      !edm
          TM=PRSITY(J,I,K)/FINFIL(J,I)                              !edm
          IF(ABS(TM).LT.DTSSM) THEN                                 !edm
            DTSSM=ABS(TM)                                           !edm
            KSSM=K                                                  !edm
            ISSM=I                                                  !edm
            JSSM=J                                                  !edm
          ENDIF                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C
  951 IF(.NOT.FEVT .AND. .NOT.FETS) GOTO 955
      DO I=1,NROW
        DO J=1,NCOL
          K=IEVT(J,I)
          IF(K.EQ.0) CYCLE
          VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
          IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN
            EVTR(J,I)=0
          ELSE
            EVTR(J,I)=EVTR(J,I)/VOLAQU
          ENDIF
          IF(EVTR(J,I).EQ.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE
            TM=PRSITY(J,I,K)/EVTR(J,I)
            IF(ABS(TM).LT.DTSSM) THEN
              DTSSM=ABS(TM)
              KSSM=K
              ISSM=I
              JSSM=J
            ENDIF
        ENDDO
      ENDDO
C
C--PERFORM LOOP ONCE FOR UZET AND AGAIN FOR GWET                    !edm
C--(UZET)                                                           !edm
  955 IF(.NOT.IETFLG) GOTO 956                                      !edm
      DO K=1,NLAY                                                   !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)                        !edm
            IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN            !edm
              UZET(J,I,K)=0                                         !edm
            ELSE                                                    !edm
              UZET(J,I,K)=UZET(J,I,K)/VOLAQU                        !edm
            ENDIF                                                   !edm
            IF(UZET(J,I,K).EQ.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE    !edm
            TM=PRSITY(J,I,K)/UZET(J,I,K)                            !edm
            IF(ABS(TM).LT.DTSSM) THEN                               !edm
              DTSSM=ABS(TM)                                         !edm
              KSSM=K                                                !edm
              ISSM=I                                                !edm
              JSSM=J                                                !edm
            ENDIF                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C--(GWET)                                                           !edm
  956 IF(.NOT.IETFLG) GOTO 960                                      !edm
      DO K=1,NLAY                                                   !edm
        DO I=1,NROW                                                 !edm
          DO J=1,NCOL                                               !edm
            VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)                        !edm
            IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN            !edm
              GWET(J,I,K)=0                                         !edm
            ELSE                                                    !edm
              GWET(J,I,K)=GWET(J,I,K)/VOLAQU                        !edm
            ENDIF                                                   !edm
            IF(GWET(J,I,K).EQ.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE    !edm
            TM=PRSITY(J,I,K)/GWET(J,I,K)                            !edm
            IF(ABS(TM).LT.DTSSM) THEN                               !edm
              DTSSM=ABS(TM)                                         !edm
              KSSM=K                                                !edm
              ISSM=I                                                !edm
              JSSM=J                                                !edm
            ENDIF                                                   !edm
          ENDDO                                                     !edm
        ENDDO                                                       !edm
      ENDDO                                                         !edm
C
  960 IF(NTSS.LE.0) GOTO 990
      DO NUM=1,NTSS
        K=SS(1,NUM)
        I=SS(2,NUM)
        J=SS(3,NUM)
        VOLAQU=DELR(J)*DELC(I)*DH(J,I,K)
        IF(ICBUND(J,I,K,1).EQ.0.OR.VOLAQU.LE.0) THEN
          SS(5,NUM)=0
        ELSE
          SS(5,NUM)=SS(5,NUM)/VOLAQU
        ENDIF
        IF(SS(5,NUM).LE.0 .OR. ICBUND(J,I,K,1).EQ.0) CYCLE
        TM=PRSITY(J,I,K)/SS(5,NUM)
        IF(ABS(TM).LT.DTSSM) THEN
          DTSSM=ABS(TM)
          KSSM=K
          ISSM=I
          JSSM=J
        ENDIF
      ENDDO
C
C--PRINT INFORMATION ON DTSSM
  990 WRITE(IOUT,1000) DTSSM,KSSM,ISSM,JSSM
 1000 FORMAT(/1X,'MAXIMUM STEPSIZE WHICH MEETS STABILITY CRITERION',
     & ' OF THE SINK & SOURCE TERM'/1X,'=',G11.4,
     & '(WHEN MIN. R.F.=1)  AT K=',I4,', I=',I4,
     & ', J=',I4)
C
C--SYNCHRONIZE ICBUND CONDITIONS OF ALL SPECIES
      IF(NCOMP.EQ.1) GOTO 1999
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            DO INDEX=2,NCOMP
              IF(ICBUND(J,I,K,INDEX).GE.0) THEN
                ICBUND(J,I,K,INDEX)=IABS(ICBUND(J,I,K,1))
              ELSEIF(ICBUND(J,I,K,1).EQ.0) THEN
                ICBUND(J,I,K,INDEX)=0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
 1999 RETURN
      END
C
C
      SUBROUTINE READHQ(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,FPRT)
C *****************************************************************
C THIS SUBROUTINE READS HEADS AND VOLUMETRIC FLUXES ACROSS CELL
C INTERFACES FROM AN UNFORMATTED FILE SAVED BY THE FLOW MODEL.
C *****************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
C
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,IPRTFM,K,I,J,
     &          KKSTP,KKPER,NC,NR,NL
      REAL      BUFF
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL
      ENDIF
C
C--CHECK INTERFACE
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        CALL USTOP(' ')
      ENDIF
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH CELL IN THE GRID
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) (((BUFF(J,I,K),J=1,NCOL),I=1,NROW),K=1,NLAY)
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) (((BUFF(J,I,K),J=1,NCOL),I=1,NROW),K=1,NLAY)
      ENDIF
C
C--PRINT OUT INPUT FOR CHECKING IF REQUESTED
      IF(FPRT.NE.'Y'.AND.FPRT.NE.'y') RETURN
      IPRTFM=1
      DO K=1,NLAY
        WRITE(IOUT,50) K
        CALL RPRINT(BUFF(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,0,IPRTFM,IOUT)
      ENDDO
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE.'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
   10 FORMAT(/44X,'LAYER LOCATION OF ',A16,'FLOW TERM'
     & /44X,43('-'))
   50 FORMAT(/61X,'LAYER ',I3)
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE READDS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,LOCLAY,FPRT)
C *****************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF DIFFUSIVE
C SINK/SOURCE TERMS (RECHARGE AND EVAPOTRANSPIRATION) FROM AN
C UNFORMATTED FILE SAVED BY THE FLOW MODEL.
C *****************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
C
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,IPRTFM,I,J,
     &          KKSTP,KKPER,NC,NR,NL,LOCLAY
      REAL      BUFF
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW),LOCLAY(NCOL,NROW)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL
      ENDIF
C
C--CHECK INTERFACE
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        CALL USTOP(' ')
      ENDIF
C
C--READ LAYER LOCATION IF FLOW TERM IS RECHARGE OR E.T.
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) ((LOCLAY(J,I),J=1,NCOL),I=1,NROW)
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) ((LOCLAY(J,I),J=1,NCOL),I=1,NROW)
      ENDIF
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH CELL IN THE GRID
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) ((BUFF(J,I),J=1,NCOL),I=1,NROW)
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) ((BUFF(J,I),J=1,NCOL),I=1,NROW)
      ENDIF
C
C--PRINT OUT INPUT FOR CHECKING IF REQUESTED
      IF(FPRT.NE.'Y'.AND.FPRT.NE.'y') RETURN
      IPRTFM=1
      CALL RPRINT(BUFF(1,1),TEXT,
     & 0,KSTP,KPER,NCOL,NROW,0,IPRTFM,IOUT)
      IPRTFM=3
      WRITE(IOUT,10)
      CALL IPRINT(LOCLAY(1,1),TEXT,0,KSTP,KPER,NCOL,NROW,
     & 0,IPRTFM,IOUT)
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE.'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
   10 FORMAT(/60X,'LAYER INDEX')
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE READPS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
C *********************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF POINT SINK/SOURCE
C FLOW TERMS FROM AN UNFORMATTED FILE SAVED BY THE FLOW MODEL.
C *********************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
C
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,NUM,N,MXSS,NTSS,NSS,ICBUND,IQ,ID,
     &          KKK,III,JJJ,ITEMP
      REAL      BUFF,SS,QSS,QSTEMP
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),SS(8,MXSS)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NUM
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NUM
      ENDIF
C
C--CHECK INTERFACE
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        CALL USTOP(' ')
      ENDIF
C
C--RETURN IF NUM=0
      IF(NUM.LE.0) RETURN
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH POINT SINK OR SOURCE
      DO N=1,NUM
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) K,I,J,QSTEMP
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) K,I,J,QSTEMP
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &              WRITE(IOUT,50) K,I,J,QSTEMP        
C
C--IF ALREADY DEFINED AS A SOURCE OF USER-SPECIFIED CONCENTRATION,
C--STORE FLOW RATE QSTEMP                              
        DO ITEMP=1,NSS
          KKK=SS(1,ITEMP)
          III=SS(2,ITEMP)
          JJJ=SS(3,ITEMP)
          QSS=SS(5,ITEMP)
          ID =SS(6,ITEMP)       
          IF(KKK.NE.K.OR.III.NE.I.OR.JJJ.NE.J.OR.ID.NE.IQ) CYCLE
          IF(ABS(QSS).GT.0) CYCLE                   
          SS(5,ITEMP)=QSTEMP
          SS(7,ITEMP)=0
          IF(LABEL.EQ.'WEL             ') SS(8,ITEMP)=N        !# LINE 1123 FMI
C
C--MARK CELLS NEAR THE SINK/SOURCE                   
          IF(QSTEMP.LT.0 .AND. ICBUND(J,I,K).GT.0) THEN                
            ICBUND(J,I,K)=1000+IQ
          ELSEIF(ICBUND(J,I,K).GT.0 ) THEN
            ICBUND(J,I,K)=1020+IQ
          ENDIF
          GOTO 100
        ENDDO
C       
C--OTHERWISE, ADD TO THE SS ARRAY
        NTSS=NTSS+1
        IF(NTSS.GT.MXSS) CYCLE
        SS(1,NTSS)=K
        SS(2,NTSS)=I
        SS(3,NTSS)=J
        SS(4,NTSS)=0.
        SS(5,NTSS)=QSTEMP
        SS(6,NTSS)=IQ
        SS(7,NTSS)=0.
        IF(LABEL.EQ.'WEL             ') SS(8,NTSS)=N           !# LINE 1144 FMI
        IF(QSTEMP.LT.0 .AND. ICBUND(J,I,K).GT.0) THEN                   
          ICBUND(J,I,K)=1000+IQ
        ELSEIF(ICBUND(J,I,K).GT.0) THEN
          ICBUND(J,I,K)=1020+IQ
        ENDIF
  100   CONTINUE
      ENDDO  
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
   50 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7)
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE READGS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT)
C *********************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF SINK/SOURCE GROUPS
C THAT ARE CONNECTED FROM THE FLOW-TRANSPORT LINK FILE
C *********************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT
C
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,NUM,N,MXSS,NTSS,NSS,ICBUND,IQ,ID,
     &          KKK,III,JJJ,ITEMP,IGROUP
      REAL      BUFF,SS,QSS,QSTEMP,QSW
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),SS(8,MXSS)
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NUM
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NUM
      ENDIF
C
C--CHECK INTERFACE
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        CALL USTOP(' ')
      ENDIF
C
C--RETURN IF NUM=0
      IF(NUM.LE.0) RETURN
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH POINT SINK OR SOURCE
      DO N=1,NUM
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) K,I,J,QSTEMP,IGROUP,QSW
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) K,I,J,QSTEMP,IGROUP,QSW
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,50) K,I,J,QSTEMP,IGROUP,QSW
C
C--IF ALREADY DEFINED AS A SOURCE OF USER-SPECIFIED CONCENTRATION,
C--STORE FLOW RATE QSTEMP                               
        DO ITEMP=1,NSS
          KKK=SS(1,ITEMP)
          III=SS(2,ITEMP)
          JJJ=SS(3,ITEMP)
          QSS=SS(5,ITEMP)
          ID =SS(6,ITEMP)       
          IF(KKK.NE.K.OR.III.NE.I.OR.JJJ.NE.J.OR.ID.NE.IQ) CYCLE
          IF(ABS(QSS).GT.0) CYCLE                      
          SS(5,ITEMP)=QSTEMP
          SS(7,ITEMP)=IGROUP
          IF(LABEL.EQ.'MNW             ') SS(8,ITEMP)=N        !# LINE 1241 FMI
C
C--MAKR CELLS NEAR THE SINK/SOURCE                   
          IF(QSTEMP.LT.0 .AND. ICBUND(J,I,K).GT.0 ) THEN
            ICBUND(J,I,K)=1000+IQ
          ELSEIF(ICBUND(J,I,K).GT.0) THEN              
            ICBUND(J,I,K)=1020+IQ   
          ENDIF
          GOTO 100
        ENDDO
C       
C--OTHERWISE, ADD TO THE SS ARRAY
        NTSS=NTSS+1
        IF(NTSS.GT.MXSS) CYCLE
        SS(1,NTSS)=K
        SS(2,NTSS)=I
        SS(3,NTSS)=J
        SS(4,NTSS)=0.
        SS(5,NTSS)=QSTEMP
        SS(6,NTSS)=IQ
        SS(7,NTSS)=IGROUP
        IF(LABEL.EQ.'MNW             ') SS(8,NTSS)=N           !# LINE 1262 FMI
        IF(QSTEMP.LT.0 .AND. ICBUND(J,I,K).GT.0) THEN                 
          ICBUND(J,I,K)=1000+IQ
        ELSEIF(ICBUND(J,I,K).GT.0) THEN                       
          ICBUND(J,I,K)=1020+IQ
        ENDIF
  100   CONTINUE
      ENDDO  
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
   50 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7,
     & ' SS CODE',I5,5X,'EXTERNAL FLOW',G15.7)  
C
C--RETURN
      RETURN
      END
C
C
      FUNCTION CREWET(NCOL,NROW,NLAY,CNEW,ICBUND,XBC,YBC,ZBC,
     & JJ,II,KK)
C *****************************************************************
C THIS FUNCTION OBTAINS CONCENTRATION AT A REWET CELL (JJ,II,KK)
C FROM CONCENTRATIONS AT NEIGHBORING NODES WITH INVERSE DISTANCE
C (POWER 2) WEIGHTING .
C *****************************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,ICBUND,JJ,II,KK
      REAL      XBC,YBC,ZBC,CTMP,CNEW,CREWET,D2,D2SUM
      DIMENSION ICBUND(NCOL,NROW,NLAY),CNEW(NCOL,NROW,NLAY),
     &          XBC(NCOL),YBC(NROW),ZBC(NCOL,NROW,NLAY)
C
C--INITIALIZE
      D2SUM=0
      CTMP=0
C
C--ACCUMULATE CONCENTRATIONS AT NEIGHBORING NODELS
C--IN THE LAYER DIRECTION
      IF(NLAY.EQ.1) GOTO 10
      IF(KK-1.GT.0) THEN
        IF(ICBUND(JJ,II,KK-1).NE.0) THEN
          D2=(ZBC(JJ,II,KK)-ZBC(JJ,II,KK-1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ,II,KK-1)/D2
          ELSE
            CTMP=CNEW(JJ,II,KK-1)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
      IF(KK+1.LE.NLAY) THEN
        IF(ICBUND(JJ,II,KK+1).NE.0) THEN
          D2=(ZBC(JJ,II,KK)-ZBC(JJ,II,KK+1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ,II,KK+1)/D2
          ELSE
            CTMP=CNEW(JJ,II,KK+1)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
C
C--IN THE ROW DIRECTION
   10 IF(NROW.EQ.1) GOTO 20
      IF(II-1.GT.0) THEN
        IF(ICBUND(JJ,II-1,KK).NE.0) THEN
          D2=(YBC(II)-YBC(II-1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ,II-1,KK)/D2
          ELSE
            CTMP=CNEW(JJ,II-1,KK)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
      IF(II+1.LE.NROW) THEN
        IF(ICBUND(JJ,II+1,KK).NE.0) THEN
          D2=(YBC(II)-YBC(II+1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ,II+1,KK)/D2
          ELSE
            CTMP=CNEW(JJ,II+1,KK)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
C
C--IN THE COLUMN DIRECTION
   20 IF(NCOL.EQ.1) GOTO 30
      IF(JJ-1.GT.0) THEN
        IF(ICBUND(JJ-1,II,KK).NE.0) THEN
          D2=(XBC(JJ)-XBC(JJ-1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ-1,II,KK)/D2
          ELSE
            CTMP=CNEW(JJ-1,II,KK)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
      IF(JJ+1.LE.NCOL) THEN
        IF(ICBUND(JJ+1,II,KK).NE.0) THEN
          D2=(XBC(JJ)-XBC(JJ+1))**2
          IF(D2.NE.0) THEN
            D2SUM=D2SUM+1./D2
            CTMP=CTMP+CNEW(JJ+1,II,KK)/D2
          ELSE
            CTMP=CNEW(JJ+1,II,KK)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
C
C--OBTAIN WEIGHTED CONCENTRATION
   30 IF(D2SUM.EQ.0) THEN
        ICBUND(JJ,II,KK)=0
      ELSE
        CTMP=CTMP/D2SUM
      ENDIF
C
C--ASSIGN WEIGHTED CONCENTRATION TO CREWET
  100 CREWET=CTMP
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE READSFR(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT,NCOMP)
C *********************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF SFR PAKCAGE
C *********************************************************************
C last modified: 08-10-2012
C
      USE MT3DMS_MODULE, ONLY: IFTLFMT                            !# New
      USE SFRVARS
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,NUM,N,MXSS,NTSS,NSS,ICBUND,IQ,ID, !IFTLFMT,
     &          KKK,III,JJJ,ITEMP,IGROUP,NCOMP,ICNT,IS,IR,NN
      REAL      BUFF,SS,QSS,QSTEMP,QSW
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),SS(8,MXSS)
C      COMMON /FTL/IFTLFMT
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NSTRM,NINTOT,MXSGMT,MXRCH
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NSTRM,NINTOT,MXSGMT,
     1    MXRCH
      ENDIF
C
C--DEALLOCATE AND ALLOCATE ARRAYS FOR STORING FLOW TERMS
cvsb      IF(ALLOCATED(ISFL)) DEALLOCATE(ISFL)
cvsb      IF(ALLOCATED(ISFR)) DEALLOCATE(ISFR)
cvsb      IF(ALLOCATED(ISFC)) DEALLOCATE(ISFC)
cvsb      IF(ALLOCATED(ISEG)) DEALLOCATE(ISEG)
cvsb      IF(ALLOCATED(IRCH)) DEALLOCATE(IRCH)
cvsb      IF(ALLOCATED(SFLEN)) DEALLOCATE(SFLEN)
cvsb      IF(ALLOCATED(SFNAREA)) DEALLOCATE(SFNAREA)
C      IF(ALLOCATED(SFOAREA)) DEALLOCATE(SFOAREA)
cvsb      IF(ALLOCATED(QPRECSF)) DEALLOCATE(QPRECSF)
cvsb      IF(ALLOCATED(QRUNOFSF)) DEALLOCATE(QRUNOFSF)
C      IF(ALLOCATED(QPRECSFO)) DEALLOCATE(QPRECSFO)
C      IF(ALLOCATED(QRUNOFSFO)) DEALLOCATE(QRUNOFSFO)
cvsb      IF(ALLOCATED(QSFGW)) DEALLOCATE(QSFGW)
cvsb      IF(ALLOCATED(QOUTSF)) DEALLOCATE(QOUTSF)
C      IF(ALLOCATED(QOUTSFO)) DEALLOCATE(QOUTSFO)
cvsb      IF(ALLOCATED(QETSF)) DEALLOCATE(QETSF)
cvsb      IF(ALLOCATED(NIN)) DEALLOCATE(NIN)
cvsb      IF(ALLOCATED(INFLWNOD)) DEALLOCATE(INFLWNOD)
C.....NINTOT SIZED ARRAYS
cvsb      IF(ALLOCATED(QINSF)) DEALLOCATE(QINSF)
C      IF(ALLOCATED(QINSFO)) DEALLOCATE(QINSFO)
cvsb      IF(ALLOCATED(INSEG)) DEALLOCATE(INSEG)
cvsb      IF(ALLOCATED(INRCH)) DEALLOCATE(INRCH)
cvsb      IF(ALLOCATED(IDSPFLG)) DEALLOCATE(IDSPFLG)
cvsb      IF(ALLOCATED(IDXNIN)) DEALLOCATE(IDXNIN)
C.....INDEXING TO GET ISTRM FROM SEG AND RCH NUMBERS
cvsb      IF(ALLOCATED(ISTRM)) DEALLOCATE(ISTRM)
C
      ALLOCATE(ISFL(NSTRM),ISFR(NSTRM),ISFC(NSTRM),ISEG(NSTRM),
     1  IRCH(NSTRM),SFLEN(NSTRM),SFNAREA(NSTRM), !SFOAREA(NSTRM),
     1  QPRECSF(NSTRM),QRUNOFSF(NSTRM),
!     1  QPRECSFO(NSTRM),QRUNOFSFO(NSTRM),
     1  QSFGW(NSTRM),QOUTSF(NSTRM),QETSF(NSTRM),NIN(NSTRM),
     1  INFLWNOD(NSTRM),IEXIT(NSTRM)) !,QOUTSFO(NSTRM))
      ALLOCATE(QINSF(NINTOT),INSEG(NINTOT),INRCH(NINTOT),
     1  IDSPFLG(NINTOT),IDXNIN(NSTRM+1)) !,QINSFO(NINTOT))
      ALLOCATE(ISTRM(MXRCH,MXSGMT))
      ISTRM=0
      IDXNIN=0
      IEXIT=1
C
      IF(KKPER.EQ.1.AND.KKSTP.EQ.1) THEN
        ALLOCATE(SFOAREA(NSTRM),QPRECSFO(NSTRM),QRUNOFSFO(NSTRM),
     1    QOUTSFO(NSTRM))
        ALLOCATE(QINSFO(NINTOT))
      ENDIF
C
C--CHECK INTERFACE
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        WRITE(IOUT,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        WRITE(IOUT,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        WRITE(IOUT,2) NC,NR,NL
        CALL USTOP(' ')
      ELSEIF(NSTRM.NE.NSFINIT) THEN
        WRITE(*,5) NSTRM
        WRITE(IOUT,5) NSTRM
        CALL USTOP(' ')
      ENDIF
C
C--RETURN IF NSTRM=0
      IF(NSTRM.EQ.0) RETURN
C
      IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,10) 
10    FORMAT(1X,' GW-L GW-R GW-C SEG# RCH# ',
     &  '   LENGTH       AREA         FLOW-GW      FLOWOUT      RUNOFF',
     &  '        PRECIP       EVAP     IN-NODES',/,
     &  1X,5(1X,4('-')),7(1X,12('-')),1X,8('-'))
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH LAKE INFLOWS AND OUTFLOWS
      ICNT=0
      DO N=1,NSTRM
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) ISFL(N),ISFR(N),ISFC(N),ISEG(N),
     1      IRCH(N),SFLEN(N),SFNAREA(N),QSFGW(N),QOUTSF(N),QRUNOFSF(N),
     1      QPRECSF(N),QETSF(N),NIN(N)
          IF(NIN(N).GT.0) IDXNIN(N)=ICNT+1
          DO I=1,NIN(N)
            ICNT=ICNT+1
            READ(INUF) INSEG(ICNT),INRCH(ICNT),QINSF(ICNT),
     1        IDSPFLG(ICNT)
          ENDDO
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) ISFL(N),ISFR(N),ISFC(N),ISEG(N),
     1      IRCH(N),SFLEN(N),SFNAREA(N),QSFGW(N),QOUTSF(N),QRUNOFSF(N),
     1      QPRECSF(N),QETSF(N),NIN(N)
          IF(NIN(N).GT.0) IDXNIN(N)=ICNT+1
          DO I=1,NIN(N)
            ICNT=ICNT+1
            READ(INUF,*) INSEG(ICNT),INRCH(ICNT),QINSF(ICNT),
     1        IDSPFLG(ICNT)
          ENDDO
        ENDIF
C.......STORE N
        ISTRM(IRCH(N),ISEG(N))=N
C.......WRITE TO OUTPUT FILE
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') THEN
          WRITE(IOUT,51) ISFL(N),ISFR(N),ISFC(N),ISEG(N),
     1      IRCH(N),SFLEN(N),SFNAREA(N),QSFGW(N),QOUTSF(N),QRUNOFSF(N),
     1      QPRECSF(N),QETSF(N),NIN(N)
          DO I=1,NIN(N)
            WRITE(IOUT,52) INSEG(ICNT),INRCH(ICNT),QINSF(ICNT),
     1        IDSPFLG(ICNT)
          ENDDO
        ENDIF
      ENDDO  
      IDXNIN(NSTRM+1)=ICNT+1
C
C--CHECK IF NINTOT MATCHES THE SUM OF NIN(1:NSTRM)
      IF(NINTOT.NE.ICNT) THEN
        WRITE(*,*) 'MISMATCH IN FTL FILE NINTOT<>ICNT'
        WRITE(IOUT,*) 'MISMATCH IN FTL FILE NINTOT<>ICNT'
        CALL USTOP(' ')
      ENDIF
C
C--INITIALIZE OLD=NEW FOR THE FIRST TIME
      IF(KKPER.EQ.1.AND.KKSTP.EQ.1) THEN
        QPRECSFO=QPRECSF
        QRUNOFSFO=QRUNOFSF
        QOUTSFO=QOUTSF
        QINSFO=QINSF
        SFOAREA=SFNAREA
      ENDIF
C
C--IDENTIFY NODES THAT DO NOT FEED WATER TO DOWNSTREAM NODES
      ICNT=0
      DO N=1,NSTRM
        DO NC=1,NIN(N)
          ICNT=ICNT+1
          IS=INSEG(ICNT)
          IR=INRCH(ICNT)
          IF(IS.GT.0.AND.IR.GT.0) THEN
C...........INFLOW FROM STREAM
            NN=ISTRM(IR,IS)
            IEXIT(NN)=0
          ENDIF
        ENDDO
      ENDDO
C
C--IDENTIFY CONSTANT CONCENTRATION NODES AND SET IBNDSF ARRAY
      IBNDSF=1
      DO I=1,NSSSF
        IS=ISEGBC(I)
        IR=IRCHBC(I)
        N=ISTRM(IR,IS)
        IF(ISFBCTYP(I).EQ.3) IBNDSF(N)=-1
      ENDDO
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
    5 FORMAT(1X,'ERROR: INVALID NUMBER OF STREAM REACHES',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STREAM REACHES IN FLOW-TRANSPORT LINK FILE =',I5)
   50 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7,
     & ' SS CODE',I5,5X,'EXTERNAL FLOW',G15.7)  
51    FORMAT(1X,5I5,7(1X,G12.5),1X,I5)
52    FORMAT('SEG,RCH,QIN,IDISP: ',1X,2I5,1(1X,G12.5),I5)
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE READLAK(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     & BUFF,IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT,NCOMP)
C *********************************************************************
C THIS SUBROUTINE READS LOCATIONS AND FLOW RATES OF LAK PAKCAGE
C *********************************************************************
C last modified: 08-10-2012
C
      USE LAKVARS
      IMPLICIT  NONE
      INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER,
     &          NC,NR,NL,NUM,N,MXSS,NTSS,NSS,ICBUND,IQ,ID,IFTLFMT,
     &          KKK,III,JJJ,ITEMP,IGROUP,NCOMP
      REAL      BUFF,SS,QSS,QSTEMP,QSW
      CHARACTER TEXT*16,FPRT*1,LABEL*16
      DIMENSION BUFF(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),SS(8,MXSS)
      COMMON /FTL/IFTLFMT
C
C--WRITE IDENTIFYING INFORMATION
      WRITE(IOUT,1) TEXT,KSTP,KPER,INUF
C
C--READ IDENTIFYING RECORD
      IF(IFTLFMT.EQ.0) THEN
        READ(INUF) KKPER,KKSTP,NC,NR,NL,LABEL,NLAKES,LKNODE,NSFRLAK
      ELSEIF(IFTLFMT.EQ.1) THEN
        READ(INUF,*) KKPER,KKSTP,NC,NR,NL,LABEL,NLAKES,LKNODE,NSFRLAK
      ENDIF
C
C--DEALLOCATE AND ALLOCATE ARRAYS FOR STORING FLOW TERMS
      CALL MEMDEALLOCATE6()
      ALLOCATE(LAKL(LKNODE),LAKR(LKNODE),LAKC(LKNODE),LAKNUMGW(LKNODE))
      ALLOCATE(LAKRCH(NSFRLAK),LAKSEG(NSFRLAK),LAKNUMSFR(LKNODE))
      ALLOCATE(QPRECLAK(NLAKES),QRUNOFLAK(NLAKES),QWDRLLAK(NLAKES))
      ALLOCATE(QLAKGW(LKNODE),QLAKSFR(NSFRLAK),QETLAK(NLAKES))
      ALLOCATE(VOLNLAK(NLAKES),VOLOLAK(NLAKES),DELVOLLAK(NLAKES))
C
C--CHECK INTERFACE
      IF(LABEL.NE.TEXT) THEN
        WRITE(*,4) TEXT,LABEL
        WRITE(IOUT,4) TEXT,LABEL
        CALL USTOP(' ')
      ELSEIF(KKPER.NE.KPER.OR.KKSTP.NE.KSTP) THEN
        WRITE(*,3) KKPER,KKSTP
        WRITE(IOUT,3) KKPER,KKSTP
        CALL USTOP(' ')
      ELSEIF(NC.NE.NCOL.OR.NR.NE.NROW.OR.NL.NE.NLAY) THEN
        WRITE(*,2) NC,NR,NL
        WRITE(IOUT,2) NC,NR,NL
        CALL USTOP(' ')
      ELSEIF(NLAKES.NE.NLKINIT) THEN
        WRITE(*,5) NLAKES
        WRITE(IOUT,5) NLAKES
        CALL USTOP(' ')
      ENDIF
C
C--RETURN IF NLAKES,LKNODE,NSFRLAK=0
      IF(NLAKES.EQ.0 .AND. LKNODE.EQ.0 .AND. NSFRLAK.EQ.0) RETURN
C
      IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,10) 
10    FORMAT(1X,'LAKE#    VOLUME_OLD     DEL_V        PRECIP ',
     &  '          ET        RUNOFF    WITHDRAWAL',/,
     &  1X,5('-'),6(1X,12('-')))
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH LAKE INFLOWS AND OUTFLOWS
      DO N=1,NLAKES
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) I,VOLOLAK(N),DELVOLLAK(N),QPRECLAK(N),QETLAK(N),
     1    QRUNOFLAK(N),QWDRLLAK(N)
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) I,VOLOLAK(N),DELVOLLAK(N),QPRECLAK(N),QETLAK(N),
     1    QRUNOFLAK(N),QWDRLLAK(N)
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,51) I,VOLOLAK(N),DELVOLLAK(N),QPRECLAK(N),QETLAK(N),
     1    QRUNOFLAK(N),QWDRLLAK(N)
      ENDDO  
C
C
      IF(LKNODE.GT.0) THEN
      IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,12) 
12    FORMAT(/1X,'LAKE# LAYER  ROW COLUMN GW-FLOW',/,
     &  4(1X,5('-')),1(1X,12('-')))
      ENDIF
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH LAKE-GW CONNECTION
      DO N=1,LKNODE
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) LAKNUMGW(N),LAKL(N),LAKR(N),LAKC(N),QLAKGW(N)
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) LAKNUMGW(N),LAKL(N),LAKR(N),LAKC(N),QLAKGW(N)
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,52) LAKNUMGW(N),LAKL(N),LAKR(N),LAKC(N),QLAKGW(N)
      ENDDO  
C
C
      IF(NSFRLAK.GT.0) THEN
      IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,14) 
14    FORMAT(/1X,'LAKE# SEGMENT REACH SFR-FLOW',/,
     &  3(1X,5('-')),1(1X,12('-')))
      ENDIF
C
C--READ AN UNFORMATTED RECORD CONTAINING VALUES FOR
C--EACH LAKE-SFR CONNECTION
      DO N=1,NSFRLAK
        IF(IFTLFMT.EQ.0) THEN
          READ(INUF) LAKNUMSFR(N),LAKSEG(N),LAKRCH(N),QLAKSFR(N)
        ELSEIF(IFTLFMT.EQ.1) THEN
          READ(INUF,*) LAKNUMSFR(N),LAKSEG(N),LAKRCH(N),QLAKSFR(N)
        ENDIF
        IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &   WRITE(IOUT,53) LAKNUMSFR(N),LAKSEG(N),LAKRCH(N),QLAKSFR(N)
      ENDDO  
C
C--PRINT FORMATS
    1 FORMAT(/20X,'"',A16,'" FLOW TERMS FOR TIME STEP',I3,
     & ', STRESS PERIOD',I3,' READ UNFORMATTED ON UNIT',I3
     & /20X,92('-'))
    2 FORMAT(1X,'ERROR: INVALID NUMBER OF COLUMNS, ROWS OR LAYERS',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF COLUMNS IN FLOW-TRANSPORT LINK FILE =',I5
     & /1X,'NUMBER OF ROWS IN FLOW-TRANSPORT LINK FILE    =',I5,
     & /1X,'NUMBER OF LAYERS FLOW-TRANSPORT LINK FILE     =',I5)
    3 FORMAT(/1X,'ERROR: INVALID NUMBER OF STRESS PERIOD OR TIME STEP',
     &  ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF STRESS PERIOD IN FLOW-TRANSPORT LINK FILE =',I3,
     & /1X,'NUMBER OF TIME STEP IN FLOW-TRANSPORT LINK FILE     =',I3)
    4 FORMAT(/1X,'ERROR READING FLOW-TRANSPORT LINK FILE'/1X,
     & 'NAME OF THE FLOW TERM REQUIRED =',A16/1X,
     & 'NAME OF THE FLOW TERM SAVED IN FLOW-TRANSPORT LINK FILE =',A16)
    5 FORMAT(1X,'ERROR: INVALID NUMBER OF LAKES',
     & ' IN FLOW-TRANSPORT LINK FILE.'
     & /1X,'NUMBER OF LAKES IN FLOW-TRANSPORT LINK FILE =',I5)
   50 FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7,
     & ' SS CODE',I5,5X,'EXTERNAL FLOW',G15.7)  
51    FORMAT(1X,I5,6(1X,G12.5))
52    FORMAT(1X,4I5,1(1X,G12.5))
53    FORMAT(1X,3I5,1(1X,G12.5))
C
C--RETURN
      RETURN
      END
