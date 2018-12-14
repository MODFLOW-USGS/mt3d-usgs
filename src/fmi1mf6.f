      MODULE FMI1MF6
        USE MT3DMS_MODULE, ONLY : IFTL
        IMPLICIT NONE
        INTEGER,      SAVE,                   POINTER :: ILIST
        INTEGER,      SAVE,                   POINTER :: FILNUM
        INTEGER,      SAVE,                   POINTER :: IUGRB
        INTEGER,      SAVE,                   POINTER :: IUBUD
        INTEGER,      SAVE,                   POINTER :: IUHDS
        INTEGER,      SAVE, DIMENSION(:),     POINTER :: IUFT6
        CHARACTER*40, SAVE, DIMENSION(:),     POINTER :: FT6FILNAM
        CHARACTER*6,  SAVE, DIMENsION(:,:),   POINTER :: FT6TYP
        INTEGER,      SAVE,                   POINTER :: NLAY
        INTEGER,      SAVE,                   POINTER :: NROW
        INTEGER,      SAVE,                   POINTER :: NCOL
        INTEGER                                       :: ISS_SY
        INTEGER                                       :: ISS_SS
        INTEGER                                       :: IDATA_SPDIS
        INTEGER                                       :: NBUD
        DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: HEAD,BOTM
        INTEGER,            DIMENSION(:), ALLOCATABLE :: IA
        INTEGER,            DIMENSION(:), ALLOCATABLE :: JA
C        
      CONTAINS
C    
        LOGICAL FUNCTION IS_GRB(IU)
          CHARACTER(LEN=50) HDRTXT
          INTEGER IU
          HDRTXT=''
          IS_GRB = .FALSE.
          READ(IU, ERR=500) HDRTXT
          REWIND(IU)
          IF(TRIM(ADJUSTL(HDRTXT(1:49))).EQ.'GRID DIS') IS_GRB=.TRUE.
          RETURN
  500     WRITE(ILIST, 600) IU
  600     FORMAT(1X,'UNIT NUMBER ',I4,' COULD NOT BE READ.')
        END FUNCTION
C
        LOGICAL FUNCTION IS_BUD(IU)
          INTEGER IU
          INTEGER KPER,KSTP
          CHARACTER(LEN=16) TEXT
          TEXT = ''
          IS_BUD = .FALSE.
          READ(IU, ERR=500) KPER,KSTP,TEXT
          TEXT=TRIM(ADJUSTL(TEXT))
          REWIND(IU)
          IF(TEXT.EQ.'FLOW-JA-FACE'.OR.
     &       TEXT.EQ.'STO-SS'.OR.
     &       TEXT.EQ.'STO-SY') IS_BUD = .TRUE.
          RETURN
  500     WRITE(ILIST, 600) IU
  600     FORMAT(1X,'UNIT NUMBER ',I4,' COULD NOT BE READ.')
        END FUNCTION
C
        LOGICAL FUNCTION IS_HDS(IU)
          INTEGER IU
          INTEGER KPER,KSTP
          DOUBLE PRECISION PERTIM,TOTIM
          CHARACTER(LEN=16) TEXT
          TEXT=''
          IS_HDS = .FALSE.
          READ(IU, ERR=500) KSTP,KPER,PERTIM,TOTIM,TEXT
          REWIND(IU)
          IF(TRIM(ADJUSTL(TEXT)).EQ.'HEAD') IS_HDS = .TRUE.
          RETURN
  500     WRITE(ILIST, 600) IU
  600     FORMAT(1X,'UNIT NUMBER ',I4,' COULD NOT BE READ.')
        END FUNCTION
C
        SUBROUTINE MEMDEALLOCATE()
          IF(ASSOCIATED(FILNUM))      DEALLOCATE(FILNUM)
          IF(ASSOCIATED(IUGRB))       DEALLOCATE(IUGRB)
          IF(ASSOCIATED(IUBUD))       DEALLOCATE(IUBUD)
          IF(ASSOCIATED(IUHDS))       DEALLOCATE(IUHDS)
          IF(ASSOCIATED(IUFT6))       DEALLOCATE(IUFT6)
          IF(ASSOCIATED(FT6FILNAM))   DEALLOCATE(FT6FILNAM)
          IF(ASSOCIATED(FT6TYP))      DEALLOCATE(FT6TYP)
          IF(ALLOCATED(BOTM))         DEALLOCATE(BOTM)
        END SUBROUTINE MEMDEALLOCATE
C
      SUBROUTINE FMI1MF6NM(FNAME,IU,IOUT,FILSTAT,FILACT,FMTARG,IFLEN)
      IMPLICIT     NONE
      INTEGER      IOUT,IFLEN,IU,INFT1,INFT2,INFT3
      CHARACTER*7  FILSTAT
      CHARACTER*20 FMTARG, ACCARG, FILACT
      CHARACTER*40 FNAME
C
C     SAVE ILIST
      ALLOCATE(ILIST)
      ILIST = IOUT
      INFT1 = 21
      INFT2 = 22
      INFT3 = 23
C
      IF (.NOT. ASSOCIATED(IUFT6)) ALLOCATE(IUFT6(3))
C
C---------ALLOCATE THE FOLLOWING, THOUGHT IT WON'T BE SORTED OUT 
C         UNTIL MF6FMIAR()
      IF (.NOT. ASSOCIATED(FT6TYP)) THEN
        ALLOCATE(FT6TYP(3,2))
        FT6TYP(1,1)='FT6GRD'
        FT6TYP(2,1)='FT6BUD'
        FT6TYP(3,1)='FT6HDS'
      ENDIF
C
      IF (.NOT. ASSOCIATED(FILNUM)) THEN
        ALLOCATE(IUGRB)
        ALLOCATE(IUBUD)
        ALLOCATE(IUHDS)
        ALLOCATE(FILNUM)
        ALLOCATE(FT6FILNAM(3))
        ALLOCATE(NLAY)
        ALLOCATE(NROW)
        ALLOCATE(NCOL)
        FILNUM=0
        IUGRB=0
        IUBUD=0
        IUHDS=0
        NLAY=0
        NROW=0
        NCOL=0
      ENDIF
C
      FILNUM = FILNUM + 1
C
C-------SORT OUT WHICH FILE IS GRB, BUD, AND HDS (DON'T RELY ON FILE EXTENTION, SINCE THESE CAN BE ARBITRARY)
      IF(IU.EQ.0) THEN
        IF (FILNUM.EQ.1) IU=INFT1
        IF (FILNUM.EQ.2) IU=INFT2
        IF (FILNUM.EQ.3) IU=INFT3
      ENDIF
      IUFT6(FILNUM) = IU
      FT6FILNAM(FILNUM) = FNAME(1:IFLEN)
C
C-------ONCE ALL THREE MF6 "LINKER" FILES HAVE BEEN READ, SET IFTL FLAG TO TRUE TO AVOID STOPPAGE
      IF(FILNUM.EQ.3) IFTL=1
C
      END SUBROUTINE FMI1MF6NM
C
      SUBROUTINE FMI1MF6AR()
C **********************************************************************
C THIS SUBROUTINE CHECKS FLOW-TRANSPORT LINK FILE AND ALLOCATES SPACE
C FOR ARRAYS THAT MAY BE NEEDED BY FLOW MODEL-INTERFACE (FMI) PACKAGE.
C **********************************************************************
C
      USE MT3DMS_MODULE, ONLY: INFTL,IOUT,MXTRNOP,iUnitTRNOP,NPERFL,ISS,
     &                         IVER,IFTLFMT,NPERFL,ISS,IVER,FWEL,FDRN,
     &                         FRCH,FEVT,FRIV,FGHB,FSTR,FRES,FFHB,FIBS,
     &                         FTLK,FLAK,FMNW,FDRT,FETS,FSWT,FSFR,FUZF,
     &                         NPCKGTXT,FLAKFLOWS,FMNWFLOWS,FSFRFLOWS,
     &                         FUZFFLOWS,FSWR,FSWRFLOWS,FSFRLAK,FSFRUZF,
     &                         FLAKUZF,FSNKUZF,BUFF,ICBUND,HTOP,DZ
      USE GrbModule, ONLY: read_grb
      USE BudgetDataModule, ONLY: budgetdata_init, nbudterms, 
     &                            budgetdata_read, budtxt
      INTEGER, ALLOCATABLE, DIMENSION(:) :: mshape
      INTEGER :: ncrbud
      INTEGER I
      LOGICAL :: success
      CHARACTER(LEN=:), ALLOCATABLE :: TRIMADJL
C
C--ALLOCATE
      ALLOCATE(NPERFL,ISS,IVER,FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &         FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,FETS,
     &         FSWT,FSFR,FUZF,NPCKGTXT,FLAKFLOWS,FMNWFLOWS,FSFRFLOWS,
     &         FUZFFLOWS,FSWR,FSWRFLOWS,FSFRLAK,FSFRUZF,FLAKUZF,FSNKUZF)
C         
C-----AT LEAST 1 FLAG NEEDS TO BE TRIGGERED ON EACH PASS, OTHERWISE ONE OF THE 3 MF6 FILES IS MISSING
      DO I=1,3
        IF(IS_GRB(IUFT6(I))) THEN
          IUGRB=IUFT6(I)
        ELSEIF(IS_BUD(IUFT6(I))) THEN
          IUBUD=IUFT6(I)
        ELSEIF(IS_HDS(IUFT6(I))) THEN
          IUHDS=IUFT6(I)
        ELSE
          WRITE(ILIST,101)  FT6FILNAM(I), IUFT6(I)
  101     FORMAT(/1X,'FILE: ',A40,/1X,' ON UNIT: ',I4,
     &           ' NOT RECOGNIZED AS ONE OF THE THREE NECESSARY ',
     &           ' FT6 FILES.STOPPING.')
          CALL USTOP(' ')
        ENDIF
      ENDDO
C
C-----READ BINARY GRID INFORMATION AND CLOSE FILE
      CALL READ_GRB(ILIST, IUGRB, IA, JA, MSHAPE)
      CLOSE(IUGRB)
      NLAY = mshape(1)
      NROW = mshape(2)
      NCOL = mshape(3)
      !todo: check for pass through cells and bomb
      !
C     Allocate HEAD, BOTM (3D ARRAY OF CELL BOTTOM ELEVATIONS)
      ALLOCATE(HEAD(NCOL, NROW, NLAY))
      ALLOCATE(BOTM(NCOL, NROW, NLAY))
C
C-----INITIALIZE
      IVER=2
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
      FLAKFLOWS=.FALSE.
      FMNWFLOWS=.FALSE.
      FSFRFLOWS=.FALSE.
      FUZFFLOWS=.FALSE.
      FSWR=.FALSE.
      FSWRFLOWS=.FALSE.
      FSFRLAK=.FALSE.
      FSFRUZF=.FALSE.
      FLAKUZF=.FALSE.
      FSNKUZF=.FALSE.
C
C-----INITIALIZE BUDGET READER TO DETERMINE NUMBER OF ENTRIES      
      ISS_SY = 0
      ISS_SS = 0
      IDATA_SPDIS = 0
      CALL BUDGETDATA_INIT(IUBUD, ILIST, NCRBUD)
      DO I = 1, NBUDTERMS
        CALL BUDGETDATA_READ(SUCCESS)
        TRIMADJL = TRIM(ADJUSTL(BUDTXT))
        SELECT CASE(TRIM(ADJUSTL(TRIMADJL)))
          CASE ('DATA-SPDIS')
            idata_spdis = 1
          CASE ('STO-SS')
            ISS_SS = 1
          CASE ('STO-SY')
            ISS_SY = 1
          CASE ('WEL')
            WRITE(ILIST,110) TRIMADJL
            FWEL = .TRUE.
          CASE ('DRN')
            WRITE(ILIST,110) TRIMADJL
            FDRN = .TRUE.
          CASE ('RIV')
            WRITE(ILIST,110) TRIMADJL
            FRIV = .TRUE.
          CASE ('GHB')
            WRITE(ILIST,110) TRIMADJL
            FGHB = .TRUE.
          CASE ('RCH')
            WRITE(ILIST,110) TRIMADJL
            FRCH = .TRUE.
          CASE ('EVT')
            WRITE(ILIST,110) TRIMADJL
            FEVT = .TRUE.
          CASE DEFAULT
            WRITE(ILIST,111) TRIMADJL
        END SELECT
  110   FORMAT(/1X,A4,' package is active')
  111   FORMAT(/1X,'Found',A16,' while looking for package flows. ',
     &             'Skipping')
      ENDDO
      REWIND(IUBUD)
C
C-----SET NORMAL ISS FLAG BASED ON ISS_SS AND ISS_SY
      IF(ISS_SS.NE.0.OR.ISS_SY.NE.0) ISS = 0  
C
C-----BEFORE EXITING SUBROUTINE, CALCULATE 3D ARRAY OF BOTTOM OF CELL ELEVATIONS
C     (MF2K5 PASSES THKSAT, MF6 PASSES HEADS WHICH NEED TO BE CONVERTED TO THKSAT)
      CALL CALC_LAYELEVS(NLAY,NROW,NCOL,HTOP,DZ,BOTM)
C
      RETURN
      END SUBROUTINE FMI1MF6AR
C
      SUBROUTINE FMI1MF6RP1A(KPER,KSTP)
C **********************************************************************
C THIS SUBROUTINE READS MF6-STYLE HEADS, CONVERTS TO DH (THKSAT),
C READS MF6-STYLE BUDGET FILE AND FILLS QX, QY, QZ, AND USES 
C STORAGE INFORMATION IN THE MF6-STYLE BUDGET FILE TO FILL QSTO
C **********************************************************************
      USE MT3DMS_MODULE,    ONLY: DH,QX,QY,QZ,QSTO,ICBUND,HTOP,DZ
      USE GRBMODULE,        ONLY: READ_HDS
      USE BUDGETDATAMODULE, ONLY: NBUDTERMS, FLOWJA, FLOWDATA,
     &                            BUDGETDATA_READ, BUDTXT
      INTEGER :: KPER,KSTP
      INTEGER N, I, J, K
      LOGICAL :: SUCCESS
C
C     todo: check for successive time steps
C     Read head array
      CALL READ_HDS(IUHDS,NLAY,NROW,NCOL,HEAD)
C
C-----MOVE HEAD INTO DH
      CALL FILL_DH(DH,ICBUND,NLAY,NROW,NCOL,HEAD,HTOP,DZ,BOTM)
C
C-----PROCESS FLOWJA
C     IF PRESENT, START WITH STORAGE - INITIALIZE QSTO
      NBUD = 0
      IF (ISS_SY == 1 .OR. ISS_SS == 1) THEN
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              QSTO(J,I,K)=0.
            ENDDO
          ENDDO
        ENDDO
C
C-----PROCESS QSTO WITH SPECIFIC STORAGE
        IF(ISS_SS==1) THEN
          CALL BUDGETDATA_READ(SUCCESS)
          WRITE(ILIST,117) BUDTXT
  117     FORMAT(/1X,'RP1 Processing ', A16)
          IF (TRIM(ADJUSTL(BUDTXT)).NE.'STO-SS') CALL USTOP('')
          NBUD=NBUD+1
          N=1
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                QSTO(J,I,K)=QSTO(J,I,K)+FLOWDATA(1,N)
                N=N+1
              ENDDO
            ENDDO
          ENDDO            
        ENDIF
C
C-----PROCESS QSTO WITH SPECIFIC YIELD
        IF(ISS_SY == 1) THEN
          CALL BUDGETDATA_READ(SUCCESS)
          WRITE(ILIST,118) BUDTXT
  118     FORMAT(/1X,'RP1 Processing ', A16)
          IF (TRIM(ADJUSTL(BUDTXT)).NE.'STO-SY') CALL USTOP('')
          NBUD = NBUD + 1
          N = 1
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                QSTO(J,I,K)=QSTO(J,I,K)+FLOWDATA(1,N)
                N=N+1
              ENDDO
            ENDDO
          ENDDO            
        ENDIF   
      ENDIF
C
C-----PROCESS CELL-BY-CELL FLOW
      CALL BUDGETDATA_READ(SUCCESS)
      IF (.NOT. SUCCESS) CALL USTOP('')
      WRITE(ILIST,113) BUDTXT
  113 FORMAT(/1X,'RP1 Processing: ', A16)
      IF(TRIM(ADJUSTL(BUDTXT)).NE.'FLOW-JA-FACE') CALL USTOP('')
      NBUD = NBUD+1
      CALL FLOWJA2QXQYQZ(IA,JA,FLOWJA,QX,QY,QZ)
C
C-----PROCESS SPDIS 
      IF (IDATA_SPDIS == 1) THEN
        CALL BUDGETDATA_READ(SUCCESS)
        WRITE(ILIST,119) BUDTXT
  119   FORMAT(/1X,'RP1 Processing ', A16)
        IF(TRIM(ADJUSTL(BUDTXT)).NE.'DATA-SPDIS') CALL USTOP('')
        NBUD=NBUD+1
      ENDIF
C
      RETURN
      END SUBROUTINE FMI1MF6RP1A
C
      SUBROUTINE FMI1MF6RP2A(KPER,KSTP)
C **********************************************************************
C THIS SUBROUTINE READS SATURATED CELL THICKNESS, FLUXES ACROSS CELL
C INTERFACES, AND FLOW RATE TO OR FROM TRANSIENT STORAGE
C FROM AN UNFORMATTED MODFLOW6-STYLE OUTPUT FILE.
C **********************************************************************
C
      USE MT3DMS_MODULE, ONLY: IOUT,NTSS,NSS,SS,MXSS,ICBUND,FPRT,
     &                         ICTSPKG,NCOL,NROW,NLAY
      USE BUDGETDATAMODULE, ONLY: NBUDTERMS, NODESRC, FLOWDATA,
     &                            BUDGETDATA_READ, BUDTXT,
     &                            KPER=>KPERMF6, KSTP=>KSTPMF6
      INTEGER :: KPER,KSTP
      CHARACTER(LEN=:), ALLOCATABLE   :: TEXT
      LOGICAL :: SUCCESS
      INTEGER :: IBUD
      INTEGER :: IQ
      INTEGER :: NUM
      NTSS = NSS
      SS(8, :) = 0.
      DO NUM = 1, NTSS
        SS(5, NUM) = 0.
      ENDDO
      DO IBUD = 1, NBUDTERMS - NBUD
        CALL BUDGETDATA_READ(SUCCESS)
        !TODO: MAKE SURE MF6 KPER AND KSTP ARE SAME AS MT3D KPER KSTP
        WRITE(ILIST, 114) BUDTXT
  114   FORMAT(1/X,'RP2 Processing ',A16)
        IF (.NOT. SUCCESS) CALL USTOP('')
        SELECT CASE(TRIM(ADJUSTL(BUDTXT)))
          CASE('CHD')
            IQ = 1
          CASE('WEL')
            IQ = 2
          CASE('DRN')
            IQ = 3
          CASE('RIV')
            IQ = 4
          CASE('GHB')
            IQ = 5
          CASE('RCH')
            IQ = 7
          CASE('EVT')
            IQ = 8
          CASE('SFR')
            IQ = 30
          CASE('LAK')
            IQ = 26
          CASE('MAW')
            IQ = 27
          CASE DEFAULT
            WRITE(ILIST, *) 'ERROR. MF6 FLOW TYPE NOT SUPPORTED: ' //
     &                     TRIM(ADJUSTL(BUDTXT))  
          END SELECT
        TEXT=TRIM(ADJUSTL(BUDTXT)) 
        CALL MF6PUTSS(IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT,
     &                IQ,MXSS,NTSS,NSS,SS,ICBUND,FPRT, 
     &                NODESRC,FLOWDATA,ICTSPKG)
      ENDDO
      RETURN
C
      END SUBROUTINE FMI1MF6RP2A
C      
      SUBROUTINE FLOWJA2QXQYQZ(IA,JA,FLOWJA,QX,QY,QZ)
C **********************************************************************
C USES VALUES IN FLOWJA TO FILL NATIVE MT3D-USGS QX, QY, AND QZ ARRAYS
C **********************************************************************
C
        INTEGER,          DIMENSION(:), INTENT(IN)    :: IA
        INTEGER,          DIMENSION(:), INTENT(IN)    :: JA
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN)    :: FLOWJA
        REAL,       DIMENSION(:, :, :), INTENT(INOUT) :: QX,QY,QZ
C
        INTEGER :: NLAY,NROW,NCOL,NODES,NJA
        INTEGER :: IL,IR,IC,IJ
        INTEGER :: N,IPOS,M
C
C       INITIALIZE VARIABLES
        NCOL=SIZE(QX, 1)
        NROW=SIZE(QX, 2)
        NLAY=SIZE(QX, 3)
        NJA=SIZE(FLOWJA)
        NODES=SIZE(IA)-1
        DO IL=1,NLAY
          DO IR=1,NROW
            DO IC=1,NCOL
              QX(IC,IR,IL) = 0.
              QY(IC,IR,IL) = 0.
              QZ(IC,IR,IL) = 0.
            ENDDO
          ENDDO
        ENDDO
C
C       LOOP THROUGH FLOWS AND PUT THEM INTO QX, QY, QZ
        DO N=1, NODES
          DO IPOS=IA(N)+1, IA(N+1)-1
C
C           LOOP THROUGH CONNECTIONS FOR CELL N
            M=JA(IPOS)
            IF(M<N) CYCLE
C            
C           CALCULATE LAYER, ROW, AND COLUMN INDICES FOR CELL N
            IL=(N-1)/(NCOL*NROW)+1
            IJ=N-(IL-1)*NCOL*NROW
            IR=(IJ-1)/NCOL+1
            IC=IJ-(IR-1)*NCOL
C
C           RIGHT, FRONT, AND LOWER FACES
            IF(M==N+1) QX(IC,IR,IL)=FLOWJA(IPOS) * -1
            IF(M==N+NCOL) QY(IC,IR,IL)=FLOWJA(IPOS)
            IF(M==N+NROW*NCOL) QZ(IC,IR,IL)=FLOWJA(IPOS) * -1
C
          ENDDO
        ENDDO
C
        END SUBROUTINE FLOWJA2QXQYQZ
C
        SUBROUTINE FILL_DH(DH,ICBUND,NLAY,NROW,NCOL,HEAD,HTOP,DZ,BOTM)
C **********************************************************************
C THIS SUBROUTINE TAKES THE HEAD INFORMATION FROM A MODFLOW6 
C STYLE HEADS FILE AND FILLS THE SATURATED THICKNESS ARRAY "DH"
C **********************************************************************
        INTEGER,                              INTENT(IN) :: NLAY,NROW,
     &                                                      NCOL
        INTEGER,          DIMENSION(:,:,:,:), INTENT(IN) :: ICBUND
        REAL,             DIMENSION(:,:),     INTENT(IN) :: HTOP
        DOUBLE PRECISION, DIMENSION(:,:,:),   INTENT(IN) :: HEAD,BOTM
        REAL,             DIMENSION(:,:,:),   INTENT(IN) :: DZ
        REAL,             DIMENSION(:,:,:),   INTENT(INOUT) :: DH
C
        INTEGER I,J,K,IDX
        DOUBLE PRECISION TOTDZ,THKSAT
C
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,1).NE.0) THEN
                IF(HEAD(J,I,K)-BOTM(J,I,K).LE.0.0) THEN
                  DH(J,I,K)=0.0
                ELSE
                  THKSAT=HEAD(J,I,K)-BOTM(J,I,K)
                  IF(THKSAT.GT.DZ(J,I,K)) THEN
                    DH(J,I,K)=DZ(J,I,K)
                  ELSE
                    DH(J,I,K)=THKSAT
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
      END SUBROUTINE
C
C
      SUBROUTINE CALC_LAYELEVS(NLAY,NROW,NCOL,HTOP,DZ,BOTM)
C **********************************************************************
C THIS SUBROUTINE USES HTOP (GROUND ELEVATION) AND DZ (LAYER THICKNESS)
C TO FILL A 3D ARRAY OF BOTTOM ELEVATIONS FOR USE BY FUNCTION FILL_DH()
C **********************************************************************
C
      INTEGER,                           INTENT(IN)    :: NLAY,NROW,NCOL
      REAL,             DIMENSION(:,:),  INTENT(IN)    :: HTOP
      REAL,             DIMENSION(:,:,:),INTENT(IN)    :: DZ
      DOUBLE PRECISION, DIMENSION(:,:,:),INTENT(INOUT) :: BOTM
C
      INTEGER IDX,I,J,K
      DOUBLE PRECISION TOTDZ
C
C-----LOOP THROUGH ALL CELLS AND CALCULATE BOTTOM OF CELL ELEVATION
C     (PRESUMES STRUCTURED GRID IN MODFLOW6, STOPS BEFORE NOW IF NOT)
      DO I=1,NROW
        DO J=1,NCOL
          IDX=1
          TOTDZ=DZ(J,I,IDX)
          BOTM(J,I,IDX)=HTOP(J,I)-TOTDZ
          IF(K.GT.1) THEN
            IDX=IDX+1
            DO WHILE(IDX.LE.NLAY)
              TOTDZ=TOTDZ+DZ(J,I,IDX)
              BOTM(J,I,IDX)=HTOP(J,I)-TOTDZ
              IDX=IDX+1
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      RETURN     
C
      END SUBROUTINE CALC_LAYELEVS
C
C
      SUBROUTINE MF6PUTSS(IOUT, NCOL, NROW, NLAY, KSTP, KPER, TEXT,
     &                    IQ, MXSS, NTSS, NSS, SS, ICBUND, FPRT, 
     &                    NODESRC, FLOWDATA, ICTSPKG)
C
C       ARGUMENTS
        INTEGER,               INTENT(IN)    :: IOUT,NCOL,NROW,NLAY, 
     &                                          KSTP,KPER
        CHARACTER(LEN=*)                     :: TEXT
        INTEGER,               INTENT(IN)    :: IQ, MXSS
        INTEGER,               INTENT(INOUT) :: NTSS, NSS
        REAL, DIMENSION(:, :), INTENT(INOUT) :: SS
        CHARACTER(LEN=1),      INTENT(IN)    :: FPRT
        INTEGER, DIMENSION(:), INTENT(IN)    :: NODESRC
        DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: FLOWDATA
        INTEGER, DIMENSION(NCOL, NROW, NLAY), INTENT(INOUT) :: ICBUND
        INTEGER, INTENT(IN) :: ICTSPKG
        ! -- LOCAL
        INTEGER :: NLIST
        INTEGER :: L, ITEMP
        INTEGER :: N, IL, IJ, IR, IC
        INTEGER :: KKK, III, JJJ, ID
        REAL    :: QSTEMP, QSS
        !
        NLIST = SIZE(FLOWDATA, 2)
        DO L = 1, NLIST
C
C         GET CELL NUMBER AND FLOW
          N=NODESRC(L)
          QSTEMP=FLOWDATA(1,L)
C
C         CALCULATE LAYER, ROW, AND COLUMN INDICES FOR CELL N
          IL=(N-1)/(NCOL*NROW)+1
          IJ=N-(IL-1)*NCOL*NROW
          IR=(IJ-1)/NCOL+1
          IC=IJ-(IR-1)*NCOL
          !
          IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &            WRITE(IOUT,50) IL, IR, IC, QSTEMP            
C
C         IF ALREADY DEFINED AS A SOURCE OF USER-SPECIFIED CONCENTRATION
C         THEN STORE FLOW RATE (QSTEMP)
          DO ITEMP = 1, NSS
            KKK=SS(1,ITEMP)
            III=SS(2,ITEMP)
            JJJ=SS(3,ITEMP)
            QSS=SS(5,ITEMP)
            ID=SS(6,ITEMP)       
            IF (KKK.NE.IL.OR.III.NE.IR.OR.JJJ.NE.IC.OR. 
     &          ID.NE.IQ) CYCLE
            IF(ABS(QSS).GT.0) CYCLE                   
            SS(5,ITEMP) = QSTEMP
            SS(7,ITEMP) = 0
            IF(IQ.EQ.2.AND.ICTSPKG.EQ.1)
     &        SS(8,ITEMP) = L
C
C           MARK CELLS NEAR THE SINK/SOURCE                   
            IF(QSTEMP.LT.0.AND.ICBUND(IC,IR,IL).GT.0) THEN
              ICBUND(IC,IR,IL)=1000+IQ
            ELSEIF(ICBUND(IC,IR,IL).GT.0) THEN
              ICBUND(IC,IR,IL)=1020+IQ
            ENDIF
            GOTO 100
          ENDDO
C     
C         OTHWISE, ADD TO THE SS ARRAY
          NTSS=NTSS+1
          IF(NTSS.GT.MXSS) CYCLE
          SS(1,NTSS)=IL
          SS(2,NTSS)=IR
          SS(3,NTSS)=IC
          SS(4,NTSS)=0.
          SS(5,NTSS)=QSTEMP
          SS(6,NTSS)=IQ
          SS(7,NTSS)=0.
          IF(TEXT.EQ.'WEL'.AND.ICTSPKG.EQ.1)
     &      SS(8,NTSS)=N
          IF(QSTEMP.LT.0 .AND. ICBUND(IC,IR,IL).GT.0) THEN
            ICBUND(IC,IR,IL)=1000+IQ
          ELSEIF(ICBUND(IC,IR,IL).GT.0) THEN
            ICBUND(IC,IR,IL)=1020+IQ
          ENDIF
100       CONTINUE
        ENDDO
 50   FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7)
      END SUBROUTINE 
C
      END MODULE FMI1MF6
