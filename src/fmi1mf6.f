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
        integer :: iss_sy
        integer :: iss_ss
        integer :: idata_spdis
        integer :: nbud
        double precision, dimension(:, :, :), allocatable :: head
        integer, allocatable, dimension(:) :: ia
        integer, allocatable, dimension(:) :: ja
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
          REWIND(IU)
          IF(TRIM(ADJUSTL(TEXT)).EQ.'FLOW-JA-FACE') IS_BUD = .TRUE.
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
        END SUBROUTINE MEMDEALLOCATE
C
        SUBROUTINE FMI1MF6NM(FNAME,IU,IOUT,FILSTAT,FILACT,FMTARG,IFLEN)
          IMPLICIT     NONE
          INTEGER      IOUT,IFLEN,IU,INFT1,INFT2,INFT3
          CHARACTER*7  FILSTAT
          CHARACTER*20 FMTARG, ACCARG, FILACT
          CHARACTER*40 FNAME
C
C         SAVE ILIST
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
     &                         FLAKUZF,FSNKUZF,NROW,NCOL,NLAY,BUFF
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
      CALL read_grb(ilist, iugrb, ia, ja, mshape)
      CLOSE(iugrb)
      NLAY = mshape(1)
      NROW = mshape(2)
      NCOL = mshape(3)
      !todo: check for pass through cells and bomb
      !
      ! -- allocate head
      ALLOCATE(head(ncol, nrow, nlay))
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
      CALL budgetdata_init(iubud, ilist, ncrbud)
      DO I = 1, nbudterms
        CALL budgetdata_read(success)
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
            FDRN = .TRUE.
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
      RETURN
      END SUBROUTINE FMI1MF6AR
C
      SUBROUTINE FMI1MF6RP1A(KPER,KSTP)
        USE MT3DMS_MODULE, ONLY: DH,QX,QY,QZ,QSTO
        use GrbModule, only: read_hds
        use BudgetDataModule, only: nbudterms, flowja, flowdata,
     &                              budgetdata_read, budtxt
        INTEGER :: KPER,KSTP
        integer n, i, j, k
        logical :: success
        !
        !todo: check for successive time steps
        ! -- read head array
        call read_hds(iuhds, nlay, nrow, ncol, head)
        !
        ! -- move head into dh
        ! todo
        !
        ! -- Process flowja
        nbud = 0
        call budgetdata_read(success)
        IF (.NOT. SUCCESS) CALL USTOP('')
        WRITE(ILIST,113) BUDTXT
  113   FORMAT(/1X,'RP1 Processing: ', A16)
        IF(TRIM(ADJUSTL(BUDTXT)).NE.'FLOW-JA-FACE') CALL USTOP('')
        nbud = nbud + 1
        CALL flowja2qxqyqz(ia, ja, flowja, qx, qy, qz)
        !
        ! -- process spdis 
        if (idata_spdis == 1) THEN
          call budgetdata_read(success)
          print*,'RP1 Processing ', budtxt
          if (trim(adjustl(budtxt)).ne.'DATA-SPDIS') call ustop('')
          nbud = nbud + 1
        ENDIF
        !
        ! -- initialize qsto
        IF (ISS_SY == 1 .OR. ISS_SS == 1) THEN
          DO K = 1, NLAY
            DO I = 1, NROW
              DO J = 1, NCOL
                qsto(j, i, k) = 0.
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        !
        ! -- process qsto with specific storage
        IF(ISS_SS == 1) THEN
          call budgetdata_read(success)
          print*,'RP1 Processing ', budtxt
          if (trim(adjustl(budtxt)).ne.'STO-SS') call ustop('')
          nbud = nbud + 1
          n = 1
          DO K = 1, NLAY
            DO I = 1, NROW
              DO J = 1, NCOL
                qsto(j, i, k) = qsto(j, i, k) + flowdata(1, n)
                n = n + 1
              ENDDO
            ENDDO
          ENDDO            
        ENDIF
        !
        ! -- process qsto with specific yield
        if (iss_sy == 1) THEN
          call budgetdata_read(success)
          print*,'RP1 Processing ', budtxt
          if (trim(adjustl(budtxt)).ne.'STO-SY') call ustop('')
          nbud = nbud + 1
          n = 1
          do k = 1, nlay
            do i = 1, nrow
              do j = 1, ncol
                qsto(j, i, k) = qsto(j, i, k) + flowdata(1, n)
                n = n + 1
              enddo
            enddo
          enddo            
        ENDIF

      RETURN
      END SUBROUTINE FMI1MF6RP1A
C
        SUBROUTINE FMI1MF6RP2A(KPER,KSTP)
          use MT3DMS_MODULE, only: IOUT,NTSS,NSS,SS,MXSS,ICBUND,FPRT,
     &                             ICTSPKG
          use BudgetDataModule, only: nbudterms, flowdata,
     &                                budgetdata_read, budtxt,
     &                                kper=>kpermf6, kstp=>kstpmf6
          INTEGER :: KPER,KSTP
          CHARACTER(LEN=16) :: TEXT
          logical :: success
          integer :: ibud
          integer :: iq
          integer :: num
          ntss = nss
          ss(8, :) = 0.
          DO num = 1, ntss
            ss(5, num) = 0.
          ENDDO
          DO ibud = 1, nbudterms - nbud
            CALL budgetdata_read(success)
            !todo: make sure mf6 kper and kstp are same as mt3d kper kstp
            WRITE(ILIST, 114) budtxt
  114       FORMAT(1/X,'RP2 Processing ',A16)
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
                WRITE(iout, *) 'ERROR. MF6 FLOW TYPE NOT SUPPORTED: ' //
     &                         TRIM(ADJUSTL(BUDTXT))  
            END SELECT
            CALL mf6putss(iout, ncol, nrow, nlay, kstp, kper, text,
     &                    iq, mxss, ntss, nss, ss, icbund, fprt, 
     &                    flowdata, ictspkg)
          enddo
          return
      END SUBROUTINE FMI1MF6RP2A
      
      SUBROUTINE flowja2qxqyqz(ia, ja, flowja, qx, qy, qz)
        INTEGER, DIMENSION(:), INTENT(IN) :: ia
        INTEGER, DIMENSION(:), INTENT(IN) :: ja
        double precision, dimension(:), intent(in) :: flowja
        real, dimension(:, :, :), intent(inout) :: qx, qy, qz
        integer :: nlay, nrow, ncol, nodes, nja
        integer :: il, ir, ic, ij
        integer :: n, ipos, m
        !
        ! -- initialize variables
        NCOL = SIZE(QX, 1)
        NROW = SIZE(QX, 2)
        NLAY = SIZE(QX, 3)
        NJA = SIZE(FLOWJA)
        NODES = SIZE(IA) - 1
        DO IL = 1, NLAY
          DO IR = 1, NROW
            DO IC = 1, NCOL
              QX(IC, IR, IL) = 0.
              QY(IC, IR, IL) = 0.
              QZ(IC, IR, IL) = 0.
            ENDDO
          ENDDO
        ENDDO
        !
        ! -- loop through flows and put them into qx, qy, qz
        do n = 1, nodes
          do ipos = ia(n) + 1, ia(n + 1) - 1
            !
            ! -- loop through connections for cell n
            m = ja(ipos)
            if (m < n) cycle
            
            ! -- calculate layer, row, and column indices for cell n
            il = (n - 1) / (ncol * nrow) + 1
            ij = n - (il - 1) * ncol * nrow
            ir = (ij - 1) / ncol + 1
            ic = ij - (ir - 1) * ncol
            !
            ! -- right, front, and lower faces
            IF (m == n + 1) qx(ic, ir, il) = flowja(ipos)
            IF (m == n + ncol) qy(ic, ir, il) = flowja(ipos)
            IF (m == n * nrow * ncol) qz(ic, ir, il) = flowja(ipos)
            !
          ENDDO
        ENDDO
        END SUBROUTINE FLOWJA2QXQYQZ

        SUBROUTINE mf6putss(iout, ncol, nrow, nlay, kstp, kper, text,
     &                      iq, mxss, ntss, nss, ss, icbund, fprt, 
     &                      flowdata, ictspkg)
          ! -- arguments
          integer, intent(in) :: iout, ncol, nrow, nlay, kstp, kper
          character(len=16), intent(in) :: text
          integer, intent(in) :: iq, mxss
          integer, intent(inout) :: ntss, nss
          real, dimension(:, :), intent(inout) :: ss
          integer, dimension(ncol, nrow, nlay), intent(inout) :: icbund
          character(len=1), intent(in) :: fprt
          double precision, dimension(:, :), intent(in) :: flowdata
          integer, intent(in) :: ictspkg
          ! -- local
          integer :: nlist
          integer :: l, itemp
          integer :: n, il, ij, ir, ic
          integer :: kkk, iii, jjj, id
          real :: qstemp, qss
          !
          nlist = size(flowdata, 2)
          do l = 1, nlist
            !
            ! -- get cell number and flow
            n = flowdata(1, l)
            qstemp = flowdata(2, l)
            !
            ! -- calculate layer, row, and column indices for cell n
            il = (n - 1) / (ncol * nrow) + 1
            ij = n - (il - 1) * ncol * nrow
            ir = (ij - 1) / ncol + 1
            ic = ij - (ir - 1) * ncol
            !
            IF(FPRT.EQ.'Y'.OR.FPRT.EQ.'y') 
     &              WRITE(IOUT,50) il, ir, ic, QSTEMP            
            !
            ! -- if already defined as a source of user-specified concentration
            !    then store flow rate (qstemp)
            do itemp = 1, nss
              kkk = ss(1,itemp)
              iii = ss(2,itemp)
              jjj = ss(3,itemp)
              qss = ss(5,itemp)
              id = ss(6,itemp)       
              if (kkk .ne. il .or. iii .ne. ir .or. jjj .ne. ic .or. 
     &          id .ne. iq) cycle
              if(abs(qss) .gt. 0) cycle                   
              ss(5,itemp) = qstemp
              ss(7,itemp) = 0
              if(iq .eq. 2 .and. ictspkg .eq. 1)
     &          ss(8,itemp) = l
              !
              !---Mark cells near the sink/source                   
              if(qstemp .lt. 0 .and. icbund(ic, ir, il) .gt. 0) then
                icbund(ic, ir, il)= 1000 + iq
              elseif(icbund(ic, ir, il) .gt. 0 ) then
                icbund(ic, ir, il) = 1020 + iq
              endif
              goto 100
            enddo
100         continue
          enddo
 50     FORMAT(1X,'LAYER',I5,5X,'ROW',I5,5X,'COLUMN',I5,5X,'RATE',G15.7)
        end subroutine 
        
      END MODULE FMI1MF6
