      MODULE MF6FMI
        USE MT3DMS_MODULE
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
        SUBROUTINE MF6FMINAM(FNAME,IU,IOUT,FILSTAT,FILACT,FMTARG,IFLEN)
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
          IF(IU.EQ.0) THEN
            IF (FILNUM.EQ.1) IU=INFT1
            IF (FILNUM.EQ.2) IU=INFT2
            IF (FILNUM.EQ.3) IU=INFT3
            IUFT6(FILNUM) = IU
          ENDIF
          FT6FILNAM(FILNUM) = FNAME(1:IFLEN)
C
        END SUBROUTINE MF6FMINAM
C
C-------SORT OUT WHICH FILE IS GRB, BUD, AND HDS (DON'T RELY ON FILE EXTENTION, SINCE THESE CAN BE ARBITRARY)
        SUBROUTINE MF6FMIAR()
          use MT3DMS_MODULE, only: memallocate
          use GrbModule, only: read_grb
          use BudgetDataModule, only: budgetdata_init, nbudterms, 
     &                                budgetdata_read, budtxt
          integer, allocatable, dimension(:) :: mshape
          integer :: ncrbud
          INTEGER I
          logical :: success
C         
C         AT LEAST 1 FLAG NEEDS TO BE TRIGGERED ON EACH PASS, OTHERWISE ONE OF THE 3 MF6 FILES IS MISSING
          DO I=1,3
            IF(IS_GRB(IUFT6(I))) THEN
              IUGRB=IUFT6(I)
            ELSEIF(IS_BUD(IUFT6(I))) THEN
              IUBUD=IUFT6(I)
            ELSEIF(IS_HDS(IUFT6(I))) THEN
              IUHDS=IUFT6(I)
            ELSE
              WRITE(ILIST,101)  FT6FILNAM(I), IUFT6(I)
  101         FORMAT(/1X,'FILE: ',A40,' ON UNIT: ',I4,' NOT RECOGNIZED',
     &               'AS ONE OF THE THREE NECESSARY FT6 FILES.',
     &               'STOPPING.')
              CALL USTOP(' ')
            ENDIF
          ENDDO
          !
          ! -- read binary grid information and close file
          call read_grb(ilist, iugrb, ia, ja, mshape)
          close(iugrb)
          nlay = mshape(1)
          nrow = mshape(2)
          ncol = mshape(3)
          !
          ! -- for test program, need to allocate mt modules variables
          ! todo: not needed when merged with mt3d program
          call memallocate(nlay, nrow, ncol)
          !
          ! -- allocate head
          allocate(head(ncol, nrow, nlay))
          !
          ! -- Initialize budget reader to determine number of entries
          iss_sy = 0
          iss_ss = 0
          idata_spdis = 0
          call budgetdata_init(iubud, ilist, ncrbud)
          do i = 1, nbudterms
            call budgetdata_read(success)
            print *, 'found package of type: ', budtxt
            select case(trim(adjustl(budtxt)))
            case ('DATA-SPDIS')
              idata_spdis = 1
            case ('STO-SS')
              iss_ss = 1
            case ('STO-SY')
              iss_sy = 1
            case('WEL')
              print *, 'well package is active'
              ! todo: FWEL = .TRUE.
            case('DRN')
              print *, 'drain package is active'
            end select
          enddo
          rewind(iubud)
          return
        END SUBROUTINE MF6FMIAR
C
        SUBROUTINE MF6FMIRP1()
          USE MT3DMS_MODULE, ONLY: DH,QX,QY,QZ,QSTO
          use GrbModule, only: read_hds
          use BudgetDataModule, only: nbudterms, flowja, flowdata,
     &                                budgetdata_read, budtxt
          integer n, i, j, k
          logical :: success
          !
          ! -- read head array
          call read_hds(iuhds, nlay, nrow, ncol, head)
          !
          ! -- move head into dh
          ! todo
          !
          ! -- Process flowja
          nbud = 0
          call budgetdata_read(success)
          if (.not. success) call ustop('')
          print*,'RP1 Processing ', budtxt
          if (trim(adjustl(budtxt)).ne.'FLOW-JA-FACE') call ustop('')
          nbud = nbud + 1
          call flowja2qxqyqz(ia, ja, flowja, qx, qy, qz)
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
          if (iss_sy == 1 .or. iss_ss == 1) then
            do k = 1, nlay
              do i = 1, nrow
                do j = 1, ncol
                  qsto(j, i, k) = 0.
                enddo
              enddo
            enddo
          endif
          !
          ! -- process qsto with specific storage
          if (iss_ss == 1) THEN
            call budgetdata_read(success)
            print*,'RP1 Processing ', budtxt
            if (trim(adjustl(budtxt)).ne.'STO-SS') call ustop('')
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

          return
        END SUBROUTINE MF6FMIRP1
C
        SUBROUTINE MF6FMIRP2()
          use BudgetDataModule, only: nbudterms, 
     &                                budgetdata_read, budtxt
          logical :: success
          integer :: ibud
          do ibud = 1, nbudterms - nbud
            call budgetdata_read(success)
            print*,'RP2 Processing ', budtxt
            if (.not. success) call ustop('')            
          enddo
          return
      END SUBROUTINE MF6FMIRP2
      
      subroutine flowja2qxqyqz(ia, ja, flowja, qx, qy, qz)
        integer, dimension(:), intent(in) :: ia
        integer, dimension(:), intent(in) :: ja
        double precision, dimension(:), intent(in) :: flowja
        real, dimension(:, :, :), intent(inout) :: qx, qy, qz
        integer :: nlay, nrow, ncol, nodes, nja
        integer :: il, ir, ic, ij
        integer :: n, ipos, m
        !
        ! -- initialize variables
        ncol = size(qx, 1)
        nrow = size(qx, 2)
        nlay = size(qx, 3)
        nja = size(flowja)
        nodes = size(ia) - 1
        do il = 1, nlay
          do ir = 1, nrow
            do ic = 1, ncol
              qx(ic, ir, il) = 0.
              qy(ic, ir, il) = 0.
              qz(ic, ir, il) = 0.
            enddo
          enddo
        enddo
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
            if (m == n + 1) qx(ic, ir, il) = flowja(ipos)
            if (m == n + ncol) qy(ic, ir, il) = flowja(ipos)
            if (m == n * nrow * ncol) qz(ic, ir, il) = flowja(ipos)
            !
          enddo
        enddo
        end subroutine flowja2qxqyqz


      END MODULE MF6FMI
