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
          INTEGER I
C
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
        END SUBROUTINE MF6FMIAR
C
        SUBROUTINE MF6FMIRP1()
          USE MT3DMS_MODULE, ONLY: DH,QX,QY,QZ,QSTO
          use GrbModule, only: read_grb
          integer, allocatable, dimension(:) :: ia
          integer, allocatable, dimension(:) :: ja
          integer, allocatable, dimension(:) :: mshape
          double precision, dimension(:, :, :), allocatable head
          !
          ! -- read binary grid information and close file
          call read_grb(ilist, iugrb, ia, ja, mshape)
          close(iugrb)
          nlay = mshape(1)
          nrow = mshape(2)
          ncol = mshape(3)
          !
          ! -- read head array
          allocate(head(ncol, nrow, nlay))
          call read_hds(iuhds, nlay, nrow, ncol, head)
          
          return
        END SUBROUTINE MF6FMIRP1
C
        SUBROUTINE MF6FMIRP2()
        
        END SUBROUTINE MF6FMIRP2
      END MODULE MF6FMI
