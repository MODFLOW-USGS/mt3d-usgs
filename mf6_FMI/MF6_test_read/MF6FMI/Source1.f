      MODULE DUPLICATE
        IMPLICIT NONE
        REAL,         SAVE, DIMENSION(:,:,:), ALLOCATABLE :: DH,QX,QY,
     &                                                       QZ,QSTO
      CONTAINS
        SUBROUTINE MEMDEALLOCATE_DUP()
          IF(ALLOCATED(DH))   DEALLOCATE(DH)
          IF(ALLOCATED(QX))   DEALLOCATE(QX)
          IF(ALLOCATED(QY))   DEALLOCATE(QY)
          IF(ALLOCATED(QZ))   DEALLOCATE(QZ)
          IF(ALLOCATED(QSTO)) DEALLOCATE(QSTO)
        END SUBROUTINE
      END MODULE
C
      MODULE MF6FMI
        USE DUPLICATE
        IMPLICIT NONE
        INTEGER,      SAVE,                   ALLOCATABLE :: FILNUM
        INTEGER,      SAVE,                   ALLOCATABLE :: IUGRB
        INTEGER,      SAVE,                   ALLOCATABLE :: IUBUD
        INTEGER,      SAVE,                   ALLOCATABLE :: IUHDS
        INTEGER,      SAVE, DIMENSION(:),     ALLOCATABLE :: IUFT6
        CHARACTER*40, SAVE, DIMENSION(:),     ALLOCATABLE :: FT6FILNAM
        CHARACTER*6,  SAVE, DIMENsION(:,:),   ALLOCATABLE :: FT6TYP
C        
      CONTAINS
C    
        LOGICAL FUNCTION IS_GRB(IU,ILIST)
          CHARACTER(LEN=50) :: HDRTXT
          INTEGER IU,ILIST
          HDRTXT=''
          IS_GRB = .FALSE.
          READ(IU, ERR=100) HDRTXT
          IF(HDRTXT(1:8).EQ.'GRID DIS') IS_GRB=.TRUE.
          REWIND(IU)
  100     IF(HDRTXT.EQ.'') THEN
            WRITE(ILIST,200) IU
  200       FORMAT(1X,'UNIT NUMBER ',I4,' IS NOT A MODFLOW6 BINARY GRID'
     &                ' FILE. STOPPING.')
          ENDIF
          RETURN
        END FUNCTION
C
        LOGICAL FUNCTION IS_BUD(IU,ILIST)
          INTEGER IU,ILIST
          INTEGER KPER,KSTP
          CHARACTER(LEN=16) TEXT
          TEXT = ''
          IS_BUD = .FALSE.
          READ(IU, ERR=300) KPER,KSTP,TEXT
          IF(TEXT.EQ.'    FLOW-JA-FACE') IS_BUD = .TRUE.
          REWIND(IU)
  300     IF(TEXT.EQ.'') THEN
            WRITE(ILIST,400)
  400       FORMAT(1X,'UNIT NUMBER ',I4,' IS NOT A MODFLOW6 BINARY ',
     &                'BUDGET FILE. STOPPING.')
          ENDIF
          RETURN
        END FUNCTION
C
        LOGICAL FUNCTION IS_HDS(IU,ILIST)
          INTEGER IU,ILIST
          INTEGER KPER,KSTP
          DOUBLE PRECISION PERTIM,TOTIM
          CHARACTER(LEN=16) TEXT
          TEXT=''
          IS_HDS = .FALSE.
          READ(IU, ERR=500) KSTP,KPER,PERTIM,TOTIM,TEXT
          IF(TEXT.EQ.'            HEAD') IS_HDS = .TRUE.
  500     IF(TEXT.EQ.'') THEN
            WRITE(ILIST, 600) IU
  600       FORMAT(1X,'UNIT NUMBER ',I4,' IS NOT A MODFLOW6 BINARY ',
     &                'HEADS FILE. STOPPING.')
            CALL USTOP(' ')
          ENDIF
          RETURN
        END FUNCTION
C
        SUBROUTINE MEMDEALLOCATE()
          IF(ALLOCATED(FILNUM))      DEALLOCATE(FILNUM)
          IF(ALLOCATED(IUGRB))       DEALLOCATE(IUGRB)
          IF(ALLOCATED(IUBUD))       DEALLOCATE(IUBUD)
          IF(ALLOCATED(IUHDS))       DEALLOCATE(IUHDS)
          IF(ALLOCATED(IUFT6))       DEALLOCATE(IUFT6)
          IF(ALLOCATED(FT6FILNAM))   DEALLOCATE(FT6FILNAM)
          IF(ALLOCATED(FT6TYP))      DEALLOCATE(FT6TYP)
        END SUBROUTINE MEMDEALLOCATE
C
        SUBROUTINE MF6FMINAM(FNAME,IU,ILIST,FILSTAT,FILACT,FMTARG,IFLEN)
          IMPLICIT     NONE
          INTEGER      ILIST,IFLEN,IU
          CHARACTER*7  FILSTAT
          CHARACTER*20 FMTARG, ACCARG, FILACT
          CHARACTER*40 FNAME
C
          IF (.NOT. ALLOCATED(IUFT6)) ALLOCATE(IUFT6(3))
C
C---------ALLOCATE THE FOLLOWING, THOUGHT IT WON'T BE SORTED OUT 
C         UNTIL MF6FMIAR()
          IF (.NOT. ALLOCATED(FT6TYP)) THEN
             ALLOCATE(FT6TYP(3,2))
             FT6TYP(1,1)='FT6GRD'
             FT6TYP(2,1)='FT6BUD'
             FT6TYP(3,1)='FT6HDS'
          ENDIF
C
          IF (.NOT. ALLOCATED(FILNUM)) THEN
            ALLOCATE(IUGRB)
            ALLOCATE(IUBUD)
            ALLOCATE(IUHDS)
            ALLOCATE(FILNUM)
            ALLOCATE(FT6FILNAM(3))
            FILNUM=0
            IUGRB=0
            IUBUD=0
            IUHDS=0
          ENDIF
C
          FILNUM = FILNUM + 1
          IF(IU.LT.21) THEN
            IU = IU+20+FILNUM   ! file unit numbers 21-23 not taken.  
            IUFT6(FILNUM) = IU
          ELSEIF(IU.NE.200.AND.IU.NE.300.AND.IU.NE.400.AND.IU.NE.600.
     &           .AND.IU.NE.800) THEN  ! make sure a reserved number wasn't specified
            IUFT6(FILNUM) = IU
          ENDIF
          FT6FILNAM(FILNUM) = FNAME(1:IFLEN)
C
        END SUBROUTINE MF6FMINAM
C
C-------SORT OUT WHICH FILE IS GRB, BUD, AND HDS (DON'T RELY ON FILE EXTENTION, SINCE THESE CAN BE ARBITRARY)
        SUBROUTINE MF6FMIAR(ILIST)
          INTEGER I,ILIST
          LOGICAL IS_GRB,IS_BUD,IS_HDS
C         
C         AT LEAST 1 FLAG NEEDS TO BE TRIGGERED ON EACH PASS, OTHERWISE ONE OF THE 3 MF6 FILES IS MISSING
          DO I=1,3
            IF(IS_GRB(IUFT6(I))) THEN
              IUGRB=21
            ELSEIF(IS_BUD(IUFT6(I))) THEN
              IUBUD=22
            ELSEIF(IS_HDS(IUFT6(I))) THEN
              IUHDS=23
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
          USE DUPLICATE
          
        END SUBROUTINE MF6FMIRP1
C
        SUBROUTINE MF6FMIRP2()
        
        END SUBROUTINE MF6FMIRP2
      END MODULE MF6FMI
!
!  ************************************************
!  *                                              *
!  *                MAIN PROGRAM                  *
!  *                                              *
!  ************************************************
      USE MF6FMI
      USE DUPLICATE
C
      INTEGER   INUNIT,ILIST,IU,LLOC,IFLEN,N,ISTART,ISTOP,ITYP1,ITYP2,
     &          INAM1,INAM2
      REAL      R
      CHARACTER LINE*200
      CHARACTER FNAME*40
      CHARACTER FILSTAT*7
      CHARACTER*20 FMTARG, ACCARG, FILACT 
      CHARACTER FLNAME*5000
      CHARACTER COMLIN*2000
      LOGICAL   EXISTED
C
      INCLUDE 'openspec.inc'
C
C-----INITIALIZE (HARD-WIRE) SOME VARIABLE FOR THIS SIMPLE TEST PROBLEM
      ILIST   = 1
      FILSTAT = 'OLD    '
      FMTARG  = 'FORMATTED'
      ACCARG  = 'SEQUENTIAL'
      FILACT  = ACTION(2)
!
!-----OPEN UP FAKE NAME FILE
      CALL GETARG(1,COMLIN)
C                                          
      IF(COMLIN.NE.' ') FLNAME=COMLIN            
C
C-Open files using the Name File method as in MODFLOW-2000      
      IFLEN=INDEX(FLNAME,' ')-1
      INQUIRE(FILE=FLNAME(1:IFLEN),EXIST=EXISTED)
      IF(.NOT.EXISTED) THEN
        FLNAME=FLNAME(1:IFLEN)//'.nam'
        INQUIRE(FILE=FLNAME(1:iflen+4),EXIST=EXISTED)
        IF(.NOT.EXISTED) THEN
          WRITE(*,103) FLNAME(1:IFLEN),FLNAME(1:IFLEN+4)
          CALL USTOP(' ')
        ENDIF
      ENDIF
  103 FORMAT(1x,'STOP. Specified Name file does not exist: ',
     &        a,' or ',a)
      WRITE(*,104) TRIM(FLNAME)
  104 FORMAT(1x,'Using NAME File: ',a)
      iNameFile=99
      OPEN(iNameFile,file=flname,status='old')
!
      OPEN(UNIT=11, FILE='exp_listing_fl.txt', STATUS='UNKNOWN')
!
!-----SET AN ARBITRARY UNIT NUMBER FOR FAKE NAME FILE CONTAINING FT6 ENTRIES
      INUNIT = 10
!
!-----READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
   10 READ(iNameFile,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GOTO 10
      IF(LINE(1:1).EQ.'#') THEN
        IF(ILIST.NE.0) WRITE(ILIST,'(A)') TRIM(LINE)
        GOTO 10
      ENDIF
!
!--DECODE THE FILE TYPE AND UNIT NUMBER.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,ILIST,INUNIT)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,ILIST,INUNIT)
!
!--DECODE THE FILE NAME.  (This will be an ELSEIF in the main code.)
      IF(LINE(ITYP1:ITYP2).EQ.'FT6') THEN
        CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,ILIST,INUNIT)
        IFLEN=INAM2-INAM1+1
        FNAME(1:IFLEN)=LINE(INAM1:INAM2)
!
!--SET VARIABLES FOR CALL TO MF6FMINAM(..)
        FILACT=ACTION(1)
        FMTARG=FORM  ! FORM is equal to 'BINARY'
!--CHECK FOR "FT6"
        CALL MF6FMINAM(FNAME,IU,ILIST,FILSTAT,FILACT,FMTARG,IFLEN)
      ENDIF
      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),STATUS=FILSTAT, 
     &         FORM=FMTARG,ACCESS=ACCARG,ACTION=FILACT)
      GO TO 10
      !
!-----END OF FAKE NAME FILE.  RETURN PROVIDED THAT NEW FTL FILE
!-----TYPE HAS BEEN OPENED.
 1000 CALL MF6FMIAR(ILIST)
      
      IF(ILIST.NE.0) THEN
        WRITE(ILIST,1001)
      ENDIF
 1001 FORMAT(1X,'STOPPING.  HAVE TESTED READING FAKE FTL FILES.')   
!
      CALL MEMDEALLOCATE()
      STOP
      END