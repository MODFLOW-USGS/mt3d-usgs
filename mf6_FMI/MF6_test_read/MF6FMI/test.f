C
!
!  ************************************************
!  *                                              *
!  *                MAIN PROGRAM                  *
!  *                                              *
!  ************************************************
      USE MF6FMI
      USE MT3DMS_MODULE
C
      INTEGER   INUNIT,IOUT,IU,LLOC,IFLEN,N,ISTART,ISTOP,ITYP1,ITYP2,
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
      IOUT   = 1
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
      OPEN(UNIT=IOUT, FILE='exp_listing_fl.txt', STATUS='UNKNOWN')
!
!-----SET AN ARBITRARY UNIT NUMBER FOR FAKE NAME FILE CONTAINING FT6 ENTRIES
      INUNIT = 10
!
!-----READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
   10 READ(iNameFile,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GOTO 10
      IF(LINE(1:1).EQ.'#') THEN
        IF(IOUT.NE.0) WRITE(IOUT,'(A)') TRIM(LINE)
        GOTO 10
      ENDIF
!
!--DECODE THE FILE TYPE AND UNIT NUMBER.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
!
!--DECODE THE FILE NAME.  (This will be an ELSEIF in the main code.)
      IF(LINE(ITYP1:ITYP2).EQ.'FT6') THEN
        CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INUNIT)
        IFLEN=INAM2-INAM1+1
        FNAME=''
        FNAME(1:IFLEN)=LINE(INAM1:INAM2)
!
!--SET VARIABLES FOR CALL TO MF6FMINAM(..)
        FILACT=ACTION(1)
        FMTARG=FORM  ! FORM is equal to 'BINARY'
!--CHECK FOR "FT6"
        CALL MF6FMINAM(FNAME,IU,IOUT,FILSTAT,FILACT,FMTARG,IFLEN)
      ENDIF
      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),STATUS=FILSTAT, 
     &         FORM=FMTARG,ACCESS=ACCARG,ACTION=FILACT)
      GO TO 10
      !
!-----END OF FAKE NAME FILE.  RETURN PROVIDED THAT NEW FTL FILE
!-----TYPE HAS BEEN OPENED.
 1000 CONTINUE
      
!-----MF6 FMI ALLOCATE AND READ
      CALL MF6FMIAR()
!
!-----MF6 RP1 FILL DH,QX,QY,QZ FROM BUDGET AND HEADS FILE
      CALL MF6FMIRP1()

!-----MF6 RP2 FILL SS ARRAY FROM BUDGET FILE
      CALL MF6FMIRP2()
      
      IF(IOUT.NE.0) THEN
        WRITE(IOUT,1001)
      ENDIF
 1001 FORMAT(1X,'STOPPING.  HAVE TESTED READING FAKE FTL FILES.')   
!
      CALL MEMDEALLOCATE()
      STOP
      END