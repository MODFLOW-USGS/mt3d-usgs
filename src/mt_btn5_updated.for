C
      SUBROUTINE BTN5OPEN(INUNIT)
C *******************************************************************
C OPEN FILES, USING THE METHOD OF MODFLOW-96, 2000 & 2005
c NOTE: THE STYLE OF UNFORMATTED FILES IS SPECIFIED IN THE
C INCLUDE FILE 'FILESPEC.INC'
C *******************************************************************
C Last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: IOUT,INBTN,INADV,INDSP,INSSM,INRCT,INGCG,
     &                         INTOB,INHSS,INFTL,FPRT,MXTRNOP,
     &                         iUnitTRNOP,NameTRNOP,ICNF,IUCN,IUCN2,
     &                         IOBS,IMAS,ICBM,IFTLFMT,
     &                         INUZT,                               !edm
     &                         INRTR,INLKT,INSFT,INCTS,INTSO,MINVOL !# LINES 3-4 BTN
C
      USE MIN_SAT                                                   !# LINE 12 BTN
      IMPLICIT NONE
      INTEGER       INUNIT,IBTN,IFTL,IFLEN,I,ILIST,LLOC,
     &              ITYP1,ITYP2,N,ISTART,ISTOP,IU,INAM1,INAM2,
     &              ISTART2,ISTOP2
      REAL          R
      LOGICAL       LOP
      CHARACTER*200 LINE,FNAME
      CHARACTER*20  FMTARG,ACCARG,FILACT
      CHARACTER     FILSTAT*7
C
      INCLUDE 'FILESPEC.INC'
C
C--ALLOCATE
      ALLOCATE(IOUT,INBTN,INADV,INDSP,INSSM,INRCT,INGCG,INTOB,INHSS,
     &         INFTL,ICNF,IUCN,IUCN2,IOBS,IMAS,ICBM,IFTLFMT,FPRT,
     &         iUnitTRNOP(MXTRNOP),
     &         INUZT,INCTS,INTSO,INRTR,INLKT,INSFT)                 !edm
C--ALLOCATE SCALAR VARIABLES
      !ALLOCATE(DOMINSAT)                                       !# NEEDED
      ALLOCATE(IATS)                                            !# NEEDED
      !ALLOCATE(DRYON)                                          !# NEEDED
      ALLOCATE(MUTDRY,IC2DRY,MINVOL,IDRYBUD)                    !# NEW
C
C--SET DEFAULT UNIT NUMBERS
      INBTN=1
      INFTL=10
      IOUT =16
      INADV=2
      INDSP=3
      INSSM=4
      INCTS=6                                                       !# Not set in Vivek's Main as expected, setting it here
      INUZT=7                                                       !edm
      INRCT=8
      INGCG=9  
      INTOB=12
      INHSS=13
      INTSO=14                                                      !# Not set in Vivek's Main as expected, setting it here
      INRTR=15                                                      !# LINE 144 MAIN
      ICNF =17
      INLKT=18                                                      !# LINE 145 MAIN
      INSFT=19                                                      !# LINE 145 MAIN
      IUCN =200
      IUCN2=300
      IOBS =400
      IMAS =600
      ICBM =800
C
C--INITIALIZE.
      FPRT=' '
      ILIST=0
      IBTN=0
      IFTL=0      
      DO I=1,MXTRNOP
        iUnitTRNOP(I)=0
      ENDDO
C
C--READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
   10 READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GOTO 10
      IF(LINE(1:1).EQ.'#') THEN
        IF(ILIST.NE.0) WRITE(IOUT,'(A)') LINE
        GOTO 10
      ENDIF
C
C--DECODE THE FILE TYPE AND UNIT NUMBER.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
C
C--DECODE THE FILE NAME.
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INUNIT)
      IFLEN=INAM2-INAM1+1
      FNAME(1:IFLEN)=LINE(INAM1:INAM2)
C
C--CHECK FOR A VALID FILE TYPE.
      FMTARG='FORMATTED'
      ACCARG='SEQUENTIAL'
      FILSTAT='UNKNOWN'
      FILACT=ACTION(2)
C
C--FIRST ENTRY MUST BE FILE-TYPE "LIST".
      IF(ILIST.EQ.0) THEN
        IF(LINE(ITYP1:ITYP2).NE.'LIST') THEN
          WRITE(*,11)
   11     FORMAT(1X,'FIRST ENTRY IN NAME FILE MUST BE "LIST".')
          CALL USTOP(' ')
        ENDIF
        IF(IU.EQ.0) THEN
          IU=IOUT
        ELSEIF(IU.GT.0) THEN
          IOUT=IU
        ENDIF  
C
C--CHECK FOR "BTN" FILE TYPE.
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'BTN') THEN
        IBTN=1 
        FILSTAT='OLD    '
        FILACT=ACTION(1)
        IF(IU.EQ.0) THEN
          IU=INBTN               
        ELSEIF(IU.GT.0) THEN
          INBTN=IU
        ENDIF         
C
        MUTDRY=0                                               !# LINE 83 BTN
        IC2DRY=0                                               !# LINE 84 BTN
        MINVOL=0                                               !# LINE 85 BTN
        IDRYBUD=0                                              !# LINE 86 BTN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INUNIT)  !# LINE 87 BTN
        IF(LINE(ISTART:ISTOP).EQ.'MUTDRY') MUTDRY=1            !# LINE 88 BTN
        IF(LINE(ISTART:ISTOP).EQ.'C2DRY') IC2DRY=1             !# LINE 89 BTN
        IF(LINE(ISTART:ISTOP).EQ.'MINVOL') MINVOL=1            !# LINE 90 BTN
        IF(LINE(ISTART:ISTOP).EQ.'DRYBUD') IDRYBUD=1           !# LINE 91 BTN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INUNIT)  !# LINE 92 BTN
        IF(LINE(ISTART:ISTOP).EQ.'MUTDRY') MUTDRY=1            !# LINE 93 BTN
        IF(LINE(ISTART:ISTOP).EQ.'C2DRY') IC2DRY=1             !# LINE 94 BTN
        IF(LINE(ISTART:ISTOP).EQ.'MINVOL') MINVOL=1            !# LINE 95 BTN
        IF(LINE(ISTART:ISTOP).EQ.'DRYBUD') IDRYBUD=1           !# LINE 96 BTN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INUNIT)  !# LINE 97 BTN
        IF(LINE(ISTART:ISTOP).EQ.'MUTDRY') MUTDRY=1            !# LINE 98 BTN
        IF(LINE(ISTART:ISTOP).EQ.'C2DRY') IC2DRY=1             !# LINE 99 BTN
        IF(LINE(ISTART:ISTOP).EQ.'MINVOL') MINVOL=1            !# LINE 100 BTN
        IF(LINE(ISTART:ISTOP).EQ.'DRYBUD') IDRYBUD=1           !# LINE 101 BTN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INUNIT)  !# LINE 102 BTN
        IF(LINE(ISTART:ISTOP).EQ.'MUTDRY') MUTDRY=1            !# LINE 103 BTN
        IF(LINE(ISTART:ISTOP).EQ.'C2DRY') IC2DRY=1             !# LINE 104 BTN
        IF(LINE(ISTART:ISTOP).EQ.'MINVOL') MINVOL=1            !# LINE 105 BTN
        IF(LINE(ISTART:ISTOP).EQ.'DRYBUD') IDRYBUD=1           !# LINE 106 BTN
C
C--CHECK FOR "FTL" FILE TYPE.
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'FTL') THEN
C
C--DECODE OPTIONAL FORMAT AND OUTPUT KEYWORDS
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INUNIT)
        CALL URWORD(LINE,LLOC,ISTART2,ISTOP2,1,N,R,IOUT,INUNIT)
        IFTL=1
        IFTLFMT=0
        FILSTAT='OLD    '
        FILACT=ACTION(1)
        FMTARG=FORM
        ACCARG=ACCESS
        IF(IU.EQ.0) THEN
          IU=INFTL
        ELSE
          INFTL=IU
        ENDIF
        IF(LINE(ISTART:ISTOP)  .EQ.'FREE' .OR.
     &   LINE(ISTART2:ISTOP2).EQ.'FREE') THEN
          IFTLFMT=1
          FMTARG='FORMATTED'
          ACCARG='SEQUENTIAL'
        ENDIF
        IF(LINE(ISTART:ISTOP)  .EQ.'PRINT' .OR.
     &     LINE(ISTART2:ISTOP2).EQ.'PRINT') THEN
          FPRT='Y'
        ENDIF
C               
C--CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'DATA(BINARY)') THEN
        FMTARG=FORM
        ACCARG=ACCESS
C
C--CHECK FOR "FORMATTED FILE TYPE.
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'DATA') THEN
        FMTARG='FORMATTED'
        ACCARG='SEQUENTIAL'                        
C
C--CHECK FOR MAJOR OPTIONS.
      ELSE                       
        DO I=1,MXTRNOP
          IF(LINE(ITYP1:ITYP2).EQ.NameTRNOP(I)) THEN
            IF(IU.EQ.0) THEN          
              if(NameTRNOP(i).EQ.'ADV') THEN
                IU=INADV
              elseif(NameTRNOP(i).EQ.'DSP') THEN
                IU=INDSP
              elseif(NameTRNOP(i).EQ.'SSM') THEN
                IU=INSSM
              elseif(NameTRNOP(i).EQ.'CTS') THEN              !# New 7-11-13
                IU=INCTS                                      !# New 7-11-13
              elseif(NameTRNOP(i).EQ.'TSO') THEN              !# New 7-11-13
                IU=INTSO                                      !# New 7-11-13
              elseif(NameTRNOP(i).EQ.'UZT') THEN                    !edm
                IU=INUZT                                            !edm
              elseif(NameTRNOP(i).EQ.'RCT') THEN
                IU=INRCT
              elseif(NameTRNOP(i).EQ.'GCG') THEN
                IU=INGCG
              elseif(NameTRNOP(i).EQ.'TOB') THEN
                IU=INTOB
              elseif(NameTRNOP(i).EQ.'HSS') THEN
                IU=INHSS
              elseif(NameTRNOP(i).EQ.'RTR') THEN              !# LINE 171 BTN
                IU=INRTR                                      !# LINE 172 BTN
              elseif(NameTRNOP(i).EQ.'LKT') THEN              !# LINE 173 BTN
                IU=INLKT                                      !# LINE 174 BTN
              elseif(NameTRNOP(i).EQ.'SFT') THEN              !# LINE 175 BTN
                IU=INSFT                                      !# LINE 176 BTN
              else
                WRITE(*,20) LINE(ITYP1:ITYP2)
   20           FORMAT(1X,'UNDEFINED UNIT # FOR FILE TYPE: ',A)  
                CALL USTOP(' ')  
              endif  
            ENDIF  
            iUnitTRNOP(I)=IU
            FILSTAT='OLD    '
            FILACT=ACTION(1)
            GO TO 30
          ENDIF
        ENDDO     
        WRITE(*,21) LINE(ITYP1:ITYP2)
   21   FORMAT(1X,'ILLEGAL FILE TYPE IN NAME FILE: ',A)
        CALL USTOP(' ')
   30   CONTINUE
      ENDIF      
C
C--WRITE THE FILE NAME IF THE FILE IS NOT THE
C--LISTING FILE.  THEN OPEN THE FILE.
      INQUIRE(UNIT=IU,OPENED=LOP)
      IF(LOP) CLOSE(UNIT=IU)
      IF(ILIST.NE.0) WRITE(IOUT,36) LINE(INAM1:INAM2),
     &     LINE(ITYP1:ITYP2),IU
   36 FORMAT(1X,/1X,'OPENING ',A,/
     &     1X,'FILE TYPE:',A,'   UNIT',I4)
      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),STATUS=FILSTAT,
     &     FORM=FMTARG,ACCESS=ACCARG,ACTION=FILACT)
C
C--IF THE OPENED FILE IS THE LISTING FILE, WRITE ITS NAME.
C--GO BACK AND READ NEXT RECORD.
      IF(ILIST.EQ.0) WRITE(IOUT,37) LINE(INAM1:INAM2),IU
   37 FORMAT(1X,'LISTING FILE: ',A,/25X,'UNIT',I4)
      ILIST=1
      GOTO 10
C
C--END OF NAME FILE.  RETURN PROVIDED THAT LISTING FILE,
C--FTL and BTN FILES HAVE BEEN OPENED.
 1000 IF(ILIST.EQ.0) THEN
        WRITE(*,1001)
        CALL USTOP(' ')
      ELSEIF(IFTL.EQ.0) THEN
        WRITE(IOUT,1002)
        CALL USTOP(' ')
      ELSEIF(IBTN.EQ.0) THEN
        WRITE(IOUT,1003)
        CALL USTOP(' ')
      ENDIF
 1001 FORMAT(1X,'NAME FILE IS EMPTY.')
 1002 FORMAT(1X,'Flow-Transport Link FILE HAS NOT BEEN OPENED.')
 1003 FORMAT(1X,'BTN PACKAGE FILE HAS NOT BEEN OPENED.')
c
      RETURN
      END
C
C
      SUBROUTINE BTN5AR(IN)
C **********************************************************************
C THIS SUBROUTINE READS AND PREPARES INPUT DATA RELEVANT TO THE ENTIRE
C SIMULATION.
C***********************************************************************
C last modified: 02-15-2005
C
      USE UZTVARS, ONLY: PRSITYSAV
	USE MT3DMS_MODULE
      USE MIN_SAT                                              !# LINE 241 BTN
      USE RCTMOD, ONLY: IREACTION                              !# LINE 439 BTN
      IMPLICIT  NONE
      INTEGER   IN,N,J,I,K,IP1,IERR,INDEX,ISTART,ISTOP,
     &          LLOC,LLOCSAVE
      REAL      ZZ,TMP,CDRY,R
      LOGICAL   UNIFOR
      CHARACTER ANAME*24,FLNAME*50,FINDEX*30,HEADNG*80,Line*80
      DIMENSION HEADNG(2)
C
C--ALLOCATE SCALAR VARIABLES
      ALLOCATE(NCOL)
      ALLOCATE(NROW)
      ALLOCATE(NLAY)
      ALLOCATE(NPER)
      ALLOCATE(NCOMP)
      ALLOCATE(MCOMP)
      ALLOCATE(NODES)
      ALLOCATE(NSTP)
      ALLOCATE(MXSTRN)
      ALLOCATE(iSSTrans)
      ALLOCATE(MXPART)
      ALLOCATE(DT0)
      ALLOCATE(TTSMULT)
      ALLOCATE(TTSMAX)
      ALLOCATE(TUNIT)
      ALLOCATE(LUNIT)
      ALLOCATE(MUNIT)
	ALLOCATE(ISOTHM)
	ALLOCATE(HT1)
	ALLOCATE(HT2)
	ALLOCATE(DTRANS)
	ALLOCATE(DELT)
	ALLOCATE(TIME1)
	ALLOCATE(TIME2)
      ALLOCATE(CINACT)
      ALLOCATE(THKMIN)
      ALLOCATE(RFMIN)
      ALLOCATE(IMPSOL)
      ALLOCATE(DTRACK)
      ALLOCATE(DTRACK2)
      ALLOCATE(DTSSM)
      ALLOCATE(DTDISP)
      ALLOCATE(DTRCT)
      ALLOCATE(MIXELM)
      ALLOCATE(ISPD)
      ALLOCATE(PERCEL)
      ALLOCATE(IFMTCN)
      ALLOCATE(IFMTNP)
      ALLOCATE(IFMTRF)
      ALLOCATE(IFMTDP)
      ALLOCATE(NPS)
      ALLOCATE(NPRS)
      ALLOCATE(NPRMAS)
      ALLOCATE(SAVUCN)
      ALLOCATE(SAVCBM)
      ALLOCATE(CHKMAS)
      ALLOCATE(PRTOUT)
      ALLOCATE(UPDLHS)
      ALLOCATE(NOBS)
      ALLOCATE(NPROBS)
      ALLOCATE(HORIGN)
      ALLOCATE(XMAX)
      ALLOCATE(YMAX)
      ALLOCATE(ZMAX)
      ALLOCATE(UNIDX)
      ALLOCATE(UNIDY)
      ALLOCATE(UNIDZ)
C
C--INITIALIZE VARIABLES THAT DEPEND ON OTHER PACKAGES
	ISOTHM=0
      IF(FPRT.EQ.' ') FPRT='N'
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,7) IN
   7  FORMAT(1X,'BTN5 -- BASIC TRANSPORT PACKAGE,',
     & ' VERSION 5, FEBRUARY 2010, INPUT READ FROM UNIT',I3)
C
C--READ AND PRINT HEADING
      READ(IN,'(A80)') (HEADNG(I),I=1,2)
      WRITE(IOUT,10)
      WRITE(IOUT,15) (HEADNG(I),I=1,2)
      WRITE(IOUT,10)
   10 FORMAT(1X,' ----- ')
   15 FORMAT(1X,'| M T | ',A80/1X,'| 3 D | ',A80)
C
C--IDENTIFY PRESENCE OF KEYWORDS, CODE DOES NOT REQUIRE A SPECIFIC ORDER,
C  KEYWORDS MAY BE ENTERED IN ANY ORDER.
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'MST') THEN
        DOMINSAT=.TRUE.
        WRITE(IOUT,*)
        WRITE(IOUT,'(A)')'*** MST OPTION HAS BEEN ACTIVATED BY ',
     &                   'KEYWORD ON THE FIRST LINE OF THE BTN PACKAGE'
        WRITE(IOUT,*)
        LLOCSAVE=LLOC
      ELSEIF(LINE(ISTART:ISTOP).EQ.'DRY') THEN
        DRYON=.TRUE.
        WRITE(IOUT,*)
        WRITE(IOUT,'(A)')'*** DRY OPTION HAS BEEN ACTIVATED BY ',
     &                   'KEYWORD ON THE FIRST LINE OF THE BTN PACKAGE'
        WRITE(IOUT,*)
        LLOCSAVE=LLOC
      ELSE
        WRITE(IOUT,*)
        WRITE(IOUT,'(A)')'NEITHER MST OR DRY OPTIONS SET'
        WRITE(IOUT,*)
        BACKSPACE(IN)
        GOTO 16
      ENDIF
C
C--CHECK FOR SECOND KEYWORD
      IF(DOMINSAT.OR.DRYON) THEN
        LLOC=LLOCSAVE          
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'MST') THEN
          DOMINSAT=.TRUE.
          WRITE(IOUT,*)
          WRITE(IOUT,'(A)')'*** MST OPTION HAS BEEN ACTIVATED BY ',
     &                   'KEYWORD ON THE FIRST LINE OF THE BTN PACKAGE'
          WRITE(IOUT,*)
        ELSEIF(LINE(ISTART:ISTOP).EQ.'DRY') THEN
          DRYON=.TRUE.
          WRITE(IOUT,*)
        WRITE(IOUT,'(A)')'*** DRY OPTION HAS BEEN ACTIVATED BY ',
     &                   'KEYWORD ON THE FIRST LINE OF THE BTN PACKAGE'
          WRITE(IOUT,*)
        ELSE
          GOTO 16
        ENDIF
      ENDIF
C
C--IF MST IS TRUE AND DRYON IS FALSE THEN IDRYBUD MUST BE 1; 
C  THIS TERM MUST BE TREATED AS SINK
   16 IF(DOMINSAT.EQ..FALSE.) DRYON=.FALSE.
      IF(DOMINSAT.EQ..TRUE. .AND. DRYON.EQ..FALSE.) THEN
        IDRYBUD=1
      ENDIF
      IF(DOMINSAT.EQ..FALSE.) IDRYBUD=0
C
C--READ AND PRINT NO. OF LAYERS, ROWS, COLUMNS, AND STRESS PERIODS,
C--COMPONENTS
      IATS=0                                                   !# LINE 261 BTN
      READ(IN,'(7I10)',ERR=25,IOSTAT=IERR)                     !# LINE 262 BTN
     &                    NLAY,NROW,NCOL,NPER,NCOMP,MCOMP,IATS !# LINE 263 BTN
      IF(NCOMP.LT.1) NCOMP=1
      IF(MCOMP.LT.1) MCOMP=1
   25 IF(IERR.NE.0) THEN
        BACKSPACE(IN)
        READ(IN,'(4I10)') NLAY,NROW,NCOL,NPER
        NCOMP=1
        MCOMP=1
      ENDIF
      WRITE(IOUT,700) NLAY,NROW,NCOL,NPER,NCOMP,MCOMP
  700 FORMAT(1X,'THE TRANSPORT MODEL CONSISTS OF ',I5,' LAYER(S)',I5,
     & ' ROW(S)',I5,' COLUMN(S)',
     & /1X,'NUMBER OF STRESS PERIOD(S) FOR TRANSPORT SIMULATION =',I5,
     & /1X,'NUMBER OF ALL COMPONENTS INCLUDED IN SIMULATION =',I5,
     & /1X,'NUMBER OF MOBILE COMPONENTS INCLUDED IN SIMULATION =',I5)
C
C-----WRITE MESSAGE IF ATS OPTION IS SELECTED                     !# LINE 285 BTN
      IF(IATS.GE.1.AND.iUnitTRNOP(14).GT.0) THEN                  !# LINE 286 BTN
        WRITE(IOUT,1001) iUnitTRNOP(14)                           !# LINE 287 BTN
        READ(iUnitTRNOP(14),*)                                    !# LINE 288 BTN
      ENDIF                                                       !# LINE 289 BTN
      IF(IATS.GE.1.AND.iUnitTRNOP(14).LE.0) THEN                  !# LINE 290 BTN
        IATS=0                                                    !# LINE 291 BTN
        WRITE(IOUT,1002) iUnitTRNOP(14)                           !# LINE 292 BTN
      ENDIF                                                       !# LINE 293 BTN
 1001 FORMAT(1X,'AUTO-TIME-STEPPING(IATS) INFORMATION READ FROM', !# LINE 294 BTN
     & ' EXTERNAL FILE ON UNIT ',I5)                              !# LINE 295 BTN
 1002 FORMAT(1X,'***AUTO-TIME-STEPPING(IATS) FLAG DEACTIVATED***',!# LINE 296 BTN
     & ' ENTER A VALID UNIT NUMBER IN NAM FILE',I5)               !# LINE 297 BTN
C
C--READ AND PRINT UNITS FOR TIME, LENGTH AND MASS TO BE USED
      READ(IN,'(3A4)') TUNIT,LUNIT,MUNIT
      WRITE(IOUT,750) TUNIT,LUNIT,MUNIT
  750 FORMAT(1X,'UNIT FOR TIME IS ',A4,';',2X,'UNIT FOR LENGTH IS ',
     & A4,';',2X,'UNIT FOR MASS IS ',A4)
C
C--IGNORE TRANSPORT OPTIONS INPUT WHICH ARE DEFINED THROUGH NameFile     
      READ(IN,'(A)',ERR=755) Line
  755 WRITE(IOUT,760)
      DO I=1,MXTRNOP
        IF(iUnitTRNOP(I).GT.0) 
     &   WRITE(IOUT,770) NameTRNOP(I),iUnitTRNOP(I)
      ENDDO     
      WRITE(IOUT,1024)     
  760 FORMAT(1X,'OPTIONAL PACKAGES INCLUDED IN CURRENT SIMULATION:')
  770 FORMAT(1X,' o ',A, '  ON UNIT',I3)
 1024 FORMAT(1X)
C
C--WRITE STATUS OF MST AND DRY FLAGS                              !# LINE 313 BTN
      IF(DOMINSAT)                                                !# LINE 314 BTN
     1  WRITE(IOUT,*) 'MST OPTION - TO LET MASS ENTER DRY CELLS'  !# LINE 315 BTN
      IF(DRYON)                                                   !# LINE 316 BTN
     1  WRITE(IOUT,*) 'DRY OPTION - TO LET MASS ENTER FROM DRY CELLS' !# LINE 317 BTN
C
C--GET TOTAL NUMBER OF MODEL NODES
      NODES=NCOL*NROW*NLAY
C
C--ALLOCATE AND INITIALIZE
      ALLOCATE(LAYCON(NLAY))
      ALLOCATE(ICBUND(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(DELR(NCOL))
      ALLOCATE(DELC(NROW))
      ALLOCATE(DZ(NCOL,NROW,NLAY))
      ALLOCATE(PRSITY(NCOL,NROW,NLAY))
      ALLOCATE(HTOP(NCOL,NROW))
      ALLOCATE(XBC(NCOL))
      ALLOCATE(YBC(NROW))
      ALLOCATE(ZBC(NCOL,NROW,NLAY))
      ALLOCATE(CNEW(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(COLD(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(QX(NCOL,NROW,NLAY))
      ALLOCATE(QY(NCOL,NROW,NLAY))
      ALLOCATE(QZ(NCOL,NROW,NLAY))
      ALLOCATE(QSTO(NCOL,NROW,NLAY))
      ALLOCATE(DH(NCOL,NROW,NLAY))
      ALLOCATE(CWGT(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(CADV(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(RETA(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(SRCONC(NCOL,NROW,NLAY,NCOMP))
      ALLOCATE(BUFF(NCOL,NROW,NLAY))
      ALLOCATE(TSLNGH(MXSTP))
      IF (iUnitTRNOP(4).GT.0) THEN
        ALLOCATE(RHOB(NCOL,NROW,NLAY))
        ALLOCATE(PRSITY2(NCOL,NROW,NLAY))
        ALLOCATE(RETA2(NCOL,NROW,NLAY,NCOMP))
      ELSE
        ALLOCATE(RHOB(1,1,1))
        ALLOCATE(PRSITY2(1,1,1))
        ALLOCATE(RETA2(1,1,1,1))
      ENDIF
      ALLOCATE(NPINS(NCOMP))
      ALLOCATE(NRC(NCOMP))
      ALLOCATE(TMASIN(NCOMP))
      ALLOCATE(TMASOT(NCOMP))
      ALLOCATE(ERROR(NCOMP))
      ALLOCATE(ERROR2(NCOMP))
      ALLOCATE(TMASIO(122,2,NCOMP))
      ALLOCATE(RMASIO(122,2,NCOMP))
      ALLOCATE(TMASS(4,3,NCOMP))
      ALLOCATE(IABSMIN)
      LAYCON=0
      ICBUND=0
      DELR=0.
      DELC=0.
      DZ=0.
      PRSITY=0.
      HTOP=0.
      XBC=0.
      YBC=0.
      ZBC=0.
      CNEW=0.
      COLD=0.
      QX=0.
      QY=0.
      QZ=0.
      QSTO=0.
      DH=0.
      CWGT=0.
      CADV=0.
      RETA=0.
      SRCONC=0.
      BUFF=0.
      TSLNGH=0.
      RHOB=0.
      PRSITY2=0.
      RETA2=0.
      NPINS=0
      NRC=0
      TMASIN=0.
      TMASOT=0.
      ERROR=0.
      ERROR2=0.
      TMASIO=0.
      RMASIO=0.
      TMASS=0.
C
C--READ AND ECHO LAYER TYPE CODES
      READ(IN,'(40I2)') (LAYCON(K),K=1,NLAY)
      WRITE(IOUT,1000)
 1000 FORMAT(1X,'LAYER NUMBER  AQUIFER TYPE',
     &      /1X,'------------  ------------')
      DO K=1,NLAY
        WRITE(IOUT,1010) K,LAYCON(K)
      ENDDO
 1010 FORMAT(1X,4X,I3,10X,I3)
C
C--CALL RARRAY TO READ IN CELL WIDTH ALONG ROWS
      ANAME='WIDTH ALONG ROWS (DELR)'
      CALL RARRAY(DELR(1:NCOL),ANAME,1,NCOL,0,IN,IOUT)
C
C--CHECK WHETHER ELEMENTS OF DELR ARE UNIFROM
      UNIDX=UNIFOR(DELR(1:NCOL),NCOL,1,1)
C
C--CALL RARRAY TO READ IN CELL WIDTH ALONG COLUMNS
      ANAME='WIDTH ALONG COLS (DELC)'
      CALL RARRAY(DELC(1:NROW),ANAME,1,NROW,0,IN,IOUT)
C
C--CHECK WHETHER ELEMENTS OF DELC ARE UNIFROM
      UNIDY=UNIFOR(DELC(1:NROW),NROW,1,1)
C
C--CALL RARRAY TO READ IN TOP ELEVATION OF 1ST LAYER
      ANAME='TOP ELEV. OF 1ST LAYER'
      CALL RARRAY(HTOP,ANAME,NROW,NCOL,0,IN,IOUT)
C
C--CALL RARRAY TO READ IN THICKNESS ONE LAYER AT A TIME
      ANAME='CELL THICKNESS (DZ)'
      DO K=1,NLAY
        CALL RARRAY(DZ(1:NCOL,1:NROW,K),ANAME,NROW,NCOL,K,IN,IOUT)
      ENDDO
C
C--CHECK WHETHER VERTICAL DISCRTIZATION IS HORIZONTAL
      UNIDZ=UNIFOR(HTOP,NCOL,NROW,1)
     & .AND.UNIFOR(DZ,NCOL,NROW,NLAY)
C
C--CALL RARRAY TO READ IN POROSITY ONE LAYER AT A TIME
      ANAME='POROSITY'
      DO K=1,NLAY
        CALL RARRAY(PRSITY(1:NCOL,1:NROW,K),ANAME,NROW,NCOL,K,IN,IOUT)
      ENDDO
C                                                                   !edm
      IF(iUnitTRNOP(7).GT.0) THEN                                   !edm
C                                                                   !edm
C--IMMEDIATELY POINT PRSITYSAV TO PRSITY SO THAT THE ORIGINAL       !edm
C--BTN PRSITY IS RETAINED FOR THE REMAINDER OF CODE EXECUTION       !edm
        PRSITYSAV=>PRSITY                                           !edm
      ENDIF                                                         !edm
C
C--CALL IARRAY TO READ IN CONCENTRATION BOUNDARY ARRAY
      ANAME='CONCN. BOUNDARY ARRAY'
      DO K=1,NLAY
        CALL IARRAY(ICBUND(1:NCOL,1:NROW,K,1),ANAME,NROW,NCOL,K,IN,IOUT)
      ENDDO
C
C--CALL RARRAY TO READ IN INITIAL CONCENTRATION
      ANAME='INITIAL CONC.: COMP. NO.'
      DO INDEX=1,NCOMP
        WRITE(ANAME(22:24),'(I3.2)') INDEX
        DO K=1,NLAY
          CALL RARRAY(COLD(1:NCOL,1:NROW,K,INDEX),ANAME,NROW,NCOL,
     &                     K,IN,IOUT)
        ENDDO
      ENDDO
C
C--ALLOCATE SPACE FOR REACTION VARIABLES                       !# LINE 521 BTN
!      IF(IREACTION.EQ.2) THEN                                 !# LINE 522 BTN
!        ALLOCATE(INIC(NCOL,NROW,NLAY,NCOMP))                  !# LINE 523 BTN
!        INIC=COLD                                             !# LINE 524 BTN
!      ENDIF                                                   !# LINE 525 BTN
C                                                              !# LINE 526 BTN
C--READ AND ECHO CINACT,THKMIN
      READ(IN,'(2F10.0)',ERR=50,IOSTAT=IERR) CINACT,THKMIN
CVSB      IF(THKMIN.LT.0) THKMIN=0.                            
C.....READ NEGATIVE THKMIN AS ABSOLUTE VALUE OF MINTHK         !# LINE 530 BTN
      IABSMIN=0                                                !# LINE 531 BTN
      IF(THKMIN.LT.0) THEN                                     !# LINE 532 BTN
        IABSMIN=1                                              !# LINE 533 BTN
        THKMIN=ABS(THKMIN)                                     !# LINE 534 BTN
      ENDIF                                                    !# LINE 535 BTN
C
   50 IF(IERR.NE.0) THEN
        BACKSPACE (IN)
        READ(IN,'(F10.0)') CINACT
        THKMIN=0.
      ENDIF
C                                                              !# LINE 541 BTN
      IF(IABSMIN.EQ.1) THEN                                    !# LINE 542 BTN
        WRITE(IOUT,1021) CINACT,THKMIN                         !# LINE 543 BTN
      ELSE                                                     !# LINE 544 BTN
        WRITE(IOUT,1020) CINACT,THKMIN
      ENDIF                                                    !# LINE 546 BTN
      IF(IABSMIN.EQ.0) THEN                                    !# LINE 548 BTN
        IF(THKMIN.GT.0.05) THEN
          WRITE(IOUT,1022)
          THKMIN=0.01
        ENDIF
      ENDIF                                                    !# LINE 552 BTN
 1020 FORMAT(/1X,'VALUE INDICATING INACTIVE CONCENTRATION CELLS = ',
     & G15.7/1X,'MINIMUM SATURATED THICKNESS [THKMIN] ',
     & 'ALLOWED =',F8.4,' OF TOTAL CELL THICKNESS')
 1021 FORMAT(/1X,'VALUE INDICATING INACTIVE CONCENTRATION CELLS = ', !# LINE 557
     & G15.7/1X,'ABSOLUTE MINIMUM SATURATED THICKNESS [THKMIN] ',    !# LINE 558
     & 'ALLOWED =',F8.4)                                             !# LINE 559
 1022 FORMAT(1X,'WARNING: [THKMIN] MUST BE < OR = 0.05;',
     & /10X,'RESET TO DEFAULT OF 0.01 OR 1% OF TOTAL CELL THICKNESS')
C
C--READ AND ECHO OUTPUT CONTROL OPTIONS
      READ(IN,'(4I10,L10)') IFMTCN,IFMTNP,IFMTRF,IFMTDP,SAVUCN
      SAVCBM=.FALSE.
      WRITE(IOUT,1025)
C
      IF(IFMTCN.NE.0) WRITE(IOUT,1030) IFMTCN
      IF(IFMTCN.EQ.0) WRITE(IOUT,1032)
 1025 FORMAT(//1X,'OUTPUT CONTROL OPTIONS'/1X,22('-'))
 1030 FORMAT(/1X,'PRINT CELL CONCENTRATION USING FORMAT CODE:',I5)
 1032 FORMAT(/1X,'DO NOT PRINT CELL CONCENTRATION')
C
      IF(IFMTNP.NE.0) WRITE(IOUT,1034) IFMTNP
      IF(IFMTNP.EQ.0) WRITE(IOUT,1036)
 1034 FORMAT(1X,'PRINT PARTICLE NUMBER IN EACH CELL',
     &          ' USING FORMAT CODE:',I5)
 1036 FORMAT(1X,'DO NOT PRINT PARTICLE NUMBER IN EACH CELL')
C
      IF(IFMTNP.NE.0) WRITE(IOUT,1038) IFMTRF
      IF(IFMTNP.EQ.0) WRITE(IOUT,1040)
 1038 FORMAT(1X,'PRINT RETARDATION FACTOR USING FORMAT CODE:',I5)
 1040 FORMAT(1X,'DO NOT PRINT RETARDATION FACTOR')
C
      IF(IFMTDP.NE.0) WRITE(IOUT,1042) IFMTDP
      IF(IFMTDP.EQ.0) WRITE(IOUT,1044)
 1042 FORMAT(1X,'PRINT DISPERSION COEFFICIENT USING FORMAT CODE:',I5)
 1044 FORMAT(1X,'DO NOT PRINT DISPERSION COEFFICIENT')
C
      IF(SAVUCN) THEN
        WRITE(IOUT,1046) IUCN+1
        FLNAME='MT3Dnnn.UCN'
        DO INDEX=1,NCOMP
          WRITE(FLNAME(5:7),'(I3.3)') INDEX
          CALL OPENFL(-(IUCN+INDEX),0,FLNAME,1,FINDEX)
        ENDDO
        IF(ISOTHM.GT.0) THEN
          WRITE(IOUT,2046) IUCN2+1
          FLNAME='MT3DnnnS.UCN'
          DO INDEX=1,NCOMP
            WRITE(FLNAME(5:7),'(I3.3)') INDEX
            CALL OPENFL(-(IUCN2+INDEX),0,FLNAME,1,FINDEX)
          ENDDO
        ENDIF
      ELSE
        WRITE(IOUT,1047)
      ENDIF
 1046 FORMAT(1X,'SAVE DISSOLVED PHASE CONCENTRATIONS ',
     & 'IN UNFORMATTED FILES [MT3Dnnn.UCN]'/1X,' FOR EACH SPECIES ',
     & 'ON UNITS ',I3,' AND ABOVE')
 2046 FORMAT(1X,'SAVE SORBED/IMMOBILE PHASE CONCENTRATIONS ',
     & 'IN UNFORMATTED FILES [MT3DnnnS.UCN]'/1X,' FOR EACH SPECIES ',
     & 'ON UNITS ',I3,' AND ABOVE, ',
     & 'IF SORPTION/MASS TRANSFER SIMULATED')
 1047 FORMAT(1X,'DO NOT SAVE CONCENTRATIONS IN UNFORMATTED FILES')
C
C--READ NUMBER OF TIMES AT WHICH SIMULATION RESULTS SHOULD BE SAVED
C--IN STANDARD OUTPUT FILE OR RECORDED IN UNFORMATTED FILE
      READ(IN,'(I10)') NPRS
      IF(NPRS.LT.0) THEN
        WRITE(IOUT,1050) -NPRS
      ELSE
        WRITE(IOUT,1052) NPRS
      ENDIF
C--ALLOCATE TIMPRS
      IF(NPRS.GT.0) THEN
        ALLOCATE(TIMPRS(NPRS))
      ELSE
        ALLOCATE(TIMPRS(1))
      ENDIF
      IF(NPRS.GT.0) THEN
        READ(IN,'(8F10.0)') (TIMPRS(I),I=1,NPRS)
C
C--MAKE SURE ELEMENTS IN ARRAY [TIMPRS] ARE MONOTONICALLY INCREASING
        DO I=1,NPRS-1
          DO IP1=I+1,NPRS
            IF(TIMPRS(I).GT.TIMPRS(IP1)) THEN
              TMP=TIMPRS(I)
              TIMPRS(I)=TIMPRS(IP1)
              TIMPRS(IP1)=TMP
            ENDIF
          ENDDO
        ENDDO
        WRITE(IOUT,1055) (TIMPRS(I),I=1,NPRS)
      ENDIF
 1050 FORMAT(/1X,'SIMULATION RESULTS ARE SAVED EVERY ',I3,
     & ' TRANSPORT STEP(S)')
 1052 FORMAT(/1X,'NUMBER OF TIMES AT WHICH SIMULATION RESULTS',
     &' ARE SAVED =',I5)
 1055 FORMAT(1X,'TOTAL ELAPSED TIMES AT WHICH SIMULATION RESULTS ',
     & 'ARE SAVED: ',100(/1X,8G13.5))
C
C--READ NUMBER OF OBSERVATION POINTS
      READ(IN,'(2I10)',ERR=100,IOSTAT=IERR) NOBS,NPROBS
      IF(NPROBS.LT.1) NPROBS=1
  100 IF(IERR.NE.0) THEN
        BACKSPACE (IN)
        READ(IN,'(I10)') NOBS
        NPROBS=1
      ENDIF
      WRITE(IOUT,1056) NOBS
      IF(NOBS.GT.0) THEN
        ALLOCATE(LOCOBS(3,NOBS))
      ELSE
        ALLOCATE(LOCOBS(3,1))
      ENDIF
      IF(NOBS.GT.0) THEN
        WRITE(IOUT,1062) IOBS+1,NPROBS
        WRITE(IOUT,1060)
        DO N=1,NOBS
          READ(IN,'(3I10)') (LOCOBS(I,N),I=1,3)
          WRITE(IOUT,'(I4,4X,3(I5,2X))') N,(LOCOBS(I,N),I=1,3)
        ENDDO
        FLNAME='MT3Dnnn.OBS'
        DO INDEX=1,NCOMP
          WRITE(FLNAME(5:7),'(I3.3)') INDEX
          CALL OPENFL(IOBS+INDEX,0,FLNAME,1,FINDEX)
          WRITE(IOBS+INDEX,1063) ((LOCOBS(I,N),I=1,3),N=1,NOBS)
        ENDDO
      ENDIF
 1056 FORMAT(/1X,'NUMBER OF OBSERVATION POINTS =',I5)
 1060 FORMAT(1X,'LOCATION OF OBSERVATION POINTS'/1X,30('.')
     &      /1X,'NUMBER  LAYER   ROW   COLUMN')
 1062 FORMAT(1X,'SAVE CONCENTRATIONS AT OBSERVATION POINTS IN FILES ',
     & '[MT3Dnnn.OBS]'/1X,' FOR EACH SPECIES ',
     & 'ON UNITS ',I3,' AND ABOVE, EVERY',I3,' TRANSPORT STEPS')
 1063 FORMAT(1X,' STEP   TOTAL TIME',
     & '             LOCATION OF OBSERVATION POINTS (K,I,J)'
     & /1X,17X,16(1X,3I4,1X)/(1X,17X,16(1X,3I4,1X)))
C
C--READ AND ECHO LOGICAL FLAG CHKMAS
      READ(IN,'(L10,I10)',ERR=105,IOSTAT=IERR) CHKMAS,NPRMAS
      IF(NPRMAS.LT.1) NPRMAS=1
  105 IF(IERR.NE.0) THEN
        BACKSPACE (IN)
        READ(IN,'(L10)') CHKMAS
        NPRMAS=1
      ENDIF
      IF(CHKMAS) THEN
        WRITE(IOUT,1064) IMAS+1,NPRMAS
        FLNAME='MT3Dnnn.MAS'
        DO INDEX=1,NCOMP
          WRITE(FLNAME(5:7),'(I3.3)') INDEX
          CALL OPENFL(IMAS+INDEX,0,FLNAME,1,FINDEX)
          WRITE(IMAS+INDEX,1066)
          WRITE(IMAS+INDEX,1068) TUNIT,MUNIT,MUNIT,MUNIT,MUNIT
        ENDDO
        IF(DOMINSAT) THEN                                      !# LINE 710 BTN
C.......CREATE ANOTHER FILE SIMILAR TO MAS - BUT TO STORE CELL-BY-CELL AND MASS-TO-DRY  !# LINE 711 BTN
          WRITE(IOUT,2064) IMAS+1,NPRMAS                       !# LINE 712 BTN
          FLNAME='MT3Dnnn.DRY'                                 !# LINE 713 BTN
          DO INDEX=1,NCOMP                                     !# LINE 714 BTN
            WRITE(FLNAME(5:7),'(I3.3)') INDEX                  !# LINE 715 BTN
            CALL OPENFL(IMAS+NCOMP+INDEX,0,FLNAME,1,FINDEX)    !# LINE 716 BTN
            WRITE(IMAS+NCOMP+INDEX,2066)                       !# LINE 717 BTN
            WRITE(IMAS+NCOMP+INDEX,2068)                       !# LINE 718 BTN
          ENDDO                                                !# LINE 719 BTN
        ENDIF                                                  !# LINE 720 BTN
      ELSE
        WRITE(IOUT,1065)
      ENDIF
 1064 FORMAT(/1X,'SAVE ONE-LINE SUMMARY OF MASS BUDGETS IN FILES ',
     & '[MT3Dnnn.MAS]'/1X,' FOR EACH SPECIES ',
     & 'ON UNITS ',I3,' AND ABOVE, EVERY',I3,' TRANSPORT STEPS')
 1065 FORMAT(/1X,'DO NOT SAVE ONE-LINE SUMMARY OF MASS BUDGETS')
 1066 FORMAT(1X,'     TIME       TOTAL IN      TOTAL OUT      SOURCES',
     & '        SINKS      NET MASS FROM  TOTAL MASS',
     & '        DISCREPANCY(%)')
 1068 FORMAT(1X,5(4X,'(',A4,')',4X),
     & ' FLUID-STORAGE  IN AQUIFER  (TOTAL IN-OUT)  (ALTERNATIVE)')
 2064 FORMAT(/1X,'SAVE ONE-LINE SUMMARY OF CELL-BY-CELL MASS IN  ',      !# LINE 733 BTN
     & '[MT3Dnnn.DRY]'/1X,' FOR EACH SPECIES ',                          !# LINE 734 BTN
     & 'ON UNITS ',I3,' AND ABOVE, EVERY',I3,' TRANSPORT STEPS')         !# LINE 735 BTN
 2066 FORMAT(1X,'     TIME       MASS INTO     MASS FROM     MASS INT',  !# LINE 736 BTN
     & 'O     MASS FROM     MASS INTO     MASS FROM ',                   !# LINE 737 BTN
     & '    TOTAL MASS        ')                                         !# LINE 738 BTN
 2068 FORMAT(1X,'                DRY CELLS     DRY CELLS    ACTIVE CE',  !# LINE 739 BTN
     & 'LLS  ACTIVE CELLS  CONSTANT HD   CONSTANT HD',                   !# LINE 740 BTN
     & '    IN AQUIFER        ')                                         !# LINE 741 BTN
C
C--SAVE MODEL GRID CONFIGURATION IN FILE [MT3D.CNF]
C--FOR USE WITH UNFORMATTED CONCENTRATION FILE BY POST-PROCESSOR
      IF(SAVUCN) THEN
        CDRY=CINACT
        FLNAME='MT3D.CNF'
        CALL OPENFL(ICNF,0,FLNAME,1,FINDEX)
        WRITE(ICNF,*) NLAY,NROW,NCOL
        WRITE(ICNF,*) (DELR(J),J=1,NCOL)
        WRITE(ICNF,*) (DELC(I),I=1,NROW)
        WRITE(ICNF,*) ((HTOP(J,I),J=1,NCOL),I=1,NROW)
        WRITE(ICNF,*) (((DZ(J,I,K),J=1,NCOL),I=1,NROW),K=1,NLAY)
        WRITE(ICNF,*) CINACT,CDRY
        CLOSE(ICNF)
      ENDIF
C
C--PROCESS INPUT DATA
C  ==================
C
C--ASSIGN SHARED ICBUND ARRAY TO ALL SPECIES

      DO INDEX=2,NCOMP
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              ICBUND(J,I,K,INDEX)=ICBUND(J,I,K,1)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--ASSIGN CINACT TO INACTIVE CELLS AND COPY COLD TO CNEW
      DO INDEX=1,NCOMP
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,INDEX).EQ.0) COLD(J,I,K,INDEX)=CINACT
              CNEW(J,I,K,INDEX)=COLD(J,I,K,INDEX)
              CADV(J,I,K,INDEX)=COLD(J,I,K,INDEX)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--SET PRSITY=0 IN CELLS WHERE ICBUND=0
C--AND ENSURE ICBUND=0 IF POROSITY IS ZERO
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,1).EQ.0) THEN
              PRSITY(J,I,K)=0
            ELSEIF(PRSITY(J,I,K).EQ.0) THEN
              DO INDEX=1,NCOMP
                ICBUND(J,I,K,INDEX)=0
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--CALCULATE COORDINATE ARRAYS XBC, YBC AND ZBC
C--AND MAXIMUN WIDTHS ALONG ROWS, COLUMNS AND LAYERS
      HORIGN=HTOP(1,1)
      XBC(1)=DELR(1)/2.
      YBC(1)=DELC(1)/2.
      DO J=2,NCOL
        XBC(J)=XBC(J-1)+(DELR(J-1)+DELR(J))/2.
      ENDDO
      DO I=2,NROW
        YBC(I)=YBC(I-1)+(DELC(I-1)+DELC(I))/2.
      ENDDO
      XMAX=XBC(NCOL)+DELR(NCOL)/2.
      YMAX=YBC(NROW)+DELC(NROW)/2.
      ZMAX=0
      DO I=1,NROW
        DO J=1,NCOL
          ZBC(J,I,1)=DZ(J,I,1)/2.+(HORIGN-HTOP(J,I))
          ZZ=DZ(J,I,1)
          DO K=2,NLAY
            ZBC(J,I,K)=ZBC(J,I,K-1)+(DZ(J,I,K-1)+DZ(J,I,K))/2.
            ZZ=ZZ+DZ(J,I,K)
          ENDDO
          ZMAX=MAX(ZMAX,ZZ)
        ENDDO
      ENDDO
      WRITE(IOUT,1300) XMAX,YMAX,ZMAX
 1300 FORMAT(/1X,'MAXIMUM LENGTH ALONG THE X (J) AXIS =',G15.7,
     &       /1X,'MAXIMUM LENGTH ALONG THE Y (I) AXIS =',G15.7,
     &       /1X,'MAXIMUM LENGTH ALONG THE Z (K) AXIS =',G15.7)
C
C--INITIALIZE RETARDATION FACTOR ARRAY AND THE MINUMIN
      DO INDEX=1,NCOMP
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              RETA(J,I,K,INDEX)=1.
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      RFMIN=1.
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE BTN5ST(KPER)
C *****************************************************************
C THIS SUBROUTINE GETS TIMING INFORMATION FOR EACH STRESS PERIOD.
C *****************************************************************
C last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: INBTN,IOUT,NSTP,MXSTP,TSLNGH,DT0,
     &                         MXSTRN,TTSMULT,TTSMAX,TUNIT,iSSTrans,
     &                         INTSO                           !# LINE 850 BTN
      USE MIN_SAT, ONLY: IATS                                  !# LINE 856 BTN
C
      IMPLICIT  NONE
      INTEGER   IN,N,LLOC,inam1,inam2,itmp
      INTEGER   KPERF,KSTPF,KPER                               !# LINE 859 BTN
      REAL      PERLEN,TSMULT,EPSILON,R
      REAL      DELTF,PERTIMF,TOTIMF                           !# LINE 860-861 BTN
      CHARACTER Line*200
      PARAMETER (EPSILON=0.5E-6)
C
      IN=INBTN
C
C--READ AND PRINT OUT TIMING INFORMATION
      IF(IATS.GE.1) THEN                                         !# LINE 867 BTN
        NSTP=0                                                   !# LINE 868 BTN
        DO                                                       !# LINE 869 BTN
          READ(INTSO,'(2I10,3E20.10)',END=10) KPERF,KSTPF,DELTF, !# LINE 870 BTN
     1      PERTIMF,TOTIMF                                       !# LINE 871 BTN
          IF(KPERF.EQ.KPER) THEN                                 !# LINE 872 BTN
            NSTP=NSTP+1                                          !# LINE 873 BTN
            PERLEN=PERTIMF                                       !# LINE 874 BTN
            TSLNGH(NSTP)=DELTF                                   !# LINE 875 BTN
          ELSEIF(KPERF.GT.KPER) THEN                             !# LINE 876 BTN
            BACKSPACE(INTSO)                                     !# LINE 877 BTN
            EXIT                                                 !# LINE 878 BTN
          ELSE                                                   !# LINE 879 BTN
10          CONTINUE                                             !# LINE 880 BTN
            WRITE(IOUT,*) 'END OF TSO FILE'                      !# LINE 881 BTN
            EXIT                                                 !# LINE 882 BTN
          ENDIF                                                  !# LINE 883 BTN
        ENDDO                                                    !# LINE 884 BTN
        READ(IN,*)                                               !# LINE 885 BTN
        WRITE(IOUT,122) PERLEN,NSTP                              !# LINE 886 BTN
      ELSE                                                       !# LINE 887 BTN
        READ(IN,'(F10.0,I10,F10.0)') PERLEN,NSTP,TSMULT
        WRITE(IOUT,22) PERLEN,NSTP,TSMULT
      ENDIF                                                      !# LINE 890 BTN
C
C--Read an optional flag for steady-state transport simulation
      backspace (in)
      read(in,'(a)') Line
      LLOC=31
      CALL URWORD(Line,LLOC,inam1,inam2,1,ITMP,R,IOUT,IN)
      if(Line(inam1:inam2).eq.'SSTATE') then
        iSSTrans=1
        write(iout,23)
      else
        iSSTrans=0
        write(iout,24)
      endif            
C            
   22 FORMAT(/1X,'LENGTH OF CURRENT STRESS PERIOD =',G15.7,
     & /1X,'NUMBER OF TIME STEPS FOR CURRENT STRESS PERIOD =',I5,
     & /1X,'TIME STEP MULTIPLIER USED IN FLOW SOLUTION =',G15.7)
  122 FORMAT(/1X,'LENGTH OF CURRENT STRESS PERIOD =',G15.7,       !# LINE 908 BTN
     & /1X,'NUMBER OF TIME STEPS FOR CURRENT STRESS PERIOD =',I5) !# LINE 909 BTN
   23 FORMAT(/1X,'***Type of Transport Simulation is STEADY-STATE'/)
   24 FORMAT(/1X,'***Type of Transport Simulation is TRANSIENT'/)    
C   
      IF(NSTP.GT.MXSTP) THEN
        WRITE(*,25)
   25   FORMAT(/1X,'ERROR: MAXIMUM NUMBER OF TIME STEPS EXCEEDED!',
     &   /1X,'INCREASE DIMENSION OF [MXSTP] IN THE MAIN PROGRAM.')
        CALL USTOP(' ')
      ENDIF
C
C--IF [TSMULT] IS A NUMBER GREATER THAN ZERO, THE LENGTH OF
C--EACH TIME STEP FOR CURRENT STRESS PERIOD IS CALCULATED BY
C--PROGRAM USING THE GEOMETRIC PROGRESSION.
C--IF TSMULT IS A NUMBER LESS THAN OR EQUAL TO ZERO,
C--READ IN SPECIFIED LENGTH OF EACH TIME STEP.
      IF(IATS.EQ.0) THEN                                       !# LINE 925 BTN
        IF(TSMULT.LE.0) THEN
          READ(IN,'(8F10.0)') (TSLNGH(N),N=1,NSTP)
          WRITE(IOUT,30) (TSLNGH(N),N=1,NSTP)
   30     FORMAT(1X,'SPECIFIED LENGTH OF EACH TIME STEP:',/1X,8G15.7)
          GOTO 50
        ELSEIF(ABS(TSMULT-1.).LT.ABS(TSMULT+1.)*EPSILON) THEN
          TSLNGH(1)=PERLEN/FLOAT(NSTP)
        ELSE
          TSLNGH(1)=PERLEN*(1.-TSMULT)/(1.-TSMULT**NSTP)
        ENDIF
        DO N=2,NSTP
          TSLNGH(N)=TSLNGH(N-1)*TSMULT
        ENDDO
      ENDIF                                                    !# LINE 939 BTN
C
   50 CONTINUE
C
C--READ INITIAL TRANSPORT STEPSIZE AND MAXIMUM NUMBER OF STEPS
      TTSMULT=1.0
      TTSMAX=0.0
      READ(IN,'(F10.0,I10,2F10.0)',ERR=100) DT0,MXSTRN,TTSMULT,TTSMAX
      GOTO 101
  100 BACKSPACE(IN)
      READ(IN,'(F10.0,I10)') DT0,MXSTRN
  101 IF(TTSMULT.LT.1.0) TTSMULT=1.0
      WRITE(IOUT,51) DT0,TUNIT,MXSTRN,TTSMULT,TTSMAX,TUNIT
   51 FORMAT(1X,'USER-SPECIFIED TRANSPORT STEPSIZE =',G15.7,A4
     & /1X,'MAXIMUM NUMBER OF TRANSPORT STEPS ALLOWED ',
     & ' IN ONE FLOW TIME STEP =',I10,
     & /1X,'MULTIPLIER FOR SUCCESSIVE TRANSPORT STEPS ',
     & ' [USED IN IMPLICIT SCHEMES] =',F10.3,
     & /1X,'MAXIMUM TRANSPORT STEP SIZE ',
     & ' [USED IN IMPLICIT SCHEMES] =',G15.7,A4)
C
      IF(DT0.LT.0) WRITE(*,55)
   55 FORMAT(/1X,'NEGATIVE VALUE FOR INPUT VARIABLE [DT0] DETECTED; ',
     & /1X,'MODEL-CALCULATED TRANSPORT STEPSIZE REPLACED WITH [-DT0]',
     & /1X,'REGARDLESS OF STABILITY CONSTRAINTS FOR EXPLICIT SCHEMES',
     & /1X,'OR TRANSPORT STEP MULTIPLIER FOR IMPLICIT SCHEMES.')
C
      RETURN
      END
C
C
      SUBROUTINE BTN5AD(NTRANS,TIME1,TIME2,HT2,DELT,KSTP,KPER,DTRANS,
     &                  NPS)
C **********************************************************************
C THIS SUBROUTINE ADVANCES THE TRANSPORT SIMULATION ONE STEP,
C DETERMINING THE STEPSIZE TO BE USED AND WHETHER PRINTOUT IS REQUIRED
C FOR NEXT TRANSPORT STEP. IT ALSO COMPUTES TOTAL MASS IN THE AQUIFER
C AT THE FIRST TRANSPORT STEP OF EACH TRANSPORT LOOP.
C **********************************************************************
C last modified: 02-20-2010
C
      USE UZTVARS,       ONLY: SATOLD,PRSITYSAV
      USE MT3DMS_MODULE, ONLY: IOUT,MXTRNOP,iUnitTRNOP,iSSTrans,NSTP,
     &                         TIMPRS,DT0,MXSTRN,MIXELM,DTRACK,
     &                         DTRACK2,PERCEL,DTDISP,DTSSM,DTRCT,RFMIN,
     &                         NPRS,PRTOUT,NCOL,NROW,NLAY,NCOMP,ICBUND,
     &                         CNEW,COLD,CINACT,UPDLHS,IMPSOL,TTSMULT,
     &                         TTSMAX,DELR,DELC,DH,PRSITY,SRCONC,
     &                         RHOB,RETA,PRSITY2,RETA2,ISOTHM,TMASIO,
     &                         RMASIO,TMASS,
     &                         iUnitTRNOP                !edm
C
      IMPLICIT  NONE
      INTEGER   NTRANS,KSTP,NPS,INDEX,K,I,J,KPER
      REAL      TIME1,TIME2,HT2,DELT,DTOLD,CMML,CMMS,CIML,CIMS,
     &          VOLUME,EPSILON,TEMP,TTMP,DTRANS
      DIMENSION TEMP(4)
      PARAMETER (EPSILON=0.5E-6)
C
C--SAVE PREVIOUS TRANSPORT STEPSIZE
      DTOLD=DTRANS
C
C--DETERMINE STEPSIZE FOR NEXT TRANSPORT STEP
      IF(MIXELM.EQ.0) THEN   !fully implicit fd scheme
        DTRANS=DELT
        IF(iUnitTRNOP(1).GT.0) DTRANS=MIN(DTRANS,DTRACK*PERCEL*RFMIN)
        IF(DT0.GT.0) DTRANS=DT0
        IF(TTSMULT.GT.1) THEN
          TTMP=LOG10(DTRANS)+(NTRANS-1)*LOG10(TTSMULT)
          IF(TTMP.GE.10.) THEN
            DTRANS=10.**TTMP
          ELSE
            DTRANS=DTRANS*TTSMULT**(NTRANS-1)
          ENDIF
        ENDIF       
        IF(TTSMAX.GT.0.AND.DTRANS.GT.TTSMAX) DTRANS=TTSMAX
      ELSE                   !moc/mmoc/hmoc/tvd schemes        
        DTRANS=DELT
        IF(iUnitTRNOP(1).GT.0.AND.MIXELM.GT.0) THEN
          DTRANS=MIN(DTRANS,DTRACK*PERCEL*RFMIN)
        ELSEIF(iUnitTRNOP(1).GT.0.AND.MIXELM.LT.0) THEN
          DTRANS=MIN(DTRANS,DTRACK2*PERCEL*RFMIN)
        ENDIF
        IF(DT0.GT.0.AND.DT0.LT.DTRANS) DTRANS=DT0
        IF(TTSMAX.GT.0.AND.DTRANS.GT.TTSMAX) DTRANS=TTSMAX
      ENDIF  
C
C--IF DT0 NEGATIVE, USE |DT0| AS DEFAULT TRANSPORT STEPSIZE
      IF(DT0.LT.0) DTRANS=ABS(DT0)
c      
c--IF steady-state transport simulation, reset time step
      IF(iSSTrans.EQ.1) THEN
        if(MIXELM.NE.0) then
          write(iout,101)
          write(*,101) 
          call ustop(' ')
        endif
        DTRANS=DELT      
      ENDIF
  101 format(1x,'ERROR: Steady-state transport can only be simulated',
     &      /1x,'with fully implicit finite-difference [MIXELM=0].')    
C
C--UPDATES TOTAL ELASPED TIME
      TIME1=TIME2
      TIME2=TIME1+DTRANS
C
C--DETERMIN IF PRINTOUT OF SIMULATION RESULTS
C--IS NEEDED FOR NEXT STEP
      PRTOUT=.FALSE.
      IF(NTRANS.EQ.MXSTRN) THEN
        PRTOUT=.TRUE.
      ELSEIF(NPRS.LT.0) THEN
        IF(MOD(NTRANS,-NPRS).EQ.0) PRTOUT=.TRUE.
      ENDIF
C
C--IF TOTAL ELAPSED TIME AT NEXT TRANSPORT STEP EXCEEDS
C--THE LIMIT OF CURRENT TIME STEP, CUT DOWN STEPSIZE
      IF(TIME2.GE.HT2) THEN         
        TIME2=HT2
        DTRANS=TIME2-TIME1
        IF(KSTP.EQ.NSTP) PRTOUT=.TRUE.
      ENDIF
C
C--IF TOTAL ELAPSED TIME AT NEXT STEP EXCEEDS TIME AT WHICH
C--PRINTOUT IS REQUESTED, CUT DOWN STEPSIZE
      IF(NPRS.GT.0.AND.NPS.LE.NPRS) THEN
        IF(TIME2.GE.TIMPRS(NPS)) THEN
          IF(TIME2.GT.TIMPRS(NPS)) THEN
            TIME2=TIMPRS(NPS)
            DTRANS=TIME2-TIME1
          ENDIF
          PRTOUT=.TRUE.
          NPS=NPS+1
        ENDIF
      ENDIF
C
      UPDLHS=.TRUE.
      IF(ABS(DTRANS-DTOLD).LT.ABS(DTRANS+DTOLD)*EPSILON
     & .AND. NTRANS.GT.1) UPDLHS=.FALSE.
C
C--PRINT OUT AN IDENTIFYING MSGGAGE
      WRITE(*,70) NTRANS,DTRANS,TIME2
   70 FORMAT(1X,'Transport Step:',I5,3X,'Step Size:',1PG12.4,
     & ' Total Elapsed Time:',G13.5)
C
C--COPY ARRAY [CNEW] TO [COLD]
      DO INDEX=1,NCOMP
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,INDEX).EQ.0) THEN
                CNEW(J,I,K,INDEX)=CINACT
              ELSE
                COLD(J,I,K,INDEX)=CNEW(J,I,K,INDEX)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C--CLEAR RMASIO ARRAY FOR ACCUMULATING MASS IN/OUT
C--AT NEXT TRANSPORT STEP
      DO INDEX=1,NCOMP
        DO I=1,122
          RMASIO(I,1,INDEX)=0.
          RMASIO(I,2,INDEX)=0.
        ENDDO
      ENDDO
C
C--CALCAULTE TOTAL MASS IN AQUIFER AT THE FIRST TRANSPORT STEP
      IF(NTRANS.GT.1) GOTO 9999
C
C--1: MOBILE-LIQUID   (MML) PHASE
C--2: MOBILE-SORBED   (MMS) PHASE
C--3: IMMOBILE-LIQUID (IML) PHASE
C--4: IMMOBILE-SORBED (IMS) PHASE
C
      DO INDEX=1,NCOMP
        TEMP(1)=0.
        TEMP(2)=0.
        TEMP(3)=0.
        TEMP(4)=0.
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(ICBUND(J,I,K,INDEX).LE.0) CYCLE
              VOLUME=DELR(J)*DELC(I)*DH(J,I,K)
              IF(iUnitTRNOP(7).EQ.0) THEN
                CMML=COLD(J,I,K,INDEX)*PRSITY(J,I,K)*VOLUME
              ELSE
                CMML=COLD(J,I,K,INDEX)*SATOLD(J,I,K)*PRSITYSAV(J,I,K)
     &                 *VOLUME
              ENDIF
              CMMS=0.
              CIML=0.
              CIMS=0.
              IF(ISOTHM.EQ.1) THEN
                CMMS=(RETA(J,I,K,INDEX)-1.)*CMML
              ELSEIF(ISOTHM.GT.1.AND.ISOTHM.LE.4) THEN
                CMMS=SRCONC(J,I,K,INDEX)*RHOB(J,I,K)*VOLUME
              ELSEIF(ISOTHM.GT.4) THEN
                CMMS=(RETA(J,I,K,INDEX)-1.)*CMML
                CIML=PRSITY2(J,I,K)*SRCONC(J,I,K,INDEX)*VOLUME
                CIMS=(RETA2(J,I,K,INDEX)-1.)*CIML
              ENDIF
              TEMP(1)=TEMP(1)+CMML
              TEMP(2)=TEMP(2)+CMMS
              TEMP(3)=TEMP(3)+CIML
              TEMP(4)=TEMP(4)+CIMS
            ENDDO
          ENDDO
        ENDDO
C
C--STORE INITIAL MASS IF THE CURRENT TRANSPORT STEP
C--IS AT THE BEGINNING OF SIMULATION
        IF(KPER*KSTP.EQ.1) THEN
          TMASS(1,1,INDEX)=TEMP(1)
          TMASS(2,1,INDEX)=TEMP(2)
          TMASS(3,1,INDEX)=TEMP(3)
          TMASS(4,1,INDEX)=TEMP(4)
C
C--OTHERWISE DETERMINE AND ACCUMULATE CHANGE IN MASS STORAGE
C--CAUSED BY CHANGE IN SATURATED THICKNESS OF UNCONFINED AQUIFER
        ELSE
          TEMP(1)=TMASS(1,2,INDEX)-TEMP(1)
          TEMP(2)=TMASS(2,2,INDEX)-TEMP(2)
          TEMP(3)=TMASS(3,2,INDEX)-TEMP(3)
          TEMP(4)=TMASS(4,2,INDEX)-TEMP(4)
          TMASS(1,3,INDEX)=TMASS(1,3,INDEX)+TEMP(1)
          TMASS(2,3,INDEX)=TMASS(2,3,INDEX)+TEMP(2)
          TMASS(3,3,INDEX)=TMASS(3,3,INDEX)+TEMP(3)
          TMASS(4,3,INDEX)=TMASS(4,3,INDEX)+TEMP(4)
        ENDIF
      ENDDO
C
 9999 CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE BTN5SV(ICOMP)
C **************************************************************
C THIS SUBROUTINE UPDATES CELL CONCENTRATION AND MASS IN/OUT
C ACCUMULATING ARRAY TO PREPARE FOR SIMULATION AT NEXT STEP.
C **************************************************************
C last modified: 02-15-2005
C
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,CNEW,
     &                         CWGT,CINACT,RMASIO
C
      IMPLICIT  NONE
      INTEGER   ICOMP,J,I,K
C
C--COPY CNEW TO CWGT
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,ICOMP).EQ.0) CYCLE
            CWGT(J,I,K,ICOMP)=CNEW(J,I,K,ICOMP)
          ENDDO
        ENDDO
      ENDDO
C
C--CLEAR RMASIO ARRAY FOR ACCUMULATING MASS IN/OUT
C--AT NEXT TRANSPORT STEP
      DO I=1,122
        RMASIO(I,1,ICOMP)=0.
        RMASIO(I,2,ICOMP)=0.
      ENDDO
C
      RETURN
      END
C
C
      SUBROUTINE BTN5BD(ICOMP,DTRANS,TIME2,HT2)
C **********************************************************************
C THIS SUBROUTINE SUMMARIZES VOLUMETRIC MASS BUDGETS AND CALCULATES
C MASS BALANCE DISCREPANCY SINCE THE BEGINNING OF THE SIMULATION.
C **********************************************************************
C last modified: 02-20-2010
C
      USE UZTVARS,       ONLY: PRSITYSAV,SATOLD
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,DELR,DELC,DH,
     &                         PRSITY,RETA,CNEW,COLD,RHOB,SRCONC,
     &                         PRSITY2,RETA2,iSSTrans,ISOTHM,TMASIN,
     &                         TMASOT,ERROR,ERROR2,TMASIO,RMASIO,TMASS,
     &                         ISS,iUnitTRNOP,
     &                         IALTFM,QSTO                !edm
      USE MIN_SAT, ONLY: IDRYBUD,DRYON,NICBND2,ID2D,TMASS2,QC7,COLD7                                    !# LINE 1227 BTN
      USE RCTMOD, ONLY: IREACTION,IFESLD,MASS_NEG                   !# LINE 1228 BTN
C
      IMPLICIT  NONE
      INTEGER   ICOMP,K,I,J,IQ,INDX,N
      REAL      DMSTRG,SOURCE,SINK,TM1,TM2,DTRANS,
     &          CMML,CMMS,CIML,CIMS,VOLUME,STRMAS
      REAL TIME2,HT2
C
C--CALCULATE SOLUTE AND SORBED MASS STORAGE CHANGES (MOBILE-DOMAIN)
C--FOR THE CURRENT TRANSPORT STEP
      IF(iSSTrans.ne.0) goto 1110  !skip if steady-state transport 
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,ICOMP).GT.0.AND.DTRANS.GT.0) THEN
              IF(.NOT.(iUnitTRNOP(7).GT.0)) THEN
                IF(IALTFM.EQ.2) THEN
                DMSTRG=(CNEW(J,I,K,ICOMP)-COLD(J,I,K,ICOMP))
     &                     *PRSITY(J,I,K)*(DELR(J)*DELC(I)*DH(J,I,K)
     &              +DELR(J)*DELC(I)*DH(J,I,K)*QSTO(J,I,K)/PRSITY(J,I,K)
     &              *(HT2-TIME2))
                ELSE
                DMSTRG=(CNEW(J,I,K,ICOMP)-COLD(J,I,K,ICOMP))
     &                     *DELR(J)*DELC(I)*DH(J,I,K)*PRSITY(J,I,K)
                ENDIF
C                                                                   !# LINE 1254 BTN
                IF(IREACTION.EQ.2) THEN                             !# LINE 1255 BTN
                  IF(ICOMP==NCOMP.AND.IFESLD>0)THEN                 !# LINE 1256 BTN
                    DMSTRG=DMSTRG/PRSITY(J,I,K)*RHOB(J,I,K)         !# LINE 1257 BTN
                  ENDIF                                             !# LINE 1258 BTN
                ENDIF                                               !# LINE 1259 BTN
C                                                                   !# LINE 1260 BTN
              ELSE
                DMSTRG=((CNEW(J,I,K,ICOMP)*PRSITY(J,I,K))-          !edm
     &          (COLD(J,I,K,ICOMP)*SATOLD(J,I,K)*PRSITYSAV(J,I,K)))*!edm
     &                   DELR(J)*DELC(I)*DH(J,I,K)                  !edm
              ENDIF
              IF(DMSTRG.LT.0) THEN
                RMASIO(119,1,ICOMP)=RMASIO(119,1,ICOMP)-DMSTRG
                RMASIO(120,1,ICOMP)=RMASIO(120,1,ICOMP)
     &           -(RETA(J,I,K,ICOMP)-1.)*DMSTRG
              ELSE
                RMASIO(119,2,ICOMP)=RMASIO(119,2,ICOMP)-DMSTRG
                RMASIO(120,2,ICOMP)=RMASIO(120,2,ICOMP)
     &           -(RETA(J,I,K,ICOMP)-1.)*DMSTRG
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
 1110 Continue   
C
C--ACCUMULATE MASS IN/OUT FOR VARIOUS SINK/SOURCE TERMS AND
C--MASS STOAGE CHANGES SINCE THE BEGINNING OF SIMULATION
      IF(.NOT.(iUnitTRNOP(7).GT.0)) THEN                            !edm
        DO IQ=1,122
          TMASIO(IQ,1,ICOMP)=TMASIO(IQ,1,ICOMP)+RMASIO(IQ,1,ICOMP)
          TMASIO(IQ,2,ICOMP)=TMASIO(IQ,2,ICOMP)+RMASIO(IQ,2,ICOMP)
        ENDDO
      ELSE                                                          !edm
        DO IQ=1,117                                                 !edm
          TMASIO(IQ,1,ICOMP)=TMASIO(IQ,1,ICOMP)+RMASIO(IQ,1,ICOMP)  !edm
          TMASIO(IQ,2,ICOMP)=TMASIO(IQ,2,ICOMP)+RMASIO(IQ,2,ICOMP)  !edm
        ENDDO                                                       !edm
        DO IQ=119,122                                               !edm
          TMASIO(IQ,1,ICOMP)=TMASIO(IQ,1,ICOMP)+RMASIO(IQ,1,ICOMP)  !edm
          TMASIO(IQ,2,ICOMP)=TMASIO(IQ,2,ICOMP)+RMASIO(IQ,2,ICOMP)  !edm
        ENDDO                                                       !edm
      ENDIF                                                         !edm
C
C--DETERMINE TOTAL MASS IN AND OUT
      TMASIN(ICOMP)=0.
      TMASOT(ICOMP)=0.
      IF(.NOT.(iUnitTRNOP(7).GT.0)) THEN                            !edm
        DO IQ=1,122
          IF(IDRYBUD.EQ.0 .AND. IQ.EQ.12) CYCLE !SKIP MASS-TO-DRY  !# LINE 1287 BTN
          IF(IQ.EQ.14) CYCLE !SKIP CELL-BY-CELL MASS               !# LINE 1288 BTN
          TMASIN(ICOMP)=TMASIN(ICOMP)+TMASIO(IQ,1,ICOMP)
          TMASOT(ICOMP)=TMASOT(ICOMP)+TMASIO(IQ,2,ICOMP)
        ENDDO
      ELSE                                                          !edm
        DO IQ=1,117                                                 !edm
          IF(IDRYBUD.EQ.0 .AND. IQ.EQ.12) CYCLE !SKIP MASS-TO-DRY   !# LINE 1287 BTN
          IF(IQ.EQ.14) CYCLE !SKIP CELL-BY-CELL MASS                !# LINE 1288 BTN
          TMASIN(ICOMP)=TMASIN(ICOMP)+TMASIO(IQ,1,ICOMP)            !edm
          TMASOT(ICOMP)=TMASOT(ICOMP)+TMASIO(IQ,2,ICOMP)            !edm
        ENDDO                                                       !edm
        DO IQ=119,122                                               !edm
          IF(IDRYBUD.EQ.0 .AND. IQ.EQ.12) CYCLE !SKIP MASS-TO-DRY   !# LINE 1287 BTN
          IF(IQ.EQ.14) CYCLE !SKIP CELL-BY-CELL MASS                !# LINE 1288 BTN
          TMASIN(ICOMP)=TMASIN(ICOMP)+TMASIO(IQ,1,ICOMP)            !edm
          TMASOT(ICOMP)=TMASOT(ICOMP)+TMASIO(IQ,2,ICOMP)            !edm
        ENDDO                                                       !edm
      ENDIF                                                         !edm
C
C--COMPUTE ACCUMULATIVE DISCREPANCY BETWEEN MASS IN AND OUT
      ERROR(ICOMP)=0.
      IF(ABS(TMASIN(ICOMP))+ABS(TMASOT(ICOMP)).NE.0) THEN
        IF(IREACTION.EQ.2) THEN                                          !# LINE 1296 BTN
         ERROR(ICOMP)=100.*(TMASIN(ICOMP)+TMASOT(ICOMP)+MASS_NEG(ICOMP)) !# LINE 1297 BTN
     &    /(0.5*(ABS(TMASIN(ICOMP)-MASS_NEG(ICOMP)))+ABS(TMASOT(ICOMP))) !# LINE 1298 BTN
        ELSE                                                             !# LINE 1299 BTN
          ERROR(ICOMP)=100.*(TMASIN(ICOMP)+TMASOT(ICOMP))
     &     /(0.5*(ABS(TMASIN(ICOMP))+ABS(TMASOT(ICOMP))))
        ENDIF                                                            !# LINE 1302 BTN
      ENDIF
C
C--CALCULATE TOTAL MASS IN AQUIFER FOR CURRENT TRANSPORT STEP
      TMASS(1,2,ICOMP)=0.
      TMASS(2,2,ICOMP)=0.
      TMASS(3,2,ICOMP)=0.
      TMASS(4,2,ICOMP)=0.
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(ICBUND(J,I,K,ICOMP).LE.0) CYCLE
            VOLUME=DELR(J)*DELC(I)*DH(J,I,K)
            IF(IALTFM.GE.1) THEN
              VOLUME=VOLUME+QSTO(J,I,K)*DELR(J)*DELC(I)*DH(J,I,K)*
     1        (HT2-TIME2)/PRSITY(J,I,K)
            ENDIF
            CMML=CNEW(J,I,K,ICOMP)*PRSITY(J,I,K)*VOLUME
            CMMS=0.
            CIML=0.
            CIMS=0.
            IF(ISOTHM.EQ.1) THEN
              CMMS=(RETA(J,I,K,ICOMP)-1.)*CMML
            ELSEIF(ISOTHM.GT.1.AND.ISOTHM.LE.4) THEN
              CMMS=SRCONC(J,I,K,ICOMP)*RHOB(J,I,K)*VOLUME
            ELSEIF(ISOTHM.GT.4) THEN
              CMMS=(RETA(J,I,K,ICOMP)-1.)*CMML
              CIML=PRSITY2(J,I,K)*SRCONC(J,I,K,ICOMP)*VOLUME
              CIMS=(RETA2(J,I,K,ICOMP)-1.)*CIML
            ENDIF
            TMASS(1,2,ICOMP)=TMASS(1,2,ICOMP)+CMML
            TMASS(2,2,ICOMP)=TMASS(2,2,ICOMP)+CMMS
            TMASS(3,2,ICOMP)=TMASS(3,2,ICOMP)+CIML
            TMASS(4,2,ICOMP)=TMASS(4,2,ICOMP)+CIMS
          ENDDO
        ENDDO
      ENDDO
C
C--CALCULATE TOTAL MASS IN INACTIVE CELL STORAGE
cvsb123      TMASS2=0.
cvsb123      IF(DRYON.AND.IDRYBUD.NE.0) THEN
cvsb123      DO INDX=1,NICBND2
cvsb123        N=ID2D(INDX)
cvsb123        CALL NODE2KIJ(N,NLAY,NROW,NCOL,K,I,J)
cvsb123c        TMASS2(1,1,ICOMP)=QC7(J,I,K,ICOMP,7)*COLD7(J,I,K,ICOMP)*
cvsb123c     1                   -QC7(J,I,K,ICOMP,7)*COLD7(J,I,K,ICOMP)*
cvsb123      ENDDO
cvsb123      ENDIF
C
C--COMPUTE TOTAL SOURCE AND SINK EXCLUDING MASS
C--FROM OR INTO FLUID-STORAGE IN TRANSIENT FLOW FIELD
      SOURCE=0.
      SINK=0.
      DO IQ=1,117
        IF(IQ.EQ.12) CYCLE ! SKIP MASS-TO-DRY                 !# LINE 1341 BTN
        IF(IQ.EQ.14) CYCLE ! SKIP CELL-BY-CELL MASS           !# LINE 1342 BTN
        SOURCE=SOURCE+TMASIO(IQ,1,ICOMP)
        SINK=SINK+TMASIO(IQ,2,ICOMP)
      ENDDO
C
C--GET SUM OF TOTAL SOURCE AND INITIAL MASS
      TM1=ABS(SOURCE)+TMASS(1,1,ICOMP)+TMASS(2,1,ICOMP)
     &   +TMASS(3,1,ICOMP)+TMASS(4,1,ICOMP)
C
C--GET SUM OF TOTAL SINK AND CURRENT MASS
      TM2=ABS(SINK)+TMASS(1,2,ICOMP)+TMASS(2,2,ICOMP)
     &   +TMASS(3,2,ICOMP)+TMASS(4,2,ICOMP)
C
C--CORRECT FOR NET MASS FROM/INTO FLUID-STORAGE
      STRMAS=(TMASIO(118,1,ICOMP)+TMASIO(118,2,ICOMP))
     &     -(TMASS(1,3,ICOMP)+TMASS(2,3,ICOMP)
     &      +TMASS(3,3,ICOMP)+TMASS(4,3,ICOMP))
      TM1=TM1+STRMAS
C
C--COMPUTE ALTERNATVE MEASURE OF MASS DISCREPANCY
      ERROR2(ICOMP)=0.
      if(iSSTrans.ne.0) goto 1120 !skip if steady-state transport 
      IF( TM1+TM2 .NE.0. ) THEN
        ERROR2(ICOMP)=100.*(TM1-TM2)/(0.5*(TM1+TM2))
      ENDIF
 1120 continue    
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE BTN5OT(KPER,KSTP,NTRANS,ICOMP,TIME2)
C **********************************************************************
C THIS SUBROUTINE SAVES SIMULATION RESULTS IN THE STANDARD OUTPUT FILE
C AND VARIOUS OPTIONAL OUTPUT FILES, ACCORDING TO THE OUTPUT CONTROL
C OPTIONS SPECIFIED IN THE BASIC TRANSPORT INPUT FILE.
C **********************************************************************
C last modified: 02-20-2010
C
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,ICBUND,CNEW,
     &                         NPINS,NRC,RETA,SRCONC,MXTRNOP,TUNIT,
     &                         MUNIT,IOUT,IOBS,IUCN,IUCN2,IMAS,ICBM,
     &                         iUnitTRNOP,
     &                         NOBS,NPROBS,LOCOBS,MIXELM,ISOTHM,TMASIN,
     &                         TMASOT,ERROR,ERROR2,TMASIO,RMASIO,TMASS,
     &                         PRTOUT,IFMTCN,IFMTNP,IFMTRF,IFMTDP,
     &                         SAVUCN,SAVCBM,CHKMAS,NPRMAS,
     &                         FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &                         FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMNW,FDRT,
     &                         FETS,FSWT,FSFR,
     &                         NCOUNT,NPCHEK
      USE RCTMOD                                               !# LINE 1386 BTN
      USE MIN_SAT                                              !# LINE 1387 BTN
C
      IMPLICIT  NONE
      INTEGER   KPER,KSTP,NTRANS,K,I,J,N,ICOMP,IGRID
      REAL      TIME2,SOURCE,SINK,STRMAS,TOTMAS
      CHARACTER TEXT*16
C
C--PRINT OUT CONCENTRATIONS AT SPECIFIED OBSERVATION POINTS
C--TO FILE [MT3Dnnn.OBS] IF REQUESTED.
      IF(NOBS.GT.0.AND.(MOD(NTRANS-1,NPROBS).EQ.0.OR.PRTOUT)) THEN
        IF(NOBS.LE.16) THEN
          WRITE(IOBS+ICOMP,1000) NTRANS,TIME2,(CNEW(LOCOBS(3,N),
     &     LOCOBS(2,N),LOCOBS(1,N),ICOMP),N=1,NOBS)
        ELSE
          WRITE(IOBS+ICOMP,1010) NTRANS,TIME2,(CNEW(LOCOBS(3,N),
     &     LOCOBS(2,N),LOCOBS(1,N),ICOMP),N=1,NOBS)
        ENDIF
      ENDIF
 1000 FORMAT(1X,I5,1X,1PG13.5,1X,16(G13.5,1X))
 1010 FORMAT(1X,I5,1X,1PG13.5,1X,16(G13.5,1X)/(1X,20X,16(G13.5,1X)))
C
C--WRITE A ONE-LINE SUMMARY OF MASS BALANCE
C--TO FILE [MT3Dnnn.MAS] IF REQUESTED
      IF(CHKMAS.AND.(MOD(NTRANS-1,NPRMAS).EQ.0.OR.PRTOUT)) THEN
        SOURCE=0.
        SINK=0.
        DO N=1,117
          IF(DOMINSAT) THEN                                    !# LINE 1430
            IF(N.EQ.12) CYCLE !SKIP MASS-TO-DRY                !# LINE 1431
            IF(N.EQ.14) CYCLE !SKIP CELL-BY-CELL MASS          !# LINE 1432
          ENDIF                                                !# LINE 1433
          SOURCE=SOURCE+TMASIO(N,1,ICOMP)
          SINK=SINK+TMASIO(N,2,ICOMP)
        ENDDO
        STRMAS=(TMASIO(118,1,ICOMP)+TMASIO(118,2,ICOMP))
     &        -(TMASS(1,3,ICOMP)+TMASS(2,3,ICOMP)
     &        +TMASS(3,3,ICOMP)+TMASS(4,3,ICOMP))
        TOTMAS=TMASS(1,2,ICOMP)+TMASS(2,2,ICOMP)
     &        +TMASS(3,2,ICOMP)+TMASS(4,2,ICOMP)
        WRITE(IMAS+ICOMP,1012) TIME2,TMASIN(ICOMP),TMASOT(ICOMP),
     &   SOURCE,SINK,STRMAS,TOTMAS,
     &   ERROR(ICOMP),ERROR2(ICOMP)
C.......WRITE TO DRY FILE                                            !# LINE 1445 BTN
        IF(DOMINSAT)                                                 !# LINE 1446 BTN
     &    WRITE(IMAS+NCOMP+ICOMP,2012) TIME2,TMASIO(12,2,ICOMP),     !# LINE 1447 BTN
     &     TMASIO(12,1,ICOMP),TMASIO(14,2,ICOMP),TMASIO(14,1,ICOMP), !# LINE 1448 BTN
     &     TMASIO(1,2,ICOMP),TMASIO(1,1,ICOMP),TOTMAS                !# LINE 1449 BTN
        ENDIF
 1012   FORMAT(1X,1P,7(G13.5,1X),4X,G10.3,5X,G10.3)
 2012   FORMAT(1X,1P,8(G13.5,1X),4X,G10.3,5X,G10.3)                  !# LINE 1452 BTN
C
C--SAVE CELL CONCENTRATIONS TO UNFORMATTED FILES IF REQUESTED
      IF(SAVUCN .AND. PRTOUT) THEN
        TEXT='CONCENTRATION'
C
C--DISSOLVED PHASE CONCENTRATION TO FILE [MT3Dnnn.UCN]
        DO K=1,NLAY
          WRITE(IUCN+ICOMP) NTRANS,KSTP,KPER,TIME2,TEXT,NCOL,NROW,K
          WRITE(IUCN+ICOMP) ((CNEW(J,I,K,ICOMP),J=1,NCOL),I=1,NROW)
        ENDDO
C
C--SORBED/IMMOBILE PHASE CONCENTRATIONS TO FILE [MT3DnnnS.UCN]
        IF(ISOTHM.GT.0) THEN
          DO K=1,NLAY
            WRITE(IUCN2+ICOMP) NTRANS,KSTP,KPER,TIME2,TEXT,NCOL,NROW,K
            WRITE(IUCN2+ICOMP) ((SRCONC(J,I,K,ICOMP),J=1,NCOL),I=1,NROW)
          ENDDO
        ENDIF
C                                                                      !# LINE 1473
C--MASS FOR METHANE FOR KINETIC REACTION                               !# LINE 1474
        IF(IREACTION.EQ.2) THEN                                        !# LINE 1475
          IF(NSTORE.GT.0.AND.ICOMP<=NED+NEA)THEN                       !# LINE 1476
            IF(SPECIAL(ICOMP)=='STORE')THEN                            !# LINE 1477
              DO K=1,NLAY                                              !# LINE 1478
                WRITE(IUMETH) NTRANS,KSTP,KPER,TIME2,TEXT,NCOL,NROW,K  !# LINE 1479
                WRITE(IUMETH) ((MASSSTOR(J,I,K),J=1,NCOL),I=1,NROW)    !# LINE 1480
              ENDDO                                                    !# LINE 1481
            ENDIF                                                      !# LINE 1482
          ENDIF                                                        !# LINE 1483
        ENDIF                                                          !# LINE 1484
      ENDIF
C
C--WRITE SIMULATION RESULTS AND MASS BUDGET TERMS
C--TO THE STANDARD OUTFILE IF NEEDED
      IF(.NOT.PRTOUT) GOTO 9999
C
C--PRINT A HEADER
      WRITE(IOUT,1019) ICOMP
 1019 FORMAT(///1X,55('>'),'FOR COMPONENT NO.',I3.2,55('<'))
C
      WRITE(IOUT,1020) NTRANS,TIME2,TUNIT
 1020 FORMAT(//44X,43('-')/54X,'TRANSPORT STEP NO.',I5/44X,43('-')
     & //1X,'TOTAL ELAPSED TIME SINCE BEGINNING OF SIMULATION =',
     & G15.7,A4/1X,69('.'))
C
C--PRINT CELL CONCENTRATIONS IF NEEDED
      IF(IFMTCN.EQ.0) GOTO 40
C
      TEXT='CONCENTRATIONS'
      DO K=1,NLAY
        CALL RPRINT(CNEW(1:NCOL,1:NROW,K,ICOMP),TEXT,
     &   NTRANS,KSTP,KPER,NCOL,NROW,K,IFMTCN,IOUT)
      ENDDO
C--Print cell sorbed concentrations if specified                !edm
      IF(ISOTHM.EQ.4)THEN                                       !edm
        TEXT='SORBED CONC.'                                     !edm
        DO K=1,NLAY                                             !edm
          CALL RPRINT(SRCONC(:,:,K,ICOMP),TEXT,                 !edm
     &     NTRANS,KSTP,KPER,NCOL,NROW,K,IFMTCN,IOUT)            !edm
        ENDDO                                                   !edm
      ENDIF                                                     !edm
C
C--PRINT NONLINEAR RETARDATION FACTOR IF NEEDED
   40 IF(iUnitTRNOP(4).EQ.0) GOTO 50
      IF(ISOTHM.NE.2.AND.ISOTHM.NE.3) GOTO 50
      IF(IFMTRF.EQ.0) GOTO 50
C
      TEXT='RETARD. FACTOR'
      DO K=1,NLAY
        CALL RPRINT(RETA(1:NCOL,1:NROW,K,ICOMP),TEXT,
     &   NTRANS,KSTP,KPER,NCOL,NROW,K,IFMTRF,IOUT)
      ENDDO
C
C--PRINT PARTICLE USAGE INFORMATION IF NECESSARY
   50 IF(iUnitTRNOP(1).EQ.0) GOTO 70
      IF(MIXELM.LE.0) GOTO 70
C
      WRITE(IOUT,1030) NCOUNT(ICOMP),NPINS(ICOMP),NRC(ICOMP)
 1030 FORMAT(/1X,'TOTAL PARTICLES USED IN THE CURRENT STEP =',I10
     &       /1X,'PARTICLES ADDED AT BEGINNING OF THE STEP =',I10
     &       /1X,'PARTICLES REMOVED AT END OF LAST STEP    =',I10)
C
C--PRINT PARTICLE NUMBER PER CELL IF NEEDED
      IF(IFMTNP.EQ.0) GOTO 70
C
      TEXT='PARTICLE NUMBER '
      DO K=1,NLAY
        CALL IPRINT(NPCHEK(1:NCOL,1:NROW,K,ICOMP),TEXT,
     &   NTRANS,KSTP,KPER,NCOL,NROW,K,IFMTNP,IOUT)
      ENDDO
C
C--PRINT OUT ACCUMULATIVE MASS BALANCE INFORMATION
   70 WRITE(IOUT,1110) NTRANS,KSTP,KPER
      WRITE(IOUT,1114)
      WRITE(IOUT,1122) TMASIO(6,1,ICOMP),TMASIO(6,2,ICOMP)
      WRITE(IOUT,1120) TMASIO(1,1,ICOMP),TMASIO(1,2,ICOMP)
      IF(FWEL) WRITE(IOUT,1130) TMASIO(2,1,ICOMP),TMASIO(2,2,ICOMP)
      IF(FDRN) WRITE(IOUT,1140) TMASIO(3,1,ICOMP),TMASIO(3,2,ICOMP)
      IF(FRIV) WRITE(IOUT,1150) TMASIO(4,1,ICOMP),TMASIO(4,2,ICOMP)
      IF(FGHB) WRITE(IOUT,1160) TMASIO(5,1,ICOMP),TMASIO(5,2,ICOMP)
      IF(FRCH) WRITE(IOUT,1162) TMASIO(7,1,ICOMP),TMASIO(7,2,ICOMP)
      IF(FEVT) WRITE(IOUT,1164) TMASIO(8,1,ICOMP),TMASIO(8,2,ICOMP)
      IF(TMASIO(15,1,ICOMP)-TMASIO(15,2,ICOMP).NE.0)
     &   WRITE(IOUT,1165) TMASIO(15,1,ICOMP),TMASIO(15,2,ICOMP)
C
      IF(FSTR) WRITE(IOUT,2100) TMASIO(21,1,ICOMP),TMASIO(21,2,ICOMP)
      IF(FRES) WRITE(IOUT,2102) TMASIO(22,1,ICOMP),TMASIO(22,2,ICOMP)
      IF(FFHB) WRITE(IOUT,2104) TMASIO(23,1,ICOMP),TMASIO(23,2,ICOMP)
      IF(FIBS) WRITE(IOUT,2106) TMASIO(24,1,ICOMP),TMASIO(24,2,ICOMP)
      IF(FTLK) WRITE(IOUT,2108) TMASIO(25,1,ICOMP),TMASIO(25,2,ICOMP)
      IF(FLAK) WRITE(IOUT,2110) TMASIO(26,1,ICOMP),TMASIO(26,2,ICOMP)
      IF(FMNW) WRITE(IOUT,2112) TMASIO(27,1,ICOMP),TMASIO(27,2,ICOMP)
      IF(FDRT) WRITE(IOUT,2114) TMASIO(28,1,ICOMP),TMASIO(28,2,ICOMP)
      IF(FETS) WRITE(IOUT,2116) TMASIO( 8,1,ICOMP),TMASIO( 8,2,ICOMP)
      IF(iUnitTRNOP(13).GT.0) 
     &   WRITE(IOUT,2118) TMASIO(50,1,ICOMP),TMASIO(50,2,ICOMP)      
      IF(FSWT) WRITE(IOUT,2200) TMASIO(51,1,ICOMP),TMASIO(51,2,ICOMP)
      IF(FSFR) WRITE(IOUT,2202) TMASIO(52,1,ICOMP),TMASIO(52,2,ICOMP)
      IF(iUnitTRNOP(7).GT.0) 
     &   WRITE(IOUT,2204) TMASIO(53,1,ICOMP),TMASIO(53,2,ICOMP)
C
      IF(iUnitTRNOP(6).GT.0)                                     !# LINE 1567 BTN
     &  WRITE(IOUT,1190) TMASIO(11,1,ICOMP),TMASIO(11,2,ICOMP)   !# LINE 1568 BTN
C                                                                !# LINE 1569 BTN
      IF(DOMINSAT.EQ..TRUE.) THEN                                !# LINE 1570 BTN
        IF(IDRYBUD.EQ.1)                                         !# LINE 1571 BTN
     &    WRITE(IOUT,1192) TMASIO(12,1,ICOMP),TMASIO(12,2,ICOMP) !# LINE 1572 BTN
      ENDIF                                                      !# LINE 1573 BTN
C                                                                !# LINE 1574 BTN
      IF(IREACTION.EQ.1) THEN                                    !# LINE 1575 BTN
        WRITE(IOUT,1194) TMASIO(13,1,ICOMP),TMASIO(13,2,ICOMP)   !# LINE 1576 BTN
      ELSEIF(IREACTION.EQ.2) THEN                                !# LINE 1577 BTN
        WRITE(IOUT,1195) TMASIO(13,1,ICOMP),TMASIO(13,2,ICOMP)   !# LINE 1578 BTN
      ENDIF                                                      !# LINE 1579 BTN
C                                                                !# LINE 1580 BTN
      IF(iUnitTRNOP(4).GT.0) 
     &  WRITE(IOUT,1166) TMASIO(9,1,ICOMP),TMASIO(9,2,ICOMP)
      WRITE(IOUT,1169) TMASIO(118,1,ICOMP),TMASIO(118,2,ICOMP)   !edm
C                                                                !# LINE 1585 BTN
      IF(IREACTION.EQ.2) THEN                                    !# LINE 1586 BTN
        WRITE(IOUT,1170) TMASIO(119,1,ICOMP)+MASS_NEG(ICOMP),    !# LINE 1587 BTN
     &   TMASIO(119,2,ICOMP)                                     !# LINE 1588 BTN
    ! & +                                                        !# LINE 1589 BTN
    ! & mass_neg(icomp)                                          !# LINE 1590 BTN
      ELSE                                                       !# LINE 1591 BTN
        WRITE(IOUT,1170) TMASIO(119,1,ICOMP),TMASIO(119,2,ICOMP)
      ENDIF                                                      !# LINE 1593 BTN
C                                                                !# LINE 1594 BTN
      IF(iUnitTRNOP(4).GT.0.AND.ISOTHM.GT.0)
     &   WRITE(IOUT,1172) TMASIO(120,1,ICOMP),TMASIO(120,2,ICOMP)
      IF(iUnitTRNOP(4).GT.0.AND.ISOTHM.GT.4) THEN
        WRITE(IOUT,1173)
        WRITE(IOUT,1174) TMASIO(10,1,ICOMP),TMASIO(10,2,ICOMP)
        WRITE(IOUT,1175) TMASIO(121,1,ICOMP),TMASIO(121,2,ICOMP)
        WRITE(IOUT,1176) TMASIO(122,1,ICOMP),TMASIO(122,2,ICOMP)
      ENDIF
      IF(IREACTION.EQ.2) THEN                                    !# LINE 1603 BTN
        WRITE(IOUT,1180) TMASIN(ICOMP)- MASS_NEG(ICOMP),         !# LINE 1604 BTN
     &   MUNIT,TMASOT(ICOMP),MUNIT,                              !# LINE 1605 BTN
     &   TMASIN(ICOMP)+TMASOT(ICOMP)+MASS_NEG(ICOMP),ERROR(ICOMP) !+mass_neg(icomp) !# LINE 1606 BTN
        WRITE(IOUT,*) MASS_NEG(ICOMP),"MASS_NEG", ICOMP          !# LINE 1607 BTN
        WRITE(IOUT,*) CON_NEG(ICOMP),"CON_NEG", ICOMP            !# LINE 1608 BTN
      ELSE                                                       !# LINE 1609 BTN
        WRITE(IOUT,1180) TMASIN(ICOMP),MUNIT,TMASOT(ICOMP),MUNIT,
     &   TMASIN(ICOMP)+TMASOT(ICOMP),ERROR(ICOMP)
      ENDIF                                                      !# LINE 1612 BTN
C
 1110 FORMAT(/21X,'CUMMULATIVE MASS BUDGETS AT END OF TRANSPORT STEP',
     & I5,', TIME STEP',I5,', STRESS PERIOD',I5/21X,90('-'))
 1114 FORMAT(/30X,24X,7X,'IN',8X,13X,6X,'OUT',
     &      /30X,24X,16('-'),13X,16('-'))
 1122 FORMAT(30X,' CONSTANT CONCENTRATION: ',G15.7,13X,G15.7)
 1120 FORMAT(30X,'          CONSTANT HEAD: ',G15.7,13X,G15.7)
 1130 FORMAT(30X,'                  WELLS: ',G15.7,13X,G15.7)
 1140 FORMAT(30X,'                 DRAINS: ',G15.7,13X,G15.7)
 1150 FORMAT(30X,'                 RIVERS: ',G15.7,13X,G15.7)
 1160 FORMAT(30X,'HEAD-DEPENDENT BOUNDARY: ',G15.7,13X,G15.7)
 1162 FORMAT(30X,'               RECHARGE: ',G15.7,13X,G15.7)
 1164 FORMAT(30X,'     EVAPOTRANSPIRATION: ',G15.7,13X,G15.7)
 1165 FORMAT(30X,'           MASS LOADING: ',G15.7,13X,G15.7)
 1190 FORMAT(30X,'       TREATMENT SYSTEM: ',G15.7,13X,G15.7)    !# LINE 1627 BTN
 1192 FORMAT(30X,'INACTIVE CELLS(ICBND=0): ',G15.7,13X,G15.7)    !# LINE 1628 BTN
 1194 FORMAT(30X,'         EA-ED REACTION: ',G15.7,13X,G15.7)    !# LINE 1629 BTN
 1195 FORMAT(30X,'DECAY OR BIODEGRADATION: ',G15.7,13X,G15.7)    !# LINE 1630 BTN
C
 2100 FORMAT(30X,'     STREAMFLOW ROUTING: ',G15.7,13X,G15.7)
 2102 FORMAT(30X,'              RESERVOIR: ',G15.7,13X,G15.7)
 2104 FORMAT(30X,' FLOW AND HEAD BOUNDARY: ',G15.7,13X,G15.7)
 2106 FORMAT(30X,'  INTERBED STORAGE FLOW: ',G15.7,13X,G15.7)
 2108 FORMAT(30X,'     TRANSIENT LEACKAGE: ',G15.7,13X,G15.7)
 2110 FORMAT(30X,'                   LAKE: ',G15.7,13X,G15.7)
 2112 FORMAT(30X,'        MULTI-NODE WELL: ',G15.7,13X,G15.7)
 2114 FORMAT(30X,' DRAIN WITH RETURN FLOW: ',G15.7,13X,G15.7)
 2116 FORMAT(30X,'           SEGMENTED ET: ',G15.7,13X,G15.7)
 2118 FORMAT(30X,'HSS TIME-VARYING SOURCE: ',G15.7,13X,G15.7) 
 2200 FORMAT(30X,'      USER-DEFINED NO.1: ',G15.7,13X,G15.7)
 2202 FORMAT(30X,'                 STREAM: ',G15.7,13X,G15.7)
 2204 FORMAT(30X,'      USER-DEFINED NO.3: ',G15.7,13X,G15.7)
C
 1166 FORMAT(30X,' 1ST/0TH ORDER REACTION: ',G15.7,13X,G15.7)
 1169 FORMAT(30X,' MASS STOR (FLOW MODEL): ',G15.7,13X,G15.7)
 1170 FORMAT(30X,'  MASS STORAGE (SOLUTE): ',G15.7,13X,G15.7)
 1172 FORMAT(30X,'  MASS STORAGE (SORBED): ',G15.7,13X,G15.7)
 1173 FORMAT(30X,'....immobile domain....')
 1174 FORMAT(30X,' 1ST/0TH ORDER REACTION: ',G15.7,13X,G15.7)
 1175 FORMAT(30X,'  MASS STORAGE (SOLUTE): ',G15.7,13X,G15.7)
 1176 FORMAT(30X,'  MASS STORAGE (SORBED): ',G15.7,13X,G15.7)
 1180 FORMAT(28X,75('-'),
     &      /30X,'                [TOTAL]: ',G15.7,1X,A4,8X,G15.7,
     & 1X,A4//40X,'                  NET (IN - OUT): ',G15.7,
     &       /40X,'           DISCREPANCY (PERCENT): ',G15.7)
C
C--RETURN
 9999 RETURN
      END
C
C
      SUBROUTINE BTN5FM(ICOMP,ICBUND,CADV,COLD,RETA,PRSITY,DH,DTRANS,
     &                  PRSITYSAV,SATOLD,HT2,TIME2)                           !edm
C *********************************************************************
C THIS SUBROUTINE INITIALIZES ALL MATRICES FOR THE IMPLICIT SCHEME.
C *********************************************************************
C last modified: 02-20-2010
C
      USE UZTVARS,       ONLY: IUZFBND
      USE MT3DMS_MODULE, ONLY: NCOL,NROW,NLAY,NCOMP,DELR,DELC,L,A,RHS,
     &                         NODES,UPDLHS,NCRS,MIXELM,iSSTrans,
     &                         IUZFBND,IALTFM,QSTO,iUnitTRNOP                 !edm
      USE MIN_SAT, ONLY: COLD7,DRYON
C
      IMPLICIT  NONE
      INTEGER   ICOMP,J,I,K,ICBUND,N,NRC,
     &          NSIZE
      REAL      CADV,COLD,PRSITY,DTRANS,RETA,TEMP,DH,
     &          PRSITYSAV,SATOLD,HT2,TIME2                                    !edm
      DIMENSION ICBUND(NODES,NCOMP),CADV(NODES,NCOMP),COLD(NODES,NCOMP),
     &          RETA(NODES,NCOMP),PRSITY(NODES),DH(NODES),
     &          PRSITYSAV(NODES),SATOLD(NODES)                      !edm
C
C--GET RIGHT-HAND-SIDE ARRAY [RHS]
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            N=(K-1)*NCOL*NROW + (I-1)*NCOL + J
            IF(MIXELM.EQ.0) THEN
              TEMP=COLD(N,ICOMP)
cvsb123              IF(DRYON) THEN
cvsb123                TEMP=COLD7(J,I,K,ICOMP)
cvsb123              ENDIF
            ELSE
              TEMP=CADV(N,ICOMP)
            ENDIF
            IF(ICBUND(N,ICOMP).LE.0) THEN
              RHS(N)=-TEMP
C              
            elseif(iSSTrans.eq.1) then  
              rhs(n)=0.           
C            
            ELSE
              IF(.NOT.(iUnitTRNOP(7).GT.0)) THEN
  10            IF(IALTFM.EQ.2) THEN
                  RHS(N)=-TEMP*RETA(N,ICOMP)/DTRANS*PRSITY(N)
     &                 *(DELR(J)*DELC(I)*DH(N)
     &         +DELR(J)*DELC(I)*DH(N)*QSTO(J,I,K)/PRSITY(N)*(HT2-TIME2))
                ELSE
                RHS(N)=-TEMP*RETA(N,ICOMP)/DTRANS*PRSITY(N)
     &                 *DELR(J)*DELC(I)*DH(N)
                ENDIF

              ELSE                                                  !edm
                IF(IUZFBND(J,I).LE.0) GOTO 10
                RHS(N)=-TEMP*RETA(N,ICOMP)/DTRANS*PRSITYSAV(N)      !edm
     &                  *SATOLD(N)*DELR(J)*DELC(I)*DH(N)            !edm
              ENDIF                                                 !edm
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN IF COEFF MATRIX [A] NOT TO BE UPDATED
      IF(.NOT.UPDLHS) GOTO 999
C
C--RESET COEFF MATRIX [A]
C--IF ALL CROSS TERMS ARE INVOLVED, ARRAY [A] HAS
C--LENGTH OF 19 * NODES
      IF(NCRS.EQ.1) THEN
         NSIZE=19*NODES
C
C--OTHERWISE IT HAS LENGTH OF 7 * NODES
      ELSE
         NSIZE=7*NODES
      ENDIF
C
C--CLEAR THE ARRAY
      DO I=1,NSIZE
        A(I)=0.
      ENDDO
C
C--LOOP THROUGH ALL CELLS AND RESET A
      N=0
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            N=N+1
C
C--IF INACTIVE OR CONSTANT CELL
            IF(ICBUND(N,ICOMP).LE.0) THEN
               A(N)=-1.
            ELSE if(iSSTrans.eq.0)  then
              IF(IALTFM.EQ.2) THEN
               A(N)=-RETA(N,ICOMP)/DTRANS*PRSITY(N)
     &              *(DELR(J)*DELC(I)*DH(N)
     &         +DELR(J)*DELC(I)*DH(N)*QSTO(J,I,K)/PRSITY(N)*(HT2-TIME2))
              ELSE
               A(N)=-RETA(N,ICOMP)/DTRANS*PRSITY(N)
     &              *DELR(J)*DELC(I)*DH(N)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C
!        OPEN(201,FILE='C:\\TMP\\A_Array.TXT')
!        N=0
!        DO K=1,NLAY
!          DO I=1,NROW
!            DO J=1,NCOL
!              N=N+1
!              WRITE(201,171)(A(N))
!            ENDDO
!          ENDDO    
!        ENDDO                                     
!  171   FORMAT(40E20.10)                        
!        CLOSE(201)                                
!C                                                 
!        OPEN(202,FILE='C:\\TMP\\RHS_Array.TXT')
!        DO K=1,NLAY
!          DO I=1,NROW
!            DO J=1,NCOL
!              N=(K-1)*NCOL*NROW + (I-1)*NCOL + J
!              WRITE(202,172)(RHS(N))
!            ENDDO
!          ENDDO
!        ENDDO                                     
!  172   FORMAT(1000E20.10)                        
!        CLOSE(202)                                
C
C
C--CALCULATE MATRIX INDICES FOR THE GCG SOLVER
      NRC = NROW*NCOL
      L(1) =  0
      L(2) = -NRC
      L(3) =  NRC
      L(4) = -NCOL
      L(5) =  NCOL
      L(6) = -1
      L(7) =  1
      L(8) = -NCOL-NRC
      L(9) = -1-NRC
      L(10)= 1-NRC
      L(11)= NCOL-NRC
      L(12)=-NCOL+NRC
      L(13)=-1+NRC
      L(14)= 1+NRC
      L(15)= NCOL+NRC
      L(16)=-1-NCOL
      L(17)= 1-NCOL
      L(18)=-1+NCOL
      L(19)= 1+NCOL
C
C--NORMAL RETURN
  999 RETURN
      END
C
C
      LOGICAL FUNCTION UNIFOR(A,NC,NR,NL)
C ***************************************************
C THIS FUNCTION CHECKS WHETHER ELEMENTS IN AN ARRAY
C ARE UNIFORM.
C ***************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   NC,NR,NL,J,I,K
      REAL      A,AI,EPSILON
      PARAMETER (EPSILON=0.5E-6)
      DIMENSION A(NC,NR,NL)
C
C--GET THE 1ST ELEMENT
      AI=A(1,1,1)
C
C--COMPARE REST OF ELEMENTS WITH THE 1ST ELEMENT
      DO K=1,NL
        DO I=1,NR
          DO J=1,NC
            IF(ABS(A(J,I,K)-AI).GT.ABS(A(J,I,K)+AI)*EPSILON) THEN
              UNIFOR=.FALSE.
              RETURN
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--IF ALL ELEMENTS ARE EQUAL, SET [UNIFOR] TO T
      UNIFOR=.TRUE.
      RETURN
      END