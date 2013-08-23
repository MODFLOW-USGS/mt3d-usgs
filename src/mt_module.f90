    MODULE MIN_SAT             !# This module comes from mt_mst.for of Vivek's code
        LOGICAL,          SAVE,                      POINTER :: DOMINSAT
        LOGICAL,          SAVE,                      POINTER :: DRYON
        INTEGER,          SAVE, DIMENSION(:,:,:),    POINTER :: ICBND2 
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ID2D
        REAL,             SAVE, DIMENSION(:,:,:,:,:),POINTER :: QC7
        REAL,             SAVE, DIMENSION(:),        POINTER :: C7
        REAL,             SAVE, DIMENSION(:,:,:,:),  POINTER :: COLD7
        REAL,             SAVE, DIMENSION(:,:,:),    POINTER :: TMASS2
        INTEGER,          SAVE,                      POINTER :: ICNTDRY
        INTEGER,          SAVE,                      POINTER :: NCNTDRY
        INTEGER,          SAVE,                      POINTER :: NICBND2
        INTEGER,          SAVE,                      POINTER :: MUTDRY
        INTEGER,          SAVE,                      POINTER :: IC2DRY
        INTEGER,          SAVE,                      POINTER :: IABSMIN
        INTEGER,          SAVE,                      POINTER :: IDRYBUD
        INTEGER,          SAVE,                      POINTER :: IATS
    END MODULE MIN_SAT
    MODULE SFRVARS
        INTEGER,          SAVE,                      POINTER :: NSTRM
        INTEGER,          SAVE,                      POINTER :: MXSFBC
        INTEGER,          SAVE,                      POINTER :: NSSSF
        INTEGER,          SAVE,                      POINTER :: NSFINIT
        INTEGER,          SAVE,                      POINTER :: ICBCSF
        INTEGER,          SAVE,                      POINTER :: IETSFR
        INTEGER,          SAVE,                      POINTER :: ISFSOLV
        INTEGER,          SAVE,                      POINTER :: MXITERSF
        REAL,             SAVE,                      POINTER :: WIMP
        REAL,             SAVE,                      POINTER :: WUPS
        REAL,             SAVE,                      POINTER :: CCLOSESF
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFL
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFR
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFC
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISEG
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IREACH
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: NIN
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INFLWNOD
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IEXIT
        REAL,             SAVE, DIMENSION(:),        POINTER :: SFLEN
        REAL,             SAVE, DIMENSION(:),        POINTER :: SFNAREA
        REAL,             SAVE, DIMENSION(:),        POINTER :: SFOAREA
        REAL,             SAVE, DIMENSION(:),        POINTER :: QPRECSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: QRUNOFSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: QPRECSFO
        REAL,             SAVE, DIMENSION(:),        POINTER :: QRUNOFSFO
        REAL,             SAVE, DIMENSION(:),        POINTER :: QSFGW
        REAL,             SAVE, DIMENSION(:),        POINTER :: QOUTSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: QETSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: QOUTSFO
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IBNDSF
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),      POINTER :: CNEWSF
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: COLDSF
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: COLDSF2
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CNEWSFTMP
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: DISPSF
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CBCSF
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISEGBC
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IRCHBC
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFBCTYP
        REAL,             SAVE, DIMENSION(:),        POINTER :: RMASSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: VOUTSF
!
        INTEGER,          SAVE,                      POINTER :: MXSGMT
        INTEGER,          SAVE,                      POINTER :: MXRCH
        INTEGER,          SAVE, DIMENSION(:,:),      POINTER :: ISTRM
!
!.......OBSERVATION VARIABLES
        INTEGER,          SAVE,                      POINTER :: NOBSSF
        INTEGER,          SAVE,                      POINTER :: IOUTOBS
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISOBS
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IROBS
!
!.......SIZE NINTOT
        INTEGER,          SAVE,                      POINTER :: NINTOT
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IDXNIN
        REAL,             SAVE, DIMENSION(:),        POINTER :: QINSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: QINSFO
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INSEG
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INRCH
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IDSPFLG
!
!.......SOLVER VARIABLES
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IASF
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: JASF
        DOUBLE PRECISION, SAVE, DIMENSION(:),        POINTER :: AMATSF
        DOUBLE PRECISION, SAVE, DIMENSION(:),        POINTER :: RHSSF
        INTEGER,          SAVE,                      POINTER :: NJASF
        REAL,             SAVE,                      POINTER :: CRNTSF
!.......CUMULATIVE TERMS
        REAL,             SAVE, DIMENSION(:),        POINTER :: CFLOINSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: CFLOOUTSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: CGW2SFR
        REAL,             SAVE, DIMENSION(:),        POINTER :: CGWFROMSFR
        REAL,             SAVE, DIMENSION(:),        POINTER :: CLAK2SFR
        REAL,             SAVE, DIMENSION(:),        POINTER :: CLAKFROMSFR
        REAL,             SAVE, DIMENSION(:),        POINTER :: CPRECSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: CRUNOFSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: CETSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: CSTORINSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: CSTOROTSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: CCCINSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: CCCOUTSF
CONTAINS
      SUBROUTINE MEMDEALLOCATE3()
        IF(ASSOCIATED(IASF))   DEALLOCATE(IASF)        !# LINE 496 SFR
        IF(ASSOCIATED(RHSSF))  DEALLOCATE(RHSSF)       !# LINE 497 SFR
        IF(ASSOCIATED(JASF))   DEALLOCATE(JASF)        !# LINE 498 SFR
        IF(ASSOCIATED(AMATSF)) DEALLOCATE(AMATSF)      !# LINE 499 SFR
      END SUBROUTINE MEMDEALLOCATE3
!--DEALLOCATE AND ALLOCATE ARRAYS FOR STORING FLOW TERMS
      SUBROUTINE MEMDEALLOCATE5()
        IF(ASSOCIATED(ISFL))       DEALLOCATE(ISFL)         !# LINE 1440 FMI
        IF(ASSOCIATED(ISFR))       DEALLOCATE(ISFR)         !# LINE 1441 FMI
        IF(ASSOCIATED(ISFC))       DEALLOCATE(ISFC)         !# LINE 1442 FMI
        IF(ASSOCIATED(ISEG))       DEALLOCATE(ISEG)         !# LINE 1443 FMI
        IF(ASSOCIATED(IREACH))     DEALLOCATE(IREACH)       !# LINE 1444 FMI
        IF(ASSOCIATED(NIN))        DEALLOCATE(NIN)          !# UPDATED
        IF(ASSOCIATED(INFLWNOD))   DEALLOCATE(INFLWNOD)     !# UPDATED
        IF(ASSOCIATED(SFLEN))      DEALLOCATE(SFLEN)        !# LINE 1445 FMI
        IF(ASSOCIATED(SFNAREA))    DEALLOCATE(SFNAREA)      !# LINE 1446 FMI
        IF(ASSOCIATED(SFOAREA))    DEALLOCATE(SFOAREA)      !# LINE 1447 FMI
        IF(ASSOCIATED(QPRECSF))    DEALLOCATE(QPRECSF)      !# LINE 1448 FMI
        IF(ASSOCIATED(QRUNOFSF))   DEALLOCATE(QRUNOFSF)     !# LINE 1449 FMI
        IF(ASSOCIATED(QPRECSFO))   DEALLOCATE(QPRECSFO)     !# LINE 1450 FMI
        IF(ASSOCIATED(QRUNOFSFO))  DEALLOCATE(QRUNOFSFO)    !# LINE 1451 FMI
        IF(ASSOCIATED(QSFGW))      DEALLOCATE(QSFGW)        !# LINE 1452 FMI
        IF(ASSOCIATED(QOUTSF))     DEALLOCATE(QOUTSF)       !# LINE 1453 FMI
        IF(ASSOCIATED(QOUTSFO))    DEALLOCATE(QOUTSFO)      !# LINE 1454 FMI
        IF(ASSOCIATED(QETSF))      DEALLOCATE(QETSF)        !# LINE 1455 FMI
        IF(ASSOCIATED(QINSF))      DEALLOCATE(QINSF)        !# LINE 1458 FMI
        IF(ASSOCIATED(QINSFO))     DEALLOCATE(QINSFO)       !# LINE 1459 FMI
        IF(ASSOCIATED(INSEG))      DEALLOCATE(INSEG)        !# LINE 1460 FMI
        IF(ASSOCIATED(INRCH))      DEALLOCATE(INRCH)        !# LINE 1461 FMI
        IF(ASSOCIATED(IDSPFLG))    DEALLOCATE(IDSPFLG)      !# LINE 1462 FMI
        IF(ASSOCIATED(IDXNIN))     DEALLOCATE(IDXNIN)       !# LINE 1463 FMI
!.......INDEXING TO GET ISTRM FROM SEG AND RCH NUMBERS      !# LINE 1464 FMI
        IF(ASSOCIATED(ISTRM))       DEALLOCATE(ISTRM)       !# LINE 1465 FMI
        IF(ASSOCIATED(CNEWSF))      DEALLOCATE(CNEWSF)
        IF(ASSOCIATED(COLDSF))      DEALLOCATE(COLDSF)
        IF(ASSOCIATED(COLDSF2))     DEALLOCATE(COLDSF2)
        IF(ASSOCIATED(CNEWSFTMP))   DEALLOCATE(CNEWSFTMP)
        IF(ASSOCIATED(DISPSF))      DEALLOCATE(DISPSF)
        IF(ASSOCIATED(CBCSF))       DEALLOCATE(CBCSF)
        IF(ASSOCIATED(ISEGBC))      DEALLOCATE(ISEGBC)
        IF(ASSOCIATED(IRCHBC))      DEALLOCATE(IRCHBC)
        IF(ASSOCIATED(ISFBCTYP))    DEALLOCATE(ISFBCTYP)
        IF(ASSOCIATED(RMASSF))      DEALLOCATE(RMASSF)
        IF(ASSOCIATED(VOUTSF))      DEALLOCATE(VOUTSF)
        IF(ASSOCIATED(ISOBS))       DEALLOCATE(ISOBS)
        IF(ASSOCIATED(IROBS))       DEALLOCATE(IROBS)
        IF(ASSOCIATED(CFLOINSF))    DEALLOCATE(CFLOINSF)
        IF(ASSOCIATED(CFLOOUTSF))   DEALLOCATE(CFLOOUTSF)
        IF(ASSOCIATED(CGW2SFR))     DEALLOCATE(CGW2SFR)
        IF(ASSOCIATED(CGWFROMSFR))  DEALLOCATE(CGWFROMSFR)
        IF(ASSOCIATED(CLAK2SFR))    DEALLOCATE(CLAK2SFR)
        IF(ASSOCIATED(CLAKFROMSFR)) DEALLOCATE(CLAKFROMSFR)
        IF(ASSOCIATED(CPRECSF))     DEALLOCATE(CPRECSF)
        IF(ASSOCIATED(CRUNOFSF))    DEALLOCATE(CRUNOFSF)
        IF(ASSOCIATED(CETSF))       DEALLOCATE(CETSF)
        IF(ASSOCIATED(CSTORINSF))   DEALLOCATE(CSTORINSF)
        IF(ASSOCIATED(CSTOROTSF))   DEALLOCATE(CSTOROTSF)
        IF(ASSOCIATED(CCCINSF))   DEALLOCATE(CCCINSF)
        IF(ASSOCIATED(CCCOUTSF))   DEALLOCATE(CCCOUTSF)
      END SUBROUTINE MEMDEALLOCATE5                   !# LINE 1466 FMI
    END MODULE SFRVARS
    MODULE LAKVARS
        INTEGER,          SAVE,                      POINTER :: NLAKES
        INTEGER,          SAVE,                      POINTER :: LKNODE
        INTEGER,          SAVE,                      POINTER :: NSFRLAK
        INTEGER,          SAVE,                      POINTER :: MXLKBC
        INTEGER,          SAVE,                      POINTER :: NSSLK
        INTEGER,          SAVE,                      POINTER :: NLKINIT
        INTEGER,          SAVE,                      POINTER :: ICBCLK
        INTEGER,          SAVE,                      POINTER :: IETLAK
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: LAKL
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: LAKR
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: LAKC
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: LAKNUMGW
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: LAKRCH
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: LAKSEG
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: LAKNUMSFR
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CNEWLAK
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: COLDLAK
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CBCLK
        REAL,             SAVE, DIMENSION(:),        POINTER :: QPRECLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: QRUNOFLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: QWDRLLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: QLAKGW
        REAL,             SAVE, DIMENSION(:),        POINTER :: QLAKSFR
        REAL,             SAVE, DIMENSION(:),        POINTER :: QETLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: VOLNLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: VOLOLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: DELVOLLAK
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ILKBC
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ILKBCTYP
        REAL,             SAVE, DIMENSION(:),        POINTER :: RMASLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: VOUTLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CGW2LAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CGWFROMLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CSFR2LAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CSFRFROMLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CPRECLK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CRUNOFLK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CWDRLLK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CETLK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CSTORINLK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CSTOROTLK
CONTAINS
      SUBROUTINE MEMDEALLOCATE4()
        IF(ASSOCIATED(LAKL)) DEALLOCATE(LAKL)
        IF(ASSOCIATED(LAKR)) DEALLOCATE(LAKR)
        IF(ASSOCIATED(LAKC)) DEALLOCATE(LAKC)
        IF(ASSOCIATED(LAKNUMGW)) DEALLOCATE(LAKNUMGW)
        IF(ASSOCIATED(LAKRCH)) DEALLOCATE(LAKRCH)
        IF(ASSOCIATED(LAKSEG)) DEALLOCATE(LAKSEG)
        IF(ASSOCIATED(LAKNUMSFR)) DEALLOCATE(LAKNUMSFR)
        IF(ASSOCIATED(CNEWLAK)) DEALLOCATE(CNEWLAK)
        IF(ASSOCIATED(COLDLAK)) DEALLOCATE(COLDLAK)
        IF(ASSOCIATED(CBCLK)) DEALLOCATE(CBCLK)
        IF(ASSOCIATED(QPRECLAK)) DEALLOCATE(QPRECLAK)
        IF(ASSOCIATED(QRUNOFLAK)) DEALLOCATE(QRUNOFLAK)
        IF(ASSOCIATED(QWDRLLAK)) DEALLOCATE(QWDRLLAK)
        IF(ASSOCIATED(QLAKGW)) DEALLOCATE(QLAKGW)
        IF(ASSOCIATED(QLAKSFR)) DEALLOCATE(QLAKSFR)
        IF(ASSOCIATED(QETLAK)) DEALLOCATE(QETLAK)
        IF(ASSOCIATED(VOLNLAK)) DEALLOCATE(VOLNLAK)
        IF(ASSOCIATED(VOLOLAK)) DEALLOCATE(VOLOLAK)
        IF(ASSOCIATED(DELVOLLAK)) DEALLOCATE(DELVOLLAK)
        IF(ASSOCIATED(ILKBC)) DEALLOCATE(ILKBC)
        IF(ASSOCIATED(ILKBCTYP)) DEALLOCATE(ILKBCTYP)
        IF(ASSOCIATED(RMASLAK)) DEALLOCATE(RMASLAK)
        IF(ASSOCIATED(VOUTLAK)) DEALLOCATE(VOUTLAK)
        IF(ASSOCIATED(CGW2LAK)) DEALLOCATE(CGW2LAK)
        IF(ASSOCIATED(CGWFROMLAK)) DEALLOCATE(CGWFROMLAK)
        IF(ASSOCIATED(CSFR2LAK)) DEALLOCATE(CSFR2LAK)
        IF(ASSOCIATED(CSFRFROMLAK)) DEALLOCATE(CSFRFROMLAK)
        IF(ASSOCIATED(CPRECLK)) DEALLOCATE(CPRECLK)
        IF(ASSOCIATED(CRUNOFLK)) DEALLOCATE(CRUNOFLK)
        IF(ASSOCIATED(CWDRLLK)) DEALLOCATE(CWDRLLK)
        IF(ASSOCIATED(CETLK)) DEALLOCATE(CETLK)
      END SUBROUTINE MEMDEALLOCATE4
      SUBROUTINE MEMDEALLOCATE6()
        IF(ASSOCIATED(LAKL)) DEALLOCATE(LAKL)            !# LINE 1620 FMI
        IF(ASSOCIATED(LAKR)) DEALLOCATE(LAKR)            !# LINE 1621 FMI
        IF(ASSOCIATED(LAKC)) DEALLOCATE(LAKC)            !# LINE 1622 FMI
        IF(ASSOCIATED(LAKRCH)) DEALLOCATE(LAKRCH)        !# LINE 1623 FMI
        IF(ASSOCIATED(LAKSEG)) DEALLOCATE(LAKSEG)        !# LINE 1624 FMI
        IF(ASSOCIATED(QPRECLAK)) DEALLOCATE(QPRECLAK)    !# LINE 1625 FMI
        IF(ASSOCIATED(QRUNOFLAK)) DEALLOCATE(QRUNOFLAK)  !# LINE 1626 FMI
        IF(ASSOCIATED(QWDRLLAK)) DEALLOCATE(QWDRLLAK)    !# LINE 1627 FMI
        IF(ASSOCIATED(QLAKGW)) DEALLOCATE(QLAKGW)        !# LINE 1628 FMI
        IF(ASSOCIATED(QLAKSFR)) DEALLOCATE(QLAKSFR)      !# LINE 1629 FMI
        IF(ASSOCIATED(QETLAK)) DEALLOCATE(QETLAK)        !# LINE 1630 FMI
        IF(ASSOCIATED(VOLNLAK)) DEALLOCATE(VOLNLAK)      !# LINE 1631 FMI
        IF(ASSOCIATED(VOLOLAK)) DEALLOCATE(VOLOLAK)      !# LINE 1632 FMI
        IF(ASSOCIATED(DELVOLLAK)) DEALLOCATE(DELVOLLAK)  !# LINE 1633 FMI
        IF(ASSOCIATED(LAKNUMGW)) DEALLOCATE(LAKNUMGW)    !# LINE 1634 FMI
        IF(ASSOCIATED(LAKNUMSFR)) DEALLOCATE(LAKNUMSFR)  !# LINE 1635 FMI
      END SUBROUTINE MEMDEALLOCATE6
    END MODULE LAKVARS
    MODULE RCTMOD              !# This module come from mt_rct.for in Vivek's code
        LOGICAL,            SAVE,                      POINTER :: ISORBIMONLY
        INTEGER,            SAVE,                      POINTER :: IREACTION
        INTEGER,            SAVE,                      POINTER :: IED
        INTEGER,            SAVE,                      POINTER :: IEA
        REAL,               SAVE,                      POINTER :: FEDEA
        REAL,               SAVE, DIMENSION(:,:,:,:),  POINTER :: CRCT
        REAL,               SAVE, DIMENSION(:,:,:,:),  POINTER :: RC3
        REAL,               SAVE, DIMENSION(:),        POINTER :: YLD
        INTEGER,            SAVE,                      POINTER :: IFESLD
        INTEGER,            SAVE,                      POINTER :: NED
        INTEGER,            SAVE,                      POINTER :: NEA
        INTEGER,            SAVE,                      POINTER :: NSPECIAL
        INTEGER,            SAVE,                      POINTER :: NSTORE
        INTEGER,            SAVE,                      POINTER :: IUMETH
        CHARACTER(LEN=500), SAVE,                      POINTER :: rec_FileName
        CHARACTER(LEN=50),  SAVE,                      POINTER :: Ad_methane_name
        REAL,               SAVE, DIMENSION(:,:,:),    POINTER :: MASSSTOR
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: RCNEW 
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: RCOLD
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: MAXEC
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: INHIB
        REAL*8,             SAVE, DIMENSION(:,:),      POINTER :: DECAY
        REAL*8,             SAVE, DIMENSION(:,:),      POINTER :: YIELDC
        REAL*8,             SAVE, DIMENSION(:,:,:),    POINTER :: DCDT_FE
        REAL*8,             SAVE, DIMENSION(:,:),      POINTER :: DCDT_S 
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: DEA_ED_DT
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: DCDT
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: SWITCH
        CHARACTER(LEN=5),   SAVE, DIMENSION(:),        POINTER :: SPECIAL
        REAL*8,             SAVE,                      POINTER :: RVAL
        REAL,               SAVE, DIMENSION(:),        POINTER :: MASS_NEG
        REAL,               SAVE, DIMENSION(:),        POINTER :: CON_NEG
        REAL,               SAVE, DIMENSION(:,:,:,:),  POINTER :: SP1IM
        INTEGER,            SAVE,                      POINTER :: ISP1IM
CONTAINS
      SUBROUTINE MEMDEALLOCATE2()
      
      IF(ASSOCIATED(CRCT)) DEALLOCATE(CRCT)               !# LINE 1923 RCT
      IF(ASSOCIATED(MASSSTOR)) DEALLOCATE(MASSSTOR)       !# LINE 1924 RCT
      IF(ASSOCIATED(RCNEW)) DEALLOCATE(RCNEW)             !# LINE 1925 RCT
      IF(ASSOCIATED(RCOLD)) DEALLOCATE(RCOLD)             !# LINE 1926 RCT
      IF(ASSOCIATED(MAXEC)) DEALLOCATE(MAXEC)             !# LINE 1927 RCT
      IF(ASSOCIATED(INHIB)) DEALLOCATE(INHIB)             !# LINE 1928 RCT
      IF(ASSOCIATED(DECAY)) DEALLOCATE(DECAY)             !# LINE 1929 RCT
      IF(ASSOCIATED(YIELDC)) DEALLOCATE(YIELDC)           !# LINE 1930 RCT
      IF(ASSOCIATED(DCDT_FE)) DEALLOCATE(DCDT_FE)         !# LINE 1931 RCT
      IF(ASSOCIATED(DCDT_S)) DEALLOCATE(DCDT_S)           !# LINE 1932 RCT
      IF(ASSOCIATED(DEA_ED_DT)) DEALLOCATE(DEA_ED_DT)     !# LINE 1933 RCT
      IF(ASSOCIATED(DCDT)) DEALLOCATE(DCDT)               !# LINE 1934 RCT
      IF(ASSOCIATED(SWITCH)) DEALLOCATE(SWITCH)           !# LINE 1935 RCT
      IF(ASSOCIATED(SPECIAL)) DEALLOCATE(SPECIAL)         !# LINE 1936 RCT
!      IF(ASSOCIATED(INIC)) DEALLOCATE(INIC)              !# LINE 1936 RCT
      IF(ASSOCIATED(MASS_NEG)) DEALLOCATE(MASS_NEG)       !# LINE 1938 RCT
      IF(ASSOCIATED(CON_NEG)) DEALLOCATE(CON_NEG)         !# LINE 1939 RCT
      END SUBROUTINE MEMDEALLOCATE2  
      
    END MODULE RCTMOD
    MODULE MT3DMS_MODULE
        INTEGER,PARAMETER :: MXTRNOP=20,MXSTP=9000
        CHARACTER(LEN=4), SAVE, DIMENSION(MXTRNOP) :: NameTRNOP=        &
     &  (/'ADV ', 'DSP ', 'SSM ', 'RCT ', 'GCG ',                       &
     &    'CTS ', 'UZF ', '    ', '    ', '    ',                       &      !edm
     &    'TOB ', '    ', 'HSS ', 'TSO ', 'RTR ',                       &
     &    '    ', '    ', 'LKT ', 'SFT ', '    '/)
        INTEGER,          SAVE,                       POINTER :: NCOL      
        INTEGER,          SAVE,                       POINTER :: NROW      
        INTEGER,          SAVE,                       POINTER :: NLAY      
        INTEGER,          SAVE,                       POINTER :: NPER      
        INTEGER,          SAVE,                       POINTER :: NCOMP     
        INTEGER,          SAVE,                       POINTER :: MCOMP     
        INTEGER,          SAVE,                       POINTER :: NODES     
        INTEGER,          SAVE,                       POINTER :: NSTP      
        INTEGER,          SAVE,                       POINTER :: MXSTRN    
        INTEGER,          SAVE,                       POINTER :: iSSTrans  
        INTEGER,          SAVE,                       POINTER :: MXPART    
        INTEGER,          SAVE,                       POINTER :: IOUT      
        INTEGER,          SAVE,                       POINTER :: INBTN     
        INTEGER,          SAVE,                       POINTER :: INADV     
        INTEGER,          SAVE,                       POINTER :: INDSP     
        INTEGER,          SAVE,                       POINTER :: INSSM
        INTEGER,          SAVE,                       POINTER :: INCTS         !# Not set as expected in Vivek's, but setting here
        INTEGER,          SAVE,                       POINTER :: INTSO         !# Not set as expected in Vivek's, but setting here
        INTEGER,          SAVE,                       POINTER :: INRCT     
        INTEGER,          SAVE,                       POINTER :: INGCG     
        INTEGER,          SAVE,                       POINTER :: INTOB
        INTEGER,          SAVE,                       POINTER :: INUZF         !edm     
        INTEGER,          SAVE,                       POINTER :: INHSS
        INTEGER,          SAVE,                       POINTER :: INFTL     
        INTEGER,          SAVE,                       POINTER :: IFTLFMT   
        INTEGER,          SAVE,                       POINTER :: ICNF      
        INTEGER,          SAVE,                       POINTER :: IUCN      
        INTEGER,          SAVE,                       POINTER :: IUCN2     
        INTEGER,          SAVE,                       POINTER :: IOBS      
        INTEGER,          SAVE,                       POINTER :: IMAS      
        INTEGER,          SAVE,                       POINTER :: ICBM      
        REAL,             SAVE,                       POINTER :: DT0       
        REAL,             SAVE,                       POINTER :: TTSMULT   
        REAL,             SAVE,                       POINTER :: TTSMAX    
        CHARACTER(LEN=4), SAVE,                       POINTER :: TUNIT     
        CHARACTER(LEN=4), SAVE,                       POINTER :: LUNIT     
        CHARACTER(LEN=4), SAVE,                       POINTER :: MUNIT     
        CHARACTER(LEN=1), SAVE,                       POINTER :: FPRT      
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: iUnitTRNOP 
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: LAYCON
        INTEGER,          SAVE, DIMENSION(:,:,:,:),   POINTER :: ICBUND
        REAL,             SAVE, DIMENSION(:),         POINTER :: DELR
        REAL,             SAVE, DIMENSION(:),         POINTER :: DELC
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: DZ
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: PRSITY
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: PRSITYSAV  !edm
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: HTOP
        REAL,             SAVE, DIMENSION(:),         POINTER :: XBC
        REAL,             SAVE, DIMENSION(:),         POINTER :: YBC
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: ZBC
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: CNEW
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: COLD
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: SATOLD     !edm
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: SATNEW     !edm
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: QX
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: QY
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: QZ
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IUZFBND    !edm
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: WC         !edm
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: UZFLX      !edm
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: UZQSTO     !edm
!--UZF -> SFR & UZF -> LAK CONNECTIONS
        INTEGER,          SAVE,                      POINTER :: MXUZCON
        INTEGER,          SAVE, DIMENSION(:,:),      POINTER :: IROUTE
        REAL,             SAVE, DIMENSION(:),        POINTER :: UZQ
        INTEGER,          SAVE,                      POINTER :: NCON
        INTEGER,          SAVE,                      POINTER :: NCONLK
        INTEGER,          SAVE,                      POINTER :: NCONSF
!
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: SURFLK     !edm
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: QSTO
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: DH
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: SDH        !edm
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: CWGT
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: CADV
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: RETA
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: SRCONC
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: BUFF
        REAL,             SAVE, DIMENSION(:),         POINTER :: TSLNGH
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: RHOB
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: PRSITY2
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: RETA2
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: NPINS
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: NRC
!--BTN
        REAL,             SAVE,                       POINTER :: HT1
        REAL,             SAVE,                       POINTER :: HT2
        REAL,             SAVE,                       POINTER :: DTRANS
        REAL,             SAVE,                       POINTER :: DELT
        REAL,             SAVE,                       POINTER :: TIME1
        REAL,             SAVE,                       POINTER :: TIME2
        REAL,             SAVE,                       POINTER :: CINACT
        REAL,             SAVE,                       POINTER :: THKMIN
        REAL,             SAVE,                       POINTER :: RFMIN
        INTEGER,          SAVE,                       POINTER :: IMPSOL
        REAL,             SAVE,                       POINTER :: DTRACK
        REAL,             SAVE,                       POINTER :: DTRACK2
        REAL,             SAVE,                       POINTER :: DTSSM
        REAL,             SAVE,                       POINTER :: DTDISP
        REAL,             SAVE,                       POINTER :: DTRCT
        INTEGER,          SAVE,                       POINTER :: MIXELM
        INTEGER,          SAVE,                       POINTER :: ISPD
        REAL,             SAVE,                       POINTER :: PERCEL
        INTEGER,          SAVE,                       POINTER :: IFMTCN
        INTEGER,          SAVE,                       POINTER :: IFMTNP
        INTEGER,          SAVE,                       POINTER :: IFMTRF
        INTEGER,          SAVE,                       POINTER :: IFMTDP
        INTEGER,          SAVE,                       POINTER :: NPS
        INTEGER,          SAVE,                       POINTER :: NPRS
        INTEGER,          SAVE,                       POINTER :: NPRMAS
        LOGICAL,          SAVE,                       POINTER :: SAVUCN
        LOGICAL,          SAVE,                       POINTER :: SAVCBM
        LOGICAL,          SAVE,                       POINTER :: CHKMAS
        LOGICAL,          SAVE,                       POINTER :: PRTOUT
        LOGICAL,          SAVE,                       POINTER :: UPDLHS
        INTEGER,          SAVE,                       POINTER :: NOBS
        INTEGER,          SAVE,                       POINTER :: NPROBS
        REAL,             SAVE,                       POINTER :: HORIGN
        REAL,             SAVE,                       POINTER :: XMAX
        REAL,             SAVE,                       POINTER :: YMAX
        REAL,             SAVE,                       POINTER :: ZMAX
        LOGICAL,          SAVE,                       POINTER :: UNIDX
        LOGICAL,          SAVE,                       POINTER :: UNIDY
        LOGICAL,          SAVE,                       POINTER :: UNIDZ
        INTEGER,          SAVE,                       POINTER :: ISOTHM
        REAL,             SAVE, DIMENSION(:),         POINTER :: TIMPRS
        REAL,             SAVE, DIMENSION(:),         POINTER :: TMASIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: TMASOT
        REAL,             SAVE, DIMENSION(:),         POINTER :: ERROR
        REAL,             SAVE, DIMENSION(:),         POINTER :: ERROR2
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: LOCOBS
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: TMASIO
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: RMASIO
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: TMASS
!--BTN-Vivek
        INTEGER,          SAVE,                       POINTER :: INRTR         !# LINE 118 MAIN
        INTEGER,          SAVE,                       POINTER :: MINVOL        !# NOT SET IN VIVEK'S CODE, DOING SO HERE
!--ADV
        INTEGER,          SAVE,                       POINTER :: NADVFD
        INTEGER,          SAVE,                       POINTER :: ITRACK
        INTEGER,          SAVE,                       POINTER :: ISEED
        INTEGER,          SAVE,                       POINTER :: NPLANE
        INTEGER,          SAVE,                       POINTER :: NPL
        INTEGER,          SAVE,                       POINTER :: NPH
        INTEGER,          SAVE,                       POINTER :: NPMIN
        INTEGER,          SAVE,                       POINTER :: NPMAX
        INTEGER,          SAVE,                       POINTER :: INTERP
        INTEGER,          SAVE,                       POINTER :: NLSINK
        INTEGER,          SAVE,                       POINTER :: NPSINK
        REAL,             SAVE,                       POINTER :: WD
        REAL,             SAVE,                       POINTER :: DCEPS
        REAL,             SAVE,                       POINTER :: SRMULT
        REAL,             SAVE,                       POINTER :: DCHMOC
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: NCOUNT
        INTEGER,          SAVE, DIMENSION(:,:,:,:),   POINTER :: NPCHEK
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: INDEXX
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: INDEXY
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: INDEXZ
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: XP
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: YP
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: ZP
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CNPT
!--ADV-VIVEK
        REAL,             SAVE, DIMENSION(:,:,:,:,:), POINTER :: QC7
!--DSP                                                
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: ALPHAL
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: DMCOEF
        REAL,             SAVE, DIMENSION(:),         POINTER :: TRPT
        REAL,             SAVE, DIMENSION(:),         POINTER :: TRPV
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: DXX
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: DYY
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: DZZ
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: DXY
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: DXZ
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: DYX
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: DYZ
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: DZX
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: DZY
!--FMI
        INTEGER,          SAVE,                       POINTER :: NPERFL
        INTEGER,          SAVE,                       POINTER :: ISS
        INTEGER,          SAVE,                       POINTER :: IVER
        LOGICAL,          SAVE,                       POINTER :: FWEL
        LOGICAL,          SAVE,                       POINTER :: FDRN
        LOGICAL,          SAVE,                       POINTER :: FRCH
        LOGICAL,          SAVE,                       POINTER :: FEVT
        LOGICAL,          SAVE,                       POINTER :: FRIV
        LOGICAL,          SAVE,                       POINTER :: FGHB
        LOGICAL,          SAVE,                       POINTER :: FSTR
        LOGICAL,          SAVE,                       POINTER :: FRES
        LOGICAL,          SAVE,                       POINTER :: FFHB
        LOGICAL,          SAVE,                       POINTER :: FIBS
        LOGICAL,          SAVE,                       POINTER :: FTLK
        LOGICAL,          SAVE,                       POINTER :: FLAK
        LOGICAL,          SAVE,                       POINTER :: FMNW
        LOGICAL,          SAVE,                       POINTER :: FDRT
        LOGICAL,          SAVE,                       POINTER :: FETS
        LOGICAL,          SAVE,                       POINTER :: FSWT
        LOGICAL,          SAVE,                       POINTER :: FSFR
        LOGICAL,          SAVE,                       POINTER :: FUZF
!--FMI-Vivek
        INTEGER,          SAVE,                       POINTER :: NOCREWET              !# LINE 99 MAIN
!--SSM                    
        INTEGER,          SAVE,                       POINTER :: ISSGOUT
        INTEGER,          SAVE,                       POINTER :: MXSS
        INTEGER,          SAVE,                       POINTER :: NSS
        INTEGER,          SAVE,                       POINTER :: NTSS
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: RECH
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: FINFIL       !edm
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CUZINF       !edm
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IRCH
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CRCH
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: EVTR
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IEVT
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CEVT
        LOGICAL,          SAVE,                       POINTER :: IETFLG       !edm
        INTEGER,          SAVE,                       POINTER :: IUZFOPTG     !edm
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IUZFOPT      !edm
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: UZET         !edm
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: CUZET        !edm
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: GWET         !edm
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: CGWET        !edm
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: CSURFLK      !edm
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: SS
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: SSMC
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: SSG
!--SSM-Vivek
        INTEGER,          SAVE,                       POINTER :: IALTFM                !# LINE 99 MAIN
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: KSSZERO               !# LINE 2 SSM
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: COLDFLW
!--RCT
        INTEGER,          SAVE,                       POINTER :: IREACT
        INTEGER,          SAVE,                       POINTER :: IRCTOP
        INTEGER,          SAVE,                       POINTER :: IGETSC
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: FRAC
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: SP1
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: SP2
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: RC1
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: RC2
!--RCT-Vivek
        INTEGER,          SAVE,                       POINTER :: ICTSOUT               !# LINE 91-95 MAIN
        INTEGER,          SAVE,                       POINTER :: MXCTS                 !# LINE 91-95 MAIN
        INTEGER,          SAVE,                       POINTER :: MXEXT                 !# LINE 91-95 MAIN
        INTEGER,          SAVE,                       POINTER :: MXINJ                 !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: KEXT                  !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IEXT                  !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: JEXT                  !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: KINJ                  !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IINJ                  !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: JINJ                  !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:,:),     POINTER :: IOPTINJ               !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:,:),     POINTER :: IOPTEXT               !# LINE 91-95 MAIN
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CMCHGEXT              !# LINE 91-95 MAIN
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CMCHGINJ              !# LINE 91-95 MAIN
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: CINCTS                !# LINE 91-95 MAIN
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: CNTE                  !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: ITRTEXT               !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: ITRTINJ               !# LINE 91-95 MAIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: QINCTS                !# LINE 91-95 MAIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: QOUTCTS               !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: NEXT                  !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: NINJ                  !# LINE 91-95 MAIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: QCTS                  !# LINE 91-95 MAIN
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: CCTS                  !# LINE 91-95 MAIN
        INTEGER,          SAVE,                       POINTER :: NCTS                  !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IWEXT                 !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IWINJ                 !# LINE 91-95 MAIN
        INTEGER,          SAVE,                       POINTER :: MXWEL                 !# LINE 91-95 MAIN
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: IWCTS                 !# LINE 91-95 MAIN
        INTEGER,          SAVE,                       POINTER :: IFORCE                !# LINE 91-95 MAIN
        INTEGER,          SAVE,                       POINTER :: NCTSOLD               !# LINE 91-95 MAIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: CEXT2CTS              !# LINE 97-98 MAIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: CGW2CTS               !# LINE 97-98 MAIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: CADDM                 !# LINE 97-98 MAIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: CCTS2EXT              !# LINE 97-98 MAIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: CCTS2GW               !# LINE 97-98 MAIN
        REAL,             SAVE, DIMENSION(:),         POINTER :: CREMM                 !# LINE 97-98 MAIN
!        INTEGER,          SAVE, 
!--SFR/LAK, from Vivek's code
        INTEGER,          SAVE,                       POINTER :: INLKT                 !# LINE 96 MAIN
        INTEGER,          SAVE,                       POINTER :: INSFT                 !# LINE 96 MAIN
!--TOB
        INTEGER,          SAVE,                       POINTER :: MaxConcObs
        INTEGER,          SAVE,                       POINTER :: MaxFluxObs
        INTEGER,          SAVE,                       POINTER :: MaxFluxCells
        INTEGER,          SAVE,                       POINTER :: inConcObs
        INTEGER,          SAVE,                       POINTER :: nConcObs
        INTEGER,          SAVE,                       POINTER :: iOutCobs
        INTEGER,          SAVE,                       POINTER :: iConcLOG
        INTEGER,          SAVE,                       POINTER :: iConcINTP
        INTEGER,          SAVE,                       POINTER :: inFluxObs
        INTEGER,          SAVE,                       POINTER :: nFluxGroup
        INTEGER,          SAVE,                       POINTER :: nFluxObs
        INTEGER,          SAVE,                       POINTER :: iOutFlux
        INTEGER,          SAVE,                       POINTER :: inSaveObs
        REAL,             SAVE,                       POINTER :: CScale
        REAL,             SAVE,                       POINTER :: FScale
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: mLayer
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: prLayer
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: COBSWEL
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: TEMP
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: FluxGroup
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: GroupData
        CHARACTER(LEN=12),SAVE, DIMENSION(:),         POINTER :: FOBSNAM
        CHARACTER(LEN=12),SAVE, DIMENSION(:),         POINTER :: COBSNAM
!--GCG
        INTEGER,          SAVE,                       POINTER :: MXITER
        INTEGER,          SAVE,                       POINTER :: ITER1
        INTEGER,          SAVE,                       POINTER :: ISOLVE
        INTEGER,          SAVE,                       POINTER :: NCRS
        INTEGER,          SAVE,                       POINTER :: IPRGCG
        REAL,             SAVE,                       POINTER :: ACCL
        REAL,             SAVE,                       POINTER :: CCLOSE
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: LRCH
        REAL,             SAVE, DIMENSION(:),         POINTER :: A
        REAL,             SAVE, DIMENSION(:),         POINTER :: Q
        REAL,             SAVE, DIMENSION(:),         POINTER :: WK
        REAL,             SAVE, DIMENSION(:),         POINTER :: CNCG
        REAL,             SAVE, DIMENSION(:),         POINTER :: RHS
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: L
!--GCG-Vivek
        INTEGER,          SAVE,                       POINTER :: INOCROSS              !# LINE 99 MAIN
!--HSS                                                
        INTEGER,          SAVE,                       POINTER :: MaxHSSSource
        INTEGER,          SAVE,                       POINTER :: MaxHSSCells
        INTEGER,          SAVE,                       POINTER :: MaxHSSStep
        INTEGER,          SAVE,                       POINTER :: nHSSSource
        INTEGER,          SAVE,                       POINTER :: iRunHSSM
!        INTEGER,          SAVE,                       POINTER :: IHSSGEN
        REAL,             SAVE,                       POINTER :: faclength
        REAL,             SAVE,                       POINTER :: factime
        REAL,             SAVE,                       POINTER :: facmass
        INTEGER,          SAVE, DIMENSION(:,:,:),     POINTER :: iHSSLoc       
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: HSSData       
        CHARACTER(LEN=12),SAVE, DIMENSION(:),         POINTER :: HSSNAM
!--HSS-Vivek
        INTEGER,          SAVE,                       POINTER :: IHSSGEN              !# LINE 99 MAIN
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: P                    !# NEW
!        
CONTAINS
      SUBROUTINE MEMDEALLOCATE()
      IF(ASSOCIATED(NCOL)) DEALLOCATE(NCOL)
      IF(ASSOCIATED(NROW)) DEALLOCATE(NROW)
      IF(ASSOCIATED(NLAY)) DEALLOCATE(NLAY)
      IF(ASSOCIATED(NPER)) DEALLOCATE(NPER)
      IF(ASSOCIATED(NCOMP)) DEALLOCATE(NCOMP)
      IF(ASSOCIATED(MCOMP)) DEALLOCATE(MCOMP)
      IF(ASSOCIATED(NODES)) DEALLOCATE(NODES)
      IF(ASSOCIATED(NSTP)) DEALLOCATE(NSTP)
      IF(ASSOCIATED(MXSTRN)) DEALLOCATE(MXSTRN)
      IF(ASSOCIATED(iSSTrans)) DEALLOCATE(iSSTrans)
      IF(ASSOCIATED(MXPART)) DEALLOCATE(MXPART)
      IF(ASSOCIATED(IOUT)) DEALLOCATE(IOUT)
      IF(ASSOCIATED(INBTN)) DEALLOCATE(INBTN)
      IF(ASSOCIATED(INADV)) DEALLOCATE(INADV)
      IF(ASSOCIATED(INDSP)) DEALLOCATE(INDSP)
      IF(ASSOCIATED(INSSM)) DEALLOCATE(INSSM)
      IF(ASSOCIATED(INRCT)) DEALLOCATE(INRCT)
      IF(ASSOCIATED(INGCG)) DEALLOCATE(INGCG)
      IF(ASSOCIATED(INTOB)) DEALLOCATE(INTOB)
      IF(ASSOCIATED(INUZF)) DEALLOCATE(INUZF)                 !edm
      IF(ASSOCIATED(INHSS)) DEALLOCATE(INHSS)
      IF(ASSOCIATED(INFTL)) DEALLOCATE(INFTL)
      IF(ASSOCIATED(IFTLFMT)) DEALLOCATE(IFTLFMT)
      IF(ASSOCIATED(ICNF)) DEALLOCATE(ICNF)
      IF(ASSOCIATED(IUCN)) DEALLOCATE(IUCN)
      IF(ASSOCIATED(IUCN2)) DEALLOCATE(IUCN2)
      IF(ASSOCIATED(IOBS)) DEALLOCATE(IOBS)
      IF(ASSOCIATED(IMAS)) DEALLOCATE(IMAS)
      IF(ASSOCIATED(ICBM)) DEALLOCATE(ICBM)
      IF(ASSOCIATED(DT0)) DEALLOCATE(DT0)
      IF(ASSOCIATED(TTSMULT)) DEALLOCATE(TTSMULT)
      IF(ASSOCIATED(TTSMAX)) DEALLOCATE(TTSMAX)
      IF(ASSOCIATED(TUNIT)) DEALLOCATE(TUNIT)
      IF(ASSOCIATED(LUNIT)) DEALLOCATE(LUNIT)
      IF(ASSOCIATED(MUNIT)) DEALLOCATE(MUNIT)
      IF(ASSOCIATED(FPRT)) DEALLOCATE(FPRT)
      IF(ASSOCIATED(iUnitTRNOP)) DEALLOCATE(iUnitTRNOP)
      IF(ASSOCIATED(LAYCON)) DEALLOCATE(LAYCON)
      IF(ASSOCIATED(ICBUND)) DEALLOCATE(ICBUND)
      IF(ASSOCIATED(DELR)) DEALLOCATE(DELR)
      IF(ASSOCIATED(DELC)) DEALLOCATE(DELC)
      IF(ASSOCIATED(DZ)) DEALLOCATE(DZ)
      IF(ASSOCIATED(PRSITY)) DEALLOCATE(PRSITY)
!      IF(FUZF) THEN                                          !edm
!        IF(ASSOCIATED(PRSITYSAV)) DEALLOCATE(PRSITYSAV)      !edm
!      ENDIF                                                  !edm
      IF(ASSOCIATED(HTOP)) DEALLOCATE(HTOP)
      IF(ASSOCIATED(XBC)) DEALLOCATE(XBC)
      IF(ASSOCIATED(YBC)) DEALLOCATE(YBC)
      IF(ASSOCIATED(ZBC)) DEALLOCATE(ZBC)
      IF(ASSOCIATED(CNEW)) DEALLOCATE(CNEW)
      IF(ASSOCIATED(COLD)) DEALLOCATE(COLD)
      IF(ASSOCIATED(SATOLD)) DEALLOCATE(SATOLD)              !edm
      IF(ASSOCIATED(SATNEW)) DEALLOCATE(SATNEW)              !edm
      IF(ASSOCIATED(QX)) DEALLOCATE(QX)
      IF(ASSOCIATED(QY)) DEALLOCATE(QY)
      IF(ASSOCIATED(QZ)) DEALLOCATE(QZ)
      IF(ASSOCIATED(QSTO)) DEALLOCATE(QSTO)
      IF(ASSOCIATED(DH)) DEALLOCATE(DH)
      IF(ASSOCIATED(SDH))    DEALLOCATE(SDH)                 !edm
      IF(ASSOCIATED(IUZFBND)) DEALLOCATE(IUZFBND)            !edm
!      IF(ASSOCIATED(WC))     DEALLOCATE(WC)                 !edm
      IF(ASSOCIATED(UZFLX))  DEALLOCATE(UZFLX)               !edm
      IF(ASSOCIATED(UZQSTO)) DEALLOCATE(UZQSTO)              !edm
      IF(ASSOCIATED(SURFLK)) DEALLOCATE(SURFLK)              !edm
      
      IF(ASSOCIATED(CWGT)) DEALLOCATE(CWGT)
      IF(ASSOCIATED(CADV)) DEALLOCATE(CADV)
      IF(ASSOCIATED(RETA)) DEALLOCATE(RETA)
      IF(ASSOCIATED(SRCONC)) DEALLOCATE(SRCONC)
      IF(ASSOCIATED(BUFF)) DEALLOCATE(BUFF)
      IF(ASSOCIATED(TSLNGH)) DEALLOCATE(TSLNGH)
      IF(ASSOCIATED(RHOB)) DEALLOCATE(RHOB)
      IF(ASSOCIATED(PRSITY2)) DEALLOCATE(PRSITY2)
      IF(ASSOCIATED(RETA2)) DEALLOCATE(RETA2)
      IF(ASSOCIATED(NPINS)) DEALLOCATE(NPINS)
      IF(ASSOCIATED(NRC)) DEALLOCATE(NRC)
!
      IF(ASSOCIATED(HT1)) DEALLOCATE(HT1)
      IF(ASSOCIATED(HT2)) DEALLOCATE(HT2)
      IF(ASSOCIATED(DTRANS)) DEALLOCATE(DTRANS)
      IF(ASSOCIATED(DELT)) DEALLOCATE(DELT)
      IF(ASSOCIATED(TIME1)) DEALLOCATE(TIME1)
      IF(ASSOCIATED(TIME2)) DEALLOCATE(TIME2)
      IF(ASSOCIATED(CINACT)) DEALLOCATE(CINACT)
      IF(ASSOCIATED(THKMIN)) DEALLOCATE(THKMIN)
      IF(ASSOCIATED(RFMIN)) DEALLOCATE(RFMIN)
      IF(ASSOCIATED(IMPSOL)) DEALLOCATE(IMPSOL)
      IF(ASSOCIATED(DTRACK)) DEALLOCATE(DTRACK)
      IF(ASSOCIATED(DTRACK2)) DEALLOCATE(DTRACK2)
      IF(ASSOCIATED(DTSSM)) DEALLOCATE(DTSSM)
      IF(ASSOCIATED(DTDISP)) DEALLOCATE(DTDISP)
      IF(ASSOCIATED(DTRCT)) DEALLOCATE(DTRCT)
      IF(ASSOCIATED(MIXELM)) DEALLOCATE(MIXELM)
      IF(ASSOCIATED(ISPD)) DEALLOCATE(ISPD)
      IF(ASSOCIATED(PERCEL)) DEALLOCATE(PERCEL)
      IF(ASSOCIATED(IFMTCN)) DEALLOCATE(IFMTCN)
      IF(ASSOCIATED(IFMTNP)) DEALLOCATE(IFMTNP)
      IF(ASSOCIATED(IFMTRF)) DEALLOCATE(IFMTRF)
      IF(ASSOCIATED(IFMTDP)) DEALLOCATE(IFMTDP)
      IF(ASSOCIATED(NPS)) DEALLOCATE(NPS)
      IF(ASSOCIATED(NPRS)) DEALLOCATE(NPRS)
      IF(ASSOCIATED(NPRMAS)) DEALLOCATE(NPRMAS)
      IF(ASSOCIATED(SAVUCN)) DEALLOCATE(SAVUCN)
      IF(ASSOCIATED(SAVCBM)) DEALLOCATE(SAVCBM)
      IF(ASSOCIATED(CHKMAS)) DEALLOCATE(CHKMAS)
      IF(ASSOCIATED(PRTOUT)) DEALLOCATE(PRTOUT)
      IF(ASSOCIATED(UPDLHS)) DEALLOCATE(UPDLHS)
      IF(ASSOCIATED(NOBS)) DEALLOCATE(NOBS)
      IF(ASSOCIATED(NPROBS)) DEALLOCATE(NPROBS)
      IF(ASSOCIATED(HORIGN)) DEALLOCATE(HORIGN)
      IF(ASSOCIATED(XMAX)) DEALLOCATE(XMAX)
      IF(ASSOCIATED(YMAX)) DEALLOCATE(YMAX)
      IF(ASSOCIATED(ZMAX)) DEALLOCATE(ZMAX)
      IF(ASSOCIATED(UNIDX)) DEALLOCATE(UNIDX)
      IF(ASSOCIATED(UNIDY)) DEALLOCATE(UNIDY)
      IF(ASSOCIATED(UNIDZ)) DEALLOCATE(UNIDZ)
      IF(ASSOCIATED(ISOTHM)) DEALLOCATE(ISOTHM)
      IF(ASSOCIATED(TIMPRS)) DEALLOCATE(TIMPRS)
      IF(ASSOCIATED(TMASIN)) DEALLOCATE(TMASIN)
      IF(ASSOCIATED(TMASOT)) DEALLOCATE(TMASOT)
      IF(ASSOCIATED(ERROR)) DEALLOCATE(ERROR)
      IF(ASSOCIATED(ERROR2)) DEALLOCATE(ERROR2)
      IF(ASSOCIATED(LOCOBS)) DEALLOCATE(LOCOBS)
      IF(ASSOCIATED(TMASIO)) DEALLOCATE(TMASIO)
      IF(ASSOCIATED(RMASIO)) DEALLOCATE(RMASIO)
      IF(ASSOCIATED(TMASS)) DEALLOCATE(TMASS)
!
      IF(ASSOCIATED(NADVFD)) DEALLOCATE(NADVFD)
      IF(ASSOCIATED(ITRACK)) DEALLOCATE(ITRACK)
      IF(ASSOCIATED(ISEED)) DEALLOCATE(ISEED)
      IF(ASSOCIATED(NPLANE)) DEALLOCATE(NPLANE)
      IF(ASSOCIATED(NPL)) DEALLOCATE(NPL)
      IF(ASSOCIATED(NPH)) DEALLOCATE(NPH)
      IF(ASSOCIATED(NPMIN)) DEALLOCATE(NPMIN)
      IF(ASSOCIATED(NPMAX)) DEALLOCATE(NPMAX)
      IF(ASSOCIATED(INTERP)) DEALLOCATE(INTERP)
      IF(ASSOCIATED(NLSINK)) DEALLOCATE(NLSINK)
      IF(ASSOCIATED(NPSINK)) DEALLOCATE(NPSINK)
      IF(ASSOCIATED(WD)) DEALLOCATE(WD)
      IF(ASSOCIATED(DCEPS)) DEALLOCATE(DCEPS)
      IF(ASSOCIATED(SRMULT)) DEALLOCATE(SRMULT)
      IF(ASSOCIATED(DCHMOC)) DEALLOCATE(DCHMOC)
      IF(ASSOCIATED(NCOUNT)) DEALLOCATE(NCOUNT)
      IF(ASSOCIATED(NPCHEK)) DEALLOCATE(NPCHEK)
      IF(ASSOCIATED(INDEXX)) DEALLOCATE(INDEXX)
      IF(ASSOCIATED(INDEXY)) DEALLOCATE(INDEXY)
      IF(ASSOCIATED(INDEXZ)) DEALLOCATE(INDEXZ)
      IF(ASSOCIATED(XP)) DEALLOCATE(XP)
      IF(ASSOCIATED(YP)) DEALLOCATE(YP)
      IF(ASSOCIATED(ZP)) DEALLOCATE(ZP)
      IF(ASSOCIATED(CNPT)) DEALLOCATE(CNPT)
!
      IF(ASSOCIATED(ALPHAL)) DEALLOCATE(ALPHAL)
      IF(ASSOCIATED(DMCOEF)) DEALLOCATE(DMCOEF)
      IF(ASSOCIATED(TRPT)) DEALLOCATE(TRPT)
      IF(ASSOCIATED(TRPV)) DEALLOCATE(TRPV)
      IF(ASSOCIATED(DXX)) DEALLOCATE(DXX)
      IF(ASSOCIATED(DYY)) DEALLOCATE(DYY)
      IF(ASSOCIATED(DZZ)) DEALLOCATE(DZZ)
      IF(ASSOCIATED(DXY)) DEALLOCATE(DXY)
      IF(ASSOCIATED(DXZ)) DEALLOCATE(DXZ)
      IF(ASSOCIATED(DYX)) DEALLOCATE(DYX)
      IF(ASSOCIATED(DYZ)) DEALLOCATE(DYZ)
      IF(ASSOCIATED(DZX)) DEALLOCATE(DZX)
      IF(ASSOCIATED(DZY)) DEALLOCATE(DZY)
!
      IF(ASSOCIATED(NPERFL)) DEALLOCATE(NPERFL)
      IF(ASSOCIATED(ISS)) DEALLOCATE(ISS)
      IF(ASSOCIATED(IVER)) DEALLOCATE(IVER)
      IF(ASSOCIATED(FWEL)) DEALLOCATE(FWEL)
      IF(ASSOCIATED(FDRN)) DEALLOCATE(FDRN)
      IF(ASSOCIATED(FRCH)) DEALLOCATE(FRCH)
      IF(ASSOCIATED(FEVT)) DEALLOCATE(FEVT)
      IF(ASSOCIATED(FRIV)) DEALLOCATE(FRIV)
      IF(ASSOCIATED(FGHB)) DEALLOCATE(FGHB)
      IF(ASSOCIATED(FSTR)) DEALLOCATE(FSTR)
      IF(ASSOCIATED(FRES)) DEALLOCATE(FRES)
      IF(ASSOCIATED(FFHB)) DEALLOCATE(FFHB)
      IF(ASSOCIATED(FIBS)) DEALLOCATE(FIBS)
      IF(ASSOCIATED(FTLK)) DEALLOCATE(FTLK)
      IF(ASSOCIATED(FLAK)) DEALLOCATE(FLAK)
      IF(ASSOCIATED(FMNW)) DEALLOCATE(FMNW)
      IF(ASSOCIATED(FDRT)) DEALLOCATE(FDRT)
      IF(ASSOCIATED(FETS)) DEALLOCATE(FETS)
      IF(ASSOCIATED(FSWT)) DEALLOCATE(FSWT)
      IF(ASSOCIATED(FSFR)) DEALLOCATE(FSFR)
      IF(ASSOCIATED(FUZF)) DEALLOCATE(FUZF)
!
      IF(ASSOCIATED(ISSGOUT)) DEALLOCATE(ISSGOUT)
      IF(ASSOCIATED(MXSS)) DEALLOCATE(MXSS)
      IF(ASSOCIATED(NSS)) DEALLOCATE(NSS)
      IF(ASSOCIATED(NTSS)) DEALLOCATE(NTSS)
      IF(ASSOCIATED(RECH)) DEALLOCATE(RECH)
      IF(ASSOCIATED(FINFIL)) DEALLOCATE(FINFIL)        !edm
      IF(ASSOCIATED(CUZINF)) DEALLOCATE(CUZINF)        !edm
      IF(ASSOCIATED(IRCH)) DEALLOCATE(IRCH)
      IF(ASSOCIATED(CRCH)) DEALLOCATE(CRCH)
      IF(ASSOCIATED(EVTR)) DEALLOCATE(EVTR)
      IF(ASSOCIATED(IEVT)) DEALLOCATE(IEVT)
      IF(ASSOCIATED(CEVT)) DEALLOCATE(CEVT)
      IF(ASSOCIATED(IUZFOPTG)) DEALLOCATE(IUZFOPTG)    !edm
      IF(ASSOCIATED(IETFLG))  DEALLOCATE(IETFLG)       !edm
      IF(ASSOCIATED(IUZFOPT)) DEALLOCATE(IUZFOPT)      !edm
      IF(ASSOCIATED(UZET))    DEALLOCATE(UZET)         !edm
      IF(ASSOCIATED(CUZET))   DEALLOCATE(CUZET)        !edm
      IF(ASSOCIATED(GWET))    DEALLOCATE(GWET)         !edm
      IF(ASSOCIATED(CGWET))   DEALLOCATE(CGWET)        !edm
      IF(ASSOCIATED(CSURFLK)) DEALLOCATE(CSURFLK)      !edm
      
      IF(ASSOCIATED(SS)) DEALLOCATE(SS)
      IF(ASSOCIATED(SSMC)) DEALLOCATE(SSMC)
      IF(ASSOCIATED(SSG)) DEALLOCATE(SSG)
!
      IF(ASSOCIATED(IREACT)) DEALLOCATE(IREACT)
      IF(ASSOCIATED(IRCTOP)) DEALLOCATE(IRCTOP)
      IF(ASSOCIATED(IGETSC)) DEALLOCATE(IGETSC)
      IF(ASSOCIATED(FRAC)) DEALLOCATE(FRAC)
      IF(ASSOCIATED(SP1)) DEALLOCATE(SP1)
      IF(ASSOCIATED(SP2)) DEALLOCATE(SP2)
      IF(ASSOCIATED(RC1)) DEALLOCATE(RC1)
      IF(ASSOCIATED(RC2)) DEALLOCATE(RC2)
!
      IF(ASSOCIATED(MaxConcObs)) DEALLOCATE(MaxConcObs)
      IF(ASSOCIATED(MaxFluxObs)) DEALLOCATE(MaxFluxObs)
      IF(ASSOCIATED(MaxFluxCells)) DEALLOCATE(MaxFluxCells)
      IF(ASSOCIATED(inConcObs)) DEALLOCATE(inConcObs)
      IF(ASSOCIATED(nConcObs)) DEALLOCATE(nConcObs)
      IF(ASSOCIATED(iOutCobs)) DEALLOCATE(iOutCobs)
      IF(ASSOCIATED(iConcLOG)) DEALLOCATE(iConcLOG)
      IF(ASSOCIATED(iConcINTP)) DEALLOCATE(iConcINTP)
      IF(ASSOCIATED(inFluxObs)) DEALLOCATE(inFluxObs)
      IF(ASSOCIATED(nFluxGroup)) DEALLOCATE(nFluxGroup)
      IF(ASSOCIATED(nFluxObs)) DEALLOCATE(nFluxObs)
      IF(ASSOCIATED(iOutFlux)) DEALLOCATE(iOutFlux)
      IF(ASSOCIATED(inSaveObs)) DEALLOCATE(inSaveObs)
      IF(ASSOCIATED(CScale)) DEALLOCATE(CScale)
      IF(ASSOCIATED(FScale)) DEALLOCATE(FScale)
      IF(ASSOCIATED(mLayer)) DEALLOCATE(mLayer)
      IF(ASSOCIATED(prLayer)) DEALLOCATE(prLayer)
      IF(ASSOCIATED(COBSWEL)) DEALLOCATE(COBSWEL)
      IF(ASSOCIATED(TEMP)) DEALLOCATE(TEMP)
      IF(ASSOCIATED(FluxGroup)) DEALLOCATE(FluxGroup)
      IF(ASSOCIATED(GroupData)) DEALLOCATE(GroupData)
      IF(ASSOCIATED(FOBSNAM)) DEALLOCATE(FOBSNAM)
      IF(ASSOCIATED(COBSNAM)) DEALLOCATE(COBSNAM)
!
      IF(ASSOCIATED(MXITER)) DEALLOCATE(MXITER)
      IF(ASSOCIATED(ITER1)) DEALLOCATE(ITER1)
      IF(ASSOCIATED(ISOLVE)) DEALLOCATE(ISOLVE)
      IF(ASSOCIATED(NCRS)) DEALLOCATE(NCRS)
      IF(ASSOCIATED(IPRGCG)) DEALLOCATE(IPRGCG)
      IF(ASSOCIATED(ACCL)) DEALLOCATE(ACCL)
      IF(ASSOCIATED(CCLOSE)) DEALLOCATE(CCLOSE)
      IF(ASSOCIATED(LRCH)) DEALLOCATE(LRCH)
      IF(ASSOCIATED(A)) DEALLOCATE(A)
      IF(ASSOCIATED(Q)) DEALLOCATE(Q)
      IF(ASSOCIATED(WK)) DEALLOCATE(WK)
      IF(ASSOCIATED(CNCG)) DEALLOCATE(CNCG)
      IF(ASSOCIATED(RHS)) DEALLOCATE(RHS)
      IF(ASSOCIATED(L)) DEALLOCATE(L)
!
      IF(ASSOCIATED(MaxHSSSource)) DEALLOCATE(MaxHSSSource)
      IF(ASSOCIATED(MaxHSSCells)) DEALLOCATE(MaxHSSCells)
      IF(ASSOCIATED(MaxHSSStep)) DEALLOCATE(MaxHSSStep)
      IF(ASSOCIATED(nHSSSource)) DEALLOCATE(nHSSSource)
      IF(ASSOCIATED(iRunHSSM)) DEALLOCATE(iRunHSSM)
      IF(ASSOCIATED(IHSSGEN)) DEALLOCATE(IHSSGEN)
      IF(ASSOCIATED(faclength)) DEALLOCATE(faclength)
      IF(ASSOCIATED(factime)) DEALLOCATE(factime)
      IF(ASSOCIATED(facmass)) DEALLOCATE(facmass)
      IF(ASSOCIATED(iHSSLoc)) DEALLOCATE(iHSSLoc)
      IF(ASSOCIATED(HSSData)) DEALLOCATE(HSSData)
      IF(ASSOCIATED(HSSNAM)) DEALLOCATE(HSSNAM)
!      
      IF(ASSOCIATED(MXCTS)) DEALLOCATE(MXCTS)
      IF(ASSOCIATED(ICTSOUT)) DEALLOCATE(ICTSOUT)
      IF(ASSOCIATED(MXEXT)) DEALLOCATE(MXEXT)
      IF(ASSOCIATED(MXINJ)) DEALLOCATE(MXINJ)
      IF(ASSOCIATED(MXWEL)) DEALLOCATE(MXWEL)
      IF(ASSOCIATED(IFORCE)) DEALLOCATE(IFORCE)
      IF(ASSOCIATED(NCTS)) DEALLOCATE(NCTS)
      IF(ASSOCIATED(NCTSOLD)) DEALLOCATE(NCTSOLD)
      IF(ASSOCIATED(NEXT)) DEALLOCATE(NEXT)
      IF(ASSOCIATED(NINJ)) DEALLOCATE(NINJ)
      IF(ASSOCIATED(ITRTEXT)) DEALLOCATE(ITRTEXT)
      IF(ASSOCIATED(ITRTINJ)) DEALLOCATE(ITRTINJ)
      IF(ASSOCIATED(IOPTEXT)) DEALLOCATE(IOPTEXT)
      IF(ASSOCIATED(IOPTINJ)) DEALLOCATE(IOPTINJ)
      IF(ASSOCIATED(IOPTEXT)) DEALLOCATE(IOPTEXT)
      IF(ASSOCIATED(CMCHGEXT)) DEALLOCATE(CMCHGEXT)
      IF(ASSOCIATED(CMCHGINJ)) DEALLOCATE(CMCHGINJ)
      IF(ASSOCIATED(KEXT)) DEALLOCATE(KEXT)
      IF(ASSOCIATED(IEXT)) DEALLOCATE(IEXT)
      IF(ASSOCIATED(JEXT)) DEALLOCATE(JEXT)
      IF(ASSOCIATED(KINJ)) DEALLOCATE(KINJ)
      IF(ASSOCIATED(IINJ)) DEALLOCATE(IINJ)
      IF(ASSOCIATED(JINJ)) DEALLOCATE(JINJ)
      IF(ASSOCIATED(QINCTS)) DEALLOCATE(QINCTS)
      IF(ASSOCIATED(QOUTCTS)) DEALLOCATE(QOUTCTS)
      IF(ASSOCIATED(CNTE)) DEALLOCATE(CNTE)
      IF(ASSOCIATED(IWEXT)) DEALLOCATE(IWEXT)
      IF(ASSOCIATED(IWINJ)) DEALLOCATE(IWINJ)
      IF(ASSOCIATED(IWCTS)) DEALLOCATE(IWCTS)
      IF(ASSOCIATED(QCTS)) DEALLOCATE(QCTS)
      IF(ASSOCIATED(CCTS)) DEALLOCATE(CCTS)
      IF(ASSOCIATED(CEXT2CTS)) DEALLOCATE(CEXT2CTS)
      IF(ASSOCIATED(CGW2CTS)) DEALLOCATE(CGW2CTS)
      IF(ASSOCIATED(CADDM)) DEALLOCATE(CADDM)
      IF(ASSOCIATED(CCTS2EXT)) DEALLOCATE(CCTS2EXT)
      IF(ASSOCIATED(CCTS2GW)) DEALLOCATE(CCTS2GW)
      IF(ASSOCIATED(CREMM)) DEALLOCATE(CREMM)
      
      END SUBROUTINE MEMDEALLOCATE  
                           
END MODULE MT3DMS_MODULE   