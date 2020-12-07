    MODULE INTERFACE1
      INTERFACE
!
        SUBROUTINE  READFLOWS(INUF,IOUT,NCOL,NROW,NLAY,KSTP,KPER,TEXT, &
                    ICBUND,FPRT,NCOMP,NNGW,IPSGS,INFL,INFR,INFC,QNGW,ICID,QAUX, &
                    NNINIT,NNODES,NFLOWTYPE,PKGFLOWS,CFLOWTYPE, &
                    NRCHCON,INOD1,INOD2,IDISP,QAREA,QN2N)
          IMPLICIT  NONE
          INTEGER   KSTP,KPER,INUF,NCOL,NROW,NLAY,IOUT,K,I,J,KKSTP,KKPER, &
                    NC,NR,NL,IFL,IPSGS
          INTEGER   NUM,N,ICBUND,IQ,ID, &
                    KKK,III,JJJ,ITEMP,NCOMP,ICNT,IS,IR,NN,NNINIT
          CHARACTER TEXT*16,FPRT*1,LABEL*16
          DIMENSION ICBUND(NCOL,NROW,NLAY)
          INTEGER   NSFINIT,NNODES,NFLOWTYPE,NRCHCON,NNGW
          INTEGER, DIMENSION(:), POINTER :: INFL,INFR,INFC
          REAL,    DIMENSION(:), POINTER :: QNGW
          REAL,         ALLOCATABLE      :: PKGFLOWS(:,:),QAREA(:), &
                                            QN2N(:)
          CHARACTER*16, ALLOCATABLE      :: CFLOWTYPE(:)
          INTEGER,      ALLOCATABLE      :: INOD1(:),INOD2(:),IDISP(:)
          INTEGER,      ALLOCATABLE      :: ICID(:)
          REAL,         ALLOCATABLE      :: QAUX(:)
        END SUBROUTINE
!
        SUBROUTINE DEALOCTEMPARR(PKGFLOWS,QAREA,QN2N,CFLOWTYPE,INOD1, &
          INOD2,IDISP,ICID,QAUX,IP2PFLG)
          REAL,         ALLOCATABLE      :: PKGFLOWS(:,:),QAREA(:), &
                                            QN2N(:)
          CHARACTER*16, ALLOCATABLE      :: CFLOWTYPE(:)
          INTEGER,      ALLOCATABLE      :: INOD1(:),INOD2(:),IDISP(:)
          INTEGER,      ALLOCATABLE      :: ICID(:)
          REAL,         ALLOCATABLE      :: QAUX(:)
          INTEGER,      ALLOCATABLE      :: IP2PFLG(:)
        END SUBROUTINE
!
        SUBROUTINE PKG2PKGFLOW(INUF,IOUT,KSTP,KPER,TEXT,FPRT, &
                               NPKG2PKG,INOD1,INOD2,QN1N2,IP2PFLG)
          IMPLICIT  NONE
          INTEGER   KSTP,KPER,INUF,IOUT,KKSTP,KKPER,NPKG2PKG
          INTEGER   NUM,N,ICBUND,IQ,ID,ITEMP,NCOMP
          CHARACTER TEXT*16,FPRT*1,LABEL*16
          INTEGER, DIMENSION(:), POINTER :: INOD1,INOD2
          REAL,    DIMENSION(:), POINTER :: QN1N2
          INTEGER, ALLOCATABLE           :: IP2PFLG(:)
        END SUBROUTINE
!
      END INTERFACE
    END MODULE
!
    MODULE PKG2PKG
        INTEGER,          SAVE,                      POINTER :: NSFR2LAK
        INTEGER,          SAVE,                      POINTER :: NSFR2UZF
        INTEGER,          SAVE,                      POINTER :: NLAK2UZF
        INTEGER,          SAVE,                      POINTER :: NSNK2UZF
        REAL,             SAVE, DIMENSION(:),        POINTER :: QSFR2LAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: QSFR2UZF
        REAL,             SAVE, DIMENSION(:),        POINTER :: QLAK2UZF
        REAL,             SAVE, DIMENSION(:),        POINTER :: QSNK2UZF
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD1SFLK
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD2SFLK
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD1SFUZ
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD2SFUZ
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD1LKUZ
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD2LKUZ
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD1SKUZ
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD2SKUZ
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IUZCODESF
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IUZCODELK
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IUZCODESK
      CONTAINS
      SUBROUTINE MEMDEALLOCATE_PKG2PKG()
        IF(ASSOCIATED(QSFR2LAK))       DEALLOCATE(QSFR2LAK)
        IF(ASSOCIATED(QSFR2UZF))       DEALLOCATE(QSFR2UZF)
        IF(ASSOCIATED(QLAK2UZF))       DEALLOCATE(QLAK2UZF)
        IF(ASSOCIATED(QSNK2UZF))       DEALLOCATE(QSNK2UZF)
        IF(ASSOCIATED(INOD1SFLK))      DEALLOCATE(INOD1SFLK)
        IF(ASSOCIATED(INOD2SFLK))      DEALLOCATE(INOD2SFLK)
        IF(ASSOCIATED(INOD1SFUZ))      DEALLOCATE(INOD1SFUZ)
        IF(ASSOCIATED(INOD2SFUZ))      DEALLOCATE(INOD2SFUZ)
        IF(ASSOCIATED(INOD1LKUZ))      DEALLOCATE(INOD1LKUZ)
        IF(ASSOCIATED(INOD2LKUZ))      DEALLOCATE(INOD2LKUZ)
        IF(ASSOCIATED(INOD1SKUZ))      DEALLOCATE(INOD1SKUZ)
        IF(ASSOCIATED(INOD2SKUZ))      DEALLOCATE(INOD2SKUZ)
        IF(ASSOCIATED(IUZCODESF))      DEALLOCATE(IUZCODESF)
        IF(ASSOCIATED(IUZCODELK))      DEALLOCATE(IUZCODELK)
        IF(ASSOCIATED(IUZCODESK))      DEALLOCATE(IUZCODESK)
      END SUBROUTINE MEMDEALLOCATE_PKG2PKG
    END MODULE PKG2PKG
!
    MODULE DSSL
        INTEGER,          SAVE,                      POINTER :: IHSSOUT
        INTEGER,          SAVE,                      POINTER :: MAXDSSL
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IDSSL
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IDSSLCOMP
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: NSLDPHS
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISA
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IDSSLDOM
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: ANION
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: ANION2
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CATION
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IDSSOPT
        REAL,             SAVE, DIMENSION(:),        POINTER :: CONVMOL
        REAL,             SAVE, DIMENSION(:),        POINTER :: ALDSSL
        REAL,             SAVE, DIMENSION(:),        POINTER :: PORDSS
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: XB
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: XC
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: SOLU
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: RNEUTRATE
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: SSA
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: WGHTMOL
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: POWR
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: POWR2
        REAL,             SAVE, DIMENSION(:,:,:,:),  POINTER :: CRESNEW
        REAL,             SAVE, DIMENSION(:,:,:,:),  POINTER :: CRESOLD
        REAL,             SAVE, DIMENSION(:,:,:,:),  POINTER :: CRESINIT
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CSOLNEW
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CSOLOLD
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: SRCMASS
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: THKDSS
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: TSDSS
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: TEDSS
      CONTAINS
      SUBROUTINE MEMDEALLOCATE_DSSL()
        IF(ASSOCIATED(IDSSL))       DEALLOCATE(IDSSL)
        IF(ASSOCIATED(IDSSLCOMP))   DEALLOCATE(IDSSLCOMP)
        IF(ASSOCIATED(NSLDPHS))     DEALLOCATE(NSLDPHS)
        IF(ASSOCIATED(ISA))         DEALLOCATE(ISA)
        IF(ASSOCIATED(CONVMOL))     DEALLOCATE(CONVMOL)
        IF(ASSOCIATED(IDSSLDOM))    DEALLOCATE(IDSSLDOM)
        IF(ASSOCIATED(ANION))       DEALLOCATE(ANION)
        IF(ASSOCIATED(ANION2))      DEALLOCATE(ANION2)
        IF(ASSOCIATED(CATION))      DEALLOCATE(CATION)
        IF(ASSOCIATED(IDSSOPT))     DEALLOCATE(IDSSOPT)
        IF(ASSOCIATED(ALDSSL))      DEALLOCATE(ALDSSL)
        IF(ASSOCIATED(PORDSS))      DEALLOCATE(PORDSS)
        IF(ASSOCIATED(XB))          DEALLOCATE(XB)
        IF(ASSOCIATED(XC))          DEALLOCATE(XC)
        IF(ASSOCIATED(SOLU))        DEALLOCATE(SOLU)
        IF(ASSOCIATED(RNEUTRATE))   DEALLOCATE(RNEUTRATE)
        IF(ASSOCIATED(SSA))         DEALLOCATE(SSA)
        IF(ASSOCIATED(WGHTMOL))     DEALLOCATE(WGHTMOL)
        IF(ASSOCIATED(POWR))        DEALLOCATE(POWR)
        IF(ASSOCIATED(POWR2))       DEALLOCATE(POWR2)
        IF(ASSOCIATED(CRESNEW))     DEALLOCATE(CRESNEW)
        IF(ASSOCIATED(CRESOLD))     DEALLOCATE(CRESOLD)
        IF(ASSOCIATED(CRESINIT))    DEALLOCATE(CRESINIT)
        IF(ASSOCIATED(CSOLNEW))     DEALLOCATE(CSOLNEW)
        IF(ASSOCIATED(CSOLOLD))     DEALLOCATE(CSOLOLD)
        IF(ASSOCIATED(SRCMASS))     DEALLOCATE(SRCMASS)
        IF(ASSOCIATED(THKDSS))      DEALLOCATE(THKDSS)
        IF(ASSOCIATED(TSDSS))       DEALLOCATE(TSDSS)
        IF(ASSOCIATED(TEDSS))       DEALLOCATE(TEDSS)
      END SUBROUTINE MEMDEALLOCATE_DSSL
    END MODULE DSSL
!
    MODULE MIN_SAT
        LOGICAL,          SAVE,                      POINTER :: DOMINSAT
        LOGICAL,          SAVE,                      POINTER :: DRYON
        INTEGER,          SAVE, DIMENSION(:,:,:),    POINTER :: ICBND2 
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ID2D
        REAL,             SAVE, DIMENSION(:,:,:,:),  POINTER :: QC7
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: C7
        REAL,             SAVE, DIMENSION(:,:,:,:),  POINTER :: COLD7
        REAL,             SAVE, DIMENSION(:,:,:),    POINTER :: VAQSAT
        REAL,             SAVE, DIMENSION(:,:,:),    POINTER :: TMASS2
        INTEGER,          SAVE,                      POINTER :: ICNTDRY
        INTEGER,          SAVE,                      POINTER :: NCNTDRY
        INTEGER,          SAVE,                      POINTER :: NICBND2
        INTEGER,          SAVE,                      POINTER :: MUTDRY
        INTEGER,          SAVE,                      POINTER :: IC2DRY
        INTEGER,          SAVE,                      POINTER :: IABSMIN
        INTEGER,          SAVE,                      POINTER :: IDRYBUD
        INTEGER,          SAVE,                      POINTER :: IATS
        INTEGER,          SAVE,                      POINTER :: ICIMDRY
    END MODULE MIN_SAT
!
    MODULE SFRVARS
        INTEGER,          SAVE,                      POINTER :: NSTRM
        INTEGER,          SAVE,                      POINTER :: ISFRBC
        INTEGER,          SAVE,                      POINTER :: MXSFBC
        INTEGER,          SAVE,                      POINTER :: NSSSF
        INTEGER,          SAVE,                      POINTER :: NSFINIT
        INTEGER,          SAVE,                      POINTER :: ICBCSF
        INTEGER,          SAVE,                      POINTER :: IETSFR
        INTEGER,          SAVE,                      POINTER :: ISFTTR
        INTEGER,          SAVE,                      POINTER :: ISFSOLV
        INTEGER,          SAVE,                      POINTER :: MXITERSF
        INTEGER,          SAVE,                      POINTER :: IPRTXMD
        REAL,             SAVE,                      POINTER :: WIMP
        REAL,             SAVE,                      POINTER :: WUPS
        REAL,             SAVE,                      POINTER :: CCLOSESF
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFL
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFR
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFC
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISEG
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: IREACH
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
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),      POINTER :: COLDSF
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),      POINTER :: COLDSF2
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: BUFFSF
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CNEWSFTMP
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: DISPSF
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CBCSF
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFNBC
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFBCTYP
        REAL,             SAVE, DIMENSION(:),        POINTER :: RMASSF
        REAL,             SAVE, DIMENSION(:),        POINTER :: VOUTSF
        DOUBLE PRECISION, SAVE, DIMENSION(:),        POINTER :: VOLSFO
        DOUBLE PRECISION, SAVE, DIMENSION(:),        POINTER :: VOLSFN
!
        INTEGER,          SAVE,                      POINTER :: MXSGMT
        INTEGER,          SAVE,                      POINTER :: MXRCH
!
!.......OBSERVATION VARIABLES
        INTEGER,          SAVE,                      POINTER :: NOBSSF
        INTEGER,          SAVE,                      POINTER :: IOUTOBS
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ISFNOBS
!
!.......SIZE NSF2SF
        INTEGER,          SAVE,                      POINTER :: NSF2SF
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD1SF
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: INOD2SF
        REAL,             SAVE, DIMENSION(:),        POINTER :: QN2NSF
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
        REAL,             SAVE, DIMENSION(:),        POINTER :: CUZF2SFRGW
        REAL,             SAVE, DIMENSION(:),        POINTER :: CUZF2SFRINF
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
        REAL,             SAVE, DIMENSION(:),        POINTER :: CLOSTMASS
        REAL,             SAVE, DIMENSION(:),        POINTER :: CGAINEDMASS
    CONTAINS
      SUBROUTINE MEMDEALLOCATE3()
        IF(ASSOCIATED(IASF))   DEALLOCATE(IASF)  
        IF(ASSOCIATED(RHSSF))  DEALLOCATE(RHSSF) 
        IF(ASSOCIATED(JASF))   DEALLOCATE(JASF)  
        IF(ASSOCIATED(AMATSF)) DEALLOCATE(AMATSF)
      END SUBROUTINE MEMDEALLOCATE3
!--DEALLOCATE AND ALLOCATE ARRAYS FOR STORING FLOW TERMS
      SUBROUTINE MEMDEALLOCATE5()
        IF(ASSOCIATED(ISFL))       DEALLOCATE(ISFL)     
        IF(ASSOCIATED(ISFR))       DEALLOCATE(ISFR)     
        IF(ASSOCIATED(ISFC))       DEALLOCATE(ISFC)     
        IF(ASSOCIATED(ISEG))       DEALLOCATE(ISEG)     
        IF(ASSOCIATED(IREACH))     DEALLOCATE(IREACH)   
        IF(ASSOCIATED(SFLEN))      DEALLOCATE(SFLEN)    
        IF(ASSOCIATED(SFNAREA))    DEALLOCATE(SFNAREA)  
        IF(ASSOCIATED(SFOAREA))    DEALLOCATE(SFOAREA)  
        IF(ASSOCIATED(QPRECSF))    DEALLOCATE(QPRECSF)  
        IF(ASSOCIATED(QRUNOFSF))   DEALLOCATE(QRUNOFSF) 
        IF(ASSOCIATED(QPRECSFO))   DEALLOCATE(QPRECSFO) 
        IF(ASSOCIATED(QRUNOFSFO))  DEALLOCATE(QRUNOFSFO)
        IF(ASSOCIATED(QSFGW))      DEALLOCATE(QSFGW)    
        IF(ASSOCIATED(QOUTSF))     DEALLOCATE(QOUTSF)   
        IF(ASSOCIATED(QOUTSFO))    DEALLOCATE(QOUTSFO)  
        IF(ASSOCIATED(QETSF))      DEALLOCATE(QETSF)    
        IF(ASSOCIATED(IDSPFLG))    DEALLOCATE(IDSPFLG)  
!.......INDEXING TO GET ISTRM FROM SEG AND RCH NUMBERS  
        IF(ASSOCIATED(CNEWSF))      DEALLOCATE(CNEWSF)
        IF(ASSOCIATED(COLDSF))      DEALLOCATE(COLDSF)
        IF(ASSOCIATED(COLDSF2))     DEALLOCATE(COLDSF2)
        IF(ASSOCIATED(CNEWSFTMP))   DEALLOCATE(CNEWSFTMP)
        IF(ASSOCIATED(DISPSF))      DEALLOCATE(DISPSF)
        IF(ASSOCIATED(CBCSF))       DEALLOCATE(CBCSF)
        IF(ASSOCIATED(ISFNBC))      DEALLOCATE(ISFNBC)
        IF(ASSOCIATED(ISFBCTYP))    DEALLOCATE(ISFBCTYP)
        IF(ASSOCIATED(RMASSF))      DEALLOCATE(RMASSF)
        IF(ASSOCIATED(VOUTSF))      DEALLOCATE(VOUTSF)
        IF(ASSOCIATED(VOLSFO))      DEALLOCATE(VOLSFO)
        IF(ASSOCIATED(VOLSFN))      DEALLOCATE(VOLSFN)
        IF(ASSOCIATED(ISFNOBS))     DEALLOCATE(ISFNOBS)
        IF(ASSOCIATED(CFLOINSF))    DEALLOCATE(CFLOINSF)
        IF(ASSOCIATED(CFLOOUTSF))   DEALLOCATE(CFLOOUTSF)
        IF(ASSOCIATED(CGW2SFR))     DEALLOCATE(CGW2SFR)
        IF(ASSOCIATED(CUZF2SFRGW))    DEALLOCATE(CUZF2SFRGW)
        IF(ASSOCIATED(CUZF2SFRINF))    DEALLOCATE(CUZF2SFRINF)
        IF(ASSOCIATED(CGWFROMSFR))  DEALLOCATE(CGWFROMSFR)
        IF(ASSOCIATED(CLAK2SFR))    DEALLOCATE(CLAK2SFR)
        IF(ASSOCIATED(CLAKFROMSFR)) DEALLOCATE(CLAKFROMSFR)
        IF(ASSOCIATED(CPRECSF))     DEALLOCATE(CPRECSF)
        IF(ASSOCIATED(CRUNOFSF))    DEALLOCATE(CRUNOFSF)
        IF(ASSOCIATED(CETSF))       DEALLOCATE(CETSF)
        IF(ASSOCIATED(CSTORINSF))   DEALLOCATE(CSTORINSF)
        IF(ASSOCIATED(CSTOROTSF))   DEALLOCATE(CSTOROTSF)
        IF(ASSOCIATED(CCCINSF))     DEALLOCATE(CCCINSF)
        IF(ASSOCIATED(CCCOUTSF))    DEALLOCATE(CCCOUTSF)
      END SUBROUTINE MEMDEALLOCATE5
    END MODULE SFRVARS
!
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
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),      POINTER :: CNEWLAK
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),      POINTER :: COLDLAK
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: BUFFLAK
        REAL,             SAVE, DIMENSION(:,:),      POINTER :: CBCLK
        REAL,             SAVE, DIMENSION(:),        POINTER :: QPRECLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: QRUNOFLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: QWDRLLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: QLAKGW
        REAL,             SAVE, DIMENSION(:),        POINTER :: QLAKSFR
        REAL,             SAVE, DIMENSION(:),        POINTER :: QETLAK
        DOUBLE PRECISION, SAVE, DIMENSION(:),        POINTER :: VOLNLAK
        DOUBLE PRECISION, SAVE, DIMENSION(:),        POINTER :: VOLOLAK
        DOUBLE PRECISION, SAVE, DIMENSION(:),        POINTER :: DELVOLLAK
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ILKBC
        INTEGER,          SAVE, DIMENSION(:),        POINTER :: ILKBCTYP
        REAL,             SAVE, DIMENSION(:),        POINTER :: RMASLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: VOUTLAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CGW2LAK
        REAL,             SAVE, DIMENSION(:),        POINTER :: CUZF2LAK
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
        IF(ASSOCIATED(LAKL))        DEALLOCATE(LAKL)
        IF(ASSOCIATED(LAKR))        DEALLOCATE(LAKR)
        IF(ASSOCIATED(LAKC))        DEALLOCATE(LAKC)
        IF(ASSOCIATED(LAKNUMGW))    DEALLOCATE(LAKNUMGW)
        IF(ASSOCIATED(LAKRCH))      DEALLOCATE(LAKRCH)
        IF(ASSOCIATED(LAKSEG))      DEALLOCATE(LAKSEG)
        IF(ASSOCIATED(LAKNUMSFR))   DEALLOCATE(LAKNUMSFR)
        IF(ASSOCIATED(CNEWLAK))     DEALLOCATE(CNEWLAK)
        IF(ASSOCIATED(COLDLAK))     DEALLOCATE(COLDLAK)
        IF(ASSOCIATED(CBCLK))       DEALLOCATE(CBCLK)
        IF(ASSOCIATED(QPRECLAK))    DEALLOCATE(QPRECLAK)
        IF(ASSOCIATED(QRUNOFLAK))   DEALLOCATE(QRUNOFLAK)
        IF(ASSOCIATED(QWDRLLAK))    DEALLOCATE(QWDRLLAK)
        IF(ASSOCIATED(QLAKGW))      DEALLOCATE(QLAKGW)
        IF(ASSOCIATED(QLAKSFR))     DEALLOCATE(QLAKSFR)
        IF(ASSOCIATED(QETLAK))      DEALLOCATE(QETLAK)
        IF(ASSOCIATED(VOLNLAK))     DEALLOCATE(VOLNLAK)
        IF(ASSOCIATED(VOLOLAK))     DEALLOCATE(VOLOLAK)
        IF(ASSOCIATED(DELVOLLAK))   DEALLOCATE(DELVOLLAK)
        IF(ASSOCIATED(ILKBC))       DEALLOCATE(ILKBC)
        IF(ASSOCIATED(ILKBCTYP))    DEALLOCATE(ILKBCTYP)
        IF(ASSOCIATED(RMASLAK))     DEALLOCATE(RMASLAK)
        IF(ASSOCIATED(VOUTLAK))     DEALLOCATE(VOUTLAK)
        IF(ASSOCIATED(CGW2LAK))     DEALLOCATE(CGW2LAK)
        IF(ASSOCIATED(CUZF2LAK))    DEALLOCATE(CUZF2LAK)
        IF(ASSOCIATED(CGWFROMLAK))  DEALLOCATE(CGWFROMLAK)
        IF(ASSOCIATED(CSFR2LAK))    DEALLOCATE(CSFR2LAK)
        IF(ASSOCIATED(CSFRFROMLAK)) DEALLOCATE(CSFRFROMLAK)
        IF(ASSOCIATED(CPRECLK))     DEALLOCATE(CPRECLK)
        IF(ASSOCIATED(CRUNOFLK))    DEALLOCATE(CRUNOFLK)
        IF(ASSOCIATED(CWDRLLK))     DEALLOCATE(CWDRLLK)
        IF(ASSOCIATED(CETLK))       DEALLOCATE(CETLK)
      END SUBROUTINE MEMDEALLOCATE4
      SUBROUTINE MEMDEALLOCATE6()
        IF(ASSOCIATED(LAKL))        DEALLOCATE(LAKL)            
        IF(ASSOCIATED(LAKR))        DEALLOCATE(LAKR)            
        IF(ASSOCIATED(LAKC))        DEALLOCATE(LAKC)            
        IF(ASSOCIATED(LAKRCH))      DEALLOCATE(LAKRCH)        
        IF(ASSOCIATED(LAKSEG))      DEALLOCATE(LAKSEG)        
        IF(ASSOCIATED(QPRECLAK))    DEALLOCATE(QPRECLAK)    
        IF(ASSOCIATED(QRUNOFLAK))   DEALLOCATE(QRUNOFLAK)  
        IF(ASSOCIATED(QWDRLLAK))    DEALLOCATE(QWDRLLAK)    
        IF(ASSOCIATED(QLAKGW))      DEALLOCATE(QLAKGW)        
        IF(ASSOCIATED(QLAKSFR))     DEALLOCATE(QLAKSFR)      
        IF(ASSOCIATED(QETLAK))      DEALLOCATE(QETLAK)        
        IF(ASSOCIATED(VOLNLAK))     DEALLOCATE(VOLNLAK)      
        IF(ASSOCIATED(VOLOLAK))     DEALLOCATE(VOLOLAK)      
        IF(ASSOCIATED(DELVOLLAK))   DEALLOCATE(DELVOLLAK)  
        IF(ASSOCIATED(LAKNUMGW))    DEALLOCATE(LAKNUMGW)    
        IF(ASSOCIATED(LAKNUMSFR))   DEALLOCATE(LAKNUMSFR)
      END SUBROUTINE MEMDEALLOCATE6
    END MODULE LAKVARS
!
    MODULE RCTMOD
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
        CHARACTER(LEN=1000),SAVE,                      POINTER :: rec_FileName
        CHARACTER(LEN=50),  SAVE,                      POINTER :: Ad_methane_name
        INTEGER,            SAVE, DIMENSION(:),        POINTER :: NSOLID
        INTEGER,            SAVE, DIMENSION(:),        POINTER :: ISLDPH
        INTEGER,            SAVE, DIMENSION(:),        POINTER :: NCRSPIM
        REAL,               SAVE, DIMENSION(:,:,:),    POINTER :: MASSSTOR
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: RCNEW 
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: RCOLD
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: MAXEC
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: INHIB
        REAL*8,             SAVE, DIMENSION(:,:),      POINTER :: DECAY
        REAL*8,             SAVE, DIMENSION(:,:),      POINTER :: YIELDC
        REAL*8,             SAVE, DIMENSION(:,:,:),    POINTER :: DCDT_FE
        REAL*8,             SAVE, DIMENSION(:,:),      POINTER :: DCDT_S 
        REAL*8,             SAVE, DIMENSION(:,:),      POINTER :: DCDT_SYLD
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: DEA_ED_DT
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: DCDT
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: DCDTYLD
        REAL*8,             SAVE, DIMENSION(:),        POINTER :: SWITCH
        CHARACTER(LEN=5),   SAVE, DIMENSION(:),        POINTER :: SPECIAL
        REAL*8,             SAVE,                      POINTER :: RVAL
        REAL,               SAVE, DIMENSION(:),        POINTER :: MASS_NEG
        REAL,               SAVE, DIMENSION(:),        POINTER :: CON_NEG
        REAL,               SAVE, DIMENSION(:,:,:,:),  POINTER :: SP1IM
        INTEGER,            SAVE,                      POINTER :: ISP1IM
CONTAINS
      SUBROUTINE MEMDEALLOCATE2()
!
      IF(ASSOCIATED(NSOLID))      DEALLOCATE(NSOLID)
      IF(ASSOCIATED(ISLDPH))      DEALLOCATE(ISLDPH)
      IF(ASSOCIATED(NCRSPIM))     DEALLOCATE(NCRSPIM)
      IF(ASSOCIATED(CRCT))        DEALLOCATE(CRCT)      
      IF(ASSOCIATED(MASSSTOR))    DEALLOCATE(MASSSTOR)  
      IF(ASSOCIATED(RCNEW))       DEALLOCATE(RCNEW)        
      IF(ASSOCIATED(RCOLD))       DEALLOCATE(RCOLD)        
      IF(ASSOCIATED(MAXEC))       DEALLOCATE(MAXEC)        
      IF(ASSOCIATED(INHIB))       DEALLOCATE(INHIB)        
      IF(ASSOCIATED(DECAY))       DEALLOCATE(DECAY)        
      IF(ASSOCIATED(YIELDC))      DEALLOCATE(YIELDC)      
      IF(ASSOCIATED(DCDT_FE))     DEALLOCATE(DCDT_FE)    
      IF(ASSOCIATED(DCDT_S))      DEALLOCATE(DCDT_S)      
      IF(ASSOCIATED(DCDT_SYLD))   DEALLOCATE(DCDT_SYLD)
      IF(ASSOCIATED(DEA_ED_DT))   DEALLOCATE(DEA_ED_DT)
      IF(ASSOCIATED(DCDT))        DEALLOCATE(DCDT)          
      IF(ASSOCIATED(DCDTYLD))     DEALLOCATE(DCDTYLD)    
      IF(ASSOCIATED(SWITCH))      DEALLOCATE(SWITCH)      
      IF(ASSOCIATED(SPECIAL))     DEALLOCATE(SPECIAL)    
      IF(ASSOCIATED(MASS_NEG))    DEALLOCATE(MASS_NEG)  
      IF(ASSOCIATED(CON_NEG))     DEALLOCATE(CON_NEG)    
      END SUBROUTINE MEMDEALLOCATE2
    END MODULE RCTMOD
!
    MODULE UZTVARS
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: PRSITYSAV  
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: SATOLD     
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: SATNEW     
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IUZFBND    
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: WC         
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: THETAW
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: UZFLX      
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: UZQSTO     
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: SDH        
        LOGICAL,          SAVE,                       POINTER :: IETFLG     
        INTEGER,          SAVE,                       POINTER :: IUZFOPTG   
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IUZFOPT    
        INTEGER,          SAVE,                       POINTER :: ICBCUZ
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: UZET       
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: CUZET      
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IGWET       
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: GWET       
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CGWET
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: FINFIL      
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CUZINF
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IUZRCH
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: UZRECH
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CUZRCH
    CONTAINS
        SUBROUTINE MEMDEALLOCATE_UZ()
        IF(ASSOCIATED(PRSITYSAV))    DEALLOCATE(PRSITYSAV)
        IF(ASSOCIATED(SATOLD))       DEALLOCATE(SATOLD)   
        IF(ASSOCIATED(SATNEW))       DEALLOCATE(SATNEW)   
        IF(ASSOCIATED(SDH))          DEALLOCATE(SDH)      
        IF(ASSOCIATED(IUZFBND))      DEALLOCATE(IUZFBND)  
        IF(ASSOCIATED(WC))           DEALLOCATE(WC)       
        IF(ASSOCIATED(THETAW))       DEALLOCATE(THETAW)   
        IF(ASSOCIATED(UZFLX))        DEALLOCATE(UZFLX)    
        IF(ASSOCIATED(UZQSTO))       DEALLOCATE(UZQSTO)   
        IF(ASSOCIATED(FINFIL))       DEALLOCATE(FINFIL)   
        IF(ASSOCIATED(CUZINF))       DEALLOCATE(CUZINF)   
        IF(ASSOCIATED(IUZFOPTG))     DEALLOCATE(IUZFOPTG) 
        IF(ASSOCIATED(IETFLG))       DEALLOCATE(IETFLG)   
        IF(ASSOCIATED(IUZFOPT))      DEALLOCATE(IUZFOPT)  
        IF(ASSOCIATED(ICBCUZ))       DEALLOCATE(ICBCUZ)   
        IF(ASSOCIATED(UZET))         DEALLOCATE(UZET)     
        IF(ASSOCIATED(CUZET))        DEALLOCATE(CUZET)    
        IF(ASSOCIATED(IGWET))        DEALLOCATE(IGWET)    
        IF(ASSOCIATED(GWET))         DEALLOCATE(GWET)     
        IF(ASSOCIATED(CGWET))        DEALLOCATE(CGWET)    
        IF(ASSOCIATED(FINFIL))       DEALLOCATE(FINFIL)   
        IF(ASSOCIATED(CUZINF))       DEALLOCATE(CUZINF)   
        IF(ASSOCIATED(IUZRCH))       DEALLOCATE(IUZRCH)   
        IF(ASSOCIATED(UZRECH))       DEALLOCATE(UZRECH)   
        IF(ASSOCIATED(CUZRCH))       DEALLOCATE(CUZRCH)   
        END SUBROUTINE MEMDEALLOCATE_UZ
    END MODULE UZTVARS
!
    MODULE MT3DMS_MODULE
        INTEGER,PARAMETER :: MXTRNOP=20,MXSTP=9000
        CHARACTER(LEN=4), SAVE, DIMENSION(MXTRNOP) :: NameTRNOP=        &
     &  (/'ADV ', 'DSP ', 'SSM ', 'RCT ', 'GCG ',                       &
     &    '    ', 'UZT2', '    ', '    ', '    ',                       &
     &    'TOB ', '    ', 'HSS ', 'TSO ', 'RTR ',                       &
     &    '    ', '    ', 'LKT ', 'SFT ', 'CTS '/)
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
        INTEGER,          SAVE,                       POINTER :: INCTS
        INTEGER,          SAVE,                       POINTER :: INTSO
        INTEGER,          SAVE,                       POINTER :: INRCT     
        INTEGER,          SAVE,                       POINTER :: INGCG     
        INTEGER,          SAVE,                       POINTER :: INTOB
        INTEGER,          SAVE,                       POINTER :: INUZT
        INTEGER,          SAVE,                       POINTER :: INHSS
        INTEGER,          SAVE,                       POINTER :: INFTL     
        INTEGER,          SAVE,                       POINTER :: NPCKGTXT
        INTEGER,          SAVE,                       POINTER :: IFTLFMT   
        INTEGER,          SAVE,                       POINTER :: ICNF      
        INTEGER,          SAVE,                       POINTER :: IUCN      
        INTEGER,          SAVE,                       POINTER :: IUCN2     
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: ISAVUCN
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
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: HTOP
        REAL,             SAVE, DIMENSION(:),         POINTER :: XBC
        REAL,             SAVE, DIMENSION(:),         POINTER :: YBC
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: ZBC
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: CNEW
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: COLD
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: QX
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: QY
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: QZ
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: QSTO
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: DH
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
        INTEGER,          SAVE,                       POINTER :: IFTL
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
        LOGICAL,          SAVE,                       POINTER :: FLAKFLOWS
        LOGICAL,          SAVE,                       POINTER :: FMNWFLOWS !NOT SUPPORTED YET
        LOGICAL,          SAVE,                       POINTER :: FSFRFLOWS
        LOGICAL,          SAVE,                       POINTER :: FUZFFLOWS
        LOGICAL,          SAVE,                       POINTER :: FSWR
        LOGICAL,          SAVE,                       POINTER :: FSWRFLOWS !NOT SUPPORTED YET
        LOGICAL,          SAVE,                       POINTER :: FSFRLAK
        LOGICAL,          SAVE,                       POINTER :: FSFRUZF
        LOGICAL,          SAVE,                       POINTER :: FLAKUZF
        LOGICAL,          SAVE,                       POINTER :: FSNKUZF
        LOGICAL,          SAVE,                       POINTER :: FMIFMT6
        INTEGER,          SAVE,                       POINTER :: NOCREWET
!--SSM                    
        INTEGER,          SAVE,                       POINTER :: ISSGOUT
        INTEGER,          SAVE,                       POINTER :: MXSS
        INTEGER,          SAVE,                       POINTER :: NSS
        INTEGER,          SAVE,                       POINTER :: NTSS
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: RECH
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IRCH
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CRCH
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: EVTR
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IEVT
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CEVT
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: SS
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: SSMC
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: SSG
        INTEGER,          SAVE,                       POINTER :: IALTFM
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: THETAW2
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: SORBMASS
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: KSSZERO
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: COLDFLW
        INTEGER,          SAVE,                       POINTER :: IDRY2
!--RCT
        INTEGER,          SAVE,                       POINTER :: IREACT
        INTEGER,          SAVE,                       POINTER :: IRCTOP
        INTEGER,          SAVE,                       POINTER :: IGETSC
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: FRAC
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: SP1
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: SP2
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: RC1
        DOUBLE PRECISION, SAVE, DIMENSION(:,:,:,:),   POINTER :: FLAM1
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: RC2
        DOUBLE PRECISION, SAVE, DIMENSION(:,:,:,:),   POINTER :: FLAM2
!--RCT
        INTEGER,          SAVE,                       POINTER :: ICTSOUT 
        INTEGER,          SAVE,                       POINTER :: MXCTS   
        INTEGER,          SAVE,                       POINTER :: MXEXT   
        INTEGER,          SAVE,                       POINTER :: MXINJ   
        INTEGER,          SAVE,                       POINTER :: ICTSPKG
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: KEXT    
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IEXT    
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: JEXT    
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: KINJ    
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IINJ    
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: JINJ    
        INTEGER,          SAVE, DIMENSION(:,:,:),     POINTER :: IOPTINJ 
        INTEGER,          SAVE, DIMENSION(:,:,:),     POINTER :: IOPTEXT 
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CMCHGEXT
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: CMCHGINJ
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: CINCTS  
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: CNTE    
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: ITRTEXT 
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: ITRTINJ 
        REAL,             SAVE, DIMENSION(:),         POINTER :: QINCTS  
        REAL,             SAVE, DIMENSION(:),         POINTER :: QOUTCTS 
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: NEXT    
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: NINJ    
        REAL,             SAVE, DIMENSION(:),         POINTER :: QCTS    
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: CCTS    
        INTEGER,          SAVE,                       POINTER :: NCTS    
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IWEXT   
        INTEGER,          SAVE, DIMENSION(:,:),       POINTER :: IWINJ   
        INTEGER,          SAVE,                       POINTER :: MXWEL   
        INTEGER,          SAVE, DIMENSION(:),         POINTER :: IWCTS   
        INTEGER,          SAVE,                       POINTER :: IFORCE  
        INTEGER,          SAVE,                       POINTER :: NCTSOLD 
        REAL,             SAVE, DIMENSION(:),         POINTER :: CEXT2CTS
        REAL,             SAVE, DIMENSION(:),         POINTER :: CGW2CTS 
        REAL,             SAVE, DIMENSION(:),         POINTER :: CADDM   
        REAL,             SAVE, DIMENSION(:),         POINTER :: CCTS2EXT
        REAL,             SAVE, DIMENSION(:),         POINTER :: CCTS2GW 
        REAL,             SAVE, DIMENSION(:),         POINTER :: CREMM   
!--SFR/LAK
        INTEGER,          SAVE,                       POINTER :: INLKT
        INTEGER,          SAVE,                       POINTER :: INSFT
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
        INTEGER,          SAVE, DIMENSION(19)                 :: L
        INTEGER,     PARAMETER, DIMENSION(9)                  :: LL = &
                          (/ 2, 4, 6, 8, 9, 10, 11, 16, 17 /)
        INTEGER,     PARAMETER, DIMENSION(9)                  :: LU = &
                          (/ 3, 5, 7, 12, 13, 14, 15, 18, 19 /)
        INTEGER,          SAVE,                       POINTER :: INOCROSS
!--HSS                                                
        INTEGER,          SAVE,                       POINTER :: MaxHSSSource
        INTEGER,          SAVE,                       POINTER :: MaxHSSCells
        INTEGER,          SAVE,                       POINTER :: MaxHSSStep
        INTEGER,          SAVE,                       POINTER :: nHSSSource
        INTEGER,          SAVE,                       POINTER :: iRunHSSM
        REAL,             SAVE,                       POINTER :: faclength
        REAL,             SAVE,                       POINTER :: factime
        REAL,             SAVE,                       POINTER :: facmass
        INTEGER,          SAVE, DIMENSION(:,:,:),     POINTER :: iHSSLoc       
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: HSSData       
        CHARACTER(LEN=12),SAVE, DIMENSION(:),         POINTER :: HSSNAM
        INTEGER,          SAVE,                       POINTER :: IHSSGEN
        REAL,             SAVE, DIMENSION(:,:),       POINTER :: P
!        
    CONTAINS
      SUBROUTINE MEMDEALLOCATE()
      IF(ASSOCIATED(NCOL))         DEALLOCATE(NCOL)
      IF(ASSOCIATED(NROW))         DEALLOCATE(NROW)
      IF(ASSOCIATED(NLAY))         DEALLOCATE(NLAY)
      IF(ASSOCIATED(NPER))         DEALLOCATE(NPER)
      IF(ASSOCIATED(NCOMP))        DEALLOCATE(NCOMP)
      IF(ASSOCIATED(MCOMP))        DEALLOCATE(MCOMP)
      IF(ASSOCIATED(NODES))        DEALLOCATE(NODES)
      IF(ASSOCIATED(NSTP))         DEALLOCATE(NSTP)
      IF(ASSOCIATED(MXSTRN))       DEALLOCATE(MXSTRN)
      IF(ASSOCIATED(iSSTrans))     DEALLOCATE(iSSTrans)
      IF(ASSOCIATED(MXPART))       DEALLOCATE(MXPART)
      IF(ASSOCIATED(IOUT))         DEALLOCATE(IOUT)
      IF(ASSOCIATED(INBTN))        DEALLOCATE(INBTN)
      IF(ASSOCIATED(INADV))        DEALLOCATE(INADV)
      IF(ASSOCIATED(INDSP))        DEALLOCATE(INDSP)
      IF(ASSOCIATED(INSSM))        DEALLOCATE(INSSM)
      IF(ASSOCIATED(INRCT))        DEALLOCATE(INRCT)
      IF(ASSOCIATED(INUZT))        DEALLOCATE(INUZT) 
      IF(ASSOCIATED(INGCG))        DEALLOCATE(INGCG)
      IF(ASSOCIATED(INTOB))        DEALLOCATE(INTOB)
      IF(ASSOCIATED(INHSS))        DEALLOCATE(INHSS)
      IF(ASSOCIATED(INFTL))        DEALLOCATE(INFTL)
      IF(ASSOCIATED(IFTLFMT))      DEALLOCATE(IFTLFMT)
      IF(ASSOCIATED(ICNF))         DEALLOCATE(ICNF)
      IF(ASSOCIATED(IUCN))         DEALLOCATE(IUCN)
      IF(ASSOCIATED(IUCN2))        DEALLOCATE(IUCN2)
      IF(ASSOCIATED(IOBS))         DEALLOCATE(IOBS)
      IF(ASSOCIATED(IMAS))         DEALLOCATE(IMAS)
      IF(ASSOCIATED(ICBM))         DEALLOCATE(ICBM)
      IF(ASSOCIATED(DT0))          DEALLOCATE(DT0)
      IF(ASSOCIATED(TTSMULT))      DEALLOCATE(TTSMULT)
      IF(ASSOCIATED(TTSMAX))       DEALLOCATE(TTSMAX)
      IF(ASSOCIATED(TUNIT))        DEALLOCATE(TUNIT)
      IF(ASSOCIATED(LUNIT))        DEALLOCATE(LUNIT)
      IF(ASSOCIATED(MUNIT))        DEALLOCATE(MUNIT)
      IF(ASSOCIATED(FPRT))         DEALLOCATE(FPRT)
      IF(ASSOCIATED(iUnitTRNOP))   DEALLOCATE(iUnitTRNOP)
      IF(ASSOCIATED(LAYCON))       DEALLOCATE(LAYCON)
      IF(ASSOCIATED(ICBUND))       DEALLOCATE(ICBUND)
      IF(ASSOCIATED(DELR))         DEALLOCATE(DELR)
      IF(ASSOCIATED(DELC))         DEALLOCATE(DELC)
      IF(ASSOCIATED(DZ))           DEALLOCATE(DZ)
      IF(ASSOCIATED(PRSITY))       DEALLOCATE(PRSITY)
      IF(ASSOCIATED(HTOP))         DEALLOCATE(HTOP)
      IF(ASSOCIATED(XBC))          DEALLOCATE(XBC)
      IF(ASSOCIATED(YBC))          DEALLOCATE(YBC)
      IF(ASSOCIATED(ZBC))          DEALLOCATE(ZBC)
      IF(ASSOCIATED(CNEW))         DEALLOCATE(CNEW)
      IF(ASSOCIATED(COLD))         DEALLOCATE(COLD)
      IF(ASSOCIATED(QX))           DEALLOCATE(QX)
      IF(ASSOCIATED(QY))           DEALLOCATE(QY)
      IF(ASSOCIATED(QZ))           DEALLOCATE(QZ)
      IF(ASSOCIATED(QSTO))         DEALLOCATE(QSTO)
      IF(ASSOCIATED(DH))           DEALLOCATE(DH)
      IF(ASSOCIATED(CWGT))         DEALLOCATE(CWGT)
      IF(ASSOCIATED(CADV))         DEALLOCATE(CADV)
      IF(ASSOCIATED(RETA))         DEALLOCATE(RETA)
      IF(ASSOCIATED(SRCONC))       DEALLOCATE(SRCONC)
      IF(ASSOCIATED(BUFF))         DEALLOCATE(BUFF)
      IF(ASSOCIATED(TSLNGH))       DEALLOCATE(TSLNGH)
      IF(ASSOCIATED(RHOB))         DEALLOCATE(RHOB)
      IF(ASSOCIATED(PRSITY2))      DEALLOCATE(PRSITY2)
      IF(ASSOCIATED(RETA2))        DEALLOCATE(RETA2)
      IF(ASSOCIATED(NPINS))        DEALLOCATE(NPINS)
      IF(ASSOCIATED(NRC))          DEALLOCATE(NRC)
!
      IF(ASSOCIATED(HT1))          DEALLOCATE(HT1)
      IF(ASSOCIATED(HT2))          DEALLOCATE(HT2)
      IF(ASSOCIATED(DTRANS))       DEALLOCATE(DTRANS)
      IF(ASSOCIATED(DELT))         DEALLOCATE(DELT)
      IF(ASSOCIATED(TIME1))        DEALLOCATE(TIME1)
      IF(ASSOCIATED(TIME2))        DEALLOCATE(TIME2)
      IF(ASSOCIATED(CINACT))       DEALLOCATE(CINACT)
      IF(ASSOCIATED(THKMIN))       DEALLOCATE(THKMIN)
      IF(ASSOCIATED(RFMIN))        DEALLOCATE(RFMIN)
      IF(ASSOCIATED(IMPSOL))       DEALLOCATE(IMPSOL)
      IF(ASSOCIATED(IFTL))         DEALLOCATE(IFTL)
      IF(ASSOCIATED(DTRACK))       DEALLOCATE(DTRACK)
      IF(ASSOCIATED(DTRACK2))      DEALLOCATE(DTRACK2)
      IF(ASSOCIATED(DTSSM))        DEALLOCATE(DTSSM)
      IF(ASSOCIATED(DTDISP))       DEALLOCATE(DTDISP)
      IF(ASSOCIATED(DTRCT))        DEALLOCATE(DTRCT)
      IF(ASSOCIATED(MIXELM))       DEALLOCATE(MIXELM)
      IF(ASSOCIATED(ISPD))         DEALLOCATE(ISPD)
      IF(ASSOCIATED(PERCEL))       DEALLOCATE(PERCEL)
      IF(ASSOCIATED(IFMTCN))       DEALLOCATE(IFMTCN)
      IF(ASSOCIATED(IFMTNP))       DEALLOCATE(IFMTNP)
      IF(ASSOCIATED(IFMTRF))       DEALLOCATE(IFMTRF)
      IF(ASSOCIATED(IFMTDP))       DEALLOCATE(IFMTDP)
      IF(ASSOCIATED(NPS))          DEALLOCATE(NPS)
      IF(ASSOCIATED(NPRS))         DEALLOCATE(NPRS)
      IF(ASSOCIATED(NPRMAS))       DEALLOCATE(NPRMAS)
      IF(ASSOCIATED(SAVUCN))       DEALLOCATE(SAVUCN)
      IF(ASSOCIATED(SAVCBM))       DEALLOCATE(SAVCBM)
      IF(ASSOCIATED(CHKMAS))       DEALLOCATE(CHKMAS)
      IF(ASSOCIATED(PRTOUT))       DEALLOCATE(PRTOUT)
      IF(ASSOCIATED(UPDLHS))       DEALLOCATE(UPDLHS)
      IF(ASSOCIATED(NOBS))         DEALLOCATE(NOBS)
      IF(ASSOCIATED(NPROBS))       DEALLOCATE(NPROBS)
      IF(ASSOCIATED(HORIGN))       DEALLOCATE(HORIGN)
      IF(ASSOCIATED(XMAX))         DEALLOCATE(XMAX)
      IF(ASSOCIATED(YMAX))         DEALLOCATE(YMAX)
      IF(ASSOCIATED(ZMAX))         DEALLOCATE(ZMAX)
      IF(ASSOCIATED(UNIDX))        DEALLOCATE(UNIDX)
      IF(ASSOCIATED(UNIDY))        DEALLOCATE(UNIDY)
      IF(ASSOCIATED(UNIDZ))        DEALLOCATE(UNIDZ)
      IF(ASSOCIATED(ISOTHM))       DEALLOCATE(ISOTHM)
      IF(ASSOCIATED(TIMPRS))       DEALLOCATE(TIMPRS)
      IF(ASSOCIATED(TMASIN))       DEALLOCATE(TMASIN)
      IF(ASSOCIATED(TMASOT))       DEALLOCATE(TMASOT)
      IF(ASSOCIATED(ERROR))        DEALLOCATE(ERROR)
      IF(ASSOCIATED(ERROR2))       DEALLOCATE(ERROR2)
      IF(ASSOCIATED(LOCOBS))       DEALLOCATE(LOCOBS)
      IF(ASSOCIATED(TMASIO))       DEALLOCATE(TMASIO)
      IF(ASSOCIATED(RMASIO))       DEALLOCATE(RMASIO)
      IF(ASSOCIATED(TMASS))        DEALLOCATE(TMASS)
!
      IF(ASSOCIATED(NADVFD))       DEALLOCATE(NADVFD)
      IF(ASSOCIATED(ITRACK))       DEALLOCATE(ITRACK)
      IF(ASSOCIATED(ISEED))        DEALLOCATE(ISEED)
      IF(ASSOCIATED(NPLANE))       DEALLOCATE(NPLANE)
      IF(ASSOCIATED(NPL))          DEALLOCATE(NPL)
      IF(ASSOCIATED(NPH))          DEALLOCATE(NPH)
      IF(ASSOCIATED(NPMIN))        DEALLOCATE(NPMIN)
      IF(ASSOCIATED(NPMAX))        DEALLOCATE(NPMAX)
      IF(ASSOCIATED(INTERP))       DEALLOCATE(INTERP)
      IF(ASSOCIATED(NLSINK))       DEALLOCATE(NLSINK)
      IF(ASSOCIATED(NPSINK))       DEALLOCATE(NPSINK)
      IF(ASSOCIATED(WD))           DEALLOCATE(WD)
      IF(ASSOCIATED(DCEPS))        DEALLOCATE(DCEPS)
      IF(ASSOCIATED(SRMULT))       DEALLOCATE(SRMULT)
      IF(ASSOCIATED(DCHMOC))       DEALLOCATE(DCHMOC)
      IF(ASSOCIATED(NCOUNT))       DEALLOCATE(NCOUNT)
      IF(ASSOCIATED(NPCHEK))       DEALLOCATE(NPCHEK)
      IF(ASSOCIATED(INDEXX))       DEALLOCATE(INDEXX)
      IF(ASSOCIATED(INDEXY))       DEALLOCATE(INDEXY)
      IF(ASSOCIATED(INDEXZ))       DEALLOCATE(INDEXZ)
      IF(ASSOCIATED(XP))           DEALLOCATE(XP)
      IF(ASSOCIATED(YP))           DEALLOCATE(YP)
      IF(ASSOCIATED(ZP))           DEALLOCATE(ZP)
      IF(ASSOCIATED(CNPT))         DEALLOCATE(CNPT)
!
      IF(ASSOCIATED(ALPHAL))       DEALLOCATE(ALPHAL)
      IF(ASSOCIATED(DMCOEF))       DEALLOCATE(DMCOEF)
      IF(ASSOCIATED(TRPT))         DEALLOCATE(TRPT)
      IF(ASSOCIATED(TRPV))         DEALLOCATE(TRPV)
      IF(ASSOCIATED(DXX))          DEALLOCATE(DXX)
      IF(ASSOCIATED(DYY))          DEALLOCATE(DYY)
      IF(ASSOCIATED(DZZ))          DEALLOCATE(DZZ)
      IF(ASSOCIATED(DXY))          DEALLOCATE(DXY)
      IF(ASSOCIATED(DXZ))          DEALLOCATE(DXZ)
      IF(ASSOCIATED(DYX))          DEALLOCATE(DYX)
      IF(ASSOCIATED(DYZ))          DEALLOCATE(DYZ)
      IF(ASSOCIATED(DZX))          DEALLOCATE(DZX)
      IF(ASSOCIATED(DZY))          DEALLOCATE(DZY)
!
      IF(ASSOCIATED(NPERFL))       DEALLOCATE(NPERFL)
      IF(ASSOCIATED(ISS))          DEALLOCATE(ISS)
      IF(ASSOCIATED(IVER))         DEALLOCATE(IVER)
      IF(ASSOCIATED(FWEL))         DEALLOCATE(FWEL)
      IF(ASSOCIATED(FDRN))         DEALLOCATE(FDRN)
      IF(ASSOCIATED(FRCH))         DEALLOCATE(FRCH)
      IF(ASSOCIATED(FEVT))         DEALLOCATE(FEVT)
      IF(ASSOCIATED(FRIV))         DEALLOCATE(FRIV)
      IF(ASSOCIATED(FGHB))         DEALLOCATE(FGHB)
      IF(ASSOCIATED(FSTR))         DEALLOCATE(FSTR)
      IF(ASSOCIATED(FRES))         DEALLOCATE(FRES)
      IF(ASSOCIATED(FFHB))         DEALLOCATE(FFHB)
      IF(ASSOCIATED(FIBS))         DEALLOCATE(FIBS)
      IF(ASSOCIATED(FTLK))         DEALLOCATE(FTLK)
      IF(ASSOCIATED(FLAK))         DEALLOCATE(FLAK)
      IF(ASSOCIATED(FMNW))         DEALLOCATE(FMNW)
      IF(ASSOCIATED(FDRT))         DEALLOCATE(FDRT)
      IF(ASSOCIATED(FETS))         DEALLOCATE(FETS)
      IF(ASSOCIATED(FSWT))         DEALLOCATE(FSWT)
      IF(ASSOCIATED(FSFR))         DEALLOCATE(FSFR)
!
      IF(ASSOCIATED(ISSGOUT))      DEALLOCATE(ISSGOUT)
      IF(ASSOCIATED(MXSS))         DEALLOCATE(MXSS)
      IF(ASSOCIATED(NSS))          DEALLOCATE(NSS)
      IF(ASSOCIATED(NTSS))         DEALLOCATE(NTSS)
      IF(ASSOCIATED(RECH))         DEALLOCATE(RECH)
      IF(ASSOCIATED(IRCH))         DEALLOCATE(IRCH)
      IF(ASSOCIATED(CRCH))         DEALLOCATE(CRCH)
      IF(ASSOCIATED(EVTR))         DEALLOCATE(EVTR)
      IF(ASSOCIATED(IEVT))         DEALLOCATE(IEVT)
      IF(ASSOCIATED(CEVT))         DEALLOCATE(CEVT)
!
      IF(ASSOCIATED(SS))           DEALLOCATE(SS)
      IF(ASSOCIATED(SSMC))         DEALLOCATE(SSMC)
      IF(ASSOCIATED(SSG))          DEALLOCATE(SSG)
!
      IF(ASSOCIATED(IREACT))       DEALLOCATE(IREACT)
      IF(ASSOCIATED(IRCTOP))       DEALLOCATE(IRCTOP)
      IF(ASSOCIATED(IGETSC))       DEALLOCATE(IGETSC)
      IF(ASSOCIATED(FRAC))         DEALLOCATE(FRAC)
      IF(ASSOCIATED(SP1))          DEALLOCATE(SP1)
      IF(ASSOCIATED(SP2))          DEALLOCATE(SP2)
      IF(ASSOCIATED(RC1))          DEALLOCATE(RC1)
      IF(ASSOCIATED(FLAM1))      DEALLOCATE(FLAM1)
      IF(ASSOCIATED(RC2))          DEALLOCATE(RC2)
      IF(ASSOCIATED(FLAM2))      DEALLOCATE(FLAM2)
!
      IF(ASSOCIATED(MaxConcObs))   DEALLOCATE(MaxConcObs)
      IF(ASSOCIATED(MaxFluxObs))   DEALLOCATE(MaxFluxObs)
      IF(ASSOCIATED(MaxFluxCells)) DEALLOCATE(MaxFluxCells)
      IF(ASSOCIATED(inConcObs))    DEALLOCATE(inConcObs)
      IF(ASSOCIATED(nConcObs))     DEALLOCATE(nConcObs)
      IF(ASSOCIATED(iOutCobs))     DEALLOCATE(iOutCobs)
      IF(ASSOCIATED(iConcLOG))     DEALLOCATE(iConcLOG)
      IF(ASSOCIATED(iConcINTP))    DEALLOCATE(iConcINTP)
      IF(ASSOCIATED(inFluxObs))    DEALLOCATE(inFluxObs)
      IF(ASSOCIATED(nFluxGroup))   DEALLOCATE(nFluxGroup)
      IF(ASSOCIATED(nFluxObs))     DEALLOCATE(nFluxObs)
      IF(ASSOCIATED(iOutFlux))     DEALLOCATE(iOutFlux)
      IF(ASSOCIATED(inSaveObs))    DEALLOCATE(inSaveObs)
      IF(ASSOCIATED(CScale))       DEALLOCATE(CScale)
      IF(ASSOCIATED(FScale))       DEALLOCATE(FScale)
      IF(ASSOCIATED(mLayer))       DEALLOCATE(mLayer)
      IF(ASSOCIATED(prLayer))      DEALLOCATE(prLayer)
      IF(ASSOCIATED(COBSWEL))      DEALLOCATE(COBSWEL)
      IF(ASSOCIATED(TEMP))         DEALLOCATE(TEMP)
      IF(ASSOCIATED(FluxGroup))    DEALLOCATE(FluxGroup)
      IF(ASSOCIATED(GroupData))    DEALLOCATE(GroupData)
      IF(ASSOCIATED(FOBSNAM))      DEALLOCATE(FOBSNAM)
      IF(ASSOCIATED(COBSNAM))      DEALLOCATE(COBSNAM)
!
      IF(ASSOCIATED(MXITER))       DEALLOCATE(MXITER)
      IF(ASSOCIATED(ITER1))        DEALLOCATE(ITER1)
      IF(ASSOCIATED(ISOLVE))       DEALLOCATE(ISOLVE)
      IF(ASSOCIATED(NCRS))         DEALLOCATE(NCRS)
      IF(ASSOCIATED(IPRGCG))       DEALLOCATE(IPRGCG)
      IF(ASSOCIATED(ACCL))         DEALLOCATE(ACCL)
      IF(ASSOCIATED(CCLOSE))       DEALLOCATE(CCLOSE)
      IF(ASSOCIATED(LRCH))         DEALLOCATE(LRCH)
      IF(ASSOCIATED(A))            DEALLOCATE(A)
      IF(ASSOCIATED(Q))            DEALLOCATE(Q)
      IF(ASSOCIATED(WK))           DEALLOCATE(WK)
      IF(ASSOCIATED(CNCG))         DEALLOCATE(CNCG)
      IF(ASSOCIATED(RHS))          DEALLOCATE(RHS)
!
      IF(ASSOCIATED(MaxHSSSource)) DEALLOCATE(MaxHSSSource)
      IF(ASSOCIATED(MaxHSSCells))  DEALLOCATE(MaxHSSCells)
      IF(ASSOCIATED(MaxHSSStep))   DEALLOCATE(MaxHSSStep)
      IF(ASSOCIATED(nHSSSource))   DEALLOCATE(nHSSSource)
      IF(ASSOCIATED(iRunHSSM))     DEALLOCATE(iRunHSSM)
      IF(ASSOCIATED(IHSSGEN))      DEALLOCATE(IHSSGEN)
      IF(ASSOCIATED(faclength))    DEALLOCATE(faclength)
      IF(ASSOCIATED(factime))      DEALLOCATE(factime)
      IF(ASSOCIATED(facmass))      DEALLOCATE(facmass)
      IF(ASSOCIATED(iHSSLoc))      DEALLOCATE(iHSSLoc)
      IF(ASSOCIATED(HSSData))      DEALLOCATE(HSSData)
      IF(ASSOCIATED(HSSNAM))       DEALLOCATE(HSSNAM)
!      
      IF(ASSOCIATED(MXCTS))        DEALLOCATE(MXCTS)
      IF(ASSOCIATED(ICTSOUT))      DEALLOCATE(ICTSOUT)
      IF(ASSOCIATED(MXEXT))        DEALLOCATE(MXEXT)
      IF(ASSOCIATED(MXINJ))        DEALLOCATE(MXINJ)
      IF(ASSOCIATED(MXWEL))        DEALLOCATE(MXWEL)
      IF(ASSOCIATED(IFORCE))       DEALLOCATE(IFORCE)
      IF(ASSOCIATED(NCTS))         DEALLOCATE(NCTS)
      IF(ASSOCIATED(NCTSOLD))      DEALLOCATE(NCTSOLD)
      IF(ASSOCIATED(NEXT))         DEALLOCATE(NEXT)
      IF(ASSOCIATED(NINJ))         DEALLOCATE(NINJ)
      IF(ASSOCIATED(ITRTEXT))      DEALLOCATE(ITRTEXT)
      IF(ASSOCIATED(ITRTINJ))      DEALLOCATE(ITRTINJ)
      IF(ASSOCIATED(IOPTEXT))      DEALLOCATE(IOPTEXT)
      IF(ASSOCIATED(IOPTINJ))      DEALLOCATE(IOPTINJ)
      IF(ASSOCIATED(IOPTEXT))      DEALLOCATE(IOPTEXT)
      IF(ASSOCIATED(CMCHGEXT))     DEALLOCATE(CMCHGEXT)
      IF(ASSOCIATED(CMCHGINJ))     DEALLOCATE(CMCHGINJ)
      IF(ASSOCIATED(KEXT))         DEALLOCATE(KEXT)
      IF(ASSOCIATED(IEXT))         DEALLOCATE(IEXT)
      IF(ASSOCIATED(JEXT))         DEALLOCATE(JEXT)
      IF(ASSOCIATED(KINJ))         DEALLOCATE(KINJ)
      IF(ASSOCIATED(IINJ))         DEALLOCATE(IINJ)
      IF(ASSOCIATED(JINJ))         DEALLOCATE(JINJ)
      IF(ASSOCIATED(QINCTS))       DEALLOCATE(QINCTS)
      IF(ASSOCIATED(QOUTCTS))      DEALLOCATE(QOUTCTS)
      IF(ASSOCIATED(CNTE))         DEALLOCATE(CNTE)
      IF(ASSOCIATED(IWEXT))        DEALLOCATE(IWEXT)
      IF(ASSOCIATED(IWINJ))        DEALLOCATE(IWINJ)
      IF(ASSOCIATED(IWCTS))        DEALLOCATE(IWCTS)
      IF(ASSOCIATED(QCTS))         DEALLOCATE(QCTS)
      IF(ASSOCIATED(CCTS))         DEALLOCATE(CCTS)
      IF(ASSOCIATED(CEXT2CTS))     DEALLOCATE(CEXT2CTS)
      IF(ASSOCIATED(CGW2CTS))      DEALLOCATE(CGW2CTS)
      IF(ASSOCIATED(CADDM))        DEALLOCATE(CADDM)
      IF(ASSOCIATED(CCTS2EXT))     DEALLOCATE(CCTS2EXT)
      IF(ASSOCIATED(CCTS2GW))      DEALLOCATE(CCTS2GW)
      IF(ASSOCIATED(CREMM))        DEALLOCATE(CREMM)
      
      END SUBROUTINE MEMDEALLOCATE  
                           
END MODULE MT3DMS_MODULE   