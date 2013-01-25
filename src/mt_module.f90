      MODULE MT3DMS_MODULE
        INTEGER,PARAMETER :: MXTRNOP=20,MXSTP=9000
        CHARACTER(LEN=4), SAVE, DIMENSION(MXTRNOP) :: NameTRNOP=        &
     &  (/'ADV ', 'DSP ', 'SSM ', 'RCT ', 'GCG ',                       &
     &    'UZF ', '    ', '    ', '    ', '    ',                       &      !edm
     &    'TOB ', '    ', 'HSS ', '    ', '    ',                       &
     &    '    ', '    ', '    ', '    ', '    '/)
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
!--RCT
        INTEGER,          SAVE,                       POINTER :: IREACT
        INTEGER,          SAVE,                       POINTER :: IRCTOP
        INTEGER,          SAVE,                       POINTER :: IGETSC
        REAL,             SAVE, DIMENSION(:,:,:),     POINTER :: FRAC
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: SP1
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: SP2
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: RC1
        REAL,             SAVE, DIMENSION(:,:,:,:),   POINTER :: RC2
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
      IF(ASSOCIATED(faclength)) DEALLOCATE(faclength)
      IF(ASSOCIATED(factime)) DEALLOCATE(factime)
      IF(ASSOCIATED(facmass)) DEALLOCATE(facmass)
      IF(ASSOCIATED(iHSSLoc)) DEALLOCATE(iHSSLoc)
      IF(ASSOCIATED(HSSData)) DEALLOCATE(HSSData)
      IF(ASSOCIATED(HSSNAM)) DEALLOCATE(HSSNAM)
      
      END SUBROUTINE MEMDEALLOCATE  
                           
END MODULE MT3DMS_MODULE   