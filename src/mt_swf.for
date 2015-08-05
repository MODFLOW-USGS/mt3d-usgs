C
C THIS FILE CONTAINS SUBROUTINES THAT ARE COMMON TO GENERALIZED-NETWORK-TRANSPORT COMPONENTS
C
      SUBROUTINE GNT1FM(ICOMP)
      USE SFRVARS
      USE XMDMODULE
      USE MT3DMS_MODULE, ONLY: iUnitTRNOP
      INTEGER ICOMP
      INTEGER ICNVG
      REAL    BIGDIFF,BIGDIFF2,DIFF
C
C--SOLVE RIGOROUS TRANSPORT THROUGH SFR IF ISFRBC.NE.1, ELSE SKIP THE SFR SOLUTION
      IF(ISFRBC.NE.1) THEN
C
C--ITERATE OVER ALL SURFACE WATER PACKAGES
      DO KITERSF=1,MXITERSF
C
C--ASSEMBLE MATRIX FOR DIFFERENT PACKAGES
C
C.....SFR PACKAGE
      IF(iUnitTRNOP(19).GT.0) CALL SFT1FM(ICOMP)
C
C.....SWR PACKAGE

C
C--SOLVE ALL SURFACE WATER PACKAGES IN ONE SINGLE MATRIX
C
C-----CALL XMD SOLVER-------------------------------------------------
      numactive=NSTRM
      IF (IDROPTOL.EQ.0 .or.  KITERSF.gt.1) THEN
        call xmdnfctr(AMATSF, RHSSF, IASF, JASF, NJASF, numactive, ierr)
      ELSE
        call xmdprecd(AMATSF, RHSSF, epsrn, IASF, JASF, NJASF,
     [                       numactive,level, ierr)
      ENDIF
      iter = Mxiterxmd
      CNEWSFTMP=CNEWSF
      call xmdsolv(AMATSF, RHSSF, CNEWSF, hclosexmd, rrctol, IASF, 
     [                 JASF,NJASF,numactive, north, iter, iacl, ierr)
      n_iter = iter
C     
      IF(IDSCALE.EQ.1)THEN
c  ----------------
c       recover {x} because diagonal scaling subroutine (xmddgscl) is used
        do jj = 1, Numactive
          CNEWSF(jj,ICOMP) = CNEWSF(jj,ICOMP) * dgscal(jj)
        enddo                  
      ENDIF
C--------- XMD SOLVER-------------------------------------------------
C
C-----CHECK FOR CONVERGENCE
      ICNVG=0
      BIGDIFF=0.
      BIGDIFF2=0.
      DO N=1,NSTRM
        DIFF=CNEWSF(N,ICOMP)-CNEWSFTMP(N,ICOMP)
        IF(ABS(DIFF).GT.BIGDIFF) THEN
          BIGDIFF=ABS(DIFF)
          BIGDIFF2=DIFF
        ENDIF
      ENDDO
      IF(BIGDIFF.LT.CCLOSESF) ICNVG=1
      IF(ICNVG.EQ.1) EXIT
C
      ENDDO !KITERSF

      ENDIF !ISFRBC

C
C--FILL GW MATRIX COEFFICIENTS FOR ALL SURFACE WATER PACKAGES
C
C.....SFR PACKAGE
      IF(iUnitTRNOP(19).GT.0) CALL SFT1FMGW(ICOMP)
C
C.....SWR PACKAGE


      RETURN
      END
C
C
      SUBROUTINE FILLIASFJASF()
C***********************************************************************
C     THIS SUBROUTINE ALLOCATES AND FILLS IA AND JA MATRICES
C***********************************************************************
      USE SFRVARS
      IMPLICIT NONE
      INTEGER N,NC,K,I,J,IS,IR,ICNT,NN,II
C
      CALL MEMDEALLOCATE3()
C
      ALLOCATE(IASF(NSTRM+1),RHSSF(NSTRM))
      IASF=0
      RHSSF=0.
C
C--COUNT CONNECTIONS AT EACH NODE N
      ICNT=0
      NJASF=0
      DO N=1,NSTRM
        IASF(N)=IASF(N)+1
        DO NC=1,NIN(N)
          ICNT=ICNT+1
          IS=INSEG(ICNT)
          IR=INRCH(ICNT)
          IF(IS.GT.0.AND.IR.GT.0) THEN
            IASF(N)=IASF(N)+1
            NN=ISTRM(IR,IS)
            IASF(NN)=IASF(NN)+1
          ENDIF
        ENDDO
      ENDDO
C
C-------CUMULATIVE OF CONNECTIONS PER ROW IN IASF
C--REFERENCE: SORAB'S CODE
      DO II=2,NSTRM+1
        IASF(II) = IASF(II) + IASF(II-1)
      ENDDO
C-------IASF(N+1) IS CUMULATIVE_IASF(N) + 1
      DO II=NSTRM+1,2,-1
        IASF(II) = IASF(II-1) + 1
      ENDDO
      IASF(1) = 1
C--ALLOCATE JASF AND AMATSF WITH SIZE NJASF
      NJASF = IASF(NSTRM+1) - 1
      ALLOCATE(JASF(NJASF),AMATSF(NJASF))
      JASF=0
      AMATSF=0.
C
C--POPUATE JA
C-------DIAGONAL IN FIRST LOCATION
      DO N=1,NSTRM
C-------ONCE JA WORKS FINE REMOVE THIS BLOCK
        IF(JASF(IASF(N)).NE.0) THEN
          WRITE(*,*) 'ERROR IN JASF'
          READ(*,*)
          STOP
        ENDIF
C-------------------------------------------
        JASF(IASF(N)) = N
      ENDDO
C
      ICNT=0
      INFLWNOD=0
      DO N=1,NSTRM
        DO NC=1,NIN(N)
          ICNT=ICNT+1
          IS=INSEG(ICNT)
          IR=INRCH(ICNT)
          IF(IS.GT.0.AND.IR.GT.0) THEN
            NN=ISTRM(IR,IS)
            CALL FINDJASF(N,NN,IASF,JASF,NSTRM,NJASF)
            INFLWNOD(NN)=1
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
      END
C
C
      SUBROUTINE FINDJASF(N,NN,IASF,JASF,NSTRM,NJASF)
C
      INTEGER IASF(NSTRM+1),JASF(NJASF)
      INTEGER N,NN,I1,I2
C
      I1=IASF(N)
      I2=IASF(N+1)-1
      ICHECK=0
      DO I=I1,I2
        IF(JASF(I).EQ.0) THEN
          JASF(I)=NN
          ICHECK=1
          EXIT
        ENDIF
      ENDDO
      IF(ICHECK.EQ.0) THEN
        WRITE(*,*) 'ERROR IN FINDJASF',N
        READ(*,*)
        STOP
      ENDIF
C
      I1=IASF(NN)
      I2=IASF(NN+1)-1
      ICHECK=0
      DO I=I1,I2
        IF(JASF(I).EQ.0) THEN
          JASF(I)=N
          ICHECK=1
          EXIT
        ENDIF
      ENDDO
      IF(ICHECK.EQ.0) THEN
        WRITE(*,*) 'ERROR IN FINDJASF',NN
        READ(*,*)
        STOP
      ENDIF
C
      RETURN
      END
C
C
