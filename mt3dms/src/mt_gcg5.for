C
      SUBROUTINE GCG5AL(INGCG,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,MXITER,
     & ITER1,NCRS,ISOLVE,LCA,LCQ,LCWK,LCCNCG,LCLRCH,LCRHS)
C ********************************************************************
C ALLOCATE STORAGE IN THE X AND IX ARRAYS FOR GCG ARRAYS
C ********************************************************************
C last modified: 02-20-2010
C
      IMPLICIT NONE
      INTEGER  INGCG,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,MXITER,ITER1,
     &         NCRS,ISOLVE,LCA,LCQ,LCWK,LCCNCG,
     &         LCLRCH,LCRHS,NODES,ISUMX,ISUMIX,ISOLD,ISOLD2

C--PRINT A MESSAGE IDENTIFYING GCG PACKAGE
      WRITE(IOUT,1) INGCG
    1 FORMAT(1X,'GCG5 -- GENERALIZED CONJUGATE GRADIENT SOLVER PACKAGE',
     & ', VERSION 5, FEBRUARY 2010',' INPUT READ FROM UNIT',I3)
C
C--READ AND PRINT MXITER AND ISOLVE
      READ(INGCG,*) MXITER,ITER1,ISOLVE,NCRS
      WRITE(IOUT,3) MXITER,ITER1
    3 FORMAT(1X,'MAXIMUM OF',I5,' OUTER ITERATIONS',
     &      /1X,'       AND',I5,' INNER ITERATIONS ALLOWED FOR CLOSURE')
      IF(MXITER.LE.0) THEN
        WRITE(*,5)
        CALL USTOP(' ')
      ELSEIF(ITER1.LE.0) THEN
        WRITE(*,7)
        CALL USTOP(' ')
      ENDIF
      IF(ISOLVE.EQ.1) THEN
        WRITE(IOUT,13)
      ELSEIF(ISOLVE.EQ.2) THEN
        WRITE(IOUT,23)
      ELSEIF(ISOLVE.EQ.3) THEN
        WRITE(IOUT,33)
      ELSE
        WRITE(IOUT,43)
        CALL USTOP(' ')
      ENDIF
    5 FORMAT(/1X,'ERROR: OUTER ITERATION NUMBER MUST BE > 0.')
    7 FORMAT(/1X,'ERROR: INNER ITERATION NUMBER MUST BE > 0.')
   13 FORMAT(1X,'THE PRECONDITIONING TYPE SELECTED IS JACOBI.')
   23 FORMAT(1X,'THE PRECONDITIONING TYPE SELECTED IS SSOR.')
   33 FORMAT(1X,'THE PRECONDITIONING TYPE SELECTED IS ',
     &          'MODIFIED INCOMPLETE CHOLESKY (MIC).')
   43 FORMAT(1X,'ERROR: INVALID PRECONDITIONING TYPE.')
C
      IF(NCRS.GT.0) THEN
        WRITE(IOUT,50)
      ELSE
        WRITE(IOUT,52)
      ENDIF
   50 FORMAT(1X,'FULL DISPERSION TENSOR INCLUDED IN IMPLICIT SOLUTION')
   52 FORMAT(1X,'DISPERSION CROSS TERMS LUMPED INTO RIGHT-HAND-SIDE')
C
C--SET NCRS TO 0 FOR 1D PROBLEMS
      IF(NCOL*NROW.EQ.1 .OR. NCOL*NLAY.EQ.1 .OR. NROW*NLAY.EQ.1) NCRS=0
C
C--ALLOCATE SPACE FOR THE GCG ARRAYS
      ISOLD=ISUM
      ISOLD2=ISUM2
      NODES=NLAY*NROW*NCOL
C
C--INTEGER ARRAYS
      LCLRCH=ISUM2
      ISUM2=ISUM2+MXITER*ITER1*3
C
C--REAL ARRAYS
      LCA=ISUM
      IF(NCRS.GT.0) THEN
        ISUM=ISUM+NODES*19
      ELSE
        ISUM=ISUM+NODES*7
      ENDIF
      LCQ=ISUM
      IF(ISOLVE.EQ.3) THEN
        IF(NCRS.GT.0) THEN
          ISUM=ISUM+NODES*19
        ELSE
          ISUM=ISUM+NODES*7
        ENDIF
      ENDIF
      LCWK=ISUM
      ISUM=ISUM+NODES*7
      LCCNCG=ISUM
      ISUM=ISUM+MXITER*ITER1
      LCRHS=ISUM
      ISUM=ISUM+NODES
C
C--CHECK HOW MANY ELEMENTS OF THE X AND IX ARRAYS ARE USED
      ISUMX=ISUM-ISOLD
      ISUMIX=ISUM2-ISOLD2
      WRITE(IOUT,1090) ISUMX,ISUMIX
 1090 FORMAT(1X,I10,' ELEMENTS OF THE  X ARRAY USED BY THE GCG PACKAGE'
     & /1X,I10,' ELEMENTS OF THE IX ARRAY USED BY THE GCG PACKAGE'/)
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE GCG5RP(INGCG,IOUT,MXITER,ITER1,
     & ISOLVE,ACCL,CCLOSE,IPRGCG)
C ***************************************************************
C READ INPUT DATA FOR GCG PACKAGE
C ***************************************************************
C last modified: 02-15-2005
C
      IMPLICIT NONE
      INTEGER  INGCG,IOUT,MXITER,ITER1,ISOLVE,IPRGCG
      REAL     ACCL,CCLOSE
C
C--READ ACCL,CCLOSE,IPRGCG
      READ(INGCG,*) ACCL,CCLOSE,IPRGCG
      IF(ACCL.EQ.0.) ACCL=1.
C
C--PRINT DATA VALUES JUST READ
      WRITE(IOUT,100)
  100 FORMAT(///47X,'SOLUTION BY THE GENERALIZED CONJUGATE GRADIENT',
     & ' METHOD'/47X,53('-'))
      WRITE(IOUT,115) MXITER
  115 FORMAT(37X,'MAXIMUM OUTER ITERATIONS ALLOWED FOR CLOSURE =',I9)
      WRITE(IOUT,116) ITER1
  116 FORMAT(37X,'MAXIMUM INNER ITERATIONS ALLOWED FOR CLOSURE =',I9)
      WRITE(IOUT,117) ISOLVE
  117 FORMAT(52X,'PRECONDITIONING TYPE SELECTED =',I5)
      WRITE(IOUT,120) ACCL
  120 FORMAT(59X,'ACCELERATION PARAMETER =',G15.5)
      WRITE(IOUT,125) CCLOSE
  125 FORMAT(39X,'CONCENTRATION CHANGE CRITERION FOR CLOSURE =',E15.5)
      IF(IPRGCG.LE.0) IPRGCG=999
      WRITE(IOUT,130) IPRGCG
  130 FORMAT(39X,'GCG CONCENTRATION CHANGE PRINTOUT INTERVAL =',I9)
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE SGCG5P(CNCG,LRCH,ITP,MXITER,ITER1,IOUT)
C******************************************************************
C PRINT MAXIMUM CONCENTRATION CHANGES FOR EACH ITERATION DURING
C A TRANSPORT TIME STEP
C******************************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   LRCH,ITP,MXITER,ITER1,IOUT,I,J
      REAL      CNCG
      DIMENSION CNCG(MXITER*ITER1),LRCH(3,MXITER*ITER1)
C
      WRITE(IOUT,5)
    5 FORMAT(1X,' MAXIMUM CONCENTRATION CHANGES FOR EACH ITERATION:'
     &      /1X, 5(' MAX. CHANGE LAYER,ROW,COL')/1X,132('-'))
      WRITE(IOUT,10) (CNCG(J),(LRCH(I,J),I=1,3),J=1,ITP)
   10 FORMAT((1X,5(G12.4,' (',I3,',',I3,',',I3,')')))
C
      RETURN
      END
C
C
      SUBROUTINE GCG5AP(IOUT,MXITER,ITER1,ITO,ITP,METHOD,RELAX,CCLOSE,
     & ICNVG,CNCG,LRCH,NCOL,NROW,NLAY,NODES,NTRANS,KSTP,KPER,TIME2,
     & HT2,UPDLHS,IPRGCG,ICBUND,CINACT,A,CNEW,RHS,Q,WK,NCRS,ISPD)
C **********************************************************************
C SOLUTION BY THE GENERALIZED CONJUGATE GRADIENT METHODS,
C USING ONE OF THE THREE PRECONDITIONERS (JACOBI, SSOR, AND MIC)
C WITH LANCZOS/ORTHOMIN ACCELERATION, UP TO ITER1 ITERATIONS.
C HOWEVER, IF IT IS A FLOW PROBLEM OR IT IS A SYMMETRIC CASE,
C (I.E., ISPD>0), THE ORDINARY CG IS USED.
C **********************************************************************
C                           PARAMETER LIST
C  IOUT    : INTEGER, OUTPUT UNIT NUMBER.(INPUT)
C  MXITER  : INTEGER, OUTER LOOP MAX NUMBER.(INPUT)
C  ITER1   : INTEGER, MAXIMUM NUMBER OF ITERATIONS ALLOWED.(INPUT)
C  ITO     : INTEGER, OUTER LOOP COUNTER.(INPUT)
C  ITP     : INTEGER, ACTUAL NUMBER OF ITERATION REQUIRED.(OUTPUT)
C  METHOD  : INTEGER, BASIC PRECONDITIONER SELECTION.(INPUT)
C                 1=JACOBI, 2=SSOR, 3=MIC
C  RELAX   : REAL, RELAXATION FACTOR FOR THE SSOR METHOD.(INPUT)
C  CCLOSE  : REAL, STOPPING CRITERION.(INPUT)
C  ICNVG   : INTEGER, GLOBAL CONVERGENCE FLAG.(OUTPUT)
C                 1=GLOBAL CONVERGENCE, 0=NOT CONVERGED
C  LICNVG  : INTEGER, LOCAL CONVERGENCE FLAG.(OUTPUT)
C                 1=LOCAL CONVERGENCE, 0=NO CONVERGED
C  LITP    : INTEGER, LOCAL ITERATION COUNTER.
C  CNCG    : REAL ARRAY, CONTAINS MAXIMUM CHANGE OF CONCENTRATION
C            AT EVERY ITERATION.(OUTPUT)
C  LRCH    : INTEGER 2-D ARRAY, CONTAINS THE LOCATIONS OF THE
C            MAXIMUM CHANGE OF CONCENTRATION AT EVERY ITERATION.
C  NCOL,NROW,NLAY:
C            INTEGERS, NUMBER OF COLUMNS, ROWS, AND LAYERS.(INPUT)
C  NODES   : INTEGER, DIMENSION OF THE MATRIX.(INPUT)
C  NTRANS  : INTEGER, TRANSPORT STEP INDEX.(INPUT)
C  KSTP    : INTERGE, FLOW TIME STEP INDEX.(INPUT)
C  KPER    : INTERGE, STRESS PERIOD INDEX.(INPUT)
C  TIME2   : REAL, TOTAL ELAPSED TIME.(INPUT)
C  HT2     : REAL, ACCUMULATED TIME IN CURRENT STRESS PERIOD.(INPUT)
C  UPDLHS  : LOGICAL, FLAG FOR UPDATING COEFF. MATRIX.(INPUT)
C  IPRGCG  : INTEGER, INTERVAL FOR PRINTING MAX. CHANGES.(INPUT)
C  ICBUND  : INTEGER, BOUNDART TYPE INDICATOR.(INPUT)
C  A       : COEFF. MATRIX.(INPUT)
C  CNEW    : REAL ARRAY, CONTAINS CONCENTRATION.(OUTPUT)
C  RHS     : REAL ARRAY, CONTAINS RIGHT HAND SIDE OF THE SYSTEM.(INPUT)
C  Q,DQ    : BASIC ITERATIVE METHOD PRECONDITIONING MATRIX STORAGE.
C  WK      : REAL ARRAY OF LENGTH 7*NCOL*NROW*NLAY, WORK SPACES.
C  NCRS    : INTEGER, 7 OR 19 DIAGONALS INDICATOR.
C  ISPD    : INPUT INTEGER, SYMMETRIC CASE INDICATOR.
C **********************************************************************
C last modified: 2-15-2005
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,NODES,LRCH,I,IDELTA,IRDEL,IPN,
     &          IPA,IAPN,IATPN,IWK,NTRANS,IOUT,II,
     &          ITP,LITP,ITO,ICNVG,LICNVG,IPLACE,MXITER,ITER1,
     &          KSTP,KPER,ICBUND,KLAYER,IROW,JCOLMN,IJ,METHOD,
     &          IPRGCG,NCRS,ISPD
      REAL      CNEW,A,RHS,Q,WK,DELC,DCOLD,ALN,CHANGE,CTEMP,CHTMP,
     &          RLN,CCLOSE,RELAX,HT2,TIME2,CNCG,DELPN,PNAPN,SCALE,
     &          DELAPN,APNPA,GSTOP,RHSNORM,TINY,CINACT
      LOGICAL   UPDLHS
      DIMENSION A(*),CNEW(NODES),
     &          RHS(NODES),Q(*),WK(7*NODES),
     &          CNCG(MXITER*ITER1),LRCH(3,MXITER*ITER1),ICBUND(NODES)
      PARAMETER (TINY=1.E-30)
C
      IDELTA = 1
      IPN    = IDELTA+NODES
      IAPN   = IPN   +NODES
      IWK    = IAPN  +NODES
C
C--THE FOLLOWING WORK SPACE ALLOCATIONS ARE NEEDED FOR
C--NONSYMMETRIC CASES
      IF (ISPD.EQ.0) THEN
      IRDEL  = IWK   +NODES
      IPA    = IRDEL +NODES
      IATPN  = IPA   +NODES
      ENDIF
C
      IF(ITO.EQ.1) ITP=0
      LITP = 0
      ICNVG=0
      LICNVG=0
C
C--NORMALIZE CNEW AND RHS TO AVOID TOO BIG OR TOO SMALL QUANTITIES
C--NORMALIZATION FACTOR IS MAX (CNEW)
      SCALE = 0.
      DO I = 1,NODES
         IF (ICBUND(I).NE.0) THEN
           IF (ABS(CNEW(I)).GT.SCALE) SCALE = CNEW(I)
         ENDIF
      ENDDO
      IF (SCALE .GT. TINY) THEN
         DO I = 1,NODES
           IF (ICBUND(I).NE.0) THEN
             RHS(I) = RHS(I)/SCALE
             CNEW(I) = CNEW(I)/SCALE
           ENDIF
         ENDDO
      ENDIF
C
C--COMPUTE RESIDUAL VECTOR R=RHS-A*CNEW AND TEST FOR SOLUTION
      CALL MVPRD (NODES,NCRS,ICBUND,A,CNEW,WK(IWK))
      GSTOP=0.
      RHSNORM = 0.
      DO I = 1,NODES
         IF (ICBUND(I).NE.0) THEN
            WK(IWK-1+I) = RHS(I)-WK(IWK-1+I)
            RHSNORM = RHSNORM+RHS(I)*RHS(I)
            GSTOP =GSTOP+WK(IWK-1+I)*WK(IWK-1+I)
         ENDIF
      ENDDO
      RHSNORM = SQRT(RHSNORM)
      GSTOP = SQRT(GSTOP)
      IF (RHSNORM.NE.0) GSTOP = GSTOP / RHSNORM
      IF(GSTOP.LE. MIN(1.E-6,CCLOSE) ) THEN
          CHANGE = 0.
          IPLACE = 1
          ITP = ITP + 1
          LITP= LITP+ 1
          IF(UPDLHS.AND.METHOD.EQ.3) CALL MIC (NODES,NCRS,A,Q)
          GO TO 300
      ENDIF
C
C--COMPUTE PSEUDO-RESIDUAL DELTA = Q^-1*(RHS-A*CNEW)
      IF (METHOD.EQ.3) THEN
        IF(UPDLHS) CALL MIC (NODES,NCRS,A,Q)
      ENDIF
C...... JACOBI OR SSOR METHOD .....
      IF(METHOD.EQ.1.OR.METHOD.EQ.2) THEN
        CALL QSOLVE (NODES,METHOD,RELAX,NCRS,A,WK(IWK),WK(IDELTA))
      ELSE
C...... MIC METHOD .....
        CALL QSOLVE (NODES,METHOD,RELAX,NCRS,Q,WK(IWK),WK(IDELTA))
      ENDIF
C
C--BRANCH FOR THE NONSYMMETRIC CASE
C--LANCZOS/ORTHOMIN ACCELERATION IS USED
      IF (ISPD.NE.0) GO TO 200
      DO II = 1,NODES
         WK(IPN-1+II) = WK(IDELTA-1+II)
         WK(IRDEL-1+II) = WK(IDELTA-1+II)
         WK(IPA-1+II) = WK(IDELTA-1+II)
      ENDDO
      DELC = 0.
      DO II = 1,NODES
         IF (ICBUND(II).NE.0)
     &       DELC = DELC + WK(IDELTA-1+II)*WK(IRDEL-1+II)
      ENDDO
      GO TO 100
C
C--COMPUTE DIRECTION VECTORS
 90   CONTINUE
      DCOLD = DELC
      DELC = 0.
      DO II = 1, NODES
         IF (ICBUND(II).NE.0)
     &       DELC = DELC + WK(IDELTA-1+II)*WK(IRDEL-1+II)
      ENDDO
      ALN = DELC / DCOLD
      DO I = 1,NODES
         WK(IPN-1+I) = WK(IDELTA-1+I) + ALN * WK(IPN-1+I)
         WK(IPA-1+I) = WK(IRDEL-1+I) + ALN * WK(IPA-1+I)
      ENDDO
C
C--COMPUTE NEW ITERATES
 100  CONTINUE
      ITP = ITP + 1
      LITP = LITP + 1
      CALL MVPRD (NODES,NCRS,ICBUND,A,WK(IPN),WK(IWK))
      IF(METHOD.EQ.1.OR.METHOD.EQ.2) THEN
        CALL QSOLVE (NODES,METHOD,RELAX,NCRS,A,WK(IWK),WK(IAPN))
        CALL QTSLVE (NODES,METHOD,RELAX,NCRS,A,WK(IPA),WK(IWK)) 
      ELSE
        CALL QSOLVE (NODES,METHOD,RELAX,NCRS,Q,WK(IWK),WK(IAPN))
        CALL QTSLVE (NODES,METHOD,RELAX,NCRS,Q,WK(IPA),WK(IWK)) 
      ENDIF
C
      CALL MTVPRD (NODES,NCRS,ICBUND,A,WK(IWK),WK(IATPN))
      APNPA = 0.
      DO II = 1, NODES
         IF(ICBUND(II).NE.0)
     &      APNPA = APNPA + WK(IAPN-1+II)*WK(IPA-1+II)
      ENDDO
      IF(APNPA.NE.0) RLN = DELC / APNPA
      CHANGE = 0.0
      IPLACE = 1
      DO I = 1,NODES
         CTEMP = CNEW(I)
         CNEW(I) = CNEW(I) + RLN * WK(IPN-1+I)
         WK(IDELTA-1+I) = WK(IDELTA-1+I) - RLN * WK(IAPN-1+I)
         WK(IRDEL-1+I) = WK(IRDEL-1+I) - RLN * WK(IATPN-1+I)
         CHTMP = ABS(CNEW(I)-CTEMP)
         IF (CHTMP.GT.CHANGE) THEN
            CHANGE = CHTMP
            IPLACE = I
         ENDIF
      ENDDO
      GO TO 300
 200  CONTINUE
C
C--THE FOLLOWING IS FOR THE SYMMETRIC CASE
C--ORDINARY CG ACCELERATION IS USED
      DO II = 1, NODES
         WK(IPN-1+II) = WK(IDELTA-1+II)
      ENDDO
      GO TO 220
C
C--COMPUTE DIRECTION VECTORS
 210  CONTINUE
      DELAPN = 0.
      DO II = 1, NODES
         IF(ICBUND(II).NE.0)
     &      DELAPN = DELAPN + WK(IDELTA-1+II)*WK(IAPN-1+II)
      ENDDO
      ALN = -DELAPN / PNAPN
      DO I = 1,NODES
         WK(IPN-1+I) = WK(IDELTA-1+I) + ALN * WK(IPN-1+I)
      ENDDO
C
C--COMPUTE NEW ITERATES
 220  CONTINUE
      ITP = ITP + 1
      LITP = LITP + 1
      DELPN = 0.
      DO II = 1, NODES
         IF(ICBUND(II).NE.0)
     &      DELPN = DELPN + WK(IDELTA-1+II)*WK(IPN-1+II)
      ENDDO
      CALL MVPRD (NODES,NCRS,ICBUND,A,WK(IPN),WK(IWK))
      IF(METHOD.EQ.1.OR.METHOD.EQ.2) THEN
        CALL QSOLVE (NODES,METHOD,RELAX,NCRS,A,WK(IWK),WK(IAPN))
      ELSE
        CALL QSOLVE (NODES,METHOD,RELAX,NCRS,Q,WK(IWK),WK(IAPN))
      ENDIF
      PNAPN = 0.
      DO II = 1, NODES
         IF(ICBUND(II).NE.0)
     &      PNAPN = PNAPN + WK(IAPN-1+II)*WK(IPN-1+II)
      ENDDO
      IF(PNAPN.NE.0) RLN = DELPN / PNAPN
      CHANGE = 0.0
      IPLACE = 1
      DO I = 1,NODES
         CTEMP = CNEW(I)
         CNEW(I) = CNEW(I) + RLN * WK(IPN-1+I)
         WK(IDELTA-1+I) = WK(IDELTA-1+I) - RLN * WK(IAPN-1+I)
         CHTMP = ABS(CNEW(I)-CTEMP)
         IF (CHTMP.GT.CHANGE) THEN
            CHANGE = CHTMP
            IPLACE = I
         ENDIF
      ENDDO
C
 300  CONTINUE
C
C--STORE MAXIMUM CHANGE VALUE AND LOCATION
      CNCG(ITP) = CHANGE
      KLAYER = (IPLACE-1) / (NCOL*NROW) + 1
      IJ = IPLACE - (KLAYER-1)*NCOL*NROW
      IROW = (IJ-1)/NCOL + 1
      JCOLMN = IJ - (IROW-1)*NCOL
        LRCH(1,ITP) = KLAYER
        LRCH(2,ITP) = IROW
        LRCH(3,ITP) = JCOLMN
C
      WRITE(*,1111) ITO,LITP,CHANGE,KLAYER,IROW,JCOLMN
 1111 FORMAT(1X,'Outer Iter.',I3,'  Inner Iter.',I3,
     &  ':  Max. DC =',G12.4,'  [K,I,J]',3I5)
C
C--CHECK CONVERGENCE ......
      IF(CHANGE.LE.CCLOSE) THEN
         LICNVG=1
      ENDIF
      IF(MXITER.EQ.1) THEN
        IF(LICNVG.EQ.1) ICNVG=1
      ELSEIF(ITO.GT.1) THEN
        IF(LICNVG.EQ.1.AND.LITP.EQ.1) ICNVG=1
      ENDIF
C
C--LOCAL CONVERGENCE NOT MET, LOOP BACK
      IF(LICNVG.EQ.0 .AND. LITP.LT.ITER1) THEN
        IF(ISPD.EQ.0) THEN
          GOTO 90
        ELSE
          GOTO 210
        ENDIF
      ENDIF
      IF(ICNVG.EQ.0 .AND. ITO.NE.MXITER) GOTO 600
      IF(NTRANS.EQ.1) WRITE(IOUT,1000)
 1000 FORMAT(/1X)
      WRITE(IOUT,1010) ITO,NTRANS,KSTP,KPER,ITP
 1010 FORMAT(1X,I5,' CALLS TO GCG PACKAGE FOR TRANSPORT TIME STEP',I4,
     & ' IN FLOW TIME STEP',I4,' STRESS PERIOD',I4,
     & /1X,I5,' TOTAL ITERATIONS')
C
      IF(ICNVG.EQ.0 .OR. TIME2.GE.HT2 .OR. MOD(NTRANS,IPRGCG).EQ.0)
     & CALL SGCG5P(CNCG,LRCH,ITP,MXITER,ITER1,IOUT)
C
  600 CONTINUE
C
C--BEFORE RETURN UNSCALE CNEW AND RHS
         IF (SCALE .GT. TINY) THEN
            DO I = 1,NODES
              IF (ICBUND(I).NE.0) THEN
                RHS(I) = RHS(I)*SCALE
                CNEW(I) = CNEW(I)*SCALE
              ELSE
                CNEW(I)=CINACT
              ENDIF
            ENDDO
         ENDIF
      RETURN
      END
C
C
      SUBROUTINE MVPRD (N,NCRS,ICBUND,A,X,Y)
C*******************************************************************
C...  THIS SUBROUTINE, MVPRD, PERFORMS AX=Y
C...  WHERE THE MATRIX A IS STORED IN DIAGONAL FORM
C*******************************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   N,L,IDIAG,I,JCOL,K,ICBUND,NCRS
      REAL      A,X,Y
      DIMENSION A(N,*), X(N), Y(N), ICBUND(N)
      COMMON   /GCGIDX/L(19)
C
      IDIAG = 7
      IF(NCRS.GT.0) IDIAG = 19
      DO I = 1,N
         Y(I) = 0.
         DO K = 1,IDIAG
            JCOL = I + L(K)
            IF (JCOL.GE.1.AND.JCOL.LE.N) THEN
             IF(ICBUND(JCOL).NE.0) Y(I) = Y(I)+A(I,K)*X(JCOL)
            ENDIF
         ENDDO
      ENDDO
C
      RETURN
      END
C
C
      SUBROUTINE MTVPRD (N,NCRS,ICBUND,A,X,Y)
C*********************************************************************
C.... THIS SUBROUTINE, MTVPRD, PERFORMS A^TX=Y
c*********************************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   N,L,IDIAG,I,JCOL,ICBUND,NCRS,J
      REAL      A,X,Y
      DIMENSION A(N,*),X(N),Y(N),ICBUND(N)
      COMMON   /GCGIDX/L(19)
C
      IDIAG = 7
      IF(NCRS.GT.0) IDIAG = 19
      DO I = 1,N
         Y(I) = 0.
      ENDDO
      DO I = 1,N
         DO J = 1,IDIAG
            JCOL = I + L(J)
            IF(JCOL.GE.1.AND.JCOL.LE.N) THEN
             IF (ICBUND(JCOL).NE.0) Y(JCOL) = Y(JCOL)+A(I,J)*X(I)
            ENDIF
         ENDDO
      ENDDO
C
      RETURN
      END
C
C
        SUBROUTINE QSOLVE(N,METHOD,RELAX,NCRS,A,SY,Y)
C*********************************************************************
C*... FUNCTION:
C*
C*         THIS SUBROUTINE, QSOLVE, PERFORMS THE FORWARD AND BACKWARD
C*         SUBSTITUTION.  WHICH SOLVES   Q * Y = SY  FOR THE JACOBI
C*         SSOR, AND MIC METHODS.
C*
C*... PARAMETER LIST
C*
C*         N       : INPUT INTEGER, THE DIMENSION OF THE SYSTEM
C*         METHOD  : INPUT INTEGER, DEFINES BASIC ITERATIVE METHOD
C*         RELAX   : INPUT REAL, RELAXATION FACTOR FOR SSOR
C*         NCRS    : INPUT INTEGER, 7 OR 19 DIAGONALS INDICATOR.
C*         A       :  INPUT REAL ARRAY. CONTAINS THE NONZERO ELEMENTS
C*                    OF THE COEFFICIENT MATRIX A.
C*         SY      :  INPUT REAL ARRAY. THE RIGHT HAND SIDE OF THE
C*                    SYSTEM.
C*         Y       :  OUTPUT REAL ARRAY. CONTAINS THE SOLUTION.
C*********************************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   N,METHOD,NCRS,LL,LU,I,J,K,II,IDIAG,JCOL,L
      REAL      Y,SY,A,RELAX
      DIMENSION Y(N),SY(N),A(N,*),LL(9),LU(9)
      COMMON   /GCGIDX/ L(19)
C
      DO II = 1,N
         Y(II) = SY(II)
      ENDDO
C
      IF (METHOD .NE. 1) GO TO 5
C
C ... JACOBI METHOD
C ... NOTE: THE FIRST ELEMENT IN EACH ROW OF THE MATRIX A IS THE 
C ....      DIAGONAL ELEMENT OF THE ROW.
C
      DO I=1,N
        Y(I)=Y(I)/A(I,1)
      ENDDO
      RETURN
C
C     SOLVE LDUY=Y
C
 5    CONTINUE
      IF (METHOD .NE. 2) RELAX = 1.0
      IF (NCRS.GT.0) THEN
         IDIAG = 9
      ELSE
         IDIAG = 3
      ENDIF
C
      LL(1) = 2
      LL(2) = 4
      LL(3) = 6
      LU(1) = 3
      LU(2) = 5
      LU(3) = 7
      IF (NCRS.GT.0) THEN
         LL(4) = 8
         LL(5) = 9
         LL(6) = 10
         LL(7) = 11
         LL(8) = 16
         LL(9) = 17
         LU(4) = 12
         LU(5) = 13
         LU(6) = 14
         LU(7) = 15
         LU(8) = 18
         LU(9) = 19
      ENDIF
C
C ... SOLVE LOWER TRIANGULAR SYSTEM FOR THE SSOR METHOD
C
      DO I=1,N
         DO J = 1,IDIAG
            JCOL = I + L(LL(J)) 
            IF (JCOL.GT.0) Y(I) = Y(I) - A(I,LL(J))*Y(JCOL)
         ENDDO
         Y(I) = RELAX*Y(I) / A(I,1)
      ENDDO
C
C     SOLVE DY=Y
C
      IF (METHOD .EQ. 2) THEN
         DO I = 1,N
            Y(I) = (2.0-RELAX)/RELAX * Y(I) * A(I,1)
         ENDDO
      ELSE
         DO I=1,N
            Y(I)=Y(I)*A(I,1)
         ENDDO
      ENDIF
C
C     SOLVE UY=Y
C
      DO I=N,1,-1
         DO K=1,IDIAG
            JCOL = I + L(LU(K))
            IF (JCOL.LE.N) Y(I)=Y(I)-A(I,LU(K))*Y(JCOL)
         ENDDO
         Y(I)=RELAX*Y(I)/A(I,1)
      ENDDO
C
      RETURN
      END
C
C
      SUBROUTINE QTSLVE(N,METHOD,RELAX,NCRS,A,SY,Y)
C***********************************************************************
C*... FUNCTION:
C*
C*         THIS SUBROUTINE, QTSLVE, PERFORMS THE DRIVER OF SOLVING
C*         THE SYSTEM  Q ** (T) * Y = SY  WHERE THE MATRIX Q IS IN THE
C*         L * D * U FORM.
C*
C*... PARAMETER LIST:
C*
C*         N          : INPUT INTEGER, THE DIMENSION OF THE SYSTEM
C*         METHOD     : INPUT INTEGER, DEFINES BASIC ITERATIVE METHOD
C*         RELAX      : INPUT REAL, RELAXATION FACTOR FOR SSOR
C*         NCRS       : INPUT INTEGER, 7 OR 19 DIAGONALS INDICATOR
C*         A          : INPUT REAL ARRAY, THE COEFFICIENT MATRIX
C*         SY         : INPUT REAL ARRAY. IT CONTAINS THE RIGHT HAND
C*                      SIDE OF THIS SYSTEM.
C*         Y          : OUTPUT REAL ARRAY. IT CONTAINS THE SOLUTION OF
C*                      THIS SYSTEM.
C***********************************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   N,METHOD,NCRS,LL,LU,I,J,K,II,IDIAG,JCOL,L
      REAL      Y,SY,A,RELAX
      DIMENSION Y(N),SY(N),A(N,*),LL(9),LU(9)
      COMMON   /GCGIDX/ L(19)
C
      DO II = 1,N
        Y(II) = SY(II)
      ENDDO
C
      IF (METHOD .NE. 1) GOTO 5
C
C ... JACOBI METHOD 
C
      DO I = 1,N
        Y(I) = Y(I) / A(I,1)
      ENDDO
      RETURN
C
C ... SOLVE LDU ** (T) * Y = Y
C
 5    CONTINUE
      IF (METHOD .NE. 2) RELAX = 1.0
      IF (NCRS.GT.0) THEN
         IDIAG = 9
      ELSE
         IDIAG = 3
      ENDIF
C
      LL(1) = 2
      LL(2) = 4
      LL(3) = 6
      LU(1) = 3
      LU(2) = 5
      LU(3) = 7
      IF (NCRS.GT.0) THEN
         LL(4) = 8
         LL(5) = 9
         LL(6) = 10
         LL(7) = 11
         LL(8) = 16
         LL(9) = 17
         LU(4) = 12
         LU(5) = 13
         LU(6) = 14
         LU(7) = 15
         LU(8) = 18
         LU(9) = 19
      ENDIF
C
C ... SOLVE (UT)Y = Y
C
      DO I=1,N
         Y(I) = RELAX * Y(I) / A(I,1)
         DO J = 1,IDIAG
            JCOL = I + L(LU(J))
            IF (JCOL.LE.N) Y(JCOL) = Y(JCOL) - A(I,LU(J))*Y(I)
         ENDDO
      ENDDO
C
C     SOLVE DY=Y
C
      IF (METHOD .EQ. 2) THEN
         DO I = 1,N
            Y(I) = (2.0-RELAX)/RELAX * Y(I) * A(I,1)
         ENDDO
      ELSE
         DO I=1,N
            Y(I)=Y(I)*A(I,1)
         ENDDO
      ENDIF
C
C     SOLVE (LT)Y=Y
C
      DO I=N,1,-1
         Y(I) = RELAX * Y(I) / A(I,1)
         DO K = 1,IDIAG
            JCOL = I + L(LL(K))
            IF (JCOL.GT.0) Y(JCOL) = Y(JCOL) - A(I,LL(K))*Y(I) 
         ENDDO
      ENDDO
C
      RETURN
      END
C
C
      SUBROUTINE MIC(N,NCRS,A,Q)
C***********************************************************************
C*... FUNCTION:
C*
C*         THIS SUBROUTINE, MIC, PERFORMS
C*         THE MODIFIED INCOMPLETE CHOLESKY FACTORIZATION. 
C*
C*... PARAMETER USED IN THIS SUBROUTINE:
C*
C*         N       INPUT INTEGER. DIMENSION OF THE MATRIX A. (= N)
C*         NCOL    NUMBER OF COLUMNS
C*         NRC     NUMBER OF COLUMNS TIMES NUMBER OF ROWS
C*         NCRS    INPUT INTEGER, 7 OR 19 DIAGONALS INDICATOR
C*         A       INPUT REAL VECTOR. CONTAINS THE NONZERO ELEMENTS
C*                 OF THE MATRIX.
C*         Q       OUTPUT, CONTAINS THE COMPACT LDU FORM FOR THE MIC
C**********************************************************************
C last modified: 02-15-2005
C
      IMPLICIT  NONE
      INTEGER   K,II,IJ,IPVT,IICOL,ILST,IROW,JP,JCOL,KK,LL,IERR,
     &          LU,NCRS,IDIAG,L,N,J,NRC,NCOL
      REAL      A,Q,QMULT,TINY
      PARAMETER (TINY=1.E-30)
      DIMENSION A(N,*),Q(N,*),LU(9)
      COMMON   /GCGIDX/ L(19)
C
C ... SET INITIAL PARAMETERS
C
      NRC   = L(3)
      NCOL  = L(5)
      LU(1) = 3
      LU(2) = 5
      LU(3) = 7
      IF (NCRS.GT.0) THEN
         LU(4) = 12
         LU(5) = 13
         LU(6) = 14
         LU(7) = 15
         LU(8) = 18
         LU(9) = 19
      ENDIF
      IF (NCRS.GT.0) THEN
          IDIAG = 19
          ILST  = 9
      ELSE
          IDIAG = 7
          ILST  = 3
          IF(NCOL.EQ.1) ILST = 2
          IF(NRC.EQ.1)  ILST = 1
      ENDIF
      DO K = 1,N
         DO J = 1, IDIAG
            Q(K,J) = A(K,J)
         ENDDO
      ENDDO
      IF(ABS(Q(1,1)).LT.TINY) Q(1,1)=1.
C
      DO IPVT = 1,N-1
        DO KK = 1,ILST
          IROW = IPVT+L(LU(KK))
          IF(IROW.GT.N) CYCLE
          DO II = 1,IDIAG
            IICOL = IROW + L(II)
            IF (IICOL .EQ. IPVT) GO TO 95
          ENDDO
          CYCLE
 95       QMULT = Q(IROW,II)
          DO LL = 1,ILST
            JCOL = IPVT + L(LU(LL))
            IERR = 1
            DO IJ = 1,IDIAG
               JP = IROW + L(IJ)
               IF(JP .EQ. JCOL) THEN
                 IERR = 0
                 EXIT
               ENDIF
            ENDDO
            IF(IERR.EQ.0) THEN
              Q(IROW,IJ)=Q(IROW,IJ)-QMULT*Q(IPVT,LU(LL))/Q(IPVT,1)
            ELSE
              Q(IROW,1) =Q(IROW,1) -QMULT*Q(IPVT,LU(LL))/Q(IPVT,1)
              IF(ABS(Q(IROW,1)).LT.TINY) Q(IROW,1)=1.
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END