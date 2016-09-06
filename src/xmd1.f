
c             for XMD package (version 1.3)
c
c                     M. Ibaraki
c
c             Thu Oct 29 10:03:25 EDT 2009
c
c     variable definitions:
c
c      iacl               choice of acceleration method
c                         = 0; conjugate gradient
c                         = 1; ORTHOMIN
c                         = 2; CGSTAB
c      n                  number of unknowns
c      norder             = 0; original ordering
c                         = 1; RCM ordering
c                         = 2; Minimum Degree ordering
c      nja                size of ja, a, arrays
c      njaf               size of af, jaf arrays
c      level              level of ILU
c      itmax              number of maximum allowable iterations
c      north              number of orthogonalization for the ORTHOMIN
c      liwrk              size of integer work array
c      lrwrk              size of real work array
c
c      ia(n+1),ja(nja)    usual ia, ja arrays for coefficient matrix
c      lorder(n)          ordering vector: lorder( new_order ) = old_order
c      iwork(liwrk)      temporary work array
c
c      dptol              flag for the drop tolerance
c                         =.true. perform the drop tolerance
c                         =.false. do NOT perform the drop tolerance
c
c      epsrn              drop tolerance
c      ctol               absolute convergence criteria
c      rrctol             residual reduction convergence criteria
c
c      rwork(lrwrk)       temporary work array
c      a(nja)             matrix stored as linear array
c      af(njaf)           factored matrix (each row of af contains a row L\U)
c                         where A = LU
c      b(n)               right hand side vector
c      x(n)               solution vector
c
c
c      nx,ny,nz           graph of matrix is regular rectangular grid
c                         of size nx * ny * nz
c
cmi
c      MODULE XMDMODULE
c      IMPLICIT NONE
c      LOGICAL, SAVE, POINTER ::  REDSYS,LDCOMB
c      DOUBLE PRECISION, SAVE, POINTER ::  EPSRN,RRCTOL,HCLOSEXMD
c      INTEGER, SAVE, POINTER ::  MXITERXMD
c      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER ::  RWORK,AF,DGSCAL
c      INTEGER, SAVE, DIMENSION(:), POINTER ::  LORDER,IWORK,MSINDX
c      INTEGER, SAVE, POINTER :: IACL,NORDER,NJAF,LEVEL,NORTH,LIWRK,
c     *  LRWRK,IDROPTOL,NBLACK,IERR,IDSCALE,MBLACK
c      END MODULE XMDMODULE
cmi


      MODULE XMDMODULE
      IMPLICIT NONE
      LOGICAL, SAVE, POINTER ::  REDSYS,LDCOMB
      DOUBLE PRECISION, SAVE, POINTER ::  EPSRN,RRCTOL,HCLOSEXMD
      INTEGER, SAVE, POINTER ::  MXITERXMD
      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER ::  DGSCAL
      INTEGER, SAVE, POINTER :: IACL,NORDER,LEVEL,NORTH,IDROPTOL,
     [                          IERR,IDSCALE
      END MODULE XMDMODULE
C------------------------------------------------------------------
      SUBROUTINE XMD7AR()
      USE SFRVARS
      USE MT3DMS_MODULE, ONLY: IOUT
      USE XMDMODULE
cmi
      USE xmdcmn
cmi
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
cmi
c     include 'xmdcmn.com'
cmi
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER IN,NODES
!     ------------------------------------------------------------------
!     LOCAL VARIABLES
!     ------------------------------------------------------------------
      INTEGER            lloc, istart, istop, i, n,ISTORXMD,IREDSYS, 
     &                   Icomb,IFDPARAM
      INTEGER            IPRNWT,NUMACTIVE
      CHARACTER(LEN=200) line
      REAL               R,RRCTOLS,EPSRNS,HCLOSEXMDDUM
!     LOCAL VARIABLES FOR XMD SOLVER
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE AND INITIALIZE.
C
      ALLOCATE (IACL,NORDER,LEVEL,NORTH,IDROPTOL,
     &          IERR,IDSCALE,HCLOSEXMD,MXITERXMD)
      ALLOCATE (EPSRN,RRCTOL)
      ALLOCATE (REDSYS,LDCOMB)
!-----XMD INPUT
      lloc = 1
      i = 1
      IACL = 1 !2
      NORDER = 0
      LEVEL = 1
      NORTH = 7
      IREDSYS = 0
      RRCTOLS = 1.0e-06 !0.0
      IDROPTOL = 0 !1
      EPSRNS = 1.0e-10
      HCLOSEXMDDUM = 1.0e-4
      MXITERXMD = 50
C
      HCLOSEXMD = dble(HCLOSEXMDDUM)
C
      IF ( HCLOSEXMD.LT.1.0e-8 ) HCLOSEXMD = 1.0e-8
      RRCTOL = RRCTOLS
      EPSRN = EPSRNS
      MIUNIT = IOUT
      MIOUT = -1 !IPRNWT - 2
      IF(north.eq.0)north = 7   
      IF(EPSRN.LT.1.0E-20) EPSRN = 1.0e-3
      REDSYS = .FALSE.
      IDSCALE = 0
      IF(IREDSYS.EQ.1) THEN
        REDSYS = .TRUE.
        IDSCALE = 1   ! NEED DIAGONAL SCALING FOR REDUCED SYSTEMS
      ENDIF
C
   23 FORMAT(1X,'ACCELERATION METHOD                    (IACL) = ',I9/
     &      1X,'NODE ORDERING FLAG                   (NORDER) = ',I9/
     &      1X,'LEVEL OF FILL                         (LEVEL) = ',I9/
     &      1X,'MAXIMUM NUMBER OF ORTHOGONALIZATIONS  (NORTH) = ',I9/
     &      1X,'INDEX FOR USING REDUCED SYSTEM      (IREDSYS) = ',I9/
     &      1X,'RESID. REDUCTION CONVERGE CRITERION  (RRCTOL) = ',E13.6/
     &      1X,'INDEX FOR USING DROP TOLERANCE     (IDROPTOL) = ',I9/
     &      1X,'DROP TOLERANCE VALUE                  (EPSRN) = ',E13.6/
     &      1X,'CONVERGENCE CRITERIA OF           (HCLOSEXMD) = ',E13.6/
     &      1X,'MAX. NUMBER OF LINEAR ITERATIONS  (MXITERXMD) = ',I9/)
!
!4-----ALLOCATE SPACE USED BY SOLVER
      NODES=NSTRM
      numactive=NSTRM
      ALLOCATE(DGSCAL(NODES))
      DGSCAL = 0.0
cmi
c  -----------------
c     preprocessor
c  -----------------
      CALL xmdprpc(IASF, JASF, NJASF, numactive, norder, ierr, redsys)
c  ------------------------------------
c     check array sizes and structure
c  ------------------------------------
      CALL xmdcheck(IASF, JASF, numactive, NJASF, ierr)
c  ---------------------------------------------------
c     PERFORM SYMBOLIC FACTORIZATION FOR LEVEL BASED PRECONDITIONING
c  ---------------------------------------------------
      IF(IDROPTOL.EQ.0)THEN
c  --------------------------------
c     level based preconditioning
c  --------------------------------
        CALL xmdprecl(IASF, JASF, level, NJASF, nodes, ierr)
      ENDIF
C
      RETURN
      END
C-----------------------------------------------------------------------------------
      SUBROUTINE XMD7DA()
C  DEALLOCATE GLOBAL DATA
      USE xmdcmn
      USE xmdmatrix
      INTEGER ALLOC_ERR
C
      DEALLOCATE(icolour, STAT = ALLOC_ERR)
      DEALLOCATE(RBorder, STAT = ALLOC_ERR)
      DEALLOCATE(iblackend, STAT = ALLOC_ERR)
      DEALLOCATE(lorder, STAT = ALLOC_ERR)
      DEALLOCATE(iaf, STAT = ALLOC_ERR)
      DEALLOCATE(idiagf, STAT = ALLOC_ERR)
      RETURN
      END



