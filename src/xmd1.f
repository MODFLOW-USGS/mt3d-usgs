
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
C      USE GLOBAL, ONLY: IOUT
      USE MT3DMS_MODULE, ONLY: IOUT
      USE XMDMODULE
C      USE GWFNWTMODULE, ONLY: IPRNWT,NUMACTIVE,IA,JA,NJA,IFDPARAM
cmi
      use xmdcmn
cmi
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
CVSB      EXTERNAL URDCOM, URWORD
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
      INTEGER lloc, istart, istop, i, n,ISTORXMD,IREDSYS, Icomb,IFDPARAM
      INTEGER IPRNWT,NUMACTIVE
      CHARACTER(LEN=200) line
      REAL R,RRCTOLS,EPSRNS,HCLOSEXMDDUM
!     LOCAL VARIABLES FOR XMD SOLVER
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE AND INITIALIZE.
CVSB      WRITE(IOUT,1)IN
CVSB    1 FORMAT(1X,'XMD -- LINEAR SOLUTION BY XMD PACKAGE VERSION',
CVSB     &       1X,'1.30',
CVSB     & /1X,'    BY MOTOMU IBARAKI, OHIO STATE UNIVERSITY, COLOMBUS, OH',
CVSB     & /1X,'                INPUT READ FROM UNIT',I3)
C
cmi
c      ALLOCATE (IACL,NORDER,NJAF,LEVEL,NORTH,LIWRK,LRWRK,IDROPTOL,
c     *  NBLACK,IERR,IDSCALE,MBLACK,HCLOSEXMD,MXITERXMD)
c      ALLOCATE (EPSRN,RRCTOL)
c      ALLOCATE (REDSYS,LDCOMB)
cmi
      ALLOCATE (IACL,NORDER,LEVEL,NORTH,IDROPTOL,
     *  IERR,IDSCALE,HCLOSEXMD,MXITERXMD)
      ALLOCATE (EPSRN,RRCTOL)
      ALLOCATE (REDSYS,LDCOMB)
!-----XMD INPUT
CVSB      IF ( IFDPARAM.EQ.4 )CALL URDCOM(In, Iout, line)
      lloc = 1
      i = 1
CVSB      IF ( IFDPARAM.EQ.4 ) THEN
CVSB      CALL URWORD(line, lloc, istart, istop, 2, IACL, r, Iout, In)
CVSB      CALL URWORD(line, lloc, istart, istop, 2, NORDER, r, Iout, In)
CVSB      CALL URWORD(line, lloc, istart, istop, 2, LEVEL, r, Iout, In)
CVSB      CALL URWORD(line, lloc, istart, istop, 2, NORTH, r, Iout, In)
CVSB      CALL URWORD(line, lloc, istart, istop, 2, IREDSYS, r, Iout, In)
CVSB      CALL URWORD(line, lloc, istart, istop, 3, I, RRCTOLS, Iout, In)
CVSB      CALL URWORD(line, lloc, istart, istop, 2, IDROPTOL, r, Iout, In)
CVSB      CALL URWORD(line, lloc, istart, istop, 3, I, EPSRNS, Iout, In)
CVSB      CALL URWORD(line, lloc, istart, istop, 3, I,HCLOSEXMDDUM,Iout, In)
CVSB      CALL URWORD(line, lloc, istart, istop, 2, MXITERXMD,r,Iout,In)  !---added this 12/29/10
CVSB      ELSEIF ( IFDPARAM.EQ.1 ) THEN
CVSB        IACL = 2
CVSB        NORDER = 1
CVSB        LEVEL = 1
CVSB        NORTH = 2
CVSB        IREDSYS = 0
CVSB        RRCTOLS = 0.0
CVSB        IDROPTOL = 1
CVSB        EPSRNS = 1.0e-3
CVSB        HCLOSEXMDDUM = 1.0e-4
CVSB        MXITERXMD = 50
CVSB      ELSEIF ( IFDPARAM.EQ.2 ) THEN
CVSB        IACL = 2
CVSB        NORDER = 1
CVSB        LEVEL = 1
CVSB        NORTH = 2
CVSB        IREDSYS = 0
CVSB        RRCTOLS = 0.0
CVSB        IDROPTOL = 1
CVSB        EPSRNS = 1.0e-3
CVSB        HCLOSEXMDDUM = 1.0e-4
CVSB        MXITERXMD = 50
CVSB      ELSEIF ( IFDPARAM.EQ.3 ) THEN
        IACL = 1 !2
        NORDER = 0
        LEVEL = 1
        NORTH = 7
        IREDSYS = 0
        RRCTOLS = 0.0
        IDROPTOL = 1
        EPSRNS = 1.0e-3
        HCLOSEXMDDUM = 1.0e-4
        MXITERXMD = 50
CVSB      END IF    
      HCLOSEXMD = dble(HCLOSEXMDDUM)
!
!
!
c      read (*,*) IACL, NORDER, LEVEL, NORTH, IREDSYS, IDROPTOL, Icomb,
c     [            EPSRNS
c
c      write (*,1192) IACL, NORDER, LEVEL, NORTH, IREDSYS, IDROPTOL,
c     [               Icomb, EPSRNS
c
c 1192 format(7i8, 1pe10.3)
C
      IF ( HCLOSEXMD.LT.1.0e-8 ) HCLOSEXMD = 1.0e-8
      RRCTOL = RRCTOLS
      EPSRN = EPSRNS
      MIUNIT = IOUT
      MIOUT = -1 !IPRNWT - 2
      if(north.eq.0)north = 7   !!!!
      IF(EPSRN.LT.1.0E-20) EPSRN = 1.0e-3
      REDSYS = .FALSE.
      IDSCALE = 0
      IF(IREDSYS.EQ.1) THEN
        REDSYS = .TRUE.
        IDSCALE = 1   ! NEED DIAGONAL SCALING FOR REDUCED SYSTEMS
      ENDIF
C
CVSB      WRITE(IOUT,23) IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOL,
CVSB     *  IDROPTOL, EPSRN, Hclosexmd, MXITERXMD
   23 FORMAT(1X,'ACCELERATION METHOD                    (IACL) = ',I9/
     *      1X,'NODE ORDERING FLAG                   (NORDER) = ',I9/
     *      1X,'LEVEL OF FILL                         (LEVEL) = ',I9/
     *      1X,'MAXIMUM NUMBER OF ORTHOGONALIZATIONS  (NORTH) = ',I9/
     *      1X,'INDEX FOR USING REDUCED SYSTEM      (IREDSYS) = ',I9/
     *      1X,'RESID. REDUCTION CONVERGE CRITERION  (RRCTOL) = ',E13.6/
     *      1X,'INDEX FOR USING DROP TOLERANCE     (IDROPTOL) = ',I9/
     *      1X,'DROP TOLERANCE VALUE                  (EPSRN) = ',E13.6/
     *      1X,'CONVERGENCE CRITERIA OF           (HCLOSEXMD) = ',E13.6/
     *      1X,'MAX. NUMBER OF LINEAR ITERATIONS  (MXITERXMD) = ',I9/)
!
!4-----ALLOCATE SPACE USED BY SOLVER
CVSB      NODES = NUMACTIVE
      NODES=NSTRM
      numactive=NSTRM
      ALLOCATE(DGSCAL(NODES))
      DGSCAL = 0.0
cmi
c  -----------------
c     preprocessor
c  -----------------

CVSB      call xmdprpc(ia, ja, nja, numactive, norder, ierr, redsys)
      call xmdprpc(IASF, JASF, NJASF, numactive, norder, ierr, redsys)

c  ------------------------------------
c     check array sizes and structure
c  ------------------------------------
CVSB      call xmdcheck(ia, ja, numactive, nja, ierr)
      call xmdcheck(IASF, JASF, numactive, NJASF, ierr)

c  ---------------------------------------------------
c     PERFORM SYMBOLIC FACTORIZATION FOR LEVEL BASED PRECONDITIONING
c  ---------------------------------------------------
      IF(IDROPTOL.EQ.0)THEN
c  --------------------------------
c     level based preconditioning
c  --------------------------------
CVSB        call xmdprecl(ia, ja, level, nja, nodes, ierr)
        call xmdprecl(IASF, JASF, level, NJASF, nodes, ierr)
      ENDIF
C
      RETURN
      END
C-----------------------------------------------------------------------------------
      SUBROUTINE XMD7DA()
C  DEALLOCATE GLOBAL DATA
      use xmdcmn
      use xmdmatrix
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



