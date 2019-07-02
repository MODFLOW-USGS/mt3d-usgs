      MODULE MT3DMS_MODULE
        IMPLICIT NONE
        REAL,         SAVE, DIMENSION(:,:,:), POINTER :: DH,QX,QY,QZ,QSTO
        INTEGER :: ISS = 1
  CONTAINS
  
        subroutine memallocate(nlay, nrow, ncol)
          integer, intent(in) :: nlay, nrow, ncol
          IF(.not. ASSOCIATED(DH))   ALLOCATE(DH(NCOL,NROW,NLAY))
          IF(.not. ASSOCIATED(QX))   ALLOCATE(QX(NCOL,NROW,NLAY))
          IF(.not. ASSOCIATED(QY))   ALLOCATE(QY(NCOL,NROW,NLAY))
          IF(.not. ASSOCIATED(QZ))   ALLOCATE(QZ(NCOL,NROW,NLAY))
          IF(.not. ASSOCIATED(QSTO)) ALLOCATE(QSTO(NCOL,NROW,NLAY))
        end subroutine
        
        SUBROUTINE MEMDEALLOCATE_DUP()
          IF(ASSOCIATED(DH))   DEALLOCATE(DH)
          IF(ASSOCIATED(QX))   DEALLOCATE(QX)
          IF(ASSOCIATED(QY))   DEALLOCATE(QY)
          IF(ASSOCIATED(QZ))   DEALLOCATE(QZ)
          IF(ASSOCIATED(QSTO)) DEALLOCATE(QSTO)
        END SUBROUTINE
      END MODULE
