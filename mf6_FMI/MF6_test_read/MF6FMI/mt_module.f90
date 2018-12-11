      MODULE MT3DMS_MODULE
        IMPLICIT NONE
        REAL,         SAVE, DIMENSION(:,:,:), POINTER :: DH,QX,QY,QZ,QSTO
        INTEGER :: ISS = 1
      CONTAINS
        SUBROUTINE MEMDEALLOCATE_DUP()
          IF(ASSOCIATED(DH))   DEALLOCATE(DH)
          IF(ASSOCIATED(QX))   DEALLOCATE(QX)
          IF(ASSOCIATED(QY))   DEALLOCATE(QY)
          IF(ASSOCIATED(QZ))   DEALLOCATE(QZ)
          IF(ASSOCIATED(QSTO)) DEALLOCATE(QSTO)
        END SUBROUTINE
      END MODULE