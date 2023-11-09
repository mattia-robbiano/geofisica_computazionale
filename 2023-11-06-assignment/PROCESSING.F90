MODULE PROCESSING
   CONTAINS

   SUBROUTINE Padding(MatriceInput,NumeroRigheInput,NumeroColonneInput,MatriceOutput,PaddingY,PaddingX)
      IMPLICIT NONE
  
      INTEGER, INTENT(IN) :: NumeroRigheInput, NumeroColonneInput, PaddingY,PaddingX
      REAL(8), INTENT(IN), DIMENSION(NumeroRigheInput,NumeroColonneInput) :: MatriceInput
      REAL(8), INTENT(OUT), DIMENSION(NumeroRigheInput+2*PaddingY,NumeroColonneInput+2*PaddingX) :: MatriceOutput
      INTEGER :: Colonna, Riga
        
      !INIZIALIZZAZIONE
      Colonna=0
      Riga=0
  
      !PADDING (orlo la matrice con zeri di tante righe/colonne quanto specificato)
      MatriceOutput = 0.
      DO Riga=1,NumeroRigheInput
         DO Colonna=1,NumeroColonneInput
            MatriceOutput(Riga+PaddingY,Colonna+PaddingX) = MatriceInput(Riga,Colonna)
         END DO
      END DO
  
   END SUBROUTINE Padding
  
   SUBROUTINE CrossCorrelation2D(MatriceInput, MatriceOutput, Filtro, StrideY, StrideX)
      IMPLICIT NONE
  
      REAL(8), INTENT(IN), DIMENSION(:,:) :: MatriceInput
      REAL(8), INTENT(OUT), DIMENSION(:,:) :: MatriceOutput
      REAL(8), INTENT(IN), DIMENSION(:,:) :: Filtro
      INTEGER, INTENT(IN) :: StrideY, StrideX            
  
      INTEGER :: RigheInput, ColonneInput, RigheFiltro, ColonneFiltro, RigheOutput, ColonneOutput
      INTEGER :: i, j, k, l
  
      !INIZIALIZZAZIONE
      i = 0
      j = 0
      k = 0
      l = 0
      RigheInput = SIZE(MatriceInput, 1)
      ColonneInput = SIZE(MatriceInput, 2)
      RigheFiltro = SIZE(Filtro, 1)
      ColonneFiltro = SIZE(Filtro, 2)
      RigheOutput = 1 + (RigheInput  - RigheFiltro) / StrideY
      ColonneOutput = 1 + (ColonneInput - ColonneFiltro) / StrideX
  
      ! CROSS CORRELATION 2D (vedi pseudocodice)
      DO i = 1, RigheOutput
         DO j = 1, ColonneOutput
  
            DO k = 1, RigheFiltro
               DO l = 1, ColonneFiltro
                  MatriceOutput(i, j) = MatriceOutput(i, j) &
                  + Filtro(k, l) * MatriceInput(k + (i - 1) * StrideY, l + (j - 1) * StrideX)
               END DO
            END DO
  
         END DO
      END DO
  
   END SUBROUTINE CrossCorrelation2D

END MODULE PROCESSING
