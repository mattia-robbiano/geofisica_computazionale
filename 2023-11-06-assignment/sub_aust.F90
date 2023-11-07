MODULE SUB_AUST_4
CONTAINS

   SUBROUTINE CONVOLUZIONE2D(MatriceInput, MatriceOutput, Filtro, StrideY, StrideX)
      IMPLICIT NONE

      REAL(8), INTENT(IN), DIMENSION(:,:) :: MatriceInput
      REAL(8), INTENT(OUT), DIMENSION(:,:) :: MatriceOutput
      REAL(8), INTENT(IN), DIMENSION(:,:) :: Filtro
      INTEGER, INTENT(IN) :: StrideY, StrideX            ! Quanto si sposta il filtro ad ogni passo su righe e colonne rispettivamente

      INTEGER :: RigheInput, ColonneInput, RigheFiltro, ColonneFiltro, RigheOutput, ColonneOutput
      INTEGER :: i, j, k, l

      !INIZIALIZZAZIONE
      i = 0
      j = 0
      k = 0
      l = 0

      !DIMENSIONAMENTO
      RigheInput = SIZE(MatriceInput, 1)
      ColonneInput = SIZE(MatriceInput, 2)
      RigheFiltro = SIZE(Filtro, 1)
      ColonneFiltro = SIZE(Filtro, 2)
      RigheOutput = 1 + (RigheInput  - RigheFiltro) / StrideY
      ColonneOutput = 1 + (ColonneInput - ColonneFiltro) / StrideX

      ! CONVOLUZIONE2D CON STRIDE NON UNITATIO. IL "-1" E' PERCHE' LA MATRICE E' INDICIZZATA DA 1
      DO i = 1, RigheOutput
         DO j = 1, ColonneOutput

            DO k = 1, RigheFiltro
               DO l = 1, ColonneFiltro
                  MatriceOutput(i, j) = MatriceOutput(i, j) &
                  + Filtro(k, l) * MatriceInput(i + (k - 1) * StrideY, j + (l - 1) * StrideX)
               END DO
            END DO

         END DO
      END DO

   END SUBROUTINE CONVOLUZIONE2D

   SUBROUTINE FILTRA(MatriceTemperature,MatriceFiltrata, NumeroRighe,NumeroColonne,NumeroRigheFiltrata,NumeroColonneFiltrata)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NumeroRighe, NumeroColonne, NumeroRigheFiltrata, NumeroColonneFiltrata
      REAL(KIND =8), DIMENSION(NumeroRighe,NumeroColonne), INTENT(IN) :: MatriceTemperature
      REAL(KIND =8), DIMENSION(NumeroRigheFiltrata,NumeroColonneFiltrata), INTENT(OUT) :: MatriceFiltrata
      REAL(KIND =8), DIMENSION(9,9) :: MatriceFiltro

      !Definisco la matrice filtro
      MatriceFiltro = 1

      !SCRIVO FILTRO A TERMINALE
      CALL CONVOLUZIONE2D(MatriceTemperature,MatriceFiltrata,MatriceFiltro,9,9)

      MatriceFiltrata = MatriceFiltrata/81

   END SUBROUTINE

   SUBROUTINE MEDIARE(MatriceTemperature,MatriceMedia, NumeroRighe, NumeroColonne)
      IMPLICIT NONE
      INTEGER :: Colonna, Riga
      INTEGER, INTENT(IN) :: NumeroRighe, NumeroColonne
      REAL(KIND =8), DIMENSION(NumeroRighe,NumeroColonne), INTENT(IN) :: MatriceTemperature(NumeroRighe,NumeroColonne)
      REAL(KIND =8), INTENT(OUT) :: MatriceMedia(NumeroRighe,NumeroColonne)
      REAL(KIND =8), DIMENSION(NumeroRighe+2,NumeroColonne+2) :: MatriceTemperaturePadded
      REAL(KIND =8), DIMENSION(3,3) :: MatriceFiltro

      Colonna=0
      Riga=0

      !Definisco la matrice filtro
      MatriceFiltro = RESHAPE((/0.3,0.5,0.3,0.5,1.0,0.5,0.3,0.5,0.3/),(/3,3/))

      !Riempio MatriceTemperaturePadded con i valori della matrice di temperatura e con zeri
      DO Colonna=1,NumeroColonne
         DO Riga=1,NumeroRighe
            MatriceTemperaturePadded(Riga+1,Colonna+1) = MatriceTemperature(Riga,Colonna)
         END DO
      END DO
      MatriceTemperaturePadded(1,:) = 0.
      MatriceTemperaturePadded(:,1) = 0.
      MatriceTemperaturePadded(NumeroRighe+2,:) = 0.
      MatriceTemperaturePadded(:,NumeroColonne+2) = 0.

      !Assegno ad ogni punto della matrice media il valore della media dei punti adiacenti
      Colonne: DO Colonna=1,NumeroColonne
         Righe: DO Riga=1,NumeroRighe
            MatriceMedia(Riga,Colonna) = MatriceFiltro(1,1)*MatriceTemperaturePadded(Riga,Colonna) + &
               MatriceFiltro(1,2)*MatriceTemperaturePadded(Riga,Colonna+1) + &
               MatriceFiltro(1,3)*MatriceTemperaturePadded(Riga,Colonna+2) + &
               MatriceFiltro(2,1)*MatriceTemperaturePadded(Riga+1,Colonna) + &
               MatriceFiltro(2,2)*MatriceTemperaturePadded(Riga+1,Colonna+1) + &
               MatriceFiltro(2,3)*MatriceTemperaturePadded(Riga+1,Colonna+2) + &
               MatriceFiltro(3,1)*MatriceTemperaturePadded(Riga+2,Colonna) + &
               MatriceFiltro(3,2)*MatriceTemperaturePadded(Riga+2,Colonna+1) + &
               MatriceFiltro(3,3)*MatriceTemperaturePadded(Riga+2,Colonna+2)
            MatriceMedia(Riga,Colonna) = MatriceMedia(Riga,Colonna)/9
         END DO Righe
      END DO Colonne

   END SUBROUTINE
END MODULE SUB_AUST_4
