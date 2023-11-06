MODULE SUB_AUST_4
CONTAINS

    SUBROUTINE FILTRA()
    END SUBROUTINE

    SUBROUTINE MEDIARE(MatriceTemperature,MatriceMedia, NumeroRighe, NumeroColonne)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: NumeroRighe, NumeroColonne
        REAL, DIMENSION(NumeroRighe,NumeroColonne), INTENT(IN) :: MatriceTemperature(NumeroRighe,NumeroColonne)
        REAL, INTENT(OUT) :: MatriceMedia(NumeroRighe,NumeroColonne)
        REAL, DIMENSION(NumeroRighe+1,NumeroColonne+1) :: MatriceTemperaturePadded
        REAL, DIMENSION(3,3) :: MatriceFiltro
        INTEGER :: Colonna=0, Riga=0

        !Definisco la matrice filtro
        MatriceFiltro = RESHAPE((/0.3,0.5,0.3,0.5,1.0,0.5,0.3,0.5,0.3/),(/3,3/))

        !Riempio MatriceTemperaturePadded con i valori della matrice di temperatura e con zeri
        DO Colonna=1,NumeroColonne
            DO Riga=1,NumeroRighe
                MatriceTemperaturePadded(Riga+1,Colonna+1) = MatriceTemperature(Riga,Colonna)
            END DO
        END DO
        MatriceTemperaturePadded(1,:) = 0
        MatriceTemperaturePadded(:,1) = 0
        MatriceTemperaturePadded(NumeroRighe+1,:) = 0
        MatriceTemperaturePadded(:,NumeroColonne+1) = 0
        
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
            END DO Righe
        END DO Colonne
    
   END SUBROUTINE
END MODULE SUB_AUST_4
