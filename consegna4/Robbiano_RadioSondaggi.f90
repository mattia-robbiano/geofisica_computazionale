MODULE RadioSondaggi
    CONTAINS

    !Utilizzando al formula barometrica di Laplace dati z1, p2, p1, T2, T1 fornisce z2.
    SUBROUTINE GetAltitude(z2, z1, p2, p1, T2, T1)
       IMPLICIT NONE
       REAL, INTENT(IN) :: z1, p1, p2, T1, T2 !m, milliBar, °C
       REAL, INTENT(OUT) :: z2 !m
       REAL :: T
       REAL, PARAMETER :: g=9.81, R=287.0
 
       !CALCOLO TEMPERATURA MEDIA IN K
       T = ((T1 + T2) / 2) + 273.15
       !FORMULA BAROMETRICA
       z2 = z1+((R*T)/g)*LOG(p1/p2)

    END SUBROUTINE GetAltitude

    !Utilizzando al formula barometrica di Laplace dati z2, z1, p1, T2, T1 fornisce p2.
    SUBROUTINE GetPressure(p2, z2, z1, p1, T2, T1)
       IMPLICIT NONE
       REAL, INTENT(IN) :: z1, p1, z2, T1, T2 !m, milliBar, °C
       REAL, INTENT(OUT) :: p2 !milliBar
       REAL :: T
       REAL, PARAMETER :: g=9.81, R=287.0
 
       !CALCOLO TEMPERATURA MEDIA IN K
       T = ((T1 + T2) / 2) + 273.15
       !FORMULA BAROMETRICA
       p2 = p1*EXP(-g*(z2-z1)/(R*T))

    END SUBROUTINE GetPressure
 
 END MODULE RadioSondaggi
 