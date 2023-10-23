PROGRAM exercise2_pre2
!Programma che calcola la distanza dal centro dell'orbita 
!r= p /(1-epsilon*cos(teta))
!con p costante pari a 1200 km
! epsilon= 0, 0.25, 0.5
! teta da 0° a 360° (step 1°)

! Scrive a video in modo formattato   

!Dichiarazione delle variabili
IMPLICIT NONE
INTEGER :: i, j 
REAL, PARAMETER :: p= 1200000, PI=3.1415
REAL :: r, epsilon, teta


!Inizializzazione delle variabili
epsilon=0.


!Calcolo
 loop1: DO i=1,3   ! loop su epsilon
    WRITE(*,'(A10,1X,F4.2)')  'epsilon : ', epsilon
    WRITE(*,'(A)')'teta(°)  raggio(km)'
    loop2: DO j = 1,360   ! loop su teta
         teta= (j*PI)/180.
         r= p/(1-epsilon*COS(teta))
         WRITE (*,100)  j, r/1000.
     END DO loop2
     epsilon=epsilon+0.25
     WRITE(*,'(A)')'-----------------------------------------'
END DO loop1

!Fine
!STOP

!Formati
100 FORMAT (I3,7X,F7.2)

END PROGRAM exercise2_pre2
