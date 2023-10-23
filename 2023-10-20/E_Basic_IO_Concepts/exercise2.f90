PROGRAM exercise2
!Programma che calcola la distanza dal centro dell'orbita 
!r= p /(1-epsilon*cos(teta))
!con p costante pari a 1200 km
! epsilon= 0, 0.25, 0.5
! teta da 0° a 360° (step 1°)

! Scrive su un file di uscita controllando se è già presente   

!Dichiarazione delle variabili
IMPLICIT NONE
INTEGER :: i, j, ios 
LOGICAL :: lexist
REAL, PARAMETER :: p= 1200000, PI=3.1415
REAL :: r, epsilon, teta
CHARACTER (len=40) :: etichetta,nomefile
INTEGER :: nrisposta

!Inizializzazione delle variabili
epsilon=0.

!Lettura da tastiera del nome del file di uscita
WRITE(*,*) 'Scrivi l etichetta per il file di uscita (max 40 caratteri)'
READ (*,*) etichetta
nomefile=TRIM(etichetta)//'.dat'

loop_apertura: DO ! Loop infinito esce solo quando apre il file o stop
  
!Apertura del file di uscita
  INQUIRE(FILE=nomefile,EXIST=lexist)

  esistenza: IF (.NOT.lexist)THEN  !il file non esiste, lo apro

    OPEN(UNIT=55,FILE=nomefile,STATUS='NEW',IOSTAT=ios)
!    WRITE(*,*) 'ifile = ',ifile
    IF (ios /= 0) THEN
      ERROR STOP 'Errore di apertura file'
    END IF
    EXIT loop_apertura !esco
   
  ELSE ! ilfile esiste -> diverse opzioni
    
    WRITE(*,*)'ATTENZIONE file gia presente'
    WRITE(*,*)'Vuoi riscrivere sopra? (1) SI, (2) NO voglio interrompere (3) NO voglio inserire un nuovo nome file'
    READ(*,*) nrisposta
    
    SELECT CASE (nrisposta)
     CASE (1) !riscrivo
       OPEN (UNIT=55, FILE=nomefile,STATUS='REPLACE', IOSTAT=ios)
       IF (ios /= 0) THEN
         ERROR STOP 'Errore di apertura file'
       END IF
       EXIT loop_apertura 
     CASE (2) !interrompo il programma
       STOP 'programma interrotto'
     CASE (3) ! richiedo un nuovo nome e torno a inizio DO
        WRITE(*,*) 'Scrivi il nome della etichetta del nuovo file di uscita (max 40 caratteri)'
        READ (*,*) etichetta
        nomefile=TRIM(etichetta)//'.dat'
       
     CASE DEFAULT !Torno a inizio DO
       WRITE (*,*)'Puoi rispondere solo 1 , 2, o 3'
    END SELECT
    
  END IF esistenza

END DO loop_apertura

!Calcolo
 loop1: DO i=1,3   ! loop su epsilon
    WRITE(55,'(A10,1X,F4.2)')  'epsilon : ', epsilon
    WRITE(55,'(A)')'teta(°)  raggio(km)'
    loop2: DO j = 1,360   ! loop su teta
         teta= (j*PI)/180.
         r= p/(1-epsilon*COS(TETA))
         WRITE (55,'(I3,7X,F7.2)')  INT(teta*180/PI), r/1000.
     END DO loop2
     epsilon=epsilon+0.25
     WRITE(55,'(A)')'-----------------------------------------'
END DO loop1

!Fine
STOP
END PROGRAM exercise2
