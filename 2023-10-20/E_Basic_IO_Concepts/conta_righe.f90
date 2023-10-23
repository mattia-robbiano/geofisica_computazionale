PROGRAM CONTA_RIGHE
!Programma che chiede a tastiera il nome di un file, controlla se esiste,
!lo aprre e conta il numero di righe, stampa a video il numero di righe

!Dichiarazione delle variabili
IMPLICIT NONE
INTEGER :: nvals, nu, ierr, ierr2
CHARACTER(len=20) :: fileinput
LOGICAL :: l_var

!Dati iniziali
nvals=0
WRITE(*,*) 'Scrivi il nome del file'
READ(*,*) fileinput

!Controllo presenza file
INQUIRE(FILE= fileinput,EXIST=l_var)
WRITE (*,*) 'lvar = ', l_var

IF(l_var) THEN
 !Il file esiste
 !Apro il file di ingresso
 OPEN(NEWUNIT=nu, FILE=fileinput, STATUS='OLD', IOSTAT= ierr)
  IF(ierr/=0)THEN
    WRITE (*,*) 'Attenzione errore di apertura file IOSTAT: ', ierr
    STOP
  END IF
  WRITE (*,*) 'il numero del file vale', nu
  
  !Ciclo principale di lettura  
  ciclo : DO
  READ(nu, *, IOSTAT= ierr2)
  
  IF (ierr2==0) THEN   ! lettura della riga
    nvals=nvals+1
  ELSE IF (ierr2<0) THEN  !sono a fine file
    WRITE (*,*) 'Ho trovato la fine file'
    WRITE (*,*) 'il numero di righe vale ', nvals
    EXIT ciclo  ! uscita dal ciclo DO
  ELSE IF (ierr2>0) THEN  !errore nella lettura
    WRITE (*,*) 'Errore in fase di lettura'
    EXIT ciclo  !uscita dal ciclo DO  
  END IF
  END DO ciclo

  ELSE
 ! Il file non esiste 
 WRITE(*,*) 'Il file non esiste'
END IF

!Fine
END PROGRAM CONTA_RIGHE 
