PROGRAM duesensori
! Programma che legge le temperature T1 e T2 e umidità relative RH1 e RH2
! da due file separati calcola le umidità specifiche q1 e q2
! e riscrive in un file finale data ora, T1, T2, RH1, RH2, q1 e q2

! Dichiarazione delle variabili
IMPLICIT NONE
REAL :: T1 , T2, RH1, RH2, q1, q2
REAL :: e1, e2, ew1, ew2
LOGICAL :: lexist
INTEGER :: i, ierr, ios, iout_unit
INTEGER :: num, num1,num2,codice1, codice2
CHARACTER (len=2) :: ora1, ora2, minuti1,minuti2
CHARACTER (len=10) :: data1,data2
CHARACTER ,PARAMETER :: pp=':'

!Apertura dei file di ingresso

OPEN(UNIT=20,FILE='10486917_4.txt',STATUS='OLD',ACTION='READ',IOSTAT=ierr)
IF (ierr/=0) THEN
  WRITE(*,*)'Attenzione file non aperto correttamente',ierr
  STOP
END IF

OPEN(UNIT=21,FILE='10512170_4_new.txt',STATUS='OLD',ACTION='READ',IOSTAT=ierr )
IF (ierr/=0) THEN
  WRITE(*,*)'Attenzione file non aperto correttamente',ierr
  STOP
END IF

!Apertura del file di uscita

INQUIRE(FILE='risultati2.dat',EXIST=lexist)
esistenza: IF (.NOT.lexist)THEN
   OPEN(NEWUNIT=iout_unit,FILE='risultati2.dat',STATUS='new',IOSTAT=ierr)
   IF (ierr/=0) THEN
     WRITE(*,*)'Attenzione file di uscita non aperto correttamente',ierr
     STOP
   END IF
ELSE
   WRITE(*,*)'ATTENZIONE file gia presente'
   STOP
END IF esistenza


!Lettura delle prime due righe
READ(20,'(12X,I8)') codice1
READ(20,*) !salta la riga

READ(21,'(12X,I8)') codice2
READ(21,*) !salta la riga

!Scrittura del titolo della tabella
WRITE(iout_unit,*)'Plot Title: ', codice1 ,',',codice2
WRITE(iout_unit,*) '#     Date  Time, GMT+01:00  T1(°C)  T2(°C)  RH1(%)  RH2(%)  q1(g/kg)  q2(g/kg)'

!Identificazione del numero di dati
num1=0
num2=0
DO
   READ(20,*,IOSTAT=ios)
   IF (ios==0)THEN
      num1=num1+1
   ELSE
      EXIT
   END IF
END DO   

DO
   READ(21,*,IOSTAT=ios)
   IF (ios==0)THEN
      num2=num2+1
   ELSE
      EXIT
   END IF
END DO
WRITE(*,*)num1,num2
!prendo come numero di dati il minimo tra i due file
num=MIN(num1,num2)

!Lettura dei dati

!riavvolgo i file
REWIND(20)
READ(20,*)
READ(20,*)

REWIND(21)
READ(21,*)
READ(21,*)

principale: DO i=1, num
   
            IF (i<=9)THEN
               READ(20,100) data1, ora1, minuti1,T1, RH1
               READ(21,100) data2, ora2, minuti2,T2, RH2
            ELSE IF (i<=99) THEN
              READ(20,101) data1, ora1, minuti1,T1, RH1
              READ(21,101) data2, ora2, minuti2,T2, RH2    
            ELSE IF (i<=999)THEN
              READ(20,102) data1, ora1, minuti1,T1, RH1
              READ(21,102) data2, ora2, minuti2,T2, RH2
            ELSE IF (i<=9999)THEN
              READ(20,103) data1, ora1, minuti1,T1, RH1
              READ(21,103) data2, ora2, minuti2,T2, RH2
            ELSE IF(i<=99999)THEN
              READ(20,104) data1, ora1, minuti1,T1, RH1
              READ(21,104) data2, ora2, minuti2,T2, RH2
           ELSE
              WRITE(*,*) 'Attenzione : formato di lettura non corretto'
              STOP
           END IF
         
           !Controllo sulla data, ora e minuti
           IF((data1/=data2).OR.(ora1/=ora2).OR.(minuti1/=minuti1)) THEN
              WRITE(*,*) 'Attenzione la data ora e minuti non corrispondono'
              WRITE(*,*) 'data1 = ', data1,'data2 = ', data2
              WRITE(*,*) 'ora1 = ', ora1,'ora2 = ', ora2
              WRITE(*,*) 'minuti1 = ', minuti1,'minuti2 = ', minuti2
              STOP              
           END IF   

          ! Calcolo dell'umidita specifica secondo la parametrizzazione di Bolton (1980)
           
            ew1=6.112*EXP((17.67*T1)/(T1+243.5))
            ew2=6.112*EXP((17.67*T2)/(T2+243.5))

            e1=0.01*RH1*ew1
            e2=0.01*RH2*ew2

            q1=0.622*(e1/(1013.25-e1))*1000
            q2=0.622*(e2/(1013.25-e2))*1000

            !Scrittura nel file di uscita
               WRITE(iout_unit,110) i,data1,ora1,pp,minuti1,T1,T2,RH1,RH2,q1,q2   
            
         END DO principale
         

!Formati 
100      FORMAT(2X,A10,1X,A2,1X,A2,11X,F6.3,8X,F6.3)
101      FORMAT(3X,A10,1X,A2,1X,A2,11X,F6.3,8X,F6.3)
102      FORMAT(4X,A10,1X,A2,1X,A2,11X,F6.3,8X,F6.3)
103      FORMAT(5X,A10,1X,A2,1X,A2,11X,F6.3,8X,F6.3)
104      FORMAT(6X,A10,1X,A2,1X,A2,11X,F6.3,8X,F6.3)
                           
110      FORMAT(I5,4X,A10,2X,A2,A1,A2,4(2X,F6.3),2(2X,F5.2))
         
!end
END PROGRAM duesensori
  
