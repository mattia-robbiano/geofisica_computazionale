program tetto
   implicit none
   !Dichiarazione variabili
   integer :: ier
   integer, dimension(2) :: Giorno, Mese, Anno
   logical :: iexist
   character(len=1) :: OverWriteYesNo
   real(kind=8) :: Temperatura, Varianza, NumeroDatiGiorno=0
   real(kind=8),dimension(2) :: TemperaturaMedia

   !Apertura file ingresso
   open(unit=20, file='dati_tetto.txt', status='old', action='read',iostat=ier)
   if(ier/=0) stop 'Errore nell''apertura del file dati_tetto.txt'

   !Apertura file uscita
   inquire(file='dati_tetto_medie.txt',exist=iexist)
   if(iexist) then

      write(*,*) 'Il file dati_tetto_medie.txt esiste gia, vuoi sovrascriverlo? (y/n)'
      read(*,*) OverWriteYesNo

      if(OverWriteYesNo=='y' .or. OverWriteYesNo=='Y') then
         open(unit=21,file='dati_tetto_medie.txt',status='replace',action='write',iostat=ier)
         IF(ier/=0) stop 'Errore nell''apertura del file dati_tetto_medie.txt'

      else
         stop 'Nessun file modificato'
      end if
    else 
        open(unit=21,file='dati_tetto_medie.txt',status='new',action='write',iostat=ier)
        if(ier/=0) stop 'Errore nell''apertura del file dati_tetto_medie.txt'

   end if

   !Lettura primo dato
   read(20,*,iostat=ier) Giorno(1), Mese(1), Anno(1), Temperatura
   rewind(20)

   !Lettura dati
do 
    read(20,*,iostat=ier) Giorno(2), Mese(2), Anno(2), Temperatura
    if (ier/=0) exit
    NumeroDatiGiorno = NumeroDatiGiorno + 1

    if (Giorno(2)/=Giorno(1) .or. Mese(2)/=Mese(1) .or. Anno(2)/=Anno(1)) then
       !calcolo media e varianza
        TemperaturaMedia(1) = TemperaturaMedia(1) / NumeroDatiGiorno
        Varianza = sqrt((NumeroDatiGiorno*TemperaturaMedia(2) - TemperaturaMedia(1)**2)/(NumeroDatiGiorno*(NumeroDatiGiorno-1)))
        !Stampa su file
        write(21,*) Giorno(1), Mese(1), Anno(1), TemperaturaMedia(1), Varianza
        !Riinizializzazione variabili
        TemperaturaMedia(1) = 0
        TemperaturaMedia(2) = 0
        NumeroDatiGiorno = 0
        !Aggiornamento variabili
        Giorno(1) = Giorno(2)
        Mese(1) = Mese(2)
        Anno(1) = Anno(2)
    else
        TemperaturaMedia(1) = TemperaturaMedia(1) + Temperatura
        TemperaturaMedia(2) = TemperaturaMedia(2) + Temperatura**2
    end if 

end do 

end program tetto
