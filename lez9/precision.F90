 PROGRAM prova_kind
!Programma che determina i numeri di KIND dei valori
!reali in singola e doppia precisione in un particolare computer
IMPLICIT NONE
INTEGER :: n1,n2,n3,n4,n5
INTEGER, PARAMETER :: SGL= SELECTED_REAL_KIND(p=6,r=37)
INTEGER, PARAMETER :: DBL= SELECTED_REAL_KIND(p=13,r=200)
INTEGER, PARAMETER :: TRL= SELECTED_REAL_KIND(p=17)

REAL(kind=SGL) :: var1=0.
REAL(kind=DBL) :: var2=0._DBL
REAL(kind=TRL) :: var3=0._TRL

!Visualizza i numeri di KIND in singola e doppia precisione
WRITE (*,'("Il numero di KIND in singola precisione : ",I2)') KIND(0.0)
WRITE (*,*) 'Il numero di KIND in doppia precisione : ', KIND(0.0D0)

n1= SELECTED_REAL_KIND(p=6, r=37)
n2= SELECTED_REAL_KIND(p=12)
n3= SELECTED_REAL_KIND(r=100)
n4= SELECTED_REAL_KIND(p=13, r=200)
n5= SELECTED_REAL_KIND(p=17)
WRITE(*,*) 'n1 = ',n1,'n2 = ',n2,'n3 = ',n3,'n4 = ',n4, 'n5 = ',n5
WRITE (*,*) 'var1 ', 'kind ',KIND(var1),'precision ', PRECISION (var1), 'range ', RANGE(var1)
WRITE (*,*) 'var2 ', 'kind ',KIND(var2),'precision ', PRECISION (var2), 'range ', RANGE(var2)
WRITE (*,*) 'var3 ', 'kind ',KIND(var3),'precision ', PRECISION (var3), 'range ', RANGE(var3)


END PROGRAM prova_kind
