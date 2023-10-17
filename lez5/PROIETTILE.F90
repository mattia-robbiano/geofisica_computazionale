PROGRAM PROIETTILE
  IMPLICIT NONE
  
  !DECLARATIONS
  REAL :: alpha, t, u, x, y, vx, vy, v, theta, ymax
  REAL, PARAMETER :: g = 9.81
  REAL, PARAMETER :: PI = 3.14
  
  !EXECUTION
  
  !Input
  WRITE(*,*) 'Fornire alpha, t, u come reali'
  READ(*,*) alpha, t, u
  
  !Converto in radianti
  alpha = alpha * 2*PI/360
  
  !Calcoli
  x     = u*t*cos(alpha)
  y     = u*t*sin(alpha) - ((g*t**2)/2)
  vx    = u*cos(alpha)
  vy    = u*sin(alpha) - g*t
  v     = sqrt(vx**2+vy**2)
  theta = atan2(vy,vx)
  ymax  = (u**2*(sin(alpha))**2)/(2*g)
  
  !Conversione in gradi
  theta = theta * 360/(2*PI)
  
  !Output
  WRITE(*,*) 'x     = ',x
  WRITE(*,*) 'y     = ',y
  WRITE(*,*) 'vx    = ',vx
  WRITE(*,*) 'vy    = ',vy
  WRITE(*,*) 'v     = ',v
  WRITE(*,*) 'theta = ',theta
  WRITE(*,*) 'ymax  = ',ymax
  
  !Stop
  STOP
END PROGRAM PROIETTILE
