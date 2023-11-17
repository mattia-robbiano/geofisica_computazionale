MODULE MATH
CONTAINS
   !This subroutine convert a sexagesimal angle to radians
   SUBROUTINE SEXA2RAD(AngleDeg,Radians)
      IMPLICIT NONE
      REAL, INTENT(IN) :: AngleDeg
      REAL, INTENT(OUT) :: Radians
      REAL, PARAMETER :: PI = 3.14159265358979323846
      Radians = (REAL(INT(AngleDeg))*PI/180)+(AngleDeg-REAL(INT(AngleDeg)))
   END SUBROUTINE SEXA2RAD

   !This subroutine rotates a vector by the angles Phi, Theta, and Psi, where the angles are in sexagesimal degrees.
   SUBROUTINE VECTOR_ROTATION(x,y,z,xRot,yRot,zRot,PhiDeg,ThetaDeg,PsiDeg,VERBOSE)
      IMPLICIT NONE
      REAL, INTENT(IN) :: PhiDeg,ThetaDeg,PsiDeg, x,y,z
      LOGICAL, OPTIONAL, INTENT(IN) :: VERBOSE
      REAL, INTENT(OUT) :: xRot,yRot,zRot
      REAL :: Phi,Theta,Psi
      REAL, DIMENSION(3) :: Input,Output
      REAL, DIMENSION(3,3) :: RotationMatrix
      !Convert the input vector to a matrix
      Input(1) = x
      Input(2) = y
      Input(3) = z
      !Convert from sexagesimal to radians
      CALL SEXA2RAD(PhiDeg,Phi)
      CALL SEXA2RAD(ThetaDeg,Theta)
      CALL SEXA2RAD(PsiDeg,Psi)
      !Calculate the rotation matrix
      RotationMatrix(1,1) = COS(Psi)*COS(Phi)-COS(Theta)*SIN(Phi)*SIN(Psi)
      RotationMatrix(1,2) = -SIN(Psi)*COS(Phi)-COS(Theta)*SIN(Phi)*COS(Psi)
      RotationMatrix(1,3) = SIN(Theta)*SIN(Phi)
      RotationMatrix(2,1) = COS(Psi)*SIN(Phi)+COS(Theta)*COS(Phi)*SIN(Psi)
      RotationMatrix(2,2) = -SIN(Psi)*SIN(Phi)+COS(Theta)*COS(Phi)*COS(Psi)
      RotationMatrix(2,3) = -COS(Phi)*SIN(Theta)
      RotationMatrix(3,1) = SIN(Psi)*SIN(Theta)
      RotationMatrix(3,2) = COS(Psi)*SIN(Theta)
      RotationMatrix(3,3) = COS(Theta)
      !Rotate the vector
      Output = MATMUL(RotationMatrix,Input)
      !Return the rotated vector
      xRot = Output(1)
      yRot = Output(2)
      zRot = Output(3)

      !Return the rotated vector if VERBOSE
      IF(PRESENT(VERBOSE)) THEN
         !Given Rotation angles in right format:
         WRITE(*,*) 'Rotation Angles'
         WRITE(*,'(3F10.5)') PhiDeg,ThetaDeg,PsiDeg
         WRITE(*,*)
         !Print the rotation matrix in right format
         WRITE(*,*) 'Rotation Matrix'
         WRITE(*,'(3F10.5)') RotationMatrix(1,:)
         WRITE(*,'(3F10.5)') RotationMatrix(2,:)
         WRITE(*,'(3F10.5)') RotationMatrix(3,:)
         WRITE(*,*)
      END IF
   END SUBROUTINE VECTOR_ROTATION
END MODULE MATH
