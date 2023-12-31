MODULE IOSTREAM
CONTAINS
   SUBROUTINE OPEN_INPUT_FILE(FILE_UNIT, FILE_NAME, ERROR)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FILE_UNIT
      CHARACTER(LEN=*), INTENT(IN) :: FILE_NAME
      LOGICAL, INTENT(OUT) :: ERROR
      INTEGER :: io
      LOGICAL :: iexist

      !CONTROLLO SE IL FILE ESISTE
      INQUIRE(FILE=FILE_NAME, EXIST=iexist)

      IF (.NOT. iexist) THEN
         !FILE NON ESISTENTE
         WRITE(*, 101) 'Error: file not found: ', FILE_NAME
         ERROR = .TRUE.
      ELSE
         !FILE ESISTENTE
         OPEN(UNIT=FILE_UNIT, FILE=FILE_NAME, STATUS='OLD', ACTION='READ', IOSTAT=IO)
         IF (IO /= 0) THEN
            WRITE(*,101) 'Error opening file ', FILE_NAME
            ERROR = .TRUE.
         ELSE
            WRITE(*,101) 'File opened successfully: ', FILE_NAME
            ERROR = .FALSE.
         END IF
      END IF
      101   FORMAT(A, 1X, A)
   END SUBROUTINE OPEN_INPUT_FILE

   SUBROUTINE OPEN_OUTPUT_FILE(FILE_UNIT, FILE_NAME, ERROR)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FILE_UNIT
      CHARACTER(LEN=*), INTENT(IN) :: FILE_NAME
      LOGICAL, INTENT(OUT) :: ERROR
      INTEGER :: io
      LOGICAL :: iexist
      CHARACTER(1) :: response

      !CONTROLLO SE IL FILE ESISTE
      INQUIRE(FILE=FILE_NAME, EXIST=iexist)

      IF (iexist) THEN
         !FILE ESISTENTE
         WRITE(*,101) 'File already exists: ', FILE_NAME
         WRITE(*,*) 'Do you want to replace it? (Y/N)'
         READ(*,*) response

         IF (response == 'Y' .OR. response == 'y') THEN
            OPEN(UNIT=FILE_UNIT, FILE=FILE_NAME, STATUS='REPLACE', ACTION='WRITE', IOSTAT=IO)
            IF (IO /= 0) THEN
               WRITE(*,*) 'Error opening file: ', FILE_NAME
               ERROR = .TRUE.
            ELSE
               WRITE(*,101) 'File replaced and opened sucessfully: ', FILE_NAME
               ERROR = .FALSE.
            END IF
         ELSE
            WRITE(*,101) 'User chose not to replace the file: ', FILE_NAME
            ERROR = .TRUE.
         END IF
      ELSE
         !FILE NON ESISTENTE
         OPEN(UNIT=FILE_UNIT, FILE=FILE_NAME, STATUS='NEW', ACTION='WRITE', IOSTAT=IO)
         IF (IO /= 0) THEN
            WRITE(*,101) 'Error opening file: ', FILE_NAME
            ERROR = .TRUE.
         ELSE
            WRITE(*,101) 'File created and opened sucessfully: ', FILE_NAME
            ERROR = .FALSE.
         END IF
      END IF
      101   FORMAT(A, 1X, A)
   END SUBROUTINE OPEN_OUTPUT_FILE

END MODULE IOSTREAM

