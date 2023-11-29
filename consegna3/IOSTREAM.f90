MODULE IOSTREAM
CONTAINS
   SUBROUTINE OPEN_INPUT_FILE(FILE_UNIT, FILE_NAME, ERROR)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FILE_UNIT
      CHARACTER(LEN=*), INTENT(IN) :: FILE_NAME
      LOGICAL, INTENT(OUT) :: ERROR
      INTEGER :: io
      LOGICAL :: iexist

      ! Check if the file exists
      INQUIRE(FILE=FILE_NAME, EXIST=iexist)

      IF (.NOT. iexist) THEN
         ! File does not exist
         WRITE(*, 101) 'Error: file not found: ', FILE_NAME
         ERROR = .TRUE.
      ELSE
         ! File exists, open it
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

      ! Check if the file exists
      INQUIRE(FILE=FILE_NAME, EXIST=iexist)

      IF (iexist) THEN
         ! File exists; ask the user if they want to replace it
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
         ! File does not exist; open it for writing
         OPEN(UNIT=FILE_UNIT, FILE=FILE_NAME, STATUS='UNKNOWN', ACTION='WRITE', IOSTAT=IO)
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

   SUBROUTINE SKIP_LINE(FILE_UNIT,NumberOfLines, ERROR)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NumberOfLines
      INTEGER, INTENT(IN) :: FILE_UNIT
      LOGICAL, INTENT(OUT), OPTIONAL :: ERROR
      INTEGER :: i, IO

      ERROR = .FALSE.

      DO i = 1, NumberOfLines
         READ(FILE_UNIT,*,IOSTAT=IO)
         IF(IO>0)THEN
            WRITE(*,*) 'Error reading file'
            ERROR = .TRUE.
            EXIT
         ELSE IF(IO<0)THEN
            WRITE(*,*) 'End of file reached'
            ERROR = .TRUE.
            EXIT
         END IF
      END DO
   END SUBROUTINE SKIP_LINE

   SUBROUTINE COUNT_LINES(NumberOfLines, ERROR)
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: NumberOfLines
      LOGICAL, INTENT(OUT), OPTIONAL :: ERROR
      INTEGER :: IO
      
      ERROR = .FALSE.
      NumberOfLines = 0
      DO
         READ(20,*,IOSTAT=IO)
         IF (IO==0)THEN
            NumberOfLines=NumberOfLines+1
         ELSE IF(IO>0)THEN
            WRITE(*,*) 'Error reading file'
            ERROR = .TRUE.
            EXIT
         ELSE
            EXIT
         END IF
      END DO
   END SUBROUTINE COUNT_LINES

END MODULE IOSTREAM
