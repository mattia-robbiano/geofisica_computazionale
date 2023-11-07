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
         WRITE(*, '(A)', ADVANCE='NO') 'ERROR: FILE NOT FOUND - ', FILE_NAME
         ERROR = .TRUE.
      ELSE
         ! File exists, open it
         OPEN(UNIT=FILE_UNIT, FILE=FILE_NAME, STATUS='OLD', ACTION='READ', IOSTAT=IO)
         IF (IO /= 0) THEN
            WRITE(*, '(A)', ADVANCE='NO') 'ERROR OPENING FILE: ', FILE_NAME
            ERROR = .TRUE.
         ELSE
            WRITE(*, '(A)', ADVANCE='NO') 'FILE OPENED SUCCESSFULLY: ', FILE_NAME
            ERROR = .FALSE.
         END IF
      END IF
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
        WRITE(*, '(A)', ADVANCE='NO') 'File already exists: ', FILE_NAME
        WRITE(*, '(A)', ADVANCE='NO') 'Do you want to replace it? (Y/N)'
        READ(*, '(A)') response
    
        IF (response == 'Y' .OR. response == 'y') THEN
          OPEN(UNIT=FILE_UNIT, FILE=FILE_NAME, STATUS='REPLACE', ACTION='WRITE', IOSTAT=IO)
          IF (IO /= 0) THEN
            WRITE(*, '(A)', ADVANCE='NO') 'ERROR OPENING FILE: ', FILE_NAME
            ERROR = .TRUE.
          ELSE
            WRITE(*, '(A)', ADVANCE='NO') 'FILE REPLACED AND OPENED SUCCESSFULLY: ', FILE_NAME
            ERROR = .FALSE.
          END IF
        ELSE
          WRITE(*, '(A)', ADVANCE='NO') 'User chose not to replace the file: ', FILE_NAME
          ERROR = .TRUE.
        END IF
      ELSE
        ! File does not exist; open it for writing
        OPEN(UNIT=FILE_UNIT, FILE=FILE_NAME, STATUS='UNKNOWN', ACTION='WRITE', IOSTAT=IO)
        IF (IO /= 0) THEN
          WRITE(*, '(A)', ADVANCE='NO') 'ERROR OPENING FILE: ', FILE_NAME
          ERROR = .TRUE.
        ELSE
          WRITE(*, '(A)', ADVANCE='NO') 'FILE CREATED AND OPENED SUCCESSFULLY: ', FILE_NAME
          ERROR = .FALSE.
        END IF
      END IF
    END SUBROUTINE OPEN_OUTPUT_FILE    

END MODULE IOSTREAM

