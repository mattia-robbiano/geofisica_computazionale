MODULE IOSTREAM
CONTAINS

   !-----------------------------------------------------------------------
   !SUBROUTINE HANDLING THE INPUT FILE. THE FLAG ERR IS SET TO 1 IF THERE IS AN ERROR IN OPENING THE FILE
   SUBROUTINE InputFile(ProvidedFileName, Err)

      CHARACTER(LEN=*), INTENT(IN) :: ProvidedFileName
      INTEGER, INTENT(OUT) :: Err
      CHARACTER(LEN=100) :: FILENAME
      INTEGER :: IO
      LOGICAL :: lexist

      Err=0

      !PROVIDE THE FILE NAME
      IF(ProvidedFileName=='') THEN
         PRINT *,'Provide the name of the input file:'
         READ *, FILENAME
      ELSE
         FILENAME=ProvidedFileName
      END IF

      !CHECK IF THE FILE EXISTS
      INQUIRE(FILE=FILENAME,EXIST=lexist)

      !HANDLE CASES
      IF(.NOT.lexist)THEN
         PRINT *,'FILE DOES NOT EXIST, ENDING PROGRAM'
         Err =1
      ELSE
         OPEN(UNIT=20,FILE='FILENAME',STATUS='OLD',ACTION='READ',IOSTAT=IO)
         IF(IO/=0) THEN
            PRINT *,'ERROR OPENING INPUT FILE'
            Err = 1
         END IF
      END IF

   END SUBROUTINE InputFile

   !-----------------------------------------------------------------------
   !SUBROUTING HANDLING THE OUTPUT FILE. THE FLAG ERR IS SET TO 1 IF THERE IS AN ERROR IN OPENING THE FILE

   SUBROUTINE OutputFile(ProvidedFileName,Err)
      CHARACTER(LEN=*), INTENT(IN) :: ProvidedFileName
      INTEGER, INTENT(OUT) :: Err
      CHARACTER(LEN=100) :: FILENAME
      CHARACTER(LEN=1) :: Trash
      INTEGER :: IO
      LOGICAL :: lexist

      Err = 0

      !PROVIDE THE FILE NAME
      IF(ProvidedFileName=='') THEN
         PRINT *,'Provide the name of the output file:'
         READ *, FILENAME
      ELSE
         FILENAME=ProvidedFileName
      END IF
      
      !CHECK IF THE FILE EXISTS
      INQUIRE(FILE=FILENAME,EXIST=lexist)

      !HANDLE CASES
      Existence : IF(.NOT.lexist)THEN
         OPEN(UNIT=21,FILE=FILENAME,STATUS='new',IOSTAT=IO)
         IF (IO/=0) THEN
            WRITE(*,*)'ERROR OPENING OUTPUT FILE'
            Err = 1
         END IF
      ELSE
         WRITE(*,*)'PROVIDED FILE ALREADY EXISTS, DO YOU WANT TO REPLACE IT? (y/n)'
         READ(*,*)Trash

         Replacing : IF(Trash=='y' .OR. Trash=='Y')THEN
            OPEN(UNIT=21,FILE=FILENAME,STATUS='replace',IOSTAT=IO)
            IF (IO/=0) THEN
               WRITE(*,*)'ERROR OPENING OUTPUT FILE'
               Err = 1
            END IF
         ELSE
            WRITE(*,*)'FILE NOT REPLACED, ENDING PROGRAM'
            Err = 1
         END IF Replacing

      END IF Existence

   END SUBROUTINE OutputFile

END MODULE IOSTREAM
