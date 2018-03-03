MODULE stringUtils_m
        USE parameters_m,ONLY:wp
   IMPLICIT NONE
   CONTAINS

   !> Convert a string to all UPPERCASE
   !> \param[in] string sTrInG iN aNy CaSe
   !> \return upper STRING IN UPPERCASE
   PURE FUNCTION Uppercase(string) RESULT(upper)
		!> intent variables
      CHARACTER(LEN=*), INTENT(IN) :: string

		!> local variables
      CHARACTER(LEN=LEN(string)) :: upper
      INTEGER :: j

      DO j = 1,len(string)
        IF (string(j:j) >= "a" .AND. string(j:j) <= "z") THEN
             upper(j:j) = ACHAR(IACHAR(string(j:j)) - 32)
        ELSE
             upper(j:j) = string(j:j)
        END IF
      END DO
   END FUNCTION Uppercase

   !> Convert a string to all uppercase
   !> \param[in] string sTrInG iN aNy CaSe
   !> \return lower string in lowercase
   PURE FUNCTION Lowercase(string) RESULT(lower)
		!> intent variables
      CHARACTER(LEN=*), INTENT(IN) :: string

		!> local variables
      CHARACTER(LEN=LEN(string)) :: lower
      INTEGER :: j

      DO j = 1,len(string)
        IF (string(j:j) >= "A" .AND. string(j:j) <= "Z") THEN
             lower(j:j) = ACHAR(IACHAR(string(j:j)) + 32)
        ELSE
             lower(j:j) = string(j:j)
        END IF
      END DO
   END FUNCTION Lowercase

   !> transform an integer to a string
   FUNCTION num2str(x,F,length)
      
      !> intent variables
      INTEGER, INTENT(IN) :: length
      CHARACTER(len=length) :: num2str
      CHARACTER(len=*), INTENT(IN)      :: F ! format
      REAL(KIND=wp), INTENT(IN) :: x

      !> local variables
      CHARACTER(len=length) :: str

      ! convert a numeric value to a string using an internal write
      WRITE(str,F) x
      num2str = str
   END FUNCTION

END MODULE stringUtils_m
