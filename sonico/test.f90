PROGRAM TEST
    USE MATH
    IMPLICIT NONE
    REAL, DIMENSION(3) :: A, B
    A = (/ 1.0, 0.0, 0.0 /)
    B = (/ 0.0, 0.0, 0.0 /)
    !calculate 3 rotations
    CALL VECTOR_ROTATION(A,B,0.0,0.0,90.0,.TRUE.)
    CALL VECTOR_ROTATION(A,B,0.0,90.0,0.0,.TRUE.)
    CALL VECTOR_ROTATION(A,B,90.0,0.0,0.0,.TRUE.)
    
END PROGRAM