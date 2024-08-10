!==============================================================================
! 2024/08/05 by katsura
!------------------------------------------------------------------------------
MODULE output_user_define
  USE fields
  USE shared_data

  IMPLICIT NONE

CONTAINS

  SUBROUTINE output_fields_user_define(timestep, timing)

    INTEGER, INTENT(IN) :: timestep, timing
    INTEGER :: ix, ir, im, ir_min, ir_max
    CHARACTER(LEN=30) :: filename
    CHARACTER(LEN=10) :: timestep_str, timing_str, rank_str
    COMPLEX(num) :: output_data
    COMPLEX(num), ALLOCATABLE, DIMENSION(:,:) :: output_data_x
    COMPLEX(num), ALLOCATABLE, DIMENSION(:,:) :: output_data_r
    COMPLEX(num), ALLOCATABLE, DIMENSION(:,:) :: output_data_t

    WRITE(timestep_str, '(I5.5)') timestep
    WRITE(timing_str,   '(I3.3)') timing
    WRITE(rank_str,     '(I3.3)') rank

    ir_min = 0
    IF (y_min_boundary) ir_min = 1
    ir_max = ny
    IF (y_max_boundary) ir_max = ny-1

    IF (rank == 0) PRINT *, timestep_str

    ! output E field
    output_data_x = REAL(exm(:,:,0) + exm(:,:,1))
    output_data_r = REAL(erm(:,:,0) + erm(:,:,1))
    output_data_t = REAL(etm(:,:,0) + etm(:,:,1))
    IF (rank == 0) PRINT *, 'output E field'
    filename = './E_DATA/E' // TRIM(timestep_str) // '_' // TRIM(timing_str) // &
        '_' // TRIM(rank_str) // '.txt'
    OPEN(UNIT=10, FILE=filename, STATUS='REPLACE', ACTION='WRITE', &
        FORM='FORMATTED')
    WRITE(10, '(A, I10)') 'Timestep: ', timestep
    WRITE(10, '(I5, 1X, I5)') nx+1, ny-ir_min+1
    WRITE(10, '(A)') 'ix, ir, x, r, Ex, Er, Et, E'
    DO ir = ir_min, ny    ! Loop over radial cells
      DO ix = 0, nx       ! Loop over x cells, along cylindrical axis
        output_data = SQRT(output_data_x(ix,ir) * output_data_x(ix,ir) &
                         + output_data_r(ix,ir) * output_data_r(ix,ir) &
                         + output_data_t(ix,ir) * output_data_t(ix,ir))
        WRITE(10, '(I5,     1X, I5,     1X)', ADVANCE='NO') INT(ix), INT(ir)
        WRITE(10, '(F15.10, 1X, F15.10, 1X)', ADVANCE='NO') x(ix), y(ir)
        WRITE(10, '(F15.5,  1X)', ADVANCE='NO') REAL(output_data_x(ix,ir))
        WRITE(10, '(F15.5,  1X)', ADVANCE='NO') REAL(output_data_r(ix,ir))
        WRITE(10, '(F15.5,  1X)', ADVANCE='NO') REAL(output_data_t(ix,ir))
        WRITE(10, '(F15.5,  1X)', ADVANCE='NO') REAL(output_data)
        WRITE(10, *)         
      END DO
    END DO
    CLOSE(10)

    ! output B field
    output_data_x = 0.0_num
    output_data_r = 0.0_num
    output_data_t = 0.0_num
    output_data   = 0.0_num
    output_data_x = REAL(bxm(:,:,0) + bxm(:,:,1))
    output_data_r = REAL(brm(:,:,0) + brm(:,:,1))
    output_data_t = REAL(btm(:,:,0) + btm(:,:,1))
    IF (rank == 0) PRINT *, 'output B field'
    filename = './B_DATA/B' // TRIM(timestep_str) // '_' // TRIM(timing_str) // &
        '_' // TRIM(rank_str) // '.txt'
    OPEN(UNIT=20, FILE=filename, STATUS='REPLACE', ACTION='WRITE', &
        FORM='FORMATTED')
    WRITE(20, '(A, I10)') 'Timestep: ', timestep
    WRITE(20, '(I5.5, 1X, I5.5)') nx+1, ir_max+1
    WRITE(20, '(A)') 'ix, ir, x, r, Bx, Br, Bt, B'
    DO ir = 0, ir_max    ! Loop over radial cells
      DO ix = 0, nx       ! Loop over x cells, along cylindrical axis
        output_data = SQRT(output_data_x(ix,ir) * output_data_x(ix,ir) &
                         + output_data_r(ix,ir) * output_data_r(ix,ir) &
                         + output_data_t(ix,ir) * output_data_t(ix,ir))
        WRITE(20, '(I5,     1X, I5,     1X)', ADVANCE='NO') INT(ix), INT(ir)
        WRITE(20, '(F15.10, 1X, F15.10, 1X)', ADVANCE='NO') x(ix), y(ir)
        WRITE(20, '(F15.5,  1X)', ADVANCE='NO') REAL(output_data_x(ix,ir))
        WRITE(20, '(F15.5,  1X)', ADVANCE='NO') REAL(output_data_r(ix,ir))
        WRITE(20, '(F15.5,  1X)', ADVANCE='NO') REAL(output_data_t(ix,ir))
        WRITE(20, '(F15.5,  1X)', ADVANCE='NO') REAL(output_data)
        WRITE(20, *)
      END DO
    END DO
    CLOSE(20)

  END SUBROUTINE output_fields_user_define

END MODULE output_user_define
!==============================================================================