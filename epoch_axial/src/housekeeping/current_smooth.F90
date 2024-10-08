! Copyright (C) 2009-2019 University of Warwick
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

MODULE current_smooth

#ifdef HIGH_ORDER_SMOOTHING
  USE shape_functions
#else
  USE constants
#endif
  USE boundary

!==============================================================================
! 2024/08/02 by katsura
!------------------------------------------------------------------------------
  USE shared_data
!==============================================================================

  IMPLICIT NONE

CONTAINS

  SUBROUTINE current_finish

    CALL current_bcs

    CALL field_mode_bc(jxm, jng)
    CALL field_mode_bc(jrm, jng)
    CALL field_mode_bc(jtm, jng)

!==============================================================================
! 2024/08/02 by katsura
!------------------------------------------------------------------------------
!    CALL clamp_external_current
!==============================================================================

    IF (smooth_currents) CALL smooth_current

    IF (use_current_correction) THEN
      jx = jx - initial_jx
      jy = jy - initial_jy
      jz = jz - initial_jz
    END IF

  END SUBROUTINE current_finish

!==============================================================================
! 2024/08/02 by katsura
!------------------------------------------------------------------------------
  SUBROUTINE clamp_external_current
    INTEGER :: ix, ir, im, ir_min, ir_max
    REAL(num) :: I_coil, x_coil, y_coil, r_coil_1, r_coil_2
    
    ir_min = 0
    IF (y_min_boundary) ir_min = 1
    ir_max = ny 
    IF (y_max_boundary) ir_max = ny-1

    I_coil =  12940.0_num
    x_coil = -6.0e-4_num
    y_coil =  6.0e-4_num
    
    DO im = 0, n_mode-1     ! Loop over field modes, m 
      DO ir = ir_min, ny    ! Loop over radial cells
        DO ix = 0, nx       ! Loop over x cells, along cylindrical axis
          
          IF (x(ix) > (x_coil - dx/2.0_num) .and. x(ix) < (x_coil + dx/2.0_num)) THEN
            IF (y(ir) > (y_coil - dy/2.0_num) .and. y(ir) < (y_coil + dy/2.0_num)) THEN
              jtm(ix, ir, im) = I_coil / dx / dy
              jtm_old(ix, ir, im) = I_coil / dx / dy
            END IF
          END IF
          
        END DO
      END DO
    END DO
  END SUBROUTINE clamp_external_current
!==============================================================================

  SUBROUTINE smooth_current

    ! Implements strided compensated binomial filtering

    CALL smooth_mode_array(jxm, smooth_its, smooth_comp_its, smooth_strides)
    CALL smooth_mode_array(jrm, smooth_its, smooth_comp_its, smooth_strides)
    CALL smooth_mode_array(jtm, smooth_its, smooth_comp_its, smooth_strides)

  END SUBROUTINE smooth_current



  SUBROUTINE smooth_array(array, its, comp_its, stride)

    REAL(num), DIMENSION(1-jng:,1-jng:), INTENT(INOUT) :: array
    INTEGER, INTENT(IN) :: its
    INTEGER, INTENT(IN) :: comp_its
    INTEGER, INTENT(IN), DIMENSION(:), ALLOCATABLE :: stride
    REAL(num), DIMENSION(:,:), ALLOCATABLE :: wk_array
    INTEGER :: ix, iy
#ifdef HIGH_ORDER_SMOOTHING
    INTEGER :: isubx, isuby
    REAL(num), DIMENSION(sf_min:sf_max) :: weight_fn
    REAL(num) :: val, w1, w2
#else
    INTEGER, DIMENSION(:), ALLOCATABLE :: stride_inner
    INTEGER :: ng_l, it, istride, cstride
    REAL(num) :: alpha, beta
#endif

#ifdef HIGH_ORDER_SMOOTHING
    CALL particle_to_grid(0.0_num, weight_fn)

    ALLOCATE(wk_array(nx,ny))

    DO iy = 1, ny
    DO ix = 1, nx
      val = 0.0_num
      DO isuby = sf_min, sf_max
        w2 = weight_fn(isuby)
        DO isubx = sf_min, sf_max
          w1 = w2 * weight_fn(isubx)
          val = val + array(ix+isubx,iy+isuby) * w1
        END DO
      END DO
      wk_array(ix,iy) = val
    END DO
    END DO

    array(1:nx,1:ny) = wk_array(:,:)

    DEALLOCATE(wk_array)
#else
    IF (ALLOCATED(stride)) THEN
      ALLOCATE(stride_inner(SIZE(stride)), SOURCE=stride)
    ELSE
      ALLOCATE(stride_inner(1), SOURCE=[1])
    END IF

    ng_l = MAX(sng, jng)
    alpha = 0.5_num
    beta = (1.0_num - alpha) * 0.25_num

    ALLOCATE(wk_array(1-ng_l:nx+ng_l,1-ng_l:ny+ng_l))

    wk_array = 0.0_num
    wk_array(1-jng:nx+jng,1-jng:ny+jng) = array(1-jng:nx+jng,1-jng:ny+jng)

    DO it = 1, its + comp_its
      DO istride = 1, SIZE(stride_inner)
        CALL field_bc(wk_array, ng_l)
        cstride = stride_inner(istride)
        DO iy = 1, ny
        DO ix = 1, nx
          array(ix,iy) = alpha * wk_array(ix,iy) &
              + (wk_array(ix-cstride,iy) + wk_array(ix+cstride,iy) &
              +  wk_array(ix,iy-cstride) + wk_array(ix,iy+cstride)) * beta
        END DO
        END DO
        wk_array(1:nx,1:ny) = array(1:nx,1:ny)
      END DO
      IF (it > its) THEN
        alpha = REAL(its, num) * 0.5_num + 1.0_num
      END IF
    END DO

    array(1:nx,1:ny) = wk_array(1:nx,1:ny)

    DEALLOCATE(wk_array)
    DEALLOCATE(stride_inner)
#endif

  END SUBROUTINE smooth_array



  SUBROUTINE smooth_mode_array(array, its, comp_its, stride)

    COMPLEX(num), DIMENSION(1-jng:,1-jng:,0:), INTENT(INOUT) :: array
    INTEGER, INTENT(IN) :: its
    INTEGER, INTENT(IN) :: comp_its
    INTEGER, INTENT(IN), DIMENSION(:), ALLOCATABLE :: stride
    COMPLEX(num), DIMENSION(:,:,:), ALLOCATABLE :: wk_array
    INTEGER :: ix, iy, im
    INTEGER, DIMENSION(:), ALLOCATABLE :: stride_inner
    INTEGER :: ng_l, it, istride, cstride
    REAL(num) :: alpha, beta

    IF (ALLOCATED(stride)) THEN
      ALLOCATE(stride_inner(SIZE(stride)), SOURCE=stride)
    ELSE
      ALLOCATE(stride_inner(1), SOURCE=[1])
    END IF

    ng_l = MAX(sng, jng)
    alpha = 0.5_num
    beta = (1.0_num - alpha) * 0.25_num

    ALLOCATE(wk_array(1-ng_l:nx+ng_l,1-ng_l:ny+ng_l,0:n_mode-1))

    wk_array = 0.0_num
    wk_array(1-jng:nx+jng,1-jng:ny+jng,:) = array(1-jng:nx+jng,1-jng:ny+jng,:)

    DO it = 1, its + comp_its
      DO istride = 1, SIZE(stride_inner)
        CALL field_mode_bc(wk_array, ng_l)
        cstride = stride_inner(istride)
        DO im = 0, n_mode-1
        DO iy = 1, ny
        DO ix = 1, nx
          array(ix,iy,im) = alpha * wk_array(ix,iy,im) &
              + (wk_array(ix-cstride,iy,im) + wk_array(ix+cstride,iy,im) &
              +  wk_array(ix,iy-cstride,im) + wk_array(ix,iy+cstride,im)) * beta
        END DO
        END DO
        END DO
        wk_array(1:nx,1:ny,:) = array(1:nx,1:ny,:)
      END DO
      IF (it > its) THEN
        alpha = REAL(its, num) * 0.5_num + 1.0_num
      END IF
    END DO

    array(1:nx,1:ny,:) = wk_array(1:nx,1:ny,:)

    DEALLOCATE(wk_array)

  END SUBROUTINE smooth_mode_array

END MODULE current_smooth
