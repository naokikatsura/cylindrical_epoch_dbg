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

MODULE ic_module

  USE shared_data
  USE helper

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: manual_load

CONTAINS

  SUBROUTINE manual_load
!==============================================================================
! 2024/07/31 by katsura
!------------------------------------------------------------------------------
!    INTEGER :: ix, ir, im, ir_min, ir_max
!    REAL(num) :: I_coil, x_coil, y_coil, r_coil_1, r_coil_2
!    
!    ir_min = 0
!    IF (y_min_boundary) ir_min = 1
!    ir_max = ny 
!    IF (y_max_boundary) ir_max = ny-1
!
!    I_coil =  12940
!    x_coil = -6.0e-4_num
!    y_coil =  6.0e-4_num
!    
!    DO im = 0, n_mode-1  ! Loop over field modes, m 
!      DO ir = ir_min, ny  ! Loop over radial cells
!        DO ix = 0, nx  ! Loop over x cells, along cylindrical axis
!          r_coil_1 = (x(ix) - x_coil) * (x(ix) - x_coil) &
!                   + (y(ir) - y_coil) * (y(ir) - y_coil)
!          r_coil_2 = (x(ix) - x_coil) * (x(ix) - x_coil) &
!                   + (y(ir) + y_coil) * (y(ir) + y_coil)
!          IF (im == 0) THEN
!            bxm(ix, ir, im) = mu0 * I_coil / (2.0_num * pi) &
!                * (((-(y(ir)-y_coil)) / r_coil_1) &
!                - ((-(y(ir)+y_coil)) / r_coil_2))
!            brm(ix, ir, im) = mu0 * I_coil / (2.0_num * pi) &
!                * ((( (x(ix)-x_coil)) / r_coil_1) &
!                - (( (x(ix)-x_coil)) / r_coil_2))
!            !btm(ix, ir, im) = 0.0_num
!          END IF
!          !IF (im == 1) THEN
!            !bxm(ix, ir, im) = 0.0_num
!            !brm(ix, ir, im) = 0.0_num
!            !btm(ix, ir, im) = 0.0_num
!          !END IF  
!          !bxm_initial(ix, ir, im) = bxm(ix, ir, im)
!          !brm_initial(ix, ir, im) = brm(ix, ir, im)
!          !btm_initial(ix, ir, im) = btm(ix, ir, im)
!        END DO
!      END DO
!    END DO
!==============================================================================
  END SUBROUTINE manual_load

END MODULE ic_module
