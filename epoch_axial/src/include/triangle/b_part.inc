exp_min_imtheta = 1.0_num
bx_part = 0.0_num 
br_part = 0.0_num 
bt_part = 0.0_num

DO im = 0, n_mode-1
  bx_part = bx_part + REAL( exp_min_imtheta * (&
        gy(-1) * (hx(-1) * bxm(cell_x2-1,cell_y1-1,im) &
      +           hx( 0) * bxm(cell_x2  ,cell_y1-1,im) &
      +           hx( 1) * bxm(cell_x2+1,cell_y1-1,im)) &
      + gy( 0) * (hx(-1) * bxm(cell_x2-1,cell_y1  ,im) &
      +           hx( 0) * bxm(cell_x2  ,cell_y1  ,im) &
      +           hx( 1) * bxm(cell_x2+1,cell_y1  ,im)) &
      + gy( 1) * (hx(-1) * bxm(cell_x2-1,cell_y1+1,im) &
      +           hx( 0) * bxm(cell_x2  ,cell_y1+1,im) &
      +           hx( 1) * bxm(cell_x2+1,cell_y1+1,im))), num)

  br_part = br_part + REAL( exp_min_imtheta * (&
        hy(-1) * (gx(-1) * brm(cell_x1-1,cell_y2-1,im) &
      +           gx( 0) * brm(cell_x1  ,cell_y2-1,im) &
      +           gx( 1) * brm(cell_x1+1,cell_y2-1,im)) &
      + hy( 0) * (gx(-1) * brm(cell_x1-1,cell_y2  ,im) &
      +           gx( 0) * brm(cell_x1  ,cell_y2  ,im) &
      +           gx( 1) * brm(cell_x1+1,cell_y2  ,im)) &
      + hy( 1) * (gx(-1) * brm(cell_x1-1,cell_y2+1,im) &
      +           gx( 0) * brm(cell_x1  ,cell_y2+1,im) &
      +           gx( 1) * brm(cell_x1+1,cell_y2+1,im))), num)

  bt_part = bt_part + REAL( exp_min_imtheta * (&
        gy(-1) * (gx(-1) * btm(cell_x1-1,cell_y1-1,im) &
      +           gx( 0) * btm(cell_x1  ,cell_y1-1,im) &
      +           gx( 1) * btm(cell_x1+1,cell_y1-1,im)) &
      + gy( 0) * (gx(-1) * btm(cell_x1-1,cell_y1  ,im) &
      +           gx( 0) * btm(cell_x1  ,cell_y1  ,im) &
      +           gx( 1) * btm(cell_x1+1,cell_y1  ,im)) &
      + gy( 1) * (gx(-1) * btm(cell_x1-1,cell_y1+1,im) &
      +           gx( 0) * btm(cell_x1  ,cell_y1+1,im) &
      +           gx( 1) * btm(cell_x1+1,cell_y1+1,im))), num)

  exp_min_imtheta = exp_min_imtheta * exp_min_itheta
END DO

by_part =  br_part * REAL(exp_min_itheta) + bt_part * AIMAG(exp_min_itheta)
bz_part = -br_part * AIMAG(exp_min_itheta) + bt_part * REAL(exp_min_itheta)

!==============================================================================
! 2024/08/04 by katsura
!------------------------------------------------------------------------------
!bx_part = bx_part &
!    + mu0 * 12940.0_num / (2.0_num * pi) &
!    * (((-part_y - 6.0e-4_num) / ((part_x + 6.0e-4_num) * (part_x + 6.0e-4_num) &
!    + (part_y - 6.0e-4_num) * (part_y - 6.0e-4_num))) - ((-part_y + 6.0e-4_num) &
!    / ((part_x + 6.0e-4_num) * (part_x + 6.0e-4_num) &
!    + (part_y + 6.0e-4_num) * (part_y + 6.0e-4_num))))
!by_part = by_part &
!    + mu0 * 12940.0_num / (2.0_num * pi) &
!    * ((( part_x + 6.0e-4_num) / ((part_x + 6.0e-4_num) * (part_x + 6.0e-4_num) &
!    + (part_y - 6.0e-4_num) * (part_y - 6.0e-4_num))) - (( part_x + 6.0e-4_num) &
!    / ((part_x + 6.0e-4_num) * (part_x + 6.0e-4_num) &
!    + (part_y + 6.0e-4_num) * (part_y + 6.0e-4_num))))
!==============================================================================