subroutine init_model(time_steps, field_in, x_no)
  use mod_model, only: total_steps, nx, field

  implicit none

  integer, intent(in) :: time_steps
  real, dimension(x_no), target, intent(inout) :: field_in
  integer, intent(in) :: x_no

  total_steps = time_steps
  nx = x_no
  field => field_in

end subroutine
