subroutine get_rank(rank)
  use mod_parallel_pdaf, only: mype_world
  integer, intent(out) :: rank
  rank = mype_world
end subroutine
