!$Id: init_dim_obs_pdaf.F90 1864 2017-12-20 19:53:30Z lnerger $
!BOP
!
! !ROUTINE: init_dim_obs_pdaf --- Compute number of observations
!
! !INTERFACE:
SUBROUTINE init_dim_obs_pdaf(step, dim_obs_p)

! !DESCRIPTION:
! User-supplied routine for PDAF.
! Used in the filters: SEEK/SEIK/EnKF/ETKF/ESTKF
!
! The routine is called at the beginning of each
! analysis step.  It has to initialize the size of 
! the observation vector according to the current 
! time step for the PE-local domain.
!
! Implementation for the 2D online example
! without parallelization.
!
! !REVISION HISTORY:
! 2013-02 - Lars Nerger - Initial code
! Later revisions - see svn log
!
! !USES:
  USE mod_assimilation, &
       ONLY : obs_p, obs_index_p
  USE mod_model, &
       ONLY : nx

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER, INTENT(in)  :: step       ! Current time step
  INTEGER, INTENT(out) :: dim_obs_p  ! Dimension of observation vector

! !CALLING SEQUENCE:
! Called by: PDAF_seek_analysis    (as U_init_dim_obs)
! Called by: PDAF_seik_analysis, PDAF_seik_analysis_newT
! Called by: PDAF_enkf_analysis_rlm, PDAF_enkf_analysis_rsm
! Called by: PDAF_etkf_analysis, PDAF_etkf_analysis_T
! Called by: PDAF_estkf_analysis, PDAF_estkf_analysis_fixed
!EOP

! *** Local variables
  INTEGER :: i                        ! Counters
  INTEGER :: cnt, cnt0                ! Counters
  REAL, ALLOCATABLE :: obs_field(:) ! Array for observation field read from file
  CHARACTER(len=4) :: stepstr         ! String for time step


! ****************************************
! *** Initialize observation dimension ***
! ****************************************

  ! Read observation field form file
  ALLOCATE(obs_field(nx))

  IF (step<10) THEN
    WRITE (stepstr, '(i1)') step
  else if (step < 100) then
    WRITE (stepstr, '(i2)') step
  else if (step < 1000) then
    write (stepstr, '(i3)') step
  else
    write (stepstr, '(i4)') step
  END IF

  OPEN (12, file='obs/obs_step'//TRIM(stepstr)//'.txt', status='old')
  READ (12, *) obs_field
  CLOSE (12)

  ! Count observations
  cnt = 0
  DO i = 1, nx
    IF (obs_field(i) > -999.0) cnt = cnt + 1
  END DO

  ! Set number of observations
  dim_obs_p = cnt

  ! Initialize vector of observations and index array
  IF (ALLOCATED(obs_index_p)) DEALLOCATE(obs_index_p)
  IF (ALLOCATED(obs_p)) DEALLOCATE(obs_p)
  ALLOCATE(obs_index_p(dim_obs_p))
  ALLOCATE(obs_p(dim_obs_p))

  cnt = 0
  cnt0 = 0
  DO i = 1, nx
    cnt0 = cnt0 + 1
    IF (obs_field(i) > -999.0) THEN
       cnt = cnt + 1
       obs_index_p(cnt) = cnt0      ! Index of observation in state vector
       obs_p(cnt) = obs_field(i) ! Vector of observations
    END IF
  END DO


! *** Clean up ***

  DEALLOCATE(obs_field)

END SUBROUTINE init_dim_obs_pdaf
