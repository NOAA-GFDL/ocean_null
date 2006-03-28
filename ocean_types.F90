module ocean_types_mod

! This module contains type declarations and default values
! for ocean model.  
!

  use time_manager_mod, only : time_type
  use mpp_domains_mod, only : domain2d
  use mpp_mod, only : stdout

  implicit none

  private

#else

  ! for communication with FMS coupler

  type, public ::  ocean_data_type 
     type(domain2d) :: Domain
     real, pointer, dimension(:,:)    :: t_surf, s_surf, sea_lev, frazil, u_surf, v_surf
     logical, pointer, dimension(:,:) :: maskmap =>NULL()! A pointer to an array indicating which
                                                         ! logical processors are actually used for
                                                         ! the ocean code. The other logical
                                                         ! processors would be all land points and
                                                         ! are not assigned to actual processors.
                                                         ! This need not be assigned if all logical
                                                         ! processors are used. This variable is dummy and need 
                                                         ! not to be set, but it is needed to pass compilation.
     integer :: avg_kount
     logical :: pe
     integer, dimension(:), pointer :: pelist
  end type ocean_data_type

  type, public :: ice_ocean_boundary_type
     real, dimension(:,:), pointer :: u_flux, v_flux, t_flux, q_flux, salt_flux, lw_flux, sw_flux, lprec, fprec
     real, dimension(:,:), pointer :: sw_flux_vis, sw_flux_vis_dir, sw_flux_vis_dif, sw_flux_dir, sw_flux_dif
     real, dimension(:,:), pointer :: runoff, calving
     real, dimension(:,:), pointer :: p
     real, dimension(:,:,:), pointer :: data !collective field for "named" fields above
     integer :: xtype             !REGRID, REDIST or DIRECT
  end type ice_ocean_boundary_type
  
end module ocean_types_mod


