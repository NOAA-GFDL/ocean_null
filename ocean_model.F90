module ocean_model_mod

use mpp_domains_mod, only: domain2d

use fms_mod, only: error_mesg, FATAL

use coupler_types_mod,only: coupler_2d_bc_type

implicit none
private

public :: ocean_model_init, ocean_model_end, update_ocean_model, ocean_data_type, &
          ice_ocean_boundary_type, ocean_grids_type, read_ice_ocean_boundary, &
          write_ice_ocean_boundary, init_default_ice_ocean_boundary, &
          ocean_model_flux_init, ocean_model_init_sfc, ocean_stock_pe

!-----------------------------------------------------------------------

type ice_ocean_boundary_type
  real, dimension(:,:), pointer :: u_flux =>NULL(), &
                                   v_flux =>NULL(), &
                                   t_flux =>NULL(), &
                                   q_flux =>NULL(), &
                                   salt_flux =>NULL(), &
                                   lw_flux =>NULL(), &
                                   sw_flux =>NULL(), &
                                   sw_flux_vis =>NULL(), &
                                   sw_flux_dir =>NULL(), &
                                   sw_flux_dif =>NULL(), &
                                   sw_flux_vis_dir =>NULL(), &
                                   sw_flux_vis_dif =>NULL(), &
                                   lprec =>NULL(), &
                                   fprec  =>NULL()
  real, dimension(:,:), pointer :: runoff =>NULL(), &
                                   calving  =>NULL()
  real, dimension(:,:), pointer :: p  =>NULL()
  real, dimension(:,:,:), pointer :: data  =>NULL()
  integer :: xtype
  type(coupler_2d_bc_type)      :: fluxes
end type ice_ocean_boundary_type

!-----------------------------------------------------------------------

 type ocean_grids_type
    real,    pointer, dimension(:)   :: lon_bnd =>NULL(), lat_bnd =>NULL()
    real,    pointer, dimension(:,:) :: lon =>NULL(), lat =>NULL()
    logical, pointer, dimension(:,:) :: mask  =>NULL()
 end type

!-----------------------------------------------------------------------

type ocean_data_type
   type (domain2d)               :: Domain
   type (ocean_grids_type)       :: Global, Data
   real, pointer, dimension(:,:) :: t_surf =>NULL() , &
                                    frazil =>NULL() , &
                                    u_surf =>NULL() , &
                                    v_surf =>NULL() , &
                                    s_surf =>NULL() , &
                                    sea_lev=>NULL()
   logical, pointer, dimension(:,:) :: maskmap =>NULL()
   logical :: pe
   integer, pointer :: pelist(:) =>NULL()
   integer, dimension(3)            :: axes    
   type(coupler_2d_bc_type)         :: fields
end type ocean_data_type

!-----------------------------------------------------------------------

   character(len=128) :: version = '$Id: ocean_model.F90,v 14.0 2007/03/15 22:37:05 fms Exp $'
   character(len=128) :: tagname = '$Name: nalanda $'

contains

!#######################################################################

 subroutine update_ocean_model (Ice_boundary, Ocean,  &
       ocean_seg_start, ocean_seg_end, num_ocean_calls )

 type (ice_ocean_boundary_type), intent(in) :: Ice_boundary
 type (ocean_data_type), intent(inout) :: Ocean
 logical, intent(in), optional :: ocean_seg_start, ocean_seg_end
 integer ,intent(in), optional :: num_ocean_calls

 call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

 end subroutine update_ocean_model

!#######################################################################

 subroutine ocean_model_init (Ocean, Time_init, Time, Time_step)

 type (ocean_data_type), intent(inout) :: Ocean
 type (time_type), intent(in) :: Time_init, Time, Time_step

 call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

 end subroutine ocean_model_init

!#######################################################################

  subroutine ocean_model_end (Ocean)

  type(ocean_data_type), intent(in) :: Ocean

  end subroutine ocean_model_end

!#######################################################################
  subroutine ocean_model_init_sfc(Ocean)

  type(ocean_data_type), intent(in) :: Ocean

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine ocean_model_init_sfc
!#######################################################################
  subroutine ocean_model_flux_init

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine ocean_model_flux_init
!#######################################################################
  subroutine read_ice_ocean_boundary(file_name,iob,Ocean)

  character(LEN=*),             intent(IN)    :: file_name  
  type(ice_ocean_boundary_type),intent(INOUT) :: iob
  type(ocean_data_type),        intent(IN)    :: Ocean

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine read_ice_ocean_boundary
!#######################################################################
  subroutine write_ice_ocean_boundary(file_name,iob,Ocean)

  character(LEN=*),             intent(IN) :: file_name  
  type(ice_ocean_boundary_type),intent(IN) :: iob
  type(ocean_data_type),        intent(IN) :: Ocean

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine write_ice_ocean_boundary
!#######################################################################
  subroutine init_default_ice_ocean_boundary(iob)

  type(ice_ocean_boundary_type),intent(INOUT) :: iob

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine init_default_ice_ocean_boundary
!#######################################################################
  subroutine ocean_stock_pe(Ocean, index, value, time_index)
  type(ocean_data_type), intent(in)  :: Ocean
  integer,               intent(in)  :: index
  real,                  intent(out) :: value
  integer, optional,     intent(in)  :: time_index

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine ocean_stock_pe
!#######################################################################

end module ocean_model_mod
