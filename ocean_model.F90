module ocean_model_mod

use mpp_domains_mod, only: domain2d

use fms_mod, only: error_mesg, FATAL

use coupler_types_mod,only: coupler_2d_bc_type

implicit none
private

public :: ocean_model_init, ocean_model_end, update_ocean_model, ocean_data_type, &
          ice_ocean_boundary_type, ocean_grids_type, read_ice_ocean_boundary, &
          write_ice_ocean_boundary, init_default_ice_ocean_boundary, &
          ocean_model_flux_init, ocean_model_init_sfc, ocean_stock_pe, ocean_model_restart

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

type ocean_public_type
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
end type ocean_public_type

  type, public ::  ocean_state_type; private
     ! This type is private, and can therefore vary between different ocean models.
     ! All information entire ocean state may be contained here, although it is not
     ! necessary that this is implemented with all models.
     logical       :: is_ocean_pe = .false.       ! .true. on processors that run the ocean model.
  end type ocean_state_type

!-----------------------------------------------------------------------

   character(len=128) :: version = '$Id: ocean_model.F90,v 16.0.2.1 2008/09/05 13:10:35 z1l Exp $'
   character(len=128) :: tagname = '$Name: perth_2008_10 $'

contains

!#######################################################################

 subroutine update_ocean_model (Ice_boundary, Ocean, Ocean_sfc, &
       time_start_update, Ocean_coupling_time_step)

 type (ice_ocean_boundary_type), intent(in) :: Ice_boundary
 type(ocean_state_type),   pointer       :: Ocean_state
 type (ocean_public_type), intent(inout) :: Ocean
 type(time_type), intent(in)             :: time_start_update
 type(time_type), intent(in)             :: Ocean_coupling_time_step

 call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

 end subroutine update_ocean_model

!#######################################################################

 subroutine ocean_model_init (Ocean, Ocean_state, Time_init, Time)

 type (ocean_public_type), intent(inout) :: Ocean
 type(ocean_state_type),      pointer    :: Ocean_state
 type (time_type), intent(in) :: Time_init, Time, Time_step

 call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

 end subroutine ocean_model_init

!#######################################################################

  subroutine ocean_model_end (Ocean, Ocean_state, Time_in)

  type(ocean_state_type),            pointer    :: Ocean_state
  type(time_type),                   intent(in) :: Time_in
  type(ocean_public_type), optional, intent(in) :: Ocean

  end subroutine ocean_model_end

!#######################################################################
! <SUBROUTINE NAME="ocean_model_restart">
!
! <DESCRIPTION>
! dummy interface.
! Arguments: 
!   timestamp (optional, intent(in)) : A character string that represents the model time, 
!                                      used for writing restart. timestamp will append to
!                                      the any restart file name as a prefix. 
! </DESCRIPTION>
!
  subroutine ocean_model_restart(Ocean_state, timestamp)
     type(ocean_state_type),    pointer     :: Ocean_state
     character(len=*), intent(in), optional :: timestamp

     ! dummy routine

  end subroutine ocean_model_restart
! </SUBROUTINE> NAME="ocean_model_restart"

!#######################################################################
  subroutine ocean_model_init_sfc(Ocean_state, Ocean)
  type(ocean_state_type),  pointer    :: Ocean_state    
  type(ocean_public_type), intent(in) :: Ocean

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
  type(ocean_public_type),        intent(IN)    :: Ocean

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine read_ice_ocean_boundary
!#######################################################################
  subroutine write_ice_ocean_boundary(file_name,iob,Ocean)

  character(LEN=*),             intent(IN) :: file_name  
  type(ice_ocean_boundary_type),intent(IN) :: iob
  type(ocean_public_type),        intent(IN) :: Ocean

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine write_ice_ocean_boundary
!#######################################################################
  subroutine init_default_ice_ocean_boundary(iob)

  type(ice_ocean_boundary_type),intent(INOUT) :: iob

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine init_default_ice_ocean_boundary
!#######################################################################
  subroutine ocean_stock_pe(Ocean_state, index, value, time_index)
  type(ocean_state_type), intent(in) :: Ocean_state
  integer,               intent(in)  :: index
  real,                  intent(out) :: value
  integer, optional,     intent(in)  :: time_index

  call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

  end subroutine ocean_stock_pe
!#######################################################################

end module ocean_model_mod
