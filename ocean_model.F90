!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the GFDL Ocean Null Model Component.
!*
!* Ocean Null is free software: you can redistribute it and/or modify it
!* under the terms of the GNU Lesser General Public License as published
!* by the Free Software Foundation, either version 3 of the License, or
!* (at your option) any later version.
!*
!* Ocean Null is distributed in the hope that it will be useful, but
!* WITHOUT ANY WARRANTY; without even the implied warranty of
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!* General Public License for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with Ocean Null.
!* If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************

module ocean_model_mod

use   mpp_domains_mod, only: domain2d, mpp_define_layout, mpp_define_domains, mpp_get_compute_domain

use           fms_mod, only: error_mesg, FATAL, write_version_number, mpp_npes

use           mpp_mod, only: mpp_error, mpp_npes, mpp_get_current_pelist
use       fms2_io_mod, only: open_file, FmsNetcdfFile_t, get_variable_size, variable_exists
use       fms2_io_mod, only: read_data, close_file

use  time_manager_mod, only: time_type

use coupler_types_mod, only: coupler_1d_bc_type, coupler_2d_bc_type

use       mosaic2_mod, only: get_mosaic_ntiles, get_mosaic_grid_sizes, get_mosaic_xgrid
use       mosaic2_mod, only: get_mosaic_xgrid_size, calc_mosaic_grid_area, get_mosaic_tile_grid

use     constants_mod, only: PI, RADIUS

implicit none
private

public :: ocean_model_init, ocean_model_end, update_ocean_model, &
          ice_ocean_boundary_type, ocean_grids_type, &
          ocean_model_flux_init, ocean_model_init_sfc, &
          ocean_stock_pe, ocean_model_restart, &
          ice_ocn_bnd_type_chksum, ocean_public_type_chksum

public    ocean_model_data_get
interface ocean_model_data_get
   module procedure ocean_model_data1D_get
   module procedure ocean_model_data2D_get
end interface

!-----------------------------------------------------------------------

type ice_ocean_boundary_type
  real, dimension(:,:), pointer :: u_flux =>NULL(), &
                                   v_flux =>NULL(), &
                                   t_flux =>NULL(), &
                                   q_flux =>NULL(), &
                                   salt_flux =>NULL(), &
                                   lw_flux =>NULL(), &
                                   sw_flux =>NULL(), &
                                   sw_flux_nir_dir =>NULL(), &
                                   sw_flux_nir_dif =>NULL(), &
                                   sw_flux_vis_dir =>NULL(), &
                                   sw_flux_vis_dif =>NULL(), &
                                   lprec =>NULL(), &
                                   fprec  =>NULL()
  real, dimension(:,:), pointer :: runoff =>NULL(), &
                                   calving  =>NULL(), &
                                   stress_mag =>NULL(), &
                                   ustar_berg =>NULL(), &
                                   area_berg =>NULL(), &
                                   mass_berg =>NULL()
  real, pointer, dimension(:,:) :: runoff_hflx     =>NULL() ! heat flux of liquid runoff (kg/m2/s)
  real, pointer, dimension(:,:) :: calving_hflx    =>NULL() ! heat flux of frozen runoff (kg/m2/s)
  real, dimension(:,:), pointer :: p  =>NULL()
  real, dimension(:,:), pointer :: mi =>NULL()
  real, dimension(:,:,:), pointer :: data  =>NULL()
  integer :: xtype, wind_stagger
  type(coupler_2d_bc_type)      :: fluxes
end type ice_ocean_boundary_type

!-----------------------------------------------------------------------

 type ocean_grids_type
    real,    pointer, dimension(:)   :: lon_bnd =>NULL(), lat_bnd =>NULL()
    real,    pointer, dimension(:,:) :: lon =>NULL(), lat =>NULL()
    logical, pointer, dimension(:,:) :: mask  =>NULL()
 end type

!-----------------------------------------------------------------------

type, public :: ocean_public_type
   type (domain2d)               :: Domain
   type (ocean_grids_type)       :: Global, Data
   real, pointer, dimension(:,:) :: t_surf =>NULL() , &
                                    frazil =>NULL() , &
                                    u_surf =>NULL() , &
                                    v_surf =>NULL() , &
                                    s_surf =>NULL() , &
                                    area   =>NULL() , &
                                    sea_lev=>NULL()
   logical, pointer, dimension(:,:) :: maskmap =>NULL()
   logical :: is_ocean_pe = .false.
   integer, pointer :: pelist(:) =>NULL()
   integer                          :: avg_kount
   integer, dimension(3)            :: axes
   type(coupler_2d_bc_type)         :: fields
   integer                          :: stagger = -999
end type ocean_public_type

  type, public ::  ocean_state_type; private
     ! This type is private, and can therefore vary between different ocean models.
     ! All information entire ocean state may be contained here, although it is not
     ! necessary that this is implemented with all models.
     logical       :: is_ocean_pe = .false.       ! .true. on processors that run the ocean model.
  end type ocean_state_type

!-----------------------------------------------------------------------

   character(len=128) :: version = '$Id$'
   character(len=128) :: tagname = '$Name$'

contains

!#######################################################################

 subroutine update_ocean_model (Ice_boundary, Ocean_state, Ocean_sfc, &
       time_start_update, Ocean_coupling_time_step, update_dyn, update_thermo, &
       Ocn_fluxes_used)

 type(ice_ocean_boundary_type), intent(in)    :: Ice_boundary
 type(ocean_state_type),        pointer       :: Ocean_state
 type(ocean_public_type),       intent(inout) :: Ocean_sfc
 type(time_type), intent(in)                  :: time_start_update
 type(time_type), intent(in)                  :: Ocean_coupling_time_step
 logical, optional, intent(in)    :: update_dyn !< If present and false,
                                     !! do not do updates due to the ocean dynamics.
                                     !! This is a dummy argument for MOM6 compatibility
 logical, optional, intent(in)    :: update_thermo !< If present and false, do not do updates
                                                 !! due to the ocean thermodynamics or remapping.
                                                 !! This is a dummy argument for MOM6 compatibility
 logical, optional, intent(in)    :: Ocn_fluxes_used !< If present, this indicates whether the
                                              !! cumulative thermodynamic fluxes from the ocean,
                                              !! This is a dummy argument for MOM6 compatibility
!! like frazil, have been used and should be reset.

 call error_mesg('ocean_model_mod', 'null ocean model should not be executed', FATAL )

 end subroutine update_ocean_model

!#######################################################################

 subroutine ocean_model_init (Ocean, Ocean_state, Time_init, Time, gas_fields_ocn)

 type(ocean_public_type), intent(inout) :: Ocean
 type(ocean_state_type),  pointer       :: Ocean_state
 type(time_type), intent(in) :: Time_init, Time
 type(coupler_1d_bc_type), &
             optional, intent(in)    :: gas_fields_ocn !< If present, this type describes the
                                              !! ocean and surface-ice fields that will participate
                                              !! in the calculation of additional gas or other
                                              !! tracer fluxes, and can be used to spawn related
                                              !! internal variables in the ice model.

 real,    allocatable, dimension(:)     :: xgrid_area
 real,    allocatable, dimension(:,:)   :: geo_lonv, geo_latv, rmask, geo_lont, geo_latt, tmpx, tmpy, garea
 real,    allocatable, dimension(:,:,:) :: x_vert_t, y_vert_t
 integer, allocatable, dimension(:)     :: i1, j1, i2, j2
 integer                                :: i, j, ntiles, nfile_axo, nxgrid, n, m, grid_version, nlon, nlat
 integer                                :: isd, ied, jsd, jed
 integer                                :: layout(2), siz(4), nx(1), ny(1)
 character(len=256)                     :: grid_file = "INPUT/grid_spec.nc"
 character(len=256)                     :: ocean_mosaic, tile_file, ocn_mosaic_file, axo_file
 type(FmsNetcdfFile_t)                  :: grid_fileobj !< Fms2_io fileobj
 type(FmsNetcdfFile_t)                  :: ocean_mosaic_fileobj !< Fms2_io fileobj
 type(FmsNetcdfFile_t)                  :: tile_fileobj !< Fms2_io fileobj
 type(FmsNetcdfFile_t)                  :: axo_fileobj !< Fms2_io_fileobj
 integer, dimension(:), allocatable     :: pes !> Current pelist

 !> Open the grid_file with pelist argument so that only one pes open/reads the file
 allocate(pes(mpp_npes()))
 call mpp_get_current_pelist(pes)

 if ( .not. open_file(grid_fileobj, grid_file, "read", pelist=pes)) then
     call mpp_error(FATAL, 'ocean_model_init: error opening '//trim(grid_file))
 endif

 call write_version_number(version, tagname)

    if(variable_exists(grid_fileobj, 'geolon_t')) then
       grid_version = 0
       call get_variable_size(grid_fileobj, 'geolon_t', siz(1:2))
       nlon = siz(1)
       nlat = siz(2)
    else if(variable_exists(grid_fileobj, 'x_T')) then
       grid_version = 1
       call get_variable_size(grid_fileobj, 'x_T', siz(1:2))
       nlon = siz(1)
       nlat = siz(2)
    else if(variable_exists(grid_fileobj, 'ocn_mosaic_file') ) then ! read from mosaic file
       grid_version = 2
       call read_data(grid_fileobj, "ocn_mosaic_file", ocean_mosaic)
       ocean_mosaic = "INPUT/"//trim(ocean_mosaic)

       if ( .not. open_file(ocean_mosaic_fileobj, ocean_mosaic, "read", pelist=pes)) then
          call mpp_error(FATAL, 'ocean_model_init: error opening '//trim(ocean_mosaic))
       endif

       ntiles = get_mosaic_ntiles(ocean_mosaic_fileobj)
       if(ntiles .NE. 1) call error_mesg('ocean_model_init', ' ntiles should be 1 for ocean mosaic, contact developer', FATAL)
       call get_mosaic_grid_sizes( ocean_mosaic_fileobj, nx, ny)
       nlon = nx(1)
       nlat = ny(1)
    else
       call error_mesg('ocean_model_init','x_T, geolon_t, ocn_mosaic_file does not exist in file '//trim(grid_file), FATAL )
    end if

    allocate (Ocean%Global%lon_bnd (nlon+1),     &
              Ocean%Global%lat_bnd (nlat+1),     &
              Ocean%Global%lon     (nlon, nlat), &
              Ocean%Global%lat     (nlon, nlat), &
              Ocean%Global%mask    (nlon, nlat))

    allocate (rmask(nlon,nlat), geo_lont(nlon,nlat), geo_latt(nlon,nlat), &
              geo_lonv(1:nlon+1,1:nlat+1), geo_latv(1:nlon+1,1:nlat+1) )

    layout = (/0,0/)
    call mpp_define_layout( (/1,nlon,1,nlat/), mpp_npes(), layout)
    call mpp_define_domains ( (/1,nlon,1,nlat/), layout, Ocean%Domain, name='NULL Ocean')

    select case (grid_version)
    case(0)
       call read_data(grid_fileobj, "geolon_t",      geo_lont)
       call read_data(grid_fileobj, "geolat_t",      geo_latt)
       call read_data(grid_fileobj, "geolon_vert_t", geo_lonv)
       call read_data(grid_fileobj, "geolat_vert_t", geo_latv)
       call read_data(grid_fileobj, "wet",      rmask)
    case(1)
       allocate (x_vert_t(nlon,nlat,4), y_vert_t(nlon,nlat,4) )
       call read_data(grid_fileobj, "x_T", geo_lont)
       call read_data(grid_fileobj, "y_T", geo_latt)
       call read_data(grid_fileobj, "x_vert_T", x_vert_t)
       call read_data(grid_fileobj, "y_vert_T", y_vert_t)
       geo_lonv(1:nlon,1:nlat) = x_vert_t(1:nlon,1:nlat,1)
       geo_lonv(nlon+1,1:nlat) = x_vert_t(nlon,1:nlat,2)
       geo_lonv(1:nlon,nlat+1) = x_vert_t(1:nlon,nlat,4)
       geo_lonv(nlon+1,nlat+1) = x_vert_t(nlon,nlat,3)
       geo_latv(1:nlon,1:nlat) = y_vert_t(1:nlon,1:nlat,1)
       geo_latv(nlon+1,1:nlat) = y_vert_t(nlon,1:nlat,2)
       geo_latv(1:nlon,nlat+1) = y_vert_t(1:nlon,nlat,4)
       geo_latv(nlon+1,nlat+1) = y_vert_t(nlon,nlat,3)
       deallocate(x_vert_t, y_vert_t)
       call read_data(grid_fileobj, "wet",      rmask)
    case(2)
       call get_mosaic_tile_grid(tile_file, ocean_mosaic_fileobj, Ocean%Domain )
       call close_file(ocean_mosaic_fileobj)

       allocate(tmpx(2*nlon+1, 2*nlat+1), tmpy(2*nlon+1, 2*nlat+1) )
       allocate(garea(nlon, nlat))

       if ( .not. open_file(tile_fileobj, tile_file, "read")) then
          call mpp_error(FATAL, 'ocean_model_init: error opening '//trim(tile_file))
       endif

       call read_data(tile_fileobj, "x", tmpx)
       call read_data(tile_fileobj, "y", tmpy)
       call close_file(tile_fileobj)

       do j = 1, nlat
          do i = 1, nlon
             geo_lont(i,j) = tmpx(i*2,j*2)
             geo_latt(i,j) = tmpy(i*2,j*2)
          end do
       end do
       do j = 1, nlat+1
          do i = 1, nlon+1
             geo_lonv(i,j) = tmpx(i*2-1,j*2-1)
             geo_latv(i,j) = tmpy(i*2-1,j*2-1)
          end do
       end do

       call calc_mosaic_grid_area(geo_lonv*pi/180., geo_latv*pi/180., garea )
       garea = garea/(4*PI*RADIUS*RADIUS)  ! scale the earth are to be 1
       call get_variable_size(grid_fileobj, "aXo_file", siz(1:2))
       nfile_axo = siz(2)
       rmask = 0.0
       do n = 1, nfile_axo
          call read_data(grid_fileobj, "aXo_file", axo_file, corner=n)
          axo_file = 'INPUT/'//trim(axo_file)
          if ( .not. open_file(axo_fileobj, axo_file, "read")) then
             call mpp_error(FATAL, 'ocean_model_init: error opening '//trim(axo_file))
          endif

          nxgrid = get_mosaic_xgrid_size(axo_fileobj)
          allocate(i1(nxgrid), j1(nxgrid), i2(nxgrid), j2(nxgrid), xgrid_area(nxgrid))
          call get_mosaic_xgrid(aXo_fileobj, i1, j1, i2, j2, xgrid_area)
          do m = 1, nxgrid
             i = i2(m); j = j2(m)
             rmask(i,j) = rmask(i,j) + xgrid_area(m)
          end do
          call close_file(aXo_fileobj)
          deallocate(i1, j1, i2, j2, xgrid_area)
       end do
       rmask = rmask/garea

       deallocate(tmpx, tmpy, garea)
    end select

   Ocean%Global%mask = .false.
   where (rmask > 0) Ocean%Global%mask = .true.
   Ocean%Global%lon_bnd = geo_lonv(:,1)*pi/180.
   Ocean%Global%lat_bnd = geo_latv(1,:)*pi/180.
   Ocean%Global%lon = geo_lont*pi/180.
   Ocean%Global%lat = geo_latt*pi/180.
   deallocate(geo_lonv, geo_latv, geo_lont, geo_latt, rmask)

   call mpp_get_compute_domain ( Ocean%Domain, isd, ied, jsd, jed )
   allocate ( Ocean%Data%lon_bnd (isd:ied+1),      &
              Ocean%Data%lat_bnd (jsd:jed+1),      &
              Ocean%Data%lon (isd:ied, jsd:jed),   &
              Ocean%Data%lat (isd:ied, jsd:jed),   &
              Ocean%Data%mask(isd:ied,jsd:jed))

   Ocean%Data%lon_bnd = Ocean%Global%lon_bnd(isd:ied+1)
   Ocean%Data%lat_bnd = Ocean%Global%lat_bnd(jsd:jed+1)
   Ocean%Data%lon     = Ocean%Global%lon(isd:ied,jsd:jed)
   Ocean%Data%lat     = Ocean%Global%lat(isd:ied,jsd:jed)
   Ocean%Data%mask    = .FALSE.

   allocate ( Ocean%t_surf (isd:ied,jsd:jed), &
              Ocean%u_surf (isd:ied,jsd:jed), &
              Ocean%v_surf (isd:ied,jsd:jed), &
              Ocean%frazil (isd:ied,jsd:jed), &
              Ocean%area   (isd:ied,jsd:jed), &
              Ocean%s_surf (isd:ied,jsd:jed), &
              Ocean%sea_lev(isd:ied,jsd:jed))

    Ocean%t_surf  = 280.
    Ocean%u_surf  = 0.0
    Ocean%v_surf  = 0.0
    Ocean%frazil  = 0.0
    Ocean%area    = 1.0
    Ocean%s_surf  = 0.0
    Ocean%sea_lev = 0.0

    call close_file(grid_fileobj)
 end subroutine ocean_model_init

!#######################################################################

  subroutine ocean_model_end (Ocean, Ocean_state, Time_in)

  type(ocean_state_type),            pointer    :: Ocean_state
  type(time_type),                   intent(in) :: Time_in
  type(ocean_public_type), optional, intent(in) :: Ocean

     ! dummy routine

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



  end subroutine ocean_model_init_sfc
!#######################################################################
  subroutine ocean_model_flux_init(Ocean_state)
  type(ocean_state_type),pointer,optional :: Ocean_state


  end subroutine ocean_model_flux_init
!#######################################################################
  subroutine ocean_stock_pe(Ocean_state, index, value, time_index)
  type(ocean_state_type), pointer    :: Ocean_state
  integer,               intent(in)  :: index
  real,                  intent(out) :: value
  integer, optional,     intent(in)  :: time_index

  value = 0.0

  end subroutine ocean_stock_pe
!#######################################################################

subroutine ocean_model_data2D_get(OS,Ocean, name, array2D,isc,jsc)
  type(ocean_state_type),     pointer    :: OS
  type(ocean_public_type),    intent(in) :: Ocean
  character(len=*)          , intent(in) :: name
  real, dimension(isc:,jsc:), intent(out):: array2D
  integer                   , intent(in) :: isc,jsc

  array2D(isc:,jsc:) = 0.0

end subroutine ocean_model_data2D_get
!#######################################################################

subroutine ocean_model_data1D_get(OS,Ocean, name, value)
  type(ocean_state_type),     pointer    :: OS
  type(ocean_public_type),    intent(in) :: Ocean
  character(len=*)          , intent(in) :: name
  real                      , intent(out):: value

  value = 0.0

end subroutine ocean_model_data1D_get
!#######################################################################

subroutine ice_ocn_bnd_type_chksum(id, timestep, Ice_ocean_boundary)

    character(len=*), intent(in) :: id
    integer         , intent(in) :: timestep
    type(ice_ocean_boundary_type), intent(in) :: Ice_ocean_boundary
    return
end subroutine ice_ocn_bnd_type_chksum
!#######################################################################

subroutine ocean_public_type_chksum(id, timestep, Ocean)

    character(len=*), intent(in) :: id
    integer         , intent(in) :: timestep
    type(ocean_public_type), intent(in) :: Ocean
    return
end subroutine ocean_public_type_chksum
!#######################################################################

end module ocean_model_mod
