module ocean_model_mod

  use mpp_domains_mod, only : domain2d

  use time_manager_mod, only : time_type
  
  use ocean_types_mod, only : ocean_data_type, ice_ocean_boundary_type
  
  use fms_mod, only         : error_mesg, FATAL

  implicit none

  private
  
  character(len=128) :: version = '$Id: ocean_model.F90,v 10.0 2003/10/24 22:01:25 fms Exp $'
  character (len=128) :: tagname = '$Name: lima $'

  public ocean_model_init, ocean_model_end , update_ocean_model,            &
         ocean_data_type, ice_ocean_boundary_type, read_ice_ocean_boundary, &
         write_ice_ocean_boundary, init_default_ice_ocean_boundary

contains

  subroutine ocean_model_init(Ocean, Time_init, Time_in, Time_step_ocean)

    type(ocean_data_type), intent(inout) :: Ocean
    
    type(time_type), intent(in) :: Time_init, Time_in, Time_step_ocean


    return

  end subroutine ocean_model_init


  subroutine update_ocean_model(Ice_ocean_boundary, Ocean, ocean_seg_start, &
       ocean_seg_end, num_ocean_calls)

    type(ice_ocean_boundary_type), intent(inout) :: Ice_ocean_boundary
    type(ocean_data_type), intent(inout) :: Ocean
    logical, intent(in) :: ocean_seg_start, ocean_seg_end
    integer, intent(in) :: num_ocean_calls


    return

  end subroutine update_ocean_model



  subroutine ocean_model_end(Ocean)


    type(ocean_data_type), intent(in) :: Ocean

    
    return

  end subroutine ocean_model_end

!#######################################################################
! This ocean_model doesn't support concurrent runs,
! so this routine should never be called. This is a dummy routine.
  subroutine read_ice_ocean_boundary(file_name,iob,Ocean)

    character(LEN=*),             intent(IN)    :: file_name  
    type(ice_ocean_boundary_type),intent(INOUT) :: iob
    type(ocean_data_type),        intent(IN)    :: Ocean

    call error_mesg('ocean_model_mod', 'ocean_mixed_layer model can not run concurrently', &
                    FATAL )
    return

  end subroutine read_ice_ocean_boundary

!#######################################################################
! This ocean_model doesn't support concurrent runs,
! so this routine should never be called. This is a dummy routine.
  subroutine write_ice_ocean_boundary(file_name,iob,Ocean)

    character(LEN=*),             intent(IN) :: file_name  
    type(ice_ocean_boundary_type),intent(IN) :: iob
    type(ocean_data_type),        intent(IN) :: Ocean

    call error_mesg('ocean_model_mod', 'ocean_mixed_layer model can not run concurrently', &
                    FATAL )
    return

  end subroutine write_ice_ocean_boundary

!#######################################################################
! dummy routine

  subroutine init_default_ice_ocean_boundary(iob)

    type(ice_ocean_boundary_type),intent(INOUT) :: iob

    return

  end subroutine init_default_ice_ocean_boundary

!#######################################################################


end module ocean_model_mod
  
  
 
