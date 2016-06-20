module stdiocdf

	use netcdf
	
	use precision
	implicit none

	!-- Declare evrything private by default
	private

	!-- Declare exported procedure
	public :: write_var3d

contains

subroutine write_var3d(file_name)

    character(len=*),intent(in) :: file_name,var_name
    integer(ik),intent(in) :: ndim
    character(len=*),intent(in) :: dim_name(ndim)

    real(rk) :: var(:,:,:)
    logical :: file_exist
    integer(ik) :: varid(1),i
    integer(ik) :: ncid,dim_len(ndim),dimid(ndim),dim_len_check

	!-- Check if file exist
	inquire(file=file_name,exist=file_exist)

	!-- Open/Create file
	if (file_exist) then
       call io_check(nf90_open(path=file_name,mode=nf90_write,ncid=ncid))
       call io_check(nf90_redef(ncid))
    else
       call io_check(nf90_create(path=file_name,cmode=nf90_clobber,ncid=ncid))
    endif

	!-- Create/add dimensions
	do i=1,ndim
    	if (nf90_inq_dimid(ncid,dim_name(i),dimid(i))/=nf90_noerr) then 
			call io_check(nf90_def_dim(ncid,dim_name(i),dim_len(i),dimid(i)))
		else
			call io_check(nf90_inquire_dimension(ncid,dimid(i),len=dim_len_check))
			if (dim_len_check/=dim_len(i)) call error_stop("NETCDF Error : Wrong dimensions")
		endif
	enddo

	!-- end of definition
	call io_check(nf90_enddef(ncid))

	!-- write field variable
	call io_check(nf90_put_var(ncid,varid(1),var,start=(/1,1,1/),count=get_dim_size(var)))

	!-- close file
	call io_check(nf90_close(ncid))

end subroutine write_var3d

! =========================================================================================
! == Write 3d variable in a netcdf file													 ==
! =========================================================================================



end module stdiocdf
