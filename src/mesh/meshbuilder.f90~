module Meshbuilder
	use precision
	implicit none

	type gridm
		!-- Dimension grid 1d
		integer(ik) :: n
		!-- Dimensions grid 3d
		integer(ik) :: nx,ny,nz
		!-- 1d grid array
		real(rk),allocatable :: grid1d(:)
		!-- 3d grid array
		real(rk),allocatable :: grid3d(:,:,:)
		!-- spatial step
		real(rk) :: step
		!-- grid name
		character(len=512) :: gridn="canal_rectangulaire"
	end type gridm

contains

! =========================================================================================
! == Initialization and destruction methods												 ==
! =========================================================================================

!-- Allocate or reallocate 1d grid type mesh
subroutine grid1d_allocate(n,grid)

	implicit none
	integer, intent(in) :: n
	real(rk), allocatable :: grid(:)

	if (.not.allocated(grid)) then
		allocate(grid(n))
	elseif (allocated(grid)) then
		deallocate(grid)
		allocate(grid(n))
	endif
	
end subroutine grid1d_allocate

!-- Allocate or reallocate 3d grid type mesh
subroutine grid3d_allocate(n1,n2,n3,grid)
	
	implicit none
	integer, intent(in) :: n1,n2,n3
	real(rk),allocatable :: grid(:,:,:)
	
	if (.not.allocated(grid)) then
		allocate(grid(n1,n2,n3))
	elseif (allocated(grid)) then
		deallocate(grid)
		allocate(grid(n1,n2,n3))
	endif

end subroutine grid3d_allocate

!-- Initialyze type mesh
subroutine mesh_init(grid,gn,choice,nx,ny,nz)

	implicit none
	integer(ik), intent(in) :: nx,ny,nz
	type(gridm), intent(inout) :: grid
	character(len=*), intent(in) :: gn
	character(len=*), intent(in) :: choice

	!-- Dimension length of grid 1d
	if (choice=="x") grid%n=nx
	if (choice=="y") grid%n=ny
	if (choice=="z") grid%n=nz

	!-- Dimensions length of grid 3d
	grid%nx = nx
	grid%ny = ny
	grid%nz = nz

	!-- Grid name
	grid%gridn=gn

	!-- Allocate grid1d
	call grid1d_allocate(grid%n,grid%grid1d)

	!-- Allocate grid3d
	call grid3d_allocate(grid%nx,grid%ny,grid%nz,grid%grid3d)

end subroutine mesh_init

!-- Initialyze grid mesh
subroutine mesh_grid_init(grid,choice,ds,q,xlength,ylength,zlength)

	implicit none
	real(rk), intent(in) :: ds,q,xlength,ylength,zlength
	type(gridm), intent(inout) :: grid
	character(len=*), intent(in) :: choice
	integer(ik) :: i,j,k
	real(rk) :: dx,dy,dz,xi,yj,kz,ynode

	dx = xlength/grid%nx
	dz = zlength/grid%nz
		
	if (choice=="x") then
		step = xlength
	endif	 

	if (choice=="y") then

	endif

	if (choice=="z") then

	endif

end subroutine mesh_grid_init

end module Meshbuilder
