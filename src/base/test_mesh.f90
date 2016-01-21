program test_mesh

! =========================================================================================
! == Modules declaration																 ==
! =========================================================================================
	use meshbuilder

	implicit none
	integer(ik) :: i,j,k
	type(gridm) :: grid

	!call mesh_init(grid,'test-maillage',6,6,6)
	!call mesh_grid_init(grid,5.0d-9,5.0d-2,6.0d-3,2.0d-3,6.0d-5)
!
!	do k=1,grid%nz
!		print *,\n
!		do j=1,grid%ny
!			print *,\n
!			do i=1,grid%nx
!				print *,grid%grid3d(i,j,k)
!			enddo
!		enddo
!	enddo

end program test_mesh


