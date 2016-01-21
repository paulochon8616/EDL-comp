module precision

	implicit none
	!-- Explicit visibility declaration
	private
	!-- Self-documentation
	public :: ik, rk

	!-- Integer kinds
	integer, parameter :: int1 = selected_int_kind(1)
	integer, parameter :: int2 = selected_int_kind(2)
	integer, parameter :: int4 = selected_int_kind(8)
	integer, parameter :: int8 = selected_int_kind(10)

	!-- Floating point kinds
	integer, parameter :: real4 = selected_real_kind(6)
	integer, parameter :: real8 = selected_real_kind(15)
	integer, parameter :: real16 = selected_real_kind(32)

	!-- Generics kinds
	integer, parameter :: ik = int4
	integer, parameter :: rk = real8

end module precision

