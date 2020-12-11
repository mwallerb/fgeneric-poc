#ifdef MAKE_BASE

! Module providing type(matrix) and specializations for each type
module mmatrix
    use mmatrix_base
    use fmmatrix
    use dmmatrix
    use cmmatrix
    use zmmatrix
end

module mmatrix_base
    type :: matrix
    end type

    interface matrix_resize
    end interface
end module mmatrix_base

#else
#include "defines.f90"

module GENERIC(mmatrix)
    use mmatrix_base

    ! matrix type
    type, extends(matrix) :: GENERIC(matrix)
        VALUE_TYPE, allocatable :: buffer(:, :)
        integer :: order
    end type GENERIC(matrix)

    interface matrix_resize
        module procedure GENERIC(matrix_resize)
    end interface

contains
    subroutine GENERIC(matrix_init)(this)
        type(GENERIC(matrix)), intent(inout) :: this
        integer, parameter :: INIT_SIZE = 10

        this%order = 0
        allocate(this%buffer(INIT_SIZE, INIT_SIZE))
    end subroutine

    subroutine GENERIC(matrix_resize)(this, n)
        type(GENERIC(matrix)), intent(inout) :: this
        VALUE_TYPE, allocatable :: tmp(:,:)
        integer, parameter :: EXTRA = 2

        if (.not. allocated(this%buffer)) &
            stop 'matrix is not initialized'
        if (n < 0) &
            stop 'n must be non-negative'

        if (n < this%order) then
            this%order = n
        else
            allocate(tmp(n + EXTRA, n + EXTRA))
            tmp(:this%order, :this%order) = this%buffer
            call move_alloc(tmp, this%buffer)
            this%order = n
        end if
    end subroutine
end module GENERIC(mmatrix)
#endif
