



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




module fmmatrix
    use mmatrix_base

    ! matrix type
    type, extends(matrix) :: fmatrix
        real*4, allocatable :: buffer(:, :)
        integer :: order
    end type fmatrix

    interface matrix_resize
        module procedure fmatrix_resize
    end interface

contains
    subroutine fmatrix_init(this)
        type(fmatrix), intent(inout) :: this
        integer, parameter :: INIT_SIZE = 10

        this%order = 0
        allocate(this%buffer(INIT_SIZE, INIT_SIZE))
    end subroutine

    subroutine fmatrix_resize(this, n)
        type(fmatrix), intent(inout) :: this
        real*4, allocatable :: tmp(:,:)
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
end module fmmatrix



module dmmatrix
    use mmatrix_base

    ! matrix type
    type, extends(matrix) :: dmatrix
        real*8, allocatable :: buffer(:, :)
        integer :: order
    end type dmatrix

    interface matrix_resize
        module procedure dmatrix_resize
    end interface

contains
    subroutine dmatrix_init(this)
        type(dmatrix), intent(inout) :: this
        integer, parameter :: INIT_SIZE = 10

        this%order = 0
        allocate(this%buffer(INIT_SIZE, INIT_SIZE))
    end subroutine

    subroutine dmatrix_resize(this, n)
        type(dmatrix), intent(inout) :: this
        real*8, allocatable :: tmp(:,:)
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
end module dmmatrix



module cmmatrix
    use mmatrix_base

    ! matrix type
    type, extends(matrix) :: cmatrix
        complex*8, allocatable :: buffer(:, :)
        integer :: order
    end type cmatrix

    interface matrix_resize
        module procedure cmatrix_resize
    end interface

contains
    subroutine cmatrix_init(this)
        type(cmatrix), intent(inout) :: this
        integer, parameter :: INIT_SIZE = 10

        this%order = 0
        allocate(this%buffer(INIT_SIZE, INIT_SIZE))
    end subroutine

    subroutine cmatrix_resize(this, n)
        type(cmatrix), intent(inout) :: this
        complex*8, allocatable :: tmp(:,:)
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
end module cmmatrix



module zmmatrix
    use mmatrix_base

    ! matrix type
    type, extends(matrix) :: zmatrix
        complex*16, allocatable :: buffer(:, :)
        integer :: order
    end type zmatrix

    interface matrix_resize
        module procedure zmatrix_resize
    end interface

contains
    subroutine zmatrix_init(this)
        type(zmatrix), intent(inout) :: this
        integer, parameter :: INIT_SIZE = 10

        this%order = 0
        allocate(this%buffer(INIT_SIZE, INIT_SIZE))
    end subroutine

    subroutine zmatrix_resize(this, n)
        type(zmatrix), intent(inout) :: this
        complex*16, allocatable :: tmp(:,:)
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
end module zmmatrix
