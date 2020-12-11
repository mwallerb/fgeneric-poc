



module fmmatrix
    ! matrix type
    type :: fmatrix
        real*4, allocatable :: buffer(:, :)
        integer :: order
    end type fmatrix
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
    ! matrix type
    type :: dmatrix
        real*8, allocatable :: buffer(:, :)
        integer :: order
    end type dmatrix
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
    ! matrix type
    type :: cmatrix
        complex*8, allocatable :: buffer(:, :)
        integer :: order
    end type cmatrix
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
    ! matrix type
    type :: zmatrix
        complex*16, allocatable :: buffer(:, :)
        integer :: order
    end type zmatrix
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
