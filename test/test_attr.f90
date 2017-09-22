program test_attr

    use pthread_f
    use iso_c_binding

    implicit none

    type(f_pthread_attr_t)  ::  attr

    integer :: ierr

    print *, '====================== test_attr ======================'
    attr%i = 'a'
    print*, 'attr=', attr%i
    ierr = pthread_attr_init(attr)
    print*, 'attr=', ichar(attr%i)
    if (ierr.ne.0) then
        print *,'error creating attr ',ierr
        stop
    else
        print *,'successfully created attr ',ierr
    end if

    ierr = pthread_attr_destroy(attr)
    print*, 'attr=', ichar(attr%i)
    if (ierr.ne.0) then
        print *,'error destroying attr ',ierr
        stop
    else
        print *,'successfully destroyed attr ',ierr
    end if

    print *, '==================== end test_attr ===================='

end program test_attr
