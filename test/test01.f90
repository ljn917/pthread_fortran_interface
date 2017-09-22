module procmod
contains
    function example(arg) bind(c) result(res)
        use iso_c_binding
        implicit none
        type(c_ptr), intent(in), value :: arg
        type(c_ptr)             :: res

        integer, pointer        ::  arg_fptr

        call c_f_pointer(arg, arg_fptr)

        print *,'hello world',arg_fptr
        
        res = c_loc(arg_fptr)

    end function example
end module procmod

program test01

    use procmod
    use pthread_f
    use iso_c_binding
    use iso_fortran_env

    implicit none

    integer, parameter                  :: n = 10
    integer(f_pthread_t), dimension(n)  :: tid
    integer, dimension(n), target       :: arg
    type(f_pthread_attr_t)              :: attr

    integer ::  i
    integer :: ierr
    integer, pointer    ::  ret
    type(c_ptr) ::  ret_cptr

    print *, '====================== test01 ======================'
    ierr = pthread_attr_init(attr)
    if (ierr.ne.0) then
        print *,'error creating attr ',ierr
        stop
    else
        print *,'successfully created attr ',ierr
    end if

    do, i = 1, n
        arg(i) = i**2
        !ierr = pthread_create(tid(i),attr,c_funloc(example),c_loc(arg(i)))
        ierr = pthread_create(tid(i),attr,example,c_loc(arg(i)))
        if (ierr.ne.0) then
            print *,'error creating thread ',i
            stop 'ERROR:pthread_create'
        else
            print *,'successfully created thread ',i, 'threadid', tid(i)
        end if
        flush(output_unit)
    end do

    do, i = 1, n
        print *,'join thread',i
        ierr = pthread_join(tid(i), ret_cptr)
        call c_f_pointer(ret_cptr, ret)
        print*, 'i=', i, 'ret=', ret
        if (ierr.ne.0) then
            print *,'error joining thread ',i
        end if
    end do

    ierr = pthread_attr_destroy(attr)
    if (ierr.ne.0) then
        print *,'error destroying attr ',ierr
        stop
    else
        print *,'successfully destroyed attr ',ierr
    end if

    print *, '==================== end test01 ===================='


end program test01
