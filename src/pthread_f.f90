module pthread_f

use, intrinsic  ::  iso_c_binding

implicit none

#include "macro.h"

! type defined in bits/wordsize.h
! Need to check __x86_64__ and __WORDSIZE
! ASSUME __x86_64__ and __WORDSIZE==64 and Linux gfortran

!  #define __SIZEOF_PTHREAD_ATTR_T 56
!  #define __SIZEOF_PTHREAD_MUTEX_T 40
!  #define __SIZEOF_PTHREAD_MUTEXATTR_T 4
!  #define __SIZEOF_PTHREAD_COND_T 48
!  #define __SIZEOF_PTHREAD_CONDATTR_T 4
!  #define __SIZEOF_PTHREAD_RWLOCK_T 56
!  #define __SIZEOF_PTHREAD_RWLOCKATTR_T 8
!  #define __SIZEOF_PTHREAD_BARRIER_T 32
!  #define __SIZEOF_PTHREAD_BARRIERATTR_T 4

integer, parameter  ::  f_pthread_t = c_long        !unsigned long int
integer, parameter  ::  f_pthread_key_t = c_int     ! unsigned int
integer, parameter  ::  f_pthread_once_t = c_int    ! int
integer, parameter  ::  f_pthread_spinlock_t = c_int  !VOLATILE

type, bind(c)   ::  f_pthread_attr_t
    character(kind=c_char), dimension(__SIZEOF_PTHREAD_ATTR_T)          ::  i = char(0, kind=c_char)    ! not the default value
end type f_pthread_attr_t

type, bind(c)   ::  f_pthread_mutex_t
    character(kind=c_char), dimension(__SIZEOF_PTHREAD_MUTEX_T)         ::  i = char(0, kind=c_char)    ! PTHREAD_MUTEX_INITIALIZER
end type f_pthread_mutex_t

type, bind(c)   ::  f_pthread_mutexattr_t
    character(kind=c_char), dimension(__SIZEOF_PTHREAD_MUTEXATTR_T)     ::  i = char(0, kind=c_char)    ! not the default value
end type f_pthread_mutexattr_t

type, bind(c)   ::  f_pthread_cond_t
    character(kind=c_char), dimension(__SIZEOF_PTHREAD_COND_T)          ::  i = char(0, kind=c_char)    ! PTHREAD_COND_INITIALIZER
end type f_pthread_cond_t

type, bind(c)   ::  f_pthread_condattr_t
    character(kind=c_char), dimension(__SIZEOF_PTHREAD_CONDATTR_T)      ::  i = char(0, kind=c_char)    ! not the default value
end type f_pthread_condattr_t

type, bind(c)   ::  f_pthread_rwlock_t
    character(kind=c_char), dimension(__SIZEOF_PTHREAD_RWLOCK_T)        ::  i = char(0, kind=c_char)    ! PTHREAD_RWLOCK_INITIALIZER
end type f_pthread_rwlock_t

type, bind(c)   ::  f_pthread_rwlockattr_t
    character(kind=c_char), dimension(__SIZEOF_PTHREAD_RWLOCKATTR_T)    ::  i = char(0, kind=c_char)    ! not the default value
end type f_pthread_rwlockattr_t

type, bind(c)   ::  f_pthread_barrier_t
    character(kind=c_char), dimension(__SIZEOF_PTHREAD_BARRIER_T)       ::  i = char(0, kind=c_char)    ! not the default value
end type f_pthread_barrier_t

type, bind(c)   ::  f_pthread_barrierattr_t
    character(kind=c_char), dimension(__SIZEOF_PTHREAD_BARRIERATTR_T)   ::  i = char(0, kind=c_char)    ! not the default value
end type f_pthread_barrierattr_t


integer(f_pthread_once_t), parameter    ::  pthread_once_init = PTHREAD_ONCE_INIT ! PTHREAD_ONCE_INIT is a macro
type(f_pthread_mutex_t), parameter      ::  pthread_mutex_initializer = f_pthread_mutex_t(char(0, kind=c_char))
type(f_pthread_cond_t), parameter       ::  pthread_cond_initializer = f_pthread_cond_t(char(0, kind=c_char))
type(f_pthread_rwlock_t), parameter     ::  pthread_rwlock_initializer = f_pthread_rwlock_t(char(0, kind=c_char))

! dummy argument
!void* matches TYPE(C_PTR), VALUE;  void** matches TYPE(C_PTR) 

!int pthread_create (pthread_t *__restrict __newthread,
!			   const pthread_attr_t *__restrict __attr,
!			   void *(*__start_routine) (void *),
!			   void *__restrict __arg)
interface
    function pthread_create(thread_id,attr,start_routine,arg) bind(c,name="pthread_create") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_t
        import  ::  f_pthread_attr_t
        integer(f_pthread_t), intent(out)       :: thread_id
        type(f_pthread_attr_t), intent(in)      :: attr
        !type(c_funptr), intent(in), value       :: start_routine
        abstract interface
            function start_routine(arg) bind(c) result(res)
                use iso_c_binding
                type(c_ptr), intent(in), value  ::  arg
                type(c_ptr) ::  res
            end function start_routine
        end interface
        type(c_ptr), intent(in), value                 :: arg
        integer(c_int)                          :: ierr
    end function pthread_create
end interface

!int pthread_join (pthread_t __th, void **__thread_return);
interface
    function pthread_join(thread_id,value_ptr) bind(c,name="pthread_join") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_t
        integer(f_pthread_t), intent(in), value :: thread_id
        type(c_ptr), intent(out)                :: value_ptr
        integer(c_int)                          :: ierr
    end function pthread_join
end interface

!pthread_t pthread_self (void)
interface
    function pthread_self() bind(c,name="pthread_self") result(thread_id)
        use iso_c_binding
        import  ::  f_pthread_t
        integer(f_pthread_t)                    :: thread_id
    end function pthread_self
end interface

!int pthread_detach (pthread_t __th)
interface
    function pthread_detach(thread_id) bind(c,name="pthread_detach") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_t
        integer(f_pthread_t), intent(in), value :: thread_id
        integer(c_int)                          :: ierr
    end function pthread_detach
end interface

!int pthread_equal (pthread_t __thread1, pthread_t __thread2)
interface
    function pthread_equal(thread_id1, thread_id2) bind(c,name="pthread_equal") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_t
        integer(f_pthread_t), intent(in), value :: thread_id1, thread_id2
        integer(c_int)                          :: ierr
    end function pthread_equal
end interface

! attr

!int pthread_attr_init (pthread_attr_t *__attr)
interface
    function pthread_attr_init(attr) bind(c,name="pthread_attr_init") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_attr_t
        type(f_pthread_attr_t), intent(inout)   :: attr
        integer(c_int)                          :: ierr
    end function pthread_attr_init
end interface

!int pthread_attr_destroy (pthread_attr_t *__attr);
interface
    function pthread_attr_destroy(attr) bind(c,name="pthread_attr_destroy") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_attr_t
        type(f_pthread_attr_t), intent(inout)     :: attr
        integer(c_int)                          :: ierr
    end function pthread_attr_destroy
end interface

!int pthread_attr_getdetachstate (const pthread_attr_t *__attr, int *__detachstate)
interface
    function pthread_attr_getdetachstate(attr, detachstate) &
                        bind(c,name="pthread_attr_getdetachstate") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_attr_t
        type(f_pthread_attr_t), intent(in)      :: attr
        integer(c_int), intent(out)             :: detachstate
        integer(c_int)                          :: ierr
    end function pthread_attr_getdetachstate
end interface

!int pthread_attr_setdetachstate (pthread_attr_t *__attr, int __detachstate)
interface
    function pthread_attr_setdetachstate(attr, detachstate) &
                        bind(c,name="pthread_attr_setdetachstate") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_attr_t
        type(f_pthread_attr_t), intent(inout)   :: attr
        integer(c_int), intent(in), value       :: detachstate
        integer(c_int)                          :: ierr
    end function pthread_attr_setdetachstate
end interface

! Guarantee that the initialization function INIT_ROUTINE will be called
! only once, even if pthread_once is executed several times with the
! same ONCE_CONTROL argument. ONCE_CONTROL must point to a static or
! extern variable initialized to PTHREAD_ONCE_INIT.
!int pthread_once (pthread_once_t *__once_control, void (*__init_routine) (void))
interface
    function pthread_once(once_control, init_routine) bind(c,name="pthread_once") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_once_t
        integer(f_pthread_once_t), intent(inout)  :: once_control
        abstract interface
            subroutine init_routine() bind(c)
            end subroutine init_routine
        end interface
        integer(c_int)                          :: ierr
    end function pthread_once
end interface

!Mutex handling
!int pthread_mutex_init (pthread_mutex_t *__mutex, const pthread_mutexattr_t *__mutexattr)
interface
    function pthread_mutex_init(mutex, mutexattr) &
                        bind(c,name="pthread_mutex_init") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_mutex_t, f_pthread_mutexattr_t
        type(f_pthread_mutex_t), intent(inout)  :: mutex
        type(f_pthread_mutexattr_t), intent(in) :: mutexattr
        integer(c_int)                          :: ierr
    end function pthread_mutex_init
end interface

!int pthread_mutex_destroy (pthread_mutex_t *__mutex)
interface
    function pthread_mutex_destroy(mutex) &
                        bind(c,name="pthread_mutex_destroy") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_mutex_t
        type(f_pthread_mutex_t), intent(inout)  :: mutex
        integer(c_int)                          :: ierr
    end function pthread_mutex_destroy
end interface

!int pthread_mutex_trylock (pthread_mutex_t *__mutex)
interface
    function pthread_mutex_trylock(mutex) &
                        bind(c,name="pthread_mutex_trylock") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_mutex_t
        type(f_pthread_mutex_t), intent(inout)  :: mutex
        integer(c_int)                          :: ierr
    end function pthread_mutex_trylock
end interface

!int pthread_mutex_lock (pthread_mutex_t *__mutex)
interface
    function pthread_mutex_lock(mutex) &
                        bind(c,name="pthread_mutex_lock") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_mutex_t
        type(f_pthread_mutex_t), intent(inout)  :: mutex
        integer(c_int)                          :: ierr
    end function pthread_mutex_lock
end interface

!int pthread_mutex_timedlock (pthread_mutex_t *__restrict __mutex,
!				    const struct timespec *__restrict __abstime)

!int pthread_mutex_unlock (pthread_mutex_t *__mutex)
interface
    function pthread_mutex_unlock(mutex) &
                        bind(c,name="pthread_mutex_unlock") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_mutex_t
        type(f_pthread_mutex_t), intent(inout)  :: mutex
        integer(c_int)                          :: ierr
    end function pthread_mutex_unlock
end interface



!int pthread_mutexattr_init (pthread_mutexattr_t *__attr)
interface
    function pthread_mutexattr_init(attr) &
                        bind(c,name="pthread_mutexattr_init") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_mutexattr_t
        type(f_pthread_mutexattr_t), intent(inout)  :: attr
        integer(c_int)                              :: ierr
    end function pthread_mutexattr_init
end interface

!int pthread_mutexattr_destroy (pthread_mutexattr_t *__attr)
interface
    function pthread_mutexattr_destroy(attr) &
                        bind(c,name="pthread_mutexattr_destroy") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_mutexattr_t
        type(f_pthread_mutexattr_t), intent(inout)  :: attr
        integer(c_int)                              :: ierr
    end function pthread_mutexattr_destroy
end interface

! RW lock
!int pthread_rwlock_init (pthread_rwlock_t *__restrict __rwlock,
!				const pthread_rwlockattr_t *__restrict __attr)
interface
    function pthread_rwlock_init(rwlock, attr) &
                        bind(c,name="pthread_rwlock_init") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_rwlock_t, f_pthread_rwlockattr_t
        type(f_pthread_rwlock_t), intent(inout)     :: rwlock
        type(f_pthread_rwlockattr_t), intent(in)    :: attr
        integer(c_int)                              :: ierr
    end function pthread_rwlock_init
end interface

!int pthread_rwlock_destroy (pthread_rwlock_t *__rwlock)
interface
    function pthread_rwlock_destroy(rwlock) &
                        bind(c,name="pthread_rwlock_destroy") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_rwlock_t
        type(f_pthread_rwlock_t), intent(inout)     :: rwlock
        integer(c_int)                              :: ierr
    end function pthread_rwlock_destroy
end interface

!int pthread_rwlock_rdlock (pthread_rwlock_t *__rwlock)
interface
    function pthread_rwlock_rdlock(rwlock) &
                        bind(c,name="pthread_rwlock_rdlock") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_rwlock_t
        type(f_pthread_rwlock_t), intent(inout)     :: rwlock
        integer(c_int)                              :: ierr
    end function pthread_rwlock_rdlock
end interface

!int pthread_rwlock_tryrdlock (pthread_rwlock_t *__rwlock)
interface
    function pthread_rwlock_tryrdlock(rwlock) &
                        bind(c,name="pthread_rwlock_tryrdlock") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_rwlock_t
        type(f_pthread_rwlock_t), intent(inout)     :: rwlock
        integer(c_int)                              :: ierr
    end function pthread_rwlock_tryrdlock
end interface

!int pthread_rwlock_wrlock (pthread_rwlock_t *__rwlock)
interface
    function pthread_rwlock_wrlock(rwlock) &
                        bind(c,name="pthread_rwlock_wrlock") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_rwlock_t
        type(f_pthread_rwlock_t), intent(inout)     :: rwlock
        integer(c_int)                              :: ierr
    end function pthread_rwlock_wrlock
end interface

!int pthread_rwlock_trywrlock (pthread_rwlock_t *__rwlock)
interface
    function pthread_rwlock_trywrlock(rwlock) &
                        bind(c,name="pthread_rwlock_trywrlock") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_rwlock_t
        type(f_pthread_rwlock_t), intent(inout)     :: rwlock
        integer(c_int)                              :: ierr
    end function pthread_rwlock_trywrlock
end interface

!int pthread_rwlock_unlock (pthread_rwlock_t *__rwlock)
interface
    function pthread_rwlock_unlock(rwlock) &
                        bind(c,name="pthread_rwlock_unlock") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_rwlock_t
        type(f_pthread_rwlock_t), intent(inout)     :: rwlock
        integer(c_int)                              :: ierr
    end function pthread_rwlock_unlock
end interface

!int pthread_rwlockattr_init (pthread_rwlockattr_t *__attr)
interface
    function pthread_rwlockattr_init(attr) &
                        bind(c,name="pthread_rwlockattr_init") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_rwlockattr_t
        type(f_pthread_rwlockattr_t), intent(inout) :: attr
        integer(c_int)                              :: ierr
    end function pthread_rwlockattr_init
end interface

!int pthread_rwlockattr_destroy (pthread_rwlockattr_t *__attr)
interface
    function pthread_rwlockattr_destroy(attr) &
                        bind(c,name="pthread_rwlockattr_destroy") result(ierr)
        use iso_c_binding
        import  ::  f_pthread_rwlockattr_t
        type(f_pthread_rwlockattr_t), intent(inout) :: attr
        integer(c_int)                              :: ierr
    end function pthread_rwlockattr_destroy
end interface

end module pthread_f
