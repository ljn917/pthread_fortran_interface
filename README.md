1. Only pthread_attr_init, pthread_attr_destroy, pthread_create, pthread_join are tested at this stage.
2. May re-write void* as TYPE(*) if scalar, dimension(:) and dimension(..) can be handled elegantly.
3. Passing NULL to use the default attribute is tricky. Call c_f_pointer(c_null_ptr, attr) is possible but not tested.
