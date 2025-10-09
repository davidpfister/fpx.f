module fpx_global
    use fpx_constants
    use fpx_string
    use fpx_logging
    use fpx_macro
    
    implicit none; private
    
    
    
    type, public :: global_settings
        private
        type(macro), allocatable, public    :: macros(:)
        type(string), allocatable, public   :: undef(:)
        type(string), allocatable, public   :: includedir(:)
        logical, public                     :: expand_macros = .true.
        logical, public                     :: exlude_comments = .false.
    end type
    
    type(global_settings), public :: global
    
    contains
    
end module