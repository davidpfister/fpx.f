module fpx_global
    use fpx_constants
    use fpx_logging
    use fpx_macro
    
    implicit none; private
    
    
    
    type, public :: global_settings
        private
        type(macro), allocatable, public :: macros(:)
        logical, public                  :: exlude_comments = .false.
    end type
    
    type(global_settings), public :: global
    
    contains
    
end module