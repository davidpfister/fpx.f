!> @file
!! @defgroup group_context Context
!! Source context information used for diagnostics and error reporting.
!!
!! This module defines the lightweight @ref fpx_context::context type used throughout
!! the fpx preprocessor to associate source-location information with
!! diagnostics, warnings, notes, and error messages.
!!
!! Every diagnostic emitted by fpx is accompanied by a context object
!! describing where the event occurred. This enables the generation of
!! modern compiler-style messages containing file names, line numbers,
!! source snippets, and caret annotations.
!!
!! A context captures:
!!
!! - the original source line,
!! - the corresponding 1-based line number,
!! - the path of the source file being processed.
!!
!! The information stored in a context is consumed primarily by the
!! @link fpx_logging fpx_logging @endlink module to produce precise and user-friendly diagnostics.
!!
!! For example:
!!
!! @code
!! error: Undefined macro 'DEBUG'
!! --> src/main.F90:42
!!    |
!! 42 | #ifdef DEBUG
!!    | ^^^^^ not defined
!! @endcode
!!
!! Accurate context information becomes particularly important when
!! processing nested #include files, evaluating conditional directives,
!! or reporting errors originating from macro expansions.
!!
!! @section context_examples Examples
!!
!! 1. Creating a context object:
!! @code{.f90}
!! type(context) :: ctx
!!
!! ctx = context( &
!! content='real :: x = PI*r**2', &
!! line=27, &
!! path='src/utils.F90')
!! ...
!! @endcode
!!
!! 2. Using context when reporting diagnostics:
!! @code{.f90}
!! call printf(render(diagnostic_report( &
!! LEVEL_ERROR, &
!! message='Undefined macro', &
!! source=ctx%path), &
!! ctx%content, ctx%line))
!! ...
!! @endcode
!!
!! 3. Updating context during #include processing:
!! @code{.f90}
!! included_ctx = context( &
!! content=first_line, &
!! line=1, &
!! path=resolved_include_path)
!! ...
!! @endcode
module fpx_context
    implicit none; private

    !> Snapshot of a source location within the preprocessing stream. 
    !! 
    !! Instances of this type accompany diagnostics throughout fpx and 
    !! provide the information required to identify where an event 
    !! occurred in the original source. 
    !! 
    !! The stored line content is typically displayed alongside 
    !! highlighted regions when rendering diagnostics. 
    !! 
    !! @section context_type_examples Examples 
    !! @code{.f90} 
    !! type(context) :: ctx 
    !! 
    !! ctx = context('lorem ipsum', 42, 'example.F90')
    !! ...
    !! @endcode 
    !! 
    !! @section context_type_remarks Remarks
    !! - A new context is typically created for each processed source line. 
    !! - Entering an `#include` file naturally creates contexts referring 
    !! to the included file. 
    !! - Context objects are lightweight and inexpensive to copy. 
    !! - They form the foundation of fpx's compiler-style diagnostics. 
    !! 
    !! @section context_type_constructors Constructors
    !! 
    !! Initializes a new instance of the @ref context type. 
    !!
    !! @b Constructor
    !! @code{.f90} 
    !! type(context) function context(character(*) content, integer line, character(*) path) 
    !! @endcode 
    !! 
    !! @param[in] content 
    !!   Source line associated with the diagnostic. 
    !! @param[in] line 
    !!   One-based line number within the source file. 
    !! @param[in] path 
    !!   Relative or absolute path of the source file. 
    !! 
    !! @return Newly constructed context object. 
    !! 
    !! @ingroup group_context
    type, public :: context
        character(:), allocatable   :: content
        integer                     :: line
        character(:), allocatable   :: path
    end type

end module
