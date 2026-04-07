!> @file
!! @defgroup group_context Context
!! Source context tracking for diagnostics and error reporting in fpx.
!!
!! This small but critical module defines the `context` type used throughout the fpx
!! preprocessor to attach precise location information to every diagnostic message,
!! label, or error report.
!!
!! Every time the preprocessor encounters a problem (undefined macro, include not found,
!! invalid `#if` expression, unmatched `#endif`, etc.), it creates or reuses a `context`
!! object that carries:
!! - The full line content (for showing source snippets)
!! - The 1-based line number
!! - The full file path (for `-->` arrows in diagnostics)
!! - The base filename (for concise reporting when path is long)
!!
!! This information is then used by the `fpx_logging` module to produce modern,
!! rustc/clang-style error messages with line numbers, caret highlights (^), and
!! file references.
!!
!! Without accurate context, diagnostics would be vague ("macro not defined").
!! With `context`, users see exactly where the problem happened:
!!
!! ```
!! error: Undefined macro 'DEBUG'
!!   --> src/main.F90:42:10-15
!!    |
!! 42 |   #ifdef DEBUG
!!    |          ^^^^^ 'DEBUG' not defined
!! ```
!!
!! @par Typical usage examples
!!
!! 1. Creating context when reading a new file:
!! @code{.f90}
!!    type(context) :: ctx
!!
!!    ctx%path     = "/home/user/project/src/utils.F90"
!!    ctx%line     = 27
!!    ctx%content  = "real :: x = PI * r**2"
!!
!!    ! Now pass ctx to logging functions
!!    call report_undefined_macro("PI", ctx)
!! @endcode
!!
!! 2. Updating context during recursive #include processing:
!! @code{.f90}
!!    ! In handle_include
!!    type(context) :: included_ctx
!!
!!    included_ctx%path     = resolved_include_path
!!    included_ctx%line     = 1   ! reset for new file
!!    included_ctx%content  = first_line_of_included_file
!!
!!    ! Then use included_ctx for diagnostics inside the included file
!! @endcode
!!
!! 3. Attaching context to a diagnostic label:
!! @code{.f90}
!!    type(label_type) :: lbl
!!
!!    lbl = label_type(LEVEL_ERROR, "expected expression", &
!!                     line=ctx%line, first=8, last=12, primary=.true.)
!!
!!    diag = diagnostic_report(LEVEL_ERROR, "Invalid #if condition", ctx%path, [lbl])
!! @endcode
module fpx_context
    implicit none; private

    !> Source location and content snapshot for precise diagnostics
    !! Instances of this type are created for every source file (including nested
    !! #include files) and passed along with every warning, error, note, or info
    !! message to provide accurate line numbers, file names, and code snippets.
    !!
    !! <h2  class="groupheader">Examples</h2>
    !! @code{.f90}
    !!    type(context) :: ctx
    !!    ctx = context('lorem ipsum', __LINE__, 'myfile.f90')
    !!    ...
    !! @endcode
    !! <h2  class="groupheader">Remarks</h2>
    !! - Usually one `context` exists per currently processed file.
    !! - When entering an `#include`, a new context is created for the included file.
    !! - Helps produce high-quality, IDE-friendly error messages.
    !!
    !! <h2  class="groupheader">Constructors</h2>
    !! Initializes a new instance of the @ref context class
    !! <h3>context(character(*),  integer, character(*))</h3>
    !! @verbatim type(context) function context(character(*) content, integer line, character(*) path) @endverbatim
    !!
    !! @param[in] content The actual line of source code where the issue occurred
    !! @param[in] line 1-based line number in the current file
    !! @param[in] path Full absolute or relative path to the file
    !!
    !! @return The constructed datetime object.
    !!
    !! <h2  class="groupheader">Remarks</h2>
    !! @ingroup group_context
    type, public :: context
        character(:), allocatable   :: content
        integer                     :: line
        character(:), allocatable   :: path
    end type

end module
