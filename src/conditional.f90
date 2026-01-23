!> @file
!! @defgroup group_conditional Conditional
!! Full-featured conditional compilation (#if / #ifdef / #else / #endif) for the fpx preprocessor
!! This module implements standard-conforming conditional compilation with support for:
!! - `#if` with arbitrary constant expressions (using `evaluate_expression`)
!! - `#ifdef` / `#ifndef` for testing macro existence
!! - `#elif` chains (multiple alternative branches)
!! - `#else` as final fallback
!! - Proper nesting up to `MAX_COND_DEPTH` levels
!! - Correct "first-match" semantics — once a branch is taken, later `#elif`/`#else` are skipped
!! - Integration with macro expansion via `is_defined()` and expression evaluator
!! - Comprehensive diagnostics when `verbose = .true.`
!!
!! The state is maintained in a global stack (`cond_stack`) with `cond_depth` tracking nesting.
!! The function `is_active()` is used throughout the preprocessor to decide whether a line
!! should be emitted or skipped.
!!
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Classic include guard pattern:
!! @code{.f90}
!!  #ifndef MY_HEADER_H
!!  #define MY_HEADER_H
!!
!!  ! ... header content ...
!!
!!  #endif
!!  ...
!! @endcode
!!
!! 2. Feature selection with #if and #elif:
!! @code{.f90}
!!  #if DEBUG >= 2
!!     print *, 'Extra verbose debugging enabled'
!!  #elif DEBUG == 1
!!     print *, 'Standard debugging'
!!  #else
!!     ! Silent mode
!!  #endif
!!  !..
!! @endcode
!!
!! 3. Cross-platform code selection:
!! @code{.f90}
!!  #ifdef _OPENMP
!!     use omp_lib
!!  #else
!!     integer, parameter :: omp_get_thread_num = 0
!!  #endif
!!  ...
!! @endcode
!!
!! 4. Complex expression in #if (requires `evaluate_expression` support):
!! @code{.f90}
!!  #if defined(USE_MPI) && (MPI_VERSION >= 3)
!!     use mpi_f08
!!  #endif
!!  ...
!! @endcode
module fpx_conditional
    use fpx_constants
    use fpx_logging
    use fpx_string
    use fpx_macro, only: macro, is_defined
    use fpx_token, only: evaluate_expression

    implicit none; private

    public :: handle_if, &
              handle_ifdef, &
              handle_ifndef, &
              handle_elif, &
              handle_else, &
              handle_endif, &
              is_active

    !> State of a single conditional block
    !! <h2  class="groupheader">Constructors</h2>
    !! Initializes a new instance of the @ref cond_state type
    !! <h3>cond_state(logical, logical)</h3>
    !! @verbatim type(cond_state) function cond_state(logical active, logical has_met) @endverbatim
    !!
    !! @param[in] active whether code in this block should be emitted
    !! @param[in] has_met whether a true branch has already been taken at this nesting level
    !!
    !! @return The constructed @ref cond_state object.
    !!
    !! <h2  class="groupheader">Remarks</h2>
    !! @ingroup group_conditional
    type, public :: cond_state
        logical :: active  !< Indicate whether the condition is active
        logical :: has_met  !< Indicates whether the condition has been met
    end type

    !> @brief Global stack of conditional states (depth-limited)
    !! @ingroup group_conditional
    type(cond_state), public :: cond_stack(MAX_COND_DEPTH)

    !> @brief Current nesting depth of conditional directives (0 = outside any #if)
    !! @ingroup group_conditional
    integer, public :: cond_depth = 0

contains

    !> Determine if current line is inside an active conditional block
    !! @return .true. if all enclosing #if/#elif/#else branches are active
    !!
    !! @b Remarks
    !! @ingroup group_conditional
    logical function is_active() result(res)
        integer :: i
        res = .true.
        do i = 1, cond_depth + 1
            if (.not. cond_stack(i)%active) then
                res = .false.
                exit
            end if
        end do
    end function

    !> Process a #if directive with constant expression evaluation
    !! Evaluates the expression after #if using `evaluate_expression()` and pushes
    !! a new state onto the conditional stack.
    !! @param[in] line      Full source line containing the directive
    !! @param[in] filename  Current file (for error messages)
    !! @param[in] line_num  Line number (for error messages)
    !! @param[in] macros    Current macro table
    !! @param[in] token     Usually 'IF'
    !!
    !! @b Remarks
    !! @ingroup group_conditional
    subroutine handle_if(line, filename, line_num, macros, token)
        character(*), intent(in)    :: line, filename
        integer, intent(in)         :: line_num
        type(macro), intent(in)     :: macros(:)
        character(*), intent(in)    :: token
        !private
        character(:), allocatable :: expr
        logical :: result, parent_active
        integer :: pos

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            if (verbose) print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        pos = index(uppercase(line), token) + len(token)
        expr = trim(adjustl(line(pos:)))
        if (verbose) print *, "Evaluating #if: '", trim(expr), "'"
        result = evaluate_expression(expr, macros)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = result .and. parent_active
        cond_stack(cond_depth + 1)%has_met = result
        if (verbose) print *, "#if result: ", result, ", cond_depth = ", cond_depth, ", active = ", cond_stack(cond_depth + 1)%active
    end subroutine

    !> Process #ifdef – test if a macro is defined
    !! @param[in] line      Full source line containing the directive
    !! @param[in] filename  Current file (for error messages)
    !! @param[in] line_num  Line number (for error messages)
    !! @param[in] macros    Current macro table
    !! @param[in] token     Usually 'IFDEF'
    !!
    !! @b Remarks
    !! @ingroup group_conditional
    subroutine handle_ifdef(line, filename, line_num, macros, token)
        character(*), intent(in)        :: line
        character(*), intent(in)        :: filename
        integer, intent(in)             :: line_num
        type(macro), intent(in)         :: macros(:)
        character(*), intent(in)        :: token
        !private
        character(:), allocatable :: name
        logical :: defined, parent_active
        integer :: pos

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            if (verbose) print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        pos = index(uppercase(line), token) + len(token)
        name = trim(adjustl(line(pos:)))
        defined = is_defined(name, macros)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = defined .and. parent_active
        cond_stack(cond_depth + 1)%has_met = defined
    end subroutine

    !> Process #ifndef – test if a macro is NOT defined
    !! @param[in] line      Full source line containing the directive
    !! @param[in] filename  Current file (for error messages)
    !! @param[in] line_num  Line number (for error messages)
    !! @param[in] macros    Current macro table
    !! @param[in] token     Usually 'IFNDEF'
    !!
    !! @b Remarks
    !! @ingroup group_conditional
    subroutine handle_ifndef(line, filename, line_num, macros, token)
        character(*), intent(in)        :: line
        character(*), intent(in)        :: filename
        integer, intent(in)             :: line_num
        type(macro), intent(in)         :: macros(:)
        character(*), intent(in)        :: token
        !private
        character(:), allocatable :: name
        logical :: defined, parent_active
        integer :: pos

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            if (verbose) print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        pos = index(uppercase(line), token) + len(token)
        name = trim(adjustl(line(pos:)))
        defined = is_defined(name, macros)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = (.not. defined) .and. parent_active
        cond_stack(cond_depth + 1)%has_met = .not. defined
    end subroutine

    !> Process #elif – alternative branch after #if/#elif
    !! Only activates if no previous branch in the group was taken.
    !! @param[in] line      Full source line containing the directive
    !! @param[in] filename  Current file (for error messages)
    !! @param[in] line_num  Line number (for error messages)
    !! @param[in] macros    Current macro table
    !! @param[in] token     Usually 'ELIF'
    !!
    !! @b Remarks
    !! @ingroup group_conditional
    subroutine handle_elif(line, filename, line_num, macros, token)
        character(*), intent(in)    :: line, filename
        integer, intent(in)         :: line_num
        type(macro), intent(in)     :: macros(:)
        character(*), intent(in)    :: token
        !private
        character(:), allocatable :: expr
        logical :: result, parent_active
        integer :: pos

        if (cond_depth == 0) then
            if (verbose) print *, "Error: #elif without matching #if at ", trim(filename), ":", line_num
            return
        end if

        pos = index(uppercase(line), token) + len(token)
        expr = trim(adjustl(line(pos:)))
        result = evaluate_expression(expr, macros)
        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = result .and. parent_active
            if (result) cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
    end subroutine

    !> Process #else – final fallback branch
    !! Activates only if no previous #if/#elif branch was true.
    !! @param[in] filename  Current file (for error messages)
    !! @param[in] line_num  Line number (for error messages)
    !!
    !! @b Remarks
    !! @ingroup group_conditional
    subroutine handle_else(filename, line_num)
        character(*), intent(in) :: filename
        integer, intent(in) :: line_num
        logical :: parent_active

        if (cond_depth == 0) then
            if (verbose) print *, "Error: #else without matching #if at ", trim(filename), ":", line_num
            return
        end if

        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = parent_active
            cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
        if (verbose) print *, "#else at depth ", cond_depth, ", active = ", cond_stack(cond_depth + 1)%active
    end subroutine

    !> Process #endif – end of conditional block
    !! Pops the top state from the stack. Reports error on unmatched #endif.
    !! @param[in] filename  Current file (for error messages)
    !! @param[in] line_num  Line number (for error messages)
    !!
    !! @b Remarks
    !! @ingroup group_conditional
    subroutine handle_endif(filename, line_num)
        character(*), intent(in) :: filename
        integer, intent(in) :: line_num

        if (cond_depth == 0) then
            if (verbose) print *, "Error: #endif without matching #if at ", trim(filename), ":", line_num
            return
        end if

        if (verbose) print *, "#endif at depth ", cond_depth
        cond_depth = cond_depth - 1
    end subroutine

end module