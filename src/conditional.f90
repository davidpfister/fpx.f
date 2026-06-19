!> @file
!! @defgroup group_conditional Conditional
!! Conditional support for the fpx preprocessor.
!!
!! This module implements the complete conditional compilation machinery used
!! by fpx. It provides functionality equivalent to the traditional C
!! preprocessor directives while also introducing a few convenience extensions.
!!
!! Supported directives include:
!!
!! - `#if` / `#elif`
!!   Evaluate arbitrary constant expressions using
!!   `evaluate_expression()`.
!!
!! - `#ifdef` / `#ifndef`
!!   Test whether a macro has been defined.
!!
!! - `#elifdef` / `#elifndef`
!!   fpx extensions combining `#elif` semantics with macro existence tests.
!!
!! - `#else`
!!   Select the fallback branch when no previous branch in the same
!!   conditional group has been activated.
!!
!! - `#endif`
!!   Terminate the current conditional block.
!!
!! Nested conditional blocks are supported up to
!! `MAX_COND_DEPTH` levels.
!!
!! The implementation follows the standard "first-match" semantics:
!! once a branch of a conditional group evaluates to true,
!! all remaining `#elif`, `#elifdef`, `#elifndef`, and `#else`
!! directives belonging to the same group are ignored.
!!
!! Internally, the module maintains a stack of conditional states
!! (`cond_stack`) together with the current nesting depth
!! (`cond_depth`). The helper function `is_active()` determines
!! whether the current source line belongs to an active branch and
!! therefore should be processed by the remainder of the preprocessor.
!!
!! @section conditional_design Design
!!
!! Each active conditional nesting level stores two pieces of state:
!!
!! - `active`
!!   Indicates whether the current branch should emit code.
!!
!! - `has_met`
!!   Indicates whether a previous branch within the same
!!   `#if`/`#elif`/`#else` group has already been selected.
!!
!! This design allows efficient evaluation of deeply nested
!! conditionals while preserving correct first-match semantics.
!!
!! @section conditional_examples Examples
!!
!! -# Include guard pattern:
!!
!!      @code{.f90}
!!      #ifndef MY_HEADER_H
!!      #define MY_HEADER_H
!!
!!          ! Header contents
!!
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Feature selection using expression evaluation:
!!
!!      @code{.f90}
!!      #if DEBUG >= 2
!!          print *, 'Verbose debugging'
!!      #elif DEBUG == 1
!!          print *, 'Standard debugging'
!!      #else
!!          ! Silent mode
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Platform-dependent compilation:
!!
!!      @code{.f90}
!!      #ifdef _OPENMP
!!          use omp_lib
!!      #else
!!          integer, parameter :: omp_get_thread_num = 0
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Conditional compilation using macro existence:
!!
!!      @code{.f90}
!!      #if defined(USE_MPI) && (MPI_VERSION >= 3)
!!          use mpi_f08
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Using the fpx extension `#elifdef`:
!!
!!      @code{.f90}
!!      #ifdef USE_CUDA
!!          call gpu_backend()
!!      #elifdef USE_OPENMP
!!          call omp_backend()
!!      #else
!!          call serial_backend()
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Nested conditionals:
!!
!!      @code{.f90}
!!      #ifdef DEBUG
!!          #if DEBUG > 1
!!          print *, 'Extra diagnostics'
!!          #endif
!!      #endif
!!      ...
!!      @endcode
module fpx_conditional
    use fpx_constants
    use fpx_logging
    use fpx_string
    use fpx_macro, only: macro, is_defined
    use fpx_operators, only: evaluate_expression
    use fpx_context

    implicit none; private

    public :: handle_if, &
            handle_ifdef, &
            handle_ifndef, &
            handle_elif, &
            handle_else, &
            handle_endif, &
            handle_elifdef, &
            handle_elifndef, &
            is_active

    !> State associated with a single conditional nesting level.
    !!
    !! Each `#if` directive pushes one instance of this type onto
    !! `cond_stack`, and the corresponding `#endif` removes it.
    !!
    !! The combination of `active` and `has_met` implements the
    !! standard first-match semantics of conditional preprocessing.
    !!
    !! @section cond_state_constructors Constructors
    !!
    !! @b Constructor
    !! @code{.f90} type(cond_state) function cond_state(logical active, logical has_met) @endcode
    !! @param[in] active
    !!   Whether the current branch is active and should emit code.
    !! @param[in] has_met
    !!   Whether a previous branch in the same conditional group
    !!   has already evaluated to true.
    !! @return A newly constructed conditional state object.
    !! @ingroup group_conditional
    type, public :: cond_state
        logical, public :: active
        logical, public :: has_met
    end type

    !> @brief Global stack of conditional states (depth-limited)
    !! @ingroup group_conditional
    type(cond_state), public :: cond_stack(MAX_COND_DEPTH)

    !> @brief Current nesting depth of conditional directives (0 = outside any #if)
    !! @ingroup group_conditional
    integer, public :: cond_depth = 0

contains

    !> Determine whether the current source position is active.
    !!
    !! Traverses all enclosing conditional levels and returns `.true.`
    !! only if every surrounding conditional branch is active.
    !!
    !! This routine is used throughout the preprocessing pipeline to
    !! decide whether directives should be executed and whether
    !! ordinary source lines should be emitted.
    !!
    !! @return
    !! `.true.` if the current line belongs to an active branch;
    !! `.false.` otherwise.
    !!
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
    !! @param[in] ctx       Context source line containing the directive
    !! @param[inout] macros    Current macro table
    !! @param[in] token     Usually 'if'
    !!
    !! @ingroup group_conditional
    subroutine handle_if(ctx, macros, token)
        type(context), intent(in)               :: ctx
        type(macro), allocatable, intent(inout) :: macros(:)
        character(*), intent(in)                :: token
        !private
        character(:), allocatable :: expr
        logical :: result, parent_active
        integer :: pos

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Conditional nesting too deep', &
                    source=trim(ctx%path)), &
                    ctx%content, ctx%line))
            return
        end if

        pos = index(lowercase(ctx%content), token) + len(token)
        expr = trim(adjustl(ctx%content(pos:)))
        result = evaluate_expression(expr, macros, ctx)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = result .and. parent_active
        cond_stack(cond_depth + 1)%has_met = result
    end subroutine

    !> Process #ifdef - test if a macro is defined
    !! @param[in] ctx       Context source line containing the directive
    !! @param[in] macros    Current macro table
    !! @param[in] token     Usually 'ifdef'
    !!
    !! @ingroup group_conditional
    subroutine handle_ifdef(ctx, macros, token)
        type(context), intent(in)       :: ctx
        type(macro), intent(in)         :: macros(:)
        character(*), intent(in)        :: token
        !private
        character(:), allocatable :: name
        logical :: defined, parent_active
        integer :: pos

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Conditional nesting too deep', &
                    source=trim(ctx%path)), &
                    ctx%content, ctx%line))
            return
        end if

        pos = index(lowercase(ctx%content), token) + len(token)
        name = trim(adjustl(ctx%content(pos:)))
        defined = is_defined(name, macros)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = defined .and. parent_active
        cond_stack(cond_depth + 1)%has_met = defined
    end subroutine

    !> Process #ifndef - test if a macro is NOT defined
    !! @param[in] ctx       Context source line containing the directive
    !! @param[in] macros    Current macro table
    !! @param[in] token     Usually 'ifndef'
    !!
    !! @ingroup group_conditional
    subroutine handle_ifndef(ctx, macros, token)
        type(context), intent(in)       :: ctx
        type(macro), intent(in)         :: macros(:)
        character(*), intent(in)        :: token
        !private
        character(:), allocatable :: name
        logical :: defined, parent_active
        integer :: pos

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Conditional nesting too deep', &
                    source=trim(ctx%path)), &
                    ctx%content, ctx%line))
            return
        end if

        pos = index(lowercase(ctx%content), token) + len(token)
        name = trim(adjustl(ctx%content(pos:)))
        defined = is_defined(name, macros)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = (.not. defined) .and. parent_active
        cond_stack(cond_depth + 1)%has_met = .not. defined
    end subroutine

    !> Process #elif - alternative branch after #if/#elif
    !! Only activates if no previous branch in the group was taken.
    !! @param[in] ctx       Context source line containing the directive
    !! @param[inout] macros    Current macro table
    !! @param[in] token     Usually 'elif'
    !!
    !! @ingroup group_conditional
    subroutine handle_elif(ctx, macros, token)
        type(context), intent(in)               :: ctx
        type(macro), allocatable, intent(inout) :: macros(:)
        character(*), intent(in)                :: token
        !private
        character(:), allocatable :: expr
        logical :: result, parent_active
        integer :: pos

        if (cond_depth == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Syntax error', &
                    label=label_type('#elif without matching #if', 1, len_trim(ctx%content)), &
                    source=trim(ctx%path)), &
                    ctx%content, ctx%line))
            return
        end if

        pos = index(lowercase(ctx%content), token) + len(token)
        expr = trim(adjustl(ctx%content(pos:)))
        result = evaluate_expression(expr, macros, ctx)
        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = result .and. parent_active
            if (result) cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
    end subroutine

    !> Process #elifdef - test if a macro is defined
    !! @param[in] ctx       Context source line containing the directive
    !! @param[in] macros    Current macro table
    !! @param[in] token     Usually 'elifdef'
    !!
    !! @ingroup group_conditional
    subroutine handle_elifdef(ctx, macros, token)
        type(context), intent(in)       :: ctx
        type(macro), intent(in)         :: macros(:)
        character(*), intent(in)        :: token
        !private
        character(:), allocatable :: name
        logical :: defined, parent_active
        integer :: pos

        if (cond_depth == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Syntax error', &
                    label=label_type('#elifdef without matching #if', 1, len_trim(ctx%content)), &
                    source=trim(ctx%path)), &
                    ctx%content, ctx%line))
            return
        end if

        pos = index(lowercase(ctx%content), token) + len(token)
        name = trim(adjustl(ctx%content(pos:)))
        defined = is_defined(name, macros)
        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = defined .and. parent_active
            if (defined) cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
    end subroutine

    !> Process #elifndef - test if a macro is not defined
    !! @param[in] ctx       Context source line containing the directive
    !! @param[in] macros    Current macro table
    !! @param[in] token     Usually 'elifndef'
    !!
    !! @ingroup group_conditional
    subroutine handle_elifndef(ctx, macros, token)
        type(context), intent(in)       :: ctx
        type(macro), intent(in)         :: macros(:)
        character(*), intent(in)        :: token
        !private
        character(:), allocatable :: name
        logical :: defined, parent_active
        integer :: pos

        if (cond_depth == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Syntax error', &
                    label=label_type('#elifndef without matching #if', 1, len_trim(ctx%content)), &
                    source=trim(ctx%path)), &
                    ctx%content, ctx%line))
            return
        end if

        pos = index(lowercase(ctx%content), token) + len(token)
        name = trim(adjustl(ctx%content(pos:)))
        defined = is_defined(name, macros)
        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = (.not. defined) .and. parent_active
            if (.not. defined) cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
    end subroutine

    !> Process #else - final fallback branch
    !! Activates only if no previous #if/#elif branch was true.
    !! @param[in] ctx  Context (for error messages)
    !!
    !! @ingroup group_conditional
    subroutine handle_else(ctx)
        type(context), intent(in) :: ctx
        !private
        logical :: parent_active

        if (cond_depth == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Syntax error', &
                    label=label_type('#else without matching #if', 1, len_trim(ctx%content)), &
                    source=trim(ctx%path)), &
                    ctx%content, ctx%line))
            return
        end if

        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = parent_active
            cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
    end subroutine

    !> Process #endif - end of conditional block
    !! Pops the top state from the stack. Reports error on unmatched #endif.
    !! @param[in] ctx  Context (for error messages)
    !!
    !! @ingroup group_conditional
    subroutine handle_endif(ctx)
        type(context), intent(in) :: ctx

        if (cond_depth == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Syntax error', &
                    label=label_type('#endif without matching #if', 1, len_trim(ctx%content)), &
                    source=trim(ctx%path)), &
                    ctx%content, ctx%line))
            return
        end if
        cond_depth = cond_depth - 1
    end subroutine

end module
