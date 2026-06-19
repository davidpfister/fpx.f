!> @file
!! @defgroup group_for For
!! Fortran Preprocessor (FPX) – compile-time loop expansion support
!!
!! This module implements the non-standard `#for` / `#endfor` directive pair
!! used by FPX to generate repeated source code from a list of values.
!!
!! Features:
!! - Simple iteration over explicit lists:
!!   `#for T in [integer, real, complex]`
!! - Iteration over macro-expanded lists:
!!   `#define NUMERICS [integer, real, complex]`
!!   `#for T in NUMERICS`
!! - Arbitrary nesting of `#for` blocks
!! - Integration with the normal macro expansion engine
!! - Deferred body collection until matching `#endfor`
!! - Automatic cleanup of loop-local variables
!!
!! During parsing, loop bodies are stored internally and emitted only when
!! the matching `#endfor` is encountered. Each iteration temporarily defines
!! the loop variable as a macro whose value is substituted into the collected
!! body before output.
!!
!! @section for_examples Examples
!!
!! 1. Basic iteration:
!! @code{.f90}
!!    #for T in [integer, real, complex]
!!       type(T) :: value
!!    #endfor
!!
!!    ! Expands to:
!!    type(integer) :: value
!!    type(real)    :: value
!!    type(complex) :: value
!! ...
!! @endcode
!!
!! 2. Using a macro list:
!! @code{.f90}
!!    #define NUMERICS [integer, real, complex]
!!
!!    #for T in NUMERICS
!!       type(T) :: value
!!    #endfor
!! ...
!! @endcode
!!
!! 3. Nested loops:
!! @code{.f90}
!!    #define CONCAT(a,b) a##b
!!    #for T in [integer, real]
!!    #for R in [32,64]
!!       type(CONCAT(T,R)) :: value
!!    #endfor
!!    #endfor
!! ...
!! @endcode
!!
!! 4. Generic procedure generation:
!! @code{.f90}
!!    #define CONCAT(a,b) a##b
!!    #define NUMERICS [integer, real, complex]
!!
!!    #for T in NUMERICS
!!       module procedure CONCAT(add_,T)
!!    #endfor
!! ...
!! @endcode
!!
!! 5. Cartesian product generation:
!! @code{.f90}
!!    #for T in [real, complex]
!!    #for K in [32, 64]
!!       type(T(K)) :: value
!!    #endfor
!!    #endfor
!!
!!    ! Generates:
!!    ! type(real(32))    :: value
!!    ! type(real(64))    :: value
!!    ! type(complex(32)) :: value
!!    ! type(complex(64)) :: value
!! ...
!! @endcode
!!
!! Loop variables behave exactly like temporary object-like macros and
!! therefore participate in all normal macro expansion rules, including
!! nested expansion and token pasting.
!!
!! @note
!! When nested loops are active, generated lines are appended to the
!! enclosing loop body rather than written immediately. This guarantees
!! inside-out expansion semantics.
module fpx_for
    use, intrinsic :: iso_c_binding, only: c_funptr, c_f_procpointer
    use fpx_constants
    use fpx_logging
    use fpx_macro
    use fpx_string
    use fpx_global
    use fpx_context

    implicit none; private

    public :: handle_for, &
            handle_endfor, &
            is_in_forloop, &
            add_to_loop

    !> Internal storage for deferred loop bodies.
    !!
    !! Source lines belonging to a `#for` block are collected until the
    !! corresponding `#endfor` directive is encountered.
    !!
    !! @ingroup group_for
    type :: body
        integer :: nlines = 0
        type(string), allocatable :: lines(:)
    end type

    !! @cond
    integer :: depth = 0
    integer, parameter :: BODY_BUFFER = 50 
    type(body) :: bodies(MAX_FOR_DEPTH)
    type(macro), allocatable :: fmacros(:)
    !! @endcond

contains

    !> Process a `#for` directive and initialize a new loop context.
    !!
    !! The directive header is parsed immediately, but the loop body is
    !! not expanded at this stage. Instead, subsequent source lines are
    !! collected until the matching `#endfor` directive is encountered.
    !!
    !! The loop variable behaves as a temporary object-like macro whose
    !! value changes for each iteration.
    !!
    !! Supported syntax:
    !!
    !! @code
    !! #for identifier in [value1, value2, ...]
    !! #for identifier in MACRO_NAME
    !! @endcode
    !!
    !! where `MACRO_NAME` expands to a bracketed list.
    !!
    !! @note
    !! `#for` and `#endfor` are FPX extensions and are not part of the
    !! ISO C preprocessor specification.
    !!
    !! @param[in]    ctx     
    !!   Current parsing context
    !! @param[inout] macros
    !!   Active macro table
    !! @param[in]    token   
    !!   Directive keyword (`for`)
    !!
    !! @ingroup group_for
    subroutine handle_for(ctx, macros, token)
        type(context), intent(in)                   :: ctx
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        character(:), allocatable :: val, name, temp
        integer :: pos, paren_start, paren_end, i, npar, imacro
        logical :: stitch

        depth = depth + 1
        if (depth > MAX_FOR_DEPTH) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Loop nesting too deep', &
                    source=trim(ctx%path)), &
                    ctx%content, ctx%line))
            return
        end if

        pos = index(lowercase(ctx%content), token) + len(token)
        temp = trim(adjustl(ctx%content(pos + 1:)))

        if (index(temp, ' in ') == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Syntax error', &
                    label=label_type('Missing " in " keyword', pos + 1, 4), &
                    source=ctx%path), &
                    trim(ctx%content), ctx%line))
            return
        else
            name = trim(adjustl(temp(:index(temp, ' in '))))
            if (global%undef .contains. name) return

            if (name == 'defined') then
                call printf(render(diagnostic_report(LEVEL_ERROR, &
                        message='Reserved macro name', &
                        label=label_type('"defined" cannot be used as a macro name', paren_start + 1, len(name)), &
                        source=ctx%path), &
                        trim(ctx%content), ctx%line))
            end if
        end if

        pos = index(temp, ' in ') + len(' in ')
        temp = expand_macros(temp(pos:), macros, stitch, global%implicit_continuation, global%support_dollar_insert, ctx)

        paren_start = index(temp, '[')
        if (paren_start == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Syntax error', &
                    label=label_type('Missing opening square bracket in #for expression', 1, 1), &
                    source=ctx%path), &
                    trim(ctx%content), ctx%line))
            return
        end if

        paren_end = index(temp, ']', back=.true.)
        if (paren_end == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
                    message='Syntax error', &
                    label=label_type('Missing closing square bracket in #for expression', len_trim(ctx%content) + 1, 1), &
                    source=ctx%path), &
                    trim(ctx%content), ctx%line))
            return
        end if
        temp = temp(paren_start + 1:paren_end - 1)
        npar = 0
        pos = 1
        do while (pos <= len_trim(temp))
            if (temp(pos:pos) == ',') then
                npar = npar + 1
            end if
            pos = pos + 1
        end do
        if (len_trim(temp) > 0) npar = npar + 1

        if (.not. allocated(fmacros)) allocate(fmacros(0))
        if (.not. is_defined(name, fmacros, imacro)) then
            call add(fmacros, name, '')
            imacro = size_of(fmacros)
        else
            fmacros(imacro) = macro(name, '')
        end if

        fmacros(imacro)%active = .false.
        fmacros(imacro)%is_variadic = .false.
        if (allocated(fmacros(imacro)%params)) deallocate(fmacros(imacro)%params)
        allocate(fmacros(imacro)%params(npar))
        pos = 1
        i = 1
        do while (pos <= len_trim(temp) .and. i <= npar)
            do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
                pos = pos + 1
            end do
            if (pos > len_trim(temp)) exit
            paren_start = pos
            do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',' .and. temp(pos:pos) /= ' ')
                pos = pos + 1
                if (pos > len_trim(temp)) exit
            end do
            fmacros(imacro)%params(i) = temp(paren_start:pos - 1)
            i = i + 1
            if (pos <= len_trim(temp)) then
                if (temp(pos:pos) == ',') pos = pos + 1
            end if
        end do
    end subroutine

    !> Finalize a loop and emit all expanded iterations.
    !!
    !! The collected loop body is expanded once for every value contained in
    !! the loop variable parameter list. Nested loops are handled recursively
    !! by forwarding generated lines to the enclosing loop body when present.
    !!
    !! For each iteration value:
    !! - the loop variable macro is activated,
    !! - the stored body is macro-expanded,
    !! - generated lines are reprocessed by the normal preprocessing engine,
    !! - output is either emitted directly or forwarded to an enclosing loop.
    !!
    !! When the outermost loop terminates, all temporary loop state is
    !! released automatically.
    !!
    !! @param[in] ctx     Current parsing context
    !! @param[in] ounit   Output unit
    !! @param[in] p   preprocessor function pointer
    !! @param[inout] macros  Active macro table
    !! @param[in] token   Directive keyword (`endfor`)
    !!
    !! @ingroup group_for
    subroutine handle_endfor(ctx, ounit, p, macros, token)
        type(context), intent(inout)    :: ctx
        integer, intent(in)             :: ounit
        type(c_funptr), intent(in)      :: p
        type(macro), intent(in)         :: macros(:)
        character(*), intent(in)        :: token
        !private
        integer :: i, j
        character(:), allocatable :: rst, tmp
        logical :: stitch
        type(string), allocatable :: params(:)
        type(macro), allocatable :: ms(:)
        procedure(preprocess_line), pointer :: preprocess => null()

        call c_f_procpointer(p, preprocess)
        
        tmp = ''
        depth = depth - 1

        if (depth + 1 <= size_of(fmacros)) then
            if (allocated(fmacros(depth + 1)%params)) params = fmacros(depth + 1)%params
            if (allocated(fmacros(depth + 1)%params)) deallocate(fmacros(depth + 1)%params)

            do i = 1, size_of(params)
                fmacros(depth + 1)%value = params(i)
                fmacros(depth + 1)%active = .true.
                ms = [fmacros(depth + 1), macros]
                !do j = 1, bodies(depth + 1)%nlines
                do j = 1, bodies(depth + 1)%nlines  !size_of(bodies(depth + 1)%lines)
                    if (head(bodies(depth + 1)%lines(j)%chars) == '#') then
                        if (len(bodies(depth + 1)%lines(j)%chars) == 1) then
                            return
                        else
                            rst = adjustl(expand_macros(bodies(depth + 1)%lines(j)%chars, ms, stitch, &
                                    global%implicit_continuation, global%support_dollar_insert, ctx))
                            tmp = preprocess(rst, ounit, ctx%path, ctx%line, ms, stitch)
                        end if
                    else
                        rst = adjustl(expand_macros(bodies(depth + 1)%lines(j)%chars, ms, stitch, global%implicit_continuation, &
                                global%support_dollar_insert, ctx))
                        tmp = preprocess(rst, ounit, ctx%path, ctx%line, ms, stitch)
                    end if

                    if (depth > 0) then
                        if (len_trim(tmp) > 0) then
                            call addline(bodies(depth), string(tmp))
                        end if
                    else
                        do
                            if (tmp == rst) exit
                            tmp = preprocess(rst, ounit, ctx%path, ctx%line, ms, stitch)
                            rst = tmp
                        end do
                        write(ounit, '(A)') rst
                    end if
                end do
                if (depth > 0) then
                    call addline(bodies(depth), string(''))
                else
                    write(ounit, '(A)') ''
                end if
            end do
            bodies(depth + 1)%nlines = 0
            if (allocated(bodies(depth + 1)%lines)) deallocate(bodies(depth + 1)%lines)
        end if

        if (allocated(params)) deallocate(params)
        if (allocated(ms)) deallocate(ms)
        nullify(preprocess)
        
        if (depth < 0) then
            call printf(render(diagnostic_report(LEVEL_WARNING, &
                    message='Unbalanced #for expression. Missing #for or #endfor directive.', &
                    source=ctx%path), &
                    trim(ctx%content)))
            return
        end if

        if (depth == 0) then
            if (allocated(fmacros)) deallocate(fmacros)
            do i = 1, MAX_FOR_DEPTH
                if (allocated(bodies(i)%lines)) deallocate(bodies(i)%lines)
            end do
        end if
    end subroutine

    !> Append a source line to the innermost active loop body.
    !!
    !! Lines are stored verbatim without macro expansion. Expansion is
    !! deferred until the corresponding `#endfor` directive is processed.
    !!
    !! @param[in] line Source line to store
    !!
    !! @ingroup group_for
    subroutine add_to_loop(line)
        character(*), intent(in) :: line

        call addline(bodies(depth), string(line))
    end subroutine

    !> Query whether parsing is currently inside a `#for` block.
    !! This routine is typically used by the main preprocessing engine to
    !! determine whether incoming source lines should be emitted directly
    !! or collected for later expansion.
    !! @return `.true.` when one or more loop contexts are active,
    !!         `.false.` otherwise.
    !!
    !! @ingroup group_for
    logical function is_in_forloop() result(res)
        res = depth > 0
    end function

    subroutine addline(b, line)
        type(body), intent(inout)       :: b
        type(string), intent(in)        :: line
        !private
        type(string), allocatable :: tmp(:)
        integer :: n

        if (.not. allocated(b%lines)) then
            allocate(b%lines(0))
            b%nlines = 0
        end if
        b%nlines = b%nlines + 1
        n = size(b%lines)
        if (b%nlines <= n) then
            b%lines(b%nlines) = line
        else
            allocate(tmp(n + BODY_BUFFER))
            tmp(1:n) = b%lines(1:n)
            tmp(n + 1) = line
            call move_alloc(from=tmp, to=b%lines)
        end if
    end subroutine
end module
