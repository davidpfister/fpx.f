!> @file
!! @defgroup group_for For
!! Fortran Preprocessor (fpx) – compile-time loop expansion support
!!
!! This module implements the non-standard `#for` / `#endfor` directive pair
!! used by fpx to generate repeated source code from a list of values.
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
!!    #for T in [integer, real]
!!    #for R in [32,64]
!!       type(T##R) :: value
!!    #endfor
!!    #endfor
!! ...
!! @endcode
!!
!! 4. Generic procedure generation:
!! @code{.f90}
!!    #define NUMERICS [integer, real, complex]
!!
!!    #for T in NUMERICS
!!       module procedure add_/**/T
!!    #endfor
!! ...
!! @endcode
!!
!! Loop variables behave like temporary macros and participate in normal
!! macro expansion rules.
module fpx_for
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

    type :: body
        type(string), allocatable :: lines(:)
    end type
    
    integer :: depth = 0
    type(body) :: bodies(MAX_FOR_DEPTH)
    type(macro), allocatable :: fmacros(:)

contains

    !> Process a `#for` directive and initialize a new loop context.
    !!
    !! Parses directives of the form:
    !! @code
    !!    #for variable in [item1, item2, ...]
    !! ...
    !! @endcode
    !!
    !! or
    !!
    !! @code
    !!    #for variable in MACRO_NAME
    !! ...
    !! @endcode
    !!
    !! where `MACRO_NAME` expands to a bracketed list.
    !!
    !! A temporary macro representing the loop variable is created and the
    !! iteration values are stored internally until the matching `#endfor`
    !! is reached.
    !!
    !! @param[in]    ctx     Current parsing context
    !! @param[inout] macros  Active macro table
    !! @param[in]    token   Directive keyword (`for`)
    !!
    !! @b Remarks
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
        temp = expand_macros(temp(pos:), macros, stitch, .false., ctx)
        
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
    !! When the outermost loop terminates, all temporary loop state is
    !! released automatically.
    !!
    !! @param[in] ctx     Current parsing context
    !! @param[in] ounit   Output unit
    !! @param[in] macros  Active macro table
    !! @param[in] token   Directive keyword (`endfor`)
    !!
    !! @b Remarks
    !! @ingroup group_for
    subroutine handle_endfor(ctx, ounit, macros, token)
        type(context), intent(in)   :: ctx
        integer, intent(in)         :: ounit
        character(*), intent(in)    :: token
        type(macro), intent(in)     :: macros(:)
        !private
        integer :: i, j
        character(:), allocatable :: rst, tmp
        logical :: stitch
        type(string), allocatable :: params(:)
        
        tmp = ''
        if (depth <= size_of(fmacros)) then
            params = fmacros(depth)%params
            if (allocated(fmacros(depth)%params)) deallocate(fmacros(depth)%params)
        
            do i = 1, size(params)
                fmacros(depth)%value = params(i)
                do j = 1, size(bodies(depth)%lines)
                    rst = adjustl(expand_macros(bodies(depth)%lines(j)%chars, [fmacros, macros], stitch, .false., ctx))
                    if (depth > 1) then
                        if (.not. allocated(bodies(depth - 1)%lines)) allocate(bodies(depth - 1)%lines(0))
                        bodies(depth - 1)%lines = [bodies(depth - 1)%lines, string(rst)]                       
                    else
                        do
                            tmp = adjustl(expand_macros(rst, [fmacros, macros], stitch, .false., ctx))
                            if (tmp == rst) exit
                            rst = tmp
                        end do
                        write(ounit, '(A)') rst
                    end if
                end do
                if (depth > 1) then
                    if (.not. allocated(bodies(depth - 1)%lines)) allocate(bodies(depth - 1)%lines(0))
                    bodies(depth - 1)%lines = [bodies(depth - 1)%lines, string('')]                       
                else
                    write(ounit, '(A)') ''
                end if
            end do
        end if
        depth = depth - 1
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

    !> Append a source line to the currently active loop body.
    !!
    !! Lines are stored verbatim and expanded only when the matching
    !! `#endfor` directive is encountered.
    !!
    !! @param[in] line Source line to store
    !!
    !! @b Remarks
    !! @ingroup group_for
    subroutine add_to_loop(line)
        character(*), intent(in) :: line
        
        if (.not. allocated(bodies(depth)%lines)) allocate(bodies(depth)%lines(0))
        bodies(depth)%lines = [bodies(depth)%lines, string(line)]
    end subroutine

    !> Query whether parsing is currently inside a `#for` block.
    !!
    !! @return `.true.` when one or more loop contexts are active,
    !!         `.false.` otherwise.
    !!
    !! @b Remarks
    !! @ingroup group_for
    logical function is_in_forloop() result(res)
        res = depth > 0
    end function
end module
