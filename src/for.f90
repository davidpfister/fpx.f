!> @file
!! @defgroup group_for For
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
    
    integer, public :: depth = 0
    type(body), public :: bodies(MAX_FOR_DEPTH)
    type(macro), allocatable, public :: fmacros(:)

contains

    !> Process a #for directive and register or update a macro
    !!
    !! @param[in]    ctx     Context source line containing the #define
    !! @param[inout] macros  Current macro table (updated in-place)
    !! @param[in]    token   Usually 'DEFINE' – keyword matched in lowercase
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
            imacro = sizeof(fmacros)
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

    !> Process a #endfor directive and remove a macro from the table
    !! @param[in]    ctx     Context source line containing the #endfor
    !! @param[inout] macros  Current macro table (updated in-place)
    !! @param[in]    token   Usually 'endfor' – keyword matched in lowercase
    !!
    !! @b Remarks
    !! @ingroup group_for
    subroutine handle_endfor(ctx, ounit, token)
        type(context), intent(in)   :: ctx
        integer, intent(in)         :: ounit
        character(*), intent(in)    :: token
        !private
        integer :: i, j
        character(:), allocatable :: rst
        logical :: stitch
        type(macro) :: m
        
        m = fmacros(depth)
        if (allocated(fmacros(depth)%params)) deallocate(fmacros(depth)%params)
        do i = 1, size(m%params)
            fmacros(depth)%value = m%params(i)
            do j = 1, size(bodies(depth)%lines)
                rst = adjustl(expand_macros(bodies(depth)%lines(j)%chars, fmacros, stitch, .false., &
                        ctx))
                write(ounit, '(A)') rst
            end do
        end do
        
        depth = depth - 1
        if (depth < 0) then
            call printf(render(diagnostic_report(LEVEL_WARNING, &
                    message='Unbalanced #for expression. Missing #for or #endfor directive.', &
                    source=ctx%path), &
                    trim(ctx%content)))
            return
        end if
        call remove(fmacros, depth + 1)
        deallocate(fmacros(depth + 1)%params)
        fmacros = fmacros(:depth)
    end subroutine

    subroutine add_to_loop(line)
        character(*), intent(in) :: line
        
        if (.not. allocated(bodies(depth)%lines)) allocate(bodies(depth)%lines(0))
        bodies(depth)%lines = [bodies(depth)%lines, string(line)]
    end subroutine

    logical function is_in_forloop() result(res)
        res = depth > 0
    end function
end module
