module fpx_define
    use fpx_constants
    use fpx_logging
    use fpx_macro
    use fpx_string
    use fpx_global

    implicit none; private

    public :: handle_define, &
              handle_undef

contains

    subroutine handle_define(line, macros, token)
        character(*), intent(in)                    :: line
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        character(:), allocatable :: val, name, temp
        integer :: pos, paren_start, paren_end, i, npar, imacro

        pos = index(uppercase(line), token) + len(token)
        temp = trim(adjustl(line(pos + 1:)))
        
        paren_start = index(temp, '(')
        pos = index(temp, ' ')
        if (pos > 0 .and. pos < paren_start) paren_start = 0
        
        if (paren_start > 0) then
            name = trim(temp(:paren_start - 1))
            
            if (global%undef .contains. name) return
            paren_end = index(temp, ')')
            if (paren_end == 0) then
                if (verbose) print *, "Error: Unclosed parenthesis in macro definition: ", trim(line)
                return
            end if
            val = trim(adjustl(temp(paren_end + 1:)))
            if (verbose) print *, "Raw value before allocation: ", val, ", length = ", len(val)

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

            if (.not. allocated(macros)) allocate(macros(0))
            
            if (name == 'defined') then
                if (verbose) print *, '"defined" cannot be used a a macro name'
                return
            end if
            
            if (.not. is_defined(name, macros, imacro)) then
                call add(macros, name, val)
                imacro = sizeof(macros)
            else
                macros(imacro) = macro(name, val)
            end if

            if (index(temp, '...') > 0) then
                macros(imacro)%is_variadic = .true.
                npar = npar - 1
                if (allocated(macros(imacro)%params)) deallocate(macros(imacro)%params)
                allocate(macros(imacro)%params(npar))
                pos = 1
                i = 1
                do while (pos <= len_trim(temp) .and. i <= npar)
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
                        pos = pos + 1
                    end do
                    if (pos > len_trim(temp)) exit
                    paren_start = pos
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',')
                        pos = pos + 1
                    end do
                    macros(imacro)%params(i) = temp(paren_start:pos - 1)
                    if (verbose) print *, "Param ", i, ": '", macros(imacro)%params(i), &
                        "', length = ", len_trim(macros(imacro)%params(i))
                    i = i + 1
                    pos = pos + 1
                end do
                if (verbose) print *, "Defined variadic macro: ", trim(name), &
                    "(", (macros(imacro)%params(i)//", ", i=1, npar), "...) = ", trim(val)
            else
                macros(imacro)%is_variadic = .false.
                if (allocated(macros(imacro)%params)) deallocate(macros(imacro)%params)
                allocate(macros(imacro)%params(npar))
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
                    macros(imacro)%params(i) = temp(paren_start:pos - 1)
                    if (verbose) print *, "Param ", i, ": '", trim(macros(imacro)%params(i)), &
                        "', length = ", len_trim(macros(imacro)%params(i))
                    i = i + 1
                    if (pos <= len_trim(temp)) then
                        if (temp(pos:pos) == ',') pos = pos + 1
                    end if
                end do
                if (verbose) print *, "Defined macro: ", trim(name), "(", (macros(imacro)%params(i)//", ", i=1, npar - 1), &
                    macros(imacro)%params(npar), ") = ", trim(val)
            end if
        else
            pos = index(temp, ' ')
            if (pos > 0) then
                name = trim(temp(:pos - 1))
                val = trim(adjustl(temp(pos + 1:)))
            else
                name = trim(temp)
                val = ''
            end if
            
            if (global%undef .contains. name) return
            if (.not. allocated(macros)) allocate(macros(0))
            if (.not. is_defined(name, macros, imacro)) then
                call add(macros, name, val)
                imacro = sizeof(macros)
            else
                macros(imacro) = macro(name, val)
            end if
            
            if (verbose) print *, "Defined macro: ", trim(name), " = ", trim(val)
        end if
    end subroutine

    subroutine handle_undef(line, macros, token)
        character(*), intent(in)                    :: line
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        type(macro), allocatable :: temp_macros(:)
        character(:), allocatable :: name
        integer :: i, n, pos

        n = sizeof(macros)
        pos = index(uppercase(line), token) + len(token)
        name = trim(adjustl(line(pos:)))
        do i = 1, n
            if (macros(i) == name) then
                if (verbose) print *, "Undefining macro: ", name
                call remove(macros, i)
                exit
            end if
        end do
        
        if (i > n) then
            if (verbose) print *, "Warning: Macro ", name, " not found for #undef"
        end if
    end subroutine
end module
