!> @file
!! @defgroup group_logging Logging
!! Global logging, ANSI-colored diagnostics, and pretty error/warning reporting for fpx
!!
!! This module is the central place for all human-readable output in the fpx preprocessor.
!! It provides:
!! - Full ANSI color and style support (bold, underline, colors, etc.)
!! - Structured diagnostic messages with source context, line numbers, and caret markers
!! - Pretty-printed multi-line error/warning/help/note/info reports
!! - Label-based highlighting of specific code ranges (like rustc-style diagnostics)
!! - Recursive sub-diagnostic support for nested explanations
!!
!! Designed to produce modern, readable, IDE-friendly output similar to rustc, clang, or cargo.
!! When `nocolor = .true.` (or terminal does not support ANSI), falls back to plain text.
!!
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Simple colored message (used internally for verbose logging):
!! @code{.f90}
!!    use fpx_logging
!!    
!!    verbose = .true.
!!    print *, render('Macro expanded: PI = 3.14159')
!! @endcode
!!
!! 2. Full diagnostic report (like a compiler error):
!! @code{.f90}
!!  character(*), parameter :: input = &
!!  '# This is a TOML document.' // nl // &
!!  'title = "TOML Example"' // nl // &
!!  '[owner]' // nl // &
!!  'name = "Tom Preston-Werner"' // nl // &
!!  'dob = 1979-05-27T07:32:00-08:00 # First class dates' // nl // &
!!  '[database]' // nl // &
!!  'server = "192.168.1.1"' // nl // &
!!  'ports = [ 8001, 8001, 8002 ]' // nl // &
!!  'connection_max = 5000' // nl // &
!!  'enabled = true' // nl // &
!!  '[servers]' // nl // &
!!  '  # Indentation (tabs and/or spaces) is allowed but not required' // nl // &
!!  '  [servers.alpha]' // nl // &
!!  '  ip = "10.0.0.1"' // nl // &
!!  '  dc = "eqdc10"' // nl // &
!!  '  [servers.beta]' // nl // &
!!  '  ip = "10.0.0.2"' // nl // &
!!  '  dc = "eqdc10"' // nl // &
!!  '[title]' // nl // &
!!  'data = [ ["gamma", "delta"], [1, 2] ]' // nl // &
!!  '# Line breaks are OK when inside arrays' // nl // &
!!  'hosts = [' // nl // &
!!  '  "alpha",' // nl // &
!!  '  "omega"' // nl // &
!!  ']'
!!
!!    print '(a)', render(diagnostic_report(level_error, &
!!       message="duplicated key 'title' found", &
!!       source="example.toml", &
!!       label=[label_type("table 'title' redefined here", 19, 2, 5, .true.), &
!!              label_type("first defined here", 2, 1, 5)]), &
!!              input)
!!    end
!! @endcode
!!
!!    Output might look like (colored in terminal):
!! @code
!! error: duplicated key 'title' found
!!  --> example.toml:19:2-6
!!    |
!!  1 | # This is a TOML document.
!!  2 | title = "TOML Example"
!!    | ----- first defined here
!!  3 | [owner]
!!    :
!! 18 |   dc = "eqdc10"
!! 19 | [title]
!!    |  ^^^^^ table 'title' redefined here
!! 20 | data = [ ["gamma", "delta"], [1, 2] ]
!!    |
!! @endcode
!!
!! @par ANSI style & color reference (used internally)
!! - Styles: BOLD_ON, UNDERLINE_ON, INVERSE_ON, STRIKETHROUGH_ON, ...
!! - Foreground: RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE, ...
!! - Background: same as foreground but prefixed with BG_
!!
!! @note This code is adapted from [pretty-diagnostics](https://github.com/awvwgk/pretty-diagnostics).
module fpx_logging
    use iso_c_binding
    
    implicit none; private

    public :: render, &
            diagnostic_report, &
            label_type, &
            LEVEL_ERROR, &
            LEVEL_WARNING, &
            LEVEL_HELP, &
            LEVEL_NOTE, &
            LEVEL_INFO
    
    !> @brief Master switch for verbose diagnostic output
    !! Default value is `.false.` (quiet mode).
    !! Set to `.true.` to get detailed step-by-step information about
    !! preprocessing actions. Safe to modify at any time – the change takes
    !! effect immediately for all subsequent operations.
    !! @ingroup group_logging
    logical, public :: verbose
    
    !> @brief Switch for controling the ANSI color output
    !! Default value is `.true.` (color mode on).
    !! Set to `.false.` to get raw string output.
    !! @ingroup group_logging
    logical, public :: nocolor = .false.

    character(1), parameter :: NL = new_line('a') !< New line character.
    character(1), parameter :: ESCAPE = achar(27) !< '\' character.
    character(2), parameter :: CODE_START = ESCAPE//'[' !< Start ansi code, "\[".
    character(1), parameter :: CODE_END = 'm' !< End ansi code, "m".
    character(4), parameter :: CODE_CLEAR = CODE_START//'0'//CODE_END !< Clear all styles, "\[0m".
    
    character(17), parameter :: STYLES(1:2, 1:16) = reshape([ &
                                                            'BOLD_ON          ', '1                ', & !  Bold on.
                                                            'ITALICS_ON       ', '3                ', & !  Italics on.
                                                            'UNDERLINE_ON     ', '4                ', & !  Underline on.
                                                            'INVERSE_ON       ', '7                ', & !  Inverse on: reverse foreground and background colors.
                                                            'STRIKETHROUGH_ON ', '9                ', & !  Strikethrough on.
                                                            'BOLD_OFF         ', '22               ', & !  Bold off.
                                                            'ITALICS_OFF      ', '23               ', & !  Italics off.
                                                            'UNDERLINE_OFF    ', '24               ', & !  Underline off.
                                                            'INVERSE_OFF      ', '27               ', & !  Inverse off: reverse foreground and background colors.
                                                            'STRIKETHROUGH_OFF', '29               ', & !  Strikethrough off.
                                                            'FRAMED_ON        ', '51               ', & !  Framed on.
                                                            'ENCIRCLED_ON     ', '52               ', & !  Encircled on.
                                                            'OVERLINED_ON     ', '53               ', & !  Overlined on.
                                                            'FRAMED_OFF       ', '54               ', & !  Framed off.
                                                            'ENCIRCLED_OFF    ', '54               ', & !  Encircled off.
                                                            'OVERLINED_OFF    ', '55               ' & !  Overlined off.
                                                            ], [2, 16]) !< Styles.
                                                            
    character(15), parameter :: COLORS_FG(1:2, 1:17) = reshape([ &
                                                               'BLACK          ', '30             ', & !  Black.
                                                               'RED            ', '31             ', & !  Red.
                                                               'GREEN          ', '32             ', & !  Green.
                                                               'YELLOW         ', '33             ', & !  Yellow.
                                                               'BLUE           ', '34             ', & !  Blue.
                                                               'MAGENTA        ', '35             ', & !  Magenta.
                                                               'CYAN           ', '36             ', & !  Cyan.
                                                               'WHITE          ', '37             ', & !  White.
                                                               'DEFAULT        ', '39             ', & !  Default (white).
                                                               'BLACK_INTENSE  ', '90             ', & !  Black intense.
                                                               'RED_INTENSE    ', '91             ', & !  Red intense.
                                                               'GREEN_INTENSE  ', '92             ', & !  Green intense.
                                                               'YELLOW_INTENSE ', '93             ', & !  Yellow intense.
                                                               'BLUE_INTENSE   ', '94             ', & !  Blue intense.
                                                               'MAGENTA_INTENSE', '95             ', & !  Magenta intense.
                                                               'CYAN_INTENSE   ', '96             ', & !  Cyan intense.
                                                               'WHITE_INTENSE  ', '97             ' & !  White intense.
                                                               ], [2, 17]) !< Foreground colors.
                                                               
    character(15), parameter :: COLORS_BG(1:2, 1:17) = reshape([ &
                                                               'BLACK          ', '40             ', & !  Black.
                                                               'RED            ', '41             ', & !  Red.
                                                               'GREEN          ', '42             ', & !  Green.
                                                               'YELLOW         ', '43             ', & !  Yellow.
                                                               'BLUE           ', '44             ', & !  Blue.
                                                               'MAGENTA        ', '45             ', & !  Magenta.
                                                               'CYAN           ', '46             ', & !  Cyan.
                                                               'WHITE          ', '47             ', & !  White.
                                                               'DEFAULT        ', '49             ', & !  Default (black).
                                                               'BLACK_INTENSE  ', '100            ', & !  Black intense.
                                                               'RED_INTENSE    ', '101            ', & !  Red intense.
                                                               'GREEN_INTENSE  ', '102            ', & !  Green intense.
                                                               'YELLOW_INTENSE ', '103            ', & !  Yellow intense.
                                                               'BLUE_INTENSE   ', '104            ', & !  Blue intense.
                                                               'MAGENTA_INTENSE', '105            ', & !  Magenta intense.
                                                               'CYAN_INTENSE   ', '106            ', & !  Cyan intense.
                                                               'WHITE_INTENSE  ', '107            ' & !  White intense.
                                                               ], [2, 17]) !< Background colors.
                                                               
    interface
        pure subroutine  memcpy(dest, src, n) bind(c,name='memcpy')
            import
            implicit none
            integer(c_intptr_t), value:: dest
            integer(c_intptr_t), value:: src
            integer(c_size_t), value :: n
        end subroutine
    end interface

    interface render
        module procedure :: render_diagnostic
        module procedure :: render_text
        module procedure :: render_text_with_label
        module procedure :: render_text_with_labels
    end interface

    enum, bind(c)
        enumerator :: LEVEL_ERROR = 0
        enumerator :: LEVEL_WARNING = 1
        enumerator :: LEVEL_HELP = 2
        enumerator :: LEVEL_NOTE = 3
        enumerator :: LEVEL_INFO = 4
    end enum
    
    type label_type
        !> Level of message
        integer, allocatable        :: level
        !> Primary message
        logical                     :: primary
        !> Line number of message
        integer                     :: line
        !> First character of message
        integer                     :: first
        !> Last character of message
        integer                     :: last
        !> Message text
        character(:), allocatable   :: text
        !> Identifier of context
        character(:), allocatable   :: source
    end type

    interface label_type
        module procedure :: label_new
        module procedure :: label_new_with_line
    end interface

    !> Definition of diagnostic message
    type :: diagnostic_report
        !> Level of message
        integer :: level
        !> Primary message
        character(:), allocatable :: message
        !> Context of the diagnostic source
        character(:), allocatable :: source
        !> Messages associated with this diagnostic
        type(label_type), allocatable :: label(:)
        !> Additional diagnostic information
        type(diagnostic_report), allocatable :: sub(:)
    end type

    interface diagnostic_report
        module procedure diagnostic_new
    end interface

    type :: line_token
        integer :: first, last
    end type

contains
    
    !> Colorize and stylize strings, DEFAULT kind.
    !! @param[in] string Input string.
    !! @param[in] foreground Foreground color definition.
    !! @param[in] background Background color definition.
    !! @param[in] style Style definition.
    pure function colorize(string, foreground, background, style) result(res)
        character(*), intent(in)           :: string
        character(*), intent(in), optional :: foreground
        character(*), intent(in), optional :: background
        character(*), intent(in), optional :: style
        character(:), allocatable :: res
        !private
        integer :: i

        res = string
        if (.not. nocolor) return
        if (present(foreground)) then
            i = color_index(upper(foreground))
            if (i > 0) res = CODE_START//trim(COLORS_FG(2, i))//CODE_END//res//CODE_CLEAR
        end if
        if (present(background)) then
            i = color_index(upper(background))
            if (i > 0) res = CODE_START//trim(COLORS_BG(2, i))//CODE_END//res//CODE_CLEAR
        end if
        if (present(style)) then
            i = style_index(upper(style))
            if (i > 0) res = CODE_START//trim(STYLES(2, i))//CODE_END//res//CODE_CLEAR
        end if
    end function

    !> Return the array-index corresponding to the queried color.
    !! @note Because Foreground and backround colors lists share the same name, 
    !! no matter what array is used to find the color index.
    !! Thus, the foreground array is used.
    elemental integer function color_index(color) result(res)
        character(*), intent(in) :: color !< Color definition.
        !private
        integer :: i

        res = 0
        do i = 1, size(COLORS_FG, dim=2)
            if (trim(COLORS_FG(1, i)) == trim(adjustl(color))) then
                res = i
                exit
            end if
        end do
    end function

    !> Return the array-index corresponding to the queried style.
    elemental integer function style_index(style) result(res)
        character(*), intent(in) :: style !< Style definition.
        !private
        integer :: i

        res = 0
        do i = 1, size(STYLES, dim = 2)
            if (trim(STYLES(1, i)) == trim(adjustl(style))) then
                res = i
                exit
            end if
        end do
    end function
    
    !> Return a string with all uppercase characters.
    elemental function upper(string)
        character(*), intent(in) :: string !< Input string.
        character(len(string)) :: upper !< Upper case string.
        !private
        integer, parameter :: a = iachar('a'), z = iachar('z'), CASE_DIFF = iachar('a')-iachar('A')
        integer :: i, ichar

        do i = 1, len(string)
            ichar = iachar(string(i:i))
            if (ichar >= a .and. ichar <= z) ichar = ichar - CASE_DIFF
            upper(i:i) = achar(ichar)
        enddo
    end function

    type(label_type) function label_new(text, first, length, level) result(that)
        character(*), intent(in)            :: text
        integer, intent(in)                 :: first
        integer, intent(in)                 :: length
        integer, intent(in) , optional      :: level

        that%text = text
        that%line = 1
        that%first = max(1, first)
        that%last = that%first + length
        that%primary = .true.
        if (present(level)) that%level = level
    end function
    
    type(label_type) function label_new_with_line(line, text, first, length, primary, level) result(that)
        integer, intent(in)                 :: line
        character(*), intent(in)            :: text
        integer, intent(in)                 :: first
        integer, intent(in)                 :: length
        logical, intent(in), optional       :: primary
        integer, intent(in) , optional      :: level

        that%text = text
        that%line = line
        that%first = max(1, first)
        that%last = that%first + length
        if (present(primary)) then
            that%primary = primary
        else
            that%primary = .true.
        end if
        if (present(level)) that%level = level
    end function

    !> Create new diagnostic message
    !! @param[in] level Level of message
    !! @param[in] message Primary message
    !! @param[in] source Context of the diagnostic source
    !! @param[in] label Messages associated with this diagnostic
    !! @param[in] diagnostic Additional diagnostic information
    type(diagnostic_report) function diagnostic_new(level, message, source, label, diagnostic) result(that)
        integer, intent(in)                             :: level
        character(*), intent(in), optional              :: message
        character(*), intent(in), optional              :: source
        type(label_type), intent(in), optional          :: label(..)
        type(diagnostic_report), intent(in), optional   :: diagnostic(:)
        !private
        integer :: i

        that%level = level
        if (present(message)) that%message = message
        if (present(source)) that%source = source
        if (present(label)) then
            if (allocated(that%label)) deallocate(that%label)
            select rank(label)
                rank(0)
                    allocate(that%label(1))
                    that%label(1) = label
                    if (.not. allocated(that%label(1)%level)) that%label(1)%level = level
                rank(1)
                    allocate(that%label, source = label)
                    do i = 1, size(label)
                        if (.not. allocated(that%label(i)%level)) that%label(i)%level = level
                    end do
            end select
        end if
        if (present(diagnostic)) that%sub = diagnostic
        
        if (allocated(that%label)) then
            if (.not. any(that%label(:)%primary)) then
                that%label(1)%primary = .true.
            end if
        end if
    end function

    pure function line_tokens(input) result(res)
        character(*), intent(in)        :: input
        type(line_token), allocatable :: res(:)
        !private
        integer :: first, last

        first = 1
        last = 0
        allocate(res(0))
        do while (first <= len(input))
            last = index(input(first + 1:), NL) + first - 1
            if (last < first) then
                last = len(input)
            end if

            res = [res, line_token(first, last)]

            first = last + (1 + len(NL))
        end do
    end function

    pure recursive function render_diagnostic(diag, input, linemum) result(res)
        type(diagnostic_report), intent(in)     :: diag
        character(*), intent(in)                :: input
        integer, intent(in), optional           :: linemum
        character(:), allocatable :: res
        !private
        integer :: i

        res = render_message(diag%level, diag%message)

        if (allocated(diag%label)) then
            res = res // NL // render_text_with_labels(input, diag%label, source=diag%source, linemum=linemum)
        end if

        if (allocated(diag%sub)) then
            do i = 1, size(diag%sub)
                res = res // NL // render_diagnostic(diag%sub(i), input, linemum)
            end do
        end if
    end function

    pure function render_message(level, message) result(res)
        integer, intent(in) :: level
        character(*), intent(in), optional :: message
        character(:), allocatable :: res

        if (present(message)) then
            res = level_name(level) // colorize(': ' // message, style = 'bold_on')
        else
            res = level_name(level)
        end if
    end function

    pure function level_name(level) result(res)
        integer, intent(in) :: level
        character(:), allocatable :: res

        select case (level)
        case (LEVEL_ERROR)
            res = colorize('error', foreground = 'red', style = 'bold_on')
        case (LEVEL_WARNING)
            res = colorize('warning', foreground = 'yellow', style = 'bold_on')
        case (LEVEL_HELP)
            res = colorize('help', foreground = 'cyan', style = 'bold_on')
        case (LEVEL_NOTE)
            res = colorize('note', foreground = 'blue', style = 'bold_on')
        case (LEVEL_INFO)
            res = colorize('info', foreground = 'magenta', style = 'bold_on')
        case default
            res = colorize('unknown', foreground = 'blue', style = 'bold_on')
        end select
    end function

    pure function render_source(source, offset) result(res)
        character(*), intent(in)    :: source
        integer, intent(in)         :: offset
        character(:), allocatable :: res

        res = repeat(' ', offset) // colorize('-->', foreground = 'blue') // ' ' // source
    end function

    pure function render_text(input, source, linenum) result(res)
        character(*), intent(in)            :: input
        character(*), intent(in), optional  :: source
        integer, intent(in), optional       :: linenum
        character(:), allocatable :: res
        !private
        integer :: i, offset, iline
        type(line_token), allocatable :: token(:)

        iline = 1; if (present(linenum)) iline = linenum
        token = line_tokens(input)
        offset = integer_width(size(token))

        if (present(source)) then
            res = render_source(source, offset) // NL // &
                    repeat(' ', offset + 1) // colorize('|', foreground = 'blue')
        else
            res = repeat(' ', offset + 1) // colorize('|', foreground = 'blue')
        end if

        do i = 1, size(token)
            res = res // NL // render_line(input(token(i)%first:token(i)%last), to_string(iline + i - 1, offset))
        end do
        res = res // NL // repeat(' ', offset + 1) // colorize('|', foreground = 'blue')
    end function

    pure function render_text_with_label(input, label, source, linenum) result(res)
        character(*), intent(in)            :: input
        type(label_type), intent(in)        :: label
        character(*), intent(in), optional  :: source
        integer, intent(in), optional       :: linenum
        character(:), allocatable :: res

        res = render_text_with_labels(input, [label], source, linenum)
    end function
    
    pure function render_text_with_labels(input, labels, source, linemum) result(res)
        character(*), intent(in)            :: input
        type(label_type), intent(in)        :: labels(:)
        character(*), intent(in), optional  :: source
        integer, intent(in), optional       :: linemum
        character(:), allocatable :: res
        !private
        integer :: i, j, offset, first, last, iline
        type(line_token), allocatable :: token(:)
        logical, allocatable :: display(:)

        token = line_tokens(input)
        first = max(1, minval(labels%line) - 1)
        last = min(size(token), maxval(labels%line) + 1)
        offset = integer_width(last)
        iline = 1; if (present(linemum)) iline = linemum
        
        i = 1  ! Without a primary we use the first label
        do j = 1, size(labels)
            if (labels(j)%primary) then
                i = j
                exit
            end if
        end do

        if (present(source)) then
            res = render_source(source, offset) // ':' // &
                    to_string(labels(i)%line) // ':' // &
                    to_string(labels(i)%first) // '-' // to_string(labels(i)%last) // NL // &
                    repeat(' ', offset + 1) // colorize('|', foreground = 'blue')
        else
            res = repeat(' ', offset + 1) // colorize('|', foreground = 'blue')
        end if

        allocate(display(first:last), source=.false.)
        do j = 1, size(labels)
            display(max(first, labels(j)%line - 1):min(last, labels(j)%line + 1)) = .true.
        end do

        do i = first, last
            if (.not. display(i)) then
                if (display(i - 1)) then
                    res = res // NL //&
                            repeat(' ', offset + 1) // colorize(':', foreground = 'blue')
                end if
                cycle
            end if

            res = res // NL //&
                    & render_line(input(token(i)%first:token(i)%last), &
                    &             to_string(iline + i - 1, offset))
            if (any(i == labels%line)) then
                do j = 1, size(labels)
                    if (labels(j)%line /= i) cycle
                    res = res // NL //&
                            & repeat(' ', offset + 1) // colorize('|', foreground = 'blue') // &
                            & render_label(labels(j))
                end do
            end if
        end do
        res = res // NL // repeat(' ', offset + 1) // colorize('|', foreground = 'blue')
    end function

    pure function render_label(label) result(res)
        type(label_type), intent(in) :: label
        character(:), allocatable :: res
        !private
        integer :: width
        character(1) :: marker
        character(:), allocatable :: this_color

        marker = merge('^', '-', label%primary)
        width = label%last - label%first

        if (allocated(label%level)) then
            select case (label%level)
            case (LEVEL_ERROR)
                res = repeat(' ', label%first) // colorize(repeat(marker, width), foreground = 'red', style = 'bold_on')
                if (allocated(label%text)) then
                    res = res // ' ' // colorize(label%text, foreground = 'red', style = 'bold_on')
                end if
            case (LEVEL_WARNING)
                res = repeat(' ', label%first) // colorize(repeat(marker, width), foreground = 'yellow', style = 'bold_on')
                if (allocated(label%text)) then
                    res = res // ' ' // colorize(label%text, foreground = 'yellow', style = 'bold_on')
                end if
            case (LEVEL_HELP)
                res = repeat(' ', label%first) // colorize(repeat(marker, width), foreground = 'cyan', style = 'bold_on')
                if (allocated(label%text)) then
                    res = res // ' ' // colorize(label%text, foreground = 'cyan', style = 'bold_on')
                end if
            case (LEVEL_INFO)
                res = repeat(' ', label%first) // colorize(repeat(marker, width), foreground = 'magenta', style = 'bold_on')
                if (allocated(label%text)) then
                    res = res // ' ' // colorize(label%text, foreground = 'magenta', style = 'bold_on')
                end if
            case default
                res = repeat(' ', label%first) // colorize(repeat(marker, width), foreground = 'blue', style = 'bold_on')
                if (allocated(label%text)) then
                    res = res // ' ' // colorize(label%text, foreground = 'blue', style = 'bold_on')
                end if
            end select
        else
            res = repeat(' ', label%first) // colorize(repeat(marker, width), foreground = 'blue', style = 'bold_on')
            if (allocated(label%text)) then
                res = res // ' ' // colorize(label%text, foreground = 'blue', style = 'bold_on')
            end if
        end if
    end function

    pure function render_line(input, line) result(res)
        character(*), intent(in)    :: input
        character(*), intent(in)    :: line
        character(:), allocatable :: res

        res = line // ' ' // colorize('|', foreground = 'blue') // ' ' // input
    end function

    pure integer function integer_width(input) result(res)
        integer, value :: input

        res = 0
        do while (input /= 0)
            input = input / 10
            res = res + 1
        end do

    end function

    !> Represent an integer as character sequence.
    pure function to_string(val, width) result(res)
        integer, intent(in)             :: val
        integer, intent(in), optional   :: width
        character(:), allocatable :: res
        !private
        integer, parameter :: buffer_len = range(val) + 2
        character(buffer_len) :: buffer
        integer :: n, pos
        character(1), parameter :: numbers(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

        if (val == 0) then
            res = numbers(0)
            return
        end if

        n = abs(val)
        buffer = ''

        pos = buffer_len + 1
        do while (n > 0)
            pos = pos - 1
            buffer(pos:pos) = numbers(mod(n, 10))
            n = n / 10
        end do
        if (val < 0) then
            pos = pos - 1
            buffer(pos:pos) = '-'
        end if

        if (present(width)) then
            res = repeat(' ', max(width - (buffer_len + 1 - pos), 0)) // buffer(pos:)
        else
            res = buffer(pos:)
        end if
    end function

end module
