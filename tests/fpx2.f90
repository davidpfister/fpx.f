module preprocessor_mod
  implicit none
  private
  public :: preprocess_file

  integer, parameter :: MAX_LINE_LEN = 1024
  integer, parameter :: MAX_DEPTH = 50
  integer, parameter :: MAX_COND_DEPTH = 50
  integer, parameter :: MAX_TOKENS = 100
  integer, parameter :: MAX_PARAMS = 10

  type :: macro_t
  character(len=:), allocatable :: name
  character(len=:), allocatable :: value
  character(len=MAX_LINE_LEN), allocatable :: params(:)  ! Fixed length
  integer :: num_params
end type macro_t

  type(macro_t), allocatable :: macros(:)
  integer :: num_macros = 0

  type :: cond_state_t
    logical :: active
    logical :: has_met
  end type cond_state_t
  type(cond_state_t) :: cond_stack(MAX_COND_DEPTH)
  integer :: cond_depth = 0

  type :: token_t
    character(len=:), allocatable :: value
    integer :: type  ! 0: number, 1: operator, 2: identifier, 3: parenthesis, 4: defined identifier
  end type token_t

contains

  subroutine preprocess_file(input_file, output_file)
    character(len=*), intent(in) :: input_file, output_file
    character(len=MAX_LINE_LEN) :: line, continued_line
    integer :: input_unit, output_unit, ios, line_num
    logical :: in_continuation

    open(newunit=input_unit, file=input_file, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      print *, "Error opening input file: ", trim(input_file)
      return
    end if
    open(newunit=output_unit, file=output_file, status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      print *, "Error opening output file: ", trim(output_file)
      close(input_unit)
      return
    end if

    if (allocated(macros)) deallocate(macros)
    allocate(macros(0))
    cond_depth = 0
    cond_stack(1)%active = .true.
    cond_stack(1)%has_met = .false.

    line_num = 0
    in_continuation = .false.
    continued_line = ''
    do
      read(input_unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      line_num = line_num + 1

      if (in_continuation) then
        continued_line = trim(continued_line) // trim(adjustl(line))
      else
        continued_line = trim(adjustl(line))
      end if

      if (len_trim(continued_line) > 0 .and. continued_line(len_trim(continued_line):len_trim(continued_line)) == '\') then
        in_continuation = .true.
        continued_line = continued_line(:len_trim(continued_line)-1)
        cycle
      else
        in_continuation = .false.
      end if

      call process_line(continued_line, output_unit, input_file, line_num)
    end do

    if (cond_depth > 0) then
      print *, "Error: Unclosed conditional block at end of file ", trim(input_file)
    end if

    close(input_unit)
    close(output_unit)
    deallocate(macros)
  end subroutine preprocess_file

  subroutine process_line(line, output_unit, filename, line_num)
    character(len=*) :: line
    integer, intent(in) :: output_unit, line_num
    character(len=*), intent(in) :: filename
    character(len=:), allocatable :: trimmed_line, expanded_line
    logical :: active

    trimmed_line = trim(adjustl(line))
    if (len_trim(trimmed_line) == 0) return

    active = is_active()
    print *, "Processing line ", line_num, ": '", trim(trimmed_line), "'"
    print *, "is_active() = ", active, ", cond_depth = ", cond_depth
    if (trimmed_line(1:1) == '#') then
      if (starts_with(trimmed_line, '#define') .and. active) then
        call handle_define(trimmed_line)
      else if (starts_with(trimmed_line, '#include') .and. active) then
        call handle_include(trimmed_line, output_unit, filename, line_num)
      else if (starts_with(trimmed_line, '#if')) then
        call handle_if(trimmed_line, filename, line_num)
      else if (starts_with(trimmed_line, '#ifdef')) then
        call handle_ifdef(trimmed_line, filename, line_num)
      else if (starts_with(trimmed_line, '#ifndef')) then
        call handle_ifndef(trimmed_line, filename, line_num)
      else if (starts_with(trimmed_line, '#elif')) then
        call handle_elif(trimmed_line, filename, line_num)
      else if (starts_with(trimmed_line, '#else')) then
        call handle_else(filename, line_num)
      else if (starts_with(trimmed_line, '#endif')) then
        call handle_endif(filename, line_num)
      end if
    else if (active) then
      expanded_line = expand_macros(trimmed_line)
      print *, "Writing to output: '", trim(expanded_line), "'"
      write(output_unit, '(A)') trim(expanded_line)
    end if
  end subroutine process_line

  logical function is_active()
    integer :: i
    is_active = .true.
    do i = 1, cond_depth + 1
      if (.not. cond_stack(i)%active) then
        is_active = .false.
        exit
      end if
    end do
  end function is_active

  logical function starts_with(str, prefix)
    character(len=*), intent(in) :: str, prefix
    starts_with = (index(trim(str), trim(prefix)) == 1)
  end function starts_with

  subroutine handle_define(line)
    character(len=*), intent(in) :: line
    character(len=MAX_LINE_LEN) :: name, value, temp
    integer :: pos, paren_start, paren_end, i, param_count
    type(macro_t), allocatable :: temp_macros(:)
  
    pos = index(line, ' ')
    temp = trim(adjustl(line(pos+1:)))
    paren_start = index(temp, '(')
    if (paren_start > 0) then
      ! Parameterized macro
      name = trim(temp(:paren_start-1))
      paren_end = index(temp, ')')
      if (paren_end == 0) then
        print *, "Error: Unclosed parenthesis in macro definition: ", trim(line)
        return
      end if
      value = trim(adjustl(temp(paren_end+1:)))
      
      ! Parse parameters
      temp = temp(paren_start+1:paren_end-1)
      param_count = 0
      pos = 1
      do while (pos <= len_trim(temp))
        if (temp(pos:pos) == ',') then
          param_count = param_count + 1
        end if
        pos = pos + 1
      end do
      if (len_trim(temp) > 0) param_count = param_count + 1
      
      if (.not. allocated(macros)) allocate(macros(0))
      num_macros = num_macros + 1
      if (num_macros > size(macros)) then
        allocate(temp_macros(num_macros))
        temp_macros(1:size(macros)) = macros
        call move_alloc(temp_macros, macros)
      end if
      macros(num_macros)%name = name
      macros(num_macros)%value = value
      macros(num_macros)%num_params = param_count
      
      allocate(macros(num_macros)%params(param_count))
      
      pos = 1
      i = 1
      do while (pos <= len_trim(temp) .and. i <= param_count)
        do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
          pos = pos + 1
        end do
        if (pos > len_trim(temp)) exit
        paren_start = pos
        do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',' .and. temp(pos:pos) /= ' ')
          pos = pos + 1
        end do
        macros(num_macros)%params(i) = temp(paren_start:pos-1)
        print *, "Param ", i, ": '", trim(macros(num_macros)%params(i)), "', length = ", len_trim(macros(num_macros)%params(i))
        i = i + 1
        if (pos <= len_trim(temp) .and. temp(pos:pos) == ',') pos = pos + 1
      end do
      
      print *, "Defined macro: ", trim(name), "(", (trim(macros(num_macros)%params(i))//", ", i=1,param_count-1), &
               trim(macros(num_macros)%params(param_count)), ") = ", trim(value)
    else
      ! Simple macro
      pos = index(temp, ' ')
      if (pos > 0) then
        name = trim(temp(:pos-1))
        value = trim(adjustl(temp(pos+1:)))
      else
        name = trim(temp)
        value = ''
      end if
      if (.not. allocated(macros)) allocate(macros(0))
      num_macros = num_macros + 1
      if (num_macros > size(macros)) then
        allocate(temp_macros(num_macros))
        temp_macros(1:size(macros)) = macros
        call move_alloc(temp_macros, macros)
      end if
      macros(num_macros)%name = name
      macros(num_macros)%value = value
      macros(num_macros)%num_params = 0
      print *, "Defined macro: ", trim(name), " = ", trim(value)
    end if
  end subroutine handle_define

  recursive subroutine handle_include(line, output_unit, parent_file, line_num)
    character(len=*), intent(in) :: line, parent_file
    integer, intent(in) :: output_unit, line_num
    character(len=MAX_LINE_LEN) :: include_file, buffer
    integer :: input_unit, ios
    logical :: in_continuation

    include_file = trim(adjustl(line(8:)))
    if (include_file(1:1) == '"' .or. include_file(1:1) == '<') then
      include_file = include_file(2:index(include_file, '"')-1)
      if (include_file(1:1) == '<') include_file = include_file(2:index(include_file, '>')-1)
    end if

    open(newunit=input_unit, file=include_file, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      print *, "Error: Cannot open include file '", trim(include_file), "' at ", trim(parent_file), ":", line_num
      return
    end if

    in_continuation = .false.
    buffer = ''
    do
      read(input_unit, '(A)', iostat=ios) buffer
      if (ios /= 0) exit
      if (in_continuation) then
        buffer = trim(buffer) // trim(adjustl(buffer))
      else
        buffer = trim(adjustl(buffer))
      end if
      if (len_trim(buffer) > 0 .and. buffer(len_trim(buffer):len_trim(buffer)) == '\') then
        in_continuation = .true.
        buffer = buffer(:len_trim(buffer)-1)
        cycle
      else
        in_continuation = .false.
      end if
      call process_line(buffer, output_unit, include_file, line_num)
    end do

    close(input_unit)
  end subroutine handle_include

  subroutine handle_if(line, filename, line_num)
    character(len=*), intent(in) :: line, filename
    integer, intent(in) :: line_num
    character(len=MAX_LINE_LEN) :: expr
    logical :: result, parent_active

    if (cond_depth + 1 > MAX_COND_DEPTH) then
      print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
      return
    end if

    expr = trim(adjustl(line(4:)))
    print *, "Evaluating #if: '", trim(expr), "'"
    result = evaluate_expression(expr)
    parent_active = is_active()
    cond_depth = cond_depth + 1
    cond_stack(cond_depth + 1)%active = result .and. parent_active
    cond_stack(cond_depth + 1)%has_met = result
    print *, "#if result: ", result, ", cond_depth = ", cond_depth, ", active = ", cond_stack(cond_depth + 1)%active
  end subroutine handle_if

  subroutine handle_ifdef(line, filename, line_num)
    character(len=*), intent(in) :: line, filename
    integer, intent(in) :: line_num
    character(len=MAX_LINE_LEN) :: name
    logical :: defined, parent_active

    if (cond_depth + 1 > MAX_COND_DEPTH) then
      print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
      return
    end if

    name = trim(adjustl(line(6:)))
    defined = is_defined(name)
    parent_active = is_active()
    cond_depth = cond_depth + 1
    cond_stack(cond_depth + 1)%active = defined .and. parent_active
    cond_stack(cond_depth + 1)%has_met = defined
  end subroutine handle_ifdef

  subroutine handle_ifndef(line, filename, line_num)
    character(len=*), intent(in) :: line, filename
    integer, intent(in) :: line_num
    character(len=MAX_LINE_LEN) :: name
    logical :: defined, parent_active

    if (cond_depth + 1 > MAX_COND_DEPTH) then
      print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
      return
    end if

    name = trim(adjustl(line(7:)))
    defined = is_defined(name)
    parent_active = is_active()
    cond_depth = cond_depth + 1
    cond_stack(cond_depth + 1)%active = (.not. defined) .and. parent_active
    cond_stack(cond_depth + 1)%has_met = .not. defined
  end subroutine handle_ifndef

  subroutine handle_elif(line, filename, line_num)
    character(len=*), intent(in) :: line, filename
    integer, intent(in) :: line_num
    character(len=MAX_LINE_LEN) :: expr
    logical :: result, parent_active

    if (cond_depth == 0) then
      print *, "Error: #elif without matching #if at ", trim(filename), ":", line_num
      return
    end if

    expr = trim(adjustl(line(5:)))
    result = evaluate_expression(expr)
    parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
    if (.not. cond_stack(cond_depth + 1)%has_met) then
      cond_stack(cond_depth + 1)%active = result .and. parent_active
      if (result) cond_stack(cond_depth + 1)%has_met = .true.
    else
      cond_stack(cond_depth + 1)%active = .false.
    end if
  end subroutine handle_elif

  subroutine handle_else(filename, line_num)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    logical :: parent_active

    if (cond_depth == 0) then
      print *, "Error: #else without matching #if at ", trim(filename), ":", line_num
      return
    end if

    parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
    if (.not. cond_stack(cond_depth + 1)%has_met) then
      cond_stack(cond_depth + 1)%active = parent_active
      cond_stack(cond_depth + 1)%has_met = .true.
    else
      cond_stack(cond_depth + 1)%active = .false.
    end if
    print *, "#else at depth ", cond_depth, ", active = ", cond_stack(cond_depth + 1)%active
  end subroutine handle_else

  subroutine handle_endif(filename, line_num)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num

    if (cond_depth == 0) then
      print *, "Error: #endif without matching #if at ", trim(filename), ":", line_num
      return
    end if

    print *, "#endif at depth ", cond_depth
    cond_depth = cond_depth - 1
  end subroutine handle_endif

  logical function is_defined(name)
    character(len=*), intent(in) :: name
    integer :: i
    is_defined = .false.
    do i = 1, num_macros
      if (trim(macros(i)%name) == trim(name)) then
        is_defined = .true.
        exit
      end if
    end do
  end function is_defined

  subroutine tokenize(expr, tokens, num_tokens)
    character(len=*), intent(in) :: expr
    type(token_t), allocatable, intent(out) :: tokens(:)
    integer, intent(out) :: num_tokens
    character(len=MAX_LINE_LEN) :: temp
    integer :: i, pos, len_expr
    logical :: in_word

    if (allocated(tokens)) deallocate(tokens)
    allocate(tokens(MAX_TOKENS))
    num_tokens = 0
    temp = trim(adjustl(expr))
    len_expr = len_trim(temp)
    i = 1
    in_word = .false.

    do while (i <= len_expr)
      if (temp(i:i) == ' ') then
        i = i + 1
        in_word = .false.
        cycle
      end if

      if (.not. in_word) then
        num_tokens = num_tokens + 1
        if (num_tokens > MAX_TOKENS) then
          print *, "Error: Too many tokens in expression"
          return
        end if
        in_word = .true.
      end if

      if (temp(i:i) == '(' .or. temp(i:i) == ')') then
        tokens(num_tokens)%value = temp(i:i)
        tokens(num_tokens)%type = 3
        i = i + 1
        in_word = .false.
      else if (temp(i:i) == '!') then
        tokens(num_tokens)%value = temp(i:i)
        tokens(num_tokens)%type = 1
        i = i + 1
        in_word = .false.
      else if (temp(i:i+1) == '&&' .or. temp(i:i+1) == '||' .or. temp(i:i+1) == '==' .or. &
               temp(i:i+1) == '!=' .or. temp(i:i+1) == '<=' .or. temp(i:i+1) == '>=') then
        tokens(num_tokens)%value = temp(i:i+1)
        tokens(num_tokens)%type = 1
        i = i + 2
        in_word = .false.
      else if (temp(i:i) == '<' .or. temp(i:i) == '>' .or. temp(i:i) == '=' .or. &
               temp(i:i) == '+' .or. temp(i:i) == '-') then
        tokens(num_tokens)%value = temp(i:i)
        tokens(num_tokens)%type = 1
        i = i + 1
        in_word = .false.
      else if (starts_with(temp(i:), 'defined')) then
        i = i + 7
        do while (i <= len_expr .and. temp(i:i) == ' ')
          i = i + 1
        end do
        if (i <= len_expr .and. temp(i:i) == '(') then
          i = i + 1
          pos = i
          do while (pos <= len_expr .and. temp(pos:pos) /= ')')
            pos = pos + 1
          end do
          tokens(num_tokens)%value = trim(temp(i:pos-1))
          tokens(num_tokens)%type = 4
          i = pos + 1
        else
          pos = i
          do while (pos <= len_expr .and. temp(pos:pos) /= ' ')
            pos = pos + 1
          end do
          tokens(num_tokens)%value = trim(temp(i:pos-1))
          tokens(num_tokens)%type = 4
          i = pos
        end if
        in_word = .false.
      else if (is_digit(temp(i:i))) then
        pos = i
        do while (pos <= len_expr .and. is_digit(temp(pos:pos)))
          pos = pos + 1
        end do
        tokens(num_tokens)%value = trim(temp(i:pos-1))
        tokens(num_tokens)%type = 0
        i = pos
        in_word = .false.
      else
        pos = i
        do while (pos <= len_expr .and. temp(pos:pos) /= ' ' .and. &
                  temp(pos:pos) /= '(' .and. temp(pos:pos) /= ')')
          pos = pos + 1
        end do
        tokens(num_tokens)%value = trim(temp(i:pos-1))
        tokens(num_tokens)%type = 2
        i = pos
        in_word = .false.
      end if
    end do

    print *, "Tokens for '", trim(expr), "':"
    do i = 1, num_tokens
      print *, "  Token ", i, ": ", trim(tokens(i)%value), " (type ", tokens(i)%type, ")"
    end do
  end subroutine tokenize

  logical function is_digit(ch)
    character(len=1), intent(in) :: ch
    is_digit = (ch >= '0' .and. ch <= '9')
  end function is_digit

  logical function evaluate_expression(expr)
    character(len=*), intent(in) :: expr
    type(token_t), allocatable :: tokens(:)
    integer :: num_tokens, pos, result

    call tokenize(expr, tokens, num_tokens)
    if (num_tokens == 0) then
      print *, "No tokens found for expression"
      evaluate_expression = .false.
      return
    end if

    pos = 1
    result = parse_expression(tokens, num_tokens, pos)
    print *, "Parsed '", trim(expr), "': pos = ", pos, ", num_tokens = ", num_tokens, ", result = ", result
    if (pos <= num_tokens) then
      print *, "Error: Extra tokens in expression: ", trim(tokens(pos)%value)
      evaluate_expression = .false.
      return
    end if
    evaluate_expression = (result /= 0)
  end function evaluate_expression

  recursive integer function parse_expression(tokens, num_tokens, pos) result(val)
    type(token_t), intent(in) :: tokens(:)
    integer, intent(in) :: num_tokens
    integer, intent(inout) :: pos
    val = parse_or(tokens, num_tokens, pos)
  end function parse_expression

  recursive integer function parse_or(tokens, num_tokens, pos) result(val)
    type(token_t), intent(in) :: tokens(:)
    integer, intent(in) :: num_tokens
    integer, intent(inout) :: pos
    integer :: left

    left = parse_and(tokens, num_tokens, pos)
    do while (pos <= num_tokens .and. tokens(pos)%value == '||')
      print *, "Parsing || at pos ", pos
      pos = pos + 1
      val = merge(1, 0, left /= 0 .or. parse_and(tokens, num_tokens, pos) /= 0)
      left = val
    end do
    val = left
  end function parse_or

  recursive integer function parse_and(tokens, num_tokens, pos) result(val)
    type(token_t), intent(in) :: tokens(:)
    integer, intent(in) :: num_tokens
    integer, intent(inout) :: pos
    integer :: left

    left = parse_equality(tokens, num_tokens, pos)
    do while (pos <= num_tokens .and. tokens(pos)%value == '&&')
      print *, "Parsing && at pos ", pos
      pos = pos + 1
      val = merge(1, 0, left /= 0 .and. parse_equality(tokens, num_tokens, pos) /= 0)
      left = val
    end do
    val = left
  end function parse_and

  recursive integer function parse_equality(tokens, num_tokens, pos) result(val)
    type(token_t), intent(in) :: tokens(:)
    integer, intent(in) :: num_tokens
    integer, intent(inout) :: pos
    integer :: left, right

    left = parse_relational(tokens, num_tokens, pos)
    do while (pos <= num_tokens .and. (tokens(pos)%value == '==' .or. tokens(pos)%value == '!='))
      print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
      if (tokens(pos)%value == '==') then
        pos = pos + 1
        right = parse_relational(tokens, num_tokens, pos)
        val = merge(1, 0, left == right)
      else
        pos = pos + 1
        right = parse_relational(tokens, num_tokens, pos)
        val = merge(1, 0, left /= right)
      end if
      left = val
    end do
    val = left
  end function parse_equality

  recursive integer function parse_relational(tokens, num_tokens, pos) result(val)
    type(token_t), intent(in) :: tokens(:)
    integer, intent(in) :: num_tokens
    integer, intent(inout) :: pos
    integer :: left, right

    left = parse_additive(tokens, num_tokens, pos)
    do while (pos <= num_tokens .and. (tokens(pos)%value == '<' .or. tokens(pos)%value == '>' .or. &
                                       tokens(pos)%value == '<=' .or. tokens(pos)%value == '>='))
      print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
      if (tokens(pos)%value == '<') then
        pos = pos + 1
        right = parse_additive(tokens, num_tokens, pos)
        val = merge(1, 0, left < right)
      else if (tokens(pos)%value == '>') then
        pos = pos + 1
        right = parse_additive(tokens, num_tokens, pos)
        val = merge(1, 0, left > right)
      else if (tokens(pos)%value == '<=') then
        pos = pos + 1
        right = parse_additive(tokens, num_tokens, pos)
        val = merge(1, 0, left <= right)
      else
        pos = pos + 1
        right = parse_additive(tokens, num_tokens, pos)
        val = merge(1, 0, left >= right)
      end if
      left = val
    end do
    val = left
  end function parse_relational

  recursive integer function parse_additive(tokens, num_tokens, pos) result(val)
    type(token_t), intent(in) :: tokens(:)
    integer, intent(in) :: num_tokens
    integer, intent(inout) :: pos
    integer :: left, right

    left = parse_unary(tokens, num_tokens, pos)
    do while (pos <= num_tokens .and. (tokens(pos)%value == '+' .or. tokens(pos)%value == '-'))
      print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
      if (tokens(pos)%value == '+') then
        pos = pos + 1
        right = parse_unary(tokens, num_tokens, pos)
        val = left + right
      else
        pos = pos + 1
        right = parse_unary(tokens, num_tokens, pos)
        val = left - right
      end if
      left = val
    end do
    val = left
  end function parse_additive

  recursive integer function parse_unary(tokens, num_tokens, pos) result(val)
    type(token_t), intent(in) :: tokens(:)
    integer, intent(in) :: num_tokens
    integer, intent(inout) :: pos

    if (pos <= num_tokens .and. tokens(pos)%value == '!') then
      print *, "Parsing ! at pos ", pos
      pos = pos + 1
      val = merge(0, 1, parse_unary(tokens, num_tokens, pos) /= 0)
    else
      val = parse_primary(tokens, num_tokens, pos)
    end if
  end function parse_unary

  recursive integer function parse_primary(tokens, num_tokens, pos) result(val)
    type(token_t), intent(in) :: tokens(:)
    integer, intent(in) :: num_tokens
    integer, intent(inout) :: pos
    integer :: i
    character(len=MAX_LINE_LEN) :: expanded

    if (pos > num_tokens) then
      print *, "Error: Unexpected end of expression at pos ", pos
      val = 0
      return
    end if

    print *, "Parsing primary: ", trim(tokens(pos)%value), " at pos ", pos
    if (tokens(pos)%type == 0) then
      read(tokens(pos)%value, *) val
      pos = pos + 1
    else if (tokens(pos)%type == 2) then
      expanded = expand_macros(tokens(pos)%value)
      read(expanded, *, iostat=i) val
      if (i /= 0) val = 0
      print *, "Expanded ", trim(tokens(pos)%value), " to ", trim(expanded), ", val = ", val
      pos = pos + 1
    else if (tokens(pos)%value == '(') then
      pos = pos + 1
      val = parse_expression(tokens, num_tokens, pos)
      if (pos > num_tokens .or. tokens(pos)%value /= ')') then
        print *, "Error: Missing closing parenthesis at pos ", pos
        val = 0
      else
        print *, "Parsed ) at pos ", pos
        pos = pos + 1
      end if
    else if (tokens(pos)%type == 4) then
      expanded = trim(tokens(pos)%value)
      val = merge(1, 0, is_defined(expanded))
      print *, "defined(", trim(expanded), ") = ", val
      pos = pos + 1
    else
      print *, "Error: Invalid token in expression: ", trim(tokens(pos)%value)
      val = 0
      pos = pos + 1
    end if
  end function parse_primary

  recursive function expand_macros(line) result(expanded)
  character(len=*), intent(in) :: line
  character(len=:), allocatable :: expanded, args_str, temp
  character(len=MAX_LINE_LEN) :: arg_values(MAX_PARAMS)
  integer :: i, pos, start, end_pos, paren_level, arg_start, arg_count, j, macro_start, macro_end

  expanded = line
  print *, "Initial expanded: '", trim(expanded), "'"
  do i = 1, num_macros
    pos = 1
    do while (pos > 0)
      pos = index(expanded, trim(macros(i)%name))
      if (pos > 0) then
        macro_start = pos  ! Save the start of the macro
        start = pos + len_trim(macros(i)%name)
        if (macros(i)%num_params > 0) then
          if (start <= len(expanded) .and. expanded(start:start) == '(') then
            paren_level = 1
            arg_start = start + 1
            arg_count = 0
            j = arg_start
            do while (j <= len(expanded) .and. paren_level > 0)
              if (expanded(j:j) == '(') paren_level = paren_level + 1
              if (expanded(j:j) == ')') paren_level = paren_level - 1
              if (paren_level == 1 .and. expanded(j:j) == ',' .or. paren_level == 0) then
                if (arg_count < MAX_PARAMS) then
                  arg_count = arg_count + 1
                  arg_values(arg_count) = trim(adjustl(expanded(arg_start:j-1)))
                  arg_start = j + 1
                end if
              end if
              j = j + 1
            end do
            if (arg_count /= macros(i)%num_params) then
              print *, "Error: Incorrect number of arguments for macro ", trim(macros(i)%name)
              cycle
            end if
            macro_end = j - 1  ! Save the end of the macro
            args_str = expanded(start:macro_end)
            print *, "Expanding macro: ", trim(macros(i)%name), ", args: ", trim(args_str)
            temp = macros(i)%value
            print *, "Initial temp: '", trim(temp), "'"
            do j = 1, macros(i)%num_params
              pos = 1
              do while (pos > 0)
                pos = index(trim(temp), trim(macros(i)%params(j)))
                if (pos > 0) then
                  start = pos + len_trim(macros(i)%params(j))
                  temp = temp(:pos-1) // trim(arg_values(j)) // temp(start:)
                  print *, "Substituted param ", j, ": '", trim(macros(i)%params(j)), "' with '", trim(arg_values(j)), "', temp: '", trim(temp), "'"
                end if
              end do
            end do
            print *, "Before recursive call, temp: '", trim(temp), "'"
            temp = expand_macros(temp)  ! Recursively expand only the macro value
            print *, "After recursive call, temp: '", trim(temp), "'"
            print *, "Prefix: '", expanded(:macro_start-1), "'"
            print *, "Temp: '", trim(temp), "'"
            print *, "Suffix: '", expanded(macro_end+1:), "'"
            expanded = expanded(:macro_start-1) // trim(temp) // expanded(macro_end+1:)
            print *, "After substitution, expanded: '", trim(expanded), "'"
          end if
        else
          temp = macros(i)%value
          macro_end = start - 1
          expanded = expanded(:macro_start-1) // trim(temp) // expanded(macro_end+1:)
          expanded = expand_macros(expanded)
          print *, "Simple macro expanded: '", trim(expanded), "'"
        end if
      end if
    end do
  end do
end function expand_macros

end module preprocessor_mod

program main
  use preprocessor_mod
  implicit none
  call preprocess_file('input.c', 'output.c')
end program main