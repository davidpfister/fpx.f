# Cheatsheet {#cheatsheet}

[TOC]  

## CLI Quick Commands
```bash
fpx file.F90                  → print to screen
fpx file.F90 -o out.f90       → write to file
fpx -DDEBUG=1 -Iinc file.F90  → define macro + include path
fpx                           → interactive REPL (great for testing)
fpx -v                        → show version
fpx -h                        → show help
```

## Most Common Directives

<center>

| Directive                 | Meaning & Example                                           |
|---------------------------|-------------------------------------------------------------|
| `#define NAME value`      | Object-like macro → `NAME` becomes `value`                  |
|                           | `#define PI 3.1415926535`                                   |
| `#define FUNC(x) x*x`     | Function-like macro                                         |
|                           | `real :: y = FUNC(5.0)` → `real :: y = 5.0*5.0`              |
| `#define LOG(...) print *, __VA_ARGS__` | Variadic macro (C99-style)                     |
| `#undef NAME`             | Remove a macro                                              |
| `#ifdef NAME` … `#endif`  | If NAME is defined                                          |
| `#ifndef NAME` … `#endif` | Classic header guard (most common)                          |
| `#if DEBUG >= 2` …        | Full expressions allowed (`&&`, `!`, `defined(NAME)`) |
| `#include "file.inc"`     | Local include (quotes)                                      |
| `#include <iso_c_binding.h>` | System include (angle brackets)                          |

</center>

## Built-in Predefined Macros

<center>

| Macro               | Expands to…                                          |
|---------------------|------------------------------------------------------|
| `__FILE__`          | "full/path/to/file.F90"                              |
| `__FILENAME__`      | "file.F90" (basename only)                           |
| `__LINE__`          | Current line number (integer)                        |
| `__DATE__`          | "Aug 12 2025"                                        |
| `__TIME__`          | "14:35:27"                                           |
| `__TIMESTAMP__`     | "Tue Aug 12 2025 14:35:27"                           |

</center>

## Advanced Macro Tricks

@code{.f90}
#define STRINGIFY(x) #x
#define CONCAT(a,b)  a ## _ ## b

#define DEBUG_PRINT(...) print *, "[DEBUG] ", __FILE__, ":", __LINE__, " → ", __VA_ARGS__
#define ASSERT(cond) if (.not.(cond)) then; error stop "Assertion failed: " // STRINGIFY(cond); end if

integer :: CONCAT(var,123)   ! → var_123

@endcode

## Common Patterns

@code{.f90}
! Header guard (recommended)
#ifndef MY_MODULE_MOD
#define MY_MODULE_MOD
#endif

module my_module content

! Feature toggles
#ifdef USE_MPI
  use mpi_f08, only: MPI_COMM_WORLD
#else
  integer, parameter :: &
    MPI_COMM_WORLD = -1
#endif

! Conditional compilation
#if defined(DEBUG) && DEBUG > 0
  print *, 'Debug mode active'
#endif
!...
@endcode

## Using fpx as a Library (in your own code)

Add include paths
@code{.f90}
use fpx_parser
use fpx_global

global%includedir = ['./inc', '../common']
@endcode

Add predefined macros & preprocess

@code{.f90}
  use fpx_parser
  use fpx_global

  !add version macro
  call add(global%macros, macro('_VERSION', '1.0.0'))

  !preprocess
  call preprocess('src/main.F90', 'build/main.f90')
@endcode