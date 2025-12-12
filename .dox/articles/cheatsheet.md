# **fpx Preprocessor Cheatsheet**  

```text
                  fpx – Fortran Preprocessor Cheatsheet
                  ────────────────────────────────────────
```

### 1. CLI Quick Commands
```bash
fpx file.F90                → print to screen
fpx file.F90 -o out.f90       → write to file
fpx -DDEBUG=1 -Iinc file.F90  → define macro + include path
fpx                           → interactive REPL (great for testing)
fpx -v                        → show version
fpx -h                        → show help
```

### 2. Most Common Directives
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
| `#if DEBUG >= 2` …        | Full expressions allowed (`&&`, `||`, `!`, `defined(NAME)`) |
| `#include "file.inc"`     | Local include (quotes)                                      |
| `#include <iso_c_binding.h>` | System include (angle brackets)                          |

### 3. Built-in Predefined Macros
| Macro               | Expands to…                                          |
|---------------------|------------------------------------------------------|
| `__FILE__`          | "full/path/to/file.F90"                              |
| `__FILENAME__`      | "file.F90" (basename only)                           |
| `__LINE__`          | Current line number (integer)                        |
| `__DATE__`          | "Aug 12 2025"                                        |
| `__TIME__`          | "14:35:27"                                           |
| `__TIMESTAMP__`     | "Tue Aug 12 2025 14:35:27"                           |

### 4. Advanced Macro Tricks
```fortran
#define STRINGIFY(x) #x
#define CONCAT(a,b)  a ## _ ## b

#define DEBUG_PRINT(...) print *, "[DEBUG] ", __FILE__, ":", __LINE__, " → ", __VA_ARGS__
#define ASSERT(cond) if (.not.(cond)) then; error stop "Assertion failed: " // STRINGIFY(cond); end if

integer :: CONCAT(var,123)   ! → var_123
```

### 5. Common Patterns
```fortran
! Header guard (recommended)
#ifndef MY_MODULE_MOD
#define MY_MODULE_MOD

module my_module content

#endif


! Conditional compilation
#if defined(DEBUG) && DEBUG > 0
  print *, "Debug mode active"
#endif


! Feature toggles
#ifdef USE_MPI
  use mpi_f08
#else
  integer, parameter :: MPI_COMM_WORLD = -1
#endif
```

### 6. Using fpx as a Library (in your own code)
```fortran
use fpx_parser
use fpx_global

! Add include paths & predefined macros
global%includedir = [ "./inc", "../common" ]
call add(global%macros, macro("BUILD_DATE", '"'//__DATE__//'"'))

! One-liner
call preprocess("src/main.F90", "build/main.f90")
```

### 7. One-liners You’ll Use Every Day
```bash
# Debug everything
fpx -DDEBUG=2 file.F90

# Strip all comments
global%exclude_comments = .true.; call preprocess(...)

# Force-rebuild header guards
fpx -UMY_HEADER_H file.F90
```

Happy preprocessing!  
`fpx` — because Fortran deserves a modern, pure-Fortran preprocessor.