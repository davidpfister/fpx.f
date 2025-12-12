# fpx – The Modern Fortran Preprocessor

**A fast, pure-Fortran, standards-compatible preprocessor written in Fortran**

Perfect for Fortran projects that need `#include`, `#define`, `#ifdef`, macro expansion, `__FILE__`, `__LINE__`, `__DATE__`, etc. — all without leaving the Fortran ecosystem.

---

## 1. Using fpx from the Command Line (CLI)

After compiling the program (e.g. `gfortran -O3 *.f90 -o fpx`), you get an executable called `fpx`.

### Basic usage

```bash
fpx [options] input.F90 [output.f90]
```

| Command                                   | What it does                                           |
|-------------------------------------------|--------------------------------------------------------|
| `fpx src/main.F90`                        | Preprocess `main.F90` and print result to screen       |
| `fpx src/main.F90 -o build/main.f90`      | Write preprocessed code to `build/main.f90`            |
| `cat source.F90 | fpx > clean.f90`         | Read from stdin, write to file                         |
| `fpx` (no arguments)                      | Interactive REPL mode – type lines and see them expanded instantly |

### Most useful options

| Option        | Meaning                                                    | Example                     |
|------------|------------------------------------------------------------|-----------------------------|
| `-DNAME`   | Define macro `NAME` with empty value                       | `-DDEBUG`                   |
| `-DNAME=10`| Define macro `NAME` with value `10`                        | `-DMAX_ITER=1000`           |
| `-U NAME`  | Undefine a macro (useful to force a header guard off)      | `-U _OPENMP`                |
| `-I/path`  | Add a directory to the include search path                 | `-I./include`               |
| `-o file.f90`| Specify output file (otherwise stdout)                   | `-o preprocessed.f90`       |
| `-v`       | Show version and exit                                      | `fpx -v`                    |
| `-h` / `-?`| Show help                                                  | `fpx -h`                    |

### Real-world examples

```bash
# Typical build
fpx -DDEBUG=1 -DMPI -I./include src/program.F90 -o build/program.f90

# Cross-platform build with different features
fpx -DUSE_OPENMP -DUSE_MPI=3 src/app.F90 -o app_openmp_mpi.f90

# Clean file with no comments
fpx -DNO_DEBUG src/messy.F90 -o clean.f90

# Interactive playground
fpx
[in]  #define MSG(x) print *, "=> ", x
[out] 
[in]  call MSG("Hello")
[out] call print *, "=> ", "Hello"
```

---

## 2. Using fpx as an Embedded Library in Your Own Fortran Program

You can call the preprocessor directly from any Fortran program — ideal for build systems, code generators, or tools.

Everything you need is in the module `fpx_parser`.

### Minimal example – preprocess a file to another file

```fortran
program demo_embed
    use fpx_parser
    implicit none

    call preprocess("input.F90", "output.f90")
    print *, "Preprocessing finished → output.f90"
end program
```

### Full control – predefined macros, include paths, etc.

```fortran
program my_builder
    use fpx_parser
    use fpx_global
    use fpx_macro, only: add, macro

    implicit none

    ! Include directories
    global%includedir = [ "./include", "../common", "/usr/local/fortran" ]

    ! Predefine macros
    call add(global%macros, macro("DEBUG",        "1"))
    call add(global%macros, macro("VERSION",      '"2.1.0"'))
    call add(global%macros, macro("USE_MPI",      "1"))
    call add(global%macros, macro("MAX_THREADS,  "8"))

    ! Optional settings
    global%expand_macros    = .true.   ! default
    global%exclude_comments = .true.   ! strip comments

    call preprocess("src/main.F90", "build/main.f90")
    print *, "Done!"
end program
```

### Preprocess from memory (string → string)

```fortran
program memory_preprocess
    use fpx_parser
    character(:), allocatable :: source
    integer :: unit

    source = &
    "#define GREET Hello"//new_line('a')//&
    "program hello"//new_line('a')//&
    "  print *, GREET"//new_line('a')//&
    "end program"

    open(newunit=unit, status='scratch', action='write')
    write(unit,'(A)') source
    rewind(unit)

    call preprocess(unit, output_unit)   ! prints expanded code
    close(unit)
end program
```

### Useful public interfaces

| Interface                        | Purpose                                    |
|----------------------------------|--------------------------------------------|
| `preprocess(file, [output])`     | File → file or stdout                      |
| `preprocess(unit_in, unit_out)`  | Unit → unit (most flexible)                |
| `global%macros`                  | Add/remove macros programmatically         |
| `global%includedir`              | Include paths                              |
| `global%expand_macros`           | Turn macro expansion on/off                |
| `global%exclude_comments`        | Strip comments from output                 |

---

### Summary – Which way should you use fpx?

| You want…                                 | Use…                                      |
|-------------------------------------------|-------------------------------------------|
| Just preprocess a file quickly            | Command line `fpx file.F90 -o out.f90`     |
| Build-system integration, predefined macros| CLI with `-D` / `-I`                      |
| Full control inside a Fortran tool        | Library mode (`use fpx_parser`)           |
| Generate code on the fly from strings     | Unit-to-unit or temporary files           |

Enjoy a clean, fast, pure-Fortran preprocessor that just works — no external tools, no Python, no Perl, no headaches.

**Happy Fortran hacking!**
```

Just copy the entire block above and paste it — it’s a single, perfectly formatted Markdown document.
```