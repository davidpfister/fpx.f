# A bit of History {#introduction}

[TOC]

Fortran has powered simulations of galaxies, weather systems, and quantum phenomena for over seven decades. Its enduring strength lies in its clarity, performance, and mathematical soul—qualities that resonate deeply with its community of developers. Yet, nestled within this ecosystem is a contentious tool: the preprocessor. From its ad hoc beginnings in the 1970s to its modern incarnations in tools like `cpp`, `fpp`, and `fypp`, preprocessing has been both a lifeline and a lightning rod for Fortran developers. It enables portability across diverse platforms, conditional compilation for debugging, and code generation for complex libraries—capabilities critical to Fortran’s role in high-performance computing. But it also sparks fierce debate, with many Fortraners decrying its tendency to obscure code, disrupt the language’s elegant simplicity, and introduce bugs that haunt scientific precision. This article explores the pivotal uses of preprocessing in Fortran, delving into the passionate love-hate relationship that defines its place in the community—a tug-of-war between pragmatic necessity and a purist’s devotion to Fortran’s unadulterated clarity.

## A Brief History of Preprocessing

Fortran, born in the 1950s for scientific and numerical computing, was designed for clarity and performance on early computers. Preprocessing was not part of its original vision. As Fortran evolved, the need for portability, code reuse, and conditional compilation grew, particularly in large-scale scientific projects. This led to the adoption of preprocessing tools, though their integration into Fortran’s ecosystem has been uneven and controversial. Below is a concise history of preprocessing in Fortran, culminating in the notable attempt to standardize it with CoCo.

### Early Days: Ad Hoc Preprocessing (1950s–1970s)
In Fortran’s infancy (Fortran I, II, IV), preprocessing was virtually nonexistent. Developers relied on manual code edits or rudimentary scripts to handle tasks like platform-specific tweaks. Early computers varied widely in architecture, so scientists often customized code by hand for each system—a tedious process. Some used external tools, like simple text processors, to automate repetitive changes, but these were bespoke and non-standard. Fortran 66 and 77, with their rigid structure, offered no built-in preprocessing capabilities, leaving developers to cobble together solutions.

### Rise of External Preprocessors (1980s)
By the 1980s, Fortran 77 was the workhorse of scientific computing, and large projects—like climate models or finite element simulations—demanded portability across diverse hardware (e.g., Cray, VAX, IBM). The C preprocessor (`cpp`), developed for C, became a popular stopgap. Its `#define`, `#ifdef`, and `#include` directives allowed Fortran developers to write flexible code for multiple platforms. Files with `.F` or `.F77` extensions signaled preprocessing, distinguishing them from raw `.f` files. However, `cpp` was a imperfect fit: its C-centric syntax clashed with Fortran’s column-based formatting, and it could mangle Fortran’s fixed-form source, leading to errors. Dedicated Fortran preprocessors, like `fpp`, emerged to address these issues, offering better integration but lacking universal adoption.

### Fortran 90/95: Reduced Need, Persistent Use (1990s)
Fortran 90 introduced modules, parameterized types, and dynamic memory, giving developers native tools for modularity and portability. These features reduced reliance on preprocessing for tasks like code reuse or constant definition. For example, modules replaced many `#include` use cases, and `PARAMETER` statements handled constants better than `#define`. Still, preprocessing persisted in large codebases, especially for conditional compilation (e.g., enabling/disabling debug code) or legacy Fortran 77 projects. Tools like `cpp` and `fpp` remained common, though their use was often seen as a necessary evil due to debugging challenges and code obfuscation.

### The CoCo Standardization Attempt (Late 1990s–Early 2000s)
By the late 1990s, the Fortran community recognized preprocessing’s utility but also its chaos. Different preprocessors (`cpp`, `fpp`, custom tools) produced inconsistent behavior, and there was no standard way to write portable, preprocessable Fortran code. This led to the development of **CoCo** (Conditional Compilation), a proposed standard for Fortran preprocessing, spearheaded by the ISO/IEC Fortran committee (J3).

CoCo aimed to integrate preprocessing directly into the Fortran language, defining a native syntax for directives like conditional compilation, macro expansion, and file inclusion. Unlike `cpp`, CoCo was designed with Fortran’s structure in mind, respecting its free- and fixed-form source and avoiding C’s pitfalls. Key goals included:
- **Portability**: Ensure code behaved consistently across compilers.
- **Simplicity**: Provide a minimal set of directives tailored to Fortran’s needs (e.g., scientific computing).
- **Clarity**: Preserve Fortran’s readable, explicit style, avoiding macro-heavy complexity.

CoCo’s syntax used directives prefixed with `??`, such as `??IF`, `??DEFINE`, or `??INCLUDE`, to distinguish them from Fortran code. For example:
```fortran
??IF (DEBUG)
  PRINT *, "Debug mode active"
??ENDIF
```
This allowed conditional compilation without external tools, and compilers could process it natively.

However, CoCo never made it into the Fortran standard. Several factors contributed:
- **Community Pushback**: Many Fortran developers disliked preprocessing altogether, viewing it as a C-like intrusion that muddied the language’s clarity. They argued that Fortran 90/95/2003 features (e.g., modules, derived types) already addressed most use cases.
- **Implementation Challenges**: Standardizing a preprocessor meant every Fortran compiler had to support CoCo, a burden for vendors already grappling with modern Fortran’s complexity.
- **Existing Tools**: `cpp` and `fpp` were entrenched, and many developers saw no need for a new standard when workarounds existed.
- **Scope Concerns**: Some feared CoCo would bloat the language, encouraging macro-heavy code that clashed with Fortran’s scientific focus.

By the early 2000s, CoCo faded as a proposal. The Fortran 2003 standard focused on object-oriented features and interoperability (e.g., with C), sidelining preprocessing standardization.

### Modern Era: Preprocessing Today (2000s–2025)
Today, preprocessing in Fortran remains unstandardized but widely used. `cpp` and `fpp` are still common, especially in high-performance computing (HPC) projects like LAPACK, PETSc, or WRF, hosted on platforms like GitHub. Newer tools, like `fypp` (a Python-based preprocessor), have gained traction for their flexibility, particularly in projects like the Fortran Standard Library. Fortran 2008, 2018, and the upcoming 2023 standard introduced features like submodules and enhanced generics, further reducing preprocessing needs, but legacy code and cross-platform projects keep preprocessors relevant.

The CoCo attempt left a legacy: it highlighted the community’s ambivalence toward preprocessing. While some developers value its power, others see it as a last resort, preferring Fortran’s native constructs. The lack of a standard means preprocessing remains a fragmented practice, with tools and conventions varying by project.

Preprocessing in Fortran evolved from ad hoc scripts to external tools like `cpp`, driven by the need for portability and flexibility in scientific computing. The CoCo initiative sought to bring order by standardizing preprocessing, but resistance from a community valuing simplicity, combined with practical hurdles, led to its demise. Today, preprocessing endures as a pragmatic tool in Fortran’s ecosystem, neither fully embraced nor abandoned, reflecting the language’s balance between tradition and adaptation.

In modern Fortran (referring to standards like Fortran 2003, 2008, 2018, and the upcoming 2023), preprocessing remains a practical tool despite the language’s evolution toward native constructs that reduce its necessity. While modern Fortran offers robust features like modules, derived types, and submodules, preprocessing is still widely used in specific scenarios, particularly in large-scale scientific, high-performance computing (HPC), and legacy projects. Below, I’ll expand on the **most common uses of preprocessing in modern Fortran**, focusing on their practical applications, prevalence, and integration with contemporary development practices, as seen in open-source projects and industry.

## Most frequent Usage

### Conditional Compilation
**What It Is**: Preprocessing directives like `#ifdef`, `#ifndef`, `#else`, and `#endif` allow developers to include or exclude code blocks based on predefined conditions, typically set at compile time.

**Common Uses**:
- **Debug vs. Production Code**:
  - Developers use conditional compilation to toggle debugging features, such as verbose logging or array bounds checking, which are enabled in development but stripped out for performance in production builds.
  - Example:
    ```fortran
    #ifdef DEBUG
      PRINT *, "Entering subroutine X with N =", N
      CALL CHECK_BOUNDS(array, N)
    #endif
    !...
    ```
- **Platform-Specific Code**:
  - Scientific software often runs on diverse systems—Linux clusters, GPUs, or supercomputers like those using Intel, AMD, or Cray architectures. Preprocessing lets developers tailor code to specific compilers, hardware, or libraries (e.g., MPI vs. OpenMP).
  - Example:
    ```fortran
    #ifdef USE_MPI
      CALL MPI_BCAST(data, size, MPI_DOUBLE, 0, comm, ierr)
    #else
      data_local = data
    #endif
    !...
    ```
- **Feature Toggles**:
  - In large projects, preprocessing enables or disables optional features (e.g., experimental algorithms or legacy compatibility) without maintaining separate codebases.
  - Example: A numerical solver might include a new algorithm only if a flag like `NEW_SOLVER` is defined.

**Prevalence**:
- Conditional compilation is arguably the most widespread preprocessing use in modern Fortran, especially in HPC libraries and applications (e.g., PETSc, WRF, or SPECFEM3D). It’s critical for projects on GitHub that target multiple environments or maintain long-lived codebases.
- It’s less common in small, single-purpose programs or academic code, where native Fortran constructs suffice.

**Why It Persists**:
- Modern Fortran lacks a direct equivalent to conditional compilation. While `IF` statements can handle runtime conditions, they don’t exclude code from compilation, which impacts binary size and performance optimization.

### Portability Across Systems
**What It Is**: Preprocessing helps write code that adapts to different compilers, operating systems, or hardware by defining platform-specific macros or including system-dependent code.

**Common Uses**:
- **Compiler-Specific Directives**:
  - Different Fortran compilers (e.g., GNU `gfortran`, Intel `ifort`, NVIDIA `nvfortran`) support unique extensions or optimizations. Preprocessing ensures compatibility.
  - Example:
    ```fortran
    #ifdef __GFORTRAN__
      call GETENV("PATH", path_var)
    #else
      call GET_ENVIRONMENT_VARIABLE("PATH", path_var)
    #endif
    !...
    ```
- **Library Dependencies**:
  - Scientific codes often link to external libraries like BLAS, LAPACK, or FFTW. Preprocessing manages variations in library availability or interfaces.
  - Example:
    ```fortran
    #ifdef HAVE_MKL
      call DGEMM('N', 'N', m, n, k, alpha, A, m, B, k, beta, C, m)
    #else
      call CUSTOM_GEMM(m, n, k, A, B, C)
    #endif
    !...
    ```
- **Precision Control**:
  - Fortran’s `KIND` system allows flexible numeric precision, but preprocessing is used to enforce consistent precision across platforms or to switch between single/double precision for performance.
  - Example:
    ```fortran
    #ifdef SINGLE_PRECISION
      INTEGER, PARAMETER :: wp = KIND(1.0)
    #else
      INTEGER, PARAMETER :: wp = KIND(1.0D0)
    #endif
      REAL(wp) :: array(N)
    ```

**Prevalence**:
- Portability is a top use case in open-source Fortran projects, especially those in computational physics, climate modeling, or astrophysics (e.g., MPAS, LAMMPS). Repositories on GitHub often include build scripts (CMake, Makefile) that define preprocessor flags for different systems.
- It’s less critical in controlled environments (e.g., a single supercomputer) but vital for software distributed across institutions.

**Why It Persists**:
- Despite Fortran’s `ISO_C_BINDING` and improved interoperability, hardware and library ecosystems remain fragmented. Preprocessing bridges these gaps more efficiently than rewriting code for each target.

### File Inclusion
**What It Is**: The `#include` directive inserts external files into the source code, often for shared definitions, constants, or interfaces.

**Common Uses**:
- **Shared Constants and Types**:
  - Large projects define constants, derived types, or interface blocks in a single file, included across multiple source files to ensure consistency.
  - Example:
    ```fortran
    #include "constants.h"
    REAL(wp), PARAMETER :: gravity = GRAVITY_CONSTANT
    ```
    Where `constants.h` might contain:
    ```fortran
    #define GRAVITY_CONSTANT 9.80665_wp
    ```
- **Legacy Code Integration**:
  - Older Fortran 77 codebases, lacking modules, use `#include` to share `COMMON` blocks or subroutine declarations.
  - Example:
    ```fortran
    #include "common_blocks.inc"
    ```
- **Interface Definitions**:
  - For codes interfacing with C or other languages, `#include` pulls in header-like files defining data structures or function signatures.

**Prevalence**:
- File inclusion is common in legacy-heavy projects or those maintaining Fortran 77/90 compatibility (e.g., numerical libraries like SLATEC). Modern projects prefer modules, but `#include` persists in mixed-language or older HPC codes.
- On GitHub, you’ll see it in projects like netCDF-Fortran or older simulation frameworks, though its use is declining.

**Why It Persists**:
- Modules are superior for modularity, but `#include` is entrenched in legacy workflows and simpler for small, shared snippets. It’s also used when interfacing with C-style build systems.

### Macro Definitions for Code Reuse
**What It Is**: Macros (`#define`) create reusable code snippets or constants, reducing duplication or simplifying complex expressions.

**Common Uses**:
- **Constant Definitions**:
  - While Fortran’s `PARAMETER` is preferred, macros define constants in preprocessed code, especially for conditional values.
  - Example:
    ```fortran
    #define MAX_ITER 1000
    INTEGER :: i
    DO i = 1, MAX_ITER
      ...
    END DO
    ```
- **Simplified Syntax**:
  - Macros streamline repetitive or verbose code, like array indexing or mathematical formulas in numerical algorithms.
  - Example:
    ```fortran
    #define IDX(i,j,n) ((i-1)*n + j)
    array(IDX(i,j,N)) = value
    ```
- **Code Generation**:
  - Advanced preprocessors like `fypp` generate boilerplate code, such as loops or type-specific routines, for generic programming.
  - Example (in `fypp`):
    ```fortran
    {% for type in ['REAL', 'INTEGER'] %}
    SUBROUTINE process_${type}$(data)
      ${type}$ :: data(:)
      ...
    END SUBROUTINE
    {% endfor %}
    ```

**Prevalence**:
- Basic macro use is moderately common, especially in performance-critical code where inline calculations benefit from shorthand. `fypp` is gaining traction in newer projects like the Fortran Standard Library (stdlib) for its code-generation capabilities.
- It’s less frequent in small or purely modern Fortran projects, where intrinsic features like procedures or generics suffice.

**Why It Persists**:
- Macros offer concise solutions for repetitive patterns, and tools like `fypp` extend this to template-like programming, which Fortran’s generics don’t fully replicate. However, overuse is frowned upon due to readability concerns.

### Interfacing with Build Systems and External Tools
**What It Is**: Preprocessing integrates Fortran code with build systems (e.g., CMake, Autotools) or external tools by defining flags or generating code.

**Common Uses**:
- **Build-Time Configuration**:
  - Build scripts set preprocessor flags to configure code for specific environments, libraries, or optimizations.
  - Example (CMake):
    ```cmake
    add_definitions(-DHAVE_HDF5)
    ```
    Used in Fortran as:
    ```fortran
    #ifdef HAVE_HDF5
      CALL write_hdf5_file(data)
    #endif
    !...
    ```
- **Automated Code Generation**:
  - Tools like `fypp` or custom scripts generate Fortran code for specific cases (e.g., different precisions or dimensions), embedded in the build process.
- **Testing Frameworks**:
  - Preprocessing enables test-specific code, like mocking I/O or enabling assertions, in projects using frameworks like pFUnit.

**Prevalence**:
- This is highly common in professional or open-source Fortran projects with complex build systems (e.g., WRF, Trilinos). On GitHub, repos with CMake or Makefile setups often include preprocessor flags.
- It’s rare in standalone scripts or academic code without formal build processes.

**Why It Persists**:
- Modern build systems rely on preprocessing to manage complexity, and Fortran’s role in HPC demands integration with tools like MPI, CUDA, or HDF5, where preprocessing simplifies configuration.