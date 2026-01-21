<a id="readme-top"></a>

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <h3 align="center">fpx.f</h3>

  <p align="center">
    A Fortran preprocessor in Fortran for modern Fortran.
    <br />
    <a href="https://github.com/davidpfister/fpx.f"><strong>Explore the project »</strong></a>
    <br />
  </p>
</div>



<!-- TABLE OF CONTENTS -->
[TOC]

# Introduction
<!-- ABOUT THE PROJECT -->
## About the Project

Fortran, the venerable language of scientific computing, has powered simulations of galaxies, weather systems, and quantum phenomena for over seven decades. Its enduring strength lies in its clarity, performance, and mathematical soul, qualities that resonate deeply with its community of developers. Yet, nestled within this ecosystem is a contentious tool: the preprocessor. From its ad hoc beginnings in the 1970s to its modern incarnations in tools like `cpp`, `fpp`, and `fypp`, preprocessing has been both a lifeline and a lightning rod for Fortran developers. 

It enables portability across diverse platforms, conditional compilation for debugging, and code generation for complex libraries. These capabilities are critical to Fortran’s role in high-performance computing. But it also sparks fierce debate, with many Fortraners decrying its tendency to obscure code, disrupt the language’s elegant simplicity, and introduce bugs.   

This project aims at providing a *simple*, *embeddable*, *open-source* preprocessor written in modern Fortran. `fpx` is mostly compliant to a C preprocessor, fine-tuned for the specificity of the Fortran language. 
`fpx` is an embeddable preprocessor. It can be used as a command-line tool or directly embedded into any solution with the module `fpx_parser`.

* [![fpm][fpm]][fpm-url]
* [![ifort][ifort]][ifort-url]
* [![gfortran][gfortran]][gfortran-url]

`fpx` supports: 
- conditional compilation with `#if`, `#ifdef`, `#ifndef`, `#elif`, `#else`,`#endif`
- simple macros and function like macros with `#define`, `#undef`, `defined` and `!defined`
- simple arithmetic and bitwise operations with `+`, `-`, `*`, `**`, `/`, `>`, `<`, `>=`, `=<`, `||`, `&&`, `|`, `^`, `&`, `!` and `~`.
- include files with `#include`
- variadic macros with `__VA_ARGS__`, and `__VA_OPT__`, 
- build-in macros as `__LINE__`, `__FILE__`, `__FILENAME__`, `__TIME__`, `__DATE__`, `__TIMESTAMP__`
- stringification `#` and concatenation `##`
- and more...       
<!-- GETTING STARTED -->
## Getting Started

### Requirements

To build that library you need

- a Fortran 2008 compliant compiler, or better, a Fortran 2018 compliant compiler (Intel Fortran and gfortran compilers are known to work well for _fpx.f_).

The following compilers are tested on the default branch of _fpx.f_:

<center>

| Name |	Version	| Platform	| Architecture |
|:--:|:--:|:--:|:--:|
| GCC Fortran (MinGW) | 14 | Windows 10 | x86_64 |
| Intel oneAPI classic	| 2021.5	| Windows 10 |	x86_64 |

</center>

- a preprocessor. _fpx.f_ uses some preprocessor macros. It is known to work both with intel `fpp` and `cpp`. The goal is for `fpx` to preproces itself at some point. In particular, the console line application uses the header file ['app.inc'](https://github.com/davidpfister/fortiche/tree/master/src/app) from the ['fortiche'](https://github.com/davidpfister/fortiche) repo. 

Unit test rely on the the header file [`assertion.inc`](https://github.com/davidpfister/fortiche/tree/master/src/assertion). Since the whole framework fits in a single file, it has been added directly to the repo. 

Linting, indentation, and styling is done with [fprettify](https://github.com/fortran-lang/fprettify) with the following settings
```bash
   fprettify './src/' -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations
```

### Installation

#### Get the code
```bash
   git clone https://github.com/davidpfister/fpx.f
   cd fpx.f
```

#### Build with fpm

The repo can be build using _fpm_
```bash
   fpm build
```
For convenience, the  repo also contains a response file that can be invoked as follows: 
```bash
   fpm @build
```
(For the Windows users, that command does not work in Powershell since '@' is a reserved symbol. One should use the '--%' as follows: `fpm --% @build`.
This is linked to the following [issue](https://github.com/urbanjost/M_CLI2/issues/19))

Building with ifort requires to specify the compiler name (gfortran by default)
```bash
   fpm @build --compiler ifort
```
Alternatively, the compiler can be set using fpm environment variables.
```bash
   set FPM_FC=ifort
```

Besides the build command, several commands are also available:
```bash
@pretiffy
system fprettify .\src\ -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations

@clean
option clean --all

@rebuild
system rmdir /s /q build
option build

@build
option build

@test
options test --flag 'D_QUIET'

@doc
option clean --all
system cd ./.dox & doxygen ./Doxyfile.in & cd ..
```

The settings to the cpp preprocessor are specified in the file. 

```toml
[preprocess]
cpp.suffixes = ["F90", "f90"]
cpp.macros = ["_FPM"]
```
The `_FPM` macro is used to differentiate the build when compiling with _fpm_ or _Visual Studio_. This is mostly present to adapt the hard coded paths that differs in both cases.

#### Build with Visual Studio 2019

The project was originally developed on Windows with Visual Studio 2019. The repo contains the solution file (_Fpx.f.sln_) to get you started with Visual Studio 2019. 


<!-- USAGE EXAMPLES -->
## Usage

### Command line
The preprocessor `fpx` can be used from the command-line using your favorite shell. 
The following options are available:
<center>

|Option|Definition|
|:--|:--| 
|-D\<macro>|Define a \<macro> with no value.| 
|-D\<macro>=\<val>|Define a \<macro> with \<val> as its value.| 
|-U\<macro>|Undefine \<macro>'|
|-I\<dir>|Add \<dir> to the end of the global include paths.| 
|-h, -?|Display this help.|
|-o|Output file path with name and extension.|
|-v|Display the version of the program.|

</center>

Using the file preprocessor could not be easier. The function simply takes as arguments the input and output file paths. 

### Embedded
```fortran
program test
    use fpx_parser
    
    call preprocess('tests/input.in', 'tests/output.out')
end program
```

_For more examples, please refer to the [Documentation](https://davidpfister.github.io/fpx.f/index.html)_

<!-- CONTRIBUTING -->
### Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**. So, thank you for considering contributing to _fpx.f_.
Please review and follow these guidelines to make the contribution process simple and effective for all involved. In return, the developers will help address your problem, evaluate changes, and guide you through your pull requests.

By contributing to _fpx.f_, you certify that you own or are allowed to share the content of your contribution under the same license.

### Style

Please follow the style used in this repository for any Fortran code that you contribute. This allows focusing on substance rather than style.

### Reporting a bug

A bug is a *demonstrable problem* caused by the code in this repository.
Good bug reports are extremely valuable to us—thank you!

Before opening a bug report:

1. Check if the issue has already been reported
   ([issues](https://github.com/davidpfister/fpx.f/issues)).
2. Check if it is still an issue or it has been fixed?
   Try to reproduce it with the latest version from the default branch.
3. Isolate the problem and create a minimal test case.

A good bug report should include all information needed to reproduce the bug.
Please be as detailed as possible:

1. Which version of _fpx.f_ are you using? Please be specific.
2. What are the steps to reproduce the issue?
3. What is the expected outcome?
4. What happens instead?

This information will help the developers diagnose the issue quickly and with
minimal back-and-forth.

### Pull request

If you have a suggestion that would make this project better, please create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!
1. Open a [new issue](https://github.com/davidpfister/fpx.f/issues/new) to
   describe a bug or propose a new feature.
   Refer to the earlier sections on how to write a good bug report or feature    request.
2. Discuss with the developers and reach consensus about what should be done about the bug or feature request.
   **When actively working on code towards a PR, please assign yourself to the
   issue on GitHub.**
   This is good collaborative practice to avoid duplicated effort and also inform others what you are currently working on.
3. Create your Feature Branch (```git checkout -b feature/AmazingFeature```)
4. Commit your Changes (```git commit -m 'Add some AmazingFeature'```)
5. Push to the Branch (```git push origin feature/AmazingFeature```)
6. Open a Pull Request with your contribution.
   The body of the PR should at least include a bullet-point summary of the
   changes, and a detailed description is encouraged.
   If the PR completely addresses the issue you opened in step 1, include in
   the PR description the following line: ```Fixes #<issue-number>```. If your PR implements a feature that adds or changes the behavior of _fpx.f_,
   your PR must also include appropriate changes to the documentation and associated units tests.

In brief, 
* A PR should implement *only one* feature or bug fix.
* Do not commit changes to files that are irrelevant to your feature or bug fix.
* Smaller PRs are better than large PRs, and will lead to a shorter review and
  merge cycle
* Add tests for your feature or bug fix to be sure that it stays functional and useful
* Be open to constructive criticism and requests for improving your code.


<!-- LICENSE -->
## License

Distributed under the MIT License.

<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/davidpfister/fpx.f.svg?style=for-the-badge
[contributors-url]: https://github.com/davidpfister/fpx.f/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/davidpfister/fpx.f.svg?style=for-the-badge
[forks-url]: https://github.com/davidpfister/fpx.f/network/members
[stars-shield]: https://img.shields.io/github/stars/davidpfister/fpx.f.svg?style=for-the-badge
[stars-url]: https://github.com/davidpfister/fpx.f/stargazers
[issues-shield]: https://img.shields.io/github/issues/davidpfister/fpx.f.svg?style=for-the-badge
[issues-url]: https://github.com/davidpfister/fpx.f/issues
[license-shield]: https://img.shields.io/github/license/davidpfister/fpx.f.svg?style=for-the-badge
[license-url]: https://github.com/davidpfister/fpx.f/master/LICENSE
[gfortran]: https://img.shields.io/badge/gfortran-000000?style=for-the-badge&logo=gnu&logoColor=white
[gfortran-url]: https://gcc.gnu.org/wiki/GFortran
[ifort]: https://img.shields.io/badge/ifort-000000?style=for-the-badge&logo=Intel&logoColor=61DAFB
[ifort-url]: https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html
[fpm]: https://img.shields.io/badge/fpm-000000?style=for-the-badge&logo=Fortran&logoColor=734F96
[fpm-url]: https://fpm.fortran-lang.org/
