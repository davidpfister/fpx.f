# FXP: Fortran extended preprocessing

## Compatibility with cpp/fpp

You can explicitly run fpp in the following ways:

- On the command line, use the ifort command with the fpp compiler option. By default, the specified files are then compiled and linked. To retain the intermediate (.i or .i90) file, specify the [Q]save-temps compiler option.

- On the command line, use the fpp command. In this case, the compiler is not invoked. When using fpp on the command line, you need to specify the input file and the intermediate (.i or .i90) output file. For more information, type fpp -help (Linux and macOS) or fpp /help (Windows) on the command line.

- In the Microsoft Visual Studio* IDE, set the Preprocess Source File option to Yes in the Fortran Preprocessor Option Category. To retain the intermediate files, add /Qsave-temps to Additional Options in the Fortran Command Line Category.

The following table lists some common cpp features that are supported by fpp and some common cpp features that are not supported.

| Supported cpp features:| Unsupported cpp features:|
|:--|:--|
|#define, #undef, #ifdef, #ifndef, #if, #elif, #else, #endif, #include, #error, #warning, #line|#pragma and #ident|
|# (stringsize) and ## (concatenation) operators| spaces or tab characters preceding the initial "#" character, # followed by empty line|
|! as negation operator|\ backslash-newline|

Unlike cpp, fpp does not merge continued lines into a single line when possible.

You do not usually need to specify preprocessing for Fortran source programs unless your program uses fpp preprocessing commands, such as those listed above.

> CAUTION:

Using a preprocessor that does not support Fortran can damage your Fortran code.
For example, consider the following code:
```
FORMAT (\\I4)
```
In this case, most C++ and C preprocessors will change the meaning of the program by interpreting the double backslash "\" as end-of-record.

### fpp Preprocessor Directives
All fpp preprocessor directives start with the number sign (#) as the first character on a line. White space (blank or tab characters) can appear after the initial "#" for indentation.

fpp preprocessor directives can be placed anywhere in source code, but they usually appear before a Fortran continuation line. However, fpp preprocessor directives within a macro call can not be divided among several lines by means of continuation symbols.

fpp preprocessor directives can be grouped according to their purpose.

Preprocessor directives for string substitution
The following fpp preprocessor directives cause substitutions in your program:

|Preprocessor Directive|Result|
|:--|:--|
|`__FILE__`|Replaces a string with the input file name (a character string literal).|
|`__LINE__`|Replaces a string with the current line number in the input file (an integer constant).|
|`__DATE__`|Replaces a string with the date that fpp processed the input file (a character string literal in the form Mmm dd yyyy).|
|`__TIME__`|Replaces a string with the time that fpp processed the input file (a character string literal in the form hh:mm:ss).|
|`__TIMESTAMP__`|Replaces a string with the timestamp that fpp processed the input file (a character string literal in the form "day date time year", where day is a 3-letter abbreviation, date is Mmm dd, time is hh:mm:ss and year is yyyy).|

#### Preprocessor directive for inclusion of external files
To include external files, preprocessor directive #include can be specified in one of the following forms:
```
#include "filename"
```

```
#include <filename>
```
#include reads in the contents of the named file into the specified or default location in the source. The lines read in from the file are processed by fpp as if they were part of the current file.

When the <filename> notation is used, the compiler only searches for the file name in the standard "include" directories. For more information, see the fpp preprocessor options Idir and Ydir options. No additional tokens are allowed on the directive line after the final '"' or ">".

For #include "filename", file names are searched for in the following order:

- In the directory in which the source file resides
- In the directories specified by the I or Y preprocessor option
- In the default directory

For #include <filename>, filenames are searched for in the following order:
- In the directories specified by the I or Y preprocessor option
- In the default directory

#### Preprocessor directive for line control
The preprocessor directive #line-number generates line control information for the compiler. It takes the following form:
```
#line-number "filename"
```
#line-number is an integer constant that is the line number of the next line. "filename" is the name of the file containing the line. 
If "filename" is not provided, the current file name is assumed.

#### Preprocessor directive for fpp variable and macro definitions
The preprocessor directive #define can be used to define both simple string variables and more complicated macros. It can take two forms.

Definition of an fpp variable:

> #define name token-string

In the above, occurrences of name in the source file will be replaced by token-string.

- Definition of an fppmacro:
```
#define name(argument[,argument] ... ) token-string
```
In the above, occurrences of the macro name followed by the comma-separated list of actual arguments within parentheses, will be replaced by token-string. Each occurrence of argument in token-string is replaced by the token sequence representing the corresponding "actual" argument in the macro call.

An error occurs if the number of macro call arguments is not the same as the number of arguments in the corresponding macro definition. For example, consider this macro definition:
```
#define INTSUB(m, n, o) call mysub(m, n, o)
```
Any use of the macro INTSUB must have three arguments. In macro definitions, spaces between the macro name and the open parenthesis "(" are prohibited to prevent the directive from being interpreted as an fpp variable definition with the rest of the line beginning with the open parenthesis "(" being interpreted as its token-string.

An fpp variable or macro definition can be of any length and is limited only by the newline symbol. It can be defined in multiple lines by continuing it to the next line with the insertion of "\". For example:
```
#define long_macro_name(x,\
   y) x*y
```
The occurrence of a newline without a macro-continuation signifies the end of the macro definition.

The scope of a definition begins from the #define and encloses all the source lines (and source lines from #include files) to the end of the current file, except for:

- Files included by Fortran INCLUDE statements
- fpp and Fortran comments
- Fortran IMPLICIT statements that specify a single letter
- Fortran FORMAT statements
- Numeric, typeless, and character constants

#### Preprocessor directive for undefining a macro
The preprocessor directive #undef takes the following form:
```
#undef name
```
This preprocessor directive removes any definition for name produced by the D preprocessor option, the #define preprocessor directive, or by default. No additional tokens are permitted on the directive line after name.

If name has not been previously defined, then #undef has no effect.

#### Preprocessor directive for macro expansion
If, during expansion of a macro, the column width of a line exceeds column 72 (for fixed format) or column 132 (for free format), fpp inserts appropriate Fortran continuation lines.

For fixed format, there is a limit on macro expansions in label fields (positions 1-5):

- A macro call (together with possible arguments) should not extend beyond column 5.
- A macro call whose name begins with one of the Fortran comment symbols is considered to be part of a comment.
- A macro expansion may produce text extending beyond column 5. In this case, a warning will be issued.

In fixed format, when the fpp preprocessor option Xw has been specified, an ambiguity may occur if a macro call occurs in a statement position and a macro name begins or coincides with a Fortran keyword. For example, consider the following:

```
#define callp(x)   call f(x)
  call p(0)
```

In this case, fpp cannot determine how to interpret the callp token sequence. It could be considered to be a macro name. The current implementation does the following:

- The longer identifier is chosen (callp in this case)
- From this identifier the longest macro name or keyword is extracted
- If a macro name has been extracted, a macro expansion is performed. If the name begins with some keyword, fpp issues an appropriate warning
- The rest of the identifier is considered a whole identifier

In the previous example, the macro expansion is performed and the following warning is produced:

> warning: possibly incorrect substitution of macro callp

This situation appears only when preprocessing fixed-format source code and when the space symbol is not interpreted as a token delimiter.

In the following case, a macro name coincides with a beginning of a keyword:

```
#define INT  INTEGER*8
              INTEGER k
```
The INTEGER keyword will be found earlier than the INT macro name. There will be no warning when preprocessing such a macro definition.

#### Preprocessor directives for conditional selection of source text
The following three preprocessor directives are conditional constructs that you can use to select source text.
```
#if preprocessor directive
```
When #if is specified, subsequent lines up to the matching #else, #elif, or #endif preprocessor directive appear in the output only if condition evaluates to .TRUE..

The following shows an example:
```
#if condition_1
  block_1
#elif condition_2
  block_2
#elif ...
#else
  block_n
#endif
#ifdef preprocessor directive
```
When #ifdef is specified, subsequent lines up to the matching #else, #elif, or #endif preprocessor directive appear in the output only if name has been defined, either by a #define preprocessor directive or by the D preprocessor option, with no intervening #undef preprocessor directive. No additional tokens are permitted on the preprocessor directive line after name.

The following shows an example:
```
#ifdef name
  block_1
#elif condition
  block_2
#elif ...
#else
  block_n
#endif
#ifndef preprocessor directive
```
When #ifndef is specified, subsequent lines up to the matching #else, #elif, or #endif preprocessor directive appear in the output only if name has not been defined, or if its definition has been removed with an #undef preprocessor directive. No additional tokens are permitted on the directive line after name.

The following shows an example:
```
#ifndef name
  block_1
#elif condition
  block_2
#elif ...
#else
  block_n
#endif
```
The #else, #elif, or #endif preprocessor directives are optional. They can be used in the above preprocessor directives.

Subsequent lines up to the matching #else, #elif, or #endif appear in the output only if all of the following occur:

- The condition in the preceding #if directive evaluates to .FALSE., the name in the preceding #ifdef directive is not defined, or the name in the preceding #ifndef directive is defined
- The conditions in all of the preceding #elif directives evaluate to .FALSE.
- The condition in the current #elif evaluates to .TRUE.

Any condition allowed in an #if directive is allowed in an #elif directive. Any number of #elif directives may appear between an #if, #ifdef, or #ifndef directive and a matching #else or #endif directive.

#### Conditional expressions

condition_1, condition_2, etc. are logical expressions involving fpp constants, macros, and intrinsic functions. The following items are permitted in conditional expressions:

- C language operations: <, >, ==, !=, >=, <=, +, -, /, *, %, <<, >>, &, ~, |, &&, and ||
They are interpreted by fpp in accordance to the C language semantics. This facility is provided for compatibility with "old" Fortran programs using cpp.
- Fortran language operations: .AND., .OR., .NEQV., .XOR., .EQV., .NOT., .GT., .LT., .LE., .GE., .NE., .EQ., ** (power).
- Fortran logical constants: .TRUE. , .FALSE.
- The fpp intrinsic function "defined": defined(name) or defined name, which returns .TRUE. if name is defined as an fpp variable or a macro. It returns .FALSE. if the name is not defined.

#ifdef is shorthand for #if defined(name) and #ifndef is shorthand for #if .not. defined(name).

Only these items, integer constants, and names can be used within a constant expression.

A name that has not been defined with the D preprocessor option, a #define preprocessor directive, or defined by default, has a value of zero.

The C operation != (not equal) can be used in the #if or #elif preprocessor directive, but not in the #define preprocessor directive, where the symbol ! is considered to be the Fortran comment symbol by default.
