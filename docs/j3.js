var j3 =
[
    [ "Fortran preprocessor requirements (24-114r2)", "requirements.html", [
      [ "Introduction", "requirements.html#autotoc_md22", null ],
      [ "The basic idea: phases before the \"processor\"", "requirements.html#autotoc_md23", null ],
      [ "FPP Phase 1: Line conjoining", "requirements.html#autotoc_md24", null ],
      [ "FPP Phase 2: Directive processing", "requirements.html#autotoc_md25", [
        [ "Directives accepted by the preprocessor", "requirements.html#autotoc_md26", null ],
        [ "Tokens recognized in define directives", "requirements.html#autotoc_md27", null ],
        [ "Operators accepted in if and elif expressions", "requirements.html#autotoc_md28", null ],
        [ "Macros defined by the preprocessor", "requirements.html#autotoc_md29", null ],
        [ "Fortran awareness during macro expansion", "requirements.html#autotoc_md30", null ],
        [ "Output of Phase 2", "requirements.html#autotoc_md31", null ]
      ] ]
    ] ],
    [ "Preprocessor Specifications (24-142r2)", "specifications.html", [
      [ "Introduction", "specifications.html#autotoc_md32", null ],
      [ "Lexical specifications", "specifications.html#autotoc_md33", [
        [ "Lines", "specifications.html#autotoc_md34", null ],
        [ "Case sensitivity of identifiers", "specifications.html#autotoc_md35", null ],
        [ "Significance of whitespace", "specifications.html#autotoc_md36", null ],
        [ "Comments", "specifications.html#autotoc_md37", null ],
        [ "Token lexicon", "specifications.html#autotoc_md38", null ]
      ] ],
      [ "#-Directives", "specifications.html#autotoc_md39", [
        [ "The #define object-like macro directive", "specifications.html#autotoc_md40", [
          [ "Static semantics specifications", "specifications.html#autotoc_md41", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md42", null ]
        ] ],
        [ "The #define function-like macro directive", "specifications.html#autotoc_md43", [
          [ "Static semantics specifications", "specifications.html#autotoc_md44", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md45", null ]
        ] ],
        [ "The undef directive", "specifications.html#autotoc_md46", [
          [ "Static semantics specifications", "specifications.html#autotoc_md47", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md48", null ]
        ] ],
        [ "The include directive", "specifications.html#autotoc_md49", [
          [ "Static semantics specifications", "specifications.html#autotoc_md50", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md51", null ]
        ] ],
        [ "The #if, #ifdef, #ifndef, #elif, #elifdef, #elifndef, #else, #endif conditional directives", "specifications.html#autotoc_md52", [
          [ "Static semantics specifications", "specifications.html#autotoc_md53", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md54", null ]
        ] ],
        [ "The #error and #warning directives", "specifications.html#autotoc_md55", [
          [ "Static semantics specifications", "specifications.html#autotoc_md56", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md57", null ]
        ] ],
        [ "The #line directive", "specifications.html#autotoc_md58", [
          [ "Static semantics specifications", "specifications.html#autotoc_md59", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md60", null ]
        ] ],
        [ "The #pragma directive", "specifications.html#autotoc_md61", [
          [ "Static semantics specifications", "specifications.html#autotoc_md62", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md63", null ]
        ] ],
        [ "The null directive", "specifications.html#autotoc_md64", [
          [ "Static semantics specifications", "specifications.html#autotoc_md65", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md66", null ]
        ] ],
        [ "The processor-dependent directive", "specifications.html#autotoc_md67", [
          [ "Static semantics specifications", "specifications.html#autotoc_md68", null ],
          [ "Evaluation semantics specifications", "specifications.html#autotoc_md69", null ]
        ] ]
      ] ],
      [ "Macro identification and expansion", "specifications.html#autotoc_md70", [
        [ "Comparison to macro identification and expansion in CPP", "specifications.html#autotoc_md71", null ],
        [ "The identifiers __VA_ARGS__ and __VA_OPT__", "specifications.html#autotoc_md72", null ],
        [ "The # and ## operators", "specifications.html#autotoc_md73", null ]
      ] ],
      [ "Expressions allowed in if and elif directives", "specifications.html#autotoc_md74", [
        [ "Operators allowed in controlling expressions", "specifications.html#autotoc_md75", null ]
      ] ],
      [ "Predefined macros", "specifications.html#autotoc_md76", [
        [ "__LINE__", "specifications.html#autotoc_md77", null ],
        [ "__FILE__", "specifications.html#autotoc_md78", null ],
        [ "__DATE__", "specifications.html#autotoc_md79", null ],
        [ "__TIME__", "specifications.html#autotoc_md80", null ],
        [ "__STDF__", "specifications.html#autotoc_md81", null ]
      ] ],
      [ "INCLUDE line processing", "specifications.html#autotoc_md82", null ],
      [ "Translation limits", "specifications.html#autotoc_md83", null ],
      [ "Appendix A: Divergences from C", "specifications.html#autotoc_md84", null ]
    ] ],
    [ "Macro Expansion (24-176r3)", "macro.html", [
      [ "Background", "macro.html#autotoc_md85", null ],
      [ "Straw polls", "macro.html#autotoc_md86", [
        [ "Straw poll 1", "macro.html#autotoc_md87", null ],
        [ "Straw poll 2", "macro.html#autotoc_md88", null ],
        [ "Straw poll 2 analysis", "macro.html#autotoc_md89", [
          [ "PROS OPTION R:", "macro.html#autotoc_md90", null ],
          [ "CONS OPTION R:", "macro.html#autotoc_md91", null ],
          [ "PROS OPTION O:", "macro.html#autotoc_md92", null ],
          [ "CONS OPTION O:", "macro.html#autotoc_md93", null ]
        ] ]
      ] ],
      [ "Specifications", "macro.html#autotoc_md94", [
        [ "General", "macro.html#autotoc_md95", null ],
        [ "Function-like macro invocation", "macro.html#autotoc_md96", [
          [ "Function-like macro identification", "macro.html#autotoc_md97", [
            [ "Argument gathering and separation", "macro.html#autotoc_md98", null ],
            [ "Line breaks and continuations in macro invocations (free-form)", "macro.html#autotoc_md99", null ],
            [ "Line breaks and continuations in macro invocations (fixed-form)", "macro.html#autotoc_md100", null ],
            [ "Comments in macro invocations", "macro.html#autotoc_md101", null ]
          ] ],
          [ "Argument substitution and expansion", "macro.html#autotoc_md102", [
            [ "Macro expansion during argument substitution", "macro.html#autotoc_md103", null ],
            [ "The Stringizing Operator (#)", "macro.html#autotoc_md104", null ]
          ] ],
          [ "Variadic Macros", "macro.html#autotoc_md105", [
            [ "__VA_OPT__", "macro.html#autotoc_md106", null ]
          ] ]
        ] ],
        [ "The Token-Pasting Operator (##)", "macro.html#autotoc_md107", null ],
        [ "Rescanning and Recursion Prevention", "macro.html#autotoc_md108", null ],
        [ "Appendix A: Divergences from C", "macro.html#autotoc_md109", null ]
      ] ]
    ] ]
];