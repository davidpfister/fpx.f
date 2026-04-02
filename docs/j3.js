var j3 =
[
    [ "Fortran preprocessor requirements", "requirements.html", [
      [ "1. Introduction", "requirements.html#autotoc_md35", null ],
      [ "2. The basic idea: phases before the \"processor\"", "requirements.html#autotoc_md36", null ],
      [ "3. FPP Phase 1: Line conjoining", "requirements.html#autotoc_md37", null ],
      [ "4. FPP Phase 2: Directive processing", "requirements.html#autotoc_md38", [
        [ "4.1 Directives accepted by the preprocessor", "requirements.html#autotoc_md39", null ],
        [ "4.2 Tokens recognized in define directives", "requirements.html#autotoc_md40", null ],
        [ "4.3 Operators accepted in if and elif expressions", "requirements.html#autotoc_md41", null ],
        [ "4.4 Macros defined by the preprocessor", "requirements.html#autotoc_md42", null ],
        [ "4.5 Fortran awareness during macro expansion", "requirements.html#autotoc_md43", null ],
        [ "4.6 Output of Phase 2", "requirements.html#autotoc_md44", null ]
      ] ]
    ] ],
    [ "Preprocessor Specifications", "specifications.html", [
      [ "1 Introduction", "specifications.html#autotoc_md45", null ],
      [ "2 Lexical specifications", "specifications.html#autotoc_md46", [
        [ "2.1 Lines", "specifications.html#autotoc_md47", null ],
        [ "2.2 Case sensitivity of identifiers", "specifications.html#autotoc_md48", null ],
        [ "2.3 Significance of whitespace", "specifications.html#autotoc_md49", null ],
        [ "2.4 Comments", "specifications.html#autotoc_md50", null ],
        [ "2.5 Token lexicon", "specifications.html#autotoc_md51", null ]
      ] ],
      [ "3 #-Directives", "specifications.html#autotoc_md52", [
        [ "3.1 The define object-like macro directive", "specifications.html#autotoc_md53", [
          [ "3.1.1 Static semantics specifications", "specifications.html#autotoc_md54", null ],
          [ "3.1.2 Evaluation semantics specifications", "specifications.html#autotoc_md55", null ]
        ] ],
        [ "3.2 The define function-like macro directive", "specifications.html#autotoc_md56", [
          [ "3.2.1 Static semantics specifications", "specifications.html#autotoc_md57", null ],
          [ "3.2.2 Evaluation semantics specifications", "specifications.html#autotoc_md58", null ]
        ] ],
        [ "3.3 The undef directive", "specifications.html#autotoc_md59", [
          [ "3.3.1 Static semantics specifications", "specifications.html#autotoc_md60", null ],
          [ "3.3.2 Evaluation semantics specifications", "specifications.html#autotoc_md61", null ]
        ] ],
        [ "3.4 The include directive", "specifications.html#autotoc_md62", [
          [ "3.4.1 Static semantics specifications", "specifications.html#autotoc_md63", null ],
          [ "3.4.2 Evaluation semantics specifications", "specifications.html#autotoc_md64", null ]
        ] ],
        [ "3.5 The if, ifdef, ifndef, elif, elifdef, elifndef, else, endif conditional directives", "specifications.html#autotoc_md65", [
          [ "3.5.1 Static semantics specifications", "specifications.html#autotoc_md66", null ],
          [ "3.5.2 Evaluation semantics specifications", "specifications.html#autotoc_md67", null ]
        ] ],
        [ "3.6 The error and warning directives", "specifications.html#autotoc_md68", [
          [ "3.6.1 Static semantics specifications", "specifications.html#autotoc_md69", null ],
          [ "3.6.2 Evaluation semantics specifications", "specifications.html#autotoc_md70", null ]
        ] ],
        [ "3.7 The line directive", "specifications.html#autotoc_md71", [
          [ "3.7.1 Static semantics specifications", "specifications.html#autotoc_md72", null ],
          [ "3.7.2 Evaluation semantics specifications", "specifications.html#autotoc_md73", null ]
        ] ],
        [ "3.8 The pragma directive", "specifications.html#autotoc_md74", [
          [ "3.8.1 Static semantics specifications", "specifications.html#autotoc_md75", null ],
          [ "3.8.2 Evaluation semantics specifications", "specifications.html#autotoc_md76", null ]
        ] ],
        [ "3.9 The null directive", "specifications.html#autotoc_md77", [
          [ "3.9.1 Static semantics specifications", "specifications.html#autotoc_md78", null ],
          [ "3.9.2 Evaluation semantics specifications", "specifications.html#autotoc_md79", null ]
        ] ],
        [ "3.10 The processor-dependent directive", "specifications.html#autotoc_md80", [
          [ "3.10.1 Static semantics specifications", "specifications.html#autotoc_md81", null ],
          [ "3.10.2 Evaluation semantics specifications", "specifications.html#autotoc_md82", null ]
        ] ]
      ] ],
      [ "4 Macro identification and expansion", "specifications.html#autotoc_md83", [
        [ "4.1 Comparison to macro identification and expansion in CPP", "specifications.html#autotoc_md84", null ],
        [ "4.2 The identifiers __VA_ARGS__ and __VA_OPT__", "specifications.html#autotoc_md85", null ],
        [ "4.3 The # and ## operators", "specifications.html#autotoc_md86", null ]
      ] ],
      [ "5 Expressions allowed in if and elif directives", "specifications.html#autotoc_md87", [
        [ "5.1 Operators allowed in controlling expressions", "specifications.html#autotoc_md88", null ]
      ] ],
      [ "7 Predefined macros", "specifications.html#autotoc_md89", [
        [ "7.1 __LINE__", "specifications.html#autotoc_md90", null ],
        [ "7.2 __FILE__", "specifications.html#autotoc_md91", null ],
        [ "7.3 __DATE__", "specifications.html#autotoc_md92", null ],
        [ "7.4 __TIME__", "specifications.html#autotoc_md93", null ],
        [ "7.5 __STDF__", "specifications.html#autotoc_md94", null ]
      ] ],
      [ "8 INCLUDE line processing", "specifications.html#autotoc_md95", null ],
      [ "9 Translation limits", "specifications.html#autotoc_md96", null ],
      [ "Appendix A: Divergences from C", "specifications.html#autotoc_md97", null ]
    ] ],
    [ "Macro Expansion", "macro.html", [
      [ "Background", "macro.html#autotoc_md98", null ],
      [ "Straw polls", "macro.html#autotoc_md99", [
        [ "Straw poll 1", "macro.html#autotoc_md100", null ],
        [ "Straw poll 2", "macro.html#autotoc_md101", null ],
        [ "Straw poll 2 analysis", "macro.html#autotoc_md102", [
          [ "PROS OPTION R:", "macro.html#autotoc_md103", null ],
          [ "CONS OPTION R:", "macro.html#autotoc_md104", null ],
          [ "PROS OPTION O:", "macro.html#autotoc_md105", null ],
          [ "CONS OPTION O:", "macro.html#autotoc_md106", null ]
        ] ]
      ] ],
      [ "Specifications", "macro.html#autotoc_md107", [
        [ "1 General", "macro.html#autotoc_md108", null ],
        [ "2 Function-like macro invocation", "macro.html#autotoc_md109", [
          [ "2.1 Function-like macro identification", "macro.html#autotoc_md110", [
            [ "2.1.1 Argument gathering and separation", "macro.html#autotoc_md111", null ],
            [ "2.1.2 Line breaks and continuations in macro invocations (free-form)", "macro.html#autotoc_md112", null ],
            [ "2.1.3 Line breaks and continuations in macro invocations (fixed-form)", "macro.html#autotoc_md113", null ],
            [ "2.1.4 Comments in macro invocations", "macro.html#autotoc_md114", null ]
          ] ],
          [ "2.2 Argument substitution and expansion", "macro.html#autotoc_md115", [
            [ "2.2.1 Macro expansion during argument substitution", "macro.html#autotoc_md116", null ],
            [ "2.2.2 The Stringizing Operator (#)", "macro.html#autotoc_md117", null ]
          ] ],
          [ "2.3 Variadic Macros", "macro.html#autotoc_md118", [
            [ "2.3.1 __VA_OPT__", "macro.html#autotoc_md119", null ]
          ] ]
        ] ],
        [ "3 The Token-Pasting Operator (##)", "macro.html#autotoc_md120", null ],
        [ "4 Rescanning and Recursion Prevention", "macro.html#autotoc_md121", null ],
        [ "Appendix A: Divergences from C", "macro.html#autotoc_md122", null ]
      ] ]
    ] ]
];