var j3 =
[
    [ "Fortran preprocessor requirements (24-114r2)", "requirements.html", [
      [ "Introduction", "requirements.html#autotoc_md22", null ],
      [ "The basic idea: phases before the \"processor\"", "requirements.html#autotoc_md23", null ],
      [ "FPP Phase 1: Line conjoining", "requirements.html#autotoc_md24", null ],
      [ "FPP Phase 2: Directive processing", "requirements.html#autotoc_md25", [
        [ "4.1 Directives accepted by the preprocessor", "requirements.html#autotoc_md26", null ],
        [ "4.2 Tokens recognized in define directives", "requirements.html#autotoc_md27", null ],
        [ "4.3 Operators accepted in if and elif expressions", "requirements.html#autotoc_md28", null ],
        [ "4.4 Macros defined by the preprocessor", "requirements.html#autotoc_md29", null ],
        [ "4.5 Fortran awareness during macro expansion", "requirements.html#autotoc_md30", null ],
        [ "4.6 Output of Phase 2", "requirements.html#autotoc_md31", null ]
      ] ]
    ] ],
    [ "Preprocessor Specifications (24-142r2)", "specifications.html", [
      [ "1 Introduction", "specifications.html#autotoc_md32", null ],
      [ "2 Lexical specifications", "specifications.html#autotoc_md33", [
        [ "2.1 Lines", "specifications.html#autotoc_md34", null ],
        [ "2.2 Case sensitivity of identifiers", "specifications.html#autotoc_md35", null ],
        [ "2.3 Significance of whitespace", "specifications.html#autotoc_md36", null ],
        [ "2.4 Comments", "specifications.html#autotoc_md37", null ],
        [ "2.5 Token lexicon", "specifications.html#autotoc_md38", null ]
      ] ],
      [ "3 #-Directives", "specifications.html#autotoc_md39", [
        [ "3.1 The define object-like macro directive", "specifications.html#autotoc_md40", [
          [ "3.1.1 Static semantics specifications", "specifications.html#autotoc_md41", null ],
          [ "3.1.2 Evaluation semantics specifications", "specifications.html#autotoc_md42", null ]
        ] ],
        [ "3.2 The define function-like macro directive", "specifications.html#autotoc_md43", [
          [ "3.2.1 Static semantics specifications", "specifications.html#autotoc_md44", null ],
          [ "3.2.2 Evaluation semantics specifications", "specifications.html#autotoc_md45", null ]
        ] ],
        [ "3.3 The undef directive", "specifications.html#autotoc_md46", [
          [ "3.3.1 Static semantics specifications", "specifications.html#autotoc_md47", null ],
          [ "3.3.2 Evaluation semantics specifications", "specifications.html#autotoc_md48", null ]
        ] ],
        [ "3.4 The include directive", "specifications.html#autotoc_md49", [
          [ "3.4.1 Static semantics specifications", "specifications.html#autotoc_md50", null ],
          [ "3.4.2 Evaluation semantics specifications", "specifications.html#autotoc_md51", null ]
        ] ],
        [ "3.5 The if, ifdef, ifndef, elif, elifdef, elifndef, else, endif conditional directives", "specifications.html#autotoc_md52", [
          [ "3.5.1 Static semantics specifications", "specifications.html#autotoc_md53", null ],
          [ "3.5.2 Evaluation semantics specifications", "specifications.html#autotoc_md54", null ]
        ] ],
        [ "3.6 The error and warning directives", "specifications.html#autotoc_md55", [
          [ "3.6.1 Static semantics specifications", "specifications.html#autotoc_md56", null ],
          [ "3.6.2 Evaluation semantics specifications", "specifications.html#autotoc_md57", null ]
        ] ],
        [ "3.7 The line directive", "specifications.html#autotoc_md58", [
          [ "3.7.1 Static semantics specifications", "specifications.html#autotoc_md59", null ],
          [ "3.7.2 Evaluation semantics specifications", "specifications.html#autotoc_md60", null ]
        ] ],
        [ "3.8 The pragma directive", "specifications.html#autotoc_md61", [
          [ "3.8.1 Static semantics specifications", "specifications.html#autotoc_md62", null ],
          [ "3.8.2 Evaluation semantics specifications", "specifications.html#autotoc_md63", null ]
        ] ],
        [ "3.9 The null directive", "specifications.html#autotoc_md64", [
          [ "3.9.1 Static semantics specifications", "specifications.html#autotoc_md65", null ],
          [ "3.9.2 Evaluation semantics specifications", "specifications.html#autotoc_md66", null ]
        ] ],
        [ "3.10 The processor-dependent directive", "specifications.html#autotoc_md67", [
          [ "3.10.1 Static semantics specifications", "specifications.html#autotoc_md68", null ],
          [ "3.10.2 Evaluation semantics specifications", "specifications.html#autotoc_md69", null ]
        ] ]
      ] ],
      [ "4 Macro identification and expansion", "specifications.html#autotoc_md70", [
        [ "4.1 Comparison to macro identification and expansion in CPP", "specifications.html#autotoc_md71", null ],
        [ "4.2 The identifiers __VA_ARGS__ and __VA_OPT__", "specifications.html#autotoc_md72", null ],
        [ "4.3 The # and ## operators", "specifications.html#autotoc_md73", null ]
      ] ],
      [ "5 Expressions allowed in if and elif directives", "specifications.html#autotoc_md74", [
        [ "5.1 Operators allowed in controlling expressions", "specifications.html#autotoc_md75", null ]
      ] ],
      [ "7 Predefined macros", "specifications.html#autotoc_md76", [
        [ "7.1 __LINE__", "specifications.html#autotoc_md77", null ],
        [ "7.2 __FILE__", "specifications.html#autotoc_md78", null ],
        [ "7.3 __DATE__", "specifications.html#autotoc_md79", null ],
        [ "7.4 __TIME__", "specifications.html#autotoc_md80", null ],
        [ "7.5 __STDF__", "specifications.html#autotoc_md81", null ]
      ] ],
      [ "8 INCLUDE line processing", "specifications.html#autotoc_md82", null ],
      [ "9 Translation limits", "specifications.html#autotoc_md83", null ],
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
        [ "1 General", "macro.html#autotoc_md95", null ],
        [ "2 Function-like macro invocation", "macro.html#autotoc_md96", [
          [ "2.1 Function-like macro identification", "macro.html#autotoc_md97", [
            [ "2.1.1 Argument gathering and separation", "macro.html#autotoc_md98", null ],
            [ "2.1.2 Line breaks and continuations in macro invocations (free-form)", "macro.html#autotoc_md99", null ],
            [ "2.1.3 Line breaks and continuations in macro invocations (fixed-form)", "macro.html#autotoc_md100", null ],
            [ "2.1.4 Comments in macro invocations", "macro.html#autotoc_md101", null ]
          ] ],
          [ "2.2 Argument substitution and expansion", "macro.html#autotoc_md102", [
            [ "2.2.1 Macro expansion during argument substitution", "macro.html#autotoc_md103", null ],
            [ "2.2.2 The Stringizing Operator (#)", "macro.html#autotoc_md104", null ]
          ] ],
          [ "2.3 Variadic Macros", "macro.html#autotoc_md105", [
            [ "2.3.1 __VA_OPT__", "macro.html#autotoc_md106", null ]
          ] ]
        ] ],
        [ "3 The Token-Pasting Operator (##)", "macro.html#autotoc_md107", null ],
        [ "4 Rescanning and Recursion Prevention", "macro.html#autotoc_md108", null ],
        [ "Appendix A: Divergences from C", "macro.html#autotoc_md109", null ]
      ] ]
    ] ]
];