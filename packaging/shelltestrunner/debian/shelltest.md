% SHELLTEST(1) shelltestrunner | version 1.2.1
% Simon Michael
% March 18 2012

# NAME

shelltestrunner - test command-line programs or arbitrary shell commands

# SYNOPSIS

shelltest [*options*] {*testfiles*|*testdirs*}

# DESCRIPTION

shelltestrunner tests command-line programs (or arbitrary shell
commands). It reads simple declarative tests specifying a command,
some input, and the expected output, and can run them run in parallel,
selectively, with a timeout, in color, and/or with differences
highlighted.

# OPTIONS

-a, \--all
:    Show all failure output, even if large

-c, \--color
:    Show colored output if your terminal supports it

-d, \--diff
:    Show failures in diff format

-p, \--precise
:    Show failure output precisely (good for whitespace)

-x *STR*, \--exclude=*STR*
:    Exclude test files whose path contains *STR*

\--execdir
:    Run tests from within the test file's directory. Test commands
     normally run within your current directory; `--execdir`
     makes them run within the directory where they are defined, instead.

\--extension=*EXT*
:    Filename suffix of test files (default: *.test*)

-w, \--with=*EXECUTABLE*
:    Replace the first word of (unindented) test commands. This option
     replaces the first word of all test commands with something
     else, which can be useful for testing alternate versions of a
     program. Commands which have been indented by one or more
     spaces will not be affected by this option.

\--debug
:    Show debug info, for troubleshooting

\--debug-parse
:    Show test file parsing info and stop

\--help-format
:    Display test format help

-?, \--help
:    Display help message

-V, \--version
:    Print version information

\-- *TFOPTIONS*
:    Set extra test-framework options like `-j`/`--threads`,
     `-t`/`--select-tests`, `-o`/`--timeout`, `--hide-successes`.
     Use `-- --help` for a list. Avoid spaces.

# DEFINING TESTS

Test files, typically named `tests/*.test`, contain one or more tests
consisting of:

- a one-line command
- optional standard input (`<<<`), standard output (`>>>`) and/or
  standard error output (`>>>2`) specifications
- an exit status (`>>>=`) specification

**Test format:**

    # optional comment
    the command to test
    <<<
    zero or more lines of standard input
    >>>
    zero or more lines of expected standard output
    (or /REGEXP/ added to the previous line)
    >>>2
    zero or more lines of expected standard error output
    (or /REGEXP/ added to the previous line)
    >>>= EXITCODE (or /REGEXP/)

- A `/REGEXP/` pattern may be used instead of explicit data. In this case
  a match anywhere in the output allows the test to pass. The regular
  expression syntax is [regex-tdfa](http://hackage.haskell.org/package/regex-tdfa)'s.
- `EXITCODE` is a numeric
  [exit status](http://en.wikipedia.org/wiki/Exit_status), eg `0` for a
  successful exit.
- You can put `!` before a `/REGEXP/` or `EXITCODE` to negate the match.
- Comment lines beginning with `#` may be used between tests.

# EXAMPLES

Here's `example.test`, a file containing two simple tests:

    # 1. let's test that echo runs. Numbering your tests can be helpful.
    echo
    >>>= 0

    # 2. and now the cat command. On windows, this one should fail.
    cat
    <<<
    foo
    >>>
    foo
    >>>= 0

Run it with `shelltest`:

    $ shelltest example.test
    :t.test:1: [OK]
    :t.test:2: [OK]

             Test Cases  Total
     Passed  2           2
     Failed  0           0
     Total   2           2
