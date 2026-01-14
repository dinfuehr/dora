# Test
Use "cargo test" to run all unit tests.

Parser and formatter have additional tests in dora-parser/tests and dora-format/tests. Run tests
like this:

```
tools/run-parse-tests <file>
tools/run-format-tests <file>
```

Without a file argument all tests are run. Pass a filename as argument to run a single test.
For new tests the output file is written automatically. When changing dora-parser or dora-format, run the parser or format tests after running unit tests for the crate.

Full runtime tests are in the tests-directory. Tests can be run with tools/rt.

tools/test runs all these tests after each other. This is also the script CI bots use to make sure everything works.
