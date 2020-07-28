# Instruction for running the Tests

### NOTE: The python extension module should exist for the corresponding python test file, in order to run the R tests

Ensure these R packages are installed.
- testthat
- reticulate
- R6
- purrr
- plotrix

Execute the command `testhat::test_dir(path)` providing the relative path of **Rtests** directory which has all the R test files. A summary of all test results would be generated.
