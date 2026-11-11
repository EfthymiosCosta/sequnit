# With no settings, sequnit_options() returns all the current settings.
test_that("sequnit_options returns the current settings", {
  expect_named(sequnit_options(), c("alpha", "power", "U0", "cbar0max", "amax",
                                    "p_leave", "n_bins", "n_scan"))
})

# Only the settings passed change, and the returned values restore them.
test_that("only the settings passed change", {
  withr::local_options(sequnit.U0 = NULL)
  before <- sequnit_options()
  old <- sequnit_options(U0 = 1e6)
  after <- sequnit_options()
  expect_equal(after$U0, 1e6)
  expect_equal(after[names(after) != "U0"], before[names(before) != "U0"])
  options(old)
  expect_equal(sequnit_options(), before)
})

# With local = TRUE, the settings return to their previous values when the
# calling function ends.
test_that("local settings are restored", {
  withr::local_options(sequnit.U0 = NULL)
  before <- sequnit_options()$U0
  f <- function() {
    sequnit_options(U0 = 1e6, local = TRUE)
    sequnit_options()$U0
  }
  expect_equal(f(), 1e6)
  expect_equal(sequnit_options()$U0, before)
})

# Invalid settings stop with an error and change nothing. A setting is also
# checked against the current values of the others.
test_that("invalid settings are rejected", {
  withr::local_options(sequnit.alpha = NULL, sequnit.power = NULL)
  expect_error(sequnit_options(alpha = 2), "alpha")
  expect_error(sequnit_options(alpha = 0.5), "power")
  expect_null(getOption("sequnit.alpha"))
})

# A chart is designed with the current settings. If the settings change, the
# chart is replaced by a new one with the new settings, with a warning.
test_that("a chart is replaced when the settings change", {
  dir <- local_cusum()
  expect_pval_small(function() 0.5, name = "o")
  expect_pval_small(function() 0.5, name = "o")
  log <- read_pval_log(file.path(dir, "o.jsonl"))
  expect_equal(log$header$U0, small$U0)
  expect_equal(nrow(log$data), 2)
  withr::local_options(sequnit.U0 = 200)
  expect_warning(expect_pval_small(function() 0.5, name = "o"), "U0 = 100, now 200")
  log <- read_pval_log(file.path(dir, "o.jsonl"))
  expect_equal(log$header$U0, 200)
  expect_equal(nrow(log$data), 1)
})
