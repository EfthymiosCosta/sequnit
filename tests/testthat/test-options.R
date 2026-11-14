# Loading the package sets every design option to its default.
test_that("the design options have defaults", {
  settings <- .sequnit_settings()
  expect_named(settings, c("alpha", "power", "U0", "cbar0max", "amax", "p_leave",
                           "n_bins", "n_scan"))
  expect_equal(unname(settings), unname(.sequnit_defaults))
})

# sequnit_reset() sets every design option back to its default, and returns the
# previous values.
test_that("sequnit_reset restores the defaults", {
  withr::local_options(sequnit.U0 = 1e6, sequnit.power = 0.1)
  old <- sequnit_reset()
  expect_equal(old$sequnit.U0, 1e6)
  expect_equal(getOption("sequnit.U0"), .sequnit_defaults$sequnit.U0)
  expect_equal(getOption("sequnit.power"), .sequnit_defaults$sequnit.power)
})

# An invalid setting stops expect_pval() before it touches an existing log.
test_that("invalid settings are rejected before any log is changed", {
  dir <- local_cusum()
  expect_pval_small(function() 0.5, name = "o")
  withr::local_options(sequnit.alpha = 2)
  expect_error(expect_pval_small(function() 0.5, name = "o"), "alpha")
  expect_true(file.exists(file.path(dir, "o.jsonl")))
})

# A chart is designed with the current settings. If the settings change, the
# chart is replaced by a new one with the new settings, with a warning.
test_that("a chart is replaced when the settings change", {
  dir <- local_cusum()
  expect_pval_small(function() 0.5, name = "o")
  expect_pval_small(function() 0.5, name = "o")
  log <- read_cusum_log(file.path(dir, "o.jsonl"))
  expect_equal(log$header$U0, small$U0)
  expect_equal(nrow(log$data), 2)
  withr::local_options(sequnit.U0 = 200)
  expect_warning(expect_pval_small(function() 0.5, name = "o"), "U0 = 100, now 200")
  log <- read_cusum_log(file.path(dir, "o.jsonl"))
  expect_equal(log$header$U0, 200)
  expect_equal(nrow(log$data), 1)
})