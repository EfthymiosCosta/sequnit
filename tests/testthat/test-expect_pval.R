# A new log starts with a header holding the design, with h, v and the gap
# rounded up to four decimals, followed by one line for the first commit.
test_that("a new log has a rounded header and one entry", {
  dir <- local_cusum()
  d <- small_design
  set.seed(1)
  expect_pval_small(function() runif(1), name = "a")
  lines <- readLines(file.path(dir, "a.jsonl"))
  expect_length(lines, 2)
  hd <- jsonlite::fromJSON(lines[1])
  expect_named(hd, c("name", "created", "alpha", "power", "U0", "cbar0max", "amax",
                     "p_leave", "n_bins", "delta", "h", "H", "v", "u1"))
  expect_equal(hd$h, .round_up(d$h, .log_digits))
  expect_equal(hd$v, .round_up(d$v, .log_digits))
  expect_equal(hd$H, round(hd$h + .round_up(d$gap, .log_digits), .log_digits))
  expect_gte(hd$h, d$h)
  expect_gte(hd$v, d$v)
  expect_equal(hd$delta, round(.delta(d$alpha, d$power), .log_digits))
  entry <- jsonlite::fromJSON(lines[2])
  expect_named(entry, c("signal", "t", "S", "steps"))
  expect_equal(entry$S, round(entry$S, .log_digits))
})

# Each call continues the chart from the S stored on the last line of the log.
test_that("each commit continues from the last S", {
  dir <- local_cusum()
  set.seed(1)
  r1 <- expect_pval_small(function() runif(1), name = "b")
  r2 <- expect_pval_small(function() runif(1), name = "b")
  expect_equal(r2$S_prev, r1$S)
  expect_equal(nrow(read_pval_log(file.path(dir, "b.jsonl"))$data), 2)
})

# A p-value of 0 sends the chart to H, so it signals. The signal fails the
# test, or gives a warning when sequnit.fail_on_signal is FALSE, and is logged
# either way.
test_that("a signal fails the test, or warns when sequnit.fail_on_signal is FALSE", {
  dir <- local_cusum()
  expect_failure(expect_pval_small(function() 0, name = "c"), "has crossed")
  withr::local_options(sequnit.fail_on_signal = FALSE)
  expect_warning(r <- expect_pval_small(function() 0, name = "c"), "remains above")
  expect_true(r$signal)
  expect_equal(read_pval_log(file.path(dir, "c.jsonl"))$data$signal, c(1L, 1L))
})

# pval_fun must be a function, and it must return a single number in [0, 1].
test_that("pval_fun must be a function returning a p-value", {
  local_cusum()
  expect_error(expect_pval_small(0.5, name = "d"), "must be a function")
  expect_error(expect_pval_small(function() 2, name = "d"), "single number in")
})
