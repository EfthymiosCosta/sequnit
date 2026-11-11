# Both plot types run on a log, and the region probabilities form a distribution.
test_that("the plots run on a log", {
  dir <- local_cusum()
  set.seed(1)
  for (i in 1:5)
    expect_pval_small(function() runif(1), name = "p")
  log <- read_cusum_log(file.path(dir, "p.jsonl"))
  expect_s3_class(log, "cusum_log")
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  r <- plot(log, M = 5)
  expect_equal(nrow(r$regions), 5)
  expect_equal(sum(r$regions$prob), 1, tolerance = 1e-8)
  expect_no_error(plot(log, type = "steps"))
  expect_error(plot(log, M = 0), "positive integer")
})

# Printing a log shows its name, and the current state of the chart.
test_that("a log prints a summary", {
  dir <- local_cusum()
  f <- file.path(dir, "q.jsonl")
  expect_pval_small(function() 0.5, name = "q")
  expect_output(print(read_cusum_log(f)), "Log of CUSUM chart: q")
  expect_output(print(read_cusum_log(f)), "Signals:    none")
  withr::local_options(sequnit.fail_on_signal = FALSE)
  suppressWarnings(expect_pval_small(function() 0, name = "q"))
  expect_output(print(read_cusum_log(f)), "signalling since commit 2")
  expect_output(print(read_cusum_log(f)), "1 commit, first at commit 2")
})
