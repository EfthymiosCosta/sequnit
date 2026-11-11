# The design meets both constraints, and H is h plus the gap.
test_that("a design meets its constraints", {
  d <- small_design
  expect_gte(d$u0, d$U0)
  expect_lte(d$cbar0, d$cbar0max)
  expect_equal(d$H - d$h, d$gap)
  expect_lte(d$v, d$h)
})

# A design is saved in design_dir, and a second call with the same settings
# reads it back without running the search.
test_that("designs are cached and read back", {
  dir <- withr::local_tempdir()
  d1 <- do.call(cusum_design, c(small, list(cores = 1, design_dir = dir, quiet = TRUE)))
  expect_length(list.files(dir), 1)
  expect_silent(d2 <- do.call(cusum_design, c(small, list(cores = 1, design_dir = dir))))
  expect_equal(d2[names(d1)], d1)
})

# Invalid settings stop with an error naming the setting.
test_that("invalid settings are rejected", {
  expect_error(cusum_design(alpha = 2), "alpha")
  expect_error(cusum_design(amax = 1), "amax")
  expect_error(cusum_design(power = 1.5), "power")
  expect_error(cusum_design(p_leave = 1), "p_leave")
  expect_error(cusum_design(U0 = 0), "U0")
  expect_error(cusum_design(cbar0max = 0), "cbar0max")
  expect_error(cusum_design(n_bins = 0), "n_bins")
  expect_error(cusum_design(cores = 0), "cores")
  expect_error(cusum_design(design_dir = 1), "design_dir")
})

# Zero gap and small U0; every h gives more than U0 commits between false
# alarms, and the search stops with an error.
test_that("a U0 that every h meets is rejected", {
  expect_error(cusum_design(U0 = 1000, p_leave = 0.6, n_bins = 31, n_scan = 4, cores = 1),
               "U0 = 1000 is too small")
})