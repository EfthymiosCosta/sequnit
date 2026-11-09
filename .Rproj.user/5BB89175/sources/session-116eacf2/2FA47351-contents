# Small design settings.
small <- list(U0 = 100, n_bins = 31, n_scan = 4, cores = 1)

# One small design, computed once and cached for all tests.
small_cache <- file.path(tempdir(), "sequnit-test-designs")
small_design <- do.call(cusum_design,
                        c(small, list(design_dir = small_cache, quiet = TRUE)))

# Gives a test its own temporary cusum.dir, with the small design in its cache.
# The folder and the options are reset when the test ends.
local_cusum <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_options(cusum.dir = dir,
                       cusum.group = NULL,
                       cusum.fail_on_signal = TRUE,
                       .local_envir = env)
  dir.create(file.path(dir, "designs"))
  file.copy(list.files(small_cache,
                       full.names = TRUE),
            file.path(dir, "designs"))
  dir
}

# expect_pval() with the small design settings and no messages.
expect_pval_small <- function(pval_fun, name, ...) {
  do.call(expect_pval, c(list(pval_fun, name = name), small, list(quiet = TRUE, ...)))
}
