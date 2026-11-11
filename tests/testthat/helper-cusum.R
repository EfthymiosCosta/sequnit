# Small design settings.
small <- list(U0 = 100, n_bins = 31, n_scan = 4)

# One small design, computed once and cached for all tests.
small_cache <- file.path(tempdir(), "sequnit-test-designs")
small_design <- do.call(cusum_design,
                        c(small, list(cores = 1, design_dir = small_cache, quiet = TRUE)))

# Gives a test its own temporary sequnit.dir, with the small design settings
# and the small design in its cache. The folder and the options are reset when
# the test ends.
local_cusum <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_options(sequnit.dir = dir,
                       sequnit.group = NULL,
                       sequnit.fail_on_signal = TRUE,
                       .local_envir = env)
  withr::local_options(.new = stats::setNames(small, paste0("sequnit.", names(small))),
                       .local_envir = env)
  dir.create(file.path(dir, "designs"))
  file.copy(list.files(small_cache,
                       full.names = TRUE),
            file.path(dir, "designs"))
  dir
}

# expect_pval() without its messages.
expect_pval_small <- function(pval_fun, name) {
  suppressMessages(expect_pval(pval_fun, name))
}
