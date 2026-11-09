#' sequnit: Sequential Monitoring of Randomised Unit Tests
#'
#' Monitors randomised unit tests across successive code changes with a
#' non-restarting cumulative sum (CUSUM) chart. The package builds on
#' testthat. Each call of [expect_pval()] updates one chart by one user commit and
#' appends the result to the chart's log. The chart signals, and the test fails,
#' when the CUSUM statistic ends a commit at or above the threshold `h`.
#'
#' The package also provides a ready-made GitHub Actions pipeline:
#' [workflow_gen()] writes a workflow that runs the tests of a repository or
#' package on pushes, on a schedule or by hand. It keeps the logs between runs
#' on a branch of the repository or as a workflow artifact depending on permissions.
#' Plotting functions [plot_pval_log()] and [plot_pval_steps()] are available
#' for visualising the logs.
#'
#' @section The chart:
#' Each p-value \eqn{p} adds the increment \eqn{\Phi^{-1}(1 - p) - \delta/2}
#' to the CUSUM statistic \eqn{S}, which is kept within \eqn{[0, H]}. Here
#' \eqn{\delta} is the shift in the mean of \eqn{\Phi^{-1}(1 - p)} that the
#' test detects with probability `power` at level `alpha`. Within a commit,
#' further tests are run while \eqn{S \ge v}, up to `amax` in total. The
#' chart is designed by [cusum_design()].
#'
#' @section Options:
#' \describe{
#'   \item{`cusum.dir`}{Folder holding the logs and the `designs/` cache.
#'     Defaults to the working directory.}
#'   \item{`cusum.fail_on_signal`}{If `TRUE` (the default), a signal fails the
#'     test. If `FALSE`, it is reported as a test warning instead.}
#'   \item{`cusum.group`}{Set by [cusum_group()] while its code runs; not
#'     meant to be set by hand.}
#' }
#'
#' @section Log format:
#' Each chart has one log file, `<name>.jsonl`, in JSON Lines format.
#'
#' The first line (header) of every logs file records `name`, `created`, the design
#' settings (`alpha`, `power`, `U0`, `cbar0max`, `amax`, `p_leave`, `n_bins`),
#' and the design itself (`delta`, `h`, `H`, `v`, `u1`). The thresholds `h`,
#' `H` and `v` are rounded up to four decimals, and the chart uses these
#' rounded values. `delta` and `u1` are shown for completeness.
#'
#' Each further line records one commit. `signal` is 1 if the chart ends the
#' commit at or above `h` and 0 otherwise, `t` is the time of the commit, `S`
#' is the CUSUM statistic at the end of the commit, rounded to four decimals,
#' and `steps` is the number of tests run.
#'
#' @keywords internal
#' @importFrom stats pnorm qnorm uniroot
#' @importFrom utils head tail
#' @importFrom graphics abline axis box lines mtext par points rect text title
#' @importFrom grDevices adjustcolor colorRampPalette
"_PACKAGE"
