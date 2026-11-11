#' Design a CUSUM chart for a randomised unit test
#'
#' Finds the signalling threshold `h`, the upper bound `H`, and the acceleration
#' threshold `v` of an accelerated, non-restarting CUSUM chart. The design
#' minimises the average number of commits to detect a defect when the process
#' is in-control, subject to a minimum average number of commits between false
#' alarms and a maximum average number of p-values drawn per commit.
#'
#' @param alpha Significance level of a single test run.
#' @param power Power of a single test run against the smallest defect to detect.
#' @param U0 Minimum average number of user commits until a false alarm when the
#'   code has no defect, assuming the associated chart starts from 0.
#' @param cbar0max Maximum average number of test runs per user commit when the
#'   code has no defect.
#' @param amax Maximum number of test runs per user commit.
#' @param p_leave Probability that the chart starts from `H` and ends below `h`
#'   within one one user commit when the code has no defect.
#' @param n_bins Number of bins used in the discretisation for the Markov chain
#'   approximation. More bins are more accurate but slower.
#' @param n_scan Number of values of `h` evaluated in the initial scan.
#' @param design_dir Folder of cached designs, or `NULL` for no caching. If a
#'   design with the same settings is cached there, it is returned without
#'   running the search; otherwise the new design is saved there.
#' @param cores Number of cores used for the initial scan. Defaults to all
#'   available cores.
#' @param tol_h Precision to which the signalling threshold `h` is found.
#' @param max_h_iter Maximum number of iterations of the search for `h` after
#'   the initial scan.
#' @param quiet If `TRUE`, no progress messages are shown.
#'
#' @return A list with the settings (`alpha`, `power`, `U0`, `cbar0max`,
#'   `amax`, `p_leave`, `n_bins`) and the design:
#'   \describe{
#'     \item{`delta`}{Shift parameter of the CUSUM chart.}
#'     \item{`gap`}{`H - h`, determined by `p_leave`.}
#'     \item{`p_recover`}{Probability that the chart starts from `H` and lands
#'       below `h` within one user commit, after the defect is fixed.}
#'     \item{`h`, `H`, `v`}{Signalling threshold, upper bound and acceleration
#'       threshold.}
#'     \item{`u0`, `u1`}{Average number of user commits until a signal, starting
#'       from 0, without and with a defect.}
#'     \item{`cbar0`}{Average number of test runs per user commit without a
#'       defect.}
#'     \item{`h_lo`, `h_up`}{Range of `h` searched.}
#'   }
#'
#' @details The gap `H - h` is computed first from `p_leave`. The cache key consists
#'   of `alpha`, `power`, `U0`, `cbar0max`, `amax`, `p_leave` and `n_bins`.
#'
#' @examples
#' d <- cusum_design(U0 = 100, n_bins = 31, n_scan = 4, cores = 1)
#' d[c("h", "H", "v", "u0", "u1", "cbar0")]
#' @export
cusum_design <- function(alpha = 0.05, power = 0.2, U0 = 1e5, cbar0max = 1.1,
                         amax = 10, p_leave = 0.01, n_bins = 151, n_scan = 30,
                         design_dir = NULL, cores = NULL, tol_h = 1e-3,
                         max_h_iter = 40, quiet = FALSE) {
  .check_settings(alpha, power, U0, cbar0max, amax, p_leave, n_bins, n_scan)
  if (!is.null(design_dir) && (!is.character(design_dir) || length(design_dir) != 1L || !nzchar(design_dir)))
    stop("`design_dir` must be `NULL` or a single folder name.", call. = FALSE)
  if (!is.null(cores) && (!is.numeric(cores) || length(cores) != 1L || cores < 1 || cores %% 1 != 0))
    stop("`cores` must be `NULL` or a single positive integer.", call. = FALSE)
  amax <- as.integer(amax)
  n_bins <- as.integer(n_bins)
  if (!is.null(design_dir)) {
    path <- file.path(design_dir, paste0(.design_key(alpha, power, U0, cbar0max, amax, p_leave, n_bins), ".json"))
    if (file.exists(path))
      return(jsonlite::read_json(path, simplifyVector = TRUE))
    if (!quiet) message("cusum_design(): no cached design, running the search.")
  }
  if (is.null(cores)) {
    cores <- if (.Platform$OS.type == "windows") 1L else max(1L, parallel::detectCores(), na.rm = TRUE)
    if (toupper(Sys.getenv("_R_CHECK_LIMIT_CORES_")) %in% c("TRUE", "WARN"))
      cores <- min(cores, 2L)
  }
  design <- .cusum_search(alpha, power, U0, cbar0max, amax, p_leave, n_bins, n_scan,
                          tol_h, max_h_iter, as.integer(cores))
  if (!is.null(design_dir)) {
    dir.create(design_dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(tmpdir = design_dir, fileext = ".json.tmp")
    jsonlite::write_json(design, tmp, auto_unbox = TRUE, digits = NA, pretty = TRUE)
    if (!file.rename(tmp, path)) {
      unlink(tmp)
      stop("Failed to write the design cache to '", path, "'.", call. = FALSE)
    }
    if (!quiet) message("cusum_design(): cached design in '", path, "'.")
  }
  design
}
