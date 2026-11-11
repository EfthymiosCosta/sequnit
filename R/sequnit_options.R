#' Set the design settings of new CUSUM charts
#'
#' Sets the design settings that [expect_pval()] uses when it creates a chart.
#' Only the settings passed change; the others keep their current values.
#' Called with no settings, it returns the current ones.
#'
#' @inheritParams cusum_design
#' @param local If `TRUE`, the settings return to their previous values when
#'   the calling function or test block ends.
#'
#' @return With settings, invisibly, their previous values as a list of
#'   options, which [options()] restores. Without, a named list of the current
#'   settings.
#'
#' @details Each setting is stored in the option `sequnit.<name>`, for example
#'   `sequnit.U0`. If an existing chart was created with different settings,
#'   [expect_pval()] replaces it with a new chart, with a warning.
#'
#' @examples
#' sequnit_options()
#' old <- sequnit_options(power = 0.1, U0 = 1e6)
#' sequnit_options()$U0
#' options(old)
#' @export
sequnit_options <- function(alpha = 0.05, power = 0.2, U0 = 1e5, cbar0max = 1.1, amax = 10,
                            p_leave = 0.01, n_bins = 151, n_scan = 30, local = FALSE) {
  settings <- setdiff(names(formals(sequnit_options)), "local")
  current <- lapply(stats::setNames(settings, settings), function(n)
    getOption(paste0("sequnit.", n), formals(sequnit_options)[[n]]))
  given <- setdiff(names(match.call())[-1], "local")
  if (!length(given)){
    return(current)
  }
  new <- mget(given, envir = environment())
  do.call(.check_settings, utils::modifyList(current, new))
  old <- options(stats::setNames(new, paste0("sequnit.", given)))
  if (isTRUE(local))
    withr::defer(options(old), envir = parent.frame())
  invisible(old)
}