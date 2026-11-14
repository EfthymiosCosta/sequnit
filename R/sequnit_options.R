#' Default design settings
#'
#' The options holding the design settings of new charts, with their default
#' values. They are set when the package is loaded, and by `sequnit_reset()`.
#' @noRd
.sequnit_defaults <- list(sequnit.alpha = 0.05,
                          sequnit.power = 0.2,
                          sequnit.U0 = 1e5,
                          sequnit.cbar0max = 1.1,
                          sequnit.amax = 10,
                          sequnit.p_leave = 0.01,
                          sequnit.n_bins = 151,
                          sequnit.n_scan = 30)

#' Set the default design settings on load
#'
#' Runs when the package is loaded, and sets every design option to its
#' default value from `.sequnit_defaults`, replacing any value set before.
#'
#' @param libname,pkgname Library path and name of the package, supplied by R.
#' @return Invisibly, `NULL`.
#' @noRd
.onLoad <- function(libname, pkgname) {
  options(.sequnit_defaults)
  invisible()
}

#' Current design settings
#'
#' Reads the design options, falling back to the defaults for any that are
#' unset.
#'
#' @return A named list of the settings, named without the `sequnit.` prefix.
#' @noRd
.sequnit_settings <- function() {
  settings <- lapply(names(.sequnit_defaults), function(o) getOption(o, .sequnit_defaults[[o]]))
  stats::setNames(settings, sub("^sequnit\\.", "", names(.sequnit_defaults)))
}

#' Reset the design settings to their defaults
#'
#' Sets the options holding the design settings of new charts back to their
#' default values: `sequnit.alpha = 0.05`, `sequnit.power = 0.2`,
#' `sequnit.U0 = 1e5`, `sequnit.cbar0max = 1.1`, `sequnit.amax = 10`,
#' `sequnit.p_leave = 0.01`, `sequnit.n_bins = 151` and `sequnit.n_scan = 30`.
#' These are also the values set when the package is loaded.
#'
#' @return Invisibly, the previous values of the options.
#'
#' @examples
#' options(sequnit.U0 = 1e6)
#' getOption("sequnit.U0")
#' sequnit_reset()
#' getOption("sequnit.U0")
#' @export
sequnit_reset <- function() {
  invisible(options(.sequnit_defaults))
}