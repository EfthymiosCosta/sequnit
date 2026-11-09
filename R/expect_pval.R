#' Number of decimals in the logs
#'
#' Decimals to which `S`, `h`, `H`, `v`, `delta` and `u1` are rounded in the logs.
#' @noRd
.log_digits <- 4L

#' Round up to a number of decimals
#'
#' Rounds up. Used for `h`, `v` and the gap, so that the rounded
#' chart still meets the constraints of the design.
#'
#' @param x Numbers to round.
#' @param digits Number of decimals.
#' @return The rounded numbers.
#' @noRd
.round_up <- function(x, digits) round(ceiling(x * 10^digits) / 10^digits, digits)

#' Append lines to a log
#'
#' Writes each element of `objs` as one line of JSON at the end of the log,
#' creating the file if needed.
#'
#' @param file_path Path to the log file.
#' @param objs List of lists, one per line.
#' @return Invisibly, `file_path`.
#' @noRd
.append_pval_log <- function(file_path, objs) {
  lines <- vapply(
    objs,
    function(o) as.character(jsonlite::toJSON(o, auto_unbox = TRUE, digits = NA, na = "null")),
    character(1)
  )
  con <- file(file_path, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con)
  invisible(file_path)
}

#' Read the header of a log
#'
#' Reads only the first line of the log and checks that it holds a chart design.
#'
#' @param file_path Path to the log file.
#' @return The header as a list.
#' @noRd
.read_pval_header <- function(file_path) {
  if (!file.exists(file_path))
    stop("Log file not found: ", file_path, call. = FALSE)
  con <- file(file_path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  first <- readLines(con, n = 1, warn = FALSE)
  if (length(first) == 0 || !nzchar(first))
    stop("Log '", file_path, "' is empty or has no header.", call. = FALSE)
  cfg <- jsonlite::fromJSON(first)
  if (!all(c("alpha", "power", "h", "H", "v", "amax") %in% names(cfg)))
    stop("First line of '", file_path, "' is not a CUSUM design header.", call. = FALSE)
  cfg
}

#' Read the last commit of a log
#'
#' @param file_path Path to the log file.
#' @return The last line as a list, or `NULL` if the log has no commits yet.
#' @noRd
.read_pval_last <- function(file_path) {
  if (!file.exists(file_path))
    stop("Log file not found: ", file_path, call. = FALSE)
  lines <- readLines(file_path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  if (length(lines) <= 1)
    return(NULL)
  jsonlite::fromJSON(lines[length(lines)])
}

#' Read a CUSUM log
#'
#' Reads a log written by [expect_pval()].
#'
#' @param file_path Path to the log file.
#'
#' @return A list with `header`, the chart's design as a list, and `data`, a
#'   data frame with one row per commit and columns `signal`, `t`, `S` and
#'   `steps`. See [sequnit-package] for the meaning of each field.
#'
#' @examples
#' set.seed(1)
#' dir <- tempfile()
#' old <- options(cusum.dir = dir)
#' expect_pval(function() runif(1), name = "example", U0 = 100, n_bins = 31,
#'             n_scan = 4, cores = 1, quiet = TRUE)
#' read_pval_log(file.path(dir, "example.jsonl"))
#' options(old)
#' @export
read_pval_log <- function(file_path) {
  header <- .read_pval_header(file_path)
  lines <- readLines(file_path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  entries <- lapply(lines[-1], jsonlite::fromJSON)
  data <- if (length(entries)) do.call(rbind, lapply(entries, as.data.frame)) else
    data.frame(signal = integer(0), t = character(0), S = numeric(0), steps = integer(0))
  list(header = header, data = data)
}

#' Draw one p-value
#'
#' Runs the test once and checks that it returned a single number in \eqn{[0, 1]}.
#'
#' @param fn Function with no arguments that returns a p-value.
#' @return The p-value.
#' @noRd
.draw_pvalue <- function(fn) {
  p <- fn()
  if (!is.numeric(p) || length(p) != 1L || !is.finite(p) || p < 0 || p > 1)
    stop("`p_value()` must return a single number in [0, 1].", call. = FALSE)
  p
}

#' Update a CUSUM chart by one commit
#'
#' A testthat expectation for a randomised unit test. Each call is one user commit:
#' it draws p-values from `pval_fun`, updates the chart named `name` and
#' appends the result to its log. The test fails when the chart ends
#' the commit at or above its signalling threshold `h`.
#'
#' @param pval_fun A function with no arguments that runs the test
#'   once and returns its p-value.
#' @param name Name of the chart. Its log file is `<name>.jsonl` in `dir`.
#' @param dir Folder of the log. Defaults to the folder of the current
#'   [cusum_group()], or else the option `cusum.dir`, or else the working
#'   directory.
#' @param design_dir Folder of cached designs. Defaults to `designs/` inside
#'   the option `cusum.dir`, so that all charts share one cache.
#' @inheritParams cusum_design
#'
#' @return Invisibly, a list describing the commit: `name`, `file`,
#'   `timestamp`, `S_prev`, `S`, `steps`, `signal`, `crossed` (whether this
#'   commit crossed `h`), `h`, `H` and `v`.
#'
#' @details Within a commit, p-values are drawn up to `amax` times while the
#'   chart is at or above `v`. The statistic `S` is rounded to four
#'   decimals at the end of each commit, and the next commit continues from
#'   that value.
#'
#'   The settings `alpha`, `power`, `U0`, `cbar0max`, `amax`, `p_leave`,
#'   `n_bins` and `n_scan` are only used when the log is created. Later calls
#'   use the design stored in the log's header.
#'
#'   If the option `cusum.fail_on_signal` is `FALSE`, a signal is reported as
#'   a warning and the test passes. The signal is recorded in the log
#'   either way.
#'
#' @seealso [cusum_design()] for the design, [read_pval_log()] and
#'   [plot_pval_log()] for the log, [workflow_gen()] to run the tests on
#'   GitHub Actions.
#'
#' @examples
#' set.seed(1)
#' old <- options(cusum.dir = tempfile())
#' for (i in 1:3)
#'   expect_pval(function() runif(1), name = "example", U0 = 100,
#'               n_bins = 31, n_scan = 4, cores = 1, quiet = TRUE)
#' options(old)
#' @export
expect_pval <- function(pval_fun,
                        name,
                        dir = getOption("cusum.group", getOption("cusum.dir", ".")),
                        alpha = 0.05,
                        power = 0.2,
                        U0 = 1e5,
                        cbar0max = 1.1,
                        amax = 10,
                        p_leave = 0.01,
                        n_bins = 151,
                        n_scan = 30,
                        cores = NULL,
                        design_dir = file.path(getOption("cusum.dir", "."), "designs"),
                        quiet = FALSE) {
  if (!is.function(pval_fun))
    stop("`pval_fun` must be a function that returns a single p-value.", call. = FALSE)
  if (!is.character(name) || length(name) != 1L || !nzchar(name))
    stop("`name` must be a single non-empty string.", call. = FALSE)
  file_name <- if (grepl("\\.jsonl$", name, ignore.case = TRUE)) name else paste0(name, ".jsonl")
  file_path <- file.path(dir, file_name)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!quiet) message("expect_pval(): created directory '", dir, "'.")
  }
  new_log <- !file.exists(file_path)
  if (new_log) {
    d <- cusum_design(alpha = alpha, power = power, U0 = U0, cbar0max = cbar0max,
                      amax = amax, p_leave = p_leave, n_bins = n_bins, n_scan = n_scan,
                      design_dir = design_dir, cores = cores, quiet = quiet)
    h <- .round_up(d$h, .log_digits)
    header <- list(name = name, created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   alpha = d$alpha, power = d$power, U0 = d$U0, cbar0max = d$cbar0max,
                   amax = d$amax, p_leave = d$p_leave, n_bins = d$n_bins,
                   delta = round(d$delta, .log_digits),
                   h = h, H = round(h + .round_up(d$gap, .log_digits), .log_digits),
                   v = .round_up(d$v, .log_digits), u1 = round(d$u1, .log_digits))
    S_prev <- 0
  } else {
    header <- .read_pval_header(file_path)
    last <- .read_pval_last(file_path)
    S_prev <- if (is.null(last)) 0 else last$S
  }
  S <- S_prev
  steps <- 0L
  delta <- .delta(header$alpha, header$power)
  repeat {
    steps <- steps + 1L
    p <- .draw_pvalue(pval_fun)
    S <- min(header$H, max(0, S + qnorm(p, lower.tail = FALSE) - delta / 2))
    if (steps >= header$amax || S < header$v) break
  }
  S <- round(S, .log_digits)
  signal <- S >= header$h
  crossed <- signal && S_prev < header$h
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry <- list(signal = as.integer(signal), t = timestamp, S = S, steps = steps)
  .append_pval_log(file_path, if (new_log) list(header, entry) else list(entry))
  if (new_log && !quiet) message("expect_pval(): created log '", file_path, "'.")
  failure_message <- sprintf(
    "CUSUM chart '%s' %s the signalling threshold: S = %.4f >= h = %.4f after %d computational step%s.",
    name, if (crossed) "has crossed" else "remains above", S, header$h, steps,
    if (steps == 1L) "" else "s")
  fail <- isTRUE(getOption("cusum.fail_on_signal", TRUE))
  if (signal && !fail)
    warning(failure_message, call. = FALSE)
  testthat::expect(ok = !signal || !fail, failure_message = failure_message)
  invisible(list(name = name, file = file_path, timestamp = timestamp,
                 S_prev = S_prev, S = S, steps = steps, signal = signal,
                 crossed = crossed, h = header$h, H = header$H, v = header$v))
}

#' Group CUSUM charts in a subfolder
#'
#' Runs `code` so that every [expect_pval()] call made while it runs keeps its
#' log in a subfolder named after `description`. This includes calls made from
#' helper functions defined elsewhere. Groups can be nested, giving nested
#' subfolders. The design cache stays shared.
#'
#' @param description Name of the group. Characters other than letters,
#'   digits, `_` and `-` are replaced by `_` to give the folder name.
#' @param code Code to run, usually containing [expect_pval()] calls.
#'
#' @return Invisibly, the value of `code`.
#'
#' @examples
#' set.seed(1)
#' old <- options(cusum.dir = tempfile())
#' cusum_group("uniform checks", {
#'   expect_pval(function() runif(1), name = "first", U0 = 100, n_bins = 31,
#'               n_scan = 4, cores = 1, quiet = TRUE)
#' })
#' list.files(getOption("cusum.dir"), recursive = TRUE)
#' options(old)
#' @export
cusum_group <- function(description, code) {
  if (!is.character(description) || length(description) != 1L)
    stop("`description` must be a single string.", call. = FALSE)
  group_name <- gsub("[^A-Za-z0-9_-]+", "_", description)
  group_name <- gsub("^_+|_+$", "", group_name)
  if (!nzchar(group_name))
    stop("`description` produced an empty group name.", call. = FALSE)
  group_dir <- file.path(getOption("cusum.group", getOption("cusum.dir", ".")), group_name)
  dir.create(group_dir, recursive = TRUE, showWarnings = FALSE)
  old <- options(cusum.group = group_dir)
  on.exit(options(old), add = TRUE)
  invisible(code)
}
