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
#' @return An object of class `cusum_log`: a list with `header`, the chart's
#'   design as a list, and `data`, a data frame with one row per commit and
#'   columns `signal`, `t`, `S` and `steps`. See [sequnit-package] for the
#'   meaning of each field. It has [print()][print.cusum_log()] and
#'   [plot()][plot.cusum_log()] methods.
#'
#' @examples
#' set.seed(1)
#' dir <- tempfile()
#' old <- c(options(sequnit.dir = dir),
#'          sequnit_options(U0 = 100, n_bins = 31, n_scan = 4))
#' expect_pval(function() runif(1), name = "example")
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
  structure(list(header = header, data = data), class = "cusum_log")
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
    stop("`pval_fun()` must return a single number in [0, 1].", call. = FALSE)
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
#' @param name Name of the chart. Its log file is `<name>.jsonl`.
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
#'   A new chart is designed with the settings from [sequnit_options()]. Later
#'   calls use the design stored in the log's header. If the log was created
#'   with different settings from the current ones, it is deleted and a new
#'   chart is started with the current settings, with a warning.
#'
#'   The log file is kept in the folder of the current [cusum_group()], or else
#'   the option `sequnit.dir`, or else the working directory. Designs are cached
#'   in `designs/` inside `sequnit.dir`, so that all charts share one cache.
#'
#'   If the option `sequnit.fail_on_signal` is `FALSE`, a signal is reported as
#'   a warning and the test passes. The signal is recorded in the log
#'   either way.
#'
#' @seealso [sequnit_options()] for the design settings, [cusum_design()] for
#'   the design, [read_pval_log()] and [plot.cusum_log()] for the log,
#'   [workflow_gen()] to run the tests on GitHub Actions.
#'
#' @examples
#' set.seed(1)
#' old <- c(options(sequnit.dir = tempfile()),
#'          sequnit_options(U0 = 100, n_bins = 31, n_scan = 4))
#' for (i in 1:3)
#'   expect_pval(function() runif(1), name = "example")
#' options(old)
#' @export
expect_pval <- function(pval_fun, name) {
  if (!is.function(pval_fun))
    stop("`pval_fun` must be a function that returns a single p-value.", call. = FALSE)
  if (!is.character(name) || length(name) != 1L || !nzchar(name))
    stop("`name` must be a single non-empty string.", call. = FALSE)
  dir <- getOption("sequnit.group", getOption("sequnit.dir", "."))
  file_name <- if (grepl("\\.jsonl$", name, ignore.case = TRUE)) name else paste0(name, ".jsonl")
  file_path <- file.path(dir, file_name)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message("expect_pval(): created directory '", dir, "'.")
  }
  new_log <- !file.exists(file_path)
  if (!new_log) {
    header <- .read_pval_header(file_path)
    settings <- sequnit_options()
    keys <- intersect(c("alpha", "power", "U0", "cbar0max", "amax", "p_leave", "n_bins"), names(header))
    changed <- keys[!vapply(keys, function(k) isTRUE(all.equal(header[[k]], settings[[k]])), logical(1))]
    if (length(changed)) {
      warning(sprintf("CUSUM chart '%s' was created with different settings (%s). Its log is replaced by a new chart with the current settings.",
                      name, paste(sprintf("%s = %s, now %s", changed, vapply(header[changed], format, ""),
                                          vapply(settings[changed], format, "")), collapse = "; ")),
              call. = FALSE)
      file.remove(file_path)
      new_log <- TRUE
    }
  }
  if (new_log) {
    design_dir <- file.path(getOption("sequnit.dir", "."), "designs")
    d <- do.call(cusum_design, c(sequnit_options(), list(design_dir = design_dir)))
    h <- .round_up(d$h, .log_digits)
    header <- list(name = name, created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   alpha = d$alpha, power = d$power, U0 = d$U0, cbar0max = d$cbar0max,
                   amax = d$amax, p_leave = d$p_leave, n_bins = d$n_bins,
                   delta = round(d$delta, .log_digits),
                   h = h, H = round(h + .round_up(d$gap, .log_digits), .log_digits),
                   v = .round_up(d$v, .log_digits), u1 = round(d$u1, .log_digits))
    S_prev <- 0
  } else {
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
  if (new_log) message("expect_pval(): created log '", file_path, "'.")
  failure_message <- sprintf(
    "CUSUM chart '%s' %s the signalling threshold: S = %.4f >= h = %.4f after %d computational step%s.",
    name,
    if (crossed) "has crossed" else "remains above",
    S, header$h, steps,
    if (steps == 1L) "" else "s")
  fail <- isTRUE(getOption("sequnit.fail_on_signal", TRUE))
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
#' old <- c(options(sequnit.dir = tempfile()),
#'          sequnit_options(U0 = 100, n_bins = 31, n_scan = 4))
#' cusum_group("uniform checks", {
#'   expect_pval(function() runif(1), name = "first")
#' })
#' list.files(getOption("sequnit.dir"), recursive = TRUE)
#' options(old)
#' @export
cusum_group <- function(description, code) {
  if (!is.character(description) || length(description) != 1L)
    stop("`description` must be a single string.", call. = FALSE)
  group_name <- gsub("[^A-Za-z0-9_-]+", "_", description)
  group_name <- gsub("^_+|_+$", "", group_name)
  if (!nzchar(group_name))
    stop("`description` produced an empty group name.", call. = FALSE)
  group_dir <- file.path(getOption("sequnit.group", getOption("sequnit.dir", ".")), group_name)
  dir.create(group_dir, recursive = TRUE, showWarnings = FALSE)
  old <- options(sequnit.group = group_dir)
  on.exit(options(old), add = TRUE)
  invisible(code)
}
