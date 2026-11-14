#' Read the header of a log
#'
#' Reads only the first line of the log and checks that it holds a chart design.
#'
#' @param file_path Path to the log file.
#' @return The header as a list.
#' @noRd
.read_log_header <- function(file_path) {
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
.read_log_last <- function(file_path) {
  if (!file.exists(file_path))
    stop("Log file not found: ", file_path, call. = FALSE)
  lines <- readLines(file_path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  if (length(lines) <= 1)
    return(NULL)
  jsonlite::fromJSON(lines[length(lines)])
}

#' Read the log of a CUSUM chart
#'
#' Reads the log file that [expect_pval()] keeps for a chart, with the chart's
#' design and one line per user commit.
#'
#' @param file_path Path to the log file.
#'
#' @return An object of class `cusum_log`: a list with `header`, the chart's
#'   design as a list, and `data`, a data frame with one row per commit and
#'   columns `signal`, `t`, `S` and `steps`. See [sequnit-package] for the
#'   meaning of each field. It has [print()][print.cusum_log()],
#'   [summary()][summary.cusum_log()] and [plot()][plot.cusum_log()] methods.
#'
#' @examples
#' set.seed(1)
#' dir <- tempfile()
#' old <- options(sequnit.dir = dir)
#' options(sequnit.U0 = 100, sequnit.n_bins = 31, sequnit.n_scan = 4)
#' expect_pval(function() runif(1), name = "example")
#' read_cusum_log(file.path(dir, "example.jsonl"))
#' sequnit_reset()
#' options(old)
#' @export
read_cusum_log <- function(file_path) {
  header <- .read_log_header(file_path)
  lines <- readLines(file_path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  entries <- lapply(lines[-1], jsonlite::fromJSON)
  data <- if (length(entries)) do.call(rbind, lapply(entries, as.data.frame)) else
    data.frame(signal = integer(0), t = character(0), S = numeric(0), steps = integer(0))
  structure(list(header = header, data = data), class = "cusum_log")
}

#' Print the log of a CUSUM chart
#'
#' Shows the chart's name and thresholds, and its last `n` user commits.
#'
#' @param x A log read by [read_cusum_log()].
#' @param n Number of most recent user commits to show. Use `Inf` to show all.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Invisibly, `x`.
#'
#' @seealso [summary.cusum_log()] for an overview of the chart.
#'
#' @examples
#' set.seed(1)
#' dir <- tempfile()
#' old <- options(sequnit.dir = dir)
#' options(sequnit.U0 = 100, sequnit.n_bins = 31, sequnit.n_scan = 4)
#' for (i in 1:15)
#'   expect_pval(function() runif(1), name = "example")
#' log <- read_cusum_log(file.path(dir, "example.jsonl"))
#' log
#' print(log, n = Inf)
#' sequnit_reset()
#' options(old)
#' @export
print.cusum_log <- function(x, n = 10, ...) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1)
    stop("`n` must be a single positive number, or `Inf`.", call. = FALSE)
  hd <- x$header
  dat <- x$data
  total <- nrow(dat)
  cat(sprintf("Log of CUSUM chart: %s (h = %s, H = %s, v = %s)\n", hd$name,
              format(hd$h), format(hd$H), format(hd$v)))
  if (total == 0L) {
    cat("No user commits yet.\n")
    return(invisible(x))
  }
  shown <- min(n, total)
  cat(if (shown == total) sprintf("All %d user commit%s:\n", total, if (total == 1L) "" else "s") else
    sprintf("Last %d of %d user commits:\n", shown, total))
  rows <- seq.int(total - shown + 1L, total)
  print(data.frame(commit = rows, signal = dat$signal[rows], t = dat$t[rows],
                   S = sprintf("%.4f", dat$S[rows]), steps = dat$steps[rows]),
        row.names = FALSE)
  invisible(x)
}

#' Summarise the log of a CUSUM chart
#'
#' Gives the chart's settings and thresholds, the number of user commits and
#' test runs, the current CUSUM statistic and the signals so far.
#'
#' @param object A log read by [read_cusum_log()].
#' @param x A summary from `summary()`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return `summary()` returns an object of class `summary.cusum_log`: a list
#'   with `name`, `created`, `settings` (a named list), `h`, `H`, `v`,
#'   `n_commits`, `n_runs`, `first_time` and `last_time` of the commits, `S`
#'   (the current value), `signalling`, `signalling_since` (the commit from
#'   which the current signal has lasted), `n_signals`, `first_signal` and
#'   `first_signal_time`. Values that do not apply are `NA`. Its `print()`
#'   method shows them as an overview and returns `x` invisibly.
#'
#' @examples
#' set.seed(1)
#' dir <- tempfile()
#' old <- options(sequnit.dir = dir)
#' options(sequnit.U0 = 100, sequnit.n_bins = 31, sequnit.n_scan = 4)
#' for (i in 1:5)
#'   expect_pval(function() runif(1), name = "example")
#' summary(read_cusum_log(file.path(dir, "example.jsonl")))
#' sequnit_reset()
#' options(old)
#' @export
summary.cusum_log <- function(object, ...) {
  hd <- object$header
  dat <- object$data
  n <- nrow(dat)
  keys <- intersect(c("alpha", "power", "U0", "cbar0max", "amax", "p_leave", "n_bins"), names(hd))
  times <- format(as.POSIXct(dat$t, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M")
  signals <- which(dat$signal == 1L)
  signalling <- n > 0L && dat$signal[n] == 1L
  structure(list(
    name = hd$name, created = hd$created, settings = hd[keys],
    h = hd$h, H = hd$H, v = hd$v,
    n_commits = n, n_runs = sum(dat$steps),
    first_time = if (n) times[1] else NA, last_time = if (n) times[n] else NA,
    S = if (n) dat$S[n] else NA, signalling = signalling,
    signalling_since = if (signalling) n - match(0L, rev(dat$signal), nomatch = n + 1L) + 2L else NA,
    n_signals = length(signals),
    first_signal = if (length(signals)) signals[1] else NA,
    first_signal_time = if (length(signals)) times[signals[1]] else NA),
    class = "summary.cusum_log")
}

#' @rdname summary.cusum_log
#' @export
print.summary.cusum_log <- function(x, ...) {
  line <- function(label, value) cat(formatC(paste0(label, ":"), width = -12), value, "\n", sep = "")
  items <- paste0(names(x$settings), " = ", vapply(x$settings, format, ""),
                  c(rep(",", length(x$settings) - 1), ""))
  width <- max(getOption("width") - 12, 20)
  rows <- character(0)
  for (item in items) {
    last <- length(rows)
    if (last && nchar(rows[last]) + 1 + nchar(item) <= width) rows[last] <- paste(rows[last], item)
    else rows <- c(rows, item)
  }
  cat("Summary of CUSUM chart: ", x$name, "\n", sep = "")
  line("Created", x$created)
  line("Settings", paste(rows, collapse = paste0("\n", strrep(" ", 12))))
  line("Thresholds", sprintf("h = %s, H = %s, v = %s", format(x$h), format(x$H), format(x$v)))
  if (x$n_commits == 0L) {
    line("Commits", "none")
    return(invisible(x))
  }
  line("Commits", if (x$n_commits == 1L) sprintf("1, at %s", x$first_time) else
    sprintf("%d, from %s to %s", x$n_commits, x$first_time, x$last_time))
  line("Test runs", sprintf("%d in total, %.2f per commit", x$n_runs, x$n_runs / x$n_commits))
  line("Current S", if (x$signalling)
    sprintf("%.4f (at or above h, signalling since commit %d)", x$S, x$signalling_since) else
      sprintf("%.4f (below h)", x$S))
  line("Signals", if (!x$n_signals) "none" else
    sprintf("%d commit%s, first at commit %d (%s)", x$n_signals,
            if (x$n_signals == 1L) "" else "s", x$first_signal, x$first_signal_time))
  invisible(x)
}