#' Stationary probabilities of equal-width regions
#'
#' Splits \eqn{[0, H]} into `M` equal-width regions and gives the probability
#' of each under the stationary distribution of the CUSUM statistic when the
#' code has no bug, computed using a Markov chain approximation.
#'
#' @param header Header of a log.
#' @param M Number of regions.
#' @return A data frame with the bounds `lower` and `upper` and the
#'   probability `prob` of each region.
#' @noRd
.stationary_regions <- function(header, M) {
  mesh <- .refine_mesh(.make_mesh(header$H, header$n_bins), header$v)
  K0 <- .jump_kernel(mesh, header$v, header$amax, -.delta(header$alpha, header$power) / 2)$K
  ns <- mesh$ns
  A <- t(K0) - diag(ns)
  A[ns, ] <- 1
  pi0 <- as.numeric(solve(qr(A, LAPACK = TRUE), c(rep(0, ns - 1), 1)))
  pi0 <- pmax(pi0, 0)
  pi0 <- pi0 / sum(pi0)
  edges <- seq(0, header$H, length.out = M + 1)
  region <- findInterval(mesh$grid, edges, rightmost.closed = TRUE)
  data.frame(lower = edges[-(M + 1)], upper = edges[-1],
             prob = vapply(seq_len(M), function(r) sum(pi0[region == r]), numeric(1)))
}

#' Format probabilities for the plot labels
#'
#' Shows probabilities of at least 0.001 as percentages with one decimal,
#' smaller ones in scientific notation, and exact zeros as 0.
#'
#' @param p Probabilities.
#' @return Plot labels such as `pi_0 = 12.3%`, with `pi_0` written as a Greek
#'   letter.
#' @noRd
.format_prob <- function(p) {
  lab <- ifelse(p == 0, "0", ifelse(p >= 0.001, sprintf("%.1f%%", 100 * p), sprintf("%.1e", p)))
  as.expression(lapply(lab, function(l) bquote(pi[0] == .(l))))
}

#' Check that logs share their settings
#'
#' Compares the design settings and thresholds of every log with those of the
#' first, and stops if any differ.
#'
#' @param logs List of logs read by `read_cusum_log()`.
#' @return `NULL`, invisibly.
#' @noRd
.check_same_settings <- function(logs) {
  keys <- c("alpha", "power", "U0", "cbar0max", "amax", "p_leave", "n_bins", "h", "H", "v")
  hd1 <- logs[[1]]$header
  for (lg in logs[-1]) {
    hd <- lg$header
    differ <- keys[!vapply(keys, function(k) isTRUE(all.equal(hd1[[k]], hd[[k]])), logical(1))]
    if (length(differ))
      stop(sprintf("Logs '%s' and '%s' use different settings (%s).", hd1$name, hd$name,
                   paste(differ, collapse = ", ")), call. = FALSE)
  }
  invisible()
}

#' Prepare logs for plotting
#'
#' Sets the title, joins the commit times of all logs, and works out the x
#' position of each commit and the positions and labels of the x-axis ticks.
#' With `x_axis = "commit"`, the commits are placed at equal spacing in time
#' order, and commits of different logs made at the same time share a
#' position. With `x_axis = "time"`, they are placed at their times.
#'
#' @inheritParams plot.cusum_log
#' @param logs List of logs read by `read_cusum_log()`.
#' @param prefix Default title, followed by the log's name for a single log.
#' @return A list with the `header` of the first log, the `data` and x
#'   positions `x` of each log, the log names `names`, the title `main`, the
#'   x-axis limits `xlim`, and the tick positions `ticks` and labels
#'   `tick_labels`.
#' @noRd
.prep_plot_logs <- function(logs, main, prefix, x_axis, time_format, n_ticks) {
  names <- vapply(logs, function(lg) lg$header$name, character(1))
  for (i in seq_along(logs))
    if (nrow(logs[[i]]$data) == 0L)
      stop("Log '", names[i], "' contains no commits yet.", call. = FALSE)
  if (is.null(main))
    main <- if (length(logs) == 1L) paste0(prefix, ": ", names) else prefix
  # Commit times in seconds, and how many commits of the same log so far share each time.
  times <- lapply(logs, function(lg) as.numeric(as.POSIXct(lg$data$t, format = "%Y-%m-%d %H:%M:%S")))
  occ <- lapply(times, function(tt) ave(tt, tt, FUN = seq_along))
  all_t <- unlist(times)
  if (is.null(time_format))
    time_format <- if (diff(range(all_t)) < 3600) "%Y-%m-%d\n%H:%M:%S" else "%Y-%m-%d\n%H:%M"
  if (x_axis == "commit") {
    keys <- unique(data.frame(t = all_t, occ = unlist(occ)))
    keys <- keys[order(keys$t, keys$occ), ]
    key_id <- paste(keys$t, keys$occ)
    x <- Map(function(tt, oo) match(paste(tt, oo), key_id), times, occ)
    n <- nrow(keys)
    xlim <- c(0.5, n + 0.5)
    ticks <- unique(round(seq(1, n, length.out = min(n, n_ticks))))
    tick_labels <- format(.POSIXct(keys$t[ticks]), time_format)
  } else {
    x <- times
    rng <- range(all_t)
    xlim <- rng + c(-1, 1) * max(0.02 * diff(rng), 30)
    at <- pretty(.POSIXct(xlim), n = n_ticks)
    at <- at[as.numeric(at) >= xlim[1] & as.numeric(at) <= xlim[2]]
    ticks <- as.numeric(at)
    tick_labels <- format(at, time_format)
  }
  list(header = logs[[1]]$header, data = lapply(logs, `[[`, "data"), x = x, names = names,
       main = main, xlim = xlim, ticks = ticks, tick_labels = tick_labels)
}

#' Graphical arguments of each log
#'
#' Adds to the default arguments of each log its element of every argument
#' in `args`, recycling arguments shorter than the number of logs.
#'
#' @param args List of graphical arguments.
#' @param defaults List with the default arguments of each log.
#' @return A list with the arguments of each log.
#' @noRd
.args_per_log <- function(args, defaults) {
  lapply(seq_along(defaults), function(i)
    utils::modifyList(defaults[[i]], lapply(args, function(a) a[[(i - 1) %% length(a) + 1]])))
}

#' Plot CUSUM logs
#'
#' Plots one or more logs read by [read_cusum_log()], one colour per log from
#' the Okabe-Ito palette, starting with black. Logs plotted together must have
#' the same settings.
#'
#' With `type = "chart"`, it plots the CUSUM statistic, one point per commit.
#' The area between `h` and `H`, where the chart signals, is shaded red.
#'
#' With `type = "steps"`, it plots the number of tests run in each commit
#' (equivalently, the number of p-values drawn). A dashed line marks the
#' maximum, `amax`.
#'
#' @param x A log read by [read_cusum_log()].
#' @param ... Further logs read by [read_cusum_log()], plotted along with `x`,
#'   and graphical arguments passed to [plot()], e.g. `cex.main`.
#' @param type `"chart"` for the CUSUM chart, or `"steps"` for the number of
#'   tests run per commit.
#' @param x_axis `"commit"` to space the commits equally, in time order, or
#'   `"time"` to space them by their times.
#' @param show_probs If `TRUE`, splits \eqn{[0, H]} into `M` equal-width
#'   regions and labels each with \eqn{\pi_0}, its probability under the
#'   stationary distribution of the CUSUM statistic when the code has no bug.
#'   Only used with `type = "chart"`.
#' @param M Number of regions when `show_probs = TRUE`.
#' @param show_acc If `TRUE`, adds a right axis showing the acceleration: one
#'   test run per commit below `v`, and up to `amax` above it. Only used with
#'   `type = "chart"`.
#' @param main Title of the plot. Defaults to the log's name for a single log.
#' @param time_format Format of the times shown on the x-axis, as in
#'   [format.POSIXct()]. Defaults to date and time on two lines, with seconds
#'   if the logs span less than an hour.
#' @param n_ticks Maximum number of ticks on the x-axis. With
#'   `x_axis = "time"`, this is only approximate.
#' @param line_args List of arguments passed to [lines()] for the line of each
#'   log (`type = "chart"`) or its bars (`type = "steps"`), e.g.
#'   `list(lwd = 1)`. Each argument is recycled over the logs, e.g.
#'   `list(col = c("navy", "orange"))`.
#' @param point_args List of arguments passed to [points()] for the points of
#'   each log, recycled as `line_args`. Points take the colour of their line
#'   unless `col` is given.
#'
#' @return Invisibly, `x`. With `show_probs = TRUE`, it also contains
#'   `regions`, a data frame with the bounds and stationary probability of each
#'   region.
#'
#' @examples
#' set.seed(1)
#' dir <- tempfile()
#' old <- options(sequnit.dir = dir)
#' options(sequnit.U0 = 100, sequnit.n_bins = 31, sequnit.n_scan = 4)
#' for (i in 1:20) {
#'   expect_pval(function() runif(1), name = "first")
#'   expect_pval(function() runif(1), name = "second")
#' }
#' log1 <- read_cusum_log(file.path(dir, "first.jsonl"))
#' log2 <- read_cusum_log(file.path(dir, "second.jsonl"))
#' plot(log1)
#' abline(h = log1$header$v, lty = 3)
#' plot(log1, log2)
#' plot(log1, log2, x_axis = "time")
#' plot(log1, show_probs = TRUE, show_acc = TRUE)
#' plot(log1, log2, type = "steps")
#' plot(log1, line_args = list(col = "navy"), point_args = list(col = "red"))
#' sequnit_reset()
#' options(old)
#' @export
plot.cusum_log <- function(x, ..., type = c("chart", "steps"), x_axis = c("commit", "time"),
                           show_probs = FALSE, M = 10, show_acc = FALSE, main = NULL,
                           time_format = NULL, n_ticks = 6, line_args = list(),
                           point_args = list()) {
  type <- match.arg(type)
  x_axis <- match.arg(x_axis)
  if (!is.list(line_args) || !is.list(point_args))
    stop("`line_args` and `point_args` must be lists.", call. = FALSE)
  if (!is.logical(show_probs) || length(show_probs) != 1L || is.na(show_probs) ||
      !is.logical(show_acc) || length(show_acc) != 1L || is.na(show_acc))
    stop("`show_probs` and `show_acc` must be TRUE or FALSE.", call. = FALSE)
  if (type == "chart" && show_probs &&
      (!is.numeric(M) || length(M) != 1L || M < 1 || M %% 1 != 0))
    stop("`M` must be a single positive integer.", call. = FALSE)
  chart <- type == "chart"
  # Split into further logs and graphical arguments.
  dots <- list(...)
  is_log <- vapply(dots, inherits, logical(1), "cusum_log")
  logs <- c(list(x), dots[is_log])
  .check_same_settings(logs)
  k <- length(logs)
  pr <- .prep_plot_logs(logs, main, if (chart) "CUSUM chart" else "Computational steps",
                        x_axis, time_format, n_ticks)
  hd <- pr$header
  y <- lapply(pr$data, `[[`, if (chart) "S" else "steps")
  cols <- rep_len(unname(palette.colors(9, "Okabe-Ito")), k)
  line_i <- .args_per_log(line_args, lapply(cols, function(cl)
    list(col = cl, lty = 1, lwd = 2, type = if (chart) "l" else "h")))
  point_i <- .args_per_log(point_args, lapply(line_i, function(a) list(col = a$col, pch = 16, cex = 0.5)))
  alarm_col <- adjustcolor("#de2d26", alpha.f = 0.5)
  op <- par(mar = c(3.5, 3, 3, if (chart && show_acc) 4.5 else 2))
  on.exit(par(op))
  do.call(plot, utils::modifyList(list(x = NA, xlim = pr$xlim,
                                       ylim = c(0, if (chart) hd$H else hd$amax),
                                       xaxs = "i", yaxs = if (chart) "i" else "r",
                                       xaxt = "n", xlab = "", ylab = "", main = pr$main),
                                  dots[!is_log]))
  usr <- par("usr")
  if (chart) {
    rect(usr[1], hd$h, usr[2], hd$H, col = alarm_col, border = NA)
    if (show_probs) {
      regions <- .stationary_regions(hd, as.integer(M))
      if (M > 1)
        abline(h = regions$upper[-M], col = adjustcolor("black", alpha.f = 0.2))
      text(usr[2], (regions$lower + regions$upper) / 2, .format_prob(regions$prob),
           pos = 2, cex = 0.75, col = adjustcolor("black", alpha.f = 0.6))
      x$regions <- regions
    }
  } else {
    abline(h = hd$amax, col = "firebrick", lty = 2, lwd = 2)
  }
  for (i in seq_len(k)) {
    do.call(lines, c(list(x = pr$x[[i]], y = y[[i]]), line_i[[i]]))
    do.call(points, c(list(x = pr$x[[i]], y = y[[i]]), point_i[[i]]))
  }
  axis(1, at = pr$ticks, labels = pr$tick_labels, cex.axis = 0.8, padj = 0.5)
  if (chart && show_acc) {
    axis(4, at = c(0, hd$v, hd$H), labels = FALSE)
    axis(4, at = c(hd$v / 2, (hd$v + hd$H) / 2), labels = c(1, hd$amax), tick = FALSE, las = 1)
    mtext("acceleration a(S)", side = 4, line = 2.5)
  }
  # Legend
  get <- function(args, a) unlist(lapply(args, `[[`, a))
  op_lend <- par(lend = "butt")
  legend("topleft", legend = c(pr$names, if (chart) "Alarm region"),
         col = c(get(line_i, "col"), if (chart) alarm_col), lty = c(get(line_i, "lty"), if (chart) 1),
         lwd = c(get(line_i, "lwd"), if (chart) 10), pch = c(get(point_i, "pch"), if (chart) NA),
         pt.cex = c(get(point_i, "cex"), if (chart) NA), bg = "white", cex = 0.85)
  par(op_lend)
  box()
  invisible(x)
}