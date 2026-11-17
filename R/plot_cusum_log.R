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

#' Prepare a log for plotting
#'
#' Sets the title, and works out the positions and labels of the commit times
#' on the x-axis. Used by `plot.cusum_log()`.
#'
#' @inheritParams plot.cusum_log
#' @param log A log read by `read_cusum_log()`.
#' @param prefix Text placed before the chart's name in the default title.
#' @return A list with the log's `header` and `data`, the title `main`, the
#'   x positions `x`, the x-axis limits `xlim`, and the tick positions `ticks`
#'   and labels `tick_labels`.
#' @noRd
.prep_plot_log <- function(log, main, prefix, time_format, n_ticks) {
  hd <- log$header
  dat <- log$data
  n <- nrow(dat)
  if (n == 0L)
    stop("Log '", hd$name, "' contains no commits yet.", call. = FALSE)
  if (is.null(main))
    main <- paste0(prefix, hd$name)
  times <- as.POSIXct(dat$t, format = "%Y-%m-%d %H:%M:%S")
  if (is.null(time_format))
    time_format <- if (diff(range(as.numeric(times))) < 3600) "%Y-%m-%d\n%H:%M:%S" else "%Y-%m-%d\n%H:%M"
  ticks <- unique(round(seq(1, n, length.out = min(n, n_ticks))))
  list(header = hd, data = dat, main = main, x = seq_len(n), xlim = c(0.5, n + 0.5),
       ticks = ticks, tick_labels = format(times[ticks], time_format))
}

#' Group threshold labels that would overlap
#'
#' Puts the thresholds, in increasing order, into groups whose neighbouring
#' values are closer than `min_gap`, so that each group gets one label.
#'
#' @param y Named thresholds in increasing order, such as `c(h = , H = )`.
#' @param min_gap Smallest vertical distance between two separate labels.
#' @return A list of named numeric vectors, one per group.
#' @noRd
.group_labels <- function(y, min_gap) {
  split(y, cumsum(c(TRUE, diff(y) >= min_gap)))
}

#' Plot a CUSUM log
#'
#' Plots a log read by [read_cusum_log()].
#'
#' With `type = "chart"`, it plots the CUSUM statistic, one point per commit.
#' Dashed lines mark `h` and `H`, and the area between them, where the chart
#' signals, is shaded red.
#'
#' With `type = "steps"`, it plots the number of tests run in each commit
#' (equivalently, the number of p-values drawn). A dashed line marks the
#' maximum, `amax`.
#'
#' @param x A log read by [read_cusum_log()].
#' @param type `"chart"` for the CUSUM chart, or `"steps"` for the number of
#'   tests run per commit.
#' @param show_probs If `TRUE`, splits \eqn{[0, H]} into `M` equal-width
#'   regions and labels each with \eqn{\pi_0}, its probability under the
#'   stationary distribution of the CUSUM statistic when the code has no bug.
#'   Only used with `type = "chart"`.
#' @param M Number of regions when `show_probs = TRUE`.
#' @param show_acc If `TRUE`, adds a right axis showing the acceleration: one
#'   test run per commit below `v`, and up to `amax` above it. Only used with
#'   `type = "chart"`.
#' @param main Title of the plot. Defaults to the chart's name.
#' @param time_format Format of the commit times shown on the x-axis, as in
#'   [format.POSIXct()]. Defaults to date and time on two lines, with seconds
#'   if the log spans less than an hour.
#' @param n_ticks Maximum number of labelled commits on the x-axis.
#' @param line_args List of arguments passed to [lines()] for the line through
#'   the commits (`type = "chart"`) or the bars (`type = "steps"`), e.g.
#'   `list(col = "navy", lwd = 1)`.
#' @param point_args List of arguments passed to [points()] for the point of
#'   each commit, e.g. `list(col = "red", pch = 1)`.
#' @param ... Further graphical arguments passed to [plot()], e.g.
#'   `cex.main` or `ylab`.
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
#' for (i in 1:20)
#'   expect_pval(function() runif(1), name = "example")
#' log <- read_cusum_log(file.path(dir, "example.jsonl"))
#' plot(log)
#' abline(h = log$header$v, lty = 3)
#' plot(log, show_probs = TRUE, show_acc = TRUE)
#' plot(log, type = "steps")
#' plot(log, line_args = list(col = "navy"), point_args = list(col = "red"))
#' sequnit_reset()
#' options(old)
#' @export
plot.cusum_log <- function(x, type = c("chart", "steps"), show_probs = FALSE, M = 10,
                           show_acc = FALSE, main = NULL, time_format = NULL,
                           n_ticks = 6, line_args = list(), point_args = list(), ...) {
  type <- match.arg(type)
  if (!is.list(line_args) || !is.list(point_args))
    stop("`line_args` and `point_args` must be lists.", call. = FALSE)
  if (type == "steps") {
    pr <- .prep_plot_log(x, main, "Computational steps: ", time_format, n_ticks)
    hd <- pr$header
    op <- par(mar = c(5.5, 4.5, 3, 4.5))
    on.exit(par(op))
    do.call(plot, utils::modifyList(list(x = pr$x, y = pr$data$steps, type = "n",
                                         xlim = pr$xlim, ylim = c(0, hd$amax), xaxs = "i",
                                         xaxt = "n", yaxt = "n", xlab = "",
                                         ylab = "computational steps", main = pr$main),
                                    list(...)))
    do.call(lines, utils::modifyList(list(x = pr$x, y = pr$data$steps, type = "h", lwd = 2),
                                     line_args))
    do.call(points, utils::modifyList(list(x = pr$x, y = pr$data$steps, pch = 16, cex = 0.5),
                                      point_args))
    abline(h = hd$amax, col = "firebrick", lty = 2, lwd = 2)
    axis(2, at = unique(round(pretty(c(0, hd$amax)))), las = 1)
    axis(1, at = pr$ticks, labels = pr$tick_labels, cex.axis = 0.8, padj = 0.5)
    title(xlab = "commit", line = 4)
    return(invisible(x))
  }
  if (!is.logical(show_probs) || length(show_probs) != 1L || is.na(show_probs) ||
      !is.logical(show_acc) || length(show_acc) != 1L || is.na(show_acc))
    stop("`show_probs` and `show_acc` must be TRUE or FALSE.", call. = FALSE)
  if (show_probs && (!is.numeric(M) || length(M) != 1L || M < 1 || M %% 1 != 0))
    stop("`M` must be a single positive integer.", call. = FALSE)
  pr <- .prep_plot_log(x, main, "CUSUM chart: ", time_format, n_ticks)
  hd <- pr$header
  op <- par(mar = c(5.5, 4.5, 3, if (show_acc) 4.5 else 1.5))
  on.exit(par(op))
  do.call(plot, utils::modifyList(list(x = NA, xlim = pr$xlim, ylim = c(0, hd$H), xaxs = "i",
                                       yaxs = "i", xaxt = "n", xlab = "",
                                       ylab = expression("CUSUM statistic " * S[t]),
                                       main = pr$main),
                                  list(...)))
  usr <- par("usr")
  rect(usr[1], hd$h, usr[2], hd$H, col = adjustcolor("#de2d26", alpha.f = 0.5), border = NA)
  if (show_probs) {
    regions <- .stationary_regions(hd, as.integer(M))
    if (M > 1)
      abline(h = regions$upper[-M], col = adjustcolor("black", alpha.f = 0.2))
    text(usr[2], (regions$lower + regions$upper) / 2, .format_prob(regions$prob),
         pos = 2, cex = 0.75, font = 2, col = adjustcolor("black", alpha.f = 0.6))
    x$regions <- regions
  }
  abline(h = c(hd$h, hd$H), col = "grey30", lty = 2, lwd = 2)
  x_lab <- usr[1] + 0.01 * (usr[2] - usr[1])
  for (g in .group_labels(c(h = hd$h, H = hd$H), 1.5 * strheight("h", cex = 0.85))) {
    top <- "H" %in% names(g)
    text(x_lab, if (top) hd$H else max(g), paste(names(g), collapse = ", "),
         adj = c(0, if (top) 1.4 else -0.4), cex = 0.85, font = 2, col = "grey20")
  }
  do.call(lines, utils::modifyList(list(x = pr$x, y = pr$data$S, lwd = 2), line_args))
  do.call(points, utils::modifyList(list(x = pr$x, y = pr$data$S, pch = 16, cex = 0.5),
                                    point_args))
  axis(1, at = pr$ticks, labels = pr$tick_labels, cex.axis = 0.8, padj = 0.5)
  title(xlab = "commit", line = 4)
  if (show_acc) {
    axis(4, at = c(0, hd$v, hd$H), labels = FALSE)
    axis(4, at = c(hd$v / 2, (hd$v + hd$H) / 2), labels = c(1, hd$amax), tick = FALSE, las = 1)
    mtext("acceleration a(S)", side = 4, line = 2.5)
  }
  box()
  invisible(x)
}