#' Stationary probabilities of equal-width regions
#'
#' Splits \eqn{[0, H]} into `M` equal-width regions and gives the probability
#' of each under the chart's stationary distribution when the code has no
#' defect, computed using a Markov chain approximation.
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
#' @return Labels such as "p = 12.3%".
#' @noRd
.format_prob <- function(p) {
  ifelse(p == 0, "p = 0", ifelse(p >= 0.001, sprintf("p = %.1f%%", 100 * p), sprintf("p = %.1e", p)))
}

#' Prepare a log for plotting
#'
#' Reads the log, sets the title, and works out the positions and labels of
#' the commit times on the x-axis. Shared by `plot_pval_log()` and
#' `plot_pval_steps()`.
#'
#' @inheritParams plot_pval_log
#' @param prefix Text placed before the chart's name in the default title.
#' @return A list with the log's `header` and `data`, the title `main`, the
#'   x positions `x`, the x-axis limits `xlim`, and the tick positions `ticks`
#'   and labels `tick_labels`.
#' @noRd
.prep_plot_log <- function(file_path, main, prefix, time_format, n_ticks) {
  log <- read_pval_log(file_path)
  hd <- log$header
  dat <- log$data
  n <- nrow(dat)
  if (n == 0L)
    stop("Log '", file_path, "' contains no commits yet.", call. = FALSE)
  if (is.null(main)) {
    nm <- hd$name
    if (is.null(nm) || !nzchar(nm)) nm <- sub("\\.jsonl$", "", basename(file_path))
    main <- paste0(prefix, nm)
  }
  times <- as.POSIXct(dat$t, format = "%Y-%m-%d %H:%M:%S")
  if (is.null(time_format))
    time_format <- if (diff(range(as.numeric(times))) < 3600) "%Y-%m-%d\n%H:%M:%S" else "%Y-%m-%d\n%H:%M"
  ticks <- unique(round(seq(1, n, length.out = min(n, n_ticks))))
  list(header = hd, data = dat, main = main, x = seq_len(n), xlim = c(0.5, n + 0.5),
       ticks = ticks, tick_labels = format(times[ticks], time_format))
}

#' Plot a CUSUM chart
#'
#' Plots the CUSUM statistic of a log written by [expect_pval()], one point per
#' commit, against coloured bands from green at 0 to red at `H`. Dashed lines
#' mark `h` and `H`, and a dotted line marks `v`. The right axis shows the
#' acceleration: one p-value per commit below `v`, and up to `amax` above it.
#'
#' @param file_path Path to the log file.
#' @param M Number of equal-width regions of `[0, H]`. Each region is labelled
#'   with its probability under the chart's stationary distribution when the
#'   code has no defect.
#' @param main Title of the plot. Defaults to the chart's name.
#' @param time_format Format of the commit times shown on the x-axis, as in
#'   [format.POSIXct()]. Defaults to date and time on two lines, with seconds
#'   if the log spans less than an hour.
#' @param n_ticks Maximum number of labelled commits on the x-axis.
#'
#' @return Invisibly, a list with the log's `header` and `data`, and `regions`,
#'   a data frame with the bounds and stationary probability of each region.
#'
#' @seealso [plot_pval_steps()] for the number of p-values drawn per commit.
#'
#' @examples
#' set.seed(1)
#' dir <- tempfile()
#' old <- options(cusum.dir = dir)
#' for (i in 1:20)
#'   expect_pval(function() runif(1), name = "example", U0 = 100, n_bins = 31,
#'               n_scan = 4, cores = 1, quiet = TRUE)
#' plot_pval_log(file.path(dir, "example.jsonl"))
#' options(old)
#' @export
plot_pval_log <- function(file_path, M = 10, main = NULL, time_format = NULL, n_ticks = 6) {
  if (!is.numeric(M) || length(M) != 1L || M < 1 || M %% 1 != 0)
    stop("`M` must be a single positive integer.", call. = FALSE)
  pr <- .prep_plot_log(file_path, main, "CUSUM chart: ", time_format, n_ticks)
  hd <- pr$header
  regions <- .stationary_regions(hd, as.integer(M))
  band_breaks <- seq(0, hd$H, length.out = 101)
  band_cols <- colorRampPalette(c("#66cc66", "#fffbc8", "#fc9272", "#de2d26"))(100)
  op <- par(mar = c(5.5, 4.5, 3, 4.5))
  on.exit(par(op))
  plot(NA, xlim = pr$xlim, ylim = c(0, hd$H), xaxs = "i", yaxs = "i", xaxt = "n",
       xlab = "", ylab = expression("CUSUM statistic " * S[t]), main = pr$main)
  usr <- par("usr")
  rect(usr[1], band_breaks[-101], usr[2], band_breaks[-1], col = band_cols, border = NA)
  if (M > 1)
    abline(h = regions$upper[-M], col = adjustcolor("black", alpha.f = 0.2))
  text(usr[2], (regions$lower + regions$upper) / 2, .format_prob(regions$prob),
       pos = 2, cex = 0.75, font = 2, col = adjustcolor("black", alpha.f = 0.6))
  abline(h = hd$v, col = "grey30", lty = 3, lwd = 2)
  abline(h = c(hd$h, hd$H), col = "grey30", lty = 2, lwd = 2)
  x_lab <- usr[1] + 0.01 * (usr[2] - usr[1])
  text(x_lab, c(hd$v, hd$h), c("v", "h"), adj = c(0, -0.4), cex = 0.85, font = 2, col = "grey20")
  text(x_lab, hd$H, "H", adj = c(0, 1.4), cex = 0.85, font = 2, col = "grey20")
  lines(pr$x, pr$data$S, type = "l", lwd = 2)
  points(pr$x, pr$data$S, pch = 16, cex = 0.5)
  axis(1, at = pr$ticks, labels = pr$tick_labels, cex.axis = 0.8, padj = 0.5)
  title(xlab = "commit", line = 4)
  axis(4, at = c(0, hd$v, hd$H), labels = FALSE)
  axis(4, at = c(hd$v / 2, (hd$v + hd$H) / 2), labels = c(1, hd$amax), tick = FALSE, las = 1)
  mtext("acceleration a(S)", side = 4, line = 2.5)
  box()
  invisible(list(header = hd, data = pr$data, regions = regions))
}

#' Plot the number of tests run per commit
#'
#' Plots, for each commit of a log written by [expect_pval()], the number of
#' tests run (equivalently, the number of p-values drawn).
#' A dashed line marks the maximum, `amax`.
#'
#' @inheritParams plot_pval_log
#'
#' @return Invisibly, a list with the log's `header` and `data`.
#'
#' @seealso [plot_pval_log()] for the chart itself.
#'
#' @examples
#' set.seed(1)
#' dir <- tempfile()
#' old <- options(cusum.dir = dir)
#' for (i in 1:20)
#'   expect_pval(function() runif(1), name = "example", U0 = 100, n_bins = 31,
#'               n_scan = 4, cores = 1, quiet = TRUE)
#' plot_pval_steps(file.path(dir, "example.jsonl"))
#' options(old)
#' @export
plot_pval_steps <- function(file_path, main = NULL, time_format = NULL, n_ticks = 6) {
  pr <- .prep_plot_log(file_path, main, "Computational steps: ", time_format, n_ticks)
  hd <- pr$header
  op <- par(mar = c(5.5, 4.5, 3, 4.5))
  on.exit(par(op))
  plot(pr$x, pr$data$steps, type = "h", lwd = 2, xlim = pr$xlim, ylim = c(0, hd$amax),
       xaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = "computational steps",
       main = pr$main)
  points(pr$x, pr$data$steps, pch = 16, cex = 0.5)
  abline(h = hd$amax, col = "firebrick", lty = 2, lwd = 2)
  axis(2, at = unique(round(pretty(c(0, hd$amax)))), las = 1)
  axis(1, at = pr$ticks, labels = pr$tick_labels, cex.axis = 0.8, padj = 0.5)
  title(xlab = "commit", line = 4)
  invisible(list(header = hd, data = pr$data))
}
