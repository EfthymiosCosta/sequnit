#' Shift parameter of the chart
#'
#' Mean of qnorm(1 - p) under which a one-sided level-`alpha` z-test has power
#' `power`.
#'
#' @param alpha Significance level of a single test run.
#' @param power Power of a single test run against the smallest defect to detect.
#' @return The shift parameter delta.
#' @noRd
.delta <- function(alpha, power) qnorm(1 - alpha) - qnorm(1 - power)

#' Probability of leaving the alarm region in one commit
#'
#' Probability that a chart started at `H` ends a commit of `amax` steps below
#' `h`, when the code has a defect. Uses a numerical approximation.
#'
#' @param gap Gap `H - h`.
#' @param amax Maximum number of test runs per user commit.
#' @param delta Shift parameter of the chart.
#' @param n_low Number of cells below `h`.
#' @param n_gap Number of cells between `h` and `H`.
#' @return The probability of ending the user commit below `h`.
#' @noRd
.leave_prob <- function(gap, amax, delta, n_low = 300, n_gap = 100) {
  h <- 10
  H <- h + gap
  edges <- c(seq(0, h, length.out = n_low + 1), seq(h, H, length.out = n_gap + 1)[-1])
  x <- c(0, (edges[-1] + edges[-length(edges)]) / 2, H)
  lo <- c(-Inf, edges[-length(edges)], H)
  hi <- c(0, edges[-1], Inf)
  m <- x + delta / 2
  G <- pnorm(outer(-m, hi, "+")) - pnorm(outer(-m, lo, "+"))
  p <- c(rep(0, length(x) - 1), 1)
  for (k in seq_len(amax))
    p <- as.numeric(p %*% G)
  sum(p[x < h])
}

#' Gap that gives a target leave probability
#'
#' Solves `.leave_prob(gap) = p_leave` for the gap. Returns 0 if the chart is
#' already unlikely enough to leave when `H = h`.
#'
#' @param amax Maximum number of test runs per user commit.
#' @param p_leave Target probability of leaving the alarm region in one user commit.
#' @param delta Shift parameter of the chart.
#' @return The gap `H - h`.
#' @noRd
.gap_from_pleave <- function(amax, p_leave, delta) {
  f <- function(g) .leave_prob(g, amax, delta) - p_leave
  if (f(0) <= 0) return(0)
  uniroot(f, c(0, amax), extendInt = "downX", tol = 1e-8)$root
}

#' Equally spaced discretisation of `[0, H]`
#'
#' Places `n_bins` equally spaced points on `[0, H]`. The points 0 and `H` are
#' clamped states; each other point stands for the bin around it. The first
#' bin starts at 0 and the last one ends at `H`.
#'
#' @param H Upper bound of the chart.
#' @param n_bins Number of bins used in the discretisation.
#' @return A list with the points `grid`, the bin bounds `lo` and `hi` of the
#'   interior points, `H` and the number of points `ns`.
#' @noRd
.make_mesh <- function(H, n_bins) {
  grid <- seq(0, H, length.out = n_bins)
  dx <- grid[2] - grid[1]
  lo <- grid[2:(n_bins - 1)] - dx / 2
  lo[1] <- 0
  hi <- grid[2:(n_bins - 1)] + dx / 2
  hi[length(hi)] <- H
  list(grid = grid, lo = lo, hi = hi, H = H, ns = n_bins)
}

#' Bin boundaries at thresholds
#'
#' Adds bin boundaries at the thresholds `ts`, so that no bin straddles a
#' threshold. Bins narrower than `min_width` are dropped. Each new bin keeps
#' its original point if it contains exactly one, otherwise it uses its
#' midpoint.
#'
#' @param mesh A discretisation from `.make_mesh()`.
#' @param ts Thresholds, for example `v`.
#' @param min_width Smallest bin width kept.
#' @return A discretisation in the same format as `.make_mesh()`.
#' @noRd
.refine_mesh <- function(mesh, ts, min_width = 1e-8) {
  H <- mesh$H
  orig <- mesh$grid[2:(mesh$ns - 1)]
  ts <- ts[is.finite(ts) & ts > min_width & ts < H - min_width]
  knots <- sort(unique(c(mesh$lo[1], mesh$hi, ts)))
  knots <- knots[c(TRUE, diff(knots) > min_width)]
  lo <- head(knots, -1)
  hi <- tail(knots, -1)
  pts <- vapply(seq_along(lo), function(j) {
    cand <- orig[orig > lo[j] & orig < hi[j]]
    if (length(cand) == 1L) cand else (lo[j] + hi[j]) / 2
  }, numeric(1))
  list(grid = c(0, pts, H), lo = lo, hi = hi, H = H, ns = length(pts) + 2L)
}

#' Transition matrix of one test run
#'
#' Constructs a one-step transition matrix for the region `[0, H]`. From
#' each point, one test run adds a normally distributed increment. Mass below 0
#' goes to the point 0, mass above `H` to the point `H`, and the rest to the
#' bin it falls in.
#'
#' @param mesh A discretisation.
#' @param mu Mean of each increment: -delta / 2 without a defect, delta / 2
#'   with one.
#' @return The ns x ns transition matrix.
#' @noRd
.step_kernel <- function(mesh, mu) {
  ns <- mesh$ns
  m <- mesh$grid + mu
  P <- matrix(0, ns, ns)
  P[, 1] <- pnorm(-m)
  P[, ns] <- 1 - pnorm(mesh$H - m)
  P[, 2:(ns - 1)] <- pnorm(outer(-m, mesh$hi, "+")) - pnorm(outer(-m, mesh$lo, "+"))
  P <- pmax(P, 0)
  P / rowSums(P)
}

#' Split a transition matrix at a threshold
#'
#' Splits `G` into the moves that land at or above `t` and those that land
#' below it. The bin containing `t` is split exactly at `t`.
#'
#' @param G Transition matrix of one test run.
#' @param mesh A discretisation.
#' @param t Threshold, usually `v`.
#' @param mu Mean of each increment.
#' @return A list with matrices `above` and `below`, which add up to `G`.
#' @noRd
.split_kernel <- function(G, mesh, t, mu) {
  ns <- mesh$ns
  t <- min(max(t, 0), mesh$H)
  if (t >= mesh$H) return(list(above = matrix(0, ns, ns), below = G))
  if (t <= 0) return(list(above = G, below = matrix(0, ns, ns)))
  i <- findInterval(t, c(mesh$lo[1], mesh$hi), left.open = TRUE)
  j <- i + 1L
  above <- G
  above[, seq_len(j - 1L)] <- 0
  above[, j] <- pnorm(mesh$hi[i] - mesh$grid - mu) - pnorm(t - mesh$grid - mu)
  list(above = above, below = G - above)
}

#' Transition matrix of one user commit
#'
#' A commit runs the test once, then again while the statistic stays at or
#' above `v`, up to `amax` runs. With A the moves landing at or above `v` and B
#' those landing below, the commit matrix is
#' K = B + A B + ... + A^(amax - 2) B + A^(amax - 1) G.
#'
#' @param mesh A discretisation, refined at `v`.
#' @param v Acceleration threshold.
#' @param amax Maximum number of test runs per user commit.
#' @param mu Mean of each increment.
#' @return A list with the commit matrix `K` and `cost`, the expected number of
#'   test runs in a commit started from each point.
#' @noRd
.jump_kernel <- function(mesh, v, amax, mu) {
  G <- .step_kernel(mesh, mu)
  sp <- .split_kernel(G, mesh, v, mu)
  ns <- mesh$ns
  Mn <- diag(ns)
  K <- matrix(0, ns, ns)
  cost <- numeric(ns)
  for (n in seq_len(amax) - 1L) {
    S <- if (n < amax - 1L) Mn %*% sp$below else Mn %*% G
    K <- K + S
    cost <- cost + (n + 1) * rowSums(S)
    if (n < amax - 1L) Mn <- Mn %*% sp$above
  }
  list(K = K, cost = cost)
}

#' Evaluate a chart for given thresholds
#'
#' Computes, for thresholds `h` and `v`, the average number of commits until a
#' signal starting from 0, without (`u0`) and with (`u1`) a defect, and the
#' average number of test runs per commit without a defect (`cbar0`), using
#' the stationary distribution of the chart.
#'
#' @param v Acceleration threshold.
#' @param h Signalling threshold.
#' @param gap Gap `H - h`.
#' @param amax Maximum number of test runs per user commit.
#' @param delta Shift parameter of the chart.
#' @param n_bins Number of bins used in the discretisation.
#' @return A list with `h`, `v`, `u0`, `u1` and `cbar0`.
#' @noRd
.jump_evaluate <- function(v, h, gap, amax, delta, n_bins) {
  mesh <- .refine_mesh(.make_mesh(h + gap, n_bins), v)
  ns <- mesh$ns
  tr <- which(mesh$grid < h - 1e-9 * max(1, h))
  k0 <- .jump_kernel(mesh, v, amax, -delta / 2)
  k1 <- .jump_kernel(mesh, v, amax, delta / 2)
  A <- t(k0$K) - diag(ns)
  A[ns, ] <- 1
  pi0 <- as.numeric(solve(qr(A, LAPACK = TRUE), c(rep(0, ns - 1), 1)))
  pi0 <- pi0 / sum(pi0)
  u0 <- solve(qr(diag(length(tr)) - k0$K[tr, tr, drop = FALSE], LAPACK = TRUE), rep(1, length(tr)))[1]
  u1 <- solve(qr(diag(length(tr)) - k1$K[tr, tr, drop = FALSE], LAPACK = TRUE), rep(1, length(tr)))[1]
  list(h = h, v = v, u0 = u0, u1 = u1, cbar0 = sum(k0$cost * pi0))
}

#' Find the h at which u0 reaches U0
#'
#' Finds an interval `[lo, hi]` with `u0(lo) < U0 <= u0(hi)` and uses `uniroot()`
#' to determine the value of `h` for which the U0 criterion is met.
#'
#' @param u0_at Function of `h` returning `u0`.
#' @param U0 Target value of `u0`.
#' @param start Starting value of `h`.
#' @return The value of `h`.
#' @noRd
.solve_h <- function(u0_at, U0, start) {
  g <- function(h) {
    u <- u0_at(h)
    if (!is.finite(u) || u <= 0) Inf else log(u) - log(U0)
  }
  lo <- start
  g_lo <- g(lo)
  while (g_lo > 0) {
    lo <- lo / 2
    g_lo <- g(lo)
  }
  hi <- lo
  g_hi <- g_lo
  while (g_hi < 0) {
    lo <- hi
    g_lo <- g_hi
    hi <- 1.5 * hi
    g_hi <- g(hi)
  }
  if (!is.finite(g_hi))
    stop("u0 could not be computed while bracketing h; try a smaller U0 or a larger n_bins.", call. = FALSE)
  if (g_hi == 0) return(hi)
  uniroot(g, c(lo, hi), f.lower = g_lo, f.upper = g_hi, tol = 1e-8)$root
}

#' Search for the chart's thresholds
#'
#' Computes the gap from `p_leave`, then the range of `h` in which `u0 >= U0`
#' is achievable. For each candidate threshold `h`, `v` is the smallest value
#' satisfying both `u0 >= U0` and `cbar0 <= cbar0max` constraints. The search
#' scans `n_scan` values of `h` in parallel, then moves `h` in steps around the
#' best value, halving the step when no move improves `u1`. The search stops
#' when the refinement step is smaller than `tol_h` or after `max_h_iter` iterations.
#'
#' @inheritParams cusum_design
#' @return The design, as returned by `cusum_design()`.
#' @noRd
.cusum_search <- function(alpha, power, U0, cbar0max, amax, p_leave, n_bins, n_scan,
                          tol_h, max_h_iter, cores) {
  delta <- .delta(alpha, power)
  gap <- .gap_from_pleave(amax, p_leave, delta)
  v_lo <- 1e-8
  evaluate <- function(v, h) .jump_evaluate(v, h, gap, amax, delta, n_bins)
  h_lo <- .solve_h(function(h) evaluate(h * (1 - 1e-8), h)$u0, U0, start = 1)
  h_up <- .solve_h(function(h) evaluate(v_lo, h)$u0, U0, start = h_lo)
  infeasible <- list(feasible = FALSE, u1 = Inf)
  profile_at <- function(h) {
    out <- tryCatch({
      v_hi <- h * (1 - 1e-8)
      e_hi <- evaluate(v_hi, h)
      if (!all(is.finite(c(e_hi$u0, e_hi$u1, e_hi$cbar0))) || e_hi$cbar0 > cbar0max || e_hi$u0 < U0) {
        infeasible
      } else {
        e_lo <- evaluate(v_lo, h)
        vc <- if (e_lo$cbar0 <= cbar0max) v_lo else
          uniroot(function(v) cbar0max - evaluate(v, h)$cbar0, c(v_lo, v_hi),
                  f.lower = cbar0max - e_lo$cbar0, f.upper = cbar0max - e_hi$cbar0, tol = 1e-8)$root
        vu <- if (e_lo$u0 >= U0) v_lo else
          uniroot(function(v) evaluate(v, h)$u0 - U0, c(v_lo, v_hi),
                  f.lower = e_lo$u0 - U0, f.upper = e_hi$u0 - U0, tol = 1e-8)$root
        e <- evaluate(max(vc, vu) * (1 + 1e-9), h)
        e$feasible <- e$cbar0 <= cbar0max * (1 + 1e-6) && e$u0 >= U0 * (1 - 1e-6)
        e
      }
    }, error = function(err) infeasible)
    out$h <- h
    out
  }
  key <- function(h) sprintf("%.10f", h)
  scanned <- parallel::mclapply(seq(h_lo, h_up, length.out = n_scan), profile_at, mc.cores = cores)
  cache <- list()
  for (p in scanned) cache[[key(p$h)]] <- p
  at <- function(h) {
    k <- key(h)
    if (is.null(cache[[k]])) cache[[k]] <<- profile_at(h)
    cache[[k]]
  }
  feas <- Filter(function(p) isTRUE(p$feasible), cache)
  if (!length(feas))
    stop("No feasible design found for h in [", signif(h_lo, 4), ", ", signif(h_up, 4), "].", call. = FALSE)
  cur <- feas[[which.min(vapply(feas, function(p) p$u1, numeric(1)))]]
  step <- (h_up - h_lo) / (2 * n_scan)
  dir <- -1
  for (it in seq_len(max_h_iter)) {
    moved <- FALSE
    for (d in unique(c(dir, -dir))) {
      hn <- cur$h + d * step
      if (hn < h_lo || hn > h_up) next
      p <- at(hn)
      if (isTRUE(p$feasible) && p$u1 < cur$u1) {
        cur <- p
        dir <- d
        moved <- TRUE
        break
      }
    }
    if (!moved) {
      step <- step / 2
      if (step < tol_h) break
    }
  }
  feas <- Filter(function(p) isTRUE(p$feasible), cache)
  best <- feas[[which.min(vapply(feas, function(p) p$u1, numeric(1)))]]
  list(alpha = alpha, power = power, U0 = U0, cbar0max = cbar0max, amax = amax,
       p_leave = p_leave, n_bins = n_bins, delta = delta, gap = gap,
       p_recover = .leave_prob(gap, amax, -delta),
       h = best$h, H = best$h + gap, v = best$v,
       u0 = best$u0, u1 = best$u1, cbar0 = best$cbar0,
       h_lo = h_lo, h_up = h_up)
}

#' File name of a cached design
#'
#' @inheritParams cusum_design
#' @return The file name without extension.
#' @noRd
.design_key <- function(alpha, power, U0, cbar0max, amax, p_leave, n_bins) {
  sprintf("design_alpha%g_power%g_U0%g_cbar0max%g_amax%d_pleave%g_nbins%d",
          alpha, power, U0, cbar0max, as.integer(amax), p_leave, as.integer(n_bins))
}