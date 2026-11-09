# The gap solves its defining equation: started at H with the code defective,
# the chart ends one commit below h with probability p_leave.
test_that("the gap gives the requested leave probability", {
  delta <- .delta(0.05, 0.2)
  for (amax in c(2L, 10L)) {
    g <- .gap_from_pleave(amax, 0.01, delta)
    expect_equal(.leave_prob(g, amax, delta), 0.01, tolerance = 1e-6)
  }
})

# A larger target needs a smaller gap, and a target already met at H = h
# gives a gap of 0.
test_that("the gap decreases with p_leave and can be 0", {
  delta <- .delta(0.05, 0.2)
  expect_gt(.gap_from_pleave(10L, 0.01, delta), .gap_from_pleave(10L, 0.05, delta))
  expect_equal(.gap_from_pleave(2L, 0.9, delta), 0)
})
