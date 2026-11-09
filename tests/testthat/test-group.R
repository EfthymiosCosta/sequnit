# Inside cusum_group(), every expect_pval() call writes to the group's folder,
# including calls from helper functions and from nested groups.
test_that("cusum_group redirects every call, including from helper functions", {
  dir <- local_cusum()
  helper <- function() expect_pval_small(function() runif(1), name = "inner")
  cusum_group("my group", {
    expect_pval_small(function() runif(1), name = "direct")
    helper()
    cusum_group("sub", helper())
  })
  expect_true(file.exists(file.path(dir, "my_group", "direct.jsonl")))
  expect_true(file.exists(file.path(dir, "my_group", "inner.jsonl")))
  expect_true(file.exists(file.path(dir, "my_group", "sub", "inner.jsonl")))
  expect_false(file.exists(file.path(dir, "inner.jsonl")))
  expect_null(getOption("cusum.group"))
})

# If the code inside a group fails, the group is still closed.
test_that("the group is reset after an error", {
  local_cusum()
  expect_error(cusum_group("g", stop("boom")), "boom")
  expect_null(getOption("cusum.group"))
})
