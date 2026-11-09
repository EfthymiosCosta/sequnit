# Both templates are valid YAML files, load sequnit, and use the right permissions
# and steps for their storage.
test_that("templates are valid workflows", {
  skip_if_not_installed("yaml")
  for (s in c("branch", "artifact")) {
    expect_message(capture.output(yml <- workflow_gen(storage = s)), "not stored")
    wf <- yaml::yaml.load(paste(yml, collapse = "\n"))
    steps <- vapply(wf$jobs$cusum$steps, function(x) x$name, character(1))
    run <- wf$jobs$cusum$steps[[which(steps == "Run the CUSUM tests")]]$run
    expect_match(run, "library(sequnit)", fixed = TRUE)
    if (s == "branch") {
      expect_equal(wf$permissions$contents, "write")
      expect_true("Open the log branch as a worktree" %in% steps)
    } else {
      expect_equal(wf$permissions$contents, "read")
      expect_true(all(c("Restore the CUSUM logs", "Save the CUSUM logs") %in% steps))
    }
  }
})

# save = TRUE writes the file, refuses to replace it, and replaces it with
# overwrite = TRUE.
test_that("save writes the file and respects overwrite", {
  path <- file.path(withr::local_tempdir(), "w", "x.yml")
  expect_message(capture.output(workflow_gen(save = TRUE, path = path)), "is stored")
  expect_true(file.exists(path))
  expect_error(capture.output(workflow_gen(save = TRUE, path = path)), "already exists")
  expect_message(capture.output(workflow_gen(storage = "artifact", save = TRUE, path = path,
                                             overwrite = TRUE)), "is stored")
  expect_true(any(grepl("upload-artifact", readLines(path), fixed = TRUE)))
})

# Invalid arguments stop with an error.
test_that("invalid arguments are rejected", {
  expect_error(workflow_gen(storage = "local"))
  expect_error(workflow_gen(save = NA), "TRUE")
})
