#' Workflow templates
#'
#' GitHub Actions workflows printed or saved by `workflow_gen()`, one for each
#' storage option. Both run the tests with `sequnit` on the same triggers. They
#' differ only in where the CUSUM logs are kept between runs. `branch` opens a
#' branch of the repository as a worktree and pushes the updated logs to it.
#' `artifact` downloads the logs from the latest workflow artifact and uploads
#' them again. Each template is a raw string, so the YAML appears exactly as
#' written.
#'
#' @format A named list of two character strings, `branch` and `artifact`.
#' @noRd

.workflow_templates <- list(
  branch = r"---(name: "CUSUM Drift Monitor"

on:
  # Runs on every push to main. Delete these two lines when the tests check
  # something outside this repository (for example a new R version), so that
  # only the schedule advances the charts.
  push:
    branches: [ "main" ]

  # Allows starting a run by hand, from the Actions tab or with `gh workflow run`.
  workflow_dispatch:

  # Runs on a timer (cron syntax, in UTC): daily at 03:17.
  # For hourly runs use '17 * * * *'; for Mondays only use '17 3 * * 1'.
  # Avoid minute 0, when GitHub often delays or drops scheduled runs.
  schedule:
    - cron: '17 3 * * *'

env:
  # Name of the branch that stores the CUSUM logs. It is created on the first run.
  CUSUM_DIR: cusum-data
  # Set to false to report a signal as a test warning instead of failing the run.
  CUSUM_FAIL_ON_SIGNAL: true

# Runs one at a time, so that each run continues from the logs of the previous one.
# With queue: single, at most one run waits; a newer run replaces the waiting
# one, so in a burst of pushes some commits do not advance the charts. Use
# queue: max to keep up to 100 waiting runs, so that every commit advances them.
concurrency:
  group: ${{ github.workflow }}
  cancel-in-progress: false
  queue: single

# The logs are pushed to the branch CUSUM_DIR after every run, so the workflow
# needs write access to the repository.
permissions:
  contents: write

jobs:
  cusum:
    runs-on: ubuntu-latest
    # Maximum length of a run, in minutes. The first run for a new design
    # setting includes the design search, which takes a few minutes.
    timeout-minutes: 60
    steps:
      - name: Check out the code
        uses: actions/checkout@v4

      # Opens the log branch in the folder CUSUM_DIR as a worktree, sharing the
      # repository's .git with the code. On the first run, the branch is new and empty.
      - name: Open the log branch as a worktree
        run: |
          if git ls-remote --exit-code --heads origin "$CUSUM_DIR" > /dev/null; then
            git fetch --depth 1 origin "refs/heads/$CUSUM_DIR:refs/remotes/origin/$CUSUM_DIR"
            git worktree add -b "$CUSUM_DIR" "$CUSUM_DIR" "origin/$CUSUM_DIR"
          else
            git worktree add --orphan -b "$CUSUM_DIR" "$CUSUM_DIR"
          fi

      - name: Set up R
        uses: r-lib/actions/setup-r@v2
        with:
          # To test against another R version, use "devel", "oldrel" or a version number such as "4.4.1".
          r-version: "release"
          use-public-rspm: true

      - name: Install dependencies
        uses: r-lib/actions/setup-r-dependencies@v2
        with:
          packages: |
            any::testthat
            github::EfthymiosCosta/sequnit
          # EDIT: if the tests need other packages, uncomment the lines below
          # and list them. deps::. installs every package listed in the
          # repository's DESCRIPTION file.
          # extra-packages: |
          #   any::dplyr
          #   deps::.

      - name: Run the CUSUM tests
        shell: Rscript {0}
        run: |
          library(sequnit)
          # EDIT: load the code under test here. For the package in this
          # repository, uncomment the line below and add any::pkgload to
          # extra-packages in the step above.
          # pkgload::load_all()
          library(testthat)
          dir.create(Sys.getenv("CUSUM_DIR"), showWarnings = FALSE)
          options(cusum.dir = normalizePath(Sys.getenv("CUSUM_DIR")),
                  cusum.fail_on_signal = as.logical(Sys.getenv("CUSUM_FAIL_ON_SIGNAL")))
          # Change "tests" if the test files are elsewhere, for example "tests/testthat".
          testthat::test_dir("tests", stop_on_failure = TRUE)

      - name: Commit and push the CUSUM logs
        if: always()
        working-directory: ${{ env.CUSUM_DIR }}
        run: |
          git config user.name "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add -A
          git commit -m "Update CUSUM logs for ${{ github.sha }} [skip ci]" || echo "No changes to commit"
          git push origin "HEAD:$CUSUM_DIR"
)---",
  artifact = r"---(name: "CUSUM Drift Monitor"

on:
  # Runs on every push to main. Delete these two lines when the tests check
  # something outside this repository (for example a new R version), so that
  # only the schedule advances the charts.
  push:
    branches: [ "main" ]

  # Allows starting a run by hand, from the Actions tab or with `gh workflow run`.
  workflow_dispatch:

  # Runs on a timer (cron syntax, in UTC): daily at 03:17.
  # For hourly runs use '17 * * * *'; for Mondays only use '17 3 * * 1'.
  # Avoid minute 0, when GitHub often delays or drops scheduled runs.
  # The stored logs expire after the repository's artifact retention period,
  # so keep at least one run within that period, or the charts are lost.
  schedule:
    - cron: '17 3 * * *'

env:
  # Name of the artifact that stores the CUSUM logs.
  CUSUM_DIR: cusum-data
  # Set to false to report a signal as a test warning instead of failing the run.
  CUSUM_FAIL_ON_SIGNAL: true

# Runs one at a time, so that each run continues from the logs of the previous one.
# With queue: single, at most one run waits; a newer run replaces the waiting
# one, so in a burst of pushes some commits do not advance the charts. Use
# queue: max to keep up to 100 waiting runs, so that every commit advances them.
concurrency:
  group: ${{ github.workflow }}
  cancel-in-progress: false
  queue: single

# The logs are kept as a workflow artifact, so read access is enough.
permissions:
  contents: read
  actions: read

jobs:
  cusum:
    runs-on: ubuntu-latest
    # Maximum length of a run, in minutes. The first run for a new design
    # setting includes the design search, which takes a few minutes.
    timeout-minutes: 60
    steps:
      - name: Check out the code
        uses: actions/checkout@v4

      - name: Restore the CUSUM logs
        env:
          GH_TOKEN: ${{ github.token }}
        run: |
          mkdir -p "$CUSUM_DIR"
          art=$(gh api "repos/${GITHUB_REPOSITORY}/actions/artifacts?name=${CUSUM_DIR}&per_page=1" --jq '.artifacts[0] // empty | select(.expired == false) | "\(.workflow_run.id) \(.expires_at)"')
          if [ -n "$art" ]; then
            run_id=${art%% *}
            expires=${art#* }
            gh run download "$run_id" --name "$CUSUM_DIR" --dir "$CUSUM_DIR"
            left=$(( ($(date -d "$expires" +%s) - $(date +%s)) / 86400 ))
            echo "::notice::Restored the CUSUM logs from run $run_id; they expire in $left days."
            if [ "$left" -lt 7 ]; then
              echo "::warning::The stored CUSUM logs expire in $left days. Runs must happen more often than the artifact retention period, or the charts are lost."
            fi
          else
            echo "::notice::No stored CUSUM logs found; starting new charts."
          fi

      - name: Set up R
        uses: r-lib/actions/setup-r@v2
        with:
          # To test against another R version, use "devel", "oldrel" or a version number such as "4.4.1".
          r-version: "release"
          use-public-rspm: true

      - name: Install dependencies
        uses: r-lib/actions/setup-r-dependencies@v2
        with:
          packages: |
            any::testthat
            github::EfthymiosCosta/sequnit
          # EDIT: if the tests need other packages, uncomment the lines below
          # and list them. deps::. installs every package listed in the
          # repository's DESCRIPTION file.
          # extra-packages: |
          #   any::dplyr
          #   deps::.

      - name: Run the CUSUM tests
        shell: Rscript {0}
        run: |
          library(sequnit)
          # EDIT: load the code under test here. For the package in this
          # repository, uncomment the line below and add any::pkgload to
          # extra-packages in the step above.
          # pkgload::load_all()
          library(testthat)
          dir.create(Sys.getenv("CUSUM_DIR"), showWarnings = FALSE)
          options(cusum.dir = normalizePath(Sys.getenv("CUSUM_DIR")),
                  cusum.fail_on_signal = as.logical(Sys.getenv("CUSUM_FAIL_ON_SIGNAL")))
          # Change "tests" if the test files are elsewhere, for example "tests/testthat".
          testthat::test_dir("tests", stop_on_failure = TRUE)

      - name: Save the CUSUM logs
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: ${{ env.CUSUM_DIR }}
          path: ${{ env.CUSUM_DIR }}
          if-no-files-found: ignore
)---")

#' Generate a GitHub Actions workflow template for CUSUM-monitored unit tests
#'
#' Prints and optionally saves a GitHub Actions workflow (YAML file) that runs
#' the repository's tests on each trigger and keeps the CUSUM logs written by
#' [expect_pval()] between runs. Each run advances every chart by one user commit,
#' so the logs need to persist from one run to the next; `storage` chooses where
#' the logs are kept. The file is saved to `path` only if `save = TRUE`.
#'
#' @param storage Where the CUSUM logs are kept between runs.
#'   \describe{
#'     \item{`"branch"`}{An orphan branch of the repository. The workflow
#'       creates the branch on its first run if it does not exist, and pushes
#'       the updated logs to it after every run. Requires the workflow token to
#'       have write access to the repository contents.}
#'     \item{`"artifact"`}{A workflow artifact. Each run downloads the latest
#'       artifact, updates the logs and re-uploads them. Works with a read-only
#'       token. Warning: charts are lost if no run happens within the retention
#'       period. The run summary states how many days the restored logs have
#'       left, and warns if fewer than seven remain.}
#'   }
#' @param save If `TRUE`, save the workflow to `path`. If `FALSE`, only print
#'   it.
#' @param path File the workflow is saved to, relative to the repository root.
#' @param overwrite If `TRUE`, replace the file at `path` if it already exists.
#'   If `FALSE`, stop with an error instead. Only used when `save = TRUE`.
#' @return Invisibly, the lines of the workflow file.
#'
#' @details The workflow installs `sequnit` from GitHub. The default settings
#'   trigger an action run on every push to `main`, daily at 03:17 UTC and each time
#'   a push to the repository is done. The current R release is being used by default,
#'   and the logs are stored under the name `cusum-data`. When a chart singals,
#'   the run fails. Comments in the file explain how to change these settings.
#'   Lines marked `EDIT` have no default and need filling in for the repository.
#'   These include the packages the tests need and the code under test.
#'
#'   To use the workflow, run the function with `save = TRUE` from the repository root,
#'   edit the file, then commit and push it. With `storage = "branch"`, the
#'   repository's Actions settings must allow workflows read and write
#'   permissions; with `storage = "artifact"`, read permissions are enough.
#'
#' @examples
#' workflow_gen()
#' workflow_gen(storage = "artifact")
#' path <- file.path(tempdir(), "cusum-monitor.yml")
#' workflow_gen(save = TRUE, path = path)
#' @export
workflow_gen <- function(storage = c("branch", "artifact"),
                         save = FALSE,
                         path = ".github/workflows/cusum-monitor.yml",
                         overwrite = FALSE) {
  storage <- match.arg(storage)
  if (!is.logical(save) || length(save) != 1L || is.na(save))
    stop("`save` must be `TRUE` or `FALSE`.", call. = FALSE)
  if (!is.character(path) || length(path) != 1L || !nzchar(path))
    stop("`path` must be a single non-empty string.", call. = FALSE)
  if (save && file.exists(path) && !isTRUE(overwrite))
    stop("'", path, "' already exists; use `overwrite = TRUE` to replace it.", call. = FALSE)
  yml <- strsplit(.workflow_templates[[storage]], "\n", fixed = TRUE)[[1]]
  writeLines(yml)
  if (save) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(yml, path)
    message("Workflow .yml file is stored in '", path, "'.")
  } else {
    message("Workflow .yml file not stored. Set `save = TRUE` to store it in '", path, "'.")
  }
  invisible(yml)
}
