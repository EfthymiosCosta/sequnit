# sequnit

Sequential monitoring of randomised unit tests. Each randomised test supplies a
p-value, and a non-restarting CUSUM chart accumulates evidence of a code defect
across successive commits while keeping the rate of false alarms under control.

## Installation

```r
# install.packages("remotes")
remotes::install_github("EfthymiosCosta/sequnit")
```

## Usage

In a test file, pass a function that runs the randomised test once and returns
its p-value. Here `gof_pvalue()` stands for a goodness-of-fit test of a sample:

```r
test_that("rbinom matches its distribution", {
  expect_pval(function() gof_pvalue(rbinom(1000, 10, 0.3)), name = "rbinom_gof")
})
```

Each run of the tests advances the chart by one commit and appends a line to
its log, `rbinom_gof.jsonl`. The test fails when the chart signals.

To run the tests on GitHub Actions and keep the logs between runs:

```r
workflow_gen(save = TRUE)
```

Then edit the lines marked `EDIT` in `.github/workflows/cusum-monitor.yml`, and
commit and push it.

To look at a chart:

```r
plot_pval_log("rbinom_gof.jsonl")
plot_pval_steps("rbinom_gof.jsonl")
```
