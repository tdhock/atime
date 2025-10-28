test_that("setup runs before each timing iteration for reference modifications", {
  skip_if_not_installed("data.table")
  library(data.table)
  N <- 1000
  iterations <- 5
  cat("\n=== Issue #95: bench::mark limitation ===\n")
  cat("Setup runs ONCE (current behavior):\n")
  DT <- data.table(x = sample(N), y = rnorm(N))
  times_once <- numeric(iterations)
  for (i in 1:iterations) {
    start <- Sys.time()
    setkey(DT, x)
    times_once[i] <- as.numeric(Sys.time() - start, units = "secs")
  }
  cat(sprintf("  Iteration 1: %.2f ms (actual sort)\n", times_once[1] * 1000))
  cat(sprintf("  Iterations 2-5 median: %.2f ms (just verification)\n", median(times_once[-1]) * 1000))
  cat("\nSetup runs EACH time (desired behavior):\n")
  times_each <- numeric(iterations)
  for (i in 1:iterations) {
    DT <- data.table(x = sample(N), y = rnorm(N))
    start <- Sys.time()
    setkey(DT, x)
    times_each[i] <- as.numeric(Sys.time() - start, units = "secs")
  }
  cat(sprintf("  All iterations median: %.2f ms (consistent sorting)\n", median(times_each) * 1000))
  ratio <- times_once[1] / median(times_once[-1])
  cat(sprintf("Discrepancy: First iteration is %.1fx slower than rest\n", ratio))
  expect_gt(ratio, 3, label = sprintf("Issue #95 confirmed: bench::mark does not re-run setup (ratio=%.1f)", ratio))
  cv <- sd(times_each) / mean(times_each)
  expect_lt(cv, 0.5, label = sprintf("When setup runs each time, timings are consistent (CV=%.3f)", cv))
})
