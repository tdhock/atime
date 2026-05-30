library(data.table)
library(testthat)
tdir <- tempfile()
dir.create(tdir)
gert::git_clone("https://github.com/tdhock/binsegRcpp", tdir)
test_that("error if no versions specified", {
  expect_error({
    atime.list <- atime::atime_versions(
      pkg.path=tdir,
      N=2^seq(2, 20),
      setup={
        max.segs <- as.integer(N/2)
        data.vec <- 1:N
      },
      expr=binsegRcpp::binseg_normal(data.vec, max.segs))
  },
  "need to specify at least one git SHA, in either sha.vec, or ...",
  fixed=TRUE)
})

test_that("atime_versions_exprs error when expr does not contain pkg:", {
  expect_error({
    atime::atime_versions_exprs(
      pkg.path=tdir,
      expr=dt[, .(vs = (sum(val))), by = .(id)],
      "Before"="be2f72e6f5c90622fe72e1c315ca05769a9dc854",
      "Regression"="e793f53466d99f86e70fc2611b708ae8c601a451", 
      "Fixed"="58409197426ced4714af842650b0cc3b9e2cb842") 
  }, "expr=dt[, .(vs = (sum(val))), by = .(id)] should contain at least one instance of binsegRcpp:: to replace with binsegRcpp.be2f72e6f5c90622fe72e1c315ca05769a9dc854::", fixed=TRUE)
})

if(requireNamespace("ggplot2"))test_that("atime_pkg produces tests_all_facet.png and tests_preview_facet.png on atime-test-funs", {
  ## https://github.com/tdhock/binsegRcpp/tree/atime-test-funs
  atime.dir <- file.path(tdir, ".ci", "atime")
  unlink(file.path(atime.dir, "*"))
  gert::git_branch_checkout("test-setup-HEAD", force=TRUE, repo=tdir)
  options(repos="http://cloud.r-project.org")#required to check CRAN version.
  plist <- atime::atime_pkg(tdir, ".ci", verbose=TRUE)
  tests.RData <- file.path(atime.dir, "tests.RData")
  (objs <- load(tests.RData))
  ##expect_match(issues.dt$issue, "slower")
  expected.names <- c(
    "binseg(1:N,maxSegs=N/2) DIST=l1",
    "binseg(1:N,maxSegs=N/2) DIST=meanvar_norm", 
    "binseg(1:N,maxSegs=N/2) DIST=poisson",
    "binseg_normal(1:N,maxSegs=N/2)"
  )
  expect_identical(sort(unique(bench.dt$Test)), sort(expected.names))
  expect_identical(sort(limit.dt$Test), sort(expected.names))
  expect_is(limit.dt$P.value, "factor")
  expect_is(limit.dt$pred.Nx, "factor")
  expect_identical(names(pkg.results), expected.names)
  expect_is(bench.dt[["Test"]], "character")
  install.seconds <- sapply(pkg.results, "[[", "install.seconds")
  expect_is(install.seconds, "numeric")
  expect_identical(names(install.seconds), expected.names)
  bench.seconds <- sapply(pkg.results, "[[", "bench.seconds")
  expect_is(bench.seconds, "numeric")
  expect_identical(names(bench.seconds), expected.names)
  ## also test global PNG.
  tests_all_facet.png <- file.path(atime.dir, "tests_all_facet.png")
  expect_true(file.exists(tests_all_facet.png))
  tests_preview_facet.png <- file.path(atime.dir, "tests_preview_facet.png")
  expect_true(file.exists(tests_preview_facet.png))
  HEAD_issues.md <- file.path(atime.dir, "HEAD_issues.md")
  expect_true(file.exists(HEAD_issues.md))
})

if(requireNamespace("ggplot2"))test_that("atime_pkg produces tests_all_facet.png and tests_preview_facet.png on another-branch", {
  ## https://github.com/tdhock/binsegRcpp/tree/another-branch
  inst.atime <- file.path(tdir, "inst", "atime")
  unlink(file.path(inst.atime, "*"))
  gert::git_branch_checkout("another-branch", force=TRUE, repo=tdir)
  options(repos="http://cloud.r-project.org")#required to check CRAN version.
  plist <- atime::atime_pkg(tdir)
  tests_all_facet.png <- file.path(inst.atime, "tests_all_facet.png")
  expect_true(file.exists(tests_all_facet.png))
  ##N.tests.preview=2 < N.tests=4 so should make one more PNG with those two.
  tests_preview_facet.png <- file.path(inst.atime, "tests_preview_facet.png")
  expect_true(file.exists(tests_preview_facet.png))
  install_seconds.txt <- file.path(inst.atime, "install_seconds.txt")
  install.seconds <- scan(install_seconds.txt, n=1, quiet=TRUE)
  expect_is(install.seconds, "numeric")
})

if(requireNamespace("ggplot2"))test_that("atime_pkg produces tests_all_facet.png and tests_preview_facet.png on master", {
  inst.atime <- file.path(tdir, ".ci", "atime")
  unlink(file.path(inst.atime, "*"))
  gert::git_branch_checkout("master", force=TRUE, repo=tdir)
  options(repos="http://cloud.r-project.org")#required to check CRAN version.
  plist <- atime::atime_pkg(tdir)
  tests_all_facet.png <- file.path(inst.atime, "tests_all_facet.png")
  expect_true(file.exists(tests_all_facet.png))
  tests_preview_facet.png <- file.path(inst.atime, "tests_preview_facet.png")
  expect_true(file.exists(tests_preview_facet.png))
  install_seconds.txt <- file.path(inst.atime, "install_seconds.txt")
  install.seconds <- scan(install_seconds.txt, n=1, quiet=TRUE)
  expect_is(install.seconds, "numeric")
})

if(requireNamespace("ggplot2"))test_that("atime_pkg produces tests_all_facet.png and tests_preview_facet.png on priority_queue", {
  ## https://github.com/tdhock/binsegRcpp/pull/23
  inst.atime <- file.path(tdir, ".ci", "atime")
  unlink(file.path(inst.atime, "*"))
  gert::git_branch_checkout("priority_queue", force=TRUE, repo=tdir)
  options(repos="http://cloud.r-project.org")#required to check CRAN version.
  plist <- atime::atime_pkg(tdir, verbose=TRUE)
  tests_all_facet.png <- file.path(inst.atime, "tests_all_facet.png")
  expect_true(file.exists(tests_all_facet.png))
  tests_preview_facet.png <- file.path(inst.atime, "tests_preview_facet.png")
  expect_true(file.exists(tests_preview_facet.png))
  install_seconds.txt <- file.path(inst.atime, "install_seconds.txt")
  install.seconds <- scan(install_seconds.txt, n=1, quiet=TRUE)
  expect_is(install.seconds, "numeric")
})

test_that("pkg.edit.fun is a function", {
  example_tests.R <- system.file("example_tests.R", package="atime")
  tests.dir <- file.path(tempfile(), ".ci", "atime")
  dir.create(tests.dir, showWarnings = FALSE, recursive = TRUE)
  tests.R <- file.path(tests.dir, "tests.R")
  file.copy(example_tests.R, tests.R)
  ci.dir <- dirname(tests.dir)
  pkg.dir <- dirname(ci.dir)
  cat("Package: atime\nVersion: 1.0\n", file=file.path(pkg.dir, "DESCRIPTION"))
  gert::git_init(pkg.dir)
  gert::git_add("DESCRIPTION", repo=pkg.dir)
  gert::git_commit("test commit", repo=pkg.dir)
  options(repos="http://cloud.r-project.org")#required to check CRAN version.
  test.env <- atime::atime_pkg_test_info(pkg.dir)
  test_N_expr <- test.env$test.list$test_N_expr
  expect_identical(test_N_expr$pkg.edit.fun, test.env$edit.data.table)
  expect_identical(test_N_expr$N, c(2,20))
  expect_identical(test_N_expr$expr, quote(atime:::.packageName))
  test_expr <- test.env$test.list$test_expr
  expect_identical(test_expr$pkg.edit.fun, test.env$edit.data.table)
  expect_identical(test_expr$N, c(9,90))
  expect_identical(test_expr$expr, quote(atime:::.packageName))
  e.res <- eval(test.env$test.call[["global_var_in_setup"]])
  expect_is(e.res, "atime")
  p.res <- atime::atime_pkg(pkg.dir)
  expect_is(p.res, "list")
})

gdir <- tempfile()
dir.create(gdir)
gert::git_clone("https://github.com/tdhock/grates", gdir)

test_that("informative error when pkg.path is not a package", {
  expect_error({
    atime::atime_versions(
      gdir,
      current = "1aae646888dcedb128c9076d9bd53fcb4075dcda",
      old     = "51056b9c4363797023da4572bde07e345ce57d9c",
      setup   = date_vec <- rep(Sys.Date(), N),
      expr    = grates::as_yearmonth(date_vec))
  }, sprintf("pkg.path=%s should be path to an R package, but %s/DESCRIPTION does not exist", gdir, gdir), fixed=TRUE)
})

test_that("atime_versions works with grates pkg in sub-dir of git repo", {
  if(!requireNamespace("fastymd"))install.packages("fastymd")
  glist <- atime::atime_versions(
    file.path(gdir,"pkg"),
    current = "1aae646888dcedb128c9076d9bd53fcb4075dcda",
    old     = "51056b9c4363797023da4572bde07e345ce57d9c",
    setup   = date_vec <- rep(Sys.Date(), N),
    expr    = grates::as_yearmonth(date_vec))
  expect_is(glist, "atime")
})

test_that("atime_pkg_test_info() works for data.table, run one test case", {
  dt_dir <- tempfile()
  dir.create(dt_dir)
  gert::git_clone("https://github.com/Rdatatable/data.table", dt_dir)
  dt_info <- atime::atime_pkg_test_info(dt_dir)
  tname <- "melt improved in #5054"
  tcall <- dt_info$test.call[[tname]]
  dt_result <- eval(tcall)
  expect_is(dt_result, "atime")
})

if(interactive())test_that("atime_pkg() works for poncatime", {
  poncatime_dir <- tempfile()
  dir.create(poncatime_dir)
  gert::git_clone("https://github.com/poncateam/poncatime", poncatime_dir)
  gert::git_branch_create("atime-version-test", "afe6f986836f3f37e743bda93a8d0746587aad1a", repo=poncatime_dir)
  poncatime_info <- atime::atime_pkg(poncatime_dir)
  expect_equal(nrow(poncatime_info$all), 2)#two test cases run.
})
