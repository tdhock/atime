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

dt_dir <- tempfile()
dir.create(dt_dir)
gert::git_clone("https://github.com/Rdatatable/data.table", dt_dir)
test_that("atime_pkg_test_info() works for data.table, run one test case", {
  dt_info <- atime::atime_pkg_test_info(dt_dir)
  tname <- "melt improved in #5054"
  tcall <- dt_info$test.call[[tname]]
  dt_result <- eval(tcall)
  expect_is(dt_result, "atime")
})

test_that("atime_versions_exprs works in fun", {
  dt_exprs <- function(e){
    expr <- substitute(e)
    atime::atime_versions_exprs(
      dt_dir,
      expr=expr,
      sha.vec=c(installed=""))
  }
  (computed <- dt_exprs({
    data.table::fwrite(input.df, tempfile(), showProgress = FALSE)
  }))
  (expected <- list(installed=quote({
    data.table::fwrite(input.df, tempfile(), showProgress = FALSE)
  })))
  expect_equal(computed, expected)
})

read_bisect_csv <- function(glob){
  (csv.wide <- data.table(
    csv=Sys.glob(glob)
  )[
  , fread(csv), by=csv
  ][, let(
    rank = rank(time),
    Status=factor(status)
  )][])
  min.start <- min(as.integer(csv.wide$start))
  for(cname in c("start", "end")){
    set(
      csv.wide,
      j=paste0(cname,"_minutes"),
      value=(as.integer(csv.wide[[cname]])-min.start)/60)
  }
  csv.wide
}

gert::git_branch_create(#and checkout
  "atime-dt-test-versions",
  "master",
  repo=dt_dir,
  force=TRUE)
dt_atime_dir <- file.path(dt_dir, ".ci", "atime")
file.copy(system.file("data.table_tests.R", package="atime"), file.path(dt_atime_dir, "tests.R"), overwrite = TRUE)
gert::git_add(".ci/atime/tests.R", repo=dt_dir)
gert::git_commit("commit from atime test-versions.R", repo=dt_dir)
tname <- "fread N=cols regression"
test_that("bisect.commits arg works for data.table N=cols regression", {
  dt_info <- atime::atime_pkg_test_info(dt_dir)
  names(dt_info$test.list[[tname]])
  l.item <- dt_info$test.list[[tname]]
  expect_equal(length(l.item$bisect.commits), 5)
  expect_equal(length(l.item$ci.commits), 2)
  expect_is(l.item$sha.vec, "character")
  tcall <- dt_info$test.call[[tname]]
  expect_null(tcall$bisect.commits)
  expect_null(tcall$ci.commits)
  expect_identical(tcall$sha.vec, l.item$sha.vec)
  dt_result <- eval(tcall)
  expect_is(dt_result, "atime")
  expect_in(c("Fast", "Slow", "HEAD=atime-dt-test-versions"), dt_result$measurements$expr.name)#base and others may or may not be present.
  expect_equal(sum(names(l.item$bisect.commits) %in% dt_result$measurements$expr.name), 0)
})

test_that("bisect() works for data.table N=cols regression", {
  atime::bisect(dt_dir, tname, "PR7375", "Slow")
  csv_glob <- file.path(dt_atime_dir, "bisect", "fread_N_cols_regression", "PR7375-Slow", "csv", "*")
  csv.wide <- read_bisect_csv(csv_glob)
  expect_equal(nrow(csv.wide), 7) #expected number of commits tested by git bisect.
  if(require(ggplot2)){
    ggplot()+
      geom_segment(aes(
        rank, start_minutes,
        xend=rank, yend=end_minutes),
        data=csv.wide)+
      scale_fill_manual(values=c("0"="black", "1"="white"))+
      geom_point(aes(
        rank, start_minutes, fill=Status),
        shape=21,
        data=csv.wide)
  }
  (csv.long <- melt(csv.wide, measure.vars=measure(variable, version, pattern="([^.]+)[.](.*)")))
  expect_identical(unique(sort(csv.long$version)), c("HEAD","PR7375","Slow"))
  if(require(ggplot2)){
    ggplot()+
      geom_vline(aes(
        xintercept=rank, linetype=Status),
        data=csv.wide)+
      geom_line(aes(
        rank, value, color=version),
        data=csv.long)+
      facet_grid(variable ~ ., scales="free")
  }
})

