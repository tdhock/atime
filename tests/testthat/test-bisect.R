library(data.table)
library(testthat)

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
  #expect_equal(nrow(csv.wide), 7) #expected number of commits tested by git bisect.
  expect_gt(nrow(csv.wide), 0)
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

poncatime_dir <- tempfile()
dir.create(poncatime_dir)
gert::git_clone("https://github.com/poncateam/poncatime", poncatime_dir)
system(paste("cd", poncatime_dir, "&& git submodule update --init --recursive && cd src/external/ponca && git checkout master"))
if(!requireNamespace("RcppEigen")){
  #to avoid ERROR on CI: dependency ‘RcppEigen’ is not available for package ‘Poncatime’
  install.packages("RcppEigen")
}
test_that("atime_pkg() works for poncatime", {
  tinfo <- atime::atime_pkg_test_info(poncatime_dir)
  expect_identical(tinfo$HEAD.name, "HEAD=master")
  poncatime_info <- atime::atime_pkg(poncatime_dir)
  expect_gt(nrow(poncatime_info$all), 0)#at least one test case run.
})

ponca_atime_dir <- file.path(poncatime_dir, ".ci", "atime")
file.copy(system.file("poncatime_tests.R", package="atime"), file.path(ponca_atime_dir, "tests.R"), overwrite = TRUE)
## no commit to new branch needed here, because tests.R is in poncatime repo, and git bisect is run in ponca submodule repo.
test_that("bisect() and atime_test_list(ci.commits=) work for poncatime", {
  atime::bisect(poncatime_dir, "asoCurvatureEstimation", "v1.0", "v1.4")
  gert::git_status(repo=poncatime_dir)
  csv_glob <- file.path(ponca_atime_dir,"bisect","asoCurvatureEstimation","v1.0-v1.4","csv","*")
  csv.wide <- read_bisect_csv(csv_glob)
  ##expect_equal(nrow(csv.wide), 9) #expected number of commits tested by git bisect.
  expect_gt(nrow(csv.wide), 0)
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
  expect_identical(unique(sort(csv.long$version)), c("HEAD","v1.0","v1.4"))
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
