library(data.table)
library(testthat)
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
  expect_equal(nrow(csv.wide), 9) #expected number of commits tested by git bisect.
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
