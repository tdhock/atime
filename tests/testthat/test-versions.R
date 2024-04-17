library(data.table)
library(testthat)
tdir <- tempfile()
dir.create(tdir)
git2r::clone("https://github.com/tdhock/binsegRcpp", tdir)
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

test_that("atime_pkg produces tests_all_facet.png", {
  repo <- git2r::repository(tdir)
  git2r::checkout(repo, branch="another-branch")
  inst.atime <- file.path(tdir, "inst", "atime")
  result.list <- atime::atime_pkg(tdir)
  tests_all_facet.png <- file.path(inst.atime, "tests_all_facet.png")
  expect_true(file.exists(tests_all_facet.png))
})
