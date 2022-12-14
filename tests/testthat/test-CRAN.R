library(testthat)
test_that("more.units error if not present", {
  atime.list <- atime::atime(
    PCRE=regexpr(pattern, subject, perl=TRUE),
    TRE=regexpr(pattern, subject, perl=FALSE),
    setup={
      subject <- paste(rep("a", N), collapse="")
      pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
    },
    N=1:30)
  atime.list$measurements[, `:=`(
    length.num=sapply(result, function(L){
      at <- attr(L,"match.length")
      if(is.numeric(at))at else NA_real_
    }))]
  expect_error({
    atime::references_best(atime.list, more.units="foo")
  }, "some units were not found (fix by creating columns in measurements): foo", fixed=TRUE)
  expect_error({
    atime::references_best(atime.list, more.units=c("foo", "bar", "length.num"))
  }, "some units were not found (fix by creating columns in measurements): foo, bar", fixed=TRUE)
  expect_error({
    atime::references_best(atime.list, more.units="result")
  }, "each unit must be numeric, but result is not")
  refs.more <- atime::references_best(atime.list, more.units="length.num")
  expect_true("length.num" %in% refs.more[["measurements"]][["unit"]])
  refs.units <- atime::references_best(atime.list, unit.col.vec=c(seconds="median", "length.num"))
  u.tab <- table(refs.units[["measurements"]][["unit"]])
  expect_identical(names(u.tab), c("length.num", "seconds"))
  expect_equal(sum(is.na(refs.units$measurements$empirical)), 0)
})

test_that("can extract numeric units from data table in list", {
  atime.list <- atime::atime(
    N=1:2,
    setup={},
    makeList=list(loss=data.table(value=5)))
  atime.list$measurements[, intervals := sapply(result, function(L)L$loss$value)]
  best.list <- atime::references_best(atime.list, more.units="intervals")
  emp <- best.list$measurements$empirical
  expect_equal(sum(is.na(emp)), 0)
})

test_that("results returned when some results are NULL and others not", {
  atime.list <- atime::atime(
    N=10^seq(-3, 0),
    setup={},
    seconds.limit=0.01,
    slow={
      Sys.sleep(N)
      list(msg="slow")
    },
    fast=NULL)
  expect_is(atime.list$mea$result, "list")
})

test_that("sensible error when duplicate names", {
  expect_error({
    atime::atime(
      N=10^seq(-3, 0),
      setup={},
      fast=1,
      expr.list = list(fast=2))
  }, "each expression must have a unique name, problems: fast")
})

test_that("atime_grid error if param not in expr", {
  expect_error({
    atime::atime_grid(
      list(THREADS=1:2, LAZY=c(TRUE,FALSE)),
      fread.prob=fread(f.csv, threads=THREADS),
      fread=fread(f.csv, threads=THREADS, lazy=LAZY),
      readr.prob=readr::read_csv(f.csv, lazy=LAZY))
  }, "each param should be present in each expr, problems: LAZY not in fread.prob, THREADS not in readr.prob")
})

test_that("atime_grid two params, two exprs", {
  expr.list <- atime::atime_grid(
    list(THREADS=1:2, LAZY=c(TRUE,FALSE)),
    fread=fread(f.csv, threads=THREADS, lazy=LAZY),
    readr.prob=readr::read_csv(f.csv, num_threads=THREADS, lazy=LAZY))
  expect_equal(length(expr.list), 8)
})
