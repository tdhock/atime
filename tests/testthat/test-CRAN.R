library(data.table)
library(testthat)
test_that("more.units error if not present", {
  atime.list <- atime::atime(
    PCRE=regexpr(pattern, subject, perl=TRUE),
    TRE=regexpr(pattern, subject, perl=FALSE),
    setup={
      subject <- paste(rep("a", N), collapse="")
      pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
    },
    result=TRUE,
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

test_that("result returned when some are NULL and others not", {
  atime.list <- atime::atime(
    N=10^seq(-3, 0),
    setup={},
    seconds.limit=0.01,
    slow={
      Sys.sleep(N)
      list(msg="slow")
    },
    result = TRUE,
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
      read_csv=read_csv(f.csv, lazy=LAZY))
  }, "each param should be present in each expr, problems: LAZY not in fread.prob, THREADS not in read_csv")
})

test_that("atime_grid two params, two exprs", {
  expr.list <- atime::atime_grid(
    list(THREADS=1:2, LAZY=c(TRUE,FALSE)),
    fread=fread(f.csv, threads=THREADS, lazy=LAZY),
    read_csv=read_csv(f.csv, num_threads=THREADS, lazy=LAZY))
  expect_equal(length(expr.list), 8)
})

test_that("atime_grid error for THREADS not used", {
  expect_error({
    atime::atime_grid(
      list(THREADS=1:3),
      "[.data.table"={
        loss.dt[, .(
          loss_length=.N,
          loss_mean=mean(loss),
          loss_sd=sd(loss)
        ), by=.(set, epoch)]
      })
  }, "each param should be present in each expr, problems: THREADS not in [.data.table", fixed=TRUE)
})

test_that("atime_grid ok when THREADS used", {
  expr.list <- atime::atime_grid(
    list(THREADS=1:3),
    "[.data.table"={
      data.table::setDTthreads(THREADS)
      loss.dt[, .(
        loss_length=.N,
        loss_mean=mean(loss),
        loss_sd=sd(loss)
      ), by=.(set, epoch)]
    })
  expect_equal(length(expr.list), 3)
})

test_that("error for expr.list not list", {
  expr.list <- atime::atime_grid(
    list(ENGINE=c(
      ##if(requireNamespace("re2"))"RE2",#uncomment when new nc on CRAN.
      "PCRE",
      if(requireNamespace("stringi"))"ICU")),
    nc=nc::capture_first_vec(subject, pattern, engine=ENGINE))
  dolist <- function(elist){
    atime::atime(
      N=1:25,
      setup={
        rep.collapse <- function(chr)paste(rep(chr, N), collapse="")
        subject <- rep.collapse("a")
        pattern <- list(maybe=rep.collapse("a?"), rep.collapse("a"))
      },
      expr.list=elist)
  }
  atime.list <- dolist(expr.list)
  expect_is(atime.list, "atime")
  expect_error({
    dolist(structure(2, class=c("foo","bar")))
  }, "expr.list should be a list of expressions to run for various N, but has classes foo, bar")
})

test_that("only one value in grid is OK", {
  expr.list <- atime::atime_grid(
    list(ENGINE="PCRE"),
    nc=nc::capture_first_vec(subject, pattern, engine=ENGINE))
  expect_identical(names(expr.list), "nc ENGINE=PCRE")
})

test_that("null is faster than wait", {
  alist <- atime::atime(
    N=1:2,
    setup={},
    wait=Sys.sleep(0.01),
    null=NULL,
    seconds.limit=0.001)
  expect_equal(nrow(alist$measurements[expr.name=="null"]), 2)
})

test_that("no error for results=FALSE", {
  alist <- atime::atime(
    N=1:2,
    setup={},
    wait=Sys.sleep(0.01),
    null=NULL,
    results=FALSE,
    seconds.limit=0.001)
  expect_is(alist, "atime")
  expect_equal(sort(alist$measurements$expr.name), c("null","null","results","results","wait"))
})

my.atime <- atime::atime(
  vector=x,
  "vector+1"=x+1,
  matrix=matrix(x, N, N),
  setup={
    x <- rep(1, N)
  },
  N=unique(as.integer(10^seq(0,4,l=100))))
if(interactive())plot(my.atime)
my.best <- atime::references_best(my.atime)
if(interactive())plot(my.best)

test_that("predict gives seconds.limit by default", {
  my.pred.default <- predict(my.best)
  if(interactive())plot(my.pred.default)
  expect_true(all(my.pred.default$prediction[["unit"]]=="seconds"))
  expect_true(all(
    my.pred.default$prediction[["unit.value"]]==my.pred.default$seconds.limit))
})

test_that("predict gives only kilobytes", {
  kb <- 10
  my.pred.kb <- predict(my.best, kilobytes=kb)
  if(interactive())plot(my.pred.kb)
  expect_true(all(my.pred.kb$prediction[["unit"]]=="kilobytes"))
  expect_true(all(my.pred.kb$prediction[["unit.value"]]==kb))
})

test_that("predict gives both seconds and kilobytes", {
  my.pred.both <- predict(
    my.best, kilobytes=10, seconds=my.best$seconds.limit)
  if(interactive())plot(my.pred.both)
  unit.tab <- table(my.pred.both$prediction$unit)
  expect_identical(names(unit.tab), c("kilobytes","seconds"))
})

test_that("errors for predict method", {
  expect_error({
    predict(my.best, 5)
  }, "... has an un-named argument, but must have a unit as the name of each argument", fixed=TRUE)
  expect_error({
    predict(my.best, kilobytes=100, 5)
  }, "... has an un-named argument, but must have a unit as the name of each argument", fixed=TRUE)
  expect_error({
    predict(my.best, kilobytes="a")
  }, "... has a non-numeric argument (kilobytes), but each argument must be numeric (unit value at which to interpolate/predict N)", fixed=TRUE)
  expect_error({
    predict(my.best, kilobytes=NA_real_)
  }, "... has a non-finite argument (kilobytes) but each argument must be finite (unit value at which to interpolate/predict N)", fixed=TRUE)
  expect_error({
    predict(my.best, kilobytes=1:2)
  }, "... has an argument with length != 1 (kilobytes), but each argument must be scalar (unit value at which to interpolate/predict N)", fixed=TRUE)
  expect_error({
    predict(my.best, kilobytes=1e9)
  }, "kilobytes=1e+09 is too large, please decrease to a value that intersects at least one of the empirical curves", fixed=TRUE)
  expect_error({
    predict(my.best, kilobytes=1000, kilobytes=100, foo=5, bar=5, foo=3, foo=1)
  }, "argument names should be unique, problem(count): foo(3), kilobytes(2)", fixed=TRUE)
})


