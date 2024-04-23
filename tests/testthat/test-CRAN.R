library(data.table)
library(testthat)

test_that("warning for only one N", {
  expect_warning({
    seconds.limit <- 0.001
    atime.list <- atime::atime(
      wait=Sys.sleep(seconds.limit),
      TRE=regexpr(pattern, subject, perl=FALSE),
      setup={
        subject <- paste(rep("a", N), collapse="")
        pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
      },
      seconds.limit=seconds.limit)
  },
  "please increase max N or seconds.limit, because only one N was evaluated for expr.name: wait",
  fixed=TRUE)
})

test_that("error for N wrong type", {
  expect_error({
    atime.list <- atime::atime(
      PCRE=regexpr(pattern, subject, perl=TRUE),
      TRE=regexpr(pattern, subject, perl=FALSE),
      setup={
        subject <- paste(rep("a", N), collapse="")
        pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
      },
      N="foo")
  }, "N should be a numeric vector")
})

test_that("error for length(N)==1", {
  expect_error({
    atime.list <- atime::atime(
      PCRE=regexpr(pattern, subject, perl=TRUE),
      TRE=regexpr(pattern, subject, perl=FALSE),
      setup={
        subject <- paste(rep("a", N), collapse="")
        pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
      },
      N=100)
  }, "length(N) should be at least 2", fixed=TRUE)
})

test_that("more units defined in 1 row result", {
  expr.list <- atime::atime_grid(
    list(perl=c(TRUE,FALSE)),
    regexpr=data.table(
      num=1,
      int=2L,
      match.len={
        L <- regexpr(pattern, subject, perl=perl)
        at <- attr(L,"match.length")
        if(is.numeric(at))at else NA_integer_
      }
    )
  )
  atime.list <- atime::atime(
    expr.list=expr.list,
    setup={
      subject <- paste(rep("a", N), collapse="")
      pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
    },
    result=TRUE,
    N=1:30)
  ref.list <- atime::references_best(atime.list)
  disp.units <- sort(unique(ref.list$measurements$unit))
  expected.units <- c("int","match.len","num","seconds")
  ## TDH 23 Apr 2024: kilobytes is not available on some systems.
  expect_true(all(expected.units %in% disp.units))
  expect_error({
    predict(ref.list, match.len=40)
  }, "match.len=40 is outside range of data, please change to a value that intersects at least one of the empirical curves")
  expect_error({
    predict(ref.list, match.len=0)
  }, "match.len=0 is outside range of data, please change to a value that intersects at least one of the empirical curves")
  match.len <- 2
  my.pred.length <- predict(ref.list, match.len=match.len)
  if(interactive())plot(my.pred.length)
  expect_true(all(my.pred.length$prediction[["unit"]]=="match.len"))
  expect_true(all(my.pred.length$prediction[["unit.value"]]==match.len))
  expect_error({
    predict(ref.list, foobar=0)
  }, "foobar is not a valid unit; argument names of predict must be one of these valid units:")
  my.pred.both <- predict(
    ref.list, match.len=match.len, seconds=ref.list$seconds.limit)
  if(interactive())plot(my.pred.both)
  unit.tab <- table(my.pred.both$prediction$unit)
  expect_identical(names(unit.tab), c("match.len","seconds"))
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

test_that("atime_grid symbol.params arg OK", {
  grid.result <- atime::atime_grid(list(
    PERL=TRUE,
    FUN="strcapture"
  ),
  foo=FUN(regex, text, proto, perl = PERL),
  symbol.params="FUN")
  expect_identical(grid.result, list("foo PERL=TRUE,FUN=strcapture"=quote(strcapture(regex, text, proto, perl=TRUE))))
})

test_that("atime_grid error for list of funs", {
  expect_error({
    atime::atime_grid(list(FUN=list(gsub)), strcapture=FUN(regex, text, proto, perl = TRUE))
  }, "param.list elements must be atomic, but some are not: FUN")
})

test_that("atime_grid error for un-named expr in ...", {
  expect_error({
    atime::atime_grid(list(FUN=1:2), FUN(regex, text, proto, perl = TRUE))
  }, "each expression in ... must be named")
})

test_that("atime_grid error for args named sorted, unique", {
  expect_error({
    atime::atime_grid(list(sorted=1:2), foo=FUN(regex, text, proto, perl = TRUE))
  }, "param.list must not have elements named: sorted")
})

test_that("null is faster than wait", {
  suppressWarnings({
    alist <- atime::atime(
      N=1:2,
      setup={},
      wait=Sys.sleep(0.01),
      null=NULL,
      seconds.limit=0.001)
  })
  expect_equal(nrow(alist$measurements[expr.name=="null"]), 2)
})

test_that("no error for results=FALSE", {
  suppressWarnings({
    alist <- atime::atime(
      N=1:2,
      setup={},
      wait=Sys.sleep(0.01),
      null=NULL,
      results=FALSE,
      seconds.limit=0.001)
  })
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
    predict(my.best, kilobytes=1000, kilobytes=100, foo=5, bar=5, foo=3, foo=1)
  }, "argument names should be unique, problem(count): foo(3), kilobytes(2)", fixed=TRUE)
})

test_that("atime_versions_exprs error when called with setup", {
  expect_error({
    atime::atime_versions_exprs(
      pkg.path="~/R/data.table",
      N=10^seq(2,10),
      setup={ 
        set.seed(1L)
        dt <- data.table(
          id = seq_len(N),
          val = rnorm(N))
      },
      expr=data.table:::`[.data.table`(dt[, .(vs = (sum(val))), by = .(id)]),
      "Before"="be2f72e6f5c90622fe72e1c315ca05769a9dc854",
      "Regression"="e793f53466d99f86e70fc2611b708ae8c601a451", 
      "Fixed"="58409197426ced4714af842650b0cc3b9e2cb842") 
  }, "each ... argument value and sha.vec element must be a string (package version, length=1, not NA), problems: N, setup", fixed=TRUE)
})

test_that("atime_versions_exprs error when sha.vec element not string", {
  expect_error({
    atime::atime_versions_exprs(
      pkg.path="~/R/data.table",
      sha.vec=list(N=10^seq(2,10)),
      expr=data.table:::`[.data.table`(dt[, .(vs = (sum(val))), by = .(id)]),
      "Before"="be2f72e6f5c90622fe72e1c315ca05769a9dc854",
      "Regression"="e793f53466d99f86e70fc2611b708ae8c601a451", 
      "Fixed"="58409197426ced4714af842650b0cc3b9e2cb842") 
  }, "each ... argument value and sha.vec element must be a string (package version, length=1, not NA), problems: N", fixed=TRUE)
})

test_that("atime_versions_exprs error when expr does not contain pkg:", {
  expect_error({
    atime::atime_versions_exprs(
      pkg.path="../..",#getwd is testthat
      expr=dt[, .(vs = (sum(val))), by = .(id)],
      "Before"="be2f72e6f5c90622fe72e1c315ca05769a9dc854",
      "Regression"="e793f53466d99f86e70fc2611b708ae8c601a451", 
      "Fixed"="58409197426ced4714af842650b0cc3b9e2cb842") 
  }, "expr should contain at least one instance of atime: to replace with atime.be2f72e6f5c90622fe72e1c315ca05769a9dc854:", fixed=TRUE)
})

test_that("atime_versions_exprs error when sha.vec element char vector", {
  expect_error({
    atime::atime_versions_exprs(
      pkg.path="~/R/data.table",
      sha.vec=list(foo=c("bar","baz")),
      expr=data.table:::`[.data.table`(dt[, .(vs = (sum(val))), by = .(id)]),
      "Before"="be2f72e6f5c90622fe72e1c315ca05769a9dc854",
      "Regression"="e793f53466d99f86e70fc2611b708ae8c601a451", 
      "Fixed"="58409197426ced4714af842650b0cc3b9e2cb842") 
  }, "each ... argument value and sha.vec element must be a string (package version, length=1, not NA), problems: foo", fixed=TRUE)
})

test_that("atime_versions error when ... value NA", {
  expect_error({
    atime::atime_versions(
      pkg.path="~/R/data.table",
      expr=data.table:::`[.data.table`(dt[, .(vs = (sum(val))), by = .(id)]),
      "Before"=NA_character_,
      "Regression"="e793f53466d99f86e70fc2611b708ae8c601a451", 
      "Fixed"="58409197426ced4714af842650b0cc3b9e2cb842") 
  }, "each ... argument value and sha.vec element must be a string (package version, length=1, not NA), problems: Before", fixed=TRUE)
})

test_that("atime_versions error when sha.vec element not named", {
  expect_error({
    atime::atime_versions(
      pkg.path="~/R/data.table",
      expr=data.table:::`[.data.table`(dt[, .(vs = (sum(val))), by = .(id)]),
      sha.vec=c(
        "e793f53466d99f86e70fc2611b708ae8c601a451", 
        "Fixed"="58409197426ced4714af842650b0cc3b9e2cb842"))
  }, "each ... argument and sha.vec element must be named", fixed=TRUE)
})

test_that("atime_versions error when no versions specified", {
  expect_error({
    atime::atime_versions(
      pkg.path="~/R/data.table",
      expr=data.table:::`[.data.table`(dt[, .(vs = (sum(val))), by = .(id)]))
  }, "need to specify at least one git SHA, in either sha.vec, or ...", fixed=TRUE)
})

if(requireNamespace("nc")){
  test_that("only one value in grid is OK", {
    expr.list <- atime::atime_grid(
      list(ENGINE="PCRE"),
      nc=nc::capture_first_vec(subject, pattern, engine=ENGINE))
    expect_identical(names(expr.list), "nc ENGINE=PCRE")
  })
  test_that("error for expr.list not list", {
    expr.list <- atime::atime_grid(
      list(ENGINE=c(
        if(requireNamespace("re2"))"RE2",
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
}

test_that("references for non-NA unit, with NA unit",{
  atime.list <- atime::atime(
    missing=data.frame(my_unit=NA),
    constant=data.frame(my_unit=1),
    linear=data.frame(my_unit=N),
    quadratic=data.frame(my_unit=N^2),
    seconds.limit=0.001,
    result=TRUE)
  (atab <- table(atime.list$meas$expr.name))
  ref.list <- atime::references_best(atime.list)
  if(interactive())plot(ref.list)
  (rtab <- table(ref.list$references[unit=="my_unit", expr.name]))
  expect_identical(sort(names(rtab)), c("linear","quadratic"))
})

test_that("references for non-NA unit, with NA unit",{
  expect_error({
    atime::atime(
      missing=data.frame(my_unit=NA),
      constant=data.frame(foo=1),
      linear=data.frame(my_unit=N),
      quadratic=data.frame(my_unit=N^2),
      seconds.limit=0.001,
      result=TRUE)
  }, "results are all 1 row data frames, but some have different names (missing, constant); please fix by making row names of results identical", fixed=TRUE)
})

test_that("error for new unit name conflicting with existing", {
  expect_error({
    atime::atime(
      missing=data.frame(median=NA, kilobytes=1, ok=2),
      constant=data.frame(median=1, kilobytes=1, ok=2),
      linear=data.frame(median=N, kilobytes=1, ok=2),
      quadratic=data.frame(median=N^2, kilobytes=1, ok=2),
      seconds.limit=0.001,
      result=TRUE)
  }, "result is 1 row data frame with column(s) named median, kilobytes (reserved for internal use); please fix by changing the column name(s) in your results", fixed=TRUE)
})
