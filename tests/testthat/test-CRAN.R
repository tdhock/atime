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
    length.list=sapply(result, attr, "match.length"),
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
    atime::references_best(atime.list, more.units="length.list")
  }, "each unit must be numeric, but length.list is not")
  refs.more <- atime::references_best(atime.list, more.units="length.num")
  expect_true("length.num" %in% refs.more[["measurements"]][["unit"]])
  refs.units <- atime::references_best(atime.list, unit.col.vec=c(seconds="median", "length.num"))
  u.tab <- table(refs.units[["measurements"]][["unit"]])
  expect_identical(names(u.tab), c("length.num", "seconds"))
})
