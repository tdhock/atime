test_that("setup runs before each timing iteration", {
  count_list <- list(setup = 0, expr = 0)
  result <- atime::atime(
    N = 2^(1:5),
    setup = count_list$setup <<- count_list$setup + 1,
    myexpr = count_list$expr <<- count_list$expr + 1
  )
  expect_equal(count_list$setup, count_list$expr,
    info = "Issue #95: setup should run before each expr execution")
})
