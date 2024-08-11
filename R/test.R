test_file_to_env <- function(tests.R){
  test.env <- new.env()
  tests.parsed <- parse(tests.R)
  eval(tests.parsed, test.env)
  test.env
}

atime_test <- function(...){
  as.list(match.call()[-1])
}

atime_test_list <- function(..., N, setup, expr, times, seconds.limit, verbose, pkg.edit.fun, result, tests=NULL){
  could.copy <- intersect(names(formals(atime_versions)),names(formals()))
  mc <- as.list(match.call()[-1])
  copy.names <- intersect(names(mc), could.copy)
  L <- c(tests, list(...))
  out <- list()
  for(L.i in seq_along(L)){
    test.args <- L[[L.i]]
    test.name <- names(L)[[L.i]]
    if(!is.null(test.args)){
      test.args[copy.names] <- mc[copy.names]
      out[[test.name]] <- test.args
    }
  }
  out
}
