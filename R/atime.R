atime <- function(N, setup, times=5, verbose=FALSE, seconds.limit=0.01, ...){
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  expr.list <- mc.args[!names(mc.args) %in% formal.names]
  done.vec <- structure(rep(FALSE, length(expr.list)), names=names(expr.list))
  timing.dt.list <- list()
  for(N.value in N){
    not.done.yet <- names(done.vec)[!done.vec]
    if(length(not.done.yet)){
      N.env <- new.env()
      N.env$N <- N.value
      eval(mc.args$setup, N.env)
      m.args <- list(iterations=times,check=FALSE)
      result.list <- list()
      for(expr.name in not.done.yet){
        expr <- expr.list[[expr.name]]
        m.args[[expr.name]] <- substitute(
          result.list[[NAME]] <<- EXPR,
          list(NAME=expr.name, EXPR=expr))
      }
      N.df <- with(N.env, do.call(bench::mark, m.args))
      N.stats <- data.table(N=N.value, expr.name=not.done.yet, N.df)
      N.stats[, kilobytes := as.numeric(mem_alloc)/1024]
      summary.funs <- list(
        median=median, min=min,
        q25=function(x)quantile(x,0.25),
        q75=function(x)quantile(x,0.75),
        max=max, mean=mean, sd=sd)
      for(fun.name in names(summary.funs)){
        N.stats[[fun.name]] <- sapply(N.df[["time"]], summary.funs[[fun.name]])
      }
      N.stats$result <- result.list
      done.pkgs <- N.stats[median > seconds.limit, paste(expr.name)]
      done.vec[done.pkgs] <- TRUE
      if(verbose)print(N.stats[, data.table(
        N, expr.name, seconds.median=median, seconds.limit)])
      timing.dt.list[[paste(N.value)]] <- N.stats
    }
  }
  structure(
    list(
      seconds.limit=seconds.limit,
      timings=do.call(rbind, timing.dt.list)),
    class="atime")
}

plot.atime <- function(x, ...){
  sec.dt <- data.table(panel="seconds", x[["timings"]])
  mem.dt <- data.table(panel="kilobytes", x[["timings"]])
}

print.atime <- function(x, ...){
  cat("Timings for the following expressions and sizes:\n")
  print(suppressWarnings(dcast(
    x$timings, expr.name ~ ., list(min, max), value.var="N")))
}
