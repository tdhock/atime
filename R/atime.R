atime <- function(N, setup, times=10, verbose=FALSE, seconds.limit=0.01, ...){
  kilobytes <- mem_alloc <- NULL
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  expr.list <- mc.args[!names(mc.args) %in% formal.names]
  done.vec <- structure(rep(FALSE, length(expr.list)), names=names(expr.list))
  timing.dt.list <- list()
  for(N.value in N){
    not.done.yet <- names(done.vec)[!done.vec]
    if(length(not.done.yet)){
      N.env <- new.env(parent=parent.frame())
      N.env$N <- N.value
      eval(mc.args$setup, N.env)
      m.list <- list(bench::mark, iterations=times,check=FALSE)
      N.env$result.list <- list()
      for(expr.name in not.done.yet){
        expr <- expr.list[[expr.name]]
        m.list[[expr.name]] <- substitute(
          result.list[[NAME]] <- EXPR,
          list(NAME=expr.name, EXPR=expr))
      }
      m.call <- as.call(m.list)
      N.df <- eval(m.call, N.env)
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
      N.stats$result <- N.env$result.list
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
  expr.name <- NULL
  lattice::xyplot(
    log10(median) ~ log10(N), x$timings, 
    groups=expr.name, type="l", 
    ylab="log10(median seconds)",
    auto.key=list(space="right", points=FALSE, lines=TRUE))
}

print.atime <- function(x, ...){
  N_max <- N_min <- expr.name <- NULL
  summary.dt <- suppressWarnings(dcast(
    x$timings, expr.name ~ ., list(min, max), value.var="N"))
  expr.vec <- summary.dt[, paste0(
    expr.name, "(N=", N_min, " to ", N_max, ")")]
  cat(
    nrow(x$timings),
    " timings for ", 
    paste(expr.vec, collapse=", "),
    "\n",
    sep="")
}
