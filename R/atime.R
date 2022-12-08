atime <- function(N, setup, expr.list=NULL, times=10, seconds.limit=0.01, verbose=FALSE, results=TRUE, ...){
  kilobytes <- mem_alloc <- NULL
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.list <- mc.args[!names(mc.args) %in% formal.names]
  elist <- c(expr.list, dots.list)
  done.vec <- structure(rep(FALSE, length(elist)), names=names(elist))
  metric.dt.list <- list()
  for(N.value in N){
    not.done.yet <- names(done.vec)[!done.vec]
    if(length(not.done.yet)){
      N.env <- new.env(parent=parent.frame())
      N.env$N <- N.value
      eval(mc.args$setup, N.env)
      m.list <- list(bench::mark, iterations=times,check=FALSE)
      N.env$result.list <- list()
      for(expr.name in not.done.yet){
        expr <- elist[[expr.name]]
        m.list[[expr.name]] <- if(results){
          substitute(
            result.list[NAME] <- list(EXPR),
            list(NAME=expr.name, EXPR=expr))
        }else{
          expr
        }
      }
      m.call <- as.call(m.list)
      N.df <- eval(m.call, N.env)
      if(results){
        N.df$result <- N.env$result.list
      }
      N.stats <- data.table(N=N.value, expr.name=not.done.yet, N.df)
      N.stats[, `:=`(
        kilobytes=as.numeric(mem_alloc)/1024,
        mem_alloc=NULL, total_time=NULL, expression=NULL)]
      summary.funs <- list(
        median=median, min=min,
        q25=function(x)quantile(x,0.25),
        q75=function(x)quantile(x,0.75),
        max=max, mean=mean, sd=sd)
      for(fun.name in names(summary.funs)){
        N.stats[[fun.name]] <- sapply(N.df[["time"]], summary.funs[[fun.name]])
      }
      done.pkgs <- N.stats[median > seconds.limit, paste(expr.name)]
      done.vec[done.pkgs] <- TRUE
      if(verbose)print(N.stats[, data.table(
        N, expr.name, seconds.median=median, kilobytes)],
        class=FALSE)
      metric.dt.list[[paste(N.value)]] <- N.stats
    }
  }
  structure(
    list(
      seconds.limit=seconds.limit,
      measurements=do.call(rbind, metric.dt.list)),
    class="atime")
}

plot.atime <- function(x, ...){
  expr.name <- NULL
  lattice::xyplot(
    log10(median) ~ log10(N), x$measurements, 
    groups=expr.name, type="l", 
    ylab="log10(median seconds)",
    auto.key=list(space="right", points=FALSE, lines=TRUE))
}

print.atime <- function(x, ...){
  N_max <- N_min <- expr.name <- NULL
  summary.dt <- suppressWarnings(dcast(
    x$measurements, expr.name ~ ., list(min, max), value.var="N"))
  expr.vec <- summary.dt[, paste0(
    expr.name, "(N=", N_min, " to ", N_max, ")")]
  cat(
    nrow(x$measurements),
    " measurements for ", 
    paste(expr.vec, collapse=", "),
    "\n",
    sep="")
}
