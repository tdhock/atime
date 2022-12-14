atime_grid <- function
(param.list,
  arg.name.fun=function(name.vec, value.vec){
    paste0(name.vec,"=",value.vec)
  },
  expr.name.fun=function(expr.str, arg.vec){
    paste0(expr.str," ",arg.vec)
  }, 
  collapse=",",
  ...){
  if(!is.list(param.list)){
    stop("param.list must be a named list of parameters")
  }
  if(is.null(names(param.list)) || any(names(param.list)=="")){
    stop("each element of param.list must be named")
  }
  param.dt <- do.call(CJ, param.list)
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  elist <- mc.args[!names(mc.args) %in% formal.names]
  if(is.null(names(elist)) || any(names(elist)=="")){
    stop("each expression in ... must be named")
  }
  ## check to make sure each param is in each expr.
  one.param.list <- as.list(param.dt[1])
  problem.list <- list()
  for(expr.name in names(elist)){
    before.sub <- elist[[expr.name]]
    for(param.name in names(one.param.list)){
      param.sub.list <- one.param.list[param.name]
      after.sub <- eval(substitute(
        substitute(EXPR, param.sub.list), 
        list(EXPR=before.sub)))
      if(identical(paste(before.sub), paste(after.sub))){
        problem.list[[paste(expr.name, param.name)]] <- paste(
          param.name, "not in", expr.name)
      }
    }
  }
  if(length(problem.list)){
    stop(
      "each param should be present in each expr, problems: ",
      paste(problem.list, collapse=", "))
  }
  value.mat <- sapply(param.dt, paste)
  name.value.mat <- matrix(
    arg.name.fun(colnames(value.mat)[col(value.mat)], value.mat),
    nrow(value.mat), ncol(value.mat))
  name.value.vec <- apply(name.value.mat, 1, paste, collapse=collapse)
  out.list <- list()
  for(expr.name in names(elist)){
    for(row.i in 1:nrow(param.dt)){
      out.name <- expr.name.fun(expr.name, name.value.vec[[row.i]])
      out.list[[out.name]] <- eval(substitute(
        substitute(EXPR, param.dt[row.i]), 
        list(EXPR=elist[[expr.name]])))
    }
  }
  out.list
}

atime_exprs <- function(expr, env.list=NULL, ...){
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.list <- mc.args[!names(mc.args) %in% formal.names]
  elist <- c(env.list, dots.list)
  if(is.null(names(elist)) || any(names(elist)=="")){
    stop("each element of env.list and ... must be named")
  }
  out.list <- list()
  for(expr.name in names(elist)){
    out.list[[expr.name]] <- substitute(
      substitute(EXPR, elist[[expr.name]]),
      list(EXPR=expr))
  }
  out.list
}

atime <- function(N, setup, expr.list=NULL, times=10, seconds.limit=0.01, verbose=FALSE, results=TRUE, ...){
  kilobytes <- mem_alloc <- NULL
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.list <- mc.args[!names(mc.args) %in% formal.names]
  elist <- c(expr.list, dots.list)
  name.tab <- table(names(elist))
  bad.names <- names(name.tab)[name.tab>1]
  if(length(bad.names))stop(
    "each expression must have a unique name, problems: ", 
    paste(bad.names, collapse=", "))
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
