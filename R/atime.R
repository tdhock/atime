atime_grid <- function
(param.list=list(),
  name.value.sep="=",
  expr.param.sep=" ",
  collapse=",",
  ...){
  if(!is.list(param.list)){
    stop("param.list must be a named list of parameters")
  }
  if(any(names(param.list)=="")){
    stop("each element of param.list must be named")
  }
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  elist <- mc.args[!names(mc.args) %in% formal.names]
  if(is.null(names(elist)) || any(names(elist)=="")){
    stop("each expression in ... must be named")
  }
  if(length(param.list)==0)return(elist)
  param.dt <- do.call(CJ, param.list)
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
  value.mat <- do.call(cbind, lapply(param.dt, paste))
  name.vec <- colnames(value.mat)[col(value.mat)]
  name.value.mat <- matrix(
    paste0(name.vec, name.value.sep, value.mat),
    nrow(value.mat), ncol(value.mat))
  name.value.vec <- apply(name.value.mat, 1, paste, collapse=collapse)
  out.list <- list()
  for(expr.name in names(elist)){
    for(row.i in 1:nrow(param.dt)){
      param.name.value <- name.value.vec[[row.i]]
      out.name <- paste0(expr.name, expr.param.sep, param.name.value)
      out.list[[out.name]] <- eval(substitute(
        substitute(EXPR, param.dt[row.i]), 
        list(EXPR=elist[[expr.name]])))
    }
  }
  out.list
}

atime <- function(N, setup, expr.list=NULL, times=10, seconds.limit=0.01, verbose=FALSE, result=FALSE, ...){
  kilobytes <- mem_alloc <- NULL
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.list <- mc.args[!names(mc.args) %in% formal.names]
  if(!missing(expr.list) && !is.list(expr.list)){
    stop(domain=NA, gettextf("expr.list should be a list of expressions to run for various N, but has classes %s", paste(class(expr.list), collapse=", ")))
  }
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
      m.list <- list(quote(bench::mark), iterations=times,check=FALSE)
      N.env$result.list <- list()
      for(expr.name in not.done.yet){
        expr <- elist[[expr.name]]
        m.list[expr.name] <- list(if(result){
          substitute(
            result.list[NAME] <- list(EXPR),
            list(NAME=expr.name, EXPR=expr))
        }else{
          expr
        })
      }
      m.call <- as.call(m.list)
      N.df <- eval(m.call, N.env)
      if(result){
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

log_scale_max <- function(N, prop.expand=0.3){
  max(N)*(max(N)/min(N))^prop.expand
}

plot.atime <- function(x, ...){
  expr.name <- N <- kilobytes <- NULL
  meas <- x[["measurements"]]
  if(requireNamespace("ggplot2")){
    gg <- ggplot2::ggplot()+
      ggplot2::geom_ribbon(ggplot2::aes(
        N, ymin=min, ymax=max, fill=expr.name),
        data=data.table(meas, unit="seconds"),
        alpha=0.5)+
      ggplot2::geom_line(ggplot2::aes(
        N, median, color=expr.name),
        data=data.table(meas, unit="seconds"))+
      ggplot2::geom_line(ggplot2::aes(
        N, kilobytes, color=expr.name),
        data=data.table(meas, unit="kilobytes"))+
      ggplot2::facet_grid(unit ~ ., scales="free")+
      ggplot2::scale_x_log10(
        limits=c(NA, log_scale_max(meas$N)))+
      ggplot2::scale_y_log10("median line, min/max band")
    if(requireNamespace("directlabels")){
      directlabels::direct.label(gg, "right.polygons")
    }else{
      gg
    }
  }else{
    lattice::xyplot(
      log10(median) ~ log10(N), meas, 
      groups=expr.name, type="l", 
      ylab="log10(median seconds)",
      auto.key=list(space="right", points=FALSE, lines=TRUE))
  }
}

print.atime <- function(x, ...){
  N_max <- N_min <- expr.name <- NULL
  summary.dt <- suppressWarnings(dcast(
    x$measurements, expr.name ~ ., list(min, max), value.var="N"))
  expr.vec <- summary.dt[, paste0(
    expr.name, "(N=", N_min, " to ", N_max, ")")]
  cat(
    "atime list with ",
    nrow(x$measurements),
    " measurements for\n", 
    paste(expr.vec, collapse="\n"),
    "\n",
    sep="")
}
