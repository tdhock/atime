atime_grid <- function
(param.list=list(),
  ...,
  name.value.sep="=",
  expr.param.sep=" ",
  collapse=",",
  symbol.params=character()
){
  if(!(is.character(symbol.params) && all(!is.na(symbol.params)))){
    stop("symbol.params must be a character vector with no missing values")
  }
  if(!(is.character(name.value.sep) && length(name.value.sep)==1 && !is.na(name.value.sep))){
    stop("name.value.sep must be a non-missing string")
  }
  if(!(is.character(expr.param.sep) && length(expr.param.sep)==1 && !is.na(expr.param.sep))){
    stop("expr.param.sep must be a non-missing string")
  }
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
  CJ.arg.names <- setdiff(names(formals(CJ)),"...")
  bad.params <- intersect(CJ.arg.names,names(param.list))
  if(length(bad.params)){
    stop("param.list must not have elements named: ", paste(bad.params, collapse=", "))
  }
  bad.types <- !sapply(param.list, is.atomic)
  if(any(bad.types)){
    bad.names <- names(param.list)[bad.types]
    stop("param.list elements must be atomic, but some are not: ", paste(bad.names, collapse=", "))
  }
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
      param.row.list <- as.list(param.dt[row.i])
      param.row.list[symbol.params] <- lapply(
        param.row.list[symbol.params], as.symbol)
      out.list[[out.name]] <- eval(substitute(
        substitute(EXPR, param.row.list), 
        list(EXPR=elist[[expr.name]])))
    }
  }
  out.list
}

default_N <- function(){
  as.integer(2^seq(1, 20))
}

atime <- function(N=default_N(), setup, expr.list=NULL, times=10, seconds.limit=0.01, verbose=FALSE, result=FALSE, ...){
  kilobytes <- mem_alloc <- . <- sizes <- NULL
  ## above for CRAN NOTE.
  if(!is.numeric(N)){
    stop("N should be a numeric vector")
  }
  if(length(N)<2){
    stop("length(N) should be at least 2")
  }
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.list <- mc.args[!names(mc.args) %in% formal.names]
  if(!missing(expr.list) && !is.list(expr.list)){
    stop(domain=NA, gettextf("expr.list should be a list of expressions to run for various N, but has classes %s", paste(class(expr.list), collapse=", ")))
  }
  elist <- c(expr.list, dots.list)
  if(length(elist)==0){
    stop("no expressions to measure; please provide at least one expression in ... or expr.list")
  }
  name.tab <- table(names(elist))
  bad.names <- names(name.tab)[name.tab>1]
  more.units <- character()
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
      if(
        all(sapply(N.env$result.list, is.data.frame)) &&
          all(sapply(N.env$result.list, nrow)==1)
      ){
        names.list <- lapply(N.env$result.list, names)
        for(result.i in seq_along(names.list)){
          if(!identical(names.list[[1]], names.list[[result.i]])){
            stop(sprintf("results are all 1 row data frames, but some have different names (%s, %s); please fix by making row names of results identical", names(names.list)[[1]], names(names.list)[[result.i]]))
          }
        }
        result.rows <- do.call(rbind, N.env$result.list)
        is.more <- sapply(result.rows, is.numeric)
        more.units <- names(result.rows)[is.more]
      }else{
        result.rows <- NULL
      }
      if(result){
        N.df$result <- N.env$result.list
      }
      N.stats <- data.table(
        N=N.value, expr.name=not.done.yet, N.df
      )[, `:=`(
        kilobytes=as.numeric(mem_alloc)/1024,
        memory=NULL,
        mem_alloc=NULL,
        total_time=NULL,
        expression=NULL
      )][]
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
      new.bad <- intersect(names(result.rows), names(N.stats))
      if(length(new.bad)){
        stop(sprintf("result is 1 row data frame with column(s) named %s (reserved for internal use); please fix by changing the column name(s) in your results", paste(new.bad, collapse=", ")))
      }
      N.out <- data.table(N.stats, result.rows)
      if(verbose)print(N.out[, data.table(
        N, expr.name, seconds.median=median, kilobytes)],
        class=FALSE)
      metric.dt.list[[paste(N.value)]] <- N.out
    }
  }
  unit.col.vec <- c(
    "kilobytes",
    seconds="median",
    more.units)
  measurements <- rbindlist(metric.dt.list)
  only.one <- measurements[, .(sizes=.N), by=expr.name][sizes==1]
  if(nrow(only.one)){
    warning("please increase max N or seconds.limit, because only one N was evaluated for expr.name: ", paste(only.one[["expr.name"]], collapse=", "))
  }
  structure(
    list(
      unit.col.vec=unit.col.vec,
      seconds.limit=seconds.limit,
      measurements=measurements),
    class="atime")
}

plot.atime <- function(x, ...){
  expr.name <- N <- kilobytes <- NULL
  ## Above to avoid CRAN NOTE.
  meas <- x[["measurements"]]
  if(requireNamespace("ggplot2")){
    tall.list <- list()
    for(unit.i in seq_along(x$unit.col.vec)){
      col.name <- x$unit.col.vec[[unit.i]]
      unit <- names(x$unit.col.vec)[[unit.i]]
      if(is.null(unit)||unit=="")unit <- col.name
      tall.list[[unit.i]] <- meas[, data.table(
        N, expr.name, unit, median=get(col.name))]
    }
    tall <- rbindlist(tall.list)
    gg <- ggplot2::ggplot()+
      ggplot2::theme_bw()+
      ggplot2::geom_ribbon(ggplot2::aes(
        N, ymin=min, ymax=max, fill=expr.name),
        data=data.table(meas, unit="seconds"),
        alpha=0.5)+
      ggplot2::geom_line(ggplot2::aes(
        N, median, color=expr.name),
        data=tall)+
      ggplot2::facet_grid(unit ~ ., scales="free")+
      ggplot2::scale_x_log10(
        breaks=meas[, 10^seq(
          ceiling(min(log10(N))),
          floor(max(log10(N))))],
        limits=c(NA, meas[, max(N)*(max(N)/min(N))^0.5]))+
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
