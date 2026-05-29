default_N <- function(){
  as.integer(2^seq(1, 20))
}

get_result_rows <- function(result.list, N.stats){
  out.rows <- list()
  out.names <- list()
  for(expr.name in names(result.list)){
    result <- result.list[[expr.name]]
    if(is.data.frame(result) && nrow(result)==1){
      is.num <- sapply(result, is.numeric)
      new.bad <- intersect(names(result), names(N.stats))
      if(length(new.bad)){
        stop(sprintf("value of expression %s is 1 row data frame with column(s) named %s (reserved for internal use); please fix by changing the column name(s) in your results", expr.name, paste(new.bad, collapse=", ")))
      }
      out.rows[[expr.name]] <- data.table(expr.name, result)
      out.names[[expr.name]] <- names(result)[is.num]
    }
  }
  list(
    result.rows=rbindlist(out.rows, use.names=TRUE, fill=TRUE),
    more.units=unique(unlist(out.names)))
}

run_bench_mark <- function(times, sub.elist, N.env, result){
  m.list <- list(quote(bench::mark), iterations=times, check=FALSE)
  N.env$result.list <- list()
  for(expr.name in names(sub.elist)){
    expr <- sub.elist[[expr.name]]
    m.list[expr.name] <- list(if(result$keep){
      substitute(
        result.list[NAME] <- list(FUN(EXPR)),
        list(NAME=expr.name, FUN=result$fun, EXPR=expr))
    }else{
      expr
    })
  }
  m.call <- as.call(m.list)
  N.df <- suppressWarnings(eval(m.call, N.env))
  if(result$keep){
    N.df$result <- N.env$result.list
  }
  N.df
}

check_atime_inputs <- function(N, result, elist){
  if(!is.numeric(N)){
    stop("N should be a numeric vector")
  }
  if(length(N)<2){
    stop("length(N) should be at least 2")
  }
  N.tab <- table(N)
  N.bad <- N.tab[N.tab>1]
  if(length(N.bad)){
    stop("please remove duplicate values from N: ", paste(names(N.bad), collapse=", "))
  }
  if(length(elist)==0){
    stop("no expressions to measure; please provide at least one expression in ... or expr.list")
  }
  name.tab <- table(names(elist))
  bad.names <- names(name.tab)[name.tab>1]
  if(length(bad.names))stop(
    "each expression must have a unique name, problems: ", 
    paste(bad.names, collapse=", "))
  fun <- identity
  keep <- if(is.function(result)){
    fun <- result
    TRUE
  }else if(isTRUE(result)){
    TRUE
  }else if(is.null(result)){
    fun <- function(out)if(is.data.frame(out) && nrow(out)==1)out
    TRUE
  }else{
    FALSE
  }
  list(keep=keep, fun=fun)
}

atime <- function(N=default_N(), setup, expr.list=NULL, times=10, seconds.limit=0.01, verbose=FALSE, result=NULL, N.env.parent=NULL, ...){
  kilobytes <- mem_alloc <- . <- sizes <- expr.name <- NULL
  ## above for CRAN NOTE.
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.list <- mc.args[!names(mc.args) %in% formal.names]
  if(!missing(expr.list) && !is.list(expr.list)){
    stop(domain=NA, gettextf("expr.list should be a list of expressions to run for various N, but has classes %s", paste(class(expr.list), collapse=", ")))
  }
  elist <- c(expr.list, dots.list)
  result <- check_atime_inputs(N, result, elist)
  if(is.null(N.env.parent)){
    N.env.parent <- parent.frame()
  }
  done.vec <- structure(rep(FALSE, length(elist)), names=names(elist))
  metric.dt.list <- list()
  for(N.value in N){
    not.done.yet <- names(done.vec)[!done.vec]
    if(length(not.done.yet)==0)break
    N.env <- new.env(parent=N.env.parent)
    N.env$N <- N.value
    eval(mc.args$setup, N.env)
    N.df <- run_bench_mark(times, elist[not.done.yet], N.env, result)
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
    result.row.list <- get_result_rows(N.env$result.list, N.stats)
    if(nrow(result.row.list$result.rows)){
      N.stats <- result.row.list$result.rows[N.stats, on="expr.name"]
    }
    if(verbose)print(N.stats[, data.table(
      N, expr.name, seconds.median=median, kilobytes)],
      class=FALSE)
    metric.dt.list[[paste(N.value)]] <- N.stats
  }
  unit.col.vec <- c(
    "kilobytes",
    seconds="median",
    result.row.list$more.units)
  measurements <- rbindlist(metric.dt.list, use.names=TRUE, fill=TRUE)
  expr.list.params <- attr(expr.list,"parameters")
  by.vec <- "expr.name"
  if(is.data.table(expr.list.params)){
    measurements <- expr.list.params[measurements, on="expr.name"]
    by.vec <- names(expr.list.params)
  }
  only.one <- measurements[, .(sizes=.N), by=expr.name][sizes==1]
  if(nrow(only.one)){
    warning("please increase max N or seconds.limit, because only one N was evaluated for expr.name: ", paste(only.one[["expr.name"]], collapse=", "))
  }
  structure(
    list(
      unit.col.vec=unit.col.vec,
      seconds.limit=seconds.limit,
      measurements=measurements,
      by.vec=by.vec),
    class="atime")
}

plot.atime <- function(x, ...){
  expr.name <- N <- kilobytes <- NULL
  ## Above to avoid CRAN NOTE.
  meas <- x[["measurements"]]
  by.dt <- meas[, x$by.vec, with=FALSE]
  if(requireNamespace("ggplot2")){
    tall.list <- list()
    for(unit.i in seq_along(x$unit.col.vec)){
      col.name <- x$unit.col.vec[[unit.i]]
      unit <- names(x$unit.col.vec)[[unit.i]]
      if(is.null(unit)||unit=="")unit <- col.name
      tall.list[[unit.i]] <- meas[, data.table(
        N, by.dt, unit, median=get(col.name))]
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
