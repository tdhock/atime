references_funs <- list(
  "1"=function(N)1,
  "\\log N"=function(N)log10(log(N)),
  N=function(N)log10(N),
  "N \\log N"=function(N)log10(N) + log10(log(N)),
  "N^2"=function(N)2*log10(N),
  "N^3"=function(N)3*log10(N),
  "2^N"=function(N)N*log10(2))

references <- function
(N, empirical, lower.limit,
  fun.list=NULL
){
  fun.latex <- reference <- . <- NULL
  if(is.null(fun.list))fun.list <- references_funs
  data.table(fun.latex=names(fun.list))[, {
    fun <- fun.list[[fun.latex]]
    log10.vec <- fun(N)
    last.empirical <- empirical[which.max(N)]
    one.fun <- data.table(
      N, empirical,
      reference=10^(log10.vec-max(log10.vec)+log10(last.empirical))
    )
    above <- one.fun[lower.limit < reference]
    last.two <- one.fun[(.N-1):.N]
    if(1 < nrow(above) || length(unique(one.fun$reference))==1){
      above
    }else{
      lower.N <- last.two[, stats::approx(reference, N, lower.limit)$y]
      lower.emp <- last.two[, stats::approx(N, empirical, lower.N)$y]
      rbind(data.table(
        N=as.integer(lower.N), 
        empirical=lower.emp, 
        reference=lower.limit), 
        above)
    }
  }, by=.(fun.latex, fun.name=gsub("\\", "", fun.latex, fixed=TRUE))]
}

references_best <- function(L, unit.col.vec=NULL, more.units=NULL, fun.list=NULL){
  N <- expr.name <- . <- fun.name <- dist <- empirical <- reference <-
    fun.latex <- overall.rank <- each.sign.rank <- seconds.limit <- 
      unit <- NULL
  ## Above for R CMD check.
  if(is.null(unit.col.vec)){
    unit.col.vec <- c(
      "kilobytes",
      seconds="median")
  }
  if(!is.null(more.units)){
    unit.col.vec <- c(unit.col.vec, more.units)
  }
  DT <- L[["measurements"]]
  not.found <- unit.col.vec[!unit.col.vec %in% names(DT)]
  if(length(not.found)){
    stop(
      "some units were not found (fix by creating columns in measurements): ",
      paste(not.found, collapse=", "))
  }
  to.rep <- if(is.null(names(unit.col.vec))){
    rep(TRUE, length(unit.col.vec))
  }else{
    names(unit.col.vec) == "" | is.na(names(unit.col.vec))
  }
  names(unit.col.vec)[to.rep] <- unit.col.vec[to.rep]
  ref.dt.list <- list()
  metric.dt.list <- list()
  for(unit in names(unit.col.vec)){
    col.name <- unit.col.vec[[unit]]
    values <- DT[[col.name]]
    if(!is.numeric(values)){
      stop("each unit must be numeric, but ", unit, " is not")
    }
    only.positive <- values[0 < values]
    if(length(only.positive)){
      prop.above <- 0.1
      m <- min(only.positive)
      M <- max(only.positive)
      lower.limit <- m*(M/m)^prop.above
      all.refs <- DT[
      , references(N, .SD[[col.name]], lower.limit, fun.list)
      , by=expr.name]
      all.refs[, rank := rank(-N), by=.(expr.name, fun.name)]
      second <- all.refs[rank==2]
      second[, dist := log10(empirical/reference) ]
      second[, sign := sign(dist)]
      l.cols <- list(overall="expr.name", each.sign=c("expr.name","sign"))
      for(best.type in names(l.cols)){
        by <- l.cols[[best.type]]
        second[
        , paste0(best.type,".rank") := rank(abs(dist))
        , by=by]
      }
      ref.dt.list[[unit]] <- data.table(unit, all.refs[
        second,
        on=.(expr.name, fun.name, fun.latex)])
      best <- second[overall.rank==1, .(expr.name, fun.name, fun.latex)]
      metric.dt.list[[unit]] <- data.table(unit, best[
        DT, on=.(expr.name)
      ][, `:=`(
        expr.class=paste0(expr.name,"\n",fun.name),
        expr.latex=sprintf("%s\n$O(%s)$", expr.name, fun.latex),
        empirical=get(col.name)
      )])
    }
  }
  structure(list(
    seconds.limit=L[["seconds.limit"]],
    references=do.call(rbind, ref.dt.list),
    measurements=do.call(rbind, metric.dt.list)),
    class="references_best")
}

plot.references_best <- function(x, ...){
  expr.name <- N <- reference <- fun.name <- empirical <- NULL
  ## Above for R CMD check.
  meas <- x[["measurements"]]
  if(requireNamespace("ggplot2")){
    hline.df <- with(x, data.frame(seconds.limit, unit="seconds"))
    ref.dt <- x[["references"]][each.sign.rank==1]
    ref.color <- "violet"
    emp.color <- "black"
    gg <- ggplot2::ggplot()+
      ggplot2::facet_grid(unit ~ expr.name, scales="free")+
      ggplot2::theme_bw()+
      ggplot2::geom_hline(ggplot2::aes(
        yintercept=seconds.limit),
        color="grey",
        data=hline.df)+
      ggplot2::geom_ribbon(ggplot2::aes(
        N, ymin=min, ymax=max),
        data=meas[unit=="seconds"],
        fill=emp.color,
        alpha=0.5)+
      ggplot2::geom_line(ggplot2::aes(
        N, empirical),
        size=2,
        color=emp.color,
        data=meas)+
      ggplot2::geom_line(ggplot2::aes(
        N, reference, group=fun.name),
        color=ref.color,
        size=1,
        data=ref.dt)+
      ggplot2::scale_y_log10("")+
      ggplot2::scale_x_log10()
    if(requireNamespace("directlabels")){
      gg+
        directlabels::geom_dl(ggplot2::aes(
          N, reference, label=fun.name),
          data=ref.dt,
          color=ref.color,
          method="bottom.polygons")
    }else{
      gg
    }
  }else{
    lattice::xyplot(
      log10(empirical) ~ log10(N) | unit, meas, 
      groups=expr.name, type="l", 
      ylab="log10(median)",
      scales=list(relation="free"),
      auto.key=list(space="right", points=FALSE, lines=TRUE))
  }
}

print.references_best <- function(x, ...){
  expr.name <- . <- fun.name <- unit <- NULL
  summary.dt <- x$measurements[!is.na(fun.name), .(
    summary=sprintf("%s %s", fun.name[1], unit[1])
  ), by=.(expr.name, unit)][, .(
    summary=paste(summary, collapse=", ")
  ), by=expr.name]
  summary.vec <- summary.dt[, sprintf("%s (%s)", expr.name, summary)]
  cat(with(x, sprintf(
    "references_best list with %s measurements, best fit complexity:\n%s\n",
    nrow(x[["measurements"]]),
    paste(summary.vec, collapse="\n"))))
}
