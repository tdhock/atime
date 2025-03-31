predict.references_best <- function(object, ...){
  . <- N <- expr.name <- unit <- empirical <- log10.empirical <-
    log10.N <- NULL
  ## Above for CRAN NOTEs.
  L <- list(...)
  if(length(L)==0){
    L <- list(seconds=object[["seconds.limit"]])
  }
  if(is.null(names(L)) || any(names(L)=="")){
    stop("... has an un-named argument, but must have a unit as the name of each argument")
  }
  name.tab <- table(names(L))
  too.many <- name.tab[1 < name.tab]
  if(length(too.many)){
    info.vec <- sprintf("%s(%d)", names(too.many), too.many)
    info.str <- paste(info.vec, collapse=", ")
    stop("argument names should be unique, problem(count): ", info.str)
  }
  object$prediction <- data.table(unit=names(L))[, {
    unit.value <- L[[unit]]
    if(!is.numeric(unit.value)){
      stop("... has a non-numeric argument (", unit, "), but each argument must be numeric (unit value at which to interpolate/predict N)")
    }
    if(length(unit.value) != 1){
      stop("... has an argument with length != 1 (", unit, "), but each argument must be scalar (unit value at which to interpolate/predict N)")
    }
    if(!is.finite(unit.value)){
      stop("... has a non-finite argument (", unit, ") but each argument must be finite (unit value at which to interpolate/predict N)")
    }
    is.unit <- object$measurements$unit == unit
    if(!any(is.unit)){
      stop(unit, " is not a valid unit; argument names of predict must be one of these valid units: ", paste(sort(unique(object$measurements$unit)), collapse=", "))
    }
    meas <- object$measurements[is.unit & 0<empirical]
    pred.dt <- meas[, {
      uniq.dt <- .SD[, .(
        log10.N=max(log10(N))
      ), by=.(log10.empirical=log10(empirical))]
      if(nrow(uniq.dt)<2){
        data.table()
      }else{
        uniq.dt[, data.table(
          unit.value,
          N=10^approx(log10.empirical, log10.N, log10(unit.value))$y)]
      }
    }, by=c(object$by.vec)]
    not.NA <- pred.dt[!is.na(N)]
    if(nrow(not.NA)==0){
      stop(unit, "=", unit.value, " is outside range of data, please change to a value that intersects at least one of the empirical curves")
    }
    not.NA
  }, by=unit][
  , label := paste0(
    expr.name,
    "\nN=",
    format(N, big.mark=",", scientific=FALSE, trim=TRUE)
  )][]
  class(object) <- c("atime_prediction", class(object))
  object
}

plot.atime_prediction <- function(x, ...){
  expr.name <- N <- empirical <- unit <- unit.value <- NULL
  meas <- x[["measurements"]][unit %in% x$prediction$unit]
  if(requireNamespace("ggplot2")){
    pred <- x[["prediction"]]
    one <- pred[, .SD[1], by=unit]
    gg <- ggplot2::ggplot()+
      ggplot2::theme_bw()+
      ggplot2::facet_grid(unit ~ ., scales="free")+
      ggplot2::geom_hline(ggplot2::aes(
        yintercept=unit.value),
        data=one)+
      ggplot2::geom_text(ggplot2::aes(
        0, unit.value, label=paste0(unit,"=",unit.value)),
        hjust=0,
        vjust=1.2, 
        data=one)+
      ggplot2::geom_ribbon(ggplot2::aes(
        N, ymin=min, ymax=max, fill=expr.name),
        data=meas[unit=="seconds"],
        alpha=0.5)+
      ggplot2::geom_line(ggplot2::aes(
        N, empirical, color=expr.name),
        data=meas)+
      ggplot2::scale_x_log10(
        "N",
        breaks=meas[, 10^seq(
          ceiling(min(log10(N))),
          floor(max(log10(N))))])+
      ggplot2::scale_y_log10(
        "median line, min/max band")+
      ggplot2::geom_point(ggplot2::aes(
        N, unit.value, color=expr.name),
        data=pred,
        shape=21,
        fill="white")
    if(requireNamespace("directlabels")){
      gg+
        directlabels::geom_dl(ggplot2::aes(
          N, unit.value, 
          label=label,
          color=expr.name),
          data=pred,
          method="top.polygons")+
        ggplot2::theme(legend.position="none")
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

print.atime_prediction <- function(x, ...){
  cat("atime_prediction object\n")
  print(x$prediction)
}

