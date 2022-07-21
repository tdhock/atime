\name{references_best}
\alias{references_best}
\title{Best references}
\description{Compute best asymptotic references.}
\usage{references_best(L, unit.col.vec=NULL, more.units=NULL, fun.list=NULL)}
\arguments{
  \item{L}{List output from atime.}
  \item{unit.col.vec}{Named character vector of units, default NULL
    means standard units (kilobytes and seconds).}
  \item{more.units}{Named character vector of units to add to
    \code{unit.col.vec}, default NULL means nothing.}
  \item{fun.list}{List of asymptotic complexity reference functions,
    default NULL means to use package default.}
}
\value{list of class \code{"references_best"} with elements
  \code{references} (data table of references),
  \code{measurements} (data table of measurements).}

\author{Toby Dylan Hocking}

\examples{

atime.list <- atime::atime(
  PCRE=regexpr(pattern, subject, perl=TRUE),
  TRE=regexpr(pattern, subject, perl=FALSE),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  N=1:30)
best.list <- atime::references_best(atime.list)
plot(best.list)

if(require("ggplot2")){
  hline.df <- with(atime.list, data.frame(seconds.limit, unit="seconds"))
  ref.dt <- best.list$ref[overall.rank==1]
  gg <- ggplot()+
    theme_bw()+
    facet_grid(unit ~ ., scales="free")+
    geom_line(aes(
      N, reference, group=paste(expr.name, fun.name)),
      color="grey",
      data=ref.dt)+
    geom_hline(aes(
      yintercept=seconds.limit),
      color="grey",
      data=hline.df)+
    geom_ribbon(aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=best.list$meas[unit=="seconds"],
      alpha=0.5)+
    geom_line(aes(
      N, empirical, color=expr.name),
      data=best.list$meas)+
    scale_y_log10("")+
    scale_x_log10()
  if(require("directlabels")){
    gg+
      theme(legend.position="none")+
      directlabels::geom_dl(aes(
        N, empirical, color=expr.name, label=expr.name),
        data=best.list$meas,
        method="last.polygons")+
      directlabels::geom_dl(aes(
        N, reference, label=fun.name),
        data=ref.dt,
        color="grey",
        method="bottom.polygons")+
      coord_cartesian(xlim=c(1,40))
  }else{
    gg
  }
}

}
