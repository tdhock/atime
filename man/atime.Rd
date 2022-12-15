\name{atime}
\alias{atime}
\title{Asymptotic timing}
\description{Computation time and memory for several R expressions of
  several different data sizes.}
\usage{atime(
N, setup, expr.list=NULL, times=10, seconds.limit=0.01, verbose=FALSE,
result=FALSE, ...)}
\arguments{
  \item{N}{numeric vector of data sizes to vary.}
  \item{setup}{expression to evaluate for every data size, before timings.}
  \item{expr.list}{named list of expressions to time.}
  \item{times}{number of times to evaluate each timed expression.}
  \item{seconds.limit}{if the median timing of any expression exceeds
    this many seconds, then no timings for larger N are computed.}
  \item{verbose}{logical, print messages after every data size?}
  \item{result}{logical, save each result?}
  \item{\dots}{named expressions to time.}
}
\details{Each iteration involves first computing the setup expression,
  and then computing several times the \dots expressions. For
  convenience, expressions may be specified either via code (\dots) or
  data (\code{expr.list} arg).}
\value{list of class atime with elements \code{seconds.limit} (numeric
  input param), \code{measurements} (data table of results).}

\author{Toby Dylan Hocking}

\examples{

## Example 1: polynomial vs exponential time regex.
atime.list <- atime::atime(
  PCRE=regexpr(pattern, subject, perl=TRUE),
  TRE=regexpr(pattern, subject, perl=FALSE),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  N=1:30)

if(require("ggplot2")){
  measurements <- atime.list[["measurements"]]
  sec.df <- data.frame(panel="seconds", measurements)
  mem.df <- data.frame(panel="kilobytes", measurements)
  hline.df <- with(atime.list, data.frame(seconds.limit, panel="seconds"))
  gg <- ggplot()+
    theme_bw()+
    facet_grid(panel ~ ., scales="free")+
    geom_hline(aes(
      yintercept=seconds.limit),
      color="grey",
      data=hline.df)+
    geom_ribbon(aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=sec.df,
      alpha=0.5)+
    geom_line(aes(
      N, median, color=expr.name),
      data=sec.df)+
    geom_line(aes(
      N, kilobytes, color=expr.name),
      data=mem.df)+
    scale_y_log10("")+
    scale_x_log10()
  if(require("directlabels")){
    directlabels::direct.label(gg, "last.polygons")+
      coord_cartesian(xlim=c(1,40))
  }else{
    gg
  }
}

## Example 2: split data table vs frame, constant factor difference.
library(data.table)
atime.list <- atime::atime(
  N=as.integer(10^seq(1, 7)),
  setup={
    set.seed(1)
    DT <- data.table(
      x1 = rep(c("c","d"), l=N),
      x2 = rep(c("x","y"), l=N),
      x3 = rep(c("a","b"), l=N),
      y = rnorm(N)
    )[sample(.N)]
    DF <- as.data.frame(DT)
  },
  frame=split(DF[names(DF) != "x1"], DF["x1"], drop = TRUE),
  table=split(DT, by = "x1", keep.by = FALSE, drop = TRUE)
)
best.list <- atime::references_best(atime.list)

if(require(ggplot2)){
  hline.df <- with(atime.list, data.frame(seconds.limit, unit="seconds"))
  gg <- ggplot()+
    theme_bw()+
    facet_grid(unit ~ ., scales="free")+
    geom_hline(aes(
      yintercept=seconds.limit),
      color="grey",
      data=hline.df)+
    geom_line(aes(
      N, empirical, color=expr.name),
      data=best.list$meas)+
    geom_ribbon(aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=best.list$meas[unit=="seconds"],
      alpha=0.5)+
    scale_x_log10()+
    scale_y_log10("median line, min/max band")
  if(require(directlabels)){
    gg+
      directlabels::geom_dl(aes(
        N, empirical, color=expr.name, label=expr.name),
        method="right.polygons",
        data=best.list$meas)+
      theme(legend.position="none")+
      coord_cartesian(xlim=c(1,2e7))
  }else{
    gg
  }
}

}
