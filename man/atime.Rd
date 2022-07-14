\name{atime}
\alias{atime}
\title{Asymptotic timing}
\description{Computation time and memory for several R expressions of
  several different data sizes.}
\usage{atime(N, setup, times, verbose=FALSE, seconds.limit=0.01, ...)}
\arguments{
  \item{N}{numeric vector of data sizes to vary.}
  \item{setup}{expression to evaluate for every data size, before timings.}
  \item{times}{number of times to evaluate each timed expression.}
  \item{verbose}{logical, print messages after every data size?}
  \item{seconds.limit}{if the median timing of any expression exceeds
    this many seconds, then no timings for larger N are computed.}
  \item{\dots}{named expressions to time.}
}
\details{Each iteration involves first computing the setup expression,
  and then computing several times the \dots expressions.}
\value{list of class atime with elements \code{seconds.limit} (numeric
  input param), \code{timings} (data table of results).}

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

if(require("ggplot2")){
  sec.df <- data.frame(panel="seconds", atime.list[["timings"]])
  mem.df <- data.frame(panel="kilobytes", atime.list[["timings"]])
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

}
