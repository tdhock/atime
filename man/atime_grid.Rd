\name{atime_grid}
\alias{atime_grid}
\title{
  Asymptotic timing grid
}
\description{
  Create expressions for asymptotic timing by substituting values into
  expressions. 
}
\usage{
atime_grid(
  param.list = list(), 
  name.value.sep="=",
  expr.param.sep=" ",
  collapse = ",", 
  ...)
}
\arguments{
  \item{param.list}{
    Named list of items to replace in ... expressions, default empty
    list means nothing to replace.
  }
  \item{name.value.sep}{
    string: separator between names and values from \code{param.list},
    default \code{"="}.
  }
  \item{expr.param.sep}{
    string: separator between expressions and parameters, default
    \code{" "}. 
  }
  \item{collapse}{
    string: separator between parameters, default \code{","}.
  }
  \item{\dots}{
    Named expressions which each must contain each name of
    \code{param.list}.
  }
}
\value{
  Named list of expressions which can be used as \code{expr.list}
  argument of \code{\link{atime}}.
}
\author{
  Toby Dylan Hocking
}
\examples{

## Example 0: with no param.list, same as quote inside named list.
atime::atime_grid(m=mean(data), s=sum(data))
list(m=quote(mean(data)), s=quote(sum(data)))

## Example 1: polynomial vs exponential time regex.
(expr.list <- atime::atime_grid(
  list(PERL=c(TRUE, FALSE)),
  expr.param.sep="\n",
  regexpr=regexpr(pattern, subject, perl=PERL)))
atime.list <- atime::atime(
  N=1:30,
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  expr.list=expr.list)
if(require(ggplot2)){
  best.list <- atime::references_best(atime.list)
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
      coord_cartesian(xlim=c(NA,50))
  }else{
    gg
  }
}

}
