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
  ...,
  name.value.sep="=",
  expr.param.sep=" ",
  collapse = ",", 
  symbol.params=character())
}
\arguments{
  \item{param.list}{
    Named list of items to replace in ... expressions, default empty
    list means nothing to replace.
  }
  \item{\dots}{
    Named expressions which each must contain each name of
    \code{param.list}.
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
  \item{symbol.params}{
    character vector: these elements of \code{param.list} will be converted to symbols before substitution.
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
  N=unique(as.integer(10^seq(0,3.5,l=20))),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  expr.list=expr.list)
plot(atime.list)

## Example 2: symbol.params arg.
sub.param.list <- list(FUN=c("sub","gsub"), PERL=c(TRUE,FALSE))
## with base R we can use as.symbol and substitute:
sapply(sub.param.list$FUN,function(name)substitute(fun("a","",subject), list(fun=as.symbol(name))))
## with atime_grid the analog is to use symbol.params argument:
(sub.expr.list <- atime::atime_grid(
  sub.param.list,
  replace=FUN("a","",subject,perl=PERL),
  symbol.params="FUN"))
sub.atime.list <- atime::atime(
  setup={
    subject <- paste(rep("a",N),collapse="")
  }, 
  expr.list=sub.expr.list)
plot(sub.atime.list)

## Customize ggplot, which shows expr.name column as direct labels.
sub.atime.edited <- sub.atime.list
sub.atime.edited$measurements <- data.table(sub.atime.list$measurements)[
, expr.name := paste0("PERL=",PERL)]
library(ggplot2)
plot(sub.atime.edited)+
  facet_grid(unit ~ FUN, labeller=label_both)

}
