\name{atime}
\alias{atime}
\title{Asymptotic timing}
\description{Computation time and memory for several R expressions of
  several different data sizes.}
\usage{atime(
N, setup, expr.list=NULL, times=10, seconds.limit=0.01, verbose=FALSE,
result=FALSE, ...)}
\arguments{
  \item{N}{numeric vector of at least two data sizes, default is \code{2^seq(2,20)}.}
  \item{setup}{expression to evaluate for every data size, before timings.}
  \item{expr.list}{named list of expressions to time.}
  \item{times}{number of times to evaluate each timed expression.}
  \item{seconds.limit}{if the median timing of any expression exceeds
    this many seconds, then no timings for larger N are computed.}
  \item{verbose}{logical, print messages after every data size?}
  \item{result}{logical, save each result? If \code{TRUE}, and result is
  a data frame with one row, then the numeric column names will be
  saved, for use as the default \code{more.units} argument to
  \code{\link{references_best}}.}
  \item{\dots}{named expressions to time.}
}
\details{Each iteration involves first computing the setup expression,
  and then computing several times the \dots expressions. For
  convenience, expressions may be specified either via code (\dots) or
  data (\code{expr.list} arg).}
\value{list of class atime with elements \code{more.units} (character
  vector of unit column names), \code{seconds.limit} (numeric
  input param), \code{measurements} (data table of results).}

\author{Toby Dylan Hocking}

\examples{

## Example 1: polynomial and exponential time string functions.
string.result <- atime::atime(
  N=unique(as.integer(10^seq(0,3.5,l=100))),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  seconds.limit=0.001,
  PCRE.match=regexpr(pattern, subject, perl=TRUE),
  TRE.match=regexpr(pattern, subject, perl=FALSE),
  constant.replacement=gsub("a","constant size replacement",subject),
  linear.replacement=gsub("a",subject,subject))
plot(string.result)

## Example 2: split data table vs frame, constant factor difference.
library(data.table)
split.result <- atime::atime(
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
  seconds.limit=0.001,
  frame=split(DF[names(DF) != "x1"], DF["x1"], drop = TRUE),
  table=split(DT, by = "x1", keep.by = FALSE, drop = TRUE)
)
plot(split.result)

}
