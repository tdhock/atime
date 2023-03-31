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

## Example 1: polynomial and exponential time string functions.
string.result <- atime::atime(
  N=unique(as.integer(10^seq(0,3.5,l=100))),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  PCRE.match=regexpr(pattern, subject, perl=TRUE),
  TRE.match=regexpr(pattern, subject, perl=FALSE),
  constant.replacement=gsub("a","constant size replacement",subject),
  linear.replacement=gsub("a",subject,subject))
plot(string.result)

## Example 2: split data table vs frame, constant factor difference.
library(data.table)
split.result <- atime::atime(
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
plot(split.result)

}
