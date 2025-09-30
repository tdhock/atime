\name{atime_test}
\alias{atime_test}
\title{
  Define an atime performance test
}
\description{
  Use this to define an element of your \code{test.list} in
  atime/tests.R, prior to running \code{\link{atime_pkg}}.
}
\usage{
atime_test(
  N, setup, expr, times, seconds.limit, verbose,
  pkg.edit.fun, result, ...)
}
\arguments{
  \item{N}{numeric vector of data sizes to vary.}
  \item{setup}{
    expression to evaluate for every data size, before timings.
    In contrast to \code{expr},
    no replacement of \code{Package:} is performed.
  }
  \item{expr}{
    code with package double-colon prefix, for example
    \code{PKG::fun(argA, argB)}, where PKG is the name of the
    package specified by \code{pkg.path}. This code will be
    evaluated for each different package version, by replacing
    \code{PKG:} by \code{PKG.SHA:}.
    To run different versions of implicitly-called functions
    like \code{DT[i,j]}, you need
    to call them explicitly, as in 
    \code{data.table:::`[.data.table`(DT,i,j)}.
  }
  \item{times}{number of times to evaluate each timed expression.}
  \item{seconds.limit}{if the median timing of any expression exceeds
    this many seconds, then no timings for larger N are computed.}
  \item{verbose}{logical, print messages after every data size?}
  \item{pkg.edit.fun}{function called to edit package before
    installation, should typically replace instances of PKG with
    PKG.SHA, default works with Rcpp packages.}
  \item{result}{logical, save results? (default FALSE)}
  \item{\dots}{named versions.}
}
\value{
  List of expressions.
}
\author{
  Toby Dylan Hocking
}
\seealso{
  \code{\link{atime_test_list}} for defining common arguments in each
  element of the test list,
  \code{\link{atime_pkg}} for running tests.
}
\examples{

atime::atime_test(
  N=c(1,10),
  setup=data.vec <- rnorm(N),
  expr=binsegRcpp::binseg("mean_norm",data.vec))

## https://github.com/tdhock/binsegRcpp/blob/atime-test-funs/.ci/atime/tests.R
## has a real example, see how to run it in tests/testthat/test-versions.R

}
