\name{atime_test_list}
\alias{atime_test_list}
\title{
  Define an atime performance test list.
}
\description{
  Use this to define \code{test.list} in your atime/tests.R
  file, prior to running \code{\link{atime_pkg}}.
  Arguments in \dots and \code{tests} are combined to form the test
  list, and other arguments are copied to each element of the test list.
}
\usage{
atime_test_list(
  ...,
  N, setup, expr, times, seconds.limit,
  verbose, pkg.edit.fun, result,
  tests = NULL)
}
\arguments{
  \item{\dots}{
    names for tests, values are lists of arguments to pass to
    \code{\link{atime_versions}} (combined with \code{tests}).
}
  \item{tests}{
    list of tests, with names for tests, values are lists of arguments to pass to
    \code{\link{atime_versions}} (combined with \dots).
}
  \item{N}{
    integer vector of data sizes.
}
  \item{setup}{
    expression that depends on \code{N}, to run before timings.
}
  \item{expr}{
    expression to time.
}
  \item{times}{
    number of times to run \code{expr}.
}
  \item{seconds.limit}{
    number of seconds after which we stop trying larger \code{N}.
}
  \item{verbose}{
    logical: print output?
}
  \item{pkg.edit.fun}{
    function for editing package prior to testing.
}
  \item{result}{
    logical: save results?
}
}
\value{
  List representing performance tests, each element is a list of
  arguments to pass to \code{\link{atime_versions}}.
}
\author{
  Toby Dylan Hocking
}
\seealso{
  \code{\link{atime_test}} for defining each test,
  \code{\link{atime_pkg}} for running tests.
}
\examples{

test.list <- atime::atime_test_list(
  N=as.integer(10^seq(1,3,by=0.5)),
  setup={
    set.seed(1)
    data.vec <- rnorm(N)
  },
  mean_norm=atime::atime_test(expr=binsegRcpp::binseg("mean_norm",data.vec)),
  poisson=atime::atime_test(expr=binsegRcpp::binseg("poisson",data.vec)),
  NULL)

}
