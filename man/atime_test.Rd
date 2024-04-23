\name{atime_test}
\alias{atime_test}
\title{
  Define an atime performance test.
}
\description{
  Use this to define an element of your \code{test.list} in
  atime/tests.R, prior to running \code{\link{atime_pkg}}.
}
\usage{
atime_test(...)
}
\arguments{
  \item{\dots}{
    Any arguments for \code{\link{atime_versions}}, will be quoted (not evaluated).
}
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
  setup=data.vec <- rnorm(N),
  expr=binsegRcpp::binseg("mean_norm",data.vec))

}
