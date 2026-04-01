\name{issue107}
\alias{issue107}
\docType{data}
\title{
  atime result for issue 107
}
\description{
  atime was run on windows, to test a speed and memory increase.
  Memory increases linearly, but the predict method did not return the
  correct N value for kilobytes=1e4.
}
\usage{data("issue107")}
\format{List from running atime.}
\references{
  https://github.com/Rdatatable/data.table/issues/7687
}
\examples{

data(issue107, package="atime")
refs <- atime::references_best(issue107)
pred <- predict(refs, kilobytes=1e4)
pred$pred
if(requireNamespace("directlabels"))plot(pred)

}
\keyword{datasets}
