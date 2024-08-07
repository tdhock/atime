\name{references_best}
\alias{references_best}
\title{Best references}
\description{
  Compute best asymptotic references,
  for all empirical measurements which are
  present (not missing) and increasing with data size.
}

\usage{
references_best(L, fun.list=NULL)
}
\arguments{
  \item{L}{List output from atime.}
  \item{fun.list}{List of asymptotic complexity reference functions,
    default NULL means to use package default.}
}
\value{list of class \code{"references_best"} with elements
  \code{references} (data table of all references),
  \code{plot.references} (data table of references to show using \code{plot}
  method, default is to show closest larger and smaller references),
  \code{measurements} (data table of measurements).
}
\author{Toby Dylan Hocking}

\examples{

## Example 1: polynomial and exponential time string functions.
string.result <- atime::atime(
  N=unique(as.integer(10^seq(0,4,l=100))),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  seconds.limit=0.001,
  PCRE.match=regexpr(pattern, subject, perl=TRUE),
  TRE.match=regexpr(pattern, subject, perl=FALSE),
  constant.replacement=gsub("a","constant size replacement",subject),
  linear.replacement=gsub("a",subject,subject))
(string.best <- atime::references_best(string.result))
## plot method shows each expr and unit in a separate panel.
## default is to show closest larger and smaller references.
plot(string.best)
## modifying plot.references changes violet references shown by plot.
string.best$plot.references <- string.best$ref[c("N","N^2","N^3","2^N"),on="fun.name"]
plot(string.best)
## predict method computes N for given units (default seconds limit).
string.pred = predict(string.best)
plot(string.pred)

}
