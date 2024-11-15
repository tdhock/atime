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
atime_result_string <- atime::atime(
  seconds.limit=0.001,
  N=unique(as.integer(10^seq(0,4,l=100))),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
    linear_size_replacement <- paste(rep("REPLACEMENT", N), collapse="")
  },
  PCRE.match=regexpr(pattern, subject, perl=TRUE),
  TRE.match=regexpr(pattern, subject, perl=FALSE),
  constant.replacement=gsub("a","constant size replacement",subject),
  linear.replacement=gsub("a",linear_size_replacement,subject))
(refs_best_string <- atime::references_best(atime_result_string))
## plot method shows each expr and unit in a separate panel.
## default is to show closest larger and smaller references.
plot(refs_best_string)
## modifying plot.references changes violet references shown by plot.
refs_best_string$plot.references <- refs_best_string$ref[c("N","N^2","N^3","2^N"),on="fun.name"]
plot(refs_best_string)
## predict method computes N for given units (default seconds limit).
(pred_string <- predict(refs_best_string))
plot(pred_string)

## Example 2: dense matrix versus sparse Matrix, additional unit, length.
vec_mat_result <- atime::atime(
  seconds.limit=0.001,
  N=as.integer(10^seq(1,7,by=0.25)),
  vector=numeric(N),
  matrix=matrix(0, N, N),
  Matrix=if(require(Matrix))Matrix(0, N, N),
  result=function(x)data.frame(length=length(x)))
plot(vec_mat_result)
(vec_mat_refs <- atime::references_best(vec_mat_result))
plot(vec_mat_refs)
vec_mat_refs$plot.references <- vec_mat_refs$ref[c("N","N^2"),on="fun.name"]
plot(vec_mat_refs)
(vec_mat_pred <- predict(vec_mat_refs, length=1e3))
plot(vec_mat_pred)

}
