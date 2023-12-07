\name{atime_pkg}
\alias{atime_pkg}
\title{Asymptotic timing package tests}
\description{Computation time and memory for several R expressions of
  several different data sizes, for up to four different package
  versions (base, HEAD, CRAN, merge-base).}
\usage{atime_pkg(pkg.path)}
\arguments{
  \item{pkg.path}{path to git repository containing R package.}
}
\details{
  There should be a file named pkg.path/inst/atime/tests.R which
  defines \code{test.list}, a list with names corresponding to different
  tests. Each
  element should be a list with at least three named elements: \code{N},
  \code{setup}, \code{expr}, (possibly others such as
  \code{pkg.edit.fun} and \code{version_name="1234567890abcdef"})
  to be passed as named arguments to \code{atime_versions}, along with
  the following versions which are passed using the \code{sha.vec} argument:
  base ref comes from GITHUB_BASE_REF environment variable (default master),
  HEAD ref is the branch that you want to merge into base,
  CRAN is current published version (sha value \code{""}),
  merge-base is most recent common ancestor commit between base and HEAD.
}
\value{Named list, names come from names of \code{test.list}, and values
  come from results of \code{atime_versions}. Side effect is that
  data/plot files are saved to the inst/atime directory.}

\author{Toby Dylan Hocking}

\examples{

if(FALSE){

  tdir <- tempfile()
  dir.create(tdir)
  git2r::clone("https://github.com/tdhock/binsegRcpp", tdir)
  repo <- git2r::repository(tdir)
  git2r::checkout(repo, "another-branch")
  result.list <- atime::atime_pkg(tdir)
  inst.atime <- file.path(tdir, "inst", "atime")
  dir(inst.atime)
  tests.RData <- file.path(inst.atime, "tests.RData")
  (objs <- load(tests.RData))

  atime::atime_versions_remove("binsegRcpp")

}

}
