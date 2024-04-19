\name{atime_pkg}
\alias{atime_pkg}
\title{Asymptotic timing package tests}
\description{Computation time and memory for several R expressions of
  several different data sizes, for several package
  versions (base, HEAD, CRAN, merge-base, others specified by user).}
\usage{atime_pkg(pkg.path=".", tests.dir="inst")}
\arguments{
  \item{pkg.path}{path to git repository containing R package.}
  \item{tests.dir}{path to directory which contains atime/tests.R,
    relative to \code{pkg.path} (default \code{"inst"}).}
}
\details{
  There should be a \code{tests.R} code file which
  defines \code{test.list}, a list with names corresponding to different
  tests.
  Each element should be a list with at least three named elements: \code{N},
  \code{setup}, \code{expr}, (possibly others such as
  \code{pkg.edit.fun} and \code{version_name="1234567890abcdef"})
  to be passed as named arguments to \code{atime_versions}, along with
  the following versions which are passed using the \code{sha.vec} argument:
  base ref comes from GITHUB_BASE_REF environment variable (default master),
  HEAD ref is the branch that you want to merge into base,
  CRAN is current published version (sha value \code{""}),
  merge-base is most recent common ancestor commit between base and
  HEAD.
  The \code{tests.R} code file can define \code{version.colors} which should be
  a character vector (names for versions, values for colors; names/versions are
  HEAD, base, CRAN, merge-base, and any others you may define such as
  Before, Regression, Slow, Fixed, Fast).
}
\value{Named list, names come from names of \code{test.list}, and values
  come from results of \code{atime_versions}. Side effect is that
  data/plot files are saved in \code{atime} directory, including
  tests.RData (test results which can be read into R if you want to make
  your own alternative plots/analyses), and tests_all_facet.png (plot
  summarizing all test results).
  }

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
