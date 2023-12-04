\name{atime_pkg}
\alias{atime_pkg}
\title{Asymptotic timing package tests}
\description{Computation time and memory for several R expressions of
  several different data sizes, for up to four different package
  versions (base, HEAD, CRAN, merge-base).}
\usage{atime_pkg(pkg.path)}
\arguments{
  \item{pkg.path}{path to package source directory.}
}
\details{There should be a file named pkg.path/inst/atime/tests.R which
  defines \code{test.list}, a list with names corresponding to different
  tests. Each
  element should be a list with at least three named elements: \code{N},
  \code{setup}, \code{expr}, (possibly others such as
  \code{pkg.edit.fun} and \code{version_name="1234567890abcdef"})
  to be passed as named
  arguments to \code{atime_versions}.}
\value{Named list, names come from names of \code{test.list}, and values
  come from results of \code{atime_versions}. Side effect is that
  data/plot files are saved to the inst/atime directory.}

\author{Toby Dylan Hocking}

