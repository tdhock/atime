\name{bisect}
\alias{bisect}
\alias{bisect_run}
\alias{bisect_try}
\title{
  git bisect for performance testing
}
\description{
  Use git bisect to identify a commit causing a change in
  throughput at the time limit of an atime performance test.
}
\usage{
bisect(pkg.path, Test, old.name = "Fast", new.name = "Slow")
bisect_run(test.path)
bisect_try(test.path)
}
\arguments{
  \item{pkg.path}{Path to R package with atime performance tests.}
  \item{Test}{Name of performance test to use in bisect.}
  \item{old.name}{Old commit name in R.}
  \item{new.name}{New commit name in R.}
  \item{test.path}{Path to directory created by \code{bisect()},
    containing data about the test to run in files
    \code{tests.R} and \code{Test_commits.csv}.}
}
\value{
  \code{bisect()} returns NULL, prints git bisect results, and saves
  result files in the \code{atime/bisect} directory.
  \code{bisect_run()} is not usually called directly by a user (instead
  it is called from \code{git bisect run}); it exits R with a status
  code (0=old, 1=new, 125=skip).
  \code{bisect_try()} is not usually called directly by a user (instead
  it is called from \code{bisect_run()} but in another R process, in
  order to isolate environment modifications: data.table test versions
  may break regular data.table which we use to compute results);
  it runs the test in
  \code{tryCatch}, saving results or error code 125 to an RDS file.
}
\references{
  Detecting, fixing, and preventing asymptotic performance regressions
  (research paper in progress as of Oct 2026).
}
\author{
  Toby Dylan Hocking
}
