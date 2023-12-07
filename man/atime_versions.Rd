\name{atime_versions}
\alias{atime_versions}
\title{Asymptotic timing of git versions}
\description{Computation time and memory for a single R expression
  evaluated using several different git versions.}
\usage{
atime_versions(
 pkg.path, N, setup, expr, sha.vec=NULL,
 times=10, seconds.limit=0.01, verbose=FALSE,
 pkg.edit.fun=pkg.edit.default, result=FALSE,
 ...)
}
\arguments{
  \item{pkg.path}{Path to git repo containing R package.}
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
  \item{sha.vec}{named character vector / list of versions.}
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
\details{
  For convenience, versions can
  be specified either as code (\dots), data (\code{sha.vec}), or both.
  Each version should be either \code{""} (to use currently installed
  version of package, or if missing, install most recent
  version from CRAN) or a SHA1 hash, which is passed as branch
  arg to \code{git2r::checkout}; version names used to identify/interpret
  the output/plots.
}
\value{list of class atime with elements \code{seconds.limit} (numeric
  input param), \code{timings} (data table of results).}
\seealso{
  \code{\link{atime_versions_exprs}} 
  converts \code{expr} into a list of expressions, one for each version,
  passed to \code{\link{atime}} as the \code{expr.list} argument.
}

\author{Toby Dylan Hocking}

\examples{

if(FALSE){

  tdir <- tempfile()
  dir.create(tdir)
  git2r::clone("https://github.com/tdhock/binsegRcpp", tdir)
  atime.list <- atime::atime_versions(
    pkg.path=tdir,
    N=2^seq(2, 20),
    setup={
      max.segs <- as.integer(N/2)
      data.vec <- 1:N
    },
    expr=binsegRcpp::binseg_normal(data.vec, max.segs),
    cv="908b77c411bc7f4fcbcf53759245e738ae724c3e",
    "rm unord map"="dcd0808f52b0b9858352106cc7852e36d7f5b15d",
    "mvl_construct"="5942af606641428315b0e63c7da331c4cd44c091")      
  plot(atime.list)

  atime::atime_versions_remove("binsegRcpp")

}

}
