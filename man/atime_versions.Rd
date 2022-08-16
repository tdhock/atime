\name{atime_versions}
\alias{atime_versions}
\title{Asymptotic timing of git versions}
\description{Computation time and memory for a single R expression
  evaluated using several different git versions.}
\usage{atime_versions(
pkg.path, N, setup, expr, sha.vec=NULL,
times=10, seconds.limit=0.01, verbose=FALSE,
pkg.edit.fun=pkg.edit.default, results=TRUE,
...)}
\arguments{
  \item{pkg.path}{Path to git repo containing R package.}
  \item{N}{numeric vector of data sizes to vary.}
  \item{setup}{expression to evaluate for every data size, before timings.}
  \item{expr}{code with package double-colon prefix, for example
    Package::fun(argA, argB) which will be
    evaluated for each different package version.}
  \item{sha.vec}{named character vector / list of SHA commit IDs.}
  \item{times}{number of times to evaluate each timed expression.}
  \item{seconds.limit}{if the median timing of any expression exceeds
    this many seconds, then no timings for larger N are computed.}
  \item{verbose}{logical, print messages after every data size?}
  \item{pkg.edit.fun}{function called to edit package before
    installation, should typically replace instances of PKG with
    PKG.SHA, default works with Rcpp packages.}
  \item{results}{logical, save results?}
  \item{\dots}{named SHA/commit IDs to time. Values passed as branch
    arg to \code{git2r::checkout}, names used to identify/interpret
    this version
    of the code in the output.}
}
\details{First each version specified by \dots is checked out and
  installed (to whatever R library is first on \code{.libPaths()}),
  using the package name Package.SHA. Then the atime function is called
  with arguments defined by the different SHA arguments,
  \code{atime(name1=Package.SHA1::fun(argA, argB),
    name2=Package.SHA2::fun(argA,argB))}.}
\value{list of class atime with elements \code{seconds.limit} (numeric
  input param), \code{timings} (data table of results).}

\author{Toby Dylan Hocking}

\examples{

if(interactive()){
  tdir <- tempfile()
  dir.create(tdir)
  git2r::clone("https://github.com/tdhock/binsegRcpp", tdir)
  atime.list <- if(requireNamespace("callr")){
    atime.fun <- function(TDIR){
      atime::atime_versions(
        pkg.path=TDIR,
        N=2^seq(2, 20),
        setup={
          max.segs <- as.integer(N/2)
          data.vec <- 1:N
        },
        expr=binsegRcpp::binseg_normal(data.vec, max.segs),
        cv="908b77c411bc7f4fcbcf53759245e738ae724c3e",
        "rm unord map"="dcd0808f52b0b9858352106cc7852e36d7f5b15d",
        "mvl_construct"="5942af606641428315b0e63c7da331c4cd44c091")      
    }
    callr::r(atime.fun, list(tdir))
  }else{
    atime::atime_versions(
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
  }
  plot(atime.list)
}

if(interactive())atime::atime_versions_remove("binsegRcpp")

}
