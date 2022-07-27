\name{atime_versions_exprs}
\alias{atime_versions_exprs}
\title{Create expressions for different git versions}
\description{Install different git commit versions as different
  packages, then create a list of expressions, one for each
  version. For most use cases \code{atime_versions} is simpler, but
  \code{atime_versions_exprs} is more flexible for the case of comparing
  different versions of one expression to another expression.}
\usage{atime_versions_exprs(
pkg.path, expr, sha.vec=NULL,
verbose=FALSE,
pkg.edit.fun=pkg.edit.default, ...)}
\arguments{
  \item{pkg.path}{Path to git repo containing R package.}
  \item{expr}{code with package double-colon prefix, for example
    Package::fun(argA, argB) which will be
    evaluated for each different package version.}
  \item{sha.vec}{named character vector / list of SHA commit IDs.}
  \item{verbose}{logical, print messages after every data size?}
  \item{pkg.edit.fun}{function called to edit package before
    installation, should typically replace instances of PKG with
    PKG.SHA, default works with Rcpp packages.}
  \item{\dots}{named SHA/commit IDs to time. Values passed as branch
    arg to \code{git2r::checkout}, names used to identify/interpret
    this version
    of the code in the output.}
}
\details{First each version is checked out and
  installed (to whatever R library is first on \code{.libPaths()}),
  using the package name Package.SHA. Then an expression is created for
  each version, by replacing the PKG name in colon-prefix with PKG.SHA, 
  \code{atime(name1=Package.SHA1::fun(argA, argB),
    name2=Package.SHA2::fun(argA,argB))}. For convenience, versions can
  be specified either as code (\dots) or data (\code{sha.vec}).}
\value{list of expressions.}

\author{Toby Dylan Hocking}

\examples{

if(interactive() && requireNamespace("changepoint")){
  tdir <- tempfile()
  dir.create(tdir)
  git2r::clone("https://github.com/tdhock/binsegRcpp", tdir)
  expr.list <- atime::atime_versions_exprs(
    pkg.path=tdir,
    expr=binsegRcpp::binseg_normal(data.vec, max.segs),
    cv="908b77c411bc7f4fcbcf53759245e738ae724c3e",
    "rm unord map"="dcd0808f52b0b9858352106cc7852e36d7f5b15d",
    "mvl_construct"="5942af606641428315b0e63c7da331c4cd44c091")
  atime.list <- atime::atime(
    N=2^seq(2, 20),
    setup={
      max.segs <- as.integer(N/2)
      data.vec <- 1:N
    },
    expr.list=expr.list,
    changepoint=changepoint::cpt.mean(
      data.vec, penalty="Manual", pen.value=0, method="BinSeg", Q=max.segs-1))
  plot(atime.list)
}

if(interactive())atime::atime_versions_remove("binsegRcpp")

}
