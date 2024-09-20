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
  \item{verbose}{logical, print messages after every data size?}
  \item{pkg.edit.fun}{function called to edit package before
    installation, should typically replace instances of \code{PKG} with
    \code{PKG.SHA}, default works with Rcpp packages, but does not work
    with all packages. For an example of a custom package editing
    function, see the atime vignette about data.table.}
  \item{\dots}{named versions.}
}
\details{
  For convenience, versions can
  be specified either as code (\dots), data (\code{sha.vec}), or both.
  Each version should be either \code{""} (to install most recent
  version from CRAN) or a SHA1 hash, which is passed as branch
  arg to \code{git2r::checkout}; version names used to identify/interpret
  the output/plots.
  Each version is installed as a separate package
  (to whatever R library is first on \code{.libPaths()}),
  using the package name \code{PKG.SHA}.
}
\value{
  A list of expressions, one for
  each version, created by replacing \code{PKG:}
  in \code{expr} with \code{PKG.SHA:}, 
  \code{atime(name1=Package.SHA1::fun(argA, argB),
    name2=Package.SHA2::fun(argA,argB))}.
}

\author{Toby Dylan Hocking}

\examples{

if(FALSE){

  if(requireNamespace("changepoint")){
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
        data.vec, penalty="Manual", pen.value=0, method="BinSeg",
        Q=max.segs-1))
    plot(atime.list)
  }

  atime::atime_versions_remove("binsegRcpp")

}

}
