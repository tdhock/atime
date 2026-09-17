\name{R_CMD_INSTALL}
\alias{R_CMD_INSTALL}
\title{
  Run R CMD INSTALL
}
\description{
  Uses \code{system()} and \code{shQuote()} to run \code{R CMD INSTALL},
  installing the package in \code{libPaths()[1]}.
  Can be used in \code{pkg.edit.fun} passed to \code{\link{atime_pkg}},
  to check if an edited package installs without error.
}
\usage{
R_CMD_INSTALL(new.pkg.path)
}
\arguments{
  \item{new.pkg.path}{path to package}
}
\value{
Status code from \code{system()}, 0 for successful installation, other
values for installation errors.
}
\author{
Toby Dylan Hocking
}
