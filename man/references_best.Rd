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

## Example 2: combine using rbind inside or outside for loop.
atime_result_rbind <- atime::atime(
  seconds.limit=0.001,
  setup={
    DF <- data.frame(i=1:100)
  },
  inside={
    big.frame <- data.frame()
    for(table.i in 1:N){
      big.frame <- rbind(big.frame, DF)
    }
  },
  outside={
    big.frame.list <- list()
    for(table.i in 1:N){
      big.frame.list[[table.i]] <- DF
    }
    big.frame <- do.call(rbind, big.frame.list)
  }
)
(refs_best_rbind <- atime::references_best(atime_result_rbind))
plot(refs_best_rbind)
refs_best_rbind$plot.references <- refs_best_rbind$ref[c("N","N^2"),on="fun.name"]
plot(refs_best_rbind)
(pred_rbind <- predict(refs_best_rbind))
plot(pred_rbind)

}
