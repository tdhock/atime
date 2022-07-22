atime_versions_install <- function(Package, pkg.path, new.Package.vec, sha.vec, verbose){
  first.lib <- .libPaths()[1]
  pkgs.in.lib <- dir(first.lib)
  new.not.installed <- !new.Package.vec %in% pkgs.in.lib
  if(any(new.not.installed)){
    tdir <- tempfile()
    dir.create(tdir)
    new.path <- file.path(tdir, basename(pkg.path))
    unlink(new.path, recursive=TRUE, force=TRUE)
    file.copy(pkg.path, tdir, recursive=TRUE)
    repo <- git2r::repository(new.path)
    for(new.i in which(new.not.installed)){
      sha <- sha.vec[[new.i]]
      new.Package <- new.Package.vec[[new.i]]
      if(new.Package %in% pkgs.in.lib){
        if(verbose){
          message(sprintf(
            "skipping %s because it already exists in %s",
            new.Package, first.lib))
        }
      }else{
        git2r::checkout(repo, branch=sha, force=TRUE)
        some.files <- c(
          ##Sys.glob(file.path(new.path, "R", "*.R")),
          file.path(new.path, "DESCRIPTION"),
          file.path(new.path, "NAMESPACE"))
        for(f in some.files){
          l.old <- readLines(f)
          l.new <- gsub(Package, new.Package, l.old, fixed=TRUE)
          writeLines(l.new, f)
        }
        if(file.exists(file.path(new.path, "R", "RcppExports.R"))){
          Rcpp::compileAttributes(new.path)
        }
        install.packages(
          new.path, repos=NULL, type="source", verbose=verbose)
      }
    }
  }
}  

atime_versions <- function(pkg.path, N, setup, expr, times=10, seconds.limit=0.01, verbose=FALSE, ...){
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  sha.vec <- mc.args[!names(mc.args) %in% formal.names]
  pkg.DESC <- file.path(pkg.path, "DESCRIPTION")
  DESC.mat <- read.dcf(pkg.DESC)
  Package <- DESC.mat[,"Package"]
  new.Package.vec <- sprintf("%s.%s", Package, sha.vec)
  atime_versions_install(
    Package, pkg.path, new.Package.vec, sha.vec, verbose)
  some.arg.names <- intersect(names(mc.args), names(formals(atime)))
  a.args <- mc.args[some.arg.names]
  for(commit.i in seq_along(sha.vec)){
    sha <- sha.vec[[commit.i]]
    commit.name <- names(sha.vec)[[commit.i]]
    new.Package <- new.Package.vec[[commit.i]]
    old.text <- capture.output(substitute(expr))
    new.text <- gsub(
      paste0(Package,"(:+)"),
      paste0(new.Package,"\\1"),
      old.text)
    a.args[[commit.name]] <- str2lang(new.text)
  }
  do.call(atime, a.args)
}
