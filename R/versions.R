glob_find_replace <- function(glob, FIND, REPLACE){
  some.files <- Sys.glob(glob)
  for(f in some.files){
    l.old <- readLines(f)
    l.new <- gsub(FIND, REPLACE, l.old)
    writeLines(l.new, f)
  }
}

pkg.edit.default <- function(old.Package, new.Package, sha, new.pkg.path){
  pkg_find_replace <- function(glob, FIND, REPLACE){
    glob_find_replace(file.path(new.pkg.path, glob), FIND, REPLACE)
  }
  pkg_find_replace(
    "DESCRIPTION", 
    paste0("Package:\\s+", old.Package),
    paste("Package:", new.Package))
  Package_ <- gsub(".", "_", old.Package, fixed=TRUE)
  R_init_pkg <- paste0("R_init_", Package_)
  new.Package_ <- paste0(Package_, "_", sha)
  pkg_find_replace(
    file.path("src", "RcppExports.cpp"),
    R_init_pkg,
    paste0("R_init_", new.Package_))
  pkg_find_replace(
    "NAMESPACE",
    sprintf('useDynLib\\("?%s"?', Package_),
    paste0('useDynLib(', new.Package))
}

atime_versions_remove <- function(Package){
  lib <- .libPaths()[1]
  pkg.in.lib <- file.path(lib, Package)
  pkg.sha.glob <- paste0(pkg.in.lib, ".*")
  unlink(pkg.sha.glob, recursive=TRUE, force=TRUE)
}

atime_versions_install <- function(Package, pkg.path, new.Package.vec, sha.vec, verbose, pkg.edit.fun=pkg.edit.default){
  first.lib <- .libPaths()[1]
  DESC.in.lib <- Sys.glob(file.path(first.lib, "*", "DESCRIPTION"))
  pkgs.in.lib <- basename(dirname(DESC.in.lib))
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
        ## before editing and installing, make sure directory has sha
        ## suffix, for windows checks.
        sha.path <- paste0(new.path,".",sha)
        file.rename(new.path, sha.path)
        grep_glob <- function(glob, pattern){
          some.files <- Sys.glob(file.path(sha.path, glob))
          out <- list()
          for(f in some.files){
            line.vec <- readLines(f)
            match.vec <- grep(pattern, line.vec, value=TRUE)
            if(length(match.vec)){
              out[[f]] <- match.vec
            }
          }
          out
        }
        print_pkg_info <- function(){
          if(verbose){
            cat("\nPackage info after editing and installation:\n")
            out <- c(
              grep_glob("DESCRIPTION", "^Package"),
              grep_glob("NAMESPACE", "^useDynLib"),
              grep_glob(file.path("src", "*.c"), "R_init_"),
              grep_glob(file.path("src", "*.cpp"), "R_init_"))
            src.files <- dir(file.path(sha.path, "src"))
            out[["src/*.so|dll"]] <- grep("(so|dll)$", src.files, value=TRUE)
            print(out)
            cat("\n")
          }
        }
        unlink(file.path(sha.path, "src", "*.o"))
        pkg.edit.fun(
          old.Package=Package, 
          new.Package=new.Package,
          sha=sha, 
          new.pkg.path=sha.path)
        install.packages(
          sha.path, repos=NULL, type="source", verbose=verbose)
        print_pkg_info()
        file.rename(sha.path, new.path)
      }
    }
  }
}  

atime_versions <- function(pkg.path, N, setup, expr, sha.vec=NULL, times=10, seconds.limit=0.01, verbose=FALSE, pkg.edit.fun=pkg.edit.default, results=TRUE, ...){
  ver.args <- list(
    pkg.path, substitute(expr), sha.vec, verbose, pkg.edit.fun, ...)
  ver.exprs <- do.call(atime_versions_exprs, ver.args)
  a.args <- list(
    N, substitute(setup), ver.exprs, times, seconds.limit, verbose, results)
  do.call(atime, a.args)
}

atime_versions_exprs <- function(pkg.path, expr, sha.vec=NULL, verbose=FALSE, pkg.edit.fun=pkg.edit.default, ...){
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.vec <- mc.args[!names(mc.args) %in% formal.names]
  SHA.vec <- c(dots.vec, sha.vec)
  pkg.DESC <- file.path(pkg.path, "DESCRIPTION")
  DESC.mat <- read.dcf(pkg.DESC)
  Package <- DESC.mat[,"Package"]
  new.Package.vec <- sprintf("%s.%s", Package, SHA.vec)
  atime_versions_install(
    Package, pkg.path, new.Package.vec, SHA.vec, verbose, pkg.edit.fun)
  a.args <- list()
  for(commit.i in seq_along(SHA.vec)){
    sha <- SHA.vec[[commit.i]]
    commit.name <- names(SHA.vec)[[commit.i]]
    new.Package <- new.Package.vec[[commit.i]]
    old.lines <- capture.output(substitute(expr))
    new.lines <- gsub(
      paste0(Package,"(:+)"),
      paste0(new.Package,"\\1"),
      old.lines)
    a.args[[commit.name]] <- str2lang(paste(new.lines, collapse="\n"))
  }
  a.args
}
