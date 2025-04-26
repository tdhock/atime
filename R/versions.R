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
  code <- unlink(pkg.sha.glob, recursive=TRUE, force=TRUE)
  paths.after <- Sys.glob(pkg.sha.glob)
  pkgs.after <- basename(paths.after)
  if(length(pkgs.after)){
    warning("packages were not removed, probably because they are currently loaded (fix by restarting R): ", paste(pkgs.after, collapse=", "))
  }
  code
}

atime_versions_install <- function(Package, pkg.path, new.Package.vec, sha.vec, verbose, pkg.edit.fun=pkg.edit.default){
  first.lib <- .libPaths()[1]
  DESC.in.lib <- Sys.glob(file.path(first.lib, "*", "DESCRIPTION"))
  pkgs.in.lib <- basename(dirname(DESC.in.lib))
  new.not.installed <- !new.Package.vec %in% pkgs.in.lib
  if(any(new.not.installed)){
    tdir <- tempfile()
    dir.create(tdir)
    ## pkg.path may be path/to/repo/pkg
    orig.repo <- git2r::repository(pkg.path)
    ## path/to/repo root without trailing /.git
    orig.repo.path <- dirname(orig.repo$path)
    ## /pkg
    pkg.suffix.in.repo <- sub(orig.repo.path, "", normalizePath(pkg.path), fixed=TRUE)
    for(new.i in which(new.not.installed)){
      sha <- sha.vec[[new.i]]
      new.Package <- new.Package.vec[[new.i]]
      if(new.Package %in% pkgs.in.lib){
        if(verbose){
          message(sprintf(
            "not installing %s because it already exists in %s",
            new.Package, first.lib))
        }
      }else if(sha == ""){
        install.packages(Package)
      }else{
        new.repo.path <- file.path(tdir, new.Package)
        unlink(new.repo.path, recursive=TRUE, force=TRUE)
        repo <- git2r::clone(orig.repo.path, new.repo.path, progress=FALSE)
        new.pkg.path <- paste0(new.repo.path, pkg.suffix.in.repo)
        tryCatch(
          git2r::checkout(repo, branch=sha, force=TRUE),
          error=function(e)stop(
            e, " when trying to checkout ", sha))
        ## before editing and installing, make sure directory has sha
        ## suffix, for windows checks.
        unlink(file.path(new.pkg.path, "src", "*.o"))
        pkg.edit.fun(
          old.Package=Package, 
          new.Package=new.Package,
          sha=sha, 
          new.pkg.path=new.pkg.path)
        INSTALL.cmd <- paste(
          shQuote(file.path(
            Sys.getenv("R_HOME"),
            "bin",
            "R")),
          'CMD INSTALL -l',
          shQuote(.libPaths()[1]),
          new.pkg.path)
        status.int <- system(INSTALL.cmd)
        if(status.int != 0){
          stop(INSTALL.cmd, " returned error status code ", status.int)
        }
        if(verbose){
          cat("\nPackage info after editing and installation:\n")
          grep_glob <- function(glob, pattern){
            some.files <- Sys.glob(file.path(new.pkg.path, glob))
            out <- list()
            for(f in some.files){
              line.vec <- readLines(f)
              match.vec <- grep(pattern, line.vec, value=TRUE)
              if(length(match.vec)){
                out[[f]] <- match.vec
              }
            }
            out
          }#grep_glob
          out <- c(
            grep_glob("DESCRIPTION", "^Package"),
            grep_glob("NAMESPACE", "^useDynLib"),
            grep_glob(file.path("src", "*.c"), "R_init_"),
            grep_glob(file.path("src", "*.cpp"), "R_init_"))
          src.files <- dir(file.path(new.pkg.path, "src"))
          out[["src/*.so|dll"]] <- grep("(so|dll)$", src.files, value=TRUE)
          print(out)
          cat("\n")
        }#if(verbose)
      }#if(new package not in lib)
    }#for(new.i
  }#any to install
}

atime_versions <- function(pkg.path, N=default_N(), setup, expr, sha.vec=NULL, times=10, seconds.limit=0.01, verbose=FALSE, pkg.edit.fun=pkg.edit.default, result=FALSE, N.env.parent=NULL, ...){
  ver.args <- list(
    pkg.path, substitute(expr), sha.vec, verbose, pkg.edit.fun, ...)
  install.seconds <- system.time({
    ver.exprs <- do.call(atime_versions_exprs, ver.args)
  })[["elapsed"]]
  a.args <- list(
    N, substitute(setup), ver.exprs, times, seconds.limit, verbose, result, N.env.parent)
  bench.seconds <- system.time({
    out.list <- do.call(atime, a.args)
  })[["elapsed"]]
  out.list$install.seconds <- install.seconds
  out.list$bench.seconds <- bench.seconds
  out.list
}

get_sha_vec <- function(sha.vec, dots.vec){
  SHA.vec <- as.list(c(dots.vec, sha.vec))
  if(length(SHA.vec)==0){
    stop("need to specify at least one git SHA, in either sha.vec, or ...")
  }
  if(is.null(names(SHA.vec)) || any(names(SHA.vec)=="")){
    stop("each ... argument and sha.vec element must be named")
  }
  is.problem <- !sapply(SHA.vec, function(x){
    is.character(x) && length(x)==1 && !is.na(x)
  })
  if(any(is.problem)){
    stop("each ... argument value and sha.vec element must be a string (package version, length=1, not NA), problems: ", paste(names(SHA.vec[is.problem]), collapse=", "))
  }
  SHA.vec
}  

atime_versions_exprs <- function(pkg.path, expr, sha.vec=NULL, verbose=FALSE, pkg.edit.fun=pkg.edit.default, ...){
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.vec <- mc.args[!names(mc.args) %in% formal.names]
  SHA.vec <- get_sha_vec(sha.vec, dots.vec)
  pkg.DESC <- file.path(pkg.path, "DESCRIPTION")
  DESC.mat <- read.dcf(pkg.DESC)
  Package <- DESC.mat[,"Package"]
  new.Package.vec <- paste0(
    Package, 
    ifelse(SHA.vec=="", "", "."), 
    SHA.vec)
  a.args <- list()
  for(commit.i in seq_along(SHA.vec)){
    sha <- SHA.vec[[commit.i]]
    commit.name <- names(SHA.vec)[[commit.i]]
    new.Package <- new.Package.vec[[commit.i]]
    old.lines <- capture.output(substitute(expr))
    new.lines <- gsub(
      paste0(Package,"::"),
      paste0(new.Package,"::"),
      old.lines,
      fixed=TRUE)
    if(Package!=new.Package && identical(old.lines,new.lines)){
      stop(sprintf("expr should contain at least one instance of %s:: to replace with %s::", Package, new.Package))
    }
    a.args[[commit.name]] <- str2lang(paste(new.lines, collapse="\n"))
    atime_versions_install(
      Package, normalizePath(pkg.path),
      new.Package.vec, SHA.vec, verbose, pkg.edit.fun)
  }
  a.args
}

