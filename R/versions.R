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
        find_replace <- function(glob, FIND, REPLACE){
          some.files <- Sys.glob(file.path(new.path, glob))
          for(f in some.files){
            l.old <- readLines(f)
            l.new <- gsub(FIND, REPLACE, l.old)
            writeLines(l.new, f)
          }
        }
        find_replace(
          "DESCRIPTION", 
          paste0("Package:\\s+", Package),
          paste("Package:", new.Package))
        Package_ <- gsub(".", "_", Package, fixed=TRUE)
        Package_regex <- gsub(".", "_?", Package, fixed=TRUE)
        new.Package_ <- paste0(Package_, "_", sha)
        find_replace(
          "NAMESPACE",
          sprintf('useDynLib\\("?%s"?', Package_regex),
          paste0('useDynLib(', new.Package_))
        ## If there is compiled code then there must be a *.c or *.cpp
        ## file with R_init_data_table which we need to replace with
        ## R_init_data_table_sha.
        R_init_pkg <- paste0("R_init_", Package_regex)
        for(suffix in c("c", "cpp", "cc")){
          find_replace(
            file.path("src", paste0("*.", suffix)),
            R_init_pkg,
            paste0("R_init_", new.Package_))
        }
        find_replace(
          file.path("src","Makevars.in"),
          Package_regex,
          new.Package_)
        find_replace(
          file.path("R", "onLoad.R"),
          Package_regex,
          new.Package_)
        find_replace(
          file.path("R", "onLoad.R"),
          sprintf('packageVersion\\("%s"\\)', Package),
          sprintf('packageVersion\\("%s"\\)', new.Package))
        install.packages(
          new.path, repos=NULL, type="source", verbose=verbose)
      }
    }
  }
}  

atime_versions <- function(pkg.path, N, setup, expr, sha.vec=NULL, times=10, seconds.limit=0.01, verbose=FALSE, results=TRUE, ...){
  ver.args <- list(pkg.path, substitute(expr), sha.vec, verbose, ...)
  ver.exprs <- do.call(atime_versions_exprs, ver.args)
  a.args <- list(N, substitute(setup), ver.exprs, times, seconds.limit, verbose, results)
  do.call(atime, a.args)
}

atime_versions_exprs <- function(pkg.path, expr, sha.vec=NULL, verbose=FALSE, ...){
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.vec <- mc.args[!names(mc.args) %in% formal.names]
  SHA.vec <- c(dots.vec, sha.vec)
  pkg.DESC <- file.path(pkg.path, "DESCRIPTION")
  DESC.mat <- read.dcf(pkg.DESC)
  Package <- DESC.mat[,"Package"]
  new.Package.vec <- sprintf("%s.%s", Package, SHA.vec)
  atime_versions_install(
    Package, pkg.path, new.Package.vec, SHA.vec, verbose)
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
