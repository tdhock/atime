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

get_repo_info <- function(pkg.path) {
  norm_path <- normalizePath(pkg.path)
  repo <- tryCatch(
    git2r::repository(norm_path, discover = TRUE),
    error = function(e) stop("Not a git repository: ", e$message)
  )
  repo_root <- normalizePath(git2r::workdir(repo))
  pkg_suffix <- sub(paste0(repo_root, "/?"), "", norm_path)
  
  list(
    root = repo_root,
    pkg_suffix = pkg_suffix,
    original_pkg = read.dcf(file.path(norm_path, "DESCRIPTION"))[1, "Package"]
  )
}

create_temp_dir <- function(prefix, verbose) {
  temp_dir <- normalizePath(tempfile(prefix))
  dir.create(temp_dir, showWarnings = FALSE)
  if (verbose) message("Created temp directory: ", temp_dir)
  temp_dir
}

check_installed <- function(path, pkg, verbose) {
  if (dir.exists(path)) {
    if (verbose) message(pkg, " already installed")
    return(TRUE)
  }
  FALSE
}

prepare_source <- function(repo, sha, temp_dir, new_pkg, verbose) {
  clone_dir <- file.path(temp_dir, paste0("clone_", new_pkg))
  unlink(clone_dir, recursive = TRUE, force = TRUE)
  dir.create(clone_dir)

  if (verbose) message("Cloning ", repo$root, " to ", clone_dir)
  repo_clone <- git2r::clone(repo$root, clone_dir, progress = FALSE)
  git2r::checkout(repo_clone, sha, force = TRUE)

  src_dir <- file.path(temp_dir, new_pkg)
  pkg_src <- file.path(clone_dir, repo$pkg_suffix)
  file.copy(list.files(pkg_src, full.names = TRUE), src_dir, recursive = TRUE)
  unlink(clone_dir, recursive = TRUE, force = TRUE)

  src_dir
}

try_edit_package <- function(src_dir, old_pkg, new_pkg, sha, edit_fun, verbose) {
  tryCatch({
    if (verbose) message("Modifying package: ", old_pkg, " -> ", new_pkg)
    edit_fun(
      old.Package = old_pkg,
      new.Package = new_pkg,
      sha = sha,
      new.pkg.path = src_dir
    )
    TRUE
  }, error = function(e) {
    message("Package modification failed: ", e$message)
    FALSE
  })
}

clean_build_artifacts <- function(path) {
  unlink(file.path(path, "src", c("*.o", "*.so", "*.dll")), force = TRUE)
}

install_package <- function(src_dir, target, verbose) {
  clean_build_artifacts(src_dir)
  cmd <- paste(
    shQuote(file.path(Sys.getenv("R_HOME"), "bin", "R")),
    "CMD INSTALL --preclean --no-lock -l",
    shQuote(dirname(target)),
    shQuote(src_dir)
  )
  if (verbose) message("Installing: ", cmd)
  if (system(cmd) != 0) stop("Installation failed")
}

atime_versions_install <- function(
    Package,
    pkg.path,
    new.Package.vec,
    sha.vec,
    verbose,
    pkg.edit.fun = pkg.edit.default,
    installed_cache_root = NULL
) {
  lib_path <- .libPaths()[1]
  repo_info <- get_repo_info(pkg.path)
  cache_root <- setup_cache(installed_cache_root, verbose)
  temp_dir <- create_temp_dir("atime_tmp_src_prep_", verbose)

  for (i in seq_along(new.Package.vec)) {
    new_pkg <- new.Package.vec[i]
    current_sha <- sha.vec[[i]]
    target_path <- file.path(lib_path, new_pkg)
    cache_path <- file.path(cache_root, new_pkg)
    
    if (check_installed(target_path, new_pkg, verbose)) next
    if (try_cache_restore(target_path, cache_path, verbose)) next

    # Build from source
    src_dir <- prepare_source(repo_info, current_sha, temp_dir, new_pkg, verbose)
    on.exit(unlink(src_dir, recursive = TRUE, force = TRUE), add = TRUE, after = FALSE)
    
    modified <- try_edit_package(src_dir, repo_info$original_pkg, new_pkg, current_sha, pkg.edit.fun, verbose)
    if (!modified) next
    
    install_package(src_dir, target_path, verbose)
    update_cache(target_path, cache_path, verbose)
  }
  
  unlink(temp_dir, recursive = TRUE, force = TRUE)
  if (verbose) message("Cleaned up temporary directory: ", temp_dir)
  invisible(NULL)
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
  if(!file.exists(pkg.DESC)){
    stop(sprintf("pkg.path=%s should be path to an R package, but %s does not exist", pkg.path, pkg.DESC))
  }
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

