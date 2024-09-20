find_tests_file <- function(pkg.path, tests.dir){
  stopifnot(is.character(tests.dir))
  checked <- list()
  for(one.test.dir in tests.dir){
    tests.R <- file.path(pkg.path, one.test.dir, "atime", "tests.R")
    if(file.exists(tests.R))return(tests.R)
    checked[[one.test.dir]] <- tests.R
  }
  stop("could not find tests.R file after checking ", paste(checked, collapse=", "))
}

atime_pkg_sha_colors <- function(pkg.path, color.vec){
  pkg.DESC <- file.path(pkg.path, "DESCRIPTION")
  DESC.mat <- read.dcf(pkg.DESC)
  Package <- DESC.mat[,"Package"]
  ap <- utils::available.packages()
  repo <- git2r::repository(pkg.path)
  HEAD.commit <- git2r::revparse_single(repo, "HEAD")
  sha.vec <- c()
  HEAD.name <- paste0("HEAD=",git2r::repository_head(repo)$name)
  sha.vec[[HEAD.name]] <- git2r::sha(HEAD.commit)
  CRAN.name <- paste0("CRAN=",ap[Package,"Version"])
  if(Package %in% rownames(ap)){
    sha.vec[[CRAN.name]] <- ""
  }
  base.ref <- Sys.getenv("GITHUB_BASE_REF", "master")
  base.commit <- tryCatch({
    git2r::revparse_single(repo, base.ref)
  }, error=function(e){
    NULL
  })
  base.name <- paste0("base=",base.ref)
  if(git2r::is_commit(base.commit)){
    add_if_new <- function(name, commit.obj){
      sha <- git2r::sha(commit.obj)
      if(!sha %in% sha.vec){
        sha.vec[[name]] <<- sha
      }
    }
    add_if_new(base.name, base.commit)
    mb.commit <- git2r::merge_base(HEAD.commit, base.commit)
    add_if_new("merge-base", mb.commit)
  }
  abbrev2name <- c(
    HEAD=HEAD.name,
    base=base.name,
    CRAN=CRAN.name)
  names(color.vec) <- ifelse(
    names(color.vec) %in% names(abbrev2name),
    abbrev2name[names(color.vec)],
    names(color.vec))
  list(sha.vec=sha.vec, version.colors=color.vec)
}  

atime_pkg_test_info <- function(pkg.path, tests.dir=NULL){
  if(is.null(tests.dir)){
    tests.dir <- c("inst",".ci")
  }
  tests.R <- find_tests_file(pkg.path, tests.dir)
  test.env <- test_file_to_env(tests.R)
  default.list <- list(
    width.in=4,
    height.in=8,
    expand.prop=0.5,
    version.colors=c(#RColorBrewer::brewer.pal(7, "Dark2")
      HEAD="#1B9E77",
      base="#D95F02",
      "merge-base"="#7570B3",
      CRAN="#E7298A",
      Before="#66A61E",
      Regression="#E6AB02", Slow="#E6AB02",
      Fixed="#A6761D", Fast="#A6761D"
    ))
  for(var.name in names(default.list)){
    if(is.null(test.env[[var.name]])){
      test.env[[var.name]] <- default.list[[var.name]]
    }
  }
  sha_colors <- atime_pkg_sha_colors(pkg.path, test.env$version.colors)
  pkg.sha.args <- list(
    pkg.path=pkg.path,
    sha.vec=sha_colors$sha.vec)
  test.list <- inherit_args(test.env$test.list, pkg.sha.args)
  ## goal is to return a list of tests to run via do.call(atime::atime_versions, test.list[["name"]])
}

test_file_to_env <- function(tests.R){
  test.env <- new.env()
  tests.parsed <- parse(tests.R)
  eval(tests.parsed, test.env)
  test.env
}

atime_test <- function(...){
  as.list(match.call()[-1])
}

atime_test_list <- function(..., N, setup, expr, times, seconds.limit, verbose, pkg.edit.fun, result, tests=NULL){
  could.copy <- intersect(names(formals(atime_versions)),names(formals()))
  mc <- as.list(match.call()[-1])
  common.names <- intersect(names(mc), could.copy)
  possible.uneval <- c("setup","expr")
  uneval.names <- intersect(common.names, possible.uneval)
  eval.names <- setdiff(common.names, possible.uneval)
  common.args <- mget(eval.names)
  common.args[uneval.names] <- mc[uneval.names]
  L <- c(tests, list(...))
  inherit_args(L, common.args)
}

inherit_args <- function(L, common.args){
  out <- list()
  for(L.i in seq_along(L)){
    test.args <- L[[L.i]]
    test.name <- names(L)[[L.i]]
    if(!is.null(test.args)){
      out.args <- common.args
      out.args[names(test.args)] <- test.args
      out[[test.name]] <- out.args
    }
  }
  out
}
