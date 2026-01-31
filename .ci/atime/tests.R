# Performance test for isoweek() speedup (data.table PR #7144)
# https://github.com/tdhock/atime/issues/102

pkg.edit.fun <- function(old.Package, new.Package, sha, new.pkg.path){
  pkg_find_replace <- function(glob, FIND, REPLACE){
    atime::glob_find_replace(file.path(new.pkg.path, glob), FIND, REPLACE)
  }
  Package_regex <- gsub(".", "_?", old.Package, fixed=TRUE)
  Package_ <- gsub(".", "_", old.Package, fixed=TRUE)
  new.Package_ <- paste0(Package_, "_", sha)
  pkg_find_replace(
    "DESCRIPTION", 
    paste0("Package:\\s+", old.Package),
    paste("Package:", new.Package))
  pkg_find_replace(
    file.path("src", "Makevars.*in"),
    Package_regex,
    new.Package_)
  pkg_find_replace(
    file.path("R", "onLoad.R"),
    Package_regex,
    new.Package_)
  pkg_find_replace(
    file.path("R", "onLoad.R"),
    sprintf('packageVersion\\("%s"\\)', old.Package),
    sprintf('packageVersion\\("%s"\\)', new.Package))
  pkg_find_replace(
    file.path("src", "init.c"),
    paste0("R_init_", Package_regex),
    paste0("R_init_", gsub("[.]", "_", new.Package_)))
  pkg_find_replace(
    "NAMESPACE",
    sprintf('useDynLib\\("?%s"?', Package_regex),
    paste0('useDynLib(', new.Package_))
}

test.list <- atime::atime_test_list(
  pkg.edit.fun=pkg.edit.fun,
  N=unique(as.integer(10^seq(3, 6, length.out=25))),
  times=5,
  seconds.limit=0.25,
  "isoweek improved in data.table #7144"=atime::atime_test(
    setup={
      set.seed(349)
      x <- sample(Sys.Date() - 0:5000, N, replace=TRUE)
    },
    expr=data.table::isoweek(x),
    Slow="548410d23dd74b625e8ea9aeb1a5d2e9dddd2927",  # before optimization
    Fast="c0b32a60466bed0e63420ec105bc75c34590865e"   # with optimization
  )
)
