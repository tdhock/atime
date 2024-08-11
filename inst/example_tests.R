edit.data.table <- function(old.Package, new.Package, sha, new.pkg.path){
  pkg_find_replace <- function(glob, FIND, REPLACE){
    atime::glob_find_replace(file.path(new.pkg.path, glob), FIND, REPLACE)
  }
  Package_regex <- gsub(".", "_?", old.Package, fixed=TRUE)#data_?table
  ## old.Package = data.table
  Package_ <- gsub(".", "_", old.Package, fixed=TRUE)#data_table
  new.Package_ <- paste0(Package_, "_", sha)#data_table_ddb345a
  pkg_find_replace(
    "DESCRIPTION", 
    paste0("Package:\\s+", old.Package),
    paste("Package:", new.Package))
  pkg_find_replace(
    file.path("src","Makevars.*in"),
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
  pkg.edit.fun=my.edit.fun,
  my_test=atime::atime_test(N=2^seq(1,20)))
