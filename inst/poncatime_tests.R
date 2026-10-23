checkout.path.relative <- "src/external/ponca"
remove.units <- "kilobytes"
version.colors = c(
  HEAD="#6EA358", 
  "v1.0" = "#FFD1C7",
  "v1.2" = "#FFB5A3",
  "v1.3" = "#FF9C85",
  "v1.4" = "#FF7D5E",
  "v2.0.alpha0" = "#C9DCFF",
  "v2.0.alpha1" = "#A1C1FF",
  "v2.0.alpha2" = "#82ABFF",
  "v2.0.alpha3" = "#548CFF"
)

edit_wrapper <- function(pkg.path){
  src.dir <- file.path(pkg.path, "src")
  h.vec <- Sys.glob(file.path(
    src.dir, "wrapper-versions", "*", "curvatureEstimation.h"))
  (version.dirs <- c("", dirname(h.vec)))
  for(vdir in version.dirs){
    cat(sprintf("Attempting to compile with wrapper=%s\n", vdir))
    if(vdir != ""){
      system(paste("cp", file.path(vdir, "*"), src.dir))
    }
    unlink(file.path(src.dir, "*o"))
    status.int <- atime:::R_CMD_INSTALL(pkg.path)
    if(status.int == 0)return(vdir)
  }
}
edit_poncatime <- function(old.Package, new.Package, sha, new.pkg.path){
  pkg_find_replace <- function(glob, FIND, REPLACE, warn=TRUE){
    glob_find_replace(file.path(new.pkg.path, glob), FIND, REPLACE, warn)
  }
  cat(sprintf("Attempting to find a wrapper for %s\n", sha))
  wrapper <- edit_wrapper(new.pkg.path)
  if(is.null(wrapper)){
    stop(sprintf("failed to find a wrapper which compiles for %s", sha))
  }else{
    cat(sprintf("wrapper=%s compiles for %s\n", wrapper, sha))
  }
  pkg_find_replace(
    "DESCRIPTION",
    paste0("Package:\\s+", old.Package),
    paste("Package:", new.Package))
  Package_ <- gsub(".", "_", old.Package, fixed=TRUE)
  sha_ <- gsub(".", "_", sha, fixed=TRUE)
  new.Package_ <- paste0(Package_, "_", sha_)
  pkg_find_replace(
    file.path("src", "RcppExports.cpp"),
    paste0("R_init_", Package_),
    paste0("R_init_", new.Package_))
  pkg_find_replace(
    file.path("R", "RcppExports.R"),
    sprintf("PACKAGE = '%s'", old.Package),
    sprintf("PACKAGE = '%s'", new.Package),
    warn=FALSE)#does not appear if registration used.
  pkg_find_replace(
    "NAMESPACE",
    sprintf('useDynLib\\("?%s"?', Package_),
    paste0('useDynLib(', new.Package))
}

## historical versions of ponca repo to run in each test.
ponca.versions <- c(
  "v1.0",
  "v1.4",
  "v2.0.alpha1")
test.list <- atime::atime_test_list(
  seconds.limit=0.1,
  N=10^seq(2, 6, by=0.1),
  pkg.edit.fun=edit_poncatime,
  ci.commits=setNames(ponca.versions, ponca.versions),
  asoCurvatureEstimationK=atime::atime_test(
    setup={
      N_points <- N
      N_queries <- N
      dataScale <- 10
      N_list <- Poncatime:::generatePointClouds_interface(N_points, N_queries, dataScale)
    },
    setup.version=Poncatime:::buildKdTree_interface(N_list$points),
    expr=Poncatime:::asoCurvatureEstimationK_interface(N_list$queries, k=10)
  ),
  asoCurvatureEstimation=atime::atime_test(
    setup={
      N_points <- N
      N_queries <- N
      dataScale <- 10
      N_list <- Poncatime:::generatePointClouds_interface(N_points, N_queries, dataScale)
    },
    setup.version=Poncatime:::buildKdTree_interface(N_list$points),
    expr=Poncatime:::asoCurvatureEstimation_interface(N_list$queries, dataScale/5)
  ),
  planeFitK=atime::atime_test(
    setup={
      N_points <- N
      N_queries <- N
      dataScale <- 10
      N_list <- Poncatime:::generatePointClouds_interface(N_points, N_queries, dataScale)
    },
    setup.version=Poncatime:::buildKdTree_interface(N_list$points),
    expr=Poncatime:::planeFitK_interface(N_list$queries, k=10)
  ),
  planeFit=atime::atime_test(
    setup={
      N_points <- N
      N_queries <- N
      dataScale <- 10
      N_list <- Poncatime:::generatePointClouds_interface(N_points, N_queries, dataScale)
    },
    setup.version=Poncatime:::buildKdTree_interface(N_list$points),
    expr=Poncatime:::planeFit_interface(N_list$queries, dataScale/5)
  ),
  buildKdTree=atime::atime_test(
    setup={
      N_points <- N
      N_queries <- 0
      dataScale <- 10
      N_list <- Poncatime:::generatePointClouds_interface(N_points, N_queries, dataScale)
    },
    expr=Poncatime:::buildKdTree_interface(N_list$points)
  )
)

