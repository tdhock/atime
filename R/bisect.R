bisect_tcall <- function(tinfo, Test, ...){
  tcall <- tinfo$test.call[[Test]]
  keep.args <- c(
    names(formals(atime::atime_versions)),
    "", ...)
  rm.args <- setdiff(names(tcall), keep.args)
  tcall[rm.args] <- NULL
  tcall$sha.vec <- list(
    HEAD=tcall$sha.vec[[grep("HEAD", names(tcall$sha.vec))]])
  tcall
}

bisect <- function(pkg.path, Test, old.name="Fast", new.name="Slow", reps=10){
  tinfo <- atime_pkg_test_info(pkg.path)
  tcall <- bisect_tcall(tinfo, Test, old.name, new.name)
  tcall$sha.vec <- NULL
  rep.dt <- data.table(rep.i=seq_len(10))[, {
    tres <- eval(tcall)
    tref <- atime::references_best(tres)
    tpred <- predict(tref)
    tpred[["prediction"]]
  }, by=rep.i]
  atime.path <- dirname(tinfo$tests.R)
  test.path <- file.path(
    atime.path, "bisect", test_file_name(Test),
    paste(c(
      old.name,
      new.name),
      collapse="-"))
  unlink(test.path, recursive = TRUE)
  dir.create(test.path, recursive = TRUE)
  file.copy(tinfo$tests.R, test.path)
  csv.dt.list <- list(
    Test_commits=data.table(Test, old.name, new.name),
    version_stats=dcast(
      rep.dt[, .(version=expr.name, log10_N = log10(N))],
      version ~ .,
      list(mean, sd),
      value.var="log10_N"))
  if(FALSE){
    plot(log10(N) ~ rep.i, rep.dt);csv.dt.list$version_stats[, abline(h=log10_N_mean+log10_N_sd*rep(c(-1,0,1),each=2))]
  }
  for(csv_name in names(csv.dt.list)){
    csv.dt <- csv.dt.list[[csv_name]]
    out.csv <- file.path(test.path, paste0(csv_name, ".csv"))
    fwrite(csv.dt, out.csv)
  }
  cmd <- paste(c(
    sprintf("cd %s", tinfo$checkout.path),
    "git bisect start",
    sprintf("git bisect old %s", tcall[[old.name]]),
    sprintf("git bisect new %s", tcall[[new.name]]),
    sprintf("git bisect run R -e 'atime::bisect_run(\"%s\")'", test.path),
    sprintf("rm -f %s", tinfo$tests.R),
    "git bisect reset"
  ), collapse=" && ")
  cat(cmd, "\n")
  system(cmd)
}

bisect_run <- function(test.path){
  start <- Sys.time()
  Test_commits.csv <- file.path(test.path, "Test_commits.csv")
  Test_commits <- fread(Test_commits.csv)
  version_stats.csv <- file.path(test.path, "version_stats.csv")
  version_stats <- fread(version_stats.csv)
  atime.path <- dirname(dirname(dirname(test.path)))
  pkg.path <- dirname(dirname(atime.path))
  tests.R <- file.path(test.path, "tests.R")
  dir.create(atime.path, showWarnings = FALSE, recursive = TRUE)
  file.copy(tests.R, atime.path, overwrite = TRUE)
  tinfo <- atime_pkg_test_info(pkg.path)
  if(is.function(tinfo$bisect.restore.fun)){
    tinfo$bisect.restore.fun(tinfo)
  }else{
    gert::git_restore(".", repo=tinfo$checkout.path)
  }
  Test <- Test_commits[, Test]
  new.name <- Test_commits[, new.name]
  old.name <- Test_commits[, old.name]
  tcall <- bisect_tcall(tinfo, Test, old.name, new.name)
  saveRDS(tcall, file.path(test.path, "tcall.rds"))
  N.cols <- c(old.name,"HEAD",new.name)
  log.row <- gert::git_log("HEAD", 1, repo=tinfo$checkout.path)
  status <- NA
  timings <- 0
  while(is.na(status)){
    timings <- timings+1
    cmd <- sprintf("R -e 'atime:::bisect_try(\"%s\")'", test.path)
    system(cmd)
    tres.or.status <- readRDS(file.path(test.path, "tres.or.status.rds"))
    status <- if(is.numeric(tres.or.status)){
      pwide <- sec.compare <- data.table()[, (N.cols) := NA_real_]
      tres.or.status
    }else{
      tref <- references_best(tres.or.status)
      tpred <- predict(tref)
      RData.dir <- file.path(test.path, "RData")
      dir.create(RData.dir, showWarnings = FALSE, recursive = TRUE)
      result.RData <- file.path(RData.dir, paste0(log.row[["commit"]], ".RData"))
      save(tres.or.status, tref, tpred, file=result.RData)
      sec.wide <- dcast(tres.or.status$meas, N ~ expr.name, value.var="median")
      sec.compare <- sec.wide[!apply(is.na(sec.wide), 1, any)][.N]
      pwide <- dcast(
        tpred$prediction[, log10_N := log10(N)],
        . ~ expr.name,
        value.var="log10_N")
      plong <- melt(
        pwide,
        measure.vars=c(old.name,new.name),
        variable.name="version"
      )[, diff := abs(HEAD-value)][]
      norm.dt <- version_stats[plong, on="version"][
      , normality := fcase(
        value < log10_N_mean-log10_N_sd, "lo",
        value > log10_N_mean+log10_N_sd, "hi",
        default="normal"
      )][]
      print(norm.dt)
      if(sum(norm.dt$normality=="normal")<2){
        NA # run another timing.
      }else{
        closer <- plong[which.min(diff), version]
        ## From man git-bisect, section Bisect run
        ## If you have a script that can tell if the current source code is old.name
        ## or new.name, you can bisect by issuing the command:
        ## $ git bisect run my_script arguments
        ## Note that the script (my_script in the above example) should exit with
        ## code 0 if the current source code is old, and exit with a code
        ## between 1 and 127 (inclusive), except 125, if the current source code
        ## is new.
        ifelse(closer==old.name, 0, 1)
      }
    }
  }
  log.dt <- data.table(
    timings,
    start, end=Sys.time(),
    log.row[, c("commit","time")],
    status,
    log10_N=pwide[, N.cols, with=FALSE],
    log10_seconds=log10(sec.compare[, N.cols, with=FALSE]))
  print(log.dt)
  csv.dir <- file.path(test.path, "csv")
  dir.create(csv.dir, showWarnings = FALSE, recursive = TRUE)
  result.csv <- file.path(csv.dir, paste0(log.dt[["commit"]], ".csv"))
  fwrite(log.dt, result.csv)
  q(status=status)
}

bisect_try <- function(test.path){
  tcall <- readRDS(file.path(test.path, "tcall.rds"))
  tres.or.status <- tryCatch({
    eval(tcall)
    ## From man git-bisect
    ## The special exit code 125 should be used when the current source code
    ## cannot be tested. If the script exits with this code, the current
    ## revision will be skipped (see git bisect skip above).
    ## Bisect skip
    ## Instead of choosing a nearby commit by yourself, you can ask Git to do
    ## it for you by issuing the command:
    ## $ git bisect skip                 # Current version cannot be tested
    ## However, if you skip a commit adjacent to the one you are looking for,
    ## Git will be unable to tell exactly which of those commits was the first
    ## new.name one.
  }, error=function(e)125)
  saveRDS(tres.or.status, file.path(test.path, "tres.or.status.rds"))
}
