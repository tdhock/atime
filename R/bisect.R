bisect <- function(pkg.path, Test, bad="Slow", good="Fast"){
  tinfo <- atime_pkg_test_info(pkg.path)
  tcall <- tinfo$test.call[[Test]]
  atime.path <- dirname(tinfo$tests.R)
  test.path <- file.path(atime.path, "bisect", test_file_name(Test))
  unlink(test.path, recursive = TRUE)
  dir.create(test.path, recursive = TRUE)
  file.copy(tinfo$tests.R, test.path)
  Test_commits.csv <- file.path(test.path, "Test_commits.csv")
  Test_commits <- data.table(Test, bad, good)
  fwrite(Test_commits, Test_commits.csv)
  commands <- function(...){
    cmd <- paste(c(
      sprintf("cd %s", tinfo$checkout.path),
      ...
    ), collapse=" && ")
    cat(cmd, "\n")
    system(cmd)
  }
  status <- commands(
    "git bisect start",
    sprintf("git bisect bad %s", tcall[[bad]]),
    sprintf("git bisect good %s", tcall[[good]]),
    sprintf("git bisect run R -e 'atime::bisect_run(\"%s\")'", test.path))
  if(status!=0)commands("git bisect reset")
}

bisect_run <- function(test.path){
  Test_commits.csv <- file.path(test.path, "Test_commits.csv")
  Test_commits <- fread(Test_commits.csv)
  atime.path <- dirname(dirname(test.path))
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
  tcall <- tinfo$test.call[[Test]]
  bad <- Test_commits[, bad]
  good <- Test_commits[, good]
  N.cols <- c(good,bad,"HEAD")
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
    ## bad one.
  }, error=function(e)125)
  status <- if(is.numeric(tres.or.status)){
    pwide <- data.table()[, N.cols := NA_real_]
    tres.or.status
  }else{
    tref <- references_best(tres.or.status)
    tpred <- predict(tref)
    set_version <- function(dt)dt[
    , version := ifelse(grepl("HEAD", expr.name), "HEAD", expr.name)
    ][N.cols, on="version"]
    sec.long <- set_version(tres.or.status$meas)[, .(version, N, median)]
    sec.wide <- dcast(sec.long, N ~ version, value.var="median")
    sec.compare <- sec.wide[!apply(is.na(sec.wide), 1, any)][.N]
    pwide <- dcast(
      set_version(tpred$prediction),
      . ~ version,
      value.var="N")
    plong <- melt(
      pwide,
      measure.vars=c(good,bad),
      variable.name="version"
    )[, diff := abs(HEAD-value)][]
    closer <- plong[which.min(diff), version]
    ## From man git-bisect, section Bisect run
    ## If you have a script that can tell if the current source code is good
    ## or bad, you can bisect by issuing the command:
    ## $ git bisect run my_script arguments
    ## Note that the script (my_script in the above example) should exit with
    ## code 0 if the current source code is good/old, and exit with a code
    ## between 1 and 127 (inclusive), except 125, if the current source code
    ## is bad/new.
    ifelse(closer==good, 0, 1)
  }
  log.row <- gert::git_log("HEAD", 1, repo=tinfo$checkout.path)
  log.dt <- data.table(
    log.row[, c("commit","time")],
    status,
    N=pwide[, N.cols, with=FALSE],
    seconds=sec.compare[, N.cols, with=FALSE])
  print(log.dt)
  N.dir <- file.path(test.path, "results")
  dir.create(N.dir, showWarnings = FALSE, recursive = TRUE)
  N.csv <- file.path(N.dir, paste0(log.dt[["commit"]], ".csv"))
  fwrite(log.dt, N.csv)
  q(status=status)
}
