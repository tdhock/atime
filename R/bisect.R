bisect_tcall <- function(tinfo, Test, ...){
  tcall <- tinfo$test.call[[Test]]
  l.item <- tinfo$test.list[[Test]]
  if(is.null(tcall))stop("Test case does not exist: ", Test)
  historical.commits <- c(
    l.item[["ci.commits"]], #[[ instead of with to avoid error when there are none.
    l.item[["bisect.commits"]])
  old.new <- historical.commits[c(...)]
  tcall <- get_test_call(l.item, old.new)
  tcall$sha.vec <- c(
    HEAD=tcall$sha.vec[[grep("HEAD", names(tcall$sha.vec))]])
  tcall
}

bisect <- function(pkg.path, Test, old.name="Fast", new.name="Slow"){
  N <- NULL
  ## Above to avoid CRAN check NOTEs.
  tinfo <- atime_pkg_test_info(pkg.path)
  tcall <- bisect_tcall(tinfo, Test, old.name, new.name)
  for(prefix in c("old","new")){
    vname <- paste0(prefix,".name")
    Rname <- get(vname)
    if(!(is.character(Rname) && length(Rname)==1 && !is.na(Rname))){
      stop(vname, " should be string: name of historical commit in atime test case")
    }
    ref <- tcall[[Rname]]
    if(!(is.character(ref) && length(ref)==1 && !is.na(ref))){
      stop(sprintf(
        "historical commit %s does not exist in atime test case %s",
        Rname, Test))
    }
  }
  tcall$sha.vec <- NULL
  tres <- eval(tcall)
  max.dt <- tres$meas[N==max(N)]
  if(nrow(max.dt)==2){
    stop(sprintf("both old=%s and new=%s stopped at N=%s, which means there is not a large difference in throughput using the current N sequence and time limit; if you believe there is a real difference that should be detected, you can try again with a finer N sequence", old.name, new.name, max.dt$N[1]))
  }
  tref <- references_best(tres)
  tpred <- predict(tref)
  if(nrow(tpred[["prediction"]]) != 2){
    print(tpred)
    stop("both old and new are required to have predicted values at seconds.limit; fix by decreasing time limit or increasing largest N\n")
  }
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
    Test_commits=data.table(Test, old.name, new.name))
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
    sprintf("git bisect run %s -e 'atime::bisect_run(\"%s\")'", R_HOME_R(), test.path),
    sprintf("cd %s", dirname(tinfo$tests.R)),
    "git restore tests.R",
    sprintf("cd %s", tinfo$checkout.path),
    "git bisect reset"
  ), collapse=" && ")
  cat(cmd, "\n")
  status <- system(cmd)
  if(status != 0)stop("error in git bisect")
}

bisect_run <- function(test.path){
  log10_N <- N <- HEAD <- value <- new.name <- old.name <- Test <- NULL
  ## Above to avoid CRAN check NOTEs.
  start <- Sys.time()
  Test_commits.csv <- file.path(test.path, "Test_commits.csv")
  Test_commits <- fread(Test_commits.csv)
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
  tcall <- Test_commits[, bisect_tcall(tinfo, Test, old.name, new.name)]
  saveRDS(tcall, file.path(test.path, "tcall.rds"))
  N.cols <- Test_commits[, c(old.name,"HEAD",new.name)]
  log.row <- gert::git_log("HEAD", 1, repo=tinfo$checkout.path)
  status <- NA
  while(is.na(status)){
    cmd <- sprintf("%s -e 'atime:::bisect_try(\"%s\")'", R_HOME_R(), test.path)
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
      if(nrow(tpred[["prediction"]])!=length(N.cols)){
        cat("Skipping because there are not enough measurements at time limit; fix by decreasing time limit or increasing N\n")
        print(tpred)
        q(status=125)
      }
      pwide <- dcast(
        tpred$prediction[, log10_N := log10(N)],
        . ~ expr.name,
        value.var="log10_N")
      plong <- melt(
        pwide,
        measure.vars=Test_commits[, c(old.name,new.name)],
        variable.name="version"
      )[, diff := abs(HEAD-value)][]
      closer <- plong[which.min(diff), version]
      ## From man git-bisect, section Bisect run
      ## If you have a script that can tell if the current source code is old.name
      ## or new.name, you can bisect by issuing the command:
      ## $ git bisect run my_script arguments
      ## Note that the script (my_script in the above example) should exit with
      ## code 0 if the current source code is old, and exit with a code
      ## between 1 and 127 (inclusive), except 125, if the current source code
      ## is new.
      ifelse(closer==Test_commits[, old.name], 0, 1)
    }
  }
  log.dt <- data.table(
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
  tcall.rds <- file.path(test.path, "tcall.rds")
  tcall <- readRDS(tcall.rds)
  unlink(tcall.rds)
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
