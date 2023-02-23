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
      }else{#new.Package not in lib
        if(sha==""){
          install.packages(Package, verbose=verbose)
        }else{#sha not empty
          tryCatch(
            git2r::checkout(repo, branch=sha, force=TRUE),
            error=function(e)stop(
              e, " when trying to checkout ", sha))
          ## before editing and installing, make sure directory has sha
          ## suffix, for windows checks.
          sha.path <- paste0(new.path,".",sha)
          file.rename(new.path, sha.path)
          unlink(file.path(sha.path, "src", "*.o"))
          pkg.edit.fun(
            old.Package=Package, 
            new.Package=new.Package,
            sha=sha, 
            new.pkg.path=sha.path)
          install.packages(
            sha.path, repos=NULL, type="source", verbose=verbose)
          if(verbose){
            cat("\nPackage info after editing and installation:\n")
            grep_glob <- function(glob, pattern){
              some.files <- Sys.glob(file.path(sha.path, glob))
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
            src.files <- dir(file.path(sha.path, "src"))
            out[["src/*.so|dll"]] <- grep("(so|dll)$", src.files, value=TRUE)
            print(out)
            cat("\n")
          }#if(verbose)
          file.rename(sha.path, new.path)
        }#if(sha) empty else
      }#if(new package not in lib)
    }#for(new.i
  }#any to install
}

atime_versions <- function(pkg.path, N, setup, expr, sha.vec=NULL, times=10, seconds.limit=0.01, verbose=FALSE, pkg.edit.fun=pkg.edit.default, results=TRUE, ...){
  ver.args <- list(
    pkg.path, substitute(expr), sha.vec, verbose, pkg.edit.fun, ...)
  ver.exprs <- do.call(atime_versions_exprs, ver.args)
  a.args <- list(
    N, substitute(setup), ver.exprs, times, seconds.limit, verbose, results)
  do.call(atime, a.args)
}

atime_versions_exprs <- function(pkg.path, expr, sha.vec=NULL, verbose=FALSE, pkg.edit.fun=pkg.edit.default, ...){
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  dots.vec <- mc.args[!names(mc.args) %in% formal.names]
  SHA.vec <- c(dots.vec, sha.vec)
  pkg.DESC <- file.path(pkg.path, "DESCRIPTION")
  DESC.mat <- read.dcf(pkg.DESC)
  Package <- DESC.mat[,"Package"]
  new.Package.vec <- paste0(
    Package, 
    ifelse(SHA.vec=="", "", "."), 
    SHA.vec)
  atime_versions_install(
    Package, pkg.path, new.Package.vec, SHA.vec, verbose, pkg.edit.fun)
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

atime_pkg <- function(pkg.path="."){
  ## For an example package see
  ## https://github.com/tdhock/binsegRcpp/blob/another-branch/inst/atime/tests.R
  pkg.DESC <- file.path(pkg.path, "DESCRIPTION")
  DESC.mat <- read.dcf(pkg.DESC)
  Package <- DESC.mat[,"Package"]
  ap <- available.packages()
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
  ## TODO take from tests.R file.
  width.in <- 4
  height.in <- 8
  expand.prop <- 0.5
  color.vec <- structure(
    c("red","black","deepskyblue","violet"), 
    names=c(HEAD.name, base.name, "merge-base", CRAN.name))
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
  tests.R <- file.path(pkg.path, "inst", "atime", "tests.R")
  test.env <- new.env()
  tests.parsed <- parse(tests.R)
  eval(tests.parsed, test.env)
  pkg.results <- list()
  blank.dt.list <- list()
  bench.dt.list <- list()
  limit.dt.list <- list()
  for(test.name in names(test.env$test.list)){
    pkg.sha.args <- list(pkg.path=pkg.path, sha.vec=sha.vec)
    user.args <- test.env$test.list[[test.name]]
    atv.args <- c(pkg.sha.args, user.args)
    atime.list <- do.call(atime_versions, atv.args)
    pkg.results[[test.name]] <- atime.list
    best.list <- atime::references_best(atime.list)
    ref.dt <- best.list$ref[each.sign.rank==1]
    sec.dt <- best.list$meas[unit=="seconds"]
    max.dt <- sec.dt[, .(
      N.values=.N, max.N=max(N)
    ), by=.(expr.name)]
    p.value <- sec.dt[data.table(N=min(max.dt$max.N)), {
      best.vec <- log10(as.numeric(time[[which.min(median)]]))
      head.vec <- log10(as.numeric(time[[which(expr.name==HEAD.name)]]))
      t.test(head.vec, best.vec, alternative = "greater")$p.value
    }, on="N"]
    hline.df <- with(atime.list, data.frame(seconds.limit, unit="seconds"))
    limit.dt.list[[test.name]] <- data.table(test.name, hline.df)
    bench.dt.list[[test.name]] <- data.table(
      test.name, p.value, best.list$meas)
    log10.range <- range(log10(atime.list$meas$N))
    expand <- diff(log10.range)*expand.prop
    xmax <- 10^(log10.range[2]+expand)
    one.blank <- data.table(test.name, best.list$meas[1])
    one.blank[, N := xmax]
    blank.dt.list[[test.name]] <- one.blank
    gg <- ggplot2::ggplot()+
      ggplot2::ggtitle(test.name)+
      ggplot2::theme_bw()+
      ggplot2::facet_grid(unit ~ expr.name, scales="free")+
      ggplot2::geom_hline(ggplot2::aes(
        yintercept=seconds.limit),
        color="grey",
        data=hline.df)+
      ggplot2::geom_line(ggplot2::aes(
        N, reference, group=paste(expr.name, fun.name)),
        color="grey50",
        data=ref.dt)+
      ggplot2::scale_color_manual(values=color.vec)+
      ggplot2::scale_fill_manual(values=color.vec)+
      ggplot2::geom_line(ggplot2::aes(
        N, empirical, color=expr.name),
        data=best.list$meas)+
      ggplot2::geom_ribbon(ggplot2::aes(
        N, ymin=q25, ymax=q75, fill=expr.name),
        data=best.list$meas[unit=="seconds"],
        alpha=0.5)+
      ggplot2::scale_x_log10()+
      ggplot2::scale_y_log10("median line, quartiles band")+
      directlabels::geom_dl(ggplot2::aes(
        N, reference, label.group=paste(expr.name, fun.name), label=fun.name),
        data=ref.dt,
        color="grey",
        method="bottom.polygons")+
      directlabels::geom_dl(ggplot2::aes(
        N, empirical, color=expr.name, label=expr.name),
        method="right.polygons",
        data=best.list$meas)+
      ggplot2::theme(legend.position="none")+
      ggplot2::coord_cartesian(xlim=c(NA,xmax))
    out.png <- file.path(
      dirname(tests.R), 
      paste0(gsub("[: /]", "_", test.name), ".png"))
    png(out.png, width=width.in*nrow(max.dt), height=height.in, units="in", res=100)
    print(gg)
    dev.off()
  }
  bench.dt <- rbindlist(bench.dt.list)
  setkey(bench.dt, p.value)
  bench.dt[, p.str := sprintf("%.2e", p.value)]
  bench.dt[, P.value := factor(p.str, unique(p.str))]
  meta.dt <- unique(bench.dt[, .(test.name, P.value)])
  limit.dt <- rbindlist(limit.dt.list)[meta.dt, on="test.name"]
  blank.dt <- rbindlist(blank.dt.list)[meta.dt, on="test.name"]
  tests.RData <- sub("R$", "RData", tests.R)
  save(
    pkg.results, bench.dt, limit.dt, color.vec, blank.dt, 
    file=tests.RData)
  gg <- ggplot2::ggplot()+
    ggplot2::theme_bw()+
    ggplot2::geom_hline(ggplot2::aes(
      yintercept=seconds.limit),
      color="grey",
      data=limit.dt)+
    ggplot2::scale_color_manual(values=color.vec)+
    ggplot2::scale_fill_manual(values=color.vec)+
    ggplot2::facet_grid(
      unit ~ P.value + test.name, scales="free", labeller="label_both")+
    ggplot2::geom_line(ggplot2::aes(
      N, empirical, color=expr.name),
      data=bench.dt)+
    ggplot2::geom_blank(ggplot2::aes(
      N, empirical),
      data=blank.dt)+
    ggplot2::geom_ribbon(ggplot2::aes(
      N, ymin=q25, ymax=q75, fill=expr.name),
      data=bench.dt[unit=="seconds"],
      alpha=0.5)+
    ggplot2::scale_x_log10()+
    ggplot2::scale_y_log10("median line, quartiles band")+
    directlabels::geom_dl(ggplot2::aes(
      N, empirical, color=expr.name, label=expr.name),
      method="right.polygons",
      data=bench.dt)+
    ggplot2::theme(legend.position="none")
  out.png <- file.path(
    dirname(tests.R), "tests_all_facet.png")
  N.tests <- length(test.env$test.list)
  png(out.png, width=width.in*N.tests, height=height.in, units="in", res=100)
  print(gg)
  dev.off()
  pkg.results
}
