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

atime_pkg <- function(pkg.path=".", tests.dir=NULL){
  ## For an example package see
  ## https://github.com/tdhock/binsegRcpp/blob/another-branch/inst/atime/tests.R
  each.sign.rank <- unit <- . <- N <- expr.name <- reference <- fun.name <- 
    empirical <- q25 <- q75 <- p.str <- p.value <- P.value <- 
      seconds.limit <- time <- log10.seconds <- seconds <- Test <- NULL
  ## above to avoid CRAN check NOTE.
  pkg.results <- list()
  blank.dt.list <- list()
  bench.dt.list <- list()
  limit.dt.list <- list()
  compare.dt.list <- list()
  test.info <- atime_pkg_test_info(pkg.path, tests.dir)
  for(test.name in names(test.info$test.list)){
    atv.call <- test.info$test.call[[test.name]]
    atime.list <- eval(atv.call, test.info)
    pkg.results[[test.name]] <- atime.list
    best.list <- atime::references_best(atime.list)
    ref.dt <- best.list$ref[each.sign.rank==1]
    sec.dt <- best.list$meas[unit=="seconds"]
    max.dt <- sec.dt[, .(
      N.values=.N, max.N=max(N)
    ), by=.(expr.name)]
    largest.common.N <- sec.dt[N==min(max.dt$max.N)]
    ## TODO: fixed comparison?
    compare.name <- largest.common.N[
      expr.name!=test.info$HEAD.name
    ][which.min(median), expr.name]
    HEAD.compare <- c(test.info$HEAD.name, compare.name)
    largest.common.timings <- largest.common.N[
      expr.name %in% HEAD.compare, .(
        seconds=as.numeric(time[[1]])
      ), by=.(N, unit, expr.name)][, log10.seconds := log10(seconds)][]
    compare.dt.list[[test.name]] <- data.table(
      test.name, largest.common.timings)
    test.args <- list()
    for(commit.i in seq_along(HEAD.compare)){
      commit.name <- HEAD.compare[[commit.i]]
      test.args[[commit.i]] <- largest.common.timings[
        expr.name==commit.name, log10.seconds]
    }
    test.args$alternative <- "greater"
    p.value <- do.call(stats::t.test, test.args)$p.value
    hline.df <- with(atime.list, data.frame(seconds.limit, unit="seconds"))
    limit.dt.list[[test.name]] <- data.table(test.name, hline.df)
    bench.dt.list[[test.name]] <- data.table(
      test.name, p.value, best.list$meas)
    log10.range <- range(log10(atime.list$meas$N))
    expand <- diff(log10.range)*test.info$expand.prop
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
      ggplot2::scale_color_manual(values=test.info$version.colors)+
      ggplot2::scale_fill_manual(values=test.info$version.colors)+
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
      dirname(test.info$tests.R), 
      paste0(gsub("[: /]", "_", test.name), ".png"))
    grDevices::png(out.png, width=test.info$width.in*nrow(max.dt), height=test.info$height.in, units="in", res=100)
    print(gg)
    grDevices::dev.off()
  }
  bench.dt <- rbindlist(bench.dt.list)[, Test := test.name]
  setkey(bench.dt, p.value)
  bench.dt[, p.str := sprintf("%.2e", p.value)]
  bench.dt[, P.value := factor(p.str, unique(p.str))]
  meta.dt <- unique(bench.dt[, .(Test, test.name, P.value)])
  limit.dt <- rbindlist(limit.dt.list)[meta.dt, on="test.name"]
  blank.dt <- rbindlist(blank.dt.list)[meta.dt, on="test.name"]
  compare.dt <- rbindlist(compare.dt.list)[meta.dt, on="test.name"]
  tests.RData <- sub("R$", "RData", test.info$tests.R)
  save(
    pkg.results, bench.dt, limit.dt, test.info, blank.dt, 
    file=tests.RData)
  gg <- ggplot2::ggplot()+
    ggplot2::theme_bw()+
    ggplot2::geom_hline(ggplot2::aes(
      yintercept=seconds.limit),
      color="grey",
      data=limit.dt)+
    ggplot2::scale_color_manual(values=test.info$version.colors)+
    ggplot2::scale_fill_manual(values=test.info$version.colors)+
    ggplot2::facet_grid(
      unit ~ P.value + Test, scales="free", labeller="label_both")+
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
    ggplot2::geom_point(ggplot2::aes(
      N, seconds, color=expr.name),
      shape=1,
      data=compare.dt)+
    ggplot2::scale_x_log10()+
    ggplot2::scale_y_log10("median line, quartiles band")+
    directlabels::geom_dl(ggplot2::aes(
      N, empirical, color=expr.name, label=expr.name),
      method="right.polygons",
      data=bench.dt)+
    ggplot2::theme(legend.position="none")
  out.png <- file.path(
    dirname(test.info$tests.R), "tests_all_facet.png")
  N.tests <- length(test.info$test.list)
  grDevices::png(
    out.png,
    width=test.info$width.in*N.tests,
    height=test.info$height.in,
    units="in",
    res=100)
  print(gg)
  grDevices::dev.off()
  pkg.results
}

default.version.colors <- c(#RColorBrewer::brewer.pal(7, "Dark2")
  HEAD="#1B9E77",
  base="#D95F02",
  "merge-base"="#7570B3",
  CRAN="#E7298A",
  Before="#66A61E",
  Regression="#E6AB02", Slow="#E6AB02",
  Fixed="#A6761D", Fast="#A6761D"
)

atime_pkg_test_info <- function(pkg.path=".", tests.dir=NULL){
  if(is.null(tests.dir)){
    tests.dir <- c("inst",".ci")
  }
  test.env <- new.env()
  test.env$tests.R <- find_tests_file(pkg.path, tests.dir)
  tests.parsed <- parse(test.env$tests.R)
  eval(tests.parsed, test.env)
  default.list <- list(
    width.in=4,
    height.in=8,
    expand.prop=0.5,
    version.colors=default.version.colors)
  for(var.name in names(default.list)){
    if(is.null(test.env[[var.name]])){
      test.env[[var.name]] <- default.list[[var.name]]
    }
  }
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
  test.env$HEAD.name <- HEAD.name
  test.env$base.name <- base.name
  test.env$CRAN.name <- CRAN.name
  names(test.env$version.colors) <- ifelse(
    names(test.env$version.colors) %in% names(abbrev2name),
    abbrev2name[names(test.env$version.colors)],
    names(test.env$version.colors))
  pkg.sha.args <- list(
    pkg.path=pkg.path,
    sha.vec=sha.vec)
  test.env$test.list <- inherit_args(test.env$test.list, pkg.sha.args)
  test.env$test.call <- list()
  for(test.name in names(test.env$test.list)){
    test.env$test.call[[test.name]] <- as.call(c(
      quote(atime::atime_versions),
      test.env$test.list[[test.name]]))
  }
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
