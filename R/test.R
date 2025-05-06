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
  for(Test in names(test.info$test.list)){
    atv.call <- test.info$test.call[[Test]]
    atime.list <- eval(atv.call, test.info)
    pkg.results[[Test]] <- atime.list
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
    compare.name <- test.info$base.name
    HEAD.compare <- c(test.info$HEAD.name, compare.name)
    largest.common.timings <- largest.common.N[
      expr.name %in% HEAD.compare, .(
        seconds=as.numeric(time[[1]])
      ), by=.(N, unit, expr.name)][, log10.seconds := log10(seconds)][]
    compare.dt.list[[Test]] <- data.table(
      Test, largest.common.timings)
    test.args <- list()
    for(commit.i in seq_along(HEAD.compare)){
      commit.name <- HEAD.compare[[commit.i]]
      test.args[[commit.i]] <- largest.common.timings[
        expr.name==commit.name, log10.seconds]
    }
    test.args$alternative <- "greater"
    p.value <- do.call(stats::t.test, test.args)$p.value
    hline.df <- with(atime.list, data.frame(seconds.limit, unit="seconds"))
    limit.dt.list[[Test]] <- data.table(Test, hline.df)
    bench.dt.list[[Test]] <- data.table(
      Test, p.value, best.list$meas)
    log10.range <- range(log10(atime.list$meas$N))
    expand <- diff(log10.range)*test.info$expand.prop
    xmax <- 10^(log10.range[2]+expand)
    one.blank <- data.table(Test, best.list$meas[1])
    one.blank[, N := xmax]
    blank.dt.list[[Test]] <- one.blank
    gg <- ggplot2::ggplot()+
      ggplot2::ggtitle(Test)+
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
      paste0(gsub('[\':\\ /*|<>"?\n\r]', "_", Test), ".png"))
    grDevices::png(out.png, width=test.info$width.in*nrow(max.dt), height=test.info$height.in, units="in", res=100)
    print(gg)
    grDevices::dev.off()
  }
  bench.dt <- rbindlist(bench.dt.list)
  setkey(bench.dt, p.value)
  bench.dt[, p.str := sprintf("%.2e", p.value)]
  bench.dt[, P.value := factor(p.str, unique(p.str))]
  meta.dt <- unique(bench.dt[, .(Test, P.value)])
  tests.RData <- sub("R$", "RData", test.info$tests.R)
  install.seconds <- sapply(pkg.results, "[[", "install.seconds")
  cat(
    sum(install.seconds),
    file=file.path(dirname(tests.RData), "install_seconds.txt"))
  ## create all and preview facet PNGs.
  N.tests <- length(test.info$test.list)
  out_N_list <- list(all=N.tests)
  if(test.info$N.tests.preview < N.tests){
    out_N_list$preview <- test.info$N.tests.preview
  }
  for(N_name in names(out_N_list)){
    N_int <- out_N_list[[N_name]]
    N_meta <- meta.dt[1:N_int]
    limit.dt <- rbindlist(limit.dt.list)[N_meta, on="Test"]
    blank.dt <- rbindlist(blank.dt.list)[N_meta, on="Test"]
    compare.dt <- rbindlist(compare.dt.list)[N_meta, on="Test"]
    N_bench <- bench.dt[N_meta, on="Test"]
    ## Plot only compare.dt
    ##ggplot()+geom_point(aes(seconds, expr.name), shape=1, data=compare.dt)+facet_grid(. ~ P.value + Test, labeller=label_both, scales="free")+scale_x_log10()
    gg <- ggplot2::ggplot()+
      ggplot2::ggtitle(sprintf(
        "%d test cases (%s), ordered by p-value (T-test, HEAD>min, dots show data tested)",
        N_int, N_name))+
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
        data=N_bench)+
      ggplot2::geom_blank(ggplot2::aes(
        N, empirical),
        data=blank.dt)+
      ggplot2::geom_ribbon(ggplot2::aes(
        N, ymin=q25, ymax=q75, fill=expr.name),
        data=N_bench[unit=="seconds"],
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
        data=N_bench)+
      ggplot2::theme(legend.position="none")
    out.png <- file.path(
      dirname(test.info$tests.R),
      sprintf("tests_%s_facet.png", N_name))
    grDevices::png(
      out.png,
      width=test.info$width.in*N_int,
      height=test.info$height.in,
      units="in",
      res=100)
    print(gg)
    grDevices::dev.off()
    if(N_name=="all"){
      save(
        pkg.results, bench.dt, limit.dt, test.info, blank.dt, 
        file=tests.RData)
    }
  }
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
    N.tests.preview=4,
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
  if(Package %in% rownames(ap)){
    CRAN.name <- paste0("CRAN=",ap[Package,"Version"])
    sha.vec[[CRAN.name]] <- ""
  }else{
    CRAN.name <- NA_character_
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
  common.args <- list(
    N.env.parent=test.env,
    pkg.path=pkg.path,
    sha.vec=sha.vec)
  test.env$test.list <- inherit_args(test.env$test.list, common.args)
  test.env$test.call <- list()
  for(Test in names(test.env$test.list)){
    test.env$test.call[[Test]] <- as.call(c(
      quote(atime::atime_versions),
      test.env$test.list[[Test]]))
  }
  test.env
}

get_test_args <- function(){
  s.parent <- sys.parent()
  pfun <- sys.function(s.parent)
  two.funs <- list(pfun, atime_versions)
  name.vecs <- lapply(two.funs, function(f)names(formals(f)))
  could.copy <- Reduce(intersect, name.vecs)
  mc <- as.list(match.call(pfun, sys.call(s.parent))[-1])
  common.names <- intersect(names(mc), could.copy)
  possible.uneval <- c("setup","expr")
  uneval.names <- intersect(common.names, possible.uneval)
  eval.names <- setdiff(common.names, possible.uneval)
  p.frame <- parent.frame()
  test.args <- mget(eval.names, p.frame)
  test.args[uneval.names] <- mc[uneval.names]
  test.args
}

atime_test <- function(N, setup, expr, times, seconds.limit, verbose, pkg.edit.fun, result, ...){
  c(get_test_args(), ...)
}

atime_test_list <- function(N, setup, expr, times, seconds.limit, verbose, pkg.edit.fun, result, tests=NULL, ...){
  common.args <- get_test_args()
  L <- c(tests, list(...))
  inherit_args(L, common.args)
}

inherit_args <- function(L, common.args){
  out <- list()
  for(L.i in seq_along(L)){
    test.args <- L[[L.i]]
    Test <- names(L)[[L.i]]
    if(!is.null(test.args)){
      out.args <- common.args
      out.args[names(test.args)] <- test.args
      out[[Test]] <- out.args
    }
  }
  out
}
