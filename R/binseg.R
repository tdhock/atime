atime.list <- atime(
  N=2:20,
  setup={
    N.data <- 2^N
    max.segs <- as.integer(N.data/2)
    max.changes <- max.segs-1L
    print(N.data)
    set.seed(1)
    data.vec <- 1:N.data
  },
  "changepoint::cpt.mean"={
    cpt.fit <- changepoint::cpt.mean(data.vec, method="BinSeg", Q=max.changes)
    sort(c(N.data,cpt.fit@cpts.full[max.changes,]))
  },
  "binsegRcpp::binseg_normal"={
    binseg.fit <- binsegRcpp::binseg_normal(data.vec, max.segs)
    sort(binseg.fit$splits$end)
  },
  "fpop::multiBinSeg"={
    mbs.fit <- fpop::multiBinSeg(data.vec, max.changes)
    sort(c(mbs.fit$t.est, N.data))
  },
  "wbs::sbs"={
    wbs.fit <- wbs::sbs(data.vec)
    split.dt <- data.table(wbs.fit$res)[order(-min.th, scale)]
    sort(split.dt[, c(N.data, cpt)][1:max.segs])
  },
  binsegRcpp.list={
    binseg.fit <- binsegRcpp::binseg(
      "mean_norm",data.vec, max.segs, container.str="list")
    sort(binseg.fit$splits$end)
  },
  seconds.limit=0.1,
  verbose=TRUE,
  times=5)

