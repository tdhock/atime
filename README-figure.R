## Example 1: regex functions.
(subject.size.vec <- unique(as.integer(10^seq(0,3.5,l=100))))
atime.list <- atime::atime(
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  PCRE.match=regexpr(pattern, subject, perl=TRUE),
  TRE.match=regexpr(pattern, subject, perl=FALSE),
  constant.replacement=gsub("a","constant size replacement",subject),
  linear.replacement=gsub("a",subject,subject),
  seconds.limit=0.1,
  times=10,
  N=subject.size.vec)
(best.list <- atime::references_best(atime.list))
plot(best.list)

write.N.cols <- atime::atime(
  N=as.integer(10^seq(0, 7, by=0.5)),
  setup={
    mat <- matrix("'my quoted string'", 10, N)
    df <- data.frame(mat)
    out.csv <- tempfile()
  },
  seconds.limit=1,
  times=3,
  "utils::write.csv"=utils::write.csv(df, out.csv),
  "data.table::fwrite"=data.table::fwrite(df, out.csv))
write.N.cols.best <- atime::references_best(write.N.cols)
plot(write.N.cols.best)
