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
atime.list[["measurements"]][N==323, .(expr.name, seconds=median, kilobytes)]
pred.list <- predict(best.list, seconds=1e-2, kilobytes=10)
pred.list[["prediction"]]
plot(pred.list)

png("README-predict.png", width=6, height=4, units="in", res=200)
plot(pred.list)+ggplot2::theme(text=ggplot2::element_text(size=20))
dev.off()
