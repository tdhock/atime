## no setup, only one algo at a time (no controlled comparison), yes
## stop after going over time limit.
testComplexity::asymptoticTimings()

## can not store results if check=FALSE, results must be equal if
## check=TRUE, no stop after going over time limit.
max.N <- 20
bench::press(
  N=1:max.N,
  {
    cat(sprintf("subject/pattern size %4d / %4d\n", N, max.N))
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("(a)?", "\\1"), each=N), collapse="")
    bench::mark(
      check=FALSE,
      ICU=stringi::stri_match(subject, regex=pattern),
      PCRE=regexpr(pattern, subject, perl=TRUE),
      TRE=regexpr(pattern, subject, perl=FALSE),
      min_iterations=0,
      min_time=0.01
    )
  })

max.N <- 20
time.dt <- atime(
  ICU=stringi::stri_match(subject, regex=pattern),
  PCRE=regexpr(pattern, subject, perl=TRUE),
  TRE=regexpr(pattern, subject, perl=FALSE),
  times=10,
  seconds.limit=0.1,
  N=1:max.N,
  setup={
    cat(sprintf("subject/pattern size %4d / %4d\n", N, max.N))
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("(a)?", "\\1"), each=N), collapse="")
  })

ggplot()+
  geom_ribbon(aes(
    N, ymin=min, ymax=max, fill=expr.name),
    data=time.dt,
    alpha=0.5)+
  geom_line(aes(
    N, median, color=expr.name),
    data=time.dt)

time.dt <- atime(
  ICU=stringi::stri_match(subject, regex=pattern),
  PCRE=regexpr(pattern, subject, perl=TRUE),
  TRE=regexpr(pattern, subject, perl=FALSE),
  RE2=re2r::re2_match(pattern, subject),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  N=1:25,
  times=10)
