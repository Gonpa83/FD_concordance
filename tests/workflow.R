library(fdconcordance)
set.seed(17)
d <- data.frame(a = rnorm(60), b = rnorm(60), richness = rep(1:10, 6),
                site = rep(letters[1:12], each = 5))
z <- compare_metrics(d, 'a', 'b', 'richness')
stopifnot(z$n == 60, abs(unname(z$spearman$estimate) - cor(d$a,d$b,method='spearman')) < 1e-12)
missing <- d; missing$a[2] <- NA; missing$b[4] <- Inf
m <- compare_metrics(missing, 'a', 'b')
stopifnot(m$n == 58, identical(m$excluded_rows,c(2L,4L)), is.null(m$residual_spearman))
tied <- data.frame(a=rep(0:2,each=6),b=rep(0:2,6))
t <- compare_metrics(tied,'a','b')
stopifnot(abs(t$agreement[['global']] - 1/3)<1e-12, all(abs(t$classes$jaccard-.2)<1e-12))
bad <- tryCatch(compare_metrics(d[1:5, ], 'a','b'), error=identity)
stopifnot(inherits(bad,'error'))
g <- compare_metrics(d,'a','b','richness',model='gam',group='site')
stopifnot(inherits(g$continuous_model,'gam'))
for (x in list(z,m,g)) {
  out <- tempfile('fdconcordance-test-')
  manifest <- export_comparison(x,out)
  tab <- manifest$summary
  stopifnot(!any(grepl("Spearman P-value", tab$Statistic, fixed = TRUE)),
            is.null(x$spearman$p.value), is.null(x$residual_spearman$p.value),
            abs(unname(x$spearman$estimate) - cor(x$observations$y, x$observations$x, method = "spearman")) < 1e-12)
  sm <- summary(x$continuous_model)
  if (inherits(x$continuous_model, "gam")) {
    stopifnot(tab$Value[tab$Statistic == "Predictor smooth P-value"] == sm$s.table["s(x)", "p-value"])
  } else {
    f <- sm$fstatistic
    stopifnot(tab$Value[tab$Statistic == "Overall model P-value (F-test)"] ==
                unname(stats::pf(f[1], f[2], f[3], lower.tail = FALSE)))
  }
  stopifnot(all(file.exists(file.path(out,manifest$files))))
  exported <- read.csv(file.path(out,'observations.csv'))
  stopifnot(nrow(exported)==x$n)
  overwrite <- tryCatch(export_comparison(x,out),error=identity)
  stopifnot(inherits(overwrite,'error'))
}
