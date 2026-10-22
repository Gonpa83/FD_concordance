# Teaching data only: not a biological simulation or scientific validation.
library(fdconcordance)
set.seed(17)
example_values <- data.frame(
  Plot = 1:60, Site = rep(paste0('Site', 1:12), each = 5),
  Richness = sample(3:15, 60, replace = TRUE), Index_A = runif(60, 1, 5))
example_values$Index_B <- 2 * example_values$Index_A + rnorm(60, sd = 0.5)
result <- compare_metrics(example_values, 'Index_B', 'Index_A', 'Richness',
                          model = 'gam', group = 'Site')
report_directory <- tempfile('FD_Concordance_example_')
report <- export_comparison(result, report_directory, 'FD Concordance teaching example')
print(report$summary)
cat('\nOpen this file in your browser:\n', file.path(report$directory, 'OPEN_REPORT.html'), '\n')
