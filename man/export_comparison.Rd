\name{export_comparison}
\alias{export_comparison}
\title{Export Tables and Graphs for a Metric Comparison}
\description{Saves a readable HTML report, CSV tables, PNG figures, a multipage PDF and the complete R result. Uses the fitted models without refitting.}
\usage{export_comparison(result, directory, title = "Functional diversity concordance")}
\arguments{
\item{result}{A result returned by compare_metrics.}
\item{directory}{Output directory. Existing generated filenames cause an error to prevent overwriting an earlier run.}
\item{title}{Report title.}
}
\details{Exports three stages when richness control was omitted, otherwise four. Regression curves exclude any group random intercept. Bands are approximate pointwise 95 percent intervals for the fitted mean, not prediction intervals. Rank ties use average ranks. Exported tables include source row numbers, exclusions, classifications, residuals when requested, ranks and curve values for external plotting. Spearman coefficients are descriptive; no Spearman P-values are computed or reported. No threshold verdict is assigned. The PDF is a figure collection, not the complete HTML report.}
\section{Inference}{The summary includes the overall F-test P-value for a quadratic regression, or the approximate predictor smooth P-value for a GAM. Raw-index and residual Spearman coefficients are descriptive summaries, without P-values or significance tests. Grouped models do not make these pooled correlations group-adjusted estimates. HTML displays model P-values below 0.001 as less than 0.001; CSV retains their numeric values.}
\value{Invisibly, a list with output directory, generated filenames and summary table.}
\examples{
set.seed(12)
d <- data.frame(first = runif(30), second = runif(30))
result <- compare_metrics(d, "first", "second")
export_comparison(result, tempfile("concordance-report-"))
}
