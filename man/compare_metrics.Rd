\name{compare_metrics}
\alias{compare_metrics}
\title{Compare Two Selected Functional Diversity Metrics}
\description{Evaluates dataset-specific concordance when overlap is suspected. No automatic redundancy threshold or index selection is applied.}
\usage{
compare_metrics(data, response, predictor, richness = NULL,
  model = c("quadratic", "gam"), group = NULL, k = 5)
}
\arguments{
\item{data}{A data frame with one row per observation and precomputed metric values.}
\item{response}{Name of the response metric column.}
\item{predictor}{Name of the predictor metric column.}
\item{richness}{Optional name of the taxonomic richness column. NULL omits step four.}
\item{model}{Quadratic ordinary least squares or a GAM fitted by REML.}
\item{group}{Optional grouping column for repeated observations from a site or lake. Requires the GAM option.}
\item{k}{Smooth basis dimension, default five, for GAM fits.}
}
\details{The stages are regression, Spearman association, percentile class agreement, and optional richness-controlled residual association. Class boundaries use type seven quantiles at one third and two thirds. Values above the upper boundary are High, values at or below the lower boundary are Low, and other values are Medium. Ties can produce unequal class sizes. Mean class Jaccard agreement excludes classes absent from both classifications. All requested stages use the same complete finite observations; source row numbers and exclusions are returned. No imputation, transformation or automatic model selection is performed.

Grouped GAMs include a random intercept, not an explicit temporal or spatial residual correlation model. Spearman coefficients summarize pooled rank association descriptively. No Spearman significance tests or P-values are computed. Residual association depends on the fitted richness models and does not establish biological equivalence or universal independence from richness.}
\value{A list containing settings, retained sample size, excluded rows, fitted models, fit statistics, correlation tests, agreement summaries, class details, observation-level results, and an interpretation note. GAM adjusted R squared and deviance explained are reported separately.}
\examples{
set.seed(10)
d <- data.frame(first = runif(40), second = runif(40),
                richness = rep(1:10, 4))
result <- compare_metrics(d, "first", "second", "richness")
result$agreement
}
