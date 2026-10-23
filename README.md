# FD Concordance

An R package for comparing two precomputed functional diversity metrics when overlap is suspected. It reports complementary evidence without automatically choosing an index or declaring universal equivalence.

## Development release

This is a development release prepared for publication. Manuscript reference comparisons reproduce saved results within numerical tolerance. Testing on another computer and verification of the public app deployment remain outstanding. This repository contains the R package and its companion Shiny application.

## Install

R and mgcv are required. mgcv is normally included with standard R installations. This repository is public. To install the package directly from GitHub, run:

```r
install.packages("remotes") # Only if missing.
remotes::install_github("Gonpa83/FD_concordance")
library(fdconcordance)
```

To run the included teaching example and generate a report:

```r
source(system.file("examples", "quick_start.R", package = "fdconcordance"))
```

The example uses artificial data; it does not reproduce the manuscript's empirical analyses. Installation of a downloaded GitHub ZIP differs from installation of an R source-package archive; the GitHub command above avoids that ambiguity.

## Companion Shiny application

Download or clone this repository. In R, set your working directory to the repository folder, then run:

```r
install.packages(c("shiny", "readxl")) # Only if missing.
shiny::runApp("shiny-app")
```

The app accepts CSV, TXT, TSV and Excel tables of precomputed index values. It provides the same calculations as the package, with file upload, column selection and report downloads. The web deployment link will be added after deployment has been verified. See shiny-app/README.md for deployment and privacy information.

## Citation

After installing the package, use `citation("fdconcordance")` for the software citation. A version-specific archive DOI can be added once a public release has been archived.

## One dataset and two metrics

Use one row per sampling observation. The selected index values must belong to the same observation. A grouping column identifies repeated observations from the same sampling unit. Richness control is optional.

```r
values <- read.csv("my_values.csv")
result <- compare_metrics(values, response = "FDis", predictor = "Rao",
                          richness = "Richness", model = "gam", group = "Label")
export_comparison(result, "my_new_report", title = "Rao and FDis")
```

For independent observations, omit group and choose a scientifically justified regression model. Omit richness to run only the first three stages. Excel files can be read separately with readxl before passing a data frame to compare_metrics. The package does not calculate diversity indices from trait matrices.

## Four analytical stages

1. Quadratic regression or GAM, optionally with a group random intercept.
2. Spearman rank association.
3. Percentile-based global and class Jaccard agreement.
4. Optional residual association after modelling both indices against richness.

No fixed redundancy threshold is used. Ties may produce unequal or empty percentile classes. Adjusted R squared and deviance explained are reported separately.

## Output

The export function creates an HTML report, PNG figures, a figure PDF, summary and class tables, observation data, residuals, ranks, fitted-curve values, excluded source-row numbers, model summaries and an R results file. It prevents accidental overwriting. Keep the whole output folder together to retain the HTML report's figures and links.

## Worked example

```r
source(system.file("examples", "quick_start.R", package = "fdconcordance"))
```

This uses small artificial teaching data. It is not a biological simulation study or evidence supporting the manuscript's ecological conclusions.

## Interpretation

Technical minimum-data checks do not establish scientifically adequate sample size. Consider independent groups, replication, model complexity, variation and required precision. Group random intercepts do not model every form of temporal or spatial dependence. Raw-index and residual Spearman coefficients are reported descriptively. Grouping in the regression models does not adjust the pooled correlations. Residual association depends on the fitted richness models. Findings apply to the examined dataset and do not establish ecological equivalence.

Curve bands are approximate pointwise 95 percent intervals for the fitted mean, not prediction intervals. Grouped curves exclude the group random intercept; reported fit statistics include fitted group effects.

## Authors and license

Gonzalo Sotomayor, Marie Anne Eurie Forio, Christine Van der heyden, Patricio X. Lozano Rodríguez, and Peter L. M. Goethals.

GPL-3. Full license text is in COPYING. External dependencies retain their own licenses. This release contains no empirical research datasets or computer-specific analysis paths.

