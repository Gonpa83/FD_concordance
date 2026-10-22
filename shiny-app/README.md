# FD Concordance Shiny application

This app compares precomputed functional diversity indices; it does not compute them from raw trait or abundance data. It is separate from the personal FDiversity calculator.

## Run locally

Install R with mgcv, shiny and readxl. From the repository root, run `shiny::runApp("shiny-app")`. The public deployment URL is pending.

Use one row per sampling observation. Select two numeric index columns, optional richness and optional grouping identifier. Use an additive model with a group random intercept for repeated observations when appropriate for the design. Grouping is not a general correction for temporal or spatial dependence.

Raw and residual Spearman coefficients are descriptive and omit P-values. Regression P-values remain in model summaries. Input checks are computational safeguards, not guarantees of adequate sample size. No automatic redundancy verdict is assigned.

## Reports and data handling

Downloads contain figures, tables and a full report archive. The full archive also contains the uploaded table. Keep it appropriately private when the input data are non-public.

Uploads and generated reports use temporary server storage. The app does not deliberately persist uploads or make external data calls; a hosting provider's storage, logs, access controls and retention policies require separate review. Deploy only after those settings and the applicable hosting terms have been reviewed.

## Deployment

The app.R file and core folder form a standalone Shiny deployment directory. Deploy this folder to a Shiny-compatible service using an account controlled by the authors. No credentials belong in source control. No hosting subscription or deployment has been created by preparing this folder.

The two functions in core must remain byte-identical to the corresponding files in the package R directory. The app loads them directly so the hosted app does not need an unpublished package installation.

## Verification

The package's saved final check passed. Server checks reproduced the three manuscript datasets and verified optional richness control, clearing stale results, and report downloads. The author also confirmed a successful browser upload and analysis using the Guayas Q–FDis data. A deployed end-to-end upload, calculation and download check remains necessary.
