# Development implementation based on the supplied manuscript workflow.
compare_metrics <- function(data, response, predictor, richness = NULL,
                            model = c("quadratic", "gam"), group = NULL, k = 5) {
  model <- match.arg(model)
  columns <- c(response, predictor, richness, group)
  if (!is.data.frame(data) || !all(columns %in% names(data)))
    stop("Supply a data frame containing all selected columns.")
  numeric_columns <- c(response, predictor, richness)
  if (length(unique(numeric_columns)) != length(numeric_columns))
    stop("Response, predictor and richness must be distinct columns.")
  if (!all(vapply(data[c(response, predictor, richness)], is.numeric, logical(1))))
    stop("Metric and richness columns must be numeric.")
  if (!is.null(group) && model != "gam")
    stop("Grouped observations require the GAM option.")
  d <- data.frame(y = data[[response]], x = data[[predictor]])
  if (!is.null(richness)) d$r <- data[[richness]]
  if (!is.null(group)) d$g <- factor(data[[group]])
  keep <- stats::complete.cases(d) & is.finite(d$y) & is.finite(d$x)
  if (!is.null(richness)) keep <- keep & is.finite(d$r)
  rows <- which(keep)
  d <- droplevels(d[keep, , drop = FALSE])
  if (nrow(d) < 6 || any(vapply(d[intersect(c("x", "y", "r"), names(d))], function(z) length(unique(z)) < 3, logical(1))))
    stop("At least six complete observations and three distinct values per numeric column are required.")
  if (model == "gam" && (length(k) != 1 || !is.finite(k) || k < 3 || k != as.integer(k)))
    stop("k must be an integer of at least three.")
  if (model == "gam" && any(vapply(d[intersect(c("x", "r"), names(d))], function(z) length(unique(z)) < k, logical(1))))
    stop("Too few distinct predictor or richness values for the requested k.")
  if (!is.null(group) && nlevels(d$g) < 2) stop("At least two groups are required.")
  fit <- function(outcome, input) {
    if (model == "quadratic") {
      return(stats::lm(stats::as.formula(paste(outcome, "~", input, "+ I(", input, "^2)")), data = d))
    }
    # mgcv recognises the smooth expressions in the formula environment.
    s <- mgcv::s
    rhs <- paste0("s(", input, ", k = ", k, ")")
    if (!is.null(group)) rhs <- paste(rhs, "+ s(g, bs = 're')")
    mgcv::gam(stats::as.formula(paste(outcome, "~", rhs)), data = d, method = "REML")
  }
  continuous <- fit("y", "x")
  rich_y <- if (!is.null(richness)) fit("y", "r") else NULL
  rich_x <- if (!is.null(richness)) fit("x", "r") else NULL
  classify <- function(z) {
    q <- stats::quantile(z, c(1/3, 2/3), type = 7, names = FALSE)
    ifelse(z > q[2], 1L, ifelse(z <= q[1], 3L, 2L))
  }
  a <- classify(d$y); b <- classify(d$x)
  intersection <- vapply(1:3, function(j) sum(a == j & b == j), integer(1))
  union <- vapply(1:3, function(j) sum(a == j | b == j), integer(1))
  jaccard <- ifelse(union == 0, NA_real_, intersection / union)
  ry <- if (!is.null(richness)) stats::residuals(rich_y) else rep(NA_real_, nrow(d))
  rx <- if (!is.null(richness)) stats::residuals(rich_x) else rep(NA_real_, nrow(d))
  sm <- summary(continuous)
  list(settings = list(response = response, predictor = predictor, richness = richness,
                       model = model, group = group, k = k, quantile_type = 7),
       n = nrow(d), excluded_rows = which(!keep),
       continuous_model = continuous,
       fit_statistics = if (model == "gam") c(adjusted_r_squared = sm$r.sq, deviance_explained = sm$dev.expl)
                        else c(r_squared = sm$r.squared, adjusted_r_squared = sm$adj.r.squared),
       spearman = list(estimate = c(rho = stats::cor(d$y, d$x, method = "spearman")),
                       method = "Descriptive Spearman rank association; no significance test"),
       agreement = c(global = mean(a == b), mean_class_jaccard = mean(jaccard, na.rm = TRUE)),
       classes = data.frame(class = c("High", "Medium", "Low"), intersection, union, jaccard),
       richness_models = list(response = rich_y, predictor = rich_x),
       step_four_status = if (is.null(richness)) "Omitted: richness control was not requested." else "Calculated",
       residual_spearman = if (!is.null(richness))
         list(estimate = c(rho = stats::cor(ry, rx, method = "spearman")),
              method = "Descriptive residual Spearman rank association; no significance test") else NULL,
       observations = data.frame(source_row = rows, d, response_class = a, predictor_class = b,
                                 response_residual = ry, predictor_residual = rx),
       interpretation = "Evidence is dataset-specific; no automatic index selection or equivalence claim.")
}
