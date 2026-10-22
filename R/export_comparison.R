# Export the result of compare_metrics without refitting any model.
export_comparison <- function(result, directory, title = "Functional diversity concordance") {
  if (!is.list(result) || !all(c("settings", "observations", "continuous_model", "agreement") %in% names(result)))
    stop("Supply the result of compare_metrics.")
  if (length(directory) != 1L || is.na(directory) || !nzchar(directory)) stop("Supply one output directory.")
  if (length(title) != 1L || is.na(title)) stop("Supply one report title.")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) stop("Cannot create output directory.")
  d <- result$observations
  settings <- result$settings
  xname <- settings$predictor; yname <- settings$response
  controlled <- !is.null(result$residual_spearman)
  grouped <- !is.null(settings$group)
  stage_names <- c("Step1_Regression", "Step2_Ranks", "Step3_Class_Agreement",
                   if (controlled) "Step4_Residuals")
  generated <- c("RESULTS_SUMMARY.csv", "observations.csv", "class_agreement.csv",
                 "class_cross_table.csv", "regression_curve.csv", "rank_plot_data.csv",
                 "excluded_rows.csv", "complete_result.rds", "READ_RESULTS.txt",
                 "figures.pdf", "OPEN_REPORT.html", paste0(stage_names, ".png"))
  if (any(file.exists(file.path(directory, generated))))
    stop("Output files already exist. Choose a new directory to preserve the earlier run.")
  write_table <- function(value, filename, ...) utils::write.csv(value, file.path(directory, filename), ...)
  summary_table <- data.frame(
    Step = c("Data", "Data", "Data", rep("1 Regression", length(result$fit_statistics)),
             "2 Spearman", "3 Agreement", "3 Agreement", if (controlled) "4 Richness control"),
    Statistic = c("Observations used", "Observations excluded", "Groups",
                  names(result$fit_statistics), "Spearman rho", "Global agreement",
                  "Mean class Jaccard", if (controlled) "Residual Spearman rho"),
    Value = c(result$n, length(result$excluded_rows), if (grouped) nlevels(d$g) else NA_real_,
              result$fit_statistics, unname(result$spearman$estimate), result$agreement,
              if (controlled) unname(result$residual_spearman$estimate)))
  model_summary <- summary(result$continuous_model)
  if (inherits(result$continuous_model, "gam")) {
    model_p <- unname(model_summary$s.table["s(x)", "p-value"])
    model_p_label <- "Predictor smooth P-value"
  } else {
    f <- model_summary$fstatistic
    model_p <- unname(stats::pf(f[1], f[2], f[3], lower.tail = FALSE))
    model_p_label <- "Overall model P-value (F-test)"
  }
  p_rows <- data.frame(
    Step = "1 Regression",
    Statistic = model_p_label,
    Value = model_p)
  summary_table <- rbind(summary_table, p_rows)
  summary_table <- summary_table[order(match(summary_table$Step,
    c("Data", "1 Regression", "2 Spearman", "3 Agreement", "4 Richness control"))), ]
  rownames(summary_table) <- NULL
  write_table(summary_table, "RESULTS_SUMMARY.csv", row.names = FALSE)
  observations <- data.frame(source_row = d$source_row)
  observations[[xname]] <- d$x
  observations[[yname]] <- d$y
  if (!is.null(settings$richness)) observations[[settings$richness]] <- d$r
  if (grouped) observations[[settings$group]] <- d$g
  observations$predictor_class <- d$predictor_class
  observations$response_class <- d$response_class
  if (controlled) {
    observations$predictor_residual <- d$predictor_residual
    observations$response_residual <- d$response_residual
  }
  write_table(observations, "observations.csv", row.names = FALSE)
  write_table(result$classes, "class_agreement.csv", row.names = FALSE)
  write_table(data.frame(source_row = result$excluded_rows), "excluded_rows.csv", row.names = FALSE)
  counts <- table(predictor_class = factor(d$predictor_class, levels = c(3,2,1), labels = c("Low","Medium","High")),
                  response_class = factor(d$response_class, levels = c(3,2,1), labels = c("Low","Medium","High")))
  write_table(as.data.frame.matrix(counts), "class_cross_table.csv")
  grid <- data.frame(x = seq(min(d$x), max(d$x), length.out = 200))
  if (grouped) grid$g <- factor(levels(d$g)[1], levels = levels(d$g))
  prediction <- if (grouped) stats::predict(result$continuous_model, newdata = grid, exclude = "s(g)", se.fit = TRUE)
                else stats::predict(result$continuous_model, newdata = grid, se.fit = TRUE)
  curve <- data.frame(predictor = grid$x, fitted_response = as.numeric(prediction$fit),
                      lower_95_mean = as.numeric(prediction$fit - 1.96 * prediction$se.fit),
                      upper_95_mean = as.numeric(prediction$fit + 1.96 * prediction$se.fit))
  write_table(curve, "regression_curve.csv", row.names = FALSE)
  ranks <- data.frame(source_row = d$source_row, predictor = d$x, response = d$y,
                      predictor_rank = rank(d$x), response_rank = rank(d$y))
  write_table(ranks, "rank_plot_data.csv", row.names = FALSE)
  saveRDS(result, file.path(directory, "complete_result.rds"))
  note <- paste("These are dataset-specific associations, not an automatic redundancy verdict.",
                "Raw-index and residual Spearman coefficients are descriptive; no Spearman P-values or significance tests are reported. Grouped regression does not adjust these pooled coefficients for group structure.",
                "The curve band is an approximate pointwise 95% interval for the fitted mean, not a prediction interval.",
                if (grouped) "The curve excludes the group random intercept; the reported model fit includes it." else "",
                if (!controlled) "Richness control was omitted." else "")
  utils::capture.output({
    cat(title, "\n\n"); print(summary_table, row.names = FALSE)
    cat("\nSETTINGS\n"); print(settings); cat("\n", note, "\n")
    cat("\nREGRESSION\n"); print(summary(result$continuous_model))
    cat("\nSPEARMAN\n"); print(result$spearman)
    if (controlled) {
      cat("\nRICHNESS MODELS\n"); print(summary(result$richness_models$response))
      print(summary(result$richness_models$predictor))
      cat("\nRESIDUAL SPEARMAN\n"); print(result$residual_spearman)
    }
    cat("\nSESSION\n"); print(utils::sessionInfo())
  }, file = file.path(directory, "READ_RESULTS.txt"))
  draw <- function(stage) {
    graphics::par(mar = c(5,5,5,2))
    if (stage == 1L) {
      graphics::plot(d$x, d$y, type = "n", ylim = range(d$y, curve$lower_95_mean, curve$upper_95_mean),
                     xlab = xname, ylab = yname, main = "1. Continuous relationship")
      graphics::polygon(c(curve$predictor, rev(curve$predictor)),
                        c(curve$lower_95_mean, rev(curve$upper_95_mean)), col = "#DBE9F4", border = NA)
      graphics::points(d$x,d$y,pch=16,col="#243F5F80",cex=.75)
      graphics::lines(curve$predictor,curve$fitted_response,col="#A33E22",lwd=2)
      graphics::mtext(sprintf("%s = %.3f", names(result$fit_statistics)[1],result$fit_statistics[1]),side=3,line=.3)
    } else if (stage == 2L) {
      graphics::plot(ranks$predictor_rank,ranks$response_rank,pch=16,col="#243F5F80",
                     xlab=paste(xname,"rank"),ylab=paste(yname,"rank"),main="2. Rank association")
      graphics::mtext(sprintf("Spearman rho = %.3f",result$spearman$estimate),side=3,line=.3)
    } else if (stage == 3L) {
      graphics::par(mar=c(5,7,5,2))
      graphics::plot(c(.5,3.5),c(.5,3.5),type="n",axes=FALSE,xlab=paste(xname,"class"),ylab="",
                     main="3. Percentile class agreement",asp=1)
      shades <- grDevices::colorRampPalette(c("#F0F5FA","#2A6085"))(101)
      for (i in 1:3) for (j in 1:3) {
        relative <- counts[i,j]/max(counts)
        graphics::rect(i-.48,j-.48,i+.48,j+.48,col=shades[1+round(100*relative)],border="white")
        graphics::text(i,j,counts[i,j],col=if(relative>.55) "white" else "black")
      }
      graphics::axis(1,1:3,c("Low","Medium","High"),cex.axis=.85)
      graphics::axis(2,1:3,c("Low","Medium","High"),las=1,cex.axis=.85)
      graphics::mtext(paste(yname,"class"),side=2,line=5)
      graphics::mtext(sprintf("Global = %.3f; mean class Jaccard = %.3f; cells are counts",
                              result$agreement[1],result$agreement[2]),side=3,line=.3,cex=.7)
    } else {
      graphics::plot(d$predictor_residual,d$response_residual,pch=16,col="#243F5F80",
                     xlab=paste(xname,"residual"),ylab=paste(yname,"residual"),
                     main="4. Richness-controlled association")
      graphics::abline(h=0,v=0,col="grey75",lty=2)
      graphics::mtext(sprintf("Residual Spearman rho = %.3f",result$residual_spearman$estimate),side=3,line=.3)
    }
  }
  with_device <- function(open, code) {
    open(); on.exit(grDevices::dev.off()); force(code)
  }
  for (i in seq_along(stage_names)) {
    local({
      stage <- i
      with_device(function() grDevices::png(file.path(directory,paste0(stage_names[stage],".png")),
                                            width=1800,height=1400,res=200), draw(stage))
    })
  }
  with_device(function() grDevices::pdf(file.path(directory,"figures.pdf"),width=8,height=6.5),
              for (i in seq_along(stage_names)) draw(i))
  escape <- function(s) {
    s <- gsub("&","&amp;",s,fixed=TRUE)
    s <- gsub("<","&lt;",s,fixed=TRUE)
    s <- gsub(">","&gt;",s,fixed=TRUE)
    gsub('"',"&quot;",s,fixed=TRUE)
  }
  rows <- vapply(seq_len(nrow(summary_table)),function(i) {
    value <- summary_table$Value[i]
    shown <- if (!is.na(value) && grepl("P-value", summary_table$Statistic[i], fixed = TRUE) && value < .001)
      "< 0.001" else format(value, digits = 6)
    paste0("<tr><td>",escape(summary_table$Step[i]),
      "</td><td>",escape(summary_table$Statistic[i]),"</td><td>",escape(shown),"</td></tr>")
  },character(1))
  html <- c('<!doctype html><html lang="en"><meta charset="utf-8">',
    paste0('<title>',escape(title),'</title>'),
    '<style>body{font:18px Arial;max-width:1000px;margin:35px auto;padding:15px;color:#243044}img{max-width:100%}table{border-collapse:collapse}td,th{padding:10px;border:1px solid #bbb}</style>',
    paste0('<h1>',escape(title),'</h1><p>Response: ',escape(yname),'; predictor: ',escape(xname),
           '; model: ',escape(settings$model),'; grouping: ',escape(if(grouped) settings$group else 'none'),
           '; richness control: ',escape(if(controlled) settings$richness else 'omitted'),'.</p>'),
    '<table><tr><th>Step</th><th>Statistic</th><th>Value</th></tr>',rows,'</table>',
    paste0('<p>',escape(note),'</p>'),
    '<p><a href="RESULTS_SUMMARY.csv">Summary table</a> | <a href="observations.csv">Observation data and residuals</a> | <a href="class_agreement.csv">Class Jaccard table</a> | <a href="class_cross_table.csv">Classification counts</a> | <a href="regression_curve.csv">Curve data</a> | <a href="rank_plot_data.csv">Rank data</a> | <a href="READ_RESULTS.txt">Full model results</a> | <a href="figures.pdf">Figures PDF</a></p>',
    paste0('<img src="',stage_names,'.png" alt="',stage_names,'">'),'</html>')
  writeLines(html,file.path(directory,"OPEN_REPORT.html"))
  invisible(list(directory=normalizePath(directory,winslash="/"),files=generated,summary=summary_table))
}
