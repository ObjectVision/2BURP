# Last changed 2024-12-03

results_tables <- list()

for (continent in continents) {
  print(paste0("Making results tables for ", continent))

  #>>>>>>>>>>>>>>> Results table

  # Read in data
  model_glm_down <- readRDS(paste0(folder_out, continent, "/results/model_glm_down_", marker, ".rds"))

  coefficients <- summary(model_glm_down)$coefficients
  standard_errors <- summary(model_glm_down)$standard.errors
  z_values <- coefficients / standard_errors
  p_values <- 2 * (1 - pnorm(abs(z_values)))

  # Create results table
  results_table <- data.frame(
    "Coefficient" = coefficients,
    "Standard_Error" = standard_errors,
    "Z_Value" = z_values,
    "P_Value" = p_values
  )

  # Save results table
  results_tables[[continent]] <- results_table

  fwrite(
    results_table,
    file = paste0(folder_out, continent, "/results/results_table_", marker, ".csv")
  )

  #>>>>>>>>>>>>>>> Performance metrics

  # Read in data
  BalancedLogLoss50 <- fread(paste0(folder_out, continent, "/results/BalancedLogLoss50_", marker, ".csv"))

  byClass <- fread(paste0(folder_out, continent, "/results/byClass_glm_down_", marker, ".csv"))
  twoxtable <- fread(paste0(folder_out, continent, "/results/table_glm_down_", marker, ".csv"))

  if (ncol(twoxtable) == 3) {
    message("twoxtable has 3 columns. Using the second and third columns.")
    twoxtable <- twoxtable[, 2:3]
  } else if (ncol(twoxtable) > 3) {
    warning("twoxtable has more than 3 columns. Only using the first two columns.")
  }
  twoxtable <- as.matrix(twoxtable)

  # Create performance metrics table
  metrics_table <- data.frame(
    "WeightDecay" = BalancedLogLoss50$decay,
    "TruePos" = twoxtable[2, 2],
    "FalsePos" = twoxtable[2, 1],
    "FalseNeg" = twoxtable[1, 2],
    "TrueNeg" = twoxtable[1, 1]
  ) %>%
    mutate(
      "FNR" = FalseNeg / (FalseNeg + TruePos),
      "FPR" = FalsePos / (FalsePos + TrueNeg),
      "BalancedLogLoss" = BalancedLogLoss50$BalancedLogLoss50,
      "Accuracy" = (TruePos + TrueNeg) / (TruePos + TrueNeg + FalsePos + FalseNeg),
      "BalancedAccuracy" = byClass[11, 2],
      "Jaccard" = TruePos / (TruePos + FalsePos + FalseNeg)
    ) %>%
    t()

  if (!exists("performance_metrics")) {
    performance_metrics <- data.frame(
      "Metric" = rownames(metrics_table)
    )
  }
  performance_metrics[[continent]] <- metrics_table

  fwrite(
    metrics_table,
    file = paste0(folder_out, continent, "/results/performance_metrics_", marker, ".csv")
  )
}

# Store full performance metrics table
fwrite(performance_metrics, file = paste0(folder_out, "performance_metrics_", marker, ".csv"))

# >>>>>>>>>>>>>>>> Combine results tables

combine_results <- function(results_tables, value_name) {
  combined <- lapply(results_tables, function(x) data.frame(variable = rownames(x), value = x[[value_name]]))
  combined <- dcast(
    rbindlist(combined, idcol = "continent"),
    variable ~ continent,
    value.var = "value"
  )
  return(combined)
}

print("Combining results tables")
coefficients <- combine_results(results_tables, "Coefficient")
cat("Coefficients: ", nrow(coefficients), " rows\n")
standard_errors <- combine_results(results_tables, "Standard_Error")
cat("Standard errors: ", nrow(standard_errors), " rows\n")
p_values <- combine_results(results_tables, "P_Value")
cat("P_values: ", nrow(p_values), " rows\n")

# Save combined results tables
#' I would really love for something like stargazer to auto format a nice table
#' but it doesn't support caret models and I have not found a workaround yet.
if (exists("all_vars")) {
  print("Found all_vars. Sorting results tables.")
  coefnames <- append("(Intercept)", all_vars)
  coefnames <- coefnames[!coefnames %in% c("Urban_2020_025", "latitude_factor")]

  coefficients <- coefficients[match(all_vars, coefficients$variable), ]
  standard_errors <- standard_errors[match(all_vars, standard_errors$variable), ]
  p_values <- p_values[match(all_vars, p_values$variable), ]

  fwrite(coefficients, file = paste0(folder_out, "coefficients_sorted_", marker, ".csv"))
  fwrite(standard_errors, file = paste0(folder_out, "standard_errors_sorted_", marker, ".csv"))
  fwrite(p_values, file = paste0(folder_out, "p_values_sorted_", marker, ".csv"))
} else {
  # Save unsorted results tables
  fwrite(coefficients, file = paste0(folder_out, "coefficients_", marker, ".csv"))
  fwrite(standard_errors, file = paste0(folder_out, "standard_errors_", marker, ".csv"))
  fwrite(p_values, file = paste0(folder_out, "p_values_", marker, ".csv"))
}