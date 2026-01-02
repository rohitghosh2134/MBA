# Q8.R - Effect of Critics' Opinion on Total U.S. Gross

library(readxl)

movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

wrap_backticks <- function(x) {
  is_simple <- grepl("^[[:alnum:]_\\.]+$", x)
  x[!is_simple] <- paste0("`", x[!is_simple], "`")
  x
}

cat("QUESTION 8 – EFFECT OF CRITICS' OPINION\n\n")

candidate_vars <- c(
  "Budget",
  "COMEDY_DUMMY",
  "MPAA_D",
  "Sequel",
  "Known Story",
  "Opening Theatres",
  "Summer",
  "Holiday",
  "Christmas",
  "Opening Gross",
  "Critics´ Opinion"
)

candidate_vars_for_formula <- wrap_backticks(candidate_vars)

formula_initial <- as.formula(
  paste("`Total U.S. Gross` ~", paste(candidate_vars_for_formula, collapse = " + "))
)

model_initial <- lm(formula_initial, data = movies)

cat("Initial regression model (all pre & post opening factors):\n")
print(summary(model_initial))
cat("\n")

# Identify significant variables at 10% level
coef_table <- summary(model_initial)$coefficients
p_values   <- coef_table[-1, 4]
term_names <- rownames(coef_table)[-1]

signif_terms <- term_names[p_values <= 0.10]
if (length(signif_terms) == 0) {
  signif_terms <- term_names[which.min(p_values)]
}

dropped_terms <- setdiff(term_names, signif_terms)

formula_final <- as.formula(
  paste("`Total U.S. Gross` ~", paste(signif_terms, collapse = " + "))
)

model_final <- lm(formula_final, data = movies)

cat("Final regression model after dropping variables with p-value > 0.10:\n")
print(summary(model_final))
cat("\n")

cat("Dropped variables (p-value > 0.10 in initial model):\n")
print(dropped_terms)
cat("\n")

# Prediction for 'Flags of Our Fathers'
flags_data <- movies[movies$Movie == "Flags of Our Fathers", ]

if (nrow(flags_data) == 0) {
  cat("WARNING: No movie named 'Flags of Our Fathers' found in the dataset.\n\n")
} else {
  pred_flags <- predict(model_final,
                        newdata = flags_data,
                        interval = "prediction",
                        level = 0.95)
  
  cat("Prediction for Total U.S. Gross of 'Flags of Our Fathers':\n")
  print(pred_flags)
  cat("\n")
}

# -------------------------------
# Effect of +10 critics score points
# -------------------------------
final_coef_table <- coef(summary(model_final))
final_terms      <- rownames(final_coef_table)

crit_index <- grep("Critics", final_terms, ignore.case = TRUE)

cat("EFFECT OF CRITICS' OPINION:\n")

if (length(crit_index) == 0) {
  cat("'Critics´ Opinion' is NOT in the final model (dropped, p > 0.10).\n")
  cat("=> No marginal effect can be computed.\n\n")
} else {
  
  beta_crit <- final_coef_table[crit_index, 1]
  se_crit   <- final_coef_table[crit_index, 2]
  
  delta_revenue_point <- beta_crit * 10
  
  df_final <- model_final$df.residual
  t_crit   <- qt(0.975, df = df_final)
  delta_low  <- (beta_crit - t_crit * se_crit) * 10
  delta_high <- (beta_crit + t_crit * se_crit) * 10
  
  cat("Effect of a +10 point increase in critics' score:\n")
  cat("Point estimate of extra Total U.S. Gross: ", delta_revenue_point, "\n")
  cat("95% CI for this extra gross: [", delta_low, ", ", delta_high, "]\n\n")
}

# -------------------------------
# Interpretation Section
# -------------------------------
cat("INTERPRETATION (α = 0.05):\n")

if (length(crit_index) == 0) {
  cat("Critics' score does not statistically influence Total U.S. Gross.\n",
      "Movie success is driven more by other factors in this model.\n")
} else {
  if (final_coef_table[crit_index, 4] < 0.05) {
    cat("Critics' rating significantly affects Total U.S. Gross (p < 0.05).\n",
        "Improving critics' review score is associated with higher revenue.\n")
  } else {
    cat("Critics' rating shows no statistically significant effect at 5% level.\n",
        "Adding +10 review score may not reliably change Total U.S. Gross.\n")
  }
}
