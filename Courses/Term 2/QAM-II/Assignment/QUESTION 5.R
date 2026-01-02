# Q5.R - Regression for Total U.S. Gross with pre-production variables

library(readxl)

movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

# Helper for backticks (for variable names with spaces etc.)
wrap_backticks <- function(x) {
  is_simple <- grepl("^[[:alnum:]_\\.]+$", x)
  x[!is_simple] <- paste0("`", x[!is_simple], "`")
  x
}

cat("QUESTION 5 – PRE-PRODUCTION REGRESSION AND VARIABLE SELECTION\n\n")

# Candidate predictors
candidate_vars <- c("Budget", "COMEDY_DUMMY", "MPAA_D", "Sequel", "Known Story")
candidate_vars_for_formula <- wrap_backticks(candidate_vars)

# Initial model
formula_initial <- as.formula(
  paste("`Total U.S. Gross` ~", paste(candidate_vars_for_formula, collapse = " + "))
)

model_initial <- lm(formula_initial, data = movies)

cat("Initial regression model:\n")
print(summary(model_initial))
cat("\n")

# Extract p-values (excluding intercept)
coef_table <- summary(model_initial)$coefficients
p_values   <- coef_table[-1, 4]
term_names <- names(p_values)

# Keep variables with p <= 0.10 (10% significance level)
signif_vars <- term_names[p_values <= 0.10]

# If none pass the 0.10 cutoff, keep the single most significant one
if (length(signif_vars) == 0) {
  signif_vars <- term_names[which.min(p_values)]
}

dropped_vars <- setdiff(term_names, signif_vars)

# Final model with only significant variables
signif_vars_for_formula <- wrap_backticks(signif_vars)
formula_final <- as.formula(
  paste("`Total U.S. Gross` ~", paste(signif_vars_for_formula, collapse = " + "))
)

model_final <- lm(formula_final, data = movies)

cat("Final regression model after dropping variables with p-value > 0.10:\n")
print(summary(model_final))
cat("\n")

cat("Dropped variables (p-value > 0.10 in initial model):\n")
print(dropped_vars)
cat("\n")

# Sequel effect (if retained)
final_coef_table <- coef(summary(model_final))

if ("Sequel" %in% rownames(final_coef_table)) {
  cat("Sequel effect in the final model:\n")
  print(final_coef_table["Sequel", ])
} else {
  cat("Sequel was removed because its p-value exceeded 0.10 (not statistically significant at 10% level).\n")
}
