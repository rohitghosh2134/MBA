# Q6.R - Opening weekend box-office model with preproduction + opening-weekend factors

library(readxl)

# Load movie data
movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

# Helper: wrap backticks around variable names that contain spaces or special characters
wrap_backticks <- function(x) {
  is_simple <- grepl("^[[:alnum:]_\\.]+$", x)
  x[!is_simple] <- paste0("`", x[!is_simple], "`")
  x
}

cat("QUESTION 6 – REGRESSION MODEL FOR OPENING GROSS\n")
cat("Including BOTH preproduction factors and opening-weekend factors\n\n")

###############################################################
# a. INITIAL REGRESSION MODEL
###############################################################

candidate_vars <- c(
  "Budget",
  "COMEDY_DUMMY",
  "MPAA_D",
  "Sequel",
  "Known Story",
  "Opening Theatres",
  "Summer",
  "Holiday",
  "Christmas"
)

candidate_vars_for_formula <- wrap_backticks(candidate_vars)

formula_initial <- as.formula(
  paste("`Opening Gross` ~", paste(candidate_vars_for_formula, collapse = " + "))
)

model_initial <- lm(formula_initial, data = movies)

cat("INITIAL REGRESSION MODEL:\n")
print(summary(model_initial))
cat("\n")

###############################################################
# b. DROP VARIABLES WITH p-value > 0.10
###############################################################

coef_table <- summary(model_initial)$coefficients

# Remove intercept row
p_values   <- coef_table[-1, 4]
term_names <- rownames(coef_table)[-1]

# Keep variables with p-value <= 0.10
signif_terms <- term_names[p_values <= 0.10]

# If none are significant, keep the single best predictor
if (length(signif_terms) == 0) {
  signif_terms <- term_names[which.min(p_values)]
}

# Variables dropped from the model
dropped_terms <- setdiff(term_names, signif_terms)

# Build final regression formula using only significant predictors
formula_final <- as.formula(
  paste("`Opening Gross` ~", paste(signif_terms, collapse = " + "))
)

model_final <- lm(formula_final, data = movies)

cat("FINAL REGRESSION MODEL (after dropping variables with p-value > 0.10):\n")
print(summary(model_final))
cat("\n")

cat("Variables dropped at 10% significance level (p-value > 0.10):\n")
print(dropped_terms)
cat("\n")

###############################################################
# c. INTERPRETATION OF SLOPE COEFFICIENTS
###############################################################

cat("INTERPRETATION OF SLOPE COEFFICIENTS (Final Model):\n\n")

final_coef <- coef(summary(model_final))
rn <- rownames(final_coef)

interpret_var <- function(var_name, text_intro) {
  idx <- which(rn == var_name)
  if (length(idx) == 1) {
    est <- final_coef[idx, 1]
    cat(text_intro, est, "\n")
  }
}

# Budget
interpret_var("Budget",
              "Budget: Holding other variables constant, a one-dollar increase in production budget is associated with an expected change in opening gross of "
)

# COMEDY_DUMMY
interpret_var("COMEDY_DUMMY",
              "COMEDY_DUMMY (1 = Comedy vs 0 = Non-Comedy): Holding other variables constant, a comedy is expected to have higher (if positive) or lower (if negative) opening gross by "
)

# MPAA_D
interpret_var("MPAA_D",
              "MPAA_D (1 = R-rated vs 0 = Others): Holding other variables constant, R-rated movies are expected to differ in opening gross by "
)

# Sequel
interpret_var("Sequel",
              "Sequel (1 = Sequel vs 0 = Not a sequel): Holding other variables constant, sequels are expected to differ in opening gross by "
)

# Known Story
interpret_var("`Known Story`",
              "`Known Story` (1 = Based on known story vs 0 = Original): Holding other variables constant, movies based on a known story are expected to differ in opening gross by "
)

# Opening Theatres
interpret_var("`Opening Theatres`",
              "`Opening Theatres`: Holding other variables constant, each additional theatre in the opening weekend is associated with an expected change in opening gross of "
)

# Summer
interpret_var("Summer",
              "Summer (1 = Released in summer vs 0 = Not summer): Holding other variables constant, summer releases are expected to differ in opening gross by "
)

# Holiday
interpret_var("Holiday",
              "Holiday (1 = Holiday release vs 0 = Not holiday): Holding other variables constant, holiday releases are expected to differ in opening gross by "
)

# Christmas
interpret_var("Christmas",
              "Christmas (1 = Christmas release vs 0 = Non-Christmas): Holding other variables constant, Christmas releases are expected to differ in opening gross by "
)

cat("\n(Note: Signs of the coefficients indicate direction: positive = higher opening gross, negative = lower opening gross, all else equal.)\n\n")

###############################################################
# d. EFFECT OF +100 THEATRES (POINT ESTIMATE + 95% CI)
###############################################################

cat("EFFECT OF INCREASING OPENING THEATRES BY 100:\n\n")

theatre_index <- grep("Opening Theatres", rn, fixed = TRUE)

if (length(theatre_index) == 1) {
  
  theatre_coef <- final_coef[theatre_index, 1]
  theatre_se   <- final_coef[theatre_index, 2]
  
  # Point estimate for 100 extra theatres
  point_estimate <- theatre_coef * 100
  
  # 95% CI using t-distribution
  df <- model_final$df.residual
  t_critical <- qt(0.975, df = df)
  CI_low  <- (theatre_coef - t_critical * theatre_se) * 100
  CI_high <- (theatre_coef + t_critical * theatre_se) * 100
  
  cat("Point estimate of change in Opening Gross for +100 theatres: ", point_estimate, "\n")
  cat("95% confidence interval for this change: [", CI_low, ", ", CI_high, "]\n")
  
} else {
  cat("`Opening Theatres` is not in the final model (p > 0.10).\n")
  cat("Therefore, the effect of +100 theatres and its confidence interval cannot be computed from this final regression.\n")
}
