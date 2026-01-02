library(readxl)
library(dplyr)

movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

names(movies)

###############################################
# QUESTION 1 SOLUTION
# Summary statistics and frequency counts
# WITHOUT USING dplyr
###############################################

# View column names (already given)
names(movies)

# Minimum, Mean, and Maximum for required numeric variables
min_opening_gross    <- min(movies$`Opening Gross`, na.rm = TRUE)
avg_opening_gross    <- mean(movies$`Opening Gross`, na.rm = TRUE)
max_opening_gross    <- max(movies$`Opening Gross`, na.rm = TRUE)

min_total_us_gross   <- min(movies$`Total U.S. Gross`, na.rm = TRUE)
avg_total_us_gross   <- mean(movies$`Total U.S. Gross`, na.rm = TRUE)
max_total_us_gross   <- max(movies$`Total U.S. Gross`, na.rm = TRUE)

min_total_nonus_gross <- min(movies$`Total Non-U.S. Gross`, na.rm = TRUE)
avg_total_nonus_gross <- mean(movies$`Total Non-U.S. Gross`, na.rm = TRUE)
max_total_nonus_gross <- max(movies$`Total Non-U.S. Gross`, na.rm = TRUE)

min_opening_theatres <- min(movies$`Opening Theatres`, na.rm = TRUE)
avg_opening_theatres <- mean(movies$`Opening Theatres`, na.rm = TRUE)
max_opening_theatres <- max(movies$`Opening Theatres`, na.rm = TRUE)

# COUNT how many movies are comedies
# COMEDY_DUMMY = 1 means comedy, 0 means not comedy
num_comedies <- sum(movies$COMEDY_DUMMY == 1, na.rm = TRUE)

# COUNT how many movies are R-rated
# MPAA variable contains rating categories as text
num_R_rated <- sum(movies$MPAA == "R", na.rm = TRUE)

# Print Results
min_opening_gross; avg_opening_gross; max_opening_gross
min_total_us_gross; avg_total_us_gross; max_total_us_gross
min_total_nonus_gross; avg_total_nonus_gross; max_total_nonus_gross
min_opening_theatres; avg_opening_theatres; max_opening_theatres

num_comedies
num_R_rated


###############################################################
# QUESTION 2 SOLUTION
# ROI CALCULATION, 95% CI, AND HYPOTHESIS TEST VS 12% BENCHMARK
# WITHOUT USING dplyr
###############################################################

# a. Calculate U.S. ROI: (Total U.S. Gross - Budget) / Budget
movies$US_ROI <- (movies$`Total U.S. Gross` - movies$Budget) / movies$Budget

# Display first few ROI values
head(movies$US_ROI)

# b. 95% confidence interval for mean U.S. ROI
mean_roi <- mean(movies$US_ROI, na.rm = TRUE)
sd_roi   <- sd(movies$US_ROI, na.rm = TRUE)
n        <- sum(!is.na(movies$US_ROI))   # valid sample size

# Standard error
se_roi <- sd_roi / sqrt(n)

# 95% CI using t-distribution
t_value <- qt(0.975, df = n - 1)
CI_lower <- mean_roi - t_value * se_roi
CI_upper <- mean_roi + t_value * se_roi

mean_roi
CI_lower
CI_upper

# c. Hypothesis test to show mean ROI > 12% (0.12)
# H0: mean ROI = 0.12
# H1: mean ROI > 0.12
t_statistic <- (mean_roi - 0.12) / se_roi
p_value <- 1 - pt(t_statistic, df = n - 1)  # one-tailed test

t_statistic
p_value

###############################################################
# INTERPRETATION GUIDE (not printed but for explanation):
# If p-value < 0.05, reject H0 and conclude mean ROI is significantly
# greater than 12%
###############################################################


###############################################################
# QUESTION 3 SOLUTION
# COMPARISON BETWEEN COMEDY VS NON-COMEDY
# WITHOUT USING dplyr
###############################################################

# a. Compare Total U.S. Gross of Comedies vs Non-Comedies

# Split into two groups based on COMEDY_DUMMY (1 = Comedy, 0 = Others)
comedy_us_gross     <- movies$`Total U.S. Gross`[movies$COMEDY_DUMMY == 1]
noncomedy_us_gross  <- movies$`Total U.S. Gross`[movies$COMEDY_DUMMY == 0]

# Basic statistics for comparison
mean_comedy_gross    <- mean(comedy_us_gross, na.rm = TRUE)
mean_noncomedy_gross <- mean(noncomedy_us_gross, na.rm = TRUE)

mean_comedy_gross
mean_noncomedy_gross

# Two-sample t-test for difference in means (unequal variances)
t_test_gross <- t.test(comedy_us_gross, noncomedy_us_gross,
                       alternative = "two.sided",
                       var.equal = FALSE)

t_test_gross


###############################################################
# b. Compare U.S. ROI of Comedies vs Non-Comedies
###############################################################

# Calculate ROI if not previously defined
if(!("US_ROI" %in% names(movies))){
  movies$US_ROI <- (movies$`Total U.S. Gross` - movies$Budget) / movies$Budget
}

comedy_roi     <- movies$US_ROI[movies$COMEDY_DUMMY == 1]
noncomedy_roi  <- movies$US_ROI[movies$COMEDY_DUMMY == 0]

# Means of ROI groups
mean_comedy_roi    <- mean(comedy_roi, na.rm = TRUE)
mean_noncomedy_roi <- mean(noncomedy_roi, na.rm = TRUE)

mean_comedy_roi
mean_noncomedy_roi

# Two-sample t-test for ROI difference
t_test_roi <- t.test(comedy_roi, noncomedy_roi,
                     alternative = "two.sided",
                     var.equal = FALSE)

t_test_roi

###############################################################
# INTERPRETATION GUIDE (not printed but explanatory notes)
# - Check p-values in t_test_gross and t_test_roi outputs
# - If p-value < 0.05, the difference between groups is statistically significant
###############################################################


###############################################################
# QUESTION 4 SOLUTION
# R-RATED VS OTHER MOVIES: TOTAL U.S. GROSS COMPARISON
# WITHOUT USING dplyr
###############################################################

# Separate groups based on MPAA rating variable
# Assuming MPAA contains categories such as "R", "PG-13", "PG", "G", etc.

r_rated_gross     <- movies$`Total U.S. Gross`[movies$MPAA == "R"]
non_r_rated_gross <- movies$`Total U.S. Gross`[movies$MPAA != "R"]

# Calculate group means
mean_r_rated_gross     <- mean(r_rated_gross, na.rm = TRUE)
mean_non_r_rated_gross <- mean(non_r_rated_gross, na.rm = TRUE)

mean_r_rated_gross
mean_non_r_rated_gross

# Perform two-sample t-test (unequal variances)
t_test_r_vs_others <- t.test(r_rated_gross,
                             non_r_rated_gross,
                             alternative = "two.sided",
                             var.equal = FALSE)

t_test_r_vs_others

###############################################################
# INTERPRETATION GUIDE (not printed but explanatory notes)
# If the p-value < 0.05, we conclude a significant difference
# between total U.S. gross of R-rated vs non-R-rated movies.
###############################################################

###############################################################
# QUESTION 5 SOLUTION (DYNAMIC DROPPING + REPORT DROPPED VARIABLES)
# REGRESSION MODEL FOR TOTAL U.S. GROSS AND VARIABLE SELECTION
# WITHOUT USING dplyr
###############################################################

# Helper function: wrap variable names with backticks when needed
wrap_backticks <- function(x) {
  is_simple <- grepl("^[[:alnum:]_\\.]+$", x)
  x[!is_simple] <- paste0("`", x[!is_simple], "`")
  x
}

###############################################################
# a. INITIAL REGRESSION MODEL
###############################################################

candidate_vars <- c("Budget", "COMEDY_DUMMY", "MPAA_D", "Sequel", "Known Story")

candidate_vars_for_formula <- wrap_backticks(candidate_vars)

formula_initial <- as.formula(
  paste("`Total U.S. Gross` ~", paste(candidate_vars_for_formula, collapse = " + "))
)

model_initial <- lm(formula_initial, data = movies)
summary(model_initial)

###############################################################
# b. DYNAMICALLY DROP VARIABLES (p-value > 0.10)
###############################################################

coef_table <- summary(model_initial)$coefficients
p_values <- coef_table[-1, 4]          # extract p-values (exclude intercept)
term_names <- names(p_values)          # variable names

signif_vars <- term_names[p_values <= 0.10]   # keep those <= 0.10

# If none significant, keep best one (smallest p)
if (length(signif_vars) == 0) {
  best_var <- term_names[which.min(p_values)]
  signif_vars <- best_var
}

# Determine dropped variables
dropped_vars <- setdiff(term_names, signif_vars)

# Build final regression formula
signif_vars_for_formula <- wrap_backticks(signif_vars)
formula_final <- as.formula(
  paste("`Total U.S. Gross` ~", paste(signif_vars_for_formula, collapse = " + "))
)

model_final <- lm(formula_final, data = movies)
summary(model_final)

###############################################################
# PRINT DROPPED VARIABLES
###############################################################
cat("Dropped variables (p-value > 0.10):\n")
print(dropped_vars)

###############################################################
# c. INTERPRETATION OF SEQUEL EFFECT (ONLY IF REMAINS)
###############################################################

final_coef_table <- coef(summary(model_final))

if ("Sequel" %in% rownames(final_coef_table)) {
  final_coef_table["Sequel", ]
} else {
  cat("\nSequel was removed because its p-value exceeded 0.10.\n")
}


###############################################################
# QUESTION 6 SOLUTION – FIXED VERSION
# Model: Opening Gross ~ Opening Theatres + Summer + Holiday + Christmas
# Dynamic dropping of variables with p-value > 0.10
# (NO dplyr used)
###############################################################

## Helper: wrap backticks ONLY for raw column names (not for coef names)
wrap_backticks <- function(x) {
  is_simple <- grepl("^[[:alnum:]_\\.]+$", x)
  x[!is_simple] <- paste0("`", x[!is_simple], "`")
  x
}

###############################################################
# a. INITIAL REGRESSION MODEL
###############################################################

# Only opening-weekend factors in the model
candidate_vars <- c("Opening Theatres", "Summer", "Holiday", "Christmas")

# For the formula we must backtick those with spaces
candidate_vars_for_formula <- wrap_backticks(candidate_vars)

# Build initial model: Opening Gross as dependent variable
formula_initial <- as.formula(
  paste("`Opening Gross` ~", paste(candidate_vars_for_formula, collapse = " + "))
)

model_initial <- lm(formula_initial, data = movies)
summary(model_initial)

###############################################################
# b. DYNAMICALLY DROP VARIABLES WITH p-value > 0.10
###############################################################

coef_table <- summary(model_initial)$coefficients

# Remove intercept row and extract p-values
p_values   <- coef_table[-1, 4]
term_names <- rownames(coef_table)[-1]   # these may already contain backticks

# Keep variables with p-value <= 0.10
signif_vars <- term_names[p_values <= 0.10]

# If none significant, keep the one with the smallest p-value
if (length(signif_vars) == 0) {
  signif_vars <- term_names[which.min(p_values)]
}

# Variables dropped from the model
dropped_vars <- setdiff(term_names, signif_vars)

# IMPORTANT: coef names already have correct backtick style,
# so we use them DIRECTLY in the formula (no extra wrapping)
formula_final <- as.formula(
  paste("`Opening Gross` ~", paste(signif_vars, collapse = " + "))
)

# Final model with only significant predictors
model_final <- lm(formula_final, data = movies)
summary(model_final)

###############################################################
# PRINT DROPPED VARIABLES
###############################################################
cat("\nDropped variables (p-value > 0.10):\n")
print(dropped_vars)

###############################################################
# c. INTERPRETATION GUIDE (TEXT OUTPUT)
###############################################################
cat("\nINTERPRETATION GUIDE:\n",
    "Each remaining coefficient in the final regression represents the\n",
    "expected change in Opening Gross when that predictor changes by one unit,\n",
    "holding all the other retained variables constant.\n",
    "- For 'Opening Theatres': change in Opening Gross per additional theatre.\n",
    "- For Summer/Holiday/Christmas (0/1 dummies): difference in Opening Gross\n",
    "  between movies released in that period (1) vs not (0).\n", sep = "")

###############################################################
# d. EFFECT OF +100 THEATRES (POINT ESTIMATE + 95% CI)
###############################################################

# Find the correct row for 'Opening Theatres' in the final coefficient table
final_coef_table <- coef(summary(model_final))
rn <- rownames(final_coef_table)

# Match by text, ignoring backticks
theatre_index <- grep("Opening Theatres", rn, fixed = TRUE)

if (length(theatre_index) == 1) {
  
  theatre_coef <- final_coef_table[theatre_index, 1]  # coefficient
  theatre_se   <- final_coef_table[theatre_index, 2]  # standard error
  
  # Point estimate for increasing theatres by 100
  point_estimate <- theatre_coef * 100
  
  # 95% CI for this effect
  df <- model_final$df.residual
  t_critical <- qt(0.975, df = df)
  CI_low  <- (theatre_coef - t_critical * theatre_se) * 100
  CI_high <- (theatre_coef + t_critical * theatre_se) * 100
  
  cat("\n\nEffect of increasing 'Opening Theatres' by 100:\n",
      "Point estimate of change in Opening Gross: ", point_estimate, "\n",
      "95% CI for the change: [", CI_low, ", ", CI_high, "]\n", sep = "")
  
} else {
  cat("\n\n'Opening Theatres' is not in the final model (p > 0.10),\n",
      "so the effect and its CI cannot be computed from this regression.\n", sep = "")
}

###############################################################
# QUESTION 7 SOLUTION
# RELATIONSHIP BETWEEN OPENING GROSS AND TOTAL U.S. GROSS
# SIMPLE & ENHANCED REGRESSION ANALYSIS
###############################################################

###############################################################
# a. SIMPLE LINEAR REGRESSION:
# Predict Total U.S. Gross from Opening Gross
###############################################################

model_simple <- lm(`Total U.S. Gross` ~ `Opening Gross`, data = movies)
summary(model_simple)

###############################################################
# b. EXPECTED SLOPE IF AGE-OLD WISDOM WERE TRUE
###############################################################
# If 25% of total U.S. gross = opening weekend gross:
# Opening Gross = 0.25 * Total U.S. Gross
# Total U.S. Gross = 4 * Opening Gross
# So, expected slope = 4

expected_slope <- 4
expected_slope

###############################################################
# c. STATISTICAL TEST: Is slope significantly different from 4?
###############################################################

coef_table <- summary(model_simple)$coefficients
slope_estimate <- coef_table["`Opening Gross`", 1]
slope_se       <- coef_table["`Opening Gross`", 2]

# Hypothesis test:
# H0: slope = 4
# H1: slope ≠ 4
t_statistic <- (slope_estimate - expected_slope) / slope_se
df <- model_simple$df.residual
p_value <- 2 * (1 - pt(abs(t_statistic), df=df))

t_statistic
p_value

###############################################################
# INTERPRETATION GUIDE FOR PART C (NOT printed, for report writing):
# If p_value < 0.05 → Reject H0 → slope is significantly different from 4
# If p_value >= 0.05 → Do not reject H0 → slope not statistically different from 4
###############################################################

###############################################################
# d. CRITIQUE (to include in written answer):
# The simple regression ignores other important factors
# influencing total U.S. Gross (budget, theatre count, release season,
# sequel, MPAA, marketing, genre, etc.). Therefore, the model is likely
# biased and suffers from omitted variable bias. A single predictor
# cannot fully explain variation in movie success.
###############################################################

###############################################################
# e. MULTIPLE REGRESSION MODEL:
# Predict Total U.S. Gross using Opening Gross + Opening weekend drivers
###############################################################

model_enhanced <- lm(`Total U.S. Gross` ~ `Opening Gross` + `Opening Theatres` +
                       Summer + Holiday + Christmas,
                     data = movies)

summary(model_enhanced)

###############################################################
# f. TEST AGE-OLD WISDOM USING NEW MODEL
# Check whether the coefficient for Opening Gross is statistically equal to 4
###############################################################

coef_table2 <- summary(model_enhanced)$coefficients
slope2 <- coef_table2["`Opening Gross`", 1]
se2    <- coef_table2["`Opening Gross`", 2]

t_statistic2 <- (slope2 - expected_slope) / se2
p_value2 <- 2 * (1 - pt(abs(t_statistic2), df=model_enhanced$df.residual))

t_statistic2
p_value2

###############################################################
# INTERPRETATION FOR PART F (FOR REPORT):
# If p_value2 < 0.05, reject age-old wisdom under improved model
# If p_value2 >= 0.05, do not reject → 25% claim plausible
###############################################################

###############################################################
# g. VARIATION EXPLAINED (R-squared)
###############################################################

r_squared_simple <- summary(model_simple)$r.squared
r_squared_enhanced <- summary(model_enhanced)$r.squared

cat("\nR-squared (simple model): ", r_squared_simple, "\n")
cat("R-squared (enhanced model): ", r_squared_enhanced, "\n")

###############################################################
# Interpretation for part g (include in written report):
# R-squared tells us what proportion of variation in total U.S. gross
# is explained by opening weekend gross revenue.
###############################################################


###############################################################
# QUESTION 8
# EFFECT OF CRITICS' OPINION ON TOTAL U.S. BOX-OFFICE GROSS
# - Use all factors known after opening weekend:
#   * Pre-production: Budget, COMEDY_DUMMY, MPAA_D, Sequel, Known Story
#   * Opening-weekend setup: Opening Theatres, Summer, Holiday, Christmas
#   * Opening-weekend outcome: Opening Gross, Critics´ Opinion
# - Dynamic dropping of variables with p-value > 0.10
# - Prediction & PI for "Flags of Our Fathers"
###############################################################

## Helper: wrap backticks around variable names that contain spaces
wrap_backticks <- function(x) {
  is_simple <- grepl("^[[:alnum:]_\\.]+$", x)
  x[!is_simple] <- paste0("`", x[!is_simple], "`")
  x
}

###############################################################
# a. INITIAL REGRESSION MODEL
###############################################################

# Candidate predictors (column names as they appear in movies)
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

# Protect names with spaces for the formula
candidate_vars_for_formula <- wrap_backticks(candidate_vars)

# Build formula: Total U.S. Gross ~ (all predictors)
formula_initial <- as.formula(
  paste("`Total U.S. Gross` ~", paste(candidate_vars_for_formula, collapse = " + "))
)

# Fit initial model
model_initial <- lm(formula_initial, data = movies)
summary(model_initial)

###############################################################
# b. DROP VARIABLES WITH p-value > 0.10 (DYNAMICALLY)
###############################################################

# Get coefficient table
coef_table <- summary(model_initial)$coefficients

# Exclude intercept row
p_values   <- coef_table[-1, 4]                 # column 4 = Pr(>|t|)
term_names <- rownames(coef_table)[-1]          # predictor labels used in formula

# Keep variables with p <= 0.10
signif_terms <- term_names[p_values <= 0.10]

# If none are significant, keep the single best predictor (smallest p-value)
if (length(signif_terms) == 0) {
  signif_terms <- term_names[which.min(p_values)]
}

# Determine dropped variables
dropped_terms <- setdiff(term_names, signif_terms)

# Build final model formula using the kept term labels directly
# (they already include backticks where needed)
formula_final <- as.formula(
  paste("`Total U.S. Gross` ~", paste(signif_terms, collapse = " + "))
)

# Fit final model
model_final <- lm(formula_final, data = movies)
summary(model_final)

# Show which variables were dropped due to p-value > 0.10
cat("\nDropped variables (p-value > 0.10 in initial model):\n")
print(dropped_terms)

###############################################################
# c. PREDICTION & 95% PREDICTION INTERVAL FOR
#    A MOVIE LIKE "FLAGS OF OUR FATHERS"
###############################################################

# Extract row(s) corresponding to "Flags of Our Fathers"
flags_data <- movies[movies$Movie == "Flags of Our Fathers", ]

if (nrow(flags_data) == 0) {
  cat("\nWARNING: No movie named 'Flags of Our Fathers' found in the data.\n",
      "Cannot compute prediction for part (c) and (d).\n")
} else {
  # Use the final model to get point prediction & 95% prediction interval
  pred_flags <- predict(model_final,
                        newdata = flags_data,
                        interval = "prediction",
                        level = 0.95)
  
  cat("\nPrediction for Total U.S. Gross of 'Flags of Our Fathers':\n")
  print(pred_flags)
}

###############################################################
# d. VALUE OF A +10 POINT INCREASE IN CRITICS' OPINION
#    FOR A MOVIE LIKE "FLAGS OF OUR FATHERS"
#
# Idea:
#   - Let β_C be the slope for Critics´ Opinion in the final model.
#   - A 10-point increase in critics' score changes expected Total U.S. Gross by:
#         ΔE[Gross] = β_C * 10
#   - This amount is the extra revenue you'd expect, so Griffith should
#     not spend more than this amount to "buy" 10 extra points.
#   - We also compute a 95% CI for this effect: 10 * (β_C ± t * SE_C).
###############################################################

final_coef_table <- coef(summary(model_final))
final_terms      <- rownames(final_coef_table)

# Find the coefficient row corresponding to Critics´ Opinion
crit_index <- grep("Critics", final_terms, ignore.case = TRUE)

if (length(crit_index) == 0) {
  cat("\n'Critics´ Opinion' is NOT in the final model (was dropped: p > 0.10).\n",
      "=> Based on this model, critics' score does not have a statistically\n",
      "   significant marginal impact on Total U.S. Gross at the 10% level.\n")
} else {
  # Extract coefficient and standard error
  beta_crit <- final_coef_table[crit_index, 1]
  se_crit   <- final_coef_table[crit_index, 2]
  
  # Point estimate for gain from +10 points in critics' opinion
  delta_revenue_point <- beta_crit * 10
  
  # 95% CI for that gain
  df_final   <- model_final$df.residual
  t_crit     <- qt(0.975, df = df_final)
  delta_low  <- (beta_crit - t_crit * se_crit) * 10
  delta_high <- (beta_crit + t_crit * se_crit) * 10
  
  cat("\nEffect of a +10 point increase in critics' score:\n")
  cat("Point estimate of extra Total U.S. Gross: ", delta_revenue_point, "\n")
  cat("95% CI for this extra gross: [", delta_low, ", ", delta_high, "]\n")
  
  cat("\nManagerial advice to Griffith:\n",
      "- He should be willing to invest at most about this point estimate\n",
      "  of extra revenue (or less conservatively, somewhere within the\n",
      "  lower end of the 95% CI) to influence critics.\n",
      "- Spending more than the expected additional revenue would not be\n",
      "  financially justified on average.\n", sep = "")
}

###############################################################
# QUESTION 9
# DOES CRITICS' OPINION AFFECT COMEDIES DIFFERENTLY THAN NON-COMEDIES?
#
# Based on FINAL MODEL from Question 8, which kept:
#   - Budget
#   - MPAA_D
#   - Opening Gross
#   - Critics´ Opinion
#
# We now ADD an interaction term:
#   Critics´ Opinion * COMEDY_DUMMY
#
# Hypothesis:
#   H0: Interaction coefficient = 0
#   H1: Interaction coefficient ≠ 0
#
# Interpretation:
#   - If interaction coefficient is NEGATIVE and SIGNIFICANT (p < 0.10)
#       → Critics' effect is weaker for comedies → supports Griffith’s theory.
#   - Otherwise
#       → No statistical evidence that critics matter differently for comedies.
###############################################################

###############################################################
# a. MODIFY FINAL REGRESSION FROM Q8: ADD INTERACTION TERM
###############################################################

model_q9 <- lm(
  `Total U.S. Gross` ~ Budget + MPAA_D + `Opening Gross` +
    `Critics´ Opinion` * COMEDY_DUMMY,
  data = movies
)

# Show regression output
summary(model_q9)

###############################################################
# b. INTERACTION TERM TEST
###############################################################

coef_table_q9 <- coef(summary(model_q9))

# Find the row for the interaction term (Critics × COMEDY_DUMMY)
# Row name will look something like: `Critics´ Opinion`:COMEDY_DUMMY
interaction_index <- grep("Critics", rownames(coef_table_q9), ignore.case = TRUE)
interaction_index <- interaction_index[
  grep("COMEDY", rownames(coef_table_q9)[interaction_index], ignore.case = TRUE)
]

if (length(interaction_index) == 1) {
  
  interaction_row   <- coef_table_q9[interaction_index, ]
  interaction_coeff <- interaction_row[1]
  interaction_p     <- interaction_row[4]
  
  cat("\nInteraction effect (Critics´ Opinion × COMEDY_DUMMY):\n")
  print(interaction_row)
  
  cat("\nInteraction coefficient (Critics × Comedy): ", interaction_coeff, "\n")
  cat("p-value: ", interaction_p, "\n")
  
  #############################################################
  # c. DECISION ON GRIFFITH'S THEORY
  #############################################################
  
  cat("\nDECISION & INTERPRETATION:\n")
  
  if (interaction_p < 0.10 && interaction_coeff < 0) {
    cat(
      "- p-value < 0.10 AND coefficient < 0\n",
      "- Conclusion: There is statistical evidence that critics' reviews\n",
      "  have a significantly WEAKER impact on total U.S. gross for comedies\n",
      "  than for non-comedies.\n",
      "  → Griffith’s theory is SUPPORTED by the data.\n",
      sep = ""
    )
  } else {
    cat(
      "- Either p-value ≥ 0.10 OR coefficient is not negative.\n",
      "- Conclusion: We CANNOT statistically prove that critics' influence\n",
      "  on total U.S. gross is weaker for comedies than for non-comedies.\n",
      "  → Griffith’s theory is NOT supported by this regression.\n",
      sep = ""
    )
  }
  
} else {
  cat(
    "\nWARNING: Could not uniquely identify the interaction term row.\n",
    "Check the model summary and row names of coef(summary(model_q9)).\n",
    sep = ""
  )
}

###############################################################
# TEXT GUIDE FOR WRITE-UP / PPT (not executed, but for you):
#
# 1. Mention the model:
#    Total U.S. Gross ~ Budget + MPAA_D + Opening Gross +
#                        Critics´ Opinion + Critics´ Opinion × COMEDY_DUMMY
#
# 2. Explain:
#    - The coefficient of Critics´ Opinion shows its effect on NON-COMEDIES
#      (when COMEDY_DUMMY = 0).
#    - The interaction term shows how much that effect CHANGES for comedies.
#
# 3. If the interaction term is significantly negative:
#    - “The marginal effect of critics’ ratings on total U.S. box-office
#       revenue is smaller for comedies than for non-comedies, supporting
#       Griffith’s belief.”
#
# 4. If not significant:
#    - “We do not find statistical evidence that the effect of critics’
#       ratings differs between comedies and non-comedies. Griffith’s
#       claim cannot be proven using this dataset.”
###############################################################
