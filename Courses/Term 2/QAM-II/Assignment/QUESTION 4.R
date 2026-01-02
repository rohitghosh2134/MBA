# Q4.R - R-rated vs non-R-rated comparison

library(readxl)

movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

cat("QUESTION 4 – R-RATED VS OTHER MOVIES\n\n")

# Separate groups
r_rated_gross     <- movies$`Total U.S. Gross`[movies$MPAA == "R"]
non_r_rated_gross <- movies$`Total U.S. Gross`[movies$MPAA != "R"]

# Means
mean_r_rated_gross     <- mean(r_rated_gross, na.rm = TRUE)
mean_non_r_rated_gross <- mean(non_r_rated_gross, na.rm = TRUE)

cat("Mean Total U.S. Gross (R-rated): ", mean_r_rated_gross, "\n")
cat("Mean Total U.S. Gross (Non-R-rated): ", mean_non_r_rated_gross, "\n\n")

# Two-sample t-test
t_test_r_vs_others <- t.test(r_rated_gross,
                             non_r_rated_gross,
                             alternative = "two.sided",
                             var.equal = FALSE)

cat("Two-sample t-test (R-rated vs Non-R-rated):\n")
print(t_test_r_vs_others)

# Interpretation
cat("\nINTERPRETATION (α = 0.05):\n")

if (t_test_r_vs_others$p.value < 0.05) {
  cat("Result: Reject H0. There is a statistically significant difference in mean Total U.S. Gross\n",
      "between R-rated and non-R-rated movies.\n")
  if (mean_r_rated_gross > mean_non_r_rated_gross) {
    cat("On average, R-rated movies earn more in U.S. Box Office than non-R-rated movies.\n")
  } else {
    cat("On average, non-R-rated movies earn more in U.S. Box Office than R-rated movies.\n")
  }
} else {
  cat("Result: Fail to Reject H0. There is no statistically significant evidence that the U.S.\n",
      "box office gross differs between R-rated and non-R-rated movies.\n")
}
