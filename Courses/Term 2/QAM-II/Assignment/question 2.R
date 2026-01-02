# Q2.R - ROI calculation, CI and one-sample t-test vs 12%

library(readxl)

movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

cat("QUESTION 2 – ROI, CI AND ONE-SAMPLE T-TEST\n\n")

# a. Calculate U.S. ROI
movies$US_ROI <- (movies$`Total U.S. Gross` - movies$Budget) / movies$Budget

# Print Movie names with ROI values
cat("ROI values by Movie:\n")
roi_table <- data.frame(Movie = movies$Movie, ROI = movies$US_ROI)
print(roi_table)
cat("\n")

# b. 95% CI for mean ROI
mean_roi <- mean(movies$US_ROI, na.rm = TRUE)
sd_roi   <- sd(movies$US_ROI, na.rm = TRUE)
n        <- sum(!is.na(movies$US_ROI))

se_roi   <- sd_roi / sqrt(n)
t_value  <- qt(0.975, df = n - 1)

CI_lower <- mean_roi - t_value * se_roi
CI_upper <- mean_roi + t_value * se_roi

cat("Mean ROI: ", mean_roi, "\n")
cat("95% CI for mean ROI: [", CI_lower, ", ", CI_upper, "]\n\n")

# c. Hypothesis test mean ROI > 0.12
cat("Hypothesis Test (significance level = 5%):\n")
cat("Null Hypothesis (H0): mean ROI = 0.12\n")
cat("Alternative Hypothesis (H1): mean ROI > 0.12\n\n")

t_statistic <- (mean_roi - 0.12) / se_roi
p_value     <- 1 - pt(t_statistic, df = n - 1)

cat("t-statistic: ", t_statistic, "\n")
cat("p-value (one-sided): ", p_value, "\n")
