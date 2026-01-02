# Q3.R - Comedy vs Non-Comedy comparison

library(readxl)

movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

# Ensure ROI is available
movies$US_ROI <- (movies$`Total U.S. Gross` - movies$Budget) / movies$Budget

cat("QUESTION 3 – COMEDY VS NON-COMEDY\n\n")

# a. Total U.S. Gross comparison
comedy_us_gross    <- movies$`Total U.S. Gross`[movies$COMEDY_DUMMY == 1]
noncomedy_us_gross <- movies$`Total U.S. Gross`[movies$COMEDY_DUMMY == 0]

mean_comedy_gross    <- mean(comedy_us_gross, na.rm = TRUE)
mean_noncomedy_gross <- mean(noncomedy_us_gross, na.rm = TRUE)

cat("Mean Total U.S. Gross (Comedy): ", mean_comedy_gross, "\n")
cat("Mean Total U.S. Gross (Non-Comedy): ", mean_noncomedy_gross, "\n\n")

t_test_gross <- t.test(comedy_us_gross, noncomedy_us_gross,
                       alternative = "two.sided",
                       var.equal = FALSE)

cat("Two-sample t-test for Total U.S. Gross (Comedy vs Non-Comedy):\n")
print(t_test_gross)
cat("\n")

# b. ROI comparison
comedy_roi    <- movies$US_ROI[movies$COMEDY_DUMMY == 1]
noncomedy_roi <- movies$US_ROI[movies$COMEDY_DUMMY == 0]

mean_comedy_roi    <- mean(comedy_roi, na.rm = TRUE)
mean_noncomedy_roi <- mean(noncomedy_roi, na.rm = TRUE)

cat("Mean ROI (Comedy): ", mean_comedy_roi, "\n")
cat("Mean ROI (Non-Comedy): ", mean_noncomedy_roi, "\n\n")

t_test_roi <- t.test(comedy_roi, noncomedy_roi,
                     alternative = "two.sided",
                     var.equal = FALSE)

cat("Two-sample t-test for ROI (Comedy vs Non-Comedy):\n")
print(t_test_roi)
