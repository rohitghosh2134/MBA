# Q1.R  - Descriptive statistics and counts

library(readxl)

# Load data
movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

cat("QUESTION 1 – DESCRIPTIVE STATISTICS AND COUNTS\n\n")

# View column names
cat("Column names:\n")
print(names(movies))
cat("\n")

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

cat("Opening Gross (Min, Mean, Max):\n")
print(c(min_opening_gross, avg_opening_gross, max_opening_gross))
cat("\n")

cat("Total U.S. Gross (Min, Mean, Max):\n")
print(c(min_total_us_gross, avg_total_us_gross, max_total_us_gross))
cat("\n")

cat("Total Non-U.S. Gross (Min, Mean, Max):\n")
print(c(min_total_nonus_gross, avg_total_nonus_gross, max_total_nonus_gross))
cat("\n")

cat("Opening Theatres (Min, Mean, Max):\n")
print(c(min_opening_theatres, avg_opening_theatres, max_opening_theatres))
cat("\n")

# COUNT how many movies are comedies
num_comedies <- sum(movies$COMEDY_DUMMY == 1, na.rm = TRUE)

# COUNT how many movies are R-rated
num_R_rated <- sum(movies$MPAA == "R", na.rm = TRUE)

cat("Number of comedies (COMEDY_DUMMY = 1): ", num_comedies, "\n")
cat("Number of R-rated movies (MPAA = 'R'): ", num_R_rated, "\n")
