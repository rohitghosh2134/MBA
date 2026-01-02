# Q7.R - Relationship between Opening Gross and Total U.S. Gross

library(readxl)

movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

cat("QUESTION 7 – OPENING GROSS AND TOTAL U.S. GROSS\n\n")

# a. Simple linear regression
model_simple <- lm(`Total U.S. Gross` ~ `Opening Gross`, data = movies)

cat("Simple regression: Total U.S. Gross ~ Opening Gross\n")
print(summary(model_simple))
cat("\n")

# b. Expected slope if '25% rule' true
expected_slope <- 4
cat("Expected slope if '25% rule' holds: ", expected_slope, "\n\n")

# c. Test if slope differs from 4
coef_table <- summary(model_simple)$coefficients
slope_estimate <- coef_table["`Opening Gross`", 1]
slope_se       <- coef_table["`Opening Gross`", 2]

t_statistic <- (slope_estimate - expected_slope) / slope_se
df <- model_simple$df.residual
p_value <- 2 * (1 - pt(abs(t_statistic), df = df))

cat("Test of slope = 4 in simple model:\n")
cat("Estimated slope: ", slope_estimate, "\n")
cat("t-statistic: ", t_statistic, "\n")
cat("p-value: ", p_value, "\n\n")

# d. Enhanced multiple regression
model_enhanced <- lm(`Total U.S. Gross` ~ `Opening Gross` + `Opening Theatres` +
                       Summer + Holiday + Christmas,
                     data = movies)

cat("Enhanced regression model:\n")
print(summary(model_enhanced))
cat("\n")

# e. Test slope = 4 in enhanced model
coef_table2 <- summary(model_enhanced)$coefficients
slope2 <- coef_table2["`Opening Gross`", 1]
se2    <- coef_table2["`Opening Gross`", 2]

t_statistic2 <- (slope2 - expected_slope) / se2
p_value2     <- 2 * (1 - pt(abs(t_statistic2), df = model_enhanced$df.residual))

cat("Test of slope = 4 in enhanced model:\n")
cat("Estimated slope: ", slope2, "\n")
cat("t-statistic: ", t_statistic2, "\n")
cat("p-value: ", p_value2, "\n\n")

# f. R-squared comparison
r_squared_simple   <- summary(model_simple)$r.squared
r_squared_enhanced <- summary(model_enhanced)$r.squared

cat("R-squared (simple model): ", r_squared_simple, "\n")
cat("R-squared (enhanced model): ", r_squared_enhanced, "\n\n")
