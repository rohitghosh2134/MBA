# Q9.R - Does critics' opinion affect comedies differently?

library(readxl)

movies <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")

cat("QUESTION 9 – INTERACTION: CRITICS' OPINION × COMEDY\n\n")

model_q9 <- lm(
  `Total U.S. Gross` ~ Budget + MPAA_D + `Opening Gross` +
    `Critics´ Opinion` * COMEDY_DUMMY,
  data = movies
)

cat("Regression with interaction term (Critics´ Opinion × COMEDY_DUMMY):\n")
print(summary(model_q9))
cat("\n")

coef_table_q9 <- coef(summary(model_q9))

# Find the interaction term row: Critics × COMEDY
interaction_index <- grep("Critics", rownames(coef_table_q9), ignore.case = TRUE)
interaction_index <- interaction_index[
  grep("COMEDY", rownames(coef_table_q9)[interaction_index], ignore.case = TRUE)
]

if (length(interaction_index) == 1) {
  interaction_row   <- coef_table_q9[interaction_index, ]
  interaction_coeff <- interaction_row[1]
  interaction_p     <- interaction_row[4]
  
  cat("Interaction effect row (Critics´ Opinion × COMEDY_DUMMY):\n")
  print(interaction_row)
  cat("\n")
  
  cat("Interaction coefficient: ", interaction_coeff, "\n")
  cat("p-value: ", interaction_p, "\n\n")
  
  cat("DECISION & INTERPRETATION (significance level = 10%):\n")
  
  if (interaction_p < 0.10 && interaction_coeff < 0) {
    cat(
      "- p-value < 0.10 AND coefficient < 0\n",
      "- Conclusion: There is statistical evidence that critics' reviews\n",
      "  have a significantly WEAKER impact on total U.S. gross for comedies\n",
      "  than for non-comedies.\n",
      "  Griffith’s theory is supported by the data.\n", sep = ""
    )
  } else {
    cat(
      "- Either p-value ≥ 0.10 OR coefficient is not negative.\n",
      "- Conclusion: We cannot statistically prove that critics' influence\n",
      "  on total U.S. gross is weaker for comedies than for non-comedies.\n",
      "  Griffith’s theory is not supported by this regression.\n", sep = ""
    )
  }
  
} else {
  cat("WARNING: Could not uniquely identify the interaction term row.\n",
      "Check row names of coef(summary(model_q9)).\n")
}
