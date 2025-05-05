

# === From script9.R ===
model_predictions <- predict(model)

# 2. Create a dataframe using model$model
results_df <- data.frame(
  Actual = model$model$Hospital_Admissions,
  Predicted = model_predictions
)


ggplot(results_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Hospital Admissions",
    x = "Actual Admissions",
    y = "Predicted Admissions"
  ) +
  theme_minimal()




