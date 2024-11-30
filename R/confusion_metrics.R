#Task 3:


# Confusion matrix metrics
confusion_metrics <- function(y_true, y_pred) {
  TP <- sum(y_true == 1 & y_pred == 1)
  TN <- sum(y_true == 0 & y_pred == 0)
  FP <- sum(y_true == 0 & y_pred == 1)
  FN <- sum(y_true == 1 & y_pred == 0)

  prevalence <- mean(y_true)
  accuracy <- (TP + TN) / length(y_true)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  FDR <- FP / (FP + TP)
  DOR <- (TP / FN) / (FP / TN)

  return(list(prevalence = prevalence, accuracy = accuracy, sensitivity = sensitivity,
              specificity = specificity, FDR = FDR, DOR = DOR))
}

# Predict probabilities and calculate metrics
X_design <- cbind(1, X)  # add intercept column
p_hat <- 1 / (1 + exp(-X_design %*% beta_opt))  # use design matrix with intercept
y_pred <- ifelse(p_hat > 0.5, 1, 0)  # classify predictions

# Calculate metrics
metrics <- confusion_metrics(y, y_pred)

# Print confusion matrix metrics
print(metrics)


