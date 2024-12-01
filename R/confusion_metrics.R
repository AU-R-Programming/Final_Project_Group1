#' @title Compute Confusion Matrix Metrics
#'
#' @description
#' This function calculates various evaluation metrics from a confusion matrix
#' for a binary classification model, including accuracy, sensitivity, specificity,
#' false discovery rate, and diagnostic odds ratio.
#'
#' @param y_true A numeric vector containing the true class labels (binary: 0 or 1).
#' @param y_pred A numeric vector containing the predicted class labels (binary: 0 or 1).
#'
#' @return A list containing the following metrics:
#' \describe{
#'   \item{prevalence}{Proportion of positive cases in the data.}
#'   \item{accuracy}{Overall accuracy of the classification model.}
#'   \item{sensitivity}{Proportion of true positives identified correctly.}
#'   \item{specificity}{Proportion of true negatives identified correctly.}
#'   \item{FDR}{False Discovery Rate: Proportion of predicted positives that are false.}
#'   \item{DOR}{Diagnostic Odds Ratio: Ratio of the odds of positive prediction for true positives compared to false negatives.}
#' }
#' @author Yuchen Wang
#' @export
#' @examples
#' y_true <- c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0)
#' y_pred <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0)
#' metrics <- confusion_metrics(y_true, y_pred)
#' print(metrics)

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

  return(list(
    prevalence = prevalence,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    FDR = FDR,
    DOR = DOR
  ))
}



