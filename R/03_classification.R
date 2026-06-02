#' @importFrom stats cor fft lm quantile sd var
#' @importFrom graphics par
NULL

#' K-Nearest Neighbors Classification with Stratified k-Fold Cross-Validation
#'
#' @description Implements a robust k-Nearest Neighbors (kNN) algorithm enveloped in stratified k-fold cross-validation to tune the optimal k hyperparameter without data leakage.
#'
#' @param features A matrix or data frame of numeric feature vectors (samples as rows, features as columns).
#' @param labels A factor or character vector representing the true class labels.
#' @param k_values A numeric vector representing the sequence of k values to test (default: c(3, 5, 7, 9, 11)).
#' @param folds Integer. The number of cross-validation folds (default: 5).
#'
#' @details
#' This function evaluates the classification accuracy of multiple k values. To ensure unbiased estimation, it partitions the data into `folds` using stratified sampling, guaranteeing that each fold maintains the overall class distribution.
#'
#' **Algorithm Steps:**
#' 1. Iterates through each candidate k in `k_values`.
#' 2. For each fold, trains the model on k-1 folds and tests on the hold-out fold.
#' 3. Averages the accuracy across all folds to determine the generalized performance of that specific k.
#' 4. Identifies and recommends the optimal k value.
#'
#' @return A comprehensive list containing the summary `results`, the `best_k`, the `best_accuracy`, and the granular `fold_results`.
#'
#' @examples
#' 
#' data(bird_train)
#' features <- bird_train[, 1:15]
#' labels <- bird_train$species
#'
#' # Run 5-fold CV to find the best k between 3 and 9
#' cv_results <- classify_knn_cv_tuned(features, labels, k_values = c(3, 5, 7, 9))
#' print(cv_results$best_k)
#' 
#' @export
classify_knn_cv_tuned <- function(features, labels, k_values = c(3, 5, 7, 9, 11), folds = 5) {

  X <- as.matrix(features)
  y <- as.factor(labels)

  fold_indices <- create_stratified_folds(y, folds)
  results_list <- list()

  for (k in k_values) {
    fold_accuracies <- numeric(folds)

    for (fold_idx in 1:folds) {
      test_idx <- which(fold_indices == fold_idx)
      train_X <- X[-test_idx, ]
      train_y <- y[-test_idx]
      test_X <- X[test_idx, ]
      test_y <- y[test_idx]

      predictions <- class::knn(train = train_X, test = test_X, cl = train_y, k = k)
      fold_accuracies[fold_idx] <- mean(predictions == test_y)
    }

    results_list[[as.character(k)]] <- fold_accuracies
  }

  summary_df <- data.frame(
    k = k_values,
    mean_accuracy = sapply(results_list, mean),
    sd_accuracy = sapply(results_list, sd)
  )

  best_idx <- which.max(summary_df$mean_accuracy)

  return(list(
    results = summary_df,
    best_k = summary_df$k[best_idx],
    best_accuracy = summary_df$mean_accuracy[best_idx],
    fold_results = results_list
  ))
}

#' Create Stratified Folds for Cross-Validation
#'
#' @description An internal utility that generates balanced fold assignments for cross-validation, ensuring proportional class representation in every fold.
#'
#' @param labels A factor or character vector of class labels.
#' @param folds Integer. The total number of folds to partition the data into.
#'
#' @details
#' Standard random sampling can accidentally create folds where rare bird species are entirely missing from the training or testing set. This function loops through each unique class and distributes its samples evenly across the specified number of folds.
#'
#' @return A numeric vector containing the fold assignment (1 to `folds`) for each respective sample.
#' @keywords internal
#' @examples
#' # Create an imbalanced dummy dataset of bird labels
#' bird_labels <- c(rep("Owl", 10), rep("Dove", 5), rep("Macaw", 5))
#' 
#' # Assign each sample to one of 5 stratified folds
#' fold_assignments <- create_stratified_folds(bird_labels, folds = 5)
#' 
#' # Verify that each fold gets exactly 2 Owls, 1 Dove, and 1 Macaw
#' table(Species = bird_labels, Fold = fold_assignments)
#' @export
create_stratified_folds <- function(labels, folds) {
  n <- length(labels)
  classes <- unique(labels)
  fold_indices <- numeric(n)

  for (class in classes) {
    class_indices <- which(labels == class)
    n_class <- length(class_indices)
    fold_assignment <- sample(rep(1:folds, length.out = n_class))
    fold_indices[class_indices] <- fold_assignment
  }

  return(fold_indices)
}

#' Distance-Weighted k-Nearest Neighbors Algorithm
#'
#' @description Executes a highly optimized k-Nearest Neighbors classification where the voting power of each neighbor is inversely proportional to its Euclidean distance from the target sample.
#'
#' @param train A numeric matrix representing the training feature space.
#' @param test A numeric matrix representing the testing/inference feature space.
#' @param cl A factor vector containing the true class labels for the training set.
#' @param k Integer. The number of nearest neighbors to retrieve (default: 5).
#'
#' @details
#' Standard kNN calculates the Euclidean distance to the k closest training samples and assigns the class based on a simple majority vote. However, this treats a neighbor that is physically identical to the test sample the same as a neighbor on the outer edge of the cluster.
#'
#' This function implements Distance-Weighted voting. The weight of each neighbor's vote is calculated as \eqn{w_i = \frac{1}{d_i + \epsilon}} (where \eqn{\epsilon} is a microscopic constant to prevent division by zero). The algorithm sums the weights for each class and predicts the class with the highest total weight.
#'
#' @return A factor vector containing the predicted class labels.
#'
#' @examples
#' 
#' data(bird_train)
#' data(bird_test)
#'
#' train_x <- as.matrix(bird_train[, 1:15])
#' test_x <- as.matrix(bird_test[, 1:15])
#' train_y <- bird_train$species
#'
#' # Predict test set labels using 5 distance-weighted neighbors
#' preds <- weighted_knn(train_x, test_x, train_y, k = 5)
#' print(preds[1:10])
#' 
#' @export
weighted_knn <- function(train, test, cl, k = 5) {
  train_matrix <- as.matrix(train)
  test_matrix <- as.matrix(test)

  n_test <- nrow(test_matrix)
  n_train <- nrow(train_matrix)

  distances <- matrix(0, nrow = n_test, ncol = n_train)

  for (i in 1:n_test) {
    for (j in 1:n_train) {
      distances[i, j] <- sqrt(sum((test_matrix[i, ] - train_matrix[j, ])^2))
    }
  }

  predictions <- character(n_test)

  for (i in 1:n_test) {
    d <- distances[i, ]
    k_actual <- min(k, length(d))
    knn_idx <- order(d)[1:k_actual]
    knn_labels <- as.character(cl[knn_idx])
    knn_dists <- d[knn_idx]

    weights <- 1 / (knn_dists + 1e-10)

    unique_classes <- unique(knn_labels)
    class_weights <- numeric(length(unique_classes))
    names(class_weights) <- unique_classes

    for (j in 1:k_actual) {
      class_weights[knn_labels[j]] <- class_weights[knn_labels[j]] + weights[j]
    }

    predictions[i] <- names(which.max(class_weights))
  }

  return(factor(predictions, levels = levels(as.factor(cl))))
}

#' Generate Comprehensive Classification Analytics
#'
#' @description Produces a detailed performance report comparing predicted labels against actual labels, outputting macro-averaged metrics and a confusion matrix.
#'
#' @param predictions A factor or character vector of model-predicted labels.
#' @param actual A factor or character vector of the true ground-truth labels.
#' @param model_name Character string. Used as the header for the printed console report.
#'
#' @details
#' Accuracy alone is often insufficient for evaluating biological multi-class models. This function calculates per-class Precision, Recall, Specificity, and F1-Scores. It also outputs Cohen's Kappa, a statistical measure of inter-rater reliability that accounts for the possibility of the model guessing correctly by chance.
#'
#' @return A list containing the raw confusion matrix alongside the calculated metrics.
#'
#' @examples
#' 
#' actual <- as.factor(c("Owl", "Owl", "Dove", "Dove"))
#' preds <- as.factor(c("Owl", "Dove", "Dove", "Dove"))
#'
#' metrics <- evaluate_classification(preds, actual, model_name = "kNN Test")
#' print(metrics$kappa)
#' 
#' @export
evaluate_classification <- function(predictions, actual, model_name = "Model") {
  predictions <- factor(predictions, levels = levels(actual))
  accuracy <- mean(predictions == actual)
  conf_matrix <- table(Actual = actual, Predicted = predictions)

  classes <- levels(actual)
  n_classes <- length(classes)

  precision <- recall <- f1 <- specificity <- numeric(n_classes)
  names(precision) <- names(recall) <- names(f1) <- names(specificity) <- classes

  cat("\n--------------------------------------------------------------\n")
  cat(sprintf("  CLASSIFICATION REPORT: %s\n", model_name))
  cat("--------------------------------------------------------------\n\n")

  cat(sprintf("Overall Accuracy: %.2f%%\n\n", accuracy * 100))

  cat("Confusion Matrix:\n")
  print(conf_matrix)
  cat("\nPer-Class Metrics:\n")
  cat(sprintf("%-12s %10s %10s %10s %10s %10s\n",
              "Class", "Precision", "Recall", "F1-Score", "Specificity", "Support"))
  cat(paste0(rep("-", 62), collapse = ""), "\n")

  for (i in seq_along(classes)) {
    class <- classes[i]
    tp <- sum(predictions == class & actual == class)
    fp <- sum(predictions == class & actual != class)
    fn <- sum(predictions != class & actual == class)
    tn <- sum(predictions != class & actual != class)

    precision[i] <- if (tp + fp > 0) tp / (tp + fp) else 0
    recall[i] <- if (tp + fn > 0) tp / (tp + fn) else 0
    f1[i] <- if (precision[i] + recall[i] > 0) {
      2 * (precision[i] * recall[i]) / (precision[i] + recall[i])
    } else {
      0
    }
    specificity[i] <- if (tn + fp > 0) tn / (tn + fp) else 0
    support <- tp + fn

    cat(sprintf("%-12s %10.3f %10.3f %10.3f %10.3f %10d\n",
                class, precision[i], recall[i], f1[i], specificity[i], support))
  }

  cat("\n")
  macro_precision <- mean(precision)
  macro_recall <- mean(recall)
  macro_f1 <- mean(f1)

  cat(sprintf("Macro-averaged Precision: %.3f\n", macro_precision))
  cat(sprintf("Macro-averaged Recall:    %.3f\n", macro_recall))
  cat(sprintf("Macro-averaged F1-Score:  %.3f\n\n", macro_f1))

  po <- accuracy
  classes_count <- table(actual)
  pe <- sum((classes_count / length(actual))^2)
  kappa <- (po - pe) / (1 - pe)

  cat(sprintf("Cohen's Kappa: %.3f\n", kappa))
  cat("  (0.0-0.2: Poor, 0.2-0.5: Fair, 0.5-0.8: Good, 0.8-1.0: Excellent)\n\n")
  cat("--------------------------------------------------------------\n\n")

  return(list(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1 = f1,
    specificity = specificity,
    macro_f1 = macro_f1,
    kappa = kappa,
    confusion_matrix = conf_matrix
  ))
}

#' Bootstrap Confidence Intervals for Classification Accuracy
#'
#' @description Computes the 95% confidence interval for a model's accuracy score using non-parametric bootstrap resampling.
#'
#' @param predictions A vector of model-predicted labels.
#' @param actual A vector of the true ground-truth labels.
#' @param n_bootstrap Integer. The number of bootstrap iterations to perform (default: 1000).
#'
#' @details
#' A single accuracy score on a test set is merely a point estimate. This function simulates standard error by resampling the paired predictions and actual labels with replacement `n_bootstrap` times. It calculates the accuracy of each resampled set and extracts the 2.5th and 97.5th percentiles to form a robust 95% confidence interval.
#'
#' @return A list containing the `accuracy` point estimate, `ci_lower`, `ci_upper`, and the raw `bootstrap_samples`.
#'
#' @examples
#' 
#' actual <- as.factor(c("Owl", "Owl", "Dove", "Dove", "Crow", "Crow"))
#' preds <- as.factor(c("Owl", "Dove", "Dove", "Dove", "Crow", "Crow"))
#'
#' # Get 95% CI based on 500 resamples
#' ci_data <- bootstrap_ci_accuracy(preds, actual, n_bootstrap = 500)
#' print(ci_data$ci_lower)
#' 
#' @export
bootstrap_ci_accuracy <- function(predictions, actual, n_bootstrap = 1000) {
  n <- length(actual)
  bootstrap_accs <- numeric(n_bootstrap)

  for (i in 1:n_bootstrap) {
    idx <- sample(1:n, size = n, replace = TRUE)
    bootstrap_accs[i] <- mean(predictions[idx] == actual[idx])
  }

  point_est <- mean(predictions == actual)
  ci_lower <- quantile(bootstrap_accs, 0.025)
  ci_upper <- quantile(bootstrap_accs, 0.975)

  return(list(
    accuracy = point_est,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    bootstrap_samples = bootstrap_accs
  ))
}

#' Live Inference: Predict Bird Species from Raw Audio
#'
#' @description End-to-end wrapper for predicting a species directly from a local audio file via the console, utilizing the 15-feature fPCA hybrid space.
#'
#' @param file_path Character. Path to the uploaded MP3/WAV file.
#' @param k Integer. Number of neighbors for inference (default: 5).
#'
#' @details
#' This function perfectly mirrors the live inference architecture of the Shiny App. It loads the internal training data and fPCA coefficients, extracts 69 raw features from the audio, dynamically centers the pitch trajectory using a matrix sweep, projects the curve down to 3 dimensions, binds it with 12 discrete features, and executes Distance-Weighted kNN.
#'
#' @return A character string of the predicted scientific name.
#'
#' @examples
#' \dontrun{
#' # Predict a single audio file directly from the console
#' prediction <- predict_live_audio("data-raw/audio/unknown_bird.wav", k = 3)
#' print(prediction)
#' }
#' @export
predict_live_audio <- function(file_path, k = 5) {
  if (!exists("bird_train")) utils::data("bird_train", package = "AvianEchoR", envir = environment())
  if (!exists("pitch_pca")) utils::data("pitch_pca", package = "AvianEchoR", envir = environment())
  
  wave <- read_audio_file(file_path)
  wave <- standardize_audio(wave)
  
  # --- ADDED VAD CHUNKING ---
  audio_chunks <- extract_clean_chunks(wave)
  if (length(audio_chunks) == 0) stop("No vocalizations detected in audio.")
  chunk_lengths <- sapply(audio_chunks, function(w) length(w@left))
  best_chunk <- audio_chunks[[which.max(chunk_lengths)]]
  
  # Feed the clean chunk, not the whole wave
  raw_feats <- extract_audio_features(best_chunk, verbose = FALSE)
  # --------------------------
  
  pitch_cols <- paste0("Pitch_t", 1:50)
  live_pitch <- as.matrix(raw_feats[, pitch_cols])
  live_pitch[!is.finite(live_pitch)] <- 0
  
  time_points <- 1:50
  spline_basis <- fda::create.bspline.basis(rangeval = c(1, 50), nbasis = 15)
  live_fd <- fda::smooth.basis(argvals = time_points, y = t(live_pitch), fdParobj = spline_basis)$fd
  
  live_centered_coefs <- sweep(live_fd$coefs, 1, pitch_pca$meanfd$coefs, "-")
  live_centered <- fda::fd(live_centered_coefs, spline_basis)
  
  live_fpca_scores <- fda::inprod(live_centered, pitch_pca$harmonics)
  colnames(live_fpca_scores) <- c("fPC1_Height", "fPC2_Tilt", "fPC3_Curve")
  
  discrete_cols <- c("MFCC1", "MFCC2", "MFCC3", "MFCC4", "MFCC5",
                     "ZCR", "HNR", "SpecEntropy", "Bandwidth",
                     "EnergyDist", "F0Stability", "TemporalCentroid")
  
  live_x <- cbind(as.data.frame(live_fpca_scores), raw_feats[, discrete_cols])
  live_x[!is.finite(as.matrix(live_x))] <- 0
  
  train_x <- as.matrix(bird_train[, 1:15])
  train_y <- bird_train$species
  
  prediction <- weighted_knn(train = train_x, test = live_x, cl = train_y, k = k)
  return(as.character(prediction))
}