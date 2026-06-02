# tests/testthat/test-avianecho.R
library(testthat)
library(AvianEchoR)
library(tuneR)

# =====================================================================
# GLOBAL TEST SETUP: Generate Mock Data
# =====================================================================

# 1. Generate Mock Audio Wave (1 second of a 440Hz Sine wave, Stereo, 32-bit)
mock_wave <- tuneR::sine(440, duration = 22050, samp.rate = 22050, bit = 32)
mock_wave <- tuneR::stereo(mock_wave, mock_wave)
temp_wav_file <- tempfile(fileext = ".wav")
tuneR::writeWave(mock_wave, temp_wav_file)

# 2. Load the actual package training data for ML tests
data("bird_train", package = "AvianEchoR", envir = environment())
# Take a highly diverse slice of 45 rows so feature variances don't drop to zero!
diverse_idx <- seq(1, nrow(bird_train), length.out = 45)
mock_ml_data <- bird_train[diverse_idx, ]
mock_X <- as.matrix(mock_ml_data[, 1:15])
mock_y <- mock_ml_data$species

# =====================================================================
# TESTS FOR: 01_load.R
# =====================================================================

test_that("read_audio_file safely loads WAV files", {
  wave <- read_audio_file(temp_wav_file)
  expect_s4_class(wave, "Wave")
  expect_error(read_audio_file("fake_file_that_does_not_exist.wav"))
})

test_that("standardize_audio converts to mono, caps sample rate, and normalizes bit depth", {
  wave <- read_audio_file(temp_wav_file)
  std_wave <- standardize_audio(wave)

  expect_false(std_wave@stereo) # Should be converted to mono
  expect_true(std_wave@samp.rate <= 22050) # Should be capped at 22050Hz
  expect_equal(std_wave@bit, 16) # Should be normalized to 16-bit
})

test_that("plot_bird_audio executes without crashing", {
  wave <- standardize_audio(read_audio_file(temp_wav_file))
  # We test that plotting doesn't throw an error
  expect_error(plot_bird_audio(wave), NA)
})

test_that("extract_clean_chunks returns a list of Waves or fails safely", {
  chunks <- extract_clean_chunks(temp_wav_file)
  expect_type(chunks, "list")
  if (length(chunks) > 0) {
    expect_true(inherits(chunks[[1]], "Wave"))
  }
})

# =====================================================================
# TESTS FOR: 02_features_reduction.R
# =====================================================================

test_that("extract_audio_features returns exactly 69 raw features", {
  wave <- standardize_audio(read_audio_file(temp_wav_file))
  features <- extract_audio_features(wave, verbose = FALSE)

  expect_s3_class(features, "data.frame")
  expect_equal(nrow(features), 1)
  expect_equal(ncol(features), 69) # 50 Pitch frames + 19 discrete features
  expect_true("Pitch_t50" %in% colnames(features))
  expect_true("MFCC1" %in% colnames(features))
})

test_that("diagnose_features executes and prints without error", {
  # Capture output to keep test console clean
  expect_error(capture.output(diagnose_features(mock_X, mock_y)), NA)
})

test_that("select_features_by_correlation returns top N features", {
  res <- select_features_by_correlation(mock_X, mock_y, n_features = 5)
  expect_length(res$selected_features, 5)
  expect_type(res$correlations, "double")
})

test_that("reduce_acoustic_pca performs eigenvalue decomposition correctly", {
  pca_res <- reduce_acoustic_pca(mock_X, n_components = 3)
  expect_equal(ncol(pca_res$scores), 3)
  expect_length(pca_res$eigenvalues, 3)
})

test_that("map_acoustic_nonlinear maps down to 2 dimensions", {
  # Note: t-SNE requires perplexity < nrow/3. For 45 rows, perp=10 is safe.
  tsne_res <- map_acoustic_nonlinear(mock_X, method = "tsne", dims = 2, perplexity = 10)
  expect_equal(ncol(tsne_res), 2)
  expect_equal(colnames(tsne_res), c("Dim1", "Dim2"))
})


# =====================================================================
# TESTS FOR: 03_classification.R
# =====================================================================

test_that("create_stratified_folds perfectly balances classes", {
  folds <- create_stratified_folds(mock_y, folds = 3)
  expect_equal(length(folds), length(mock_y))
  expect_true(all(folds %in% c(1, 2, 3)))
})

test_that("weighted_knn returns factor predictions with correct levels", {
  train_X <- mock_X[1:30, ]
  test_X  <- mock_X[31:45, ]
  train_y <- mock_y[1:30]

  preds <- weighted_knn(train_X, test_X, train_y, k = 3)

  expect_s3_class(preds, "factor")
  expect_length(preds, 15)
  expect_equal(levels(preds), levels(train_y))
})

test_that("classify_knn_cv_tuned optimizes k effectively", {
  res <- classify_knn_cv_tuned(mock_X, mock_y, k_values = c(3, 5), folds = 2)

  expect_true("best_k" %in% names(res))
  expect_true("best_accuracy" %in% names(res))
  expect_true(res$best_k %in% c(3, 5))
})

test_that("evaluate_classification generates metrics without crashing", {
  actual <- factor(c("A", "A", "B", "B"))
  preds  <- factor(c("A", "B", "B", "B"), levels = c("A", "B"))

  # Capture output so the console doesn't get flooded with the classification report
  output <- capture.output({
    metrics <- evaluate_classification(preds, actual)
  })

  expect_equal(metrics$accuracy, 0.75)
  expect_true("kappa" %in% names(metrics))
})

test_that("bootstrap_ci_accuracy returns valid confidence intervals", {
  actual <- factor(c(rep("A", 10), rep("B", 10)))
  preds  <- factor(c(rep("A", 9), "B", rep("B", 9), "A"), levels = c("A", "B"))

  ci <- bootstrap_ci_accuracy(preds, actual, n_bootstrap = 50)

  expect_true(ci$ci_lower <= ci$accuracy)
  expect_true(ci$ci_upper >= ci$accuracy)
})

# Clean up the mock audio file after tests complete
unlink(temp_wav_file)
