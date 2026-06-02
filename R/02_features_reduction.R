#' @importFrom stats approx setNames
NULL

# Prevent R CMD check from flagging your dynamically loaded datasets
utils::globalVariables(c("bird_train", "pitch_pca"))

#' Extract 69 Raw Acoustic Features for fPCA Pipeline
#'
#' @description Performs advanced Digital Signal Processing (DSP) to extract a 50-frame pitch trajectory and 19 distinct spectral/temporal features from a raw audio wave.
#'
#' @param audio_wave A \code{Wave} object from the \code{tuneR} package.
#' @param verbose Logical. If TRUE, prints extraction progress and warnings to the console (default: FALSE).
#'
#' @details
#' This is the core mathematical engine of AvianEchoR. It translates raw amplitude waveforms into a dense numerical vector representing the physical properties of a bird's vocalization.
#'
#' It utilizes Fast Fourier Transforms (FFT) to calculate spectral properties. The 69 features extracted include: 50-frame Pitch Trajectory (for downstream Functional PCA), Spectral Centroid, Bandwidth, Rolloff, Spectral Entropy, Spectral Flux, 5 Mel-Frequency Cepstral Coefficients (MFCC), Zero Crossing Rate (ZCR), Harmonic-to-Noise Ratio (HNR), Log Centroid, F0 Stability, Spectral Slope, Peak Frequency, Energy Distribution, Temporal Centroid, and Spectral Kurtosis.
#'
#' To prevent pipeline crashes during live inference, if an audio segment is purely silent or corrupted, the function safely returns a dataframe populated with NA values.
#'
#' @return A 1-row data frame containing exactly 69 raw acoustic features.
#'
#' @examples
#' \dontrun{
#' wave <- read_audio_file("data-raw/audio/macaw.wav")
#' clean_wave <- standardize_audio(wave)
#'
#' # Extract the 69 features silently
#' raw_features <- extract_audio_features(clean_wave, verbose = FALSE)
#' print(ncol(raw_features)) # Returns 69
#' }
#' @export
extract_audio_features <- function(audio_wave, verbose = FALSE) {

  if (verbose) cat("Starting enhanced feature extraction...\n")

  # ============================================================================
  # STEP 1: PREPROCESS AUDIO
  # Goal: Ensure all incoming audio matrices have uniform dimensions before math.
  # ============================================================================

  # Force stereo recordings into mono to prevent dimension-mismatch crashes.
  if (audio_wave@stereo) {
    audio_wave <- tuneR::mono(audio_wave, "left")
    if (verbose) cat("  Converted to mono\n")
  }

  # Cap the sample rate at 22050 Hz. Higher rates capture frequencies above 11kHz,
  # which mostly contain background hiss rather than usable bird vocalizations.
  if (audio_wave@samp.rate > 22050) {
    audio_wave <- tuneR::downsample(audio_wave, 22050)
    if (verbose) cat("  Downsampled to 22050 Hz\n")
  }

  # ============================================================================
  # STEP 2: SPECTRAL FEATURES (The "Shape" of the Sound in Frequency)
  # Goal: Extract discrete properties using a Fast Fourier Transform (FFT).
  # ============================================================================

  spec <- tryCatch(
    seewave::meanspec(audio_wave, plot = FALSE),
    error = function(e) {
      if (verbose) cat("  WARNING: Spectral analysis failed, using fallback\n")
      matrix(c(0, 0), nrow = 1)
    }
  )

  # FAIL-SAFE: If the audio chunk is corrupted or purely silent, return a structured
  # data frame of NAs so the pipeline doesn't crash during batch processing.
  if (nrow(spec) < 2) {
    if (verbose) cat("  WARNING: Silent audio detected\n")

    num_frames <- 50
    pitch_names <- paste0("Pitch_t", 1:num_frames)
    pitch_na_list <- setNames(as.list(rep(NA, num_frames)), pitch_names)

    return(data.frame(
      pitch_na_list,
      Centroid = NA, Bandwidth = NA, Rolloff = NA,
      SpecEntropy = NA, SpecFlux = NA,
      MFCC1 = NA, MFCC2 = NA, MFCC3 = NA, MFCC4 = NA, MFCC5 = NA,
      ZCR = NA, HNR = NA, LogCentroid = NA, F0Stability = NA,
      SpecSlope = NA, PeakFreq = NA, EnergyDist = NA,
      TemporalCentroid = NA, SpecKurtosis = NA
    ))
  }

  freq <- spec[, 1]
  amp <- spec[, 2]

  # **CENTROID**: The "center of mass" of the spectrum. (Perceptual Brightness)
  centroid <- sum(freq * amp) / sum(amp)

  # **BANDWIDTH**: The standard deviation of frequencies around the centroid.
  # Distinguishes pure whistles (low bandwidth) from raspy caws (high bandwidth).
  bandwidth <- sqrt(sum((freq - centroid)^2 * amp) / sum(amp))

  # **ROLLOFF**: The exact frequency threshold that contains 85% of the total energy.
  cum_energy <- cumsum(amp) / sum(amp)
  rolloff_idx <- which(cum_energy >= 0.85)
  rolloff <- if (length(rolloff_idx) > 0) freq[rolloff_idx[1]] else max(freq)

  # **SPECTRAL ENTROPY**: Measures chaos. Pure tones approach 0; white noise approaches 1.
  amp_norm <- amp / sum(amp)
  spectral_entropy <- -sum(amp_norm * log(amp_norm + 1e-10))

  # **SPECTRAL FLUX**: Measures how quickly the energy shifts over time.
  spectral_flux <- if (length(amp) > 1) mean(abs(diff(amp))) else 0

  # **PEAK FREQUENCY**: The single loudest, most dominant frequency in the chirp.
  peak_freq <- freq[which.max(amp)]

  # **LOG CENTROID**: A non-linear transformation representing how humans/birds hear pitch.
  log_centroid <- log(centroid + 1)

  # **SPECTRAL SLOPE**: Uses a linear model to find the rate of energy decay at high frequencies.
  spectral_slope <- if (length(freq) > 1) {
    lm(amp ~ freq)$coefficients[2]
  } else {
    0
  }

  if (verbose) cat("  [OK] Spectral features extracted\n")

  # ============================================================================
  # STEP 3: PITCH TRAJECTORY (fPCA Preparation)
  # Goal: Extract the melodic "curve" of the bird's song over time.
  # ============================================================================
  num_frames <- 50

  fund_freq <- tryCatch(
    seewave::fund(audio_wave, f = audio_wave@samp.rate, fmax = 8000, plot = FALSE),
    error = function(e) NULL
  )

  # If silence/broken, flatline the curve to zeros.
  if (is.null(fund_freq) || nrow(fund_freq) < 2) {
    pitch_curve <- rep(0, num_frames)
  } else {
    t_raw <- fund_freq[, 1]
    p_raw <- fund_freq[, 2]

    # Clean out inner NAs (micro-silences within a continuous chirp)
    valid_idx <- which(!is.na(p_raw))
    if (length(valid_idx) < 2) {
      pitch_curve <- rep(0, num_frames)
    } else {
      # CRITICAL MATH: We use linear interpolation (approx) to stretch or shrink
      # the raw temporal curve into EXACTLY 50 uniform frames. This allows
      # chirps of different physical lengths to be compared mathematically.
      pitch_curve <- approx(
        x = t_raw[valid_idx],
        y = p_raw[valid_idx],
        n = num_frames
      )$y

      # Pad any trailing NAs created by interpolation
      pitch_curve[is.na(pitch_curve)] <- 0
    }
  }

  # **F0 STABILITY**: Calculates how wildly the pitch fluctuates (standard deviation / mean).
  f0_stability <- if (sum(pitch_curve) > 0) {
    sd(pitch_curve) / (mean(pitch_curve) + 1e-10)
  } else {
    0
  }

  if (verbose) cat("  [OK] Pitch Trajectory extracted\n")

  # ============================================================================
  # STEP 4: MFCCs (Mel-Frequency Cepstral Coefficients)
  # Goal: Capture the biological "timbre" or physical shape of the bird's vocal tract.
  # ============================================================================

  mfcc_vals <- tryCatch(
    tuneR::melfcc(audio_wave, numcep = 5),
    error = function(e) {
      if (verbose) cat("  WARNING: MFCC extraction failed\n")
      matrix(0, nrow = 1, ncol = 5)
    }
  )

  mean_mfccs <- colMeans(mfcc_vals, na.rm = TRUE)
  if (length(mean_mfccs) < 5) mean_mfccs <- c(mean_mfccs, rep(0, 5 - length(mean_mfccs)))

  if (verbose) cat("  [OK] MFCC features extracted\n")

  # ============================================================================
  # STEP 5: ZERO CROSSING RATE (ZCR)
  # Goal: Detect percussiveness (e.g., Woodpecker drumming vs. Owl hooting).
  # ============================================================================

  zcr_data <- tryCatch(
    seewave::zcr(audio_wave, plot = FALSE),
    error = function(e) {
      if (verbose) cat("  WARNING: ZCR extraction failed\n")
      matrix(0, nrow = 1, ncol = 2)
    }
  )

  mean_zcr <- if (nrow(zcr_data) > 0) mean(zcr_data[, 2], na.rm = TRUE) else 0

  if (verbose) cat("  [OK] ZCR extracted\n")

  # ============================================================================
  # STEP 6: HARMONIC-TO-NOISE RATIO (HNR)
  # Goal: Determine if the sound is a tonal song (high HNR) or a harsh screech (low HNR).
  # ============================================================================

  hnr <- tryCatch({
    audio_vec <- as.numeric(audio_wave@left)
    fft_result <- abs(fft(audio_vec))[1:(length(audio_vec) / 2)]
    harmonic_energy <- max(fft_result) / (mean(fft_result) + 1e-10)
    log10(harmonic_energy + 1)
  }, error = function(e) {
    if (verbose) cat("  WARNING: HNR calculation failed\n")
    0
  })

  if (verbose) cat("  [OK] HNR calculated\n")

  # ============================================================================
  # STEP 7: ENERGY DISTRIBUTION & TEMPORAL FEATURES
  # Goal: Analyze how physical loudness is distributed across the chunk of time.
  # ============================================================================

  audio_vec <- as.numeric(audio_wave@left)
  squared <- audio_vec^2

  # **TEMPORAL CENTROID**: Determines if the chirp is front-heavy or back-heavy.
  temporal_centroid <- sum(seq_along(squared) * squared) / sum(squared)
  temporal_centroid <- temporal_centroid / length(audio_vec)

  # **ENERGY DISTRIBUTION**: How widely spread the energy is from the temporal center.
  energy_distribution <- sqrt(sum((seq_along(squared) - mean(seq_along(squared)))^2 * squared) / sum(squared)) / length(squared)

  # **SPECTRAL KURTOSIS**: Identifies the 'peakedness' of the spectrum (sharp spikes vs flat noise).
  amp_mean <- mean(amp)
  amp_std <- sd(amp)
  spectral_kurtosis <- mean(((amp - amp_mean) / (amp_std + 1e-10))^4)

  if (verbose) cat("  [OK] Energy/temporal features extracted\n")

  # ============================================================================
  # STEP 8: ASSEMBLE FEATURE VECTOR
  # Goal: Bind the 50 pitch frames and 19 discrete values into a strict 1x69 row.
  # ============================================================================

  # Bind the 50 Pitch frames as individual columns (Pitch_t1, Pitch_t2, etc.)
  pitch_names <- paste0("Pitch_t", 1:num_frames)
  pitch_list <- setNames(as.list(pitch_curve), pitch_names)

  features <- data.frame(
    pitch_list,
    Centroid = centroid,
    Bandwidth = bandwidth,
    Rolloff = rolloff,
    SpecEntropy = spectral_entropy,
    SpecFlux = spectral_flux,
    MFCC1 = mean_mfccs[1],
    MFCC2 = mean_mfccs[2],
    MFCC3 = mean_mfccs[3],
    MFCC4 = mean_mfccs[4],
    MFCC5 = mean_mfccs[5],
    ZCR = mean_zcr,
    HNR = hnr,
    LogCentroid = log_centroid,
    F0Stability = f0_stability,
    SpecSlope = spectral_slope,
    PeakFreq = peak_freq,
    EnergyDist = energy_distribution,
    TemporalCentroid = temporal_centroid,
    SpecKurtosis = spectral_kurtosis
  )

  if (verbose) cat("[OK] Feature extraction complete (69 features)\n")

  return(features)
}



#' Diagnose Feature Matrix Quality and Distribution
#'
#' @description Generates a comprehensive console report checking the mathematical health, variance, and class balance of an extracted feature dataset.
#'
#' @param features A data frame of numeric acoustic features.
#' @param labels Optional. A factor or character vector of class labels to check for dataset imbalance (default: NULL).
#'
#' @details
#' Before feeding data into a kNN or clustering algorithm, it must be mathematically sound. This function acts as a diagnostic tool. It checks for constant features (zero variance) which add no predictive power, calculates the percentage of missing values, and outputs the min/max/mean ranges to determine if standard scaling is required. If labels are provided, it warns the user if the class imbalance ratio exceeds 2:1.
#'
#' @examples
#' 
#' data(bird_train)
#' features <- bird_train[, 1:15]
#' labels <- bird_train$species
#'
#' diagnose_features(features, labels)
#' 
#' @export
diagnose_features <- function(features, labels = NULL) {

  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("           FEATURE QUALITY DIAGNOSTIC REPORT\n")
  cat("-----------------------------------------------------------\n\n")

  cat("DATASET DIMENSIONS:\n")
  cat("  Samples:  ", nrow(features), "\n")
  cat("  Features: ", ncol(features), "\n")

  if (!is.null(labels)) {
    cat("  Classes:  ", nlevels(as.factor(labels)), "\n")
    cat("\n  CLASS DISTRIBUTION:\n")
    class_table <- table(labels)
    for (i in seq_along(class_table)) {
      cat(sprintf("    %s: %d (%.1f%%)\n",
                  names(class_table)[i],
                  class_table[i],
                  100 * class_table[i] / length(labels)))
    }

    imbalance_ratio <- max(class_table) / min(class_table)
    if (imbalance_ratio > 2) {
      cat(sprintf("\n  [WARNING]: High class imbalance (ratio: %.2f)\n", imbalance_ratio))
      cat("     Consider: Stratified CV or class weighting\n")
    } else {
      cat("  [OK] Classes reasonably balanced\n")
    }
  }

  cat("\n")
  cat("MISSING VALUE ANALYSIS:\n")
  na_counts <- colSums(!is.finite(as.matrix(features)))
  na_pct <- 100 * na_counts / nrow(features)

  problem_features <- names(na_pct)[na_pct > 10]
  if (length(problem_features) > 0) {
    cat("  [WARNING] Features with >10% missing:\n")
    for (f in problem_features) {
      cat(sprintf("    - %s: %.1f%% missing\n", f, na_pct[f]))
    }
  } else if (any(na_pct > 0)) {
    cat("  [WARNING] Features with some missing values:\n")
    for (f in names(na_pct)[na_pct > 0]) {
      cat(sprintf("    - %s: %.1f%% missing\n", f, na_pct[f]))
    }
  } else {
    cat("  [OK] No missing values detected\n")
  }

  cat("\n")
  cat("FEATURE VARIANCE:\n")
  variances <- apply(features, 2, var, na.rm = TRUE)
  variances <- variances[is.finite(variances)]

  zero_var <- names(variances)[variances < 1e-10]
  if (length(zero_var) > 0) {
    cat("  [WARNING] Constant features (zero variance):\n")
    for (f in zero_var) {
      cat(sprintf("    - %s\n", f))
    }
  } else {
    cat("  [OK] All features have sufficient variance\n")
  }

  cat("\n")
  cat("FEATURE RANGES:\n")
  for (i in seq_len(ncol(features))) {
    col <- as.numeric(features[, i])
    col <- col[is.finite(col)]

    if (length(col) > 0) {
      cat(sprintf("  %s:\n", names(features)[i]))
      cat(sprintf("    Mean: %.4f, SD: %.4f\n", mean(col), sd(col)))
      cat(sprintf("    Range: [%.4f, %.4f]\n", min(col), max(col)))
    }
  }

  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("[OK] Diagnostic complete\n\n")
}



#' Feature Selection via Pearson Correlation
#'
#' @description Identifies and isolates the acoustic features that hold the highest absolute correlation with the target species labels.
#'
#' @param features A numeric feature matrix.
#' @param labels A vector of class labels (will be temporarily coerced to numeric for calculation).
#' @param n_features Integer. The strict number of top features to retain (default: 10).
#'
#' @details
#' High-dimensional acoustic data often contains redundant features. This function calculates the absolute Pearson correlation coefficient between every individual feature and the target variable. It sorts them in descending order to help identify the most biologically relevant predictors.
#'
#' @return A list containing the \code{selected_features} names, their top \code{correlations}, and the \code{all_correlations} vector.
#'
#' @examples
#' 
#' data(bird_train)
#' feats <- bird_train[, 1:15]
#' labels <- bird_train$species
#'
#' # Find top 5 most predictive features
#' top_features <- select_features_by_correlation(feats, labels, n_features = 5)
#' print(top_features$selected_features)
#' 
#' @export
select_features_by_correlation <- function(features, labels, n_features = 10) {

  X <- as.matrix(features)
  y_numeric <- as.numeric(as.factor(labels))

  correlations <- numeric(ncol(X))
  for (j in seq_len(ncol(X))) {
    cor_val <- cor(X[, j], y_numeric, use = "complete.obs")
    correlations[j] <- abs(cor_val)
  }

  sorted_idx <- order(correlations, decreasing = TRUE)
  selected_idx <- sorted_idx[1:min(n_features, length(sorted_idx))]
  selected_names <- colnames(X)[selected_idx]

  return(list(
    selected_features = selected_names,
    correlations = correlations[selected_idx],
    all_correlations = correlations
  ))
}

#' Non-Linear Dimensionality Reduction Wrapper (t-SNE & UMAP)
#'
#' @description A safe wrapper function to project high-dimensional acoustic data into 2D space for visualization using manifold learning.
#'
#' @param features A data frame or matrix of scaled numeric features.
#' @param method Character string: "tsne" or "umap" (default: "umap").
#' @param dims Integer. Number of dimensions for the output projection (default: 2).
#' @param perplexity Numeric. The perplexity parameter, specifically used if method is "tsne" (default: 30).
#'
#' @details
#' This function safely wraps the \code{Rtsne} and \code{uwot} packages. It includes fail-safes such as setting \code{n_threads = 1} for UMAP to prevent C++ deadlock crashes on Windows machines, and automatically capping t-SNE perplexity to prevent it from exceeding the dataset row limits.
#'
#' @return A data frame with exactly two columns: "Dim1" and "Dim2".
#'
#' @examples
#' 
#' data(bird_train)
#' # Scale data before UMAP projection
#' scaled_feats <- scale(as.matrix(bird_train[, 1:15]))
#'
#' # Create a 2D UMAP embedding
#' umap_2d <- map_acoustic_nonlinear(scaled_feats, method = "umap", dims = 2)
#' plot(umap_2d$Dim1, umap_2d$Dim2)
#' 
#' @export
map_acoustic_nonlinear <- function(features, method = "umap", dims = 2, perplexity = 30) {

  if (method == "tsne") {
    tsne_result <- Rtsne::Rtsne(
      as.matrix(features), dims = dims,
      perplexity = min(perplexity, nrow(features) / 3),
      check_duplicates = FALSE
    )
    result_df <- as.data.frame(tsne_result$Y)
    colnames(result_df) <- c("Dim1", "Dim2")

  } else if (method == "umap") {
    umap_result <- uwot::umap(
      as.matrix(features),
      n_components = dims,
      n_threads = 1,
      min_dist = 0.1
    )
    result_df <- as.data.frame(umap_result)
    colnames(result_df) <- c("Dim1", "Dim2")
  }

  return(result_df)
}

#' Custom Principal Component Analysis (PCA)
#'
#' @description Performs linear dimension reduction via explicit eigenvalue decomposition of the covariance matrix.
#'
#' @param features A data frame or matrix of numeric features (samples as rows, features as columns).
#' @param n_components Integer. The number of principal components to return (default: 3).
#'
#' @details
#' \strong{Mathematical Foundation:}
#' This function implements PCA manually without relying on base R's \code{prcomp}. It finds orthogonal directions (eigenvectors) that maximize variance in the dataset by:
#' \enumerate{
#'   \item Centering the data against the mean.
#'   \item Computing the covariance matrix.
#'   \item Performing eigendecomposition to yield eigenvalues and eigenvectors.
#'   \item Projecting the original data matrix onto the top \code{k} eigenvectors.
#' }
#'
#' @return A comprehensive list containing the PC \code{scores}, eigenvector \code{loadings}, \code{eigenvalues}, and \code{cumulative_variance} explained.
#'
#' @examples
#' 
#' data(bird_train)
#' feats <- bird_train[, 1:15]
#'
#' # Extract top 3 Principal Components
#' pca_res <- reduce_acoustic_pca(feats, n_components = 3)
#'
#' # View variance explained by the first component
#' print(pca_res$variance_explained)
#' 
#' @export
reduce_acoustic_pca <- function(features, n_components = 3) {

  X <- as.matrix(features)
  X_centered <- scale(X, center = TRUE, scale = FALSE)
  cov_matrix <- (1 / (nrow(X) - 1)) * t(X_centered) %*% X_centered
  eigen_result <- eigen(cov_matrix, symmetric = TRUE)

  n_comp_actual <- min(n_components, length(eigen_result$values))
  eigenvalues <- eigen_result$values[1:n_comp_actual]
  eigenvectors <- eigen_result$vectors[, 1:n_comp_actual]

  pc_scores <- X_centered %*% eigenvectors
  colnames(pc_scores) <- paste0("PC", 1:n_comp_actual)

  total_variance <- sum(eigen_result$values)
  variance_explained <- eigenvalues / total_variance
  cumulative_variance <- cumsum(variance_explained)

  return(list(
    scores = as.data.frame(pc_scores),
    loadings = eigenvectors,
    eigenvalues = eigenvalues,
    variance_explained = variance_explained,
    cumulative_variance = cumulative_variance,
    mean = attr(X_centered, "scaled:center"),
    original_features = colnames(X)
  ))
}
