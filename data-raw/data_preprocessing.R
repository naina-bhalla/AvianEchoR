# ==============================================================================
# SCRIPT 3: LEAK-FREE PREPROCESSING & fPCA FEATURE HYBRIDIZATION
# ==============================================================================
# Package: AvianEchoR
# Purpose: Finalizes the dataset for machine learning by enforcing class balance,
#          handling missing values, and engineering the 15-Dimensional fPCA space.
#
# Integrity Rules (Preventing Data Leakage):
#   - Splits data 80/20 (Train/Test) BEFORE any mathematical transformation.
#   - fPCA mathematically centers the Test curves using only the Training Mean.
#
# Output:
#   - Generates the compiled `bird_train` and `bird_test` datasets.
#   - Saves the `pitch_pca` mathematical model.
# ==============================================================================

library(dplyr)

# ==============================================================================
# 1. LOAD AND COMBINE RAW DATA
# ==============================================================================
cat("Loading raw extracted features...\n")
raw_train <- readRDS("raw-data/bird_train.rds")
raw_test <- readRDS("raw-data/bird_test.rds")

# Bring all the chunks together
all_chunks <- rbind(raw_train, raw_test)

# Add a unique row ID so we can cleanly split them later without duplicates
all_chunks$row_id <- 1:nrow(all_chunks)

# ==============================================================================
# 2. FILTER AND EQUALIZE CHUNKS
# ==============================================================================
# Drop any bird that suffered catastrophic failure (e.g., fewer than 110 chunks)
chunk_counts <- table(all_chunks$species)
valid_species <- names(chunk_counts[chunk_counts >= 110])
all_chunks <- all_chunks %>% filter(species %in% valid_species)

# Find the lowest common denominator of the healthy birds
chunk_counts <- table(all_chunks$species)
min_chunks <- min(chunk_counts)

cat(sprintf("\nBalancing dataset: Capping every species at exactly %d chunks.\n", min_chunks))

# Perfectly equalize the classes
balanced_data <- all_chunks %>%
  group_by(species) %>%
  slice_sample(n = min_chunks) %>%
  ungroup()

# ==============================================================================
# 3. THE HARD SPLIT (80/20)
# ==============================================================================
# Group by species to ensure an exact 80/20 split within every single bird category
new_train <- balanced_data %>%
  group_by(species) %>%
  slice_sample(prop = 0.8) %>%
  ungroup()

# The test set gets whatever rows were NOT picked for the training set
new_test <- balanced_data %>%
  filter(!row_id %in% new_train$row_id)

# ==============================================================================
# 4. FUNCTIONAL PCA (fPCA) ON PITCH TRAJECTORY
# ==============================================================================
library(fda)
cat("Applying Functional PCA to Pitch Trajectories...\n")

# Isolate the 50 pitch columns we just created
pitch_cols <- paste0("Pitch_t", 1:50)

train_pitch_mat <- as.matrix(new_train[, pitch_cols])
test_pitch_mat <- as.matrix(new_test[, pitch_cols])

# The fda package requires timeframes as rows, and samples as columns
t_train <- t(train_pitch_mat)
t_test <- t(test_pitch_mat)

# 1. Define the mathematical "Canvas" (B-splines)
time_points <- 1:50
spline_basis <- create.bspline.basis(rangeval = c(1, 50), nbasis = 15)

# 2. Smooth the raw frames into continuous functional curves
train_fd <- smooth.basis(argvals = time_points, y = t_train, fdParobj = spline_basis)$fd
test_fd <- smooth.basis(argvals = time_points, y = t_test, fdParobj = spline_basis)$fd

# 3. Run fPCA on the Training data (Extract the top 3 shapes)
pitch_pca <- pca.fd(train_fd, nharm = 3)

# 4. Extract the Train Scores and project the Test Scores
train_fpca_scores <- pitch_pca$scores
colnames(train_fpca_scores) <- c("fPC1_Height", "fPC2_Tilt", "fPC3_Curve")

# Center the test data using the TRAIN mean to prevent data leakage.
# (Extract coefficients, sweep the mean mathematically, and rebuild the object)
test_centered_coefs <- sweep(test_fd$coefs, 1, pitch_pca$meanfd$coefs, "-")
test_centered <- fda::fd(test_centered_coefs, spline_basis)

test_fpca_scores <- inprod(test_centered, pitch_pca$harmonics)
colnames(test_fpca_scores) <- c("fPC1_Height", "fPC2_Tilt", "fPC3_Curve")

# ==============================================================================
# 5. RECONSTRUCT THE 15-FEATURE HYBRID MATRIX
# ==============================================================================
cat("Formatting to the 15-feature hybrid acoustic space...\n")

discrete_cols <- c(
  "MFCC1", "MFCC2", "MFCC3", "MFCC4", "MFCC5",
  "ZCR", "HNR", "SpecEntropy", "Bandwidth",
  "EnergyDist", "F0Stability", "TemporalCentroid"
)

# Extract Discrete Features
train_x_discrete <- as.matrix(new_train[, discrete_cols])
test_x_discrete <- as.matrix(new_test[, discrete_cols])

# Bind the 3 fPCA Scores with the 12 Discrete Features
train_x <- cbind(train_fpca_scores, train_x_discrete)
test_x <- cbind(test_fpca_scores, test_x_discrete)

# Scrub any stray NAs (Pure silence chunks)
train_x[!is.finite(train_x)] <- 0
test_x[!is.finite(test_x)] <- 0

# ==============================================================================
# 6. COMPILE INTO PACKAGE MEMORY
# ==============================================================================
# We save the RAW (but scrubbed) data.
# The Shiny app will handle scaling dynamically during evaluation/inference.

bird_train <- as.data.frame(train_x)
bird_train$species <- as.factor(new_train$species)

bird_test <- as.data.frame(test_x)
bird_test$species <- as.factor(new_test$species)

cat(sprintf("\nPipeline Complete!\nTrain dimensions: %d rows\nTest dimensions: %d rows\n",
            nrow(bird_train), nrow(bird_test)))

# Save the datasets and the fPCA model directly to the package's internal data directory
usethis::use_data(bird_train, overwrite = TRUE)
usethis::use_data(bird_test, overwrite = TRUE)
usethis::use_data(pitch_pca, overwrite = TRUE)

cat("\n✅ SUCCESS! Preprocessed data and fPCA engine locked into AvianEchoR memory.\n")
