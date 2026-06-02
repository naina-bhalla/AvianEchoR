# AvianEchoR : Statistical Analysis of Bird Calls

`AvianEchoR` is a comprehensive R package designed for ornithologists and statistical researchers. It processes raw bioacoustic recordings into a 15-dimensional hybrid feature space, utilizing Digital Signal Processing (DSP) and Fast Fourier Transforms (FFT) to extract 69 spectral and temporal properties for high-accuracy species classification.

## Installation

You can install the development version of AvianEchoR directly from GitHub:

```R
# install.packages("devtools")
devtools::install_github("naina-bhalla/AvianEchoR")

```

## Core Features

* **Digital Signal Processing (DSP) Pipeline:** Extracts 69 discrete acoustic features (MFCCs, ZCR, HNR) and 50-frame pitch trajectories directly from raw audio streams.
* **Statistical Machine Learning:** Implements Functional PCA (fPCA) and Distance-Weighted k-Nearest Neighbors (kNN) equipped with stratified cross-validation.
* **Dimensionality Reduction:** Includes native support for non-linear mappings via t-SNE, UMAP, and standard PCA.
* **Real-Time Inference:** Bundled with a fully interactive Shiny dashboard for dynamic data monitoring and live audio classification.

## Quick Start

```R
library(AvianEchoR)

# Load the bundled dataset
data("avian_data")

# Launch the interactive dashboard
launch_avian_dashboard()

```

## Pipeline

### 1. Data Ingestion & Standardization

Raw acoustic data is notoriously noisy and dimensionally inconsistent. The package handles ingestion safely:

* **`read_audio_file()`**: Automatically detects and safely decompresses `.wav` and `.mp3` files via the `tuneR` engine.


* **`standardize_audio()`**: Normalizes structures by forcing stereo channels to mono, downsampling to 22050 Hz for dimensional consistency, and normalizing to a 16-bit amplitude scale.


* **`extract_clean_chunks()`**: Executes Voice Activity Detection (VAD) using a 150 Hz high-pass filter to strip wind rumble, followed by amplitude thresholding to isolate pure vocal energy spikes.



### 2. Digital Signal Processing (DSP)

* **`extract_audio_features()`**: The DSP engine utilizes Fast Fourier Transforms (FFT) to extract 69 raw acoustic features from isolated waveforms.


* Extracted predictors include a 50-frame pitch trajectory alongside 19 distinct spectral properties such as Mel-Frequency Cepstral Coefficients (MFCCs), Zero Crossing Rate (ZCR), Spectral Entropy, and Harmonic-to-Noise Ratio (HNR).


* **`diagnose_features()`**: Evaluates mathematical health by scanning matrices for zero-variance (constant) features, missing values, and class imbalances.



### 3. Dimensionality Reduction & Manifold Learning

To prevent the curse of dimensionality, the 69-feature matrix is compressed into a 15-dimensional hybrid space.

* **`pitch_pca`**: A pre-fitted Functional PCA model that projects 50-frame pitch trajectories into 3 principal shape harmonics: Height, Tilt, and Curve.


* **Leakage Prevention**: To guarantee unbiased inference, new test data is mathematically centered using exclusively the training baseline mean (`meanfd`) prior to eigenvector projection.


* **`reduce_acoustic_pca()`**: Performs linear reduction via explicit eigendecomposition of the covariance matrix.


* **`map_acoustic_nonlinear()`**: Wraps t-SNE and UMAP, implementing fail-safes such as single-threading to prevent C++ deadlocks during projection.


* **`select_features_by_correlation()`**: Isolates predictive variables by calculating absolute Pearson correlation coefficients against target labels.



### 4. Machine Learning & Statistical Validation

* **`weighted_knn()`**: Implements a custom Distance-Weighted kNN where a neighbor's voting power is inversely proportional to its Euclidean distance: $w_i = \frac{1}{d_i + \epsilon}$.


* **`classify_knn_cv_tuned()`**: Wraps the classifier in stratified k-fold cross-validation to tune hyperparameters while preventing data leakage.


* **`create_stratified_folds()`**: Ensures proportional class distributions across all validation folds to prevent minority class omission.


* **`evaluate_classification()`**: Generates macro-averaged metrics including Precision, Recall, Specificity, F1-Scores, and Cohen's Kappa.


* **`bootstrap_ci_accuracy()`**: Calculates a robust 95% confidence interval for classification accuracy utilizing non-parametric bootstrap resampling with 1,000 iterations.



---

## Live Inference

`AvianEchoR` supports both programmatic inference and interactive GUI exploration.

**Console-Based Live Inference:**

```R
library(AvianEchoR)

# Predict species directly from a raw audio file
prediction <- predict_live_audio("path/to/audio.wav", k = 5)
print(prediction)

# Generate an oscillogram and spectrogram for visual inspection
wave <- standardize_audio(read_audio_file("path/to/audio.wav"))
plot_bird_audio(wave)

```

**Interactive Shiny Dashboard:**
The package bundles a full GUI for end-to-end evaluation without writing code.

```R
# Launch the dashboard locally
launch_avian_dashboard()

```

---

## Bundled Data

* **`bird_train`**: The balanced 15-dimensional training matrix representing 45 bird species.


* **`bird_test`**: A strictly isolated holdout dataset mirroring the training schema for unbiased evaluation.

---

## Built With

* R (DSP & Statistical Modeling)
* Shiny (Interactive Dashboard)
* ggplot2 / plotly (Visualizations)
