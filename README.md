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

## Built With

* R (DSP & Statistical Modeling)
* Shiny (Interactive Dashboard)
* ggplot2 / plotly (Visualizations)
