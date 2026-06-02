#' Training Data for Acoustic Extremes Bird Classification
#'
#' A custom, balanced training dataset containing extracted bioacoustic features from 45
#' highly distinct global and Amazonian bird species. This data reduces raw audio
#' waveforms into a 15-dimensional hybrid space combining 3 Functional PCA shape metrics
#' and 12 discrete spectral/temporal properties.
#'
#' @format A data frame with extracted acoustic features and a `species` label factor.
#' \describe{
#'   \item{fPC1_Height}{Principal Component 1 of the pitch trajectory (Overall pitch height).}
#'   \item{fPC2_Tilt}{Principal Component 2 of the pitch trajectory (Rising vs. falling slope).}
#'   \item{fPC3_Curve}{Principal Component 3 of the pitch trajectory (Concavity/convexity).}
#'   \item{MFCC1}{Mel-Frequency Cepstral Coefficient 1.}
#'   \item{MFCC2}{Mel-Frequency Cepstral Coefficient 2.}
#'   \item{MFCC3}{Mel-Frequency Cepstral Coefficient 3.}
#'   \item{MFCC4}{Mel-Frequency Cepstral Coefficient 4.}
#'   \item{MFCC5}{Mel-Frequency Cepstral Coefficient 5.}
#'   \item{ZCR}{Zero Crossing Rate, distinguishing percussive clicks from smooth hoots.}
#'   \item{HNR}{Harmonic-to-Noise Ratio.}
#'   \item{SpecEntropy}{Spectral entropy, measuring disorder/complexity.}
#'   \item{Bandwidth}{Spectral bandwidth, indicating frequency spread.}
#'   \item{EnergyDist}{Spread of energy distribution across the temporal chunk.}
#'   \item{F0Stability}{Variance in the fundamental frequency over time.}
#'   \item{TemporalCentroid}{The point in time where the majority of energy is concentrated.}
#'   \item{species}{The scientific name of the bird species.}
#' }
#' @source Custom dataset engineered via the Xeno-canto API.
#' @examples
#' 
#' data(bird_train)
#' head(bird_train)
#' table(bird_train$species)
#' 
"bird_train"


#' Testing Data for Acoustic Extremes Bird Classification
#'
#' A custom, strictly isolated holdout dataset containing extracted bioacoustic features
#' from 45 highly distinct bird species. This data perfectly mirrors the schema of
#' \code{bird_train} and is explicitly reserved for evaluating model accuracy without
#' data leakage.
#'
#' @format A data frame with extracted acoustic features and a \code{species} label factor.
#' Identical in structure to \code{\link{bird_train}}.
#'
#' @source Custom dataset engineered via the Xeno-canto API.
#' @examples
#' 
#' data(bird_test)
#'
#' # Check the exact number of pristine holdout chunks
#' nrow(bird_test)
#' 
"bird_test"


#' Functional PCA Model for Bird Pitch Trajectories
#'
#' A pre-fitted Functional Principal Component Analysis (fPCA) model, trained on the
#' 50-frame pitch trajectories of the AvianEchoR training dataset. This model is
#' critically required during live inference to project new, unseen audio recordings
#' into the established 3-dimensional functional space.
#'
#' @details
#' By saving this historical model, the package ensures zero data leakage during
#' the testing or live inference phases. New audio pitch curves are mathematically
#' centered using this model's internal training mean (\code{meanfd}) and projected
#' using its established eigenvectors (\code{harmonics}).
#'
#' @format An object of class \code{pca.fd} (from the \code{fda} package). Key components include:
#' \describe{
#'   \item{harmonics}{A functional data (\code{fd}) object containing the 3 principal shape harmonics (Height, Tilt, Curve).}
#'   \item{values}{The eigenvalues indicating the variance explained by each harmonic.}
#'   \item{scores}{The fPCA scores of the original training data.}
#'   \item{meanfd}{A functional data (\code{fd}) object representing the baseline mean pitch trajectory of the entire training set.}
#' }
#' @source Custom model fitted during the AvianEchoR preprocessing pipeline.
#' @examples
#' 
#' data(pitch_pca)
#'
#' # View the proportion of variance explained by the 3 functional harmonics
#' print(pitch_pca$varprop)
#' 
"pitch_pca"

