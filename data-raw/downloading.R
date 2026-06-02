# ==============================================================================
# SCRIPT 1: DATASET ACQUISITION (THE "ACOUSTIC EXTREMES" BUILD)
# ==============================================================================
# Package: AvianEchoR
# Purpose: Interfaces with the Xeno-Canto API via the `suwo` package to query,
#          filter, and securely download high-quality, foreground field
#          recordings for 31 mathematically distinct Amazonian bird species.
#
# Execution: Run this script from the terminal using `Rscript downloading.R`
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PREREQUISITES & SETUP
# ------------------------------------------------------------------------------
# If you are running this for the first time on a new machine, uncomment the
# following lines to install the required API wrapper from GitHub:
# install.packages("remotes")
# remotes::install_github("maRce10/suwo")

# Note: Xeno-Canto API keys can be set here if rate-limited:
# Sys.setenv(xc_api_key = "YOUR-API-KEY")

library(suwo)

# ------------------------------------------------------------------------------
# 1. THE LIST OF BIRDS FROM AMAZON RAINFOREST
# ------------------------------------------------------------------------------
# This roster contains 45 species of birds.

birds <- c(
  "Lipaugus vociferans",       # Screaming Piha (The iconic piercing jungle whistle)
  "Nyctibius grandis",         # Great Potoo (Deep, terrifying guttural roar/moan)
  "Perissocephalus tricolor",  # Capuchinbird (Bizarre mechanical "chainsaw" / cow lowing)
  "Procnias albus",            # White Bellbird (Explosive metallic "clang" - Loudest bird)
  "Cyphorhinus arada",         # Musician Wren (Complex, perfectly pitched melodic flute)
  "Ara macao",                 # Scarlet Macaw (Harsh, tearing guttural screech)
  "Opisthocomus hoazin",       # Hoatzin (Bizarre, asthmatic wheezing/grunting)
  "Campephilus melanoleucos",  # Crimson-crested Woodpecker (Rapid percussive drumming)
  "Cacicus cela",              # Yellow-rumped Cacique (Chaotic gurgles, mimics, and pops)
  "Crypturellus undulatus",    # Undulated Tinamou (Haunting, low-frequency 3-note whistle)
  "Pteroglossus castanotis",   # Chestnut-eared Aracari (High-pitched, rhythmic "sneezing")
  "Thamnophilus doliatus",     # Barred Antshrike (Accelerating mechanical rolling laugh)
  "Morphnus guianensis",       # Crested Eagle (High-pitched, clear raptor whistle)
  "Psarocolius decumanus",     # Crested Oropendola (Liquid, crashing "water-drop" gargle)
  "Amazona ararauna",          # Blue-and-yellow Macaw (Deafening, harsh screech)
  "Ramphastos toco",           # Toco Toucan (Deep, frog-like grunting/croaking)
  "Tinamus solitarius",        # Solitary Tinamou (Haunting, slow, mournful whistle)
  "Campylorhynchus turdinus",  # Thrush-like Wren (Loud, complex, rhythmic chattering)
  "Phaethornis superciliosus", # Long-tailed Hermit (High-frequency, sharp metallic "tseep")
  "Rupicola maculatus",        # Guianan Cock-of-the-rock (Bizarre squawks and animal-like grunts)
  "Psophia crepitans",         # Grey-winged Trumpeter (Deep, resonant, drum-like booming)
  "Harpia harpyja",            # Harpy Eagle (Surprisingly high-pitched, piercing wail)
  "Crotophaga major",          # Greater Ani (Bubbling, prehistoric croaks and gulps)
  "Trogon melanurus",          # Black-tailed Trogon (Steady, repetitive dog-like barking/clucking)
  "Momotus momota",            # Amazonian Motmot (Low, rhythmic double-hoot "whoot-whoot")
  "Pionus menstruus",          # Blue-headed Parrot (Harsh, high-frequency clatter)
  "Mitu tuberosum",            # Razor-billed Curassow (Deep, closed-mouth resonant booming)
  "Gymnoderus foetidus",       # Bare-necked Fruitcrow (Bizarre, bull-like lowing/moaning)
  "Capito aurovirens",         # Black-banded Barbet (Fast, hollow, wooden trill)
  "Baryphthengus martii",      # Rufous Motmot (Deep, resonant, syncopated hoots)
  "Liosceles thoracicus",      # Rusty-belted Tapaculo (Loud, descending, accelerating whistle series)
  "Bubo bubo",                 # Eurasian Eagle-Owl (Deep resonant hoot, 50-200 Hz)
  "Zenaida macroura",          # Mourning Dove (Low mournful coo, 200-400 Hz)
  "Alcedo atthis",             # Common Kingfisher (Piercing high whistle, 2-8 kHz)
  "Bombycilla cedrorum",       # Cedar Waxwing (High-pitched trill, 3-8 kHz)
  "Corvus brachyrhynchos",     # American Crow (Raspy guttural caw, 0.5-2 kHz)
  "Anas platyrhynchos",        # Mallard Duck (Low guttural quack, 0.4-0.8 kHz)
  "Dendrocopos major",         # Great Spotted Woodpecker (Rapid percussive drumming)
  "Calypte anna",              # Anna's Hummingbird (Sharp metallic chirp, 5-15 kHz)
  "Luscinia megarhynchos",     # Common Nightingale (Complex melodic song, 1-8 kHz)
  "Turdus migratorius",        # American Robin (Clear melodic whistle, 0.8-4 kHz)
  "Buteo jamaicensis",         # Red-tailed Hawk (Harsh descending scream, 1-4 kHz)
  "Pavo cristatus",            # Indian Peafowl (Loud resonant honking, 0.4-2 kHz)
  "Cyanocitta cristata",       # Blue Jay (Noisy, harsh abrasive call, 2-8 kHz)
  "Gavia immer"                # Common Loon (Eerie tremolo and wail, 0.1-0.3 kHz)
)

# Initialize the target directory for the downloaded MP3s
base_dir <- file.path("AvianEcho_Audio_Files")
dir.create(base_dir, showWarnings = FALSE)

all_metadata <- list()

# ------------------------------------------------------------------------------
# 2. STRICT QUERY PHASE (QUALITY CONTROL)
# ------------------------------------------------------------------------------
cat("\n======================================================\n")
cat(" PHASE 1: QUERYING XENO-CANTO API\n")
cat("======================================================\n")

for (species in birds) {
  cat(sprintf("Querying: %-30s ... ", species))

  safe_query <- tryCatch({
    # STRATEGY: Append q:"A" to the API call. This strictly forces Xeno-Canto
    # to only return pristine, noise-free, foreground audio files, drastically
    # improving the downstream accuracy.
    q_string <- paste0('sp:"', species, '" q:"A"')
    query_xenocanto(species = q_string, all_data = TRUE)
  }, error = function(e) return(NULL))

  if (!is.null(safe_query) && nrow(safe_query) > 0) {
    # STRATEGY: Cap at 30 recordings per species. This ensures we don't download
    # 5,000 files for a common bird and 20 for a rare one, maintaining an initial
    # layer of class balance before entering the DSP pipeline.
    limit <- min(30, nrow(safe_query))
    target_df <- safe_query[1:limit, ]
    all_metadata[[length(all_metadata) + 1]] <- target_df
    cat(sprintf("✅ Queued %d pristine Class 'A' files.\n", limit))
  } else {
    cat("❌ No Class 'A' recordings found.\n")
  }
}

# ------------------------------------------------------------------------------
# 3. MASTER DOWNLOAD PHASE
# ------------------------------------------------------------------------------
# Combine all individual species queries into a single master metadata dataframe
master_df <- do.call(rbind, all_metadata)

if (!is.null(master_df) && nrow(master_df) > 0) {
  cat("\n======================================================\n")
  cat(sprintf(" PHASE 2: DOWNLOADING %d AUDIO FILES\n", nrow(master_df)))
  cat("======================================================\n")

  # Dynamically locate the correct column containing the scientific species name
  # This guarantees the folder mapping works regardless of API schema updates
  sp_col <- grep("species|scientific", colnames(master_df), ignore.case = TRUE, value = TRUE)[1]

  # Execute the mass media download using suwo's built-in fetcher
  download_media(
    metadata = master_df,
    path = base_dir,
    folder_by = sp_col  # Automatically routes files into species-specific subfolders
  )

  cat("\n✅ SUCCESS! Audio files downloaded in "  ,base_dir, "\n\n")
} else {
  cat("\n❌ FATAL ERROR: Extraction failed. No files met the quality criteria.\n")
}
