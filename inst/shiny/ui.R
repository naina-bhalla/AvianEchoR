library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)
library(tuneR)
library(seewave)
library(fda)
library(AvianEchoR)

# ==============================================================================
# UI DEFINITION
# ==============================================================================

ui <- page_navbar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "black",            
    info = "#3498DB",              
    secondary = "#0006E0", 
    success = "#18BC55", 
    warning = "#F31250", 
    danger = "#E7E23C",
    base_font = font_google("Inter"),    
    heading_font = "Arial" 
  ),
  title = "Avian-Echo",
  
  # ========== TAB 1: LIVE CLASSIFICATION ==========
  nav_panel(
    "Live Identifier",
    page_sidebar(
      sidebar = sidebar(
        width = 350,
        h4("Upload Audio"),
        fileInput("audio_upload", "Select .WAV or .MP3 file", accept = c(".wav", ".mp3")),
        h4("Settings"),
        sliderInput("live_k", "k neighbors:", 1, 15, 3),
        checkboxInput("live_weighted", "Use weighted kNN", TRUE),
        actionButton("classify_btn", "Identify Bird", class = "btn-secondary w-100 btn-lg text-white"),
        div(
          class = "bg-white rounded p-3 text-center shadow-sm",
          h6("PREDICTION:", style = "color: black; letter-spacing: 2px;"),
          h3(textOutput("live_prediction"), style = "color: #27ae60; margin: 10px 0; font-weight: bold;"),
          h6(textOutput("confidence_score"), style = "color: black; margin: 0;")
        )
      ),
      
      navset_card_tab(
        title = "Fingerprint",
        nav_panel("Spectrogram",
                  plotOutput("audio_visuals", height = "600px")),
        nav_panel("Features",
                  tableOutput("extracted_features_table")),
        nav_panel("Inference Logs",
                  verbatimTextOutput("feature_diagnostics"))
      )
    )
  ),
  
  # ========== TAB 2: MODEL & FEATURE ANALYSIS (COMBINED) ==========
  nav_panel(
    "Model Command Center",
    page_sidebar(
      sidebar = sidebar(
        width = 350,
        
        # Model Controls
        h4("kNN Configuration", class = "text-primary mt-0"),
        helpText("Evaluates model on the pristine 20% holdout test set."),
        checkboxInput("use_weighted", "Use Distance-Weighted kNN", TRUE),
        actionButton("run_model", "Run Holdout Evaluation", class = "btn-secondary w-100 mb-4"),
        
        hr(),
        
        # Feature Controls
        h4("Feature Inspection", class = "text-primary"),
        sliderInput("n_features_show", "Top N Features:", 5, 20, 10),
        actionButton("analyze_features", "Analyze Features", class = "btn-secondary w-100")
      ),
      
      # Main Content Area
      div(
        class = "mx-auto",
        
        # Top Row: Value Boxes
        layout_columns(
          fill = FALSE,       
          gap = "1rem",       
          
          value_box(
            title = "Optimal 'k' Neighbors", 
            value = textOutput("vb_best_k"), 
            theme = "success", 
            showcase = bsicons::bs_icon("diagram-3"),
            max_height = "170px", 
            class = "p-2 text-center"
          ),
          
          value_box(
            title = "Holdout Accuracy", 
            value = textOutput("vb_cv_acc"), 
            theme = "warning", 
            showcase = bsicons::bs_icon("check-all"),
            max_height = "170px",
            class = "p-2 text-center"
          ),
          
          value_box(
            title = h6("Cohen's Kappa", class = "mb-0"), 
            value = textOutput("vb_kappa"), 
            theme = "info", 
            showcase = bsicons::bs_icon("graph-up"),
            max_height = "170px",
            class = "p-2 text-center"
          )
        ),
        
        # Bottom Row: Consolidated Analytics Tabset
        card(
          fill = TRUE,
          class = "border-0 shadow-none", 
          navset_card_underline(
            title = "Analytics",
            full_screen = TRUE,
            nav_panel("Confusion Matrix", plotOutput("confMatrixPlot", height = "600px")),
            nav_panel("Pearson Correlation", plotOutput("featureImportancePlot", height = "600px")),
            nav_panel("Metrics", verbatimTextOutput("advancedMetrics")),
            nav_panel("95% Confidence Interval", verbatimTextOutput("bootstrapMetrics")),
            nav_panel("Dataset Diagnostics", verbatimTextOutput("diagnostics_out"))
          )
        )
      )
    )
  ),
  
  # ========== TAB 3: THE ACOUSTIC SPACE ==========
  nav_panel(
    "Acoustic Space",
    page_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Dimensionality Reduction"),
        selectInput("dim_method", "Method:",
                    choices = c("Supervised UMAP (Label-Aware)" = "sumap",
                                "PCA (Linear)" = "pca",
                                "t-SNE (Local)" = "tsne",
                                "UMAP (Unsupervised)" = "umap"
                    )),
        checkboxInput("view_3d", "Enable 3D View", FALSE),
        hr(),
        actionButton("refresh_plot", "Re-project Space", class = "btn-secondary w-100")
      ),
      
      card(
        class = "shadow-sm",
        full_screen = TRUE,
        card_header(class = "bg-primary text-white", "Acoustic Manifold Projection"),
        uiOutput("plot_ui"),
        card_footer("Using AvianEchoR non-linear mapping engines. Supervised UMAP uses species labels to force cluster separation.")
      )
    )
  ),
  
  # ========== TAB 4: INFORMATION ==========
  nav_panel(
    "Information",
    fluidRow(
      column(
        width = 6,
        card(
          class = "shadow-sm",
          card_header(class = "bg-info text-white", "Scientific Dictionary"),
          div(style = "height: 600px; overflow-y: scroll;",
              tableOutput("bird_dictionary_table"))
        )
      ),
      
      column(
        width = 6,
        card(
          card_header(class = "bg-info text-white", "Acoustic Features Glossary"),
          div(style = "height: 600px; overflow-y: scroll; padding: 15px;",
              h5("Spectral Features (Frequency Domain)"),
              tags$ul(
                tags$li(strong("Pitch:"), " Mean fundamental frequency (Hz) - separates owl (50 Hz) from hummingbird (15 kHz)"),
                tags$li(strong("Centroid:"), " 'Center of mass' of the sound spectrum - key for frequency distribution"),
                tags$li(strong("Bandwidth:"), " Spread of frequencies - narrow for pure kingfisher whistle, wide for harsh crow"),
                tags$li(strong("Rolloff:"), " Frequency below which 85% of energy is captured - captures frequency balance"),
                tags$li(strong("SpecEntropy:"), " Disorder of sound (high = noise-like) - separates melodic nightingale from raspy crow")
              ),
              hr(),
              h5("Timbre & Quality (Mel-Frequency)"),
              tags$ul(
                tags$li(strong("MFCC 1-5:"), " Mel-Frequency Cepstral Coefficients - captures vocal tract shape, like acoustic fingerprint")
              ),
              hr(),
              h5("Temporal Features (Time Domain)"),
              tags$ul(
                tags$li(strong("ZCR:"), " Zero Crossing Rate - detects percussive woodpecker clicks vs smooth owl hoot"),
                tags$li(strong("HNR:"), " Harmonic-to-Noise Ratio - tonal (hummingbird metallic) vs harsh (crow, jay)"),
                tags$li(strong("TemporalCentroid:"), " Point where energy peaks - distinguishes loon tremolo from waxwing brief trill")
              )
          )
        )
      )
    )
  )
)
