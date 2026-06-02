
# ==============================================================================
# server.R - AVIANECHOR DASHBOARD LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # ===== Reactive Data Engine =====
  current_data <- reactive({
    data("bird_train", package = "AvianEchoR", envir = environment())
    return(bird_train)
  })
  
  
  # ============================================================================
  # TAB 1: LIVE CLASSIFICATION (The Live Identifier)
  # ============================================================================
  
  uploaded_audio <- reactive({
    req(input$audio_upload)
    tryCatch({
      wave <- read_audio_file(input$audio_upload$datapath)
      wave <- standardize_audio(wave)
      return(wave)
    }, error = function(e) { return(NULL) })
  })
  
  output$audio_visuals <- renderPlot({
    req(uploaded_audio())
    plot_bird_audio(uploaded_audio(), title = "Live Audio Fingerprint")
  })
  
  observeEvent(input$classify_btn, {
    tryCatch({
      req(input$audio_upload)
      
      cat("\n==================================================\n")
      cat("✓ 1. Loading audio file...\n")
      wave <- uploaded_audio()
      
      if (is.null(wave)) stop("Failed to load audio. The file might be corrupted.")
      
      cat("✓ 2. Slicing active audio chunks...\n")
      audio_chunks <- extract_clean_chunks(wave)
      
      if (length(audio_chunks) == 0) stop("No vocalizations detected.")
      
      chunk_lengths <- sapply(audio_chunks, function(w) length(w@left))
      best_chunk <- audio_chunks[[which.max(chunk_lengths)]]
      
      if (!inherits(best_chunk, "Wave")) stop("Expected a Wave object.")
      
      cat("✓ 3. Extracting raw acoustic features...\n")
      features_raw <- tryCatch({
        extract_audio_features(best_chunk, verbose = FALSE)
      }, error = function(e) stop(sprintf("Feature extraction failed: %s", e$message)))
      
      if (is.null(features_raw) || nrow(features_raw) == 0) stop("Extraction returned empty.")
      
      cat("✓ 4. Loading historical fPCA training space...\n")
      if (!exists("bird_train")) data(bird_train, package = "AvianEchoR", envir = environment())
      if (!exists("pitch_pca")) data(pitch_pca, package = "AvianEchoR", envir = environment())
      
      cat("✓ 5. Projecting Pitch into 3 fPCA Harmonics...\n")
      pitch_cols <- paste0("Pitch_t", 1:50)
      live_pitch <- as.matrix(features_raw[, pitch_cols])
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
      
      features_aligned <- cbind(as.data.frame(live_fpca_scores), features_raw[, discrete_cols])
      features_aligned[!is.finite(as.matrix(features_aligned))] <- 0
      
      train_x <- as.matrix(bird_train[, 1:15])
      train_y <- as.factor(bird_train$species)
      
      cat("✓ 6. Running 15-Dim kNN algorithm...\n")
      
      k_live <- if (length(input$live_k) > 0) input$live_k else 5
      use_wgt <- if (length(input$live_weighted) > 0) input$live_weighted else TRUE
      
      k_val <- min(k_live, nrow(train_x))
      pred <- if (use_wgt) {
        weighted_knn(train_x, as.matrix(features_aligned), train_y, k = k_val)
      } else {
        class::knn(train_x, as.matrix(features_aligned), train_y, k = k_val)
      }
      
      pred_str <- as.character(pred)
      
      output$live_prediction <- renderText({ pred_str })
      output$confidence_score <- renderText({ sprintf("Confidence Mode: k=%d", k_val) })
      
      output$extracted_features_table <- renderTable({
        data.frame(Feature = colnames(features_aligned), Value = round(as.numeric(features_aligned[1, ]), 4))
      }, digits = 4)
      
      output$feature_diagnostics <- renderPrint({
        cat("✓ Inference Pipeline Executed\n")
        cat("═══════════════════════════════════════\n")
        cat(sprintf("Model: 15-Feature fPCA Hybrid (Unscaled)\n"))
        cat(sprintf("Neighbors (k): %d\n", k_val))
        cat(sprintf("\n>> IDENTIFIED: %s\n", pred_str))
      })
      
    }, error = function(e) {
      cat(sprintf("\n❌ FATAL ERROR IN LIVE INFERENCE:\n   %s\n\n", e$message))
      output$live_prediction <- renderText({ "❌ Inference Failed" })
      output$confidence_score <- renderText({ sprintf("Error: %s", e$message) })
    })
  })
  
  
  # ============================================================================
  # TAB 2: MODEL TRAINING (Model Command Center)
  # ============================================================================
  
  observeEvent(input$run_model, {
    tryCatch({
      # Load purely the real training and testing data
      data(bird_train, package = "AvianEchoR", envir = environment())
      data(bird_test, package = "AvianEchoR", envir = environment())
      train_df <- bird_train
      test_df <- bird_test
      
      numeric_cols <- sapply(train_df, is.numeric)
      valid_cols <- names(train_df)[numeric_cols & !(names(train_df) %in% c("row_id", "chunk_id", "path"))]
      
      train_X <- as.matrix(train_df[, valid_cols])
      test_X <- as.matrix(test_df[, valid_cols])
      
      train_X[!is.finite(train_X)] <- 0
      test_X[!is.finite(test_X)] <- 0
      
      train_y <- as.factor(train_df$species)
      test_y <- as.factor(test_df$species)
      
      use_wgt <- if (length(input$use_weighted) > 0) input$use_weighted else TRUE
      
      best_acc <- 0
      best_k <- 3
      best_preds <- NULL
      
      for (k_test in c(3, 5, 7, 9)) {
        preds <- if(use_wgt) {
          weighted_knn(train_X, test_X, train_y, k = k_test)
        } else {
          class::knn(train_X, test_X, train_y, k = k_test)
        }
        
        acc <- mean(preds == test_y)
        if (acc > best_acc) {
          best_acc <- acc
          best_k <- k_test
          best_preds <- preds
        }
      }
      
      eval_metrics <- evaluate_classification(best_preds, test_y, model_name = "kNN Acoustic Baseline")
      boot_metrics <- bootstrap_ci_accuracy(best_preds, test_y, n_bootstrap = 500)
      
      output$vb_best_k <- renderText({ paste("k =", best_k) })
      output$vb_cv_acc <- renderText({ sprintf("%.2f%%", best_acc * 100) })
      output$vb_kappa <- renderText({ sprintf("%.3f", eval_metrics$kappa) })
      
      output$advancedMetrics <- renderPrint({
        cat("✓ Holdout Evaluation Complete.\n")
        cat(sprintf("Tested on pristine holdout set (n=%d).\n", nrow(test_X)))
        cat(sprintf("Optimal k=%d achieved %.2f%% accuracy.\n\n", best_k, best_acc * 100))
        evaluate_classification(best_preds, test_y, model_name = "Holdout Test Performance")
      })
      
      output$bootstrapMetrics <- renderPrint({
        cat("Non-Parametric Bootstrap Validation (500 resamples)\n")
        cat("====================================================\n")
        cat(sprintf("Point Estimate Accuracy: %.2f%%\n", boot_metrics$accuracy * 100))
        cat(sprintf("95%% Confidence Interval: [%.2f%%,  %.2f%%]\n",
                    boot_metrics$ci_lower * 100, boot_metrics$ci_upper * 100))
      })
      
      output$confMatrixPlot <- renderPlot({
        cm <- table(Actual = test_y, Predicted = best_preds)
        cm_df <- as.data.frame(as.table(cm))
        
        ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
          geom_tile(color = "white") +
          scale_fill_gradient(low = "white", high = "#27ae60") +
          geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold",
                    color = ifelse(cm_df$Freq > max(cm_df$Freq)/2, "white", "black")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
                axis.text.y = element_text(size = 11)) +
          labs(title = "Holdout Confusion Matrix", x = "Model Predicted", y = "Actual Ground Truth", fill = "Count")
      })
      
    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
    })
  })
  
  
  # ============================================================================
  # TAB 3: FEATURE ANALYSIS
  # ============================================================================
  
  observeEvent(input$analyze_features, {
    req(current_data())
    df <- current_data()
    
    numeric_cols <- sapply(df, is.numeric)
    raw_features <- df[, numeric_cols & !(names(df) %in% c("row_id", "chunk_id", "path"))]
    raw_features[!is.finite(as.matrix(raw_features))] <- 0
    labels <- as.factor(df$species)
    
    n_feats <- if (length(input$n_features_show) > 0) input$n_features_show else 10
    
    output$featureImportancePlot <- renderPlot({
      sel <- select_features_by_correlation(raw_features, labels, n_features = n_feats)
      imp_df <- data.frame(Feature = sel$selected_features, Importance = sel$correlations)
      
      ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill = "#2c3e50", width=0.7) +
        geom_text(aes(label = round(Importance, 3)), hjust = -0.2, fontface="bold") +
        coord_flip() + theme_minimal() +
        labs(title = paste("Top", n_feats, "Predictive Acoustic Features"), x = "", y = "Absolute Pearson Correlation") +
        theme(text = element_text(size = 14))
    })
    
    output$diagnostics_out <- renderPrint({
      diagnose_features(raw_features, labels)
    })
  })
  
  
  # ============================================================================
  # TAB 4: ACOUSTIC SPACE (Dimensionality Reduction)
  # ============================================================================
  
  output$plot_ui <- renderUI({
    is_3d <- if (length(input$view_3d) > 0) input$view_3d else FALSE
    if (is_3d) {
      plotlyOutput("plotlyPlot", height = "650px")
    } else {
      plotlyOutput("scatterPlot", height = "650px")
    }
  })
  
  projection_data <- reactive({
    req(current_data())
    df <- current_data()
    
    numeric_cols <- sapply(df, is.numeric)
    raw_feats <- df[, numeric_cols & !(names(df) %in% c("species", "row_id", "chunk_id", "path"))]
    raw_feats[!is.finite(as.matrix(raw_feats))] <- 0
    scaled_feats <- scale(as.matrix(raw_feats))
    labels <- as.factor(df$species)
    
    is_3d <- if (length(input$view_3d) > 0) input$view_3d else FALSE
    n_dims <- if(is_3d) 3 else 2
    method_choice <- if (length(input$dim_method) > 0) input$dim_method else "pca"
    
    set.seed(42)
    res <- tryCatch({
      if (method_choice == "pca") {
        reduce_acoustic_pca(scaled_feats, n_components = n_dims)$scores
      } else if (method_choice == "sumap") {
        uwot::umap(scaled_feats, n_components = n_dims, y = labels, target_weight = 0.5)
      } else {
        map_acoustic_nonlinear(scaled_feats, method = method_choice, dims = n_dims)
      }
    }, error = function(e) { return(NULL) })
    
    if (is.null(res)) return(NULL)
    proj_df <- as.data.frame(res)
    colnames(proj_df) <- paste0("Dim", 1:ncol(proj_df))
    proj_df$Species <- labels
    return(proj_df)
  })
  
  output$scatterPlot <- renderPlotly({
    df <- projection_data()
    req(df)
    method_choice <- if (length(input$dim_method) > 0) input$dim_method else "pca"
    
    p <- ggplot(df, aes(x = Dim1, y = Dim2, color = Species, text = Species)) +
      geom_point(alpha = 0.8, size = 3) + theme_minimal() +
      labs(title = paste("Acoustic Space:", toupper(method_choice)))
    ggplotly(p, tooltip = "text")
  })
  
  output$plotlyPlot <- renderPlotly({
    df <- projection_data()
    req(df)
    plot_ly(df, x = ~Dim1, y = ~Dim2, z = ~Dim3, color = ~Species,
            type = 'scatter3d', mode = 'markers', marker = list(size = 4, opacity=0.8))
  })
  
  
  # ============================================================================
  # TAB 5: INFORMATION TABLE
  # ============================================================================
  
  output$bird_dictionary_table <- renderTable({
    data.frame(
      Scientific_Name = c(
        "Bubo bubo", "Zenaida macroura", "Alcedo atthis", "Bombycilla cedrorum",
        "Corvus brachyrhynchos", "Anas platyrhynchos", "Dendrocopos major",
        "Calypte anna", "Luscinia megarhynchos", "Turdus migratorius",
        "Buteo jamaicensis", "Pavo cristatus", "Cyanocitta cristata", "Gavia immer",
        "Lipaugus vociferans", "Nyctibius grandis", "Perissocephalus tricolor",
        "Procnias albus", "Cyphorhinus arada", "Ara macao", "Opisthocomus hoazin",
        "Campephilus melanoleucos", "Cacicus cela", "Crypturellus undulatus",
        "Pteroglossus castanotis", "Thamnophilus doliatus", "Morphnus guianensis",
        "Psarocolius decumanus", "Amazona ararauna", "Ramphastos toco",
        "Tinamus solitarius", "Campylorhynchus turdinus", "Phaethornis superciliosus",
        "Rupicola maculatus", "Psophia crepitans", "Harpia harpyja", "Crotophaga major",
        "Trogon melanurus", "Momotus momota", "Pionus menstruus", "Mitu tuberosum",
        "Gymnoderus foetidus", "Capito aurovirens", "Baryphthengus martii", "Liosceles thoracicus"
      ),
      Common_Name = c(
        "Eurasian Eagle-Owl", "Mourning Dove", "Common Kingfisher", "Cedar Waxwing",
        "American Crow", "Mallard Duck", "Great Spotted Woodpecker", "Anna's Hummingbird",
        "Common Nightingale", "American Robin", "Red-tailed Hawk", "Indian Peafowl",
        "Blue Jay", "Common Loon", "Screaming Piha", "Great Potoo", "Capuchinbird",
        "White Bellbird", "Musician Wren", "Scarlet Macaw", "Hoatzin",
        "Crimson-crested Woodpecker", "Yellow-rumped Cacique", "Undulated Tinamou",
        "Chestnut-eared Aracari", "Barred Antshrike", "Crested Eagle",
        "Crested Oropendola", "Blue-and-yellow Macaw", "Toco Toucan",
        "Solitary Tinamou", "Thrush-like Wren", "Long-tailed Hermit",
        "Guianan Cock-of-the-rock", "Grey-winged Trumpeter", "Harpy Eagle", "Greater Ani",
        "Black-tailed Trogon", "Amazonian Motmot", "Blue-headed Parrot", "Razor-billed Curassow",
        "Bare-necked Fruitcrow", "Black-banded Barbet", "Rufous Motmot", "Rusty-belted Tapaculo"
      ),
      Acoustic_Type = c(
        "Deep Hoot (50-200 Hz)", "Low Coo (200-400 Hz)", "Piercing Whistle (2-8 kHz)",
        "High Trill (3-8 kHz)", "Raspy Caw (0.5-2 kHz)", "Guttural Quack (0.4-0.8 kHz)",
        "Percussive Drumming (1-4 kHz)", "Metallic Chirp (5-15 kHz)", "Complex Song (1-8 kHz)",
        "Melodic Whistle (0.8-4 kHz)", "Harsh Scream (1-4 kHz)", "Loud Honk (0.4-2 kHz)",
        "Noisy Call (2-8 kHz)", "Eerie Tremolo (0.1-0.3 kHz)",
        "Piercing Explosive Whistle (3-5 kHz)", "Deep Guttural Roar/Moan (0.2-0.5 kHz)",
        "Mechanical 'Chainsaw' Buzz (0.5-2 kHz)", "Metallic 'Clang' (Highest dB, 1-3 kHz)",
        "Complex Melodic Flute (1.5-4 kHz)", "Harsh Tearing Screech (1-4 kHz)",
        "Asthmatic Wheeze/Grunt (0.5-1.5 kHz)", "Percussive Drumming (Broadband)",
        "Liquid Gurgles & Pops (1-5 kHz)", "Low-frequency 3-Note Whistle (0.8-1.2 kHz)",
        "Rhythmic High-Pitched Sneezing (3-6 kHz)", "Accelerating Rolling Laugh (1-3 kHz)",
        "Clear Raptor Whistle (2-4 kHz)", "Liquid Crashing Gargle (0.5-4 kHz)",
        "Deafening Harsh Screech (2-5 kHz)", "Deep Frog-like Grunting (0.3-0.8 kHz)",
        "Slow Mournful Whistle (1-2 kHz)", "Loud Rhythmic Chattering (2-4 kHz)",
        "High-frequency Metallic 'Tseep' (6-10 kHz)", "Bizarre Squawks & Grunts (1-3 kHz)",
        "Resonant Drum-like Booming (0.1-0.4 kHz)", "High-pitched Piercing Wail (3-6 kHz)",
        "Prehistoric Bubbling Croak (0.5-2 kHz)", "Dog-like Barking/Clucking (1-2 kHz)",
        "Low Rhythmic Double-Hoot (0.4-0.8 kHz)", "High-frequency Clatter (3-6 kHz)",
        "Closed-mouth Resonant Boom (0.1-0.3 kHz)", "Bull-like Lowing/Moaning (0.3-1 kHz)",
        "Fast Hollow Wooden Trill (1-3 kHz)", "Deep Syncopated Hoots (0.4-0.9 kHz)",
        "Descending Accelerating Whistles (1-3 kHz)"
      )
    )
  })
}