# ==============================================================================
# app.R - AVIAN-ECHO DASHBOARD LAUNCHER
# ==============================================================================

# 1. Load all required libraries for both UI and Server
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)
library(tuneR)
library(seewave)
library(fda)
library(AvianEchoR)
library(bsicons)


# 2. Source the UI and Server files
# Using 'local = TRUE' ensures the variables load safely into this app's environment
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# 3. Launch the Application
shinyApp(ui = ui, server = server)
