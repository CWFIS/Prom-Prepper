# ============================================================
# Prometheus Prepper
# ------------------------------------------------------------
# Description: Shiny app designed to streamline data collection for 
  #            prometheus FGM. All comments and suggestions welcome!
# Author: Rachel Dietrich - rachel.dietrich@nrcan-rncan.gc.ca
# Organization: Canadian Forest Service
# Repository: https://github.com/CWFIS/Prom_prepper
# ============================================================


##DEV NOTES
##Bugs outstanding
# Consider warning flag for initial wcs chug
# Consider moving fuels + DEM to end

##Outstanding to add
  # wx stations and data 
  # add FBP/ DEM to map
  # Wind ninja
  # FWI calculator + graphing


### Start Here ###

##Write your SpotWx API Key to environment vars ONCE!
#Sys.setenv("SPOTWX_API_KEY"="[INSERT SPOTWX API KEY HERE]")

if (!requireNamespace("BurnP3.HelpR", quietly = TRUE)) {
  remotes::install_github("BadgerOnABike/BurnP3.HelpR")
}
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  shiny, sf, terra, dplyr, lubridate, httr, glue, lutz, ggplot2, leaflet, 
  cowplot, gridExtra, shinyFiles, fs,plotly, clock, readr, BurnP3.HelpR, DT)

# ---- Initiate shiny :) ----
runApp()
