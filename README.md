# Prometheus Prepper #

A Shiny application for retrieving, displaying and analyzing input data for prometheseus FGM for fire locations or user-defined points. 

This project is in beta, please check for updates often!

Questions, comments and feature suggestions to Rachel Dietrich 
- GitHub: https://github.com/Rachel-Dietrich  
- Email: rachel.dietrich [at] nrcan-rncan [dot] gc [dot] ca

## Features ##

1. Select fire location via map click, coordinates, or uploaded perimeter

2. Clips, reprojects and saves Digital Elevation Model (MRDEM) and FBP fuels from either the National Fuel Grid or a locally saved FBP raster. 
    
   - Fuels source: https://ostrnrcan-dostrncan.canada.ca/entities/publication/fb4eae39-28cb-4e1d-8f56-9879dd78b1f1?fromSearchPage=true
    
   - DEM source: https://open.canada.ca/data/en/dataset/18752265-bda3-498c-a4ba-9dfe68cb98da
    
3. Retrieves, plots and saves: Fire history (from National Burned Area composite), recent M3 hotspots and M3 perimeters. 

   - NBAC: https://cwfis.cfs.nrcan.gc.ca/en/catalogue/results/537a1fd0-698e-4a7b-85a1-e02581ae78b2

   - Fire M3: https://cwfis.cfs.nrcan.gc.ca/en/catalogue/results/5b92c253-d40b-41e8-a98b-75c59d11da9e

5. Retrieves, plots and saves available forecasts for point location from SpotWx API.

## Outputs ##
- TIFF of FBP and DEM rasters
- Shapefiles of NBAC, M3 Hotspots and M3 Perimeters 
- CSV exports of SpotWX forecasts in Prometheus ready format
- TIFF plots comparing forecast models
- Metadata summaries for spot forecasts and Hotspot data

## Requirements ##

This app requires a SpotWx API key.

Line 26 can be used to write your SpotWx API key into your environment variables. 

    Sys.setenv("SPOTWX_API_KEY"="[INSERT SPOTWX API KEY HERE]")

Altnernatively, it can be added directly in the app. 

All R package requirements are included in the setup code. 

## Run instructions ##

1. Clone the repository:

   ```bash
   git clone https://github.com/CWFIS/Prom-Prepper
   ```

2. Open the project file: `Prom Prepper.Rproj`

3. Open the main script:`Prepper_shiny.R`

4. (Optional) Add your API key. 

   In `Prepper_shiny.R`, uncomment and edit line 27:

   ```r
   Sys.setenv("SPOTWX_API_KEY" = "[INSERT SPOTWX API KEY HERE]")
   ```

5. Run the setup section: lines 29–38 in `Prepper_shiny.R`

6. Launch the application: line 41 in `Prepper_shiny.R`
   ```r
   shinyApp(ui, server)
   ```

8. Usage: App is self explanatory (I hope!)
