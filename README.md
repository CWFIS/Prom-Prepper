Prometheus Prepper

A Shiny application for retrieving, displaying and analysing input data for prometheseus FGM collected for fire locations or user-defined points. 
This project is in beta, please check for updates often!
Questions, comments and feature suggestions to Rachel Dietrich [rachel.dietrich@nrcan-rncan.gc.ca]

  
## Features ##

1.Select fire location via map click, coordinates, or uploaded perimeter
2.Clips, reprojects and saves Digital Elevation Model either (MRDEM) and FBP fuels from the National Fuel Grid (WCS) or a locally saved FBP raster. 
    Fuels source: https://ostrnrcan-dostrncan.canada.ca/entities/publication/fb4eae39-28cb-4e1d-8f56-9879dd78b1f1?fromSearchPage=true
    DEM source: https://open.canada.ca/data/en/dataset/18752265-bda3-498c-a4ba-9dfe68cb98da
3. Retrieves, plots and saves: Fire history (from National Burned Area composite), recent M3 hotspots and M3 perimeters. 
    NBAC: https://cwfis.cfs.nrcan.gc.ca/en/catalogue/results/537a1fd0-698e-4a7b-85a1-e02581ae78b2
    Fire M3: https://cwfis.cfs.nrcan.gc.ca/en/catalogue/results/5b92c253-d40b-41e8-a98b-75c59d11da9e
4. Retrieves, plots and saves available forecasts for point location from SpotWx API

## Outputs ##
- TIFF of FBP and DEM rasters
- Shapefiles of NBAC, M3 Hotspots and M3 Perimeters 
- CSV exports of SpotWX forecasts in Prometheus ready format
- TIFF plots comparing forecast models
- Metadata summaries for spot forecasts and Hotspot data

## Requirements ##

This app requires a SpotWX API key.

Line 26 can be used to write your SpotWx API key into your environment variables. 
Sys.setenv("SPOTWX_API_KEY"="[INSERT SPOTWX API KEY HERE]")

Altnernatively, it can be added directly in the input app. 

All R package requirements are included in the setup code. 

## Run instructions ##

1. Clone repository: https://github.com/CWFIS/Prom-Prepper
2. Open Prom Prepper.Rproj
3. Open Prepper_shiny.R
4. Uncomment #Sys.setenv("SPOTWX_API_KEY"="[INSERT SPOTWX API KEY HERE]"), add API key and run [OPTIONAL]
5. Run Lines 29:38 to set up app. 
6. Run Line 41 to initiate app. 
7. Usage instructions contained within application.
