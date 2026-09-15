# ---- UI ----
ui <- fluidPage(
  titlePanel("Prometheus data prepper"),
  
  sidebarLayout(
    sidebarPanel(
      
      ## Fire Location ===========================
      
      h4("Fire Location"),
      radioButtons("loc_type", "Input method:",
                   choices = c("Map Click", "Lat/Long", "Upload Perim (kml)", "Upload Perim (.zip)")),
      
      conditionalPanel("input.loc_type == 'Lat/Long'",
                       numericInput("lat", "Latitude", ""),
                       numericInput("lon", "Longitude", "")
      ),
      
      conditionalPanel("input.loc_type == 'Upload Perim (kml)'",
                       fileInput("perim_upload", "Upload Perimeter", accept = ".kml")
      ),
      conditionalPanel("input.loc_type == 'Upload Perim (.zip)'",
                       fileInput("perim_upload", "Upload Perimeter", multiple=T)
      ),
      
      hr(),
      h4("Output folder"),
      
      shinyDirButton("outdir", "Select Output Folder", "Browse"),
      verbatimTextOutput("outdir_text"),
      
      textInput("fire_name", "Fire name:", value = ""),
      hr(),
      
      
      ## Fuel and DEM ===========================
      h4("Fuels & DEM"),
      radioButtons("fuel_source", "Source:",
                   choices = c("CWFIS National Grid (2024)", "Local File")),
      
      conditionalPanel("input.fuel_source == 'Local File'",
                       shinyFilesButton(
                         "fuel_local",
                         "Select raster (.tif)",
                         "Browse",
                         multiple = FALSE
                       ),
                       verbatimTextOutput("fuel_path_text")
      ),
      actionButton("clip", "Clip and Save"),
      hr(),
      
      h4("Data Layers"),
      checkboxInput("save_nbac", "Fire History (NBAC)", TRUE),
      conditionalPanel(
        condition = "input.save_nbac == true",
        
        sliderInput(
          "year_range",
          "Select Year Range:",
          min = 1972,
          max = as.numeric(format(Sys.Date(), "%Y")),
          value = c(
            as.numeric(format(Sys.Date(), "%Y")) - 10,
            as.numeric(format(Sys.Date(), "%Y"))
          ),
          sep = ""
        )
      ),
      #checkboxInput("save_wx", "Weather Stations", TRUE),
      checkboxInput("save_perim", "M3 Perimeters", TRUE),
      conditionalPanel(
        condition = "input.save_perim == true",
        sliderInput(
          "perim_radius",
          "Perimeter Radius (km):",
          min = 1,
          max = 50,
          value = 10,
          step = 1
        )
      ),
      checkboxInput("save_hs", "M3 Hotspots", TRUE),
      conditionalPanel(
        condition = "input.save_hs == true",
        sliderInput(
          "hs_radius",
          "Hotspot Radius (km):",
          min = 1,
          max = 50,
          value = 10,
          step = 1
        )
      ),
      checkboxInput("save_wx", "Weather Stations", TRUE),
      conditionalPanel(
        condition = "input.save_wx == true",
        sliderInput(
          "wx_date",
          "Weather Date",
          min = Sys.Date()-7,
          max = Sys.Date()-1,
          value = Sys.Date()-1,
          step = 1
        )
      ),
      actionButton("save_wcs", "Save selected"),
      hr(),
      ## Spot WX ===========================
      
      h4("SpotWx"),
      textInput("api", "API Key", value=Sys.getenv("SPOTWX_API_KEY")),
      checkboxGroupInput("models", "Weather Models", choices = NULL),
      actionButton("retrieve_models", "Retrieve models"),
      
      uiOutput("model_ui"),
      checkboxGroupInput("var", "Variables",
                         choices = c("TMP","RH","WSPD","WDIR","PRECIP_int","PRECIP_ttl",
                                     "CLOUD","GUST","DP", "1000_500MB_THICKNESS"),
                         selected = c("TMP","RH","WSPD","WDIR","PRECIP_ttl")),
      sliderInput("ndays", "Days", 1, 7, 3),
      actionButton("plot_models", "Plot models"),
      actionButton("save_plot", "Save plot"),
      actionButton("save_spotwx", "Save selected models"),
      
      hr(),
      
      ## FWI Values ===========================
      
      h4("User Starting Codes"),
      numericInput("user_ffmc",label = "FFMC:", value = ""),
      numericInput("user_dmc",label = "DMC:", value = ""),
      numericInput("user_dc",label = "DC:", value = ""),
      hr(),
      
      h4("Starting Code Use"),
      radioButtons("index_source", "Source:",
                   choices = c("User Defined", "Nearby Weather Station")),
      
      h4("Calculate FWI"),
      actionButton("calc_fwi","Calculate FWI"),
      checkboxGroupInput("fwi_var", "FWI Variables",
                         choices = c("FFMC","DMC","DC","ISI","BUI","FWI"),
                         selected = c("BUI","FWI")),
      actionButton("plot_fwi", "Plot FWI"),
      actionButton("save_fwi_plot", "Save FWI plot"),
      actionButton("save_fwi", "Save FWI Calculation")
    ),
    
    mainPanel(
      leafletOutput("map", height = 400),
      h2("Starting Codes from Nearby Stations"),
      DTOutput(outputId = "starting_codes"),
      h2("Spot Wx Plots"),
      plotlyOutput("spot_plot", height = 600),
      h2("FWI Plots"),
      plotlyOutput("fwi_plot", height = 600)
    )
  )
)


# ---- SERVER ----
server <- function(input, output, session){
  
  # ---- Setup ----
  Sys.setenv(GDAL_DATA = "", PROJ_LIB = "")
  spotwx_results <- reactiveVal(NULL) 
  spotwx_df <- reactiveVal(NULL)
  spot_plot_obj <- reactiveVal(NULL)
  fwi_plot_obj <- reactiveVal(NULL)
  nbac_raw <- reactiveVal(NULL)
  hotspots_raw <- reactiveVal(NULL)
  perim_raw <- reactiveVal(NULL)
  point_event <- reactiveVal(NULL)
  wx_raw <- reactiveVal(NULL)
  cffdrs_list <- reactiveVal(NULL)
  cffdrs_df <- reactiveVal(NULL)
  
  output$spot_plot <- renderPlotly({
    req(spot_plot_obj())
    spot_plot_obj()
  })
  output$fwi_plot <- renderPlotly({
    req(fwi_plot_obj())
    fwi_plot_obj()
  })
  output_dir <- reactive({
    req(input$outdir, nzchar(input$fire_name))
    
    base <- parseDirPath(volumes, input$outdir)
    
    req(length(base) > 0)
    
    file.path(base, input$fire_name)
  })
  ensure_output_dir <- function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  volumes <- c(Home = fs::path_home())
  shinyFiles::shinyFileChoose(
    input,
    "fuel_local",
    roots = volumes,
    session = session
  )
  fuel_path <- reactive({
    req(input$fuel_local)
    
    parseFilePaths(volumes, input$fuel_local)$datapath
  })
  
  # ---- Folder selection ----
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home())
  shinyDirChoose(input, "outdir", roots = volumes)
  outdir <- reactive({
    req(input$outdir)
    parseDirPath(volumes, input$outdir)
  })
  output$outdir_text <- renderText({
    req(outdir())
    paste("Selected:", outdir())
  })
  
  # MAP PIPELINE===============================================
 
  # ---- POINT STATE ----
  observeEvent(get_point(), {
    
    pt <- get_point()
    if (is.null(pt)) return()
    
    point_event(pt)
  })
  
  # ---- BBOX FUNCTION ----
  make_bbox <- function(radius_km) {
    
    req(point_event())
    pt <- point_event()
    
    pt_3978 <- sf::st_transform(pt, 3978)
    
    buffer <- sf::st_buffer(pt_3978, radius_km * 1000)
    
    bb <- sf::st_bbox(buffer)
    
    list(
      bbox = bb,
      bbox_str = paste(bb, collapse = ","),
      buffer = buffer
    )
  }
  
  # ---- INITIAL MAP ----
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(
        lng = -96,
        lat = 62,
        zoom = 2.5
      )
  })
  
  # ---- MAP UPDATE (POINT ONLY) ----
  observeEvent(point_event(), {
    
    req(input$perim_radius)
    
    pt <- point_event()
    bb <- make_bbox(input$perim_radius)$bbox
    
    coords <- sf::st_coordinates(pt)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      fitBounds(
        lng1 = bb["xmin"],
        lat1 = bb["ymin"],
        lng2 = bb["xmax"],
        lat2 = bb["ymax"]
      ) %>%
      addCircleMarkers(
        lng = coords[1],
        lat = coords[2],
        radius = 6,
        color = "red",
        fillOpacity = 1,
        group = "Point"
      )
  })
  
  # NBAC ===================================================

  ###Downloading###
  observeEvent(point_event(), {
    
    req(point_event())
    
    bb <- make_bbox(50)$bbox_str   
    
    nbac <- sf::st_read(
      paste0(
        "http://cwfis.cfs.nrcan.gc.ca/geoserver/public/wfs?",
        "service=WFS",
        "&request=GetFeature",
        "&typeName=public:nbac",
        "&outputFormat=application/json",
        "&BBOX=", bb
      ),
      quiet = TRUE
    )
    
    nbac$year <- suppressWarnings(as.numeric(as.character(nbac$year)))
    
    nbac_raw(nbac)
  })
  ###Filtering### 
  nbac_filtered <- reactive({
    
    req(nbac_raw(), input$year_range)
    
    dplyr::filter(
      nbac_raw(),
      year >= input$year_range[1],
      year <= input$year_range[2]
    ) |>
      sf::st_transform(4326)
  })
  ###Rendering###
  observeEvent(list(nbac_filtered(), input$save_nbac), {
    
    # ---- toggle off ----
    if (!isTRUE(input$save_nbac)) {
      
      leafletProxy("map") %>%
        clearGroup("NBAC") %>%
        removeControl("nbac_legend")
      
      return()
    }
    
    nbac_plot <- nbac_filtered()
    
    pal <- colorNumeric(
      palette = c("#fde0ef", "#f768a1", "#ae017e"),
      domain = nbac_plot$year
    )
    
    leafletProxy("map") %>%
      clearGroup("NBAC") %>%
      removeControl("nbac_legend") %>%
      
      addPolygons(
        data = nbac_plot,
        color = ~pal(year),
        fillColor = ~pal(year),
        weight = 1,
        fillOpacity = 0.5,
        group = "NBAC",
        popup = ~paste("Year:", year)
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = nbac_plot$year,
        title = "NBAC Year",
        layerId = "nbac_legend"
      )
  })
  
  # HOTSPOTS =================================================

      ## DOWNLOAD ONLY (cache) ---------
  observeEvent(point_event(), {
    
    req(point_event())
    
    bb <- make_bbox(50)$bbox_str
    
    hs <- sf::st_read(
      paste0(
        "http://cwfis.cfs.nrcan.gc.ca/geoserver/public/wfs?",
        "service=WFS",
        "&request=GetFeature",
        "&typeName=public:hotspots_last24hrs",
        "&outputFormat=application/json",
        "&BBOX=", bb
      ),
      quiet = TRUE
    )
    
    hotspots_raw(hs)
  })
  
    ## ---- FILTER + RENDER  ----
  hotspots_filtered <- reactive({
    
    req(hotspots_raw(), point_event(), input$hs_radius)
    
    hs <- hotspots_raw() |>
      sf::st_transform(3978)
    
    centre <- sf::st_transform(point_event(), 3978)
    
    d <- sf::st_distance(hs, centre)[, 1]
    
    d<-as.numeric(d)
    
    hs[d <= input$hs_radius * 1000, ]
  })
  observeEvent(
    list(hotspots_filtered(), input$save_hs),
    {
      
      if (!isTRUE(input$save_hs)) {
        leafletProxy("map") %>%
          clearGroup("Hotspots")
        return()
      }
      
      leafletProxy("map") %>%
        clearGroup("Hotspots") %>%
        addCircleMarkers(
          data = sf::st_transform(hotspots_filtered(), 4326),
          radius = 4,
          stroke = FALSE,
          fillOpacity = 0.8,
          color = "darkred",
          group = "Hotspots"
        )
    }
  )
  
  ## M3 Perimeters ==========================================

  
  ### ---- DOWNLOAD ONLY (cache) ----
  observeEvent(point_event(), {
    
    req(point_event())
    
    bb <- make_bbox(50)$bbox_str
    
    perim <- sf::st_read(
      paste0(
        "http://cwfis.cfs.nrcan.gc.ca/geoserver/public/wfs?",
        "service=WFS",
        "&request=GetFeature",
        "&typeName=public:m3_polygons_current",
        "&outputFormat=application/json",
        "&BBOX=", bb
      ),
      quiet = TRUE
    )
    
    perim_raw(perim)
  })
  
  ## ---- FILTER + RENDER ONLY ----
  perim_filtered <- reactive({
    
    req(perim_raw(), point_event(), input$perim_radius)
    
    perim <- perim_raw() |>
      sf::st_transform(3978)
    
    centre <- sf::st_transform(point_event(), 3978)
    
    d <- sf::st_distance(sf::st_centroid(perim), centre)[, 1]
    
    d<-as.numeric(d)
    
    perim[d <= input$perim_radius * 1000, ]
  })
  
  # GET WEATHER =================================================
      ## ---- DOWNLOAD ONLY (cache) ----
  
  observeEvent(point_event(), {
    
    req(point_event())
    
    bb <- make_bbox(50)$bbox_str
    
    wx <- sf::st_read(dsn = 
                        paste0(
                          "http://cwfis.cfs.nrcan.gc.ca/geoserver/public/wfs?",
                          "service=WFS",
                          "&version=2.0.1",
                          "&request=GetFeature",
                          "&typeName=public:firewx_stns",
                          "&outputFormat=application/json",
                          "&BBOX=",bb
                        ),
                      quiet = TRUE
    )
    
    wx_raw(wx)
  })
  
  ## ---- FILTER + RENDER  ----
  wx_filtered <- reactive({
    
    req(wx_raw(), point_event(), input$wx_date)
  
    wx <- wx_raw() |>
      sf::st_transform(3978)
    
    wx <- wx[which(format(wx$rep_date,"%Y-%m-%d") == input$wx_date),]
    
  })
  
  observeEvent(
    list(wx_filtered(), input$save_wx),
    {
      
      if (!isTRUE(input$save_wx)) {
        leafletProxy("map") %>%
          clearGroup("Stations")
        return()
      }
      
      leafletProxy("map") %>%
        clearGroup("Stations") %>%
        addCircleMarkers(
          data = sf::st_transform(wx_filtered(), 4326),
          radius = 4,
          stroke = FALSE,
          fillOpacity = 0.8,
          color = "darkblue",
          group = "Stations"
        )
    }
  )
  
  wx_table <- reactive({
    
    wx_filtered() |>
      sf::st_drop_geometry() |>
      dplyr::select(
        rep_date, wmo, name,
        ffmc, dmc, dc,
        isi, bui, fwi
      ) |>
      dplyr::mutate(
        row_id = dplyr::row_number(),
        .before = 1
      )
    
  })
  
  output$starting_codes <- renderDT({
    datatable(
      wx_table(),
      selection = list(
        mode = "single",
        target = "row"
      )
    )
  })
  
  observeEvent(input$starting_codes_rows_selected, {
    idx <- input$starting_codes_rows_selected
    req(length(idx) == 1)
    row <- wx_table()[idx, ]
    updateRadioButtons(
      session,
      "index_source",
      selected = "Nearby Weather Station"
    )
    updateNumericInput(
      session,
      "user_ffmc",
      value = row$ffmc
    )
    updateNumericInput(
      session,
      "user_dmc",
      value = row$dmc
    )
    updateNumericInput(
      session,
      "user_dc",
      value = row$dc
    )
    
  })
  
  

  # SAVE Web Services=======================================
  
  observeEvent(input$save_wcs, {
    
    if (!nzchar(input$fire_name)) {
      showNotification(
        "Please enter a fire name.",
        type = "error"
      )
      return()
    }
    
    outdir_parsed <- tryCatch(
      parseDirPath(volumes, input$outdir),
      error = function(e) NULL
    )
    
    if (is.null(outdir_parsed) || length(outdir_parsed) == 0) {
      showNotification("Please select an output folder.", type = "error")
      return()
    }
    
    if (is.null(point_event())) {
      showNotification(
        "Please select a point on the map.",
        type = "error"
      )
      return()
    }
    base_dir <- output_dir()
    
    out_dir <- file.path(base_dir,
                         paste0(format(Sys.Date(), "%Y%m%d"),"_Scenario"),
                         "shp_data")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(base_dir,
                         paste0(format(Sys.Date(), "%Y%m%d"),"_Scenario"),
                         "Output"), recursive = TRUE, showWarnings = FALSE)
    
    # NBAC 
    
    if (isTRUE(input$save_nbac)) {
      
      sf::write_sf(
        nbac_filtered(),
        file.path(out_dir, "nbac.shp"),
        delete_layer = TRUE
      )
    }
    
    # PERIM 
    if (isTRUE(input$save_perim)) {
      
      req(perim_filtered())
      
      sf::write_sf(
        sf::st_transform(perim_filtered(), 4326),
        file.path(
          out_dir,
          paste0(format(Sys.Date(), "%Y%m%d"), "_M3perim.shp")
        ),
        delete_layer = TRUE
      )
    }
    
    # HOTSPOTS
    
    if (isTRUE(input$save_hs)) {
      
      req(hotspots_filtered())
      
      hs <- hotspots_filtered()
      
      sf::write_sf(
        sf::st_transform(hs, 4326),
        file.path(
          out_dir,
          paste0(format(Sys.Date(), "%Y%m%d"), "_hs.shp")
        ),
        delete_layer = TRUE
      )
      
      # CSV export
      hs_csv <- sf::st_coordinates(hs) |>
        cbind(sf::st_drop_geometry(hs))
      
      write.csv(
        hs_csv,
        file = file.path(
          out_dir,
          paste0(format(Sys.Date(), "%Y%m%d"), "_hs.csv")
        ),
        row.names = FALSE
      )
    }
    
    ## Wx Station Output
    
    if (isTRUE(input$save_wx)) {
      
      wx <- wx_filtered()
      
      # CSV export
      wx_csv <- sf::st_coordinates(wx) |>
        cbind(sf::st_drop_geometry(wx))
      
      write.csv(
        wx_csv,
        file = file.path(
          out_dir,
          paste0(format(Sys.Date(), "%Y%m%d"), "_wx_stns.csv")
        ),
        row.names = FALSE
      )
    }
    showNotification("Web Service layers saved", type = "message")
  })
  

  # GET FIRE POINT=========================================================
  get_point <- reactive({
    
    req(input$loc_type)
    
    loc <- input$loc_type
    
     
    ## 1. MAP CLICK=========================================================
    
    if (loc == "Map Click") {
      
      if (is.null(input$map_click)) {
        return(NULL)
      }
      
      return(
        sf::st_as_sf(
          data.frame(
            lon = input$map_click$lng,
            lat = input$map_click$lat
          ),
          coords = c("lon", "lat"),
          crs = 4326
        )
      )
    }
    
    # 2. LAT / LONG INPUT ==================================

    if (loc == "Lat/Long") {
      
      req(input$lon, input$lat)
      
      return(
        sf::st_as_sf(
          data.frame(
            lon = input$lon,
            lat = input$lat
          ),
          coords = c("lon", "lat"),
          crs = 4326
        )
      )
    }
    
    # 3. FILE UPLOAD (KML or SHAPEFILE ZIP)====================================
    
    if (loc %in% c("Upload Perim (kml)", "Upload Perim (.zip)")) {
      
      req(input$perim_upload)
      
      file_path <- input$perim_upload$datapath
      file_name <- input$perim_upload$name
      
      ### KML PATH------------------------------------------------

      if (loc == "Upload Perim (kml)") {
        
        fire <- sf::st_read(file_path, quiet = TRUE)
        
      } else {
        
        ## ZIP SHAPEFILE PATH ==========================
        validate(
          need(grepl("\\.zip$", file_name),
               "Please upload a .zip file containing shapefile components")
        )
        
        temp_dir <- file.path(tempdir(), paste0("shp_", as.integer(Sys.time())))
        dir.create(temp_dir)
        
        unzip(file_path, exdir = temp_dir)
        
        shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
        
        validate(
          need(length(shp_file) == 1,
               "Zip must contain exactly one .shp file")
        )
        
        Sys.setenv(SHAPE_RESTORE_SHX = "YES")
        
        fire <- sf::st_read(shp_file[1], quiet = TRUE)
      }
      
      # COMMON GEOMETRY CLEANUP-------------------------------------------
   
      fire <- fire[
        sf::st_geometry_type(fire) %in% c("POLYGON", "MULTIPOLYGON"),
      ]
      
      req(nrow(fire) > 0)
      
      fire <- sf::st_make_valid(fire)
      
      # CRS handling
      if (is.na(sf::st_crs(fire))) {
        sf::st_crs(fire) <- 4326
      } else {
        fire <- sf::st_transform(fire, 4326)
      }
      
      # centroid of union
      return(
        if(any(grepl(x = st_geometry_type(fire),pattern = "MULTI|POLYGON"))){sf::st_as_sfc(sf::st_bbox(fire))} 
        else{
        sf::st_centroid(sf::st_union(fire))}
      )
    }
    
    return(NULL)
  })
  
  
  # Fuels + DEM ==============================================

   observeEvent(input$clip, {
    
    ## ---- user-facing validation ----
    point <- get_point()
    
    if (!nzchar(input$fire_name)) {
      showNotification(
        "Please enter a fire name.",
        type = "error"
      )
      return()
    }
    
    outdir_parsed <- tryCatch(
      parseDirPath(volumes, input$outdir),
      error = function(e) NULL
    )
    
    if (is.null(outdir_parsed) || length(outdir_parsed) == 0) {
      showNotification("Please select an output folder.", type = "error")
      return()
    }
    
    if (is.null(point_event())) {
      showNotification(
        "Please select a point on the map.",
        type = "error"
      )
      return()
    }
    
    outdir_parsed <- tryCatch(
      parseDirPath(volumes, input$outdir),
      error = function(e) NULL
    )
    
    
    if (input$fuel_source == "Local File" && is.null(fuel_path())) {
      showNotification("Please select a fuel raster.", type = "error")
      return()
    }
    
    base_dir <- output_dir()
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(base_dir,
                         paste0(format(Sys.Date(), "%Y%m%d"),"_Scenario"),
                         "Output"), recursive = TRUE, showWarnings = FALSE)
    
    # ---- spatial prep ----
    coord <- st_coordinates(point)
    b_4326 <- st_buffer(point, 100000)

    # ---- fuels ----
    if (input$fuel_source == "CWFIS National Grid (2024)") {
      
      showModal(modalDialog(
        title = "Processing",
        "Grabbing DEM... this may take a minute.",
        footer = NULL,
        easyClose = FALSE
      ))
      DEM<-grid_grab(aoi_e = b_4326, output_directory =paste0(base_dir, "/"))
      removeModal()
      
      showNotification("Fuels + DEM clipped successfully", type = "message")
      
    } else {
      file_info <- shinyFiles::parseFilePaths(volumes, input$fuel_local)
      fuel_path <- file_info$datapath[1]
      
      req(fuel_path)
      
      fuels <- terra::rast(fuel_path)
      
      target_crs <- crs(fuels)
      b_target <- st_transform(b_4326, target_crs)
      bb_target <- st_bbox(b_target)
      
      fuels <- crop(fuels, ext(bb_target))
      showModal(modalDialog(
        title = "Processing",
        "Grabbing DEM... this may take a minute.",
        footer = NULL,
        easyClose = FALSE
      ))
      DEM<-grid_grab(reference_grid = fuels, output_directory =paste0(base_dir, "/"),ref_is_fuel = T)
      removeModal()
      showNotification("Fuels + DEM clipped successfully", type = "message")
      
    }
  })
  
  
  # -------------------- Spot Wx ----------------------------
  
  ## ---- Get Spot models ----
  models_available <- reactive({
    req(input$api)
    
    pt <- get_point()
    req(pt)
    
    coord <- sf::st_coordinates(pt)
    
    inv_url <- paste0(
      "https://spotwx.io/api.php?key=", input$api,
      "&lat=", coord[2],
      "&lon=", coord[1],
      "&model=inventory"
    )
    
    res <- httr::GET(inv_url)
    models <- httr::content(res) |> colnames()
    
    return(models)
  })
  observe({
    models <- models_available()
    req(models)
    
    updateCheckboxGroupInput(
      session,
      "models",
      choices = models,
      selected = models
    )
  })
  observeEvent(input$select_all, {
    models <- models_available()
    req(models)
    
    updateCheckboxGroupInput(
      session,
      "models",
      selected = models
    )
  })
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(
      session,
      "models",
      selected = character(0)
    )
  })
  
   ## ---- Extract SpotWX ----
  extract_spotwx<-function(apikey,lat,lon,model,fwi=F){
    
    ##Get tz based on location ----
    tz <- lutz::tz_lookup_coords(lat=lat,lon=lon,method='accurate',warn = F)
    zone <- lutz::tz_offset(Sys.Date(),tz)$zone
    tz <- lutz::tz_offset(Sys.Date(),tz)$utc_offset_h
    
    
    model_run <- paste0(format(Sys.Date()-1,"%Y%m%d"),"_12Z")
    
    #Hit the SpotWx API for data----
    
    if(fwi){
      url<-glue::glue(
        "https://spotwx.io/api.php?key={apikey}&",
        "lat={lat}&lon={lon}&model={model}&modelrun={model_run}&tz={tz}&format=prometheus")
      url2<-glue::glue(
        "https://spotwx.io/api.php?key={apikey}&lat={lat}&lon={lon}",
        "&model={model}&modelrun={model_run}&tz={tz}&output=metadata")
      url3<-glue::glue(
        "https://spotwx.io/api.php?key={apikey}&lat={lat}&lon={lon}",
        "&model={model}&modelrun={model_run}&tz={tz}")
      
      sptget<-httr::RETRY("GET",url=url,times=10,pause_cap=4,pause_min=1.1)
      metget<-httr::RETRY("GET",url=url2,times=10,pause_cap=4,pause_min=1.1)
      runget<-httr::RETRY("GET",url=url3,times=10,pause_cap=4,pause_min=1.1)
    }else{
      url<-glue::glue(
        "https://spotwx.io/api.php?key={apikey}&",
        "lat={lat}&lon={lon}&model={model}&tz={tz}&format=prometheus")
      url2<-glue::glue(
        "https://spotwx.io/api.php?key={apikey}&lat={lat}&lon={lon}",
        "&model={model}&tz={tz}&output=metadata")
      url3<-glue::glue(
        "https://spotwx.io/api.php?key={apikey}&lat={lat}&lon={lon}",
        "&model={model}&tz={tz}")
      
    sptget<-httr::RETRY("GET",url=url,times=10,pause_cap=4,pause_min=1.1)
    metget<-httr::RETRY("GET",url=url2,times=10,pause_cap=4,pause_min=1.1)
    runget<-httr::RETRY("GET",url=url3,times=10,pause_cap=4,pause_min=1.1)
    }
    if(sptget$status_code==200){
      meta=httr::content(metget,show_col_types = F)
      meta$model_run <- httr::content(runget, show_col_types = FALSE)$ISSUEDATE[1]
      meta$Acquisition_GMT<-metget$dat
      meta$zone <- zone
      return(list(meta,prometheus=httr::content(sptget,show_col_types = F),full_model=httr::content(runget,show_col_types = F)
      ))
    } else{
      
    }
  }
  observeEvent(input$retrieve_models, {
    
    if (!nzchar(trimws(input$api))) {
      showNotification(
        "Please enter your SpotWx API key or save one in your environment variables.",
        type = "error",
        duration = 10
      )
      return(invisible())
    }
    
    if (is.null(point_event())) {
      showNotification(
        "Please select a point on the map.",
        type = "error"
      )
      return()
    }
      
    withProgress(message = "Running SpotWX + plotting...", value = 0, {
      
      incProgress(0.2, "Getting location")
      
      point <- get_point()
      coord <- st_coordinates(point)
      
      incProgress(0.5, "Downloading SpotWX")
      
      req(input$api, input$models)
      
      results <- lapply(input$models, function(m){
        extract_spotwx(input$api, coord[2], coord[1], m)
      })
      
      spotwx_results(results)
      
      ## ---- build df ----
      df <- bind_rows(lapply(results, function(x) {
        
        meta <- x[[1]]
        full <- x$full_model
        full[] <- lapply(full, as.character)
        
        ## metadata back in ---------------
        hgt <- meta$hgt_surface
        full$hgt_surface <- as.character(meta$hgt_surface)
        full$model_elev <- paste0(full$MODEL," ", full$hgt_surface,"(m) ", 
                                  as.character(full$model_run))
        
        full
      }))
      df$DATETIME <- lubridate::ymd_hm(gsub("/", "-", df$DATETIME))
      coords <- sf::st_coordinates(get_point())
      
      df$lon <- coords[1,1]
      df$lat <- coords[1,2]
      spotwx_df(df)

    })
  })
  
  observeEvent(input$plot_models, {

    results <- spotwx_results()
    if (is.null(results)) {
      showNotification("No SpotWX results found. Please retrieve models first.", type = "error")
      return()
    }
    
    req(input$var)
    req(spotwx_df())
    df <- spotwx_df()
    req(get_point())
    
    coords_now <- sf::st_coordinates(get_point())
    
    if (df$lon[1] != coords_now[1,1] || df$lat[1] != coords_now[1,2]) {
      showNotification(
        "Location has changed. Please re-run 'Retrieve Models'.",
        type = "error",
        duration = 6
      )
      return()
    }
    ## ---- build ggplots (NOT plotly yet) ----
    plots <- lapply(seq_along(input$var), function(i) {
      
      var <- input$var[i]
      
      ggplot(df, aes(
        x = DATETIME,
        y = as.numeric(.data[[var]]),
        color = model_elev
      )) +
        geom_line() +
        labs(x = "", y = var) +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_x_datetime(
          limits = c(
            min(df$DATETIME),
            min(df$DATETIME) + lubridate::days(input$ndays)
          ),
          date_breaks = "1 day",
          date_labels = "%b %d"
        )
    })
    plots_plotly <- lapply(seq_along(input$var), function(i) {
      
      var <- input$var[i]
      
      p <- ggplot(df, aes(
        x = DATETIME,
        y = as.numeric(.data[[var]]),
        color = model_elev
      )) +
        geom_line() +
        labs(x = "", y = var) +
        theme_minimal() +   
        scale_x_datetime(
          limits = c(
            min(df$DATETIME),
            min(df$DATETIME) + lubridate::days(input$ndays)
          ),
          date_breaks = "1 day",
          date_labels = "%b %d"
        )
      
      ggplotly(p) %>%
        style(showlegend = (i == 1))   
    })
    
    legend <- cowplot::get_legend(
      ggplot(df, aes(
        x = DATETIME,
        y = as.numeric(.data[[input$var[1]]]),
        color = model_elev
      )) +
        geom_line() +
        theme_minimal() +
        theme(legend.position = "right")
    )
    ## ---- arrange ggplots ----
    p_gg <- gridExtra::grid.arrange(
      grobs = c(plots, list(legend)),
      ncol = 3
    )
    ## ---- STORE STATIC ggplot object ----
    spot_plot_obj(p_gg)
    
    ## ---- RENDER INTERACTIVE VERSION (separately) ----
    output$spot_plot <- renderPlotly({
      subplot(
        plots_plotly,
        nrows = 3,
        shareX = TRUE,
        titleY = TRUE
      ) %>%
        layout(
          legend = list(
            orientation = "v",
            x = 1.02,
            y = 1,
            xanchor = "left",
            yanchor = "top"
          ),
          margin = list(r = 120)
        )
    })
  })
  
  ## Save Spotwx Plots ===============
  
  observeEvent(input$save_plot, {
    results <- spotwx_results()
    if (is.null(results)) {
      showNotification("No SpotWX results found. Please retrieve models first.", type = "error")
      return()
    }
    df <- spotwx_df()
    req(df)
    req(get_point())
    
    if (is.null(spot_plot_obj())) {
      showNotification("Please generate a plot before saving.", type="error")
      return()
    }
    
    if (!nzchar(input$fire_name)) {
      showNotification("Please enter a fire name.", type="error")
      return()
    }
    
    outdir_parsed <- tryCatch(
      parseDirPath(volumes, input$outdir),
      error = function(e) NULL
    )
    
    if (is.null(outdir_parsed) || length(outdir_parsed) == 0) {
      showNotification("Please select an output folder.", type = "error")
      return()
    }
    
    coords_now <- sf::st_coordinates(get_point())
    
    if (df$lon[1] != coords_now[1,1] || df$lat[1] != coords_now[1,2]) {
      showNotification(
        "Location has changed. Please re-run 'Retrieve Models'.",
        type = "error",
        duration = 6
      )
      return()
    }
    
    
    ### ---- single source of truth (state layer) ----
    base_dir <- output_dir()
    
    ## add run-specific folder
    out_dir <- file.path(
      base_dir,
      paste0(format(Sys.Date(), "%Y%m%d"),"_Scenario"),
      "spotwx"
    )
    
    # ensure folder exists (safe even if already created)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(base_dir,
                         paste0(format(Sys.Date(), "%Y%m%d"),"_Scenario"),
                         "Output"), recursive = TRUE, showWarnings = FALSE)
    
    ## ---- file path ----
    file_path <- file.path(
      out_dir,
      paste0("spotwx_compare_", format(Sys.Date(), "%Y%m%d"), ".tiff")
    )
    
    ## ---- save plot ----
    ggsave(
      filename = file_path,
      plot = spot_plot_obj(),
      device = "tiff",
      width = 8,
      height = 8,
      dpi = 300
    )
    
    showNotification("Plot saved as TIFF", type = "message")
  })
  ## Save Spotwx #########
  observeEvent(input$save_spotwx, {
    
    results <- spotwx_results()
    if (is.null(results)) {
      showNotification("No SpotWX results found. Please retrieve models first.", type = "error")
      return()
    }
    df <- spotwx_df()
    req(get_point())
    
    if (!nzchar(input$fire_name)) {
      showNotification("Please enter a fire name.", type="error")
      return()
    }
    
    outdir_parsed <- tryCatch(
      parseDirPath(volumes, input$outdir),
      error = function(e) NULL
    )
    
    if (is.null(outdir_parsed) || length(outdir_parsed) == 0) {
      showNotification("Please select an output folder.", type = "error")
      return()
    }
    
    coords_now <- sf::st_coordinates(get_point())
    
    if (df$lon[1] != coords_now[1,1] || df$lat[1] != coords_now[1,2]) {
      showNotification(
        "Location has changed. Please re-run 'Retrieve Models'.",
        type = "error",
        duration = 6
      )
      return()
    }
    
    ### ---- single source of truth ----
    base_dir <- output_dir()
    
    out_dir <- file.path(
      base_dir,
      paste0(format(Sys.Date(), "%Y%m%d"), "_Scenario"),
      "spotwx"
    )
    
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    ## ---- metadata ----
    meta <- lapply(results, function(x) {
      meta_data <- x[[1]]
      meta_data$land_surface <- NULL
      meta_data
    })
    
    meta <- lapply(meta, function(df) {
      df$hgt_surface <- as.numeric(df$hgt_surface)
      df
    })
    
    meta_table <- bind_rows(meta)
    
    ## ---- forecasts ----
    invisible(lapply(seq_along(input$models), function(i) {
      
      model <- input$models[i]
      
      issue_date <- results[[i]]$full_model$ISSUEDATE[1]
      issue_date_clean <- gsub("[: /]", "", issue_date)
      
      write.csv(
        results[[i]]$prometheus,
        file = file.path(
          out_dir,
          paste0(model, "_prom_", issue_date_clean, ".csv")
        ),
        row.names = FALSE
      )
    }))
    
    ## ---- metadata write ----
    write.csv(
      meta_table,
      file = file.path(
        out_dir,
        paste0("Spotwxinfo_", format(Sys.Date(), "%Y%m%d"), ".csv")
      ),
      row.names = FALSE
    )
    
    showNotification("SpotWX saved successfully", type = "message")
  })
  # Plot FWI ===============
  observeEvent(input$plot_fwi, {
    
    df <- cffdrs_df()
    
    if (is.null(df)) {
      showNotification("No FWI Calculation results found. Please retrieve models first.", type = "error")
      return()
    }
    
    req(input$fwi_var)
    req(get_point())
    
    coords_now <- sf::st_coordinates(get_point())
    
    if (df$lon[1] != coords_now[1,1] || df$lat[1] != coords_now[1,2]) {
      showNotification(
        "Location has changed. Please re-run 'Retrieve Models'.",
        type = "error",
        duration = 6
      )
      return()
    }
 
    ## ---- build ggplots (NOT plotly yet) ----
    plots <- lapply(seq_along(input$fwi_var), function(i) {
      
      var <- input$fwi_var[i]
      
      ggplot(df, aes(
        x = DATETIME,
        y = as.numeric(.data[[var]]),
        color = source
      )) +
        geom_line() +
        labs(x = "", y = var) +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_x_datetime(
          limits = c(
            min(df$DATETIME),
            min(df$DATETIME) + lubridate::days(input$ndays)
          ),
          date_breaks = "1 day",
          date_labels = "%b %d"
        )
    })
    fwi_plots <- lapply(seq_along(input$fwi_var), function(i) {
      
      var <- input$fwi_var[i]
      
      p <- ggplot(df, aes(
        x = DATETIME,
        y = as.numeric(.data[[var]]),
        color = source
      )) +
        geom_line() +
        labs(x = "", y = var) +
        theme_minimal() +   
        scale_x_datetime(
          limits = c(
            min(df$DATETIME),
            min(df$DATETIME) + lubridate::days(input$ndays)
          ),
          date_breaks = "1 day",
          date_labels = "%b %d"
        )
      
      ggplotly(p) %>%
        style(showlegend = (i == 1))   
    })
    
    legend <- cowplot::get_legend(
      ggplot(df, aes(
        x = DATETIME,
        y = as.numeric(.data[[input$fwi_var[1]]]),
        color = source
      )) +
        geom_line() +
        theme_minimal() +
        theme(legend.position = "right")
    )
    ## ---- arrange ggplots ----
    p_gg <- gridExtra::grid.arrange(
      grobs = c(plots, list(legend)),
      ncol = 3
    )
    ## ---- STORE STATIC ggplot object ----
    fwi_plot_obj(p_gg)
    
    ## ---- RENDER INTERACTIVE VERSION (separately) ----
    output$fwi_plot <- renderPlotly({
      subplot(
        fwi_plots,
        nrows = 3,
        shareX = TRUE,
        titleY = TRUE
      ) %>%
        layout(
          legend = list(
            orientation = "v",
            x = 1.02,
            y = 1,
            xanchor = "left",
            yanchor = "top"
          ),
          margin = list(r = 120)
        )
    })
    showNotification("FWI plotted successfully", type = "message")
  })

  ## Save FWI Plots ===============
  
  observeEvent(input$save_fwi_plot, {
    results <- cffdrs_list()
    if (is.null(results)) {
      showNotification("No SpotWX results found. Please retrieve models first.", type = "error")
      return()
    }
    df <- cffdrs_df()
    req(df)
    req(get_point())
    
    if (is.null(fwi_plot_obj())) {
      showNotification("Please generate a plot before saving.", type="error")
      return()
    }
    
    if (!nzchar(input$fire_name)) {
      showNotification("Please enter a fire name.", type="error")
      return()
    }
    
    outdir_parsed <- tryCatch(
      parseDirPath(volumes, input$outdir),
      error = function(e) NULL
    )
    
    if (is.null(outdir_parsed) || length(outdir_parsed) == 0) {
      showNotification("Please select an output folder.", type = "error")
      return()
    }
    
    coords_now <- sf::st_coordinates(get_point())
    
    if (df$lon[1] != coords_now[1,1] || df$lat[1] != coords_now[1,2]) {
      showNotification(
        "Location has changed. Please re-run 'Retrieve Models'.",
        type = "error",
        duration = 6
      )
      return()
    }
    
    ### ---- single source of truth (state layer) ----
    base_dir <- output_dir()
    
    ## add run-specific folder
    out_dir <- file.path(
      base_dir,
      paste0(format(Sys.Date(), "%Y%m%d"),"_Scenario"),
      "fwi"
    )
    
    # ensure folder exists (safe even if already created)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(base_dir,
                         paste0(format(Sys.Date(), "%Y%m%d"),"_Scenario"),
                         "Output"), recursive = TRUE, showWarnings = FALSE)
    
    ## ---- file path ----
    file_path <- file.path(
      out_dir,
      paste0("fwi_compare_", format(Sys.Date(), "%Y%m%d"), ".tiff")
    )
    
    ## ---- save plot ----
    ggsave(
      filename = file_path,
      plot = fwi_plot_obj(),
      device = "tiff",
      width = 8,
      height = 8,
      dpi = 300
    )
    
    showNotification("Plot saved as TIFF", type = "message")
  })
  ## Save FWI Wx #########
observeEvent(input$save_fwi, {
  
  df <- cffdrs_list()
  req(get_point())
  
  if (!nzchar(input$fire_name)) {
    showNotification("Please enter a fire name.", type="error")
    return()
  }
  
  outdir_parsed <- tryCatch(
    parseDirPath(volumes, input$outdir),
    error = function(e) NULL
  )
  
  if (is.null(outdir_parsed) || length(outdir_parsed) == 0) {
    showNotification("Please select an output folder.", type = "error")
    return()
  }
  
  ### ---- single source of truth ----
  base_dir <- output_dir()
  
  out_dir <- file.path(
    base_dir,
    paste0(format(Sys.Date(), "%Y%m%d"),"_Scenario"),
    "fwi"
  )
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  ## ---- forecasts ----
  invisible(lapply(seq_along(input$models), function(i) {
    
    if(is.null(df[[i]])){return(NULL)}
    model <- input$models[i]
    
    
    write.csv(
      df[[i]],
      file = file.path(
        out_dir,
        paste0(model, "_fwi.csv")
      ),
      row.names = FALSE
    )
  }))
  
    showNotification("FWI saved successfully", type = "message")
  })
  
  # Calculate FWI ================
  
  observeEvent(input$calc_fwi,{
    
      req(spotwx_results())
      showNotification("Collecting Yesterdays Model Run for Backfill", type = "message")
      pt <- get_point()
      req(pt)
      
      coord <- sf::st_coordinates(pt)
      wx_yest <-lapply(input$models, function(m){
        extract_spotwx(input$api, coord[2], coord[1], m,fwi=T)
      })
      showNotification("Collectiion Complete", type = "message")
      
      weather <- spotwx_results()
      showNotification("Preparing FWI Data", type = "message")
      fwi_list <- lapply(weather,function(model_wx){
        
        if(any(model_wx$prometheus$TEMP =="null")){return(NULL)}
        yesterday <- wx_yest[[which(input$models == model_wx[[1]]$model)]]$prometheus
        if(nrow(yesterday) == 0){return(NULL)}
        fwi_wx <- model_wx$prometheus
        
        ## This gets silly fast, because Spot WX is coming in as the locally used
        # timezone we need to assess if we have the value that would have been
        # associated with yesterdays starting codes for use when calculating
        # hourly values. Otherwise we cut off a substantial amount of our 
        # weather data.
        
        if(grepl("S",model_wx[[1]]$zone)){
            noon_wx <- fwi_wx[which(fwi_wx$HOUR == 12),]
        }else{ 
            noon_wx <- fwi_wx[which(fwi_wx$HOUR == 13),]
        }
        
        names(noon_wx) <- c("DATE","HOUR","TEMP","RH","WD","WS","PREC")
        
       if(fwi_wx$HOUR[1] != "17"){
          out_wx <- rbind(yesterday[if(grepl("S",model_wx[[1]]$zone)){
            which(yesterday$HOUR == "17")[1]
          }else{
              which(yesterday$HOUR == "18")[1]
          }:which(paste(yesterday$HOURLY, yesterday$HOUR) == paste(fwi_wx$HOURLY[1], fwi_wx$HOUR[1])),],fwi_wx[-1,])
       } else {out_wx <- fwi_wx[-1,]}
        names(out_wx) <- c("DATE","HOUR","TEMP","RH","WD","WS","PREC")
        out_wx[,c("DMC","DC","BUI")] <- NA
        
        noon_wx$DATE <- as.Date(noon_wx$DATE,"%d/%m/%Y")
        noon_wx$yr <- format(noon_wx$DATE,"%Y")
        noon_wx$mon <- format(noon_wx$DATE,"%m")
        noon_wx$day <- format(noon_wx$DATE,"%d")
        noon_wx$lat <- coord[2]
        noon_wx$long <- coord[1]
        
        noon_wx <- cffdrs::fwi(noon_wx,
                    init = c(input$user_ffmc,
                             input$user_dmc,
                             input$user_dc))
        
        out_wx[1,c("DMC","DC")] <- data.frame(input$user_dmc,input$user_dc)
        out_wx[1:which(out_wx$HOUR == "12")[1],c("DMC","DC","BUI")] <- data.frame("DMC"=input$user_dmc,"DC" = input$user_dc, "BUI"= cffdrs:::buildup_index(out_wx[1,"DMC"],out_wx[1,"DC"]))
        
        out_matchs <- data.frame(from=which(out_wx$HOUR == 13),to=c(which(out_wx$HOUR ==  12)[-1],nrow(out_wx)))
        
        for(i in seq(noon_wx$TEMP)){
          out_wx[out_matchs[i,"from"]:out_matchs[i,"to"],c("DMC","DC","BUI")] <- noon_wx[i,c("DMC","DC","BUI")] 
        }
        
        out_wx <- cffdrs::hffmc(out_wx,
                      ffmc_old = input$user_ffmc,
                      hourlyFWI = T)
        names(out_wx) <- toupper(names(out_wx))
    
        showNotification("Hourly FWI Calculation Complete", type = "message")
        return(out_wx)
        
      })
      names(fwi_list) <- input$models
      
      cffdrs_list(fwi_list)
      
      list2env(fwi_list, envir = .GlobalEnv)
      ## ---- build df ----
      fwi_list <- Map(
                function(df, nm) {
                    df$source <- nm
                    df
                  },
                fwi_list,
          names(fwi_list)

      )
      
      df <- bind_rows(lapply(fwi_list, function(x) {
        
        if(length(x) == 1){return(NULL)}
        x$WD <- as.numeric(x$WD)
        out <- x
        out
      }))
      df$DATETIME <- lubridate::ymd_hm(paste0(as.Date(df$DATE,"%d/%m/%Y")," ",sprintf("%02d", df$HOUR),":00"))
      coords <- sf::st_coordinates(get_point())
      
      df$lon <- coords[1,1]
      df$lat <- coords[1,2]
      
      cffdrs_df(df)
      })
      
}

shinyApp(ui, server)
