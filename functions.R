
# ---- UI ----
ui <- fluidPage(
  titlePanel("Prometheus data prepper"),
  
  sidebarLayout(
    sidebarPanel(
      
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
          value = c(2020, 2025),
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
      actionButton("save_wcs", "Save selected"),
      hr(),
      
      h4("SpotWX"),
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
      
    ),
    
    mainPanel(
      leafletOutput("map", height = 400),
      plotlyOutput("spot_plot", height = 600)
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
  nbac_raw <- reactiveVal(NULL)
  hotspots_raw <- reactiveVal(NULL)
  perim_raw <- reactiveVal(NULL)
  point_event <- reactiveVal(NULL)
  
  output$spot_plot <- renderPlotly({
    req(spot_plot_obj())
    spot_plot_obj()
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
  
  # =========================================================
  # -------------------- MAP PIPELINE ------------------------
  # =========================================================
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
  
  # =========================================================
  # ------------------------ NBAC ----------------------------
  # =========================================================
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
  
  # =========================================================
  # ---------------------- HOTSPOTS --------------------------
  # =========================================================
  # ---- DOWNLOAD ONLY (cache) ----
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
  
  # ---- FILTER + RENDER  ----
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
  
  # =========================================================
  # ------------------ M3 Perimeters ------------------------
  # =========================================================
  
  # ---- DOWNLOAD ONLY (cache) ----
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
  
  # ---- FILTER +RENDER ONLY ----
  perim_filtered <- reactive({
    
    req(perim_raw(), point_event(), input$perim_radius)
    
    perim <- perim_raw() |>
      sf::st_transform(3978)
    
    centre <- sf::st_transform(point_event(), 3978)
    
    d <- sf::st_distance(sf::st_centroid(perim), centre)[, 1]
    
    d<-as.numeric(d)
    
    perim[d <= input$perim_radius * 1000, ]
  })
  
  # =========================================================
  # -------------------- SAVE WCS ---------------------------
  # =========================================================
  observeEvent(input$save_wcs, {
    
    validate(
      need(nzchar(input$fire_name), "Please enter a fire name."),
      need(!is.null(output_dir()), "Please select an output folder.")
    )
    
    base_dir <- output_dir()
    
    out_dir <- file.path(base_dir, "shp_data")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
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
    
    showNotification("WCS layers saved", type = "message")
  })
  
  # =========================================================
  # -------------------- GET FIRE POINT ---------------------
  # =========================================================
  get_point <- reactive({
    
    req(input$loc_type)
    
    loc <- input$loc_type
    
    # =========================================================
    # 1. MAP CLICK
    # =========================================================
    if (loc == "Map Click") {
      
      req(input$map_click)
      
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
    
    # =========================================================
    # 2. LAT / LONG INPUT
    # =========================================================
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
    
    # =========================================================
    # 3. FILE UPLOAD (KML or SHAPEFILE ZIP)
    # =========================================================
    
    if (loc %in% c("Upload Perim (kml)", "Upload Perim (.zip)")) {
      
      req(input$perim_upload)
      
      file_path <- input$perim_upload$datapath
      file_name <- input$perim_upload$name
      
      # ---------------------------------------------------------
      # KML PATH
      # ---------------------------------------------------------
      if (loc == "Upload Perim (kml)") {
        
        fire <- sf::st_read(file_path, quiet = TRUE)
        
      } else {
        
        # ZIP SHAPEFILE PATH
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
      
      # ---------------------------------------------------------
      # COMMON GEOMETRY CLEANUP
      # ---------------------------------------------------------
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
      return(sf::st_centroid(sf::st_union(fire)))
    }
    
    return(NULL)
  })
  
  # --- Fuels 
   observeEvent(input$clip, {
    
    # ---- user-facing validation ----
    point <- get_point()
    
    if (is.null(point)) {
      showNotification("Please select a location first.", type = "error")
      return()
    }
    
    if (!nzchar(input$fire_name)) {
      showNotification("Please enter a fire name.", type = "error")
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
    
    if (input$fuel_source == "Local File" && is.null(fuel_path())) {
      showNotification("Please select a fuel raster.", type = "error")
      return()
    }
    
    # ---- now safe to use state layer ----
    base_dir <- output_dir()
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
    
    # ---- spatial prep ----
    coord <- st_coordinates(point)
    b_4326 <- st_buffer(point, 50000)
  #  bb_4326 <- st_bbox(b_4326)
   # b_3978 <- st_transform(b_4326, 3978)
  #  bb_4326 <- st_bbox(b_4326)
  #  bb_3978 <- st_bbox(b_3978)
    
  #  target_crs <- sf::st_intersection(utm_canada,sf::st_transform(point,sf::st_crs(utm_canada)),)$EPSG
   # point_target <- st_transform(point, target_crs)
  #  b_target<- st_buffer(point_target, 50000)
  #  bb_target<-st_bbox(b_target)
  #  bb_4326 <- bb_target |>
  #    st_as_sfc() |>
  #    st_transform(4326) |>
  #    st_bbox()
    
    
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
      b_target <- st_transform(b_3978, target_crs)
      bb_target <- st_bbox(b_target)
      
      fuels <- crop(fuels, ext(bb_target))
      showModal(modalDialog(
        title = "Processing",
        "Grabbing DEM... this may take a minute.",
        footer = NULL,
        easyClose = FALSE
      ))
      DEM<-grid_grab(reference_grid = fuels, output_directory =paste0(base_dir, "/"))
      removeModal()
      showNotification("Fuels + DEM clipped successfully", type = "message")
      
    }
  })
  
  # ---- Get Spot models ----
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
  
  # ---- Extract SpotWX ----
  extract_spotwx<-function(apikey,lat,lon,model){
    
    #Get tz based on location ----
    tz=lutz::tz_lookup_coords(lat=lat,lon=lon,method='accurate',warn = F)
    tz<-lutz::tz_offset(clock::date_today(""),tz)$utc_offset_h
    
    #Hit the SpotWx API for data----
    
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
    if(sptget$status_code==200){
      meta=httr::content(metget,show_col_types = F)
      meta$model_run <- httr::content(runget, show_col_types = FALSE)$ISSUEDATE[1]
      meta$Acquisition_GMT<-metget$dat
      return(list(meta,prometheus=httr::content(sptget,show_col_types = F),full_model=httr::content(runget,show_col_types = F)
      ))
    } else{
      
    }
  }
  observeEvent(input$retrieve_models, {
    
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
      
      # ---- build df ----
      df <- bind_rows(lapply(results, function(x) {
        
        meta <- x[[1]]
        full <- x$full_model
        full[] <- lapply(full, as.character)
        
        # metadata back in
        hgt <- meta$hgt_surface
        full$hgt_surface <- as.character(meta$hgt_surface)
        full$model_elev <- paste0(full$MODEL," ", full$hgt_surface,"(m) ", 
                                  as.character(full$model_run))
        
        full
      }))
      df$DATETIME <- lubridate::ymd_hm(gsub("/", "-", df$DATETIME))
      
      
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
    
    # ---- build ggplots (NOT plotly yet) ----
    plots <- lapply(seq_along(input$var), function(i) {
      
      var <- input$var[i]
      
      ggplot(df, aes(
        x = DATETIME,
        y = as.numeric(df[[var]]),
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
        y = as.numeric(df[[var]]),
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
        y = as.numeric(df[[input$var[1]]]),
        color = model_elev
      )) +
        geom_line() +
        theme_minimal() +
        theme(legend.position = "right")
    )
    # ---- arrange ggplots ----
    p_gg <- gridExtra::grid.arrange(
      grobs = c(plots, list(legend)),
      ncol = 3
    )
    # ---- STORE STATIC ggplot object ----
    spot_plot_obj(p_gg)
    
    # ---- RENDER INTERACTIVE VERSION (separately) ----
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
  observeEvent(input$save_plot, {
    
    # ---- user-facing validation (messages) ----
    validate(
      need(spot_plot_obj(), "Please generate a plot before saving."),
      need(nzchar(input$fire_name), "Please enter a fire name."),
      need(!is.null(input$outdir), "Please select an output folder.")
    )
    
    # ---- single source of truth (state layer) ----
    base_dir <- output_dir()
    
    # add run-specific folder
    out_dir <- file.path(
      base_dir,
      paste0(format(Sys.Date(), "%Y%m%d"), "_spotwx")
    )
    
    # ensure folder exists (safe even if already created)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # ---- file path ----
    file_path <- file.path(
      out_dir,
      paste0("spotwx_compare_", format(Sys.Date(), "%Y%m%d"), ".tiff")
    )
    
    # ---- save plot ----
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
  observeEvent(input$save_spotwx, {
    
    results <- spotwx_results()
    
    # ---- user-facing validation ----
    validate(
      need(!is.null(results), "No SpotWX results found. Please retrieve models first."),
      need(nzchar(input$fire_name), "Please enter a fire name."),
      need(!is.null(input$outdir), "Please select an output folder.")
    )
    
    # ---- single source of truth ----
    base_dir <- output_dir()
    
    out_dir <- file.path(
      base_dir,
      paste0(format(Sys.Date(), "%Y%m%d"), "_spotwx")
    )
    
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # ---- metadata ----
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
    
    # ---- forecasts ----
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
    
    # ---- metadata write ----
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
  
}
