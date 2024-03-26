library(shiny)
library(shinythemes)
library(sp)
library(sf)
library(spNetwork)
library(tmap)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(spatstat)
library(raster)
library(maptools)
library(tidyr)

mpsz <- st_read(dsn = "testdata", layer = "MPSZ-2019")
popagsex <- read_csv("testdata/respopagesextod2011to2020.csv")
childcare <- st_read("testdata/PreSchoolsLocation.geojson") %>% st_transform(crs = 3414)
binlocation <- read_rds("Data/alba/ewbins.rds")
roads_in_singapore <- read_rds("testdata/sgRoad.rds")
inbins <- read_rds("Data/nea/inbins.rds")
sg_sf <- st_read(dsn = "testdata", layer = "CostalOutline")


# Define UI for application
ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage(                                                
                  title = div(
                    a(
                      h2("RecycleSG", style = "margin-top: -8px;padding-right:10px;padding-bottom:10px"),
                      href = "https://is415-gaa-jkw.netlify.app/"
                    )
                  ),
                  tags$head(tags$style(
                    type="text/css",
                    "#childcare_image img {max-width: 100%; width: 100%; height: auto}",
                    "#introductionkernel img {max-width: 100%; width: 100%; height: auto}",
                    ".navbar {background-color: rgb(48, 48, 48)}",
                    " #clark{
                      color: grey;
                      background: rgba(0, 0, 0, 0);
                      border-color: grey;
                      border-style: solid;
                      border-width: 2px;
                    }",
                    " #netKDESettings {
                      color: grey;
                      background: rgba(0, 0, 0, 0);
                      border-color: grey;
                      border-style: solid;
                      border-width: 2px;
                    }",
                    " #KDESettings {
                      color: grey;
                      background: rgba(0, 0, 0, 0);
                      border-color: grey;
                      border-style: solid;
                      border-width: 2px;
                    }",
                    
                    
                  )),
                  tabPanel("Home Page",
                           fluidRow(
                             column(12,
                                    h2("Project Motivation"),
                                    hr(),
                                    column(12,
                                           uiOutput("projectMotivation"),                         
                                    ),
                             ),
                             
                           ),
                           fluidRow(
                             column(12,
                                    h2("What is Point Pattern Analysis and Network Constrained Point Pattern Analysis?"),
                                    hr(),
                                    uiOutput("ppancppa")),
                           ),
                           fluidRow(
                             column(12,
                                    h2("About Spatial Bros"),
                                    hr(),
                                    uiOutput("aboutus")),
                           )
                  ),
                  
                  tabPanel("Exploratory Data Analysis",
                           titlePanel("Data Exploration"),
                           tabPanel("Introduction",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("map_choice", "Map:", 
                                                    choices = c("CartoDB", "Openstreetmap", "ESRI"), 
                                                    selected = "CartoDB"),
                                        selectInput("bin_choice", "Bin Type:", 
                                                    choices = c("Blue Bins", "E-Waste Bins", "Incentive Bins", "All Bins"), 
                                                    selected = "E-Waste Bins"),
                                        checkboxInput("switch_button", "Population Density", value = TRUE)
                                      ),
                                      mainPanel(
                                        tmapOutput("edamap", width = "100%", height = "700")
                                      )
                                    ),
                                    hr(),
                                    uiOutput("edadescription") 
                           )
                  ),
        
                  # 3rd Tab
                  tabPanel("KDE Analysis",
                           titlePanel("Network Constrained Point Pattern Analysis"),
                           hr(),
                           tabsetPanel(type = "tabs",
                                       tabPanel("Introduction",
                                                fluidRow(
                                                  column(6,
                                                         h2("Welcome to the Network Constrained Point Pattern Analysis!"),
                                                         hr(),
                                                         uiOutput("introductiondescription")                         
                                                         
                                                  ),
                                                  column(6,
                                                         h2(),
                                                         imageOutput("introductionKernel")),
                                                ),
                                       ),  
                                       #tabPanel("Bin Location Points",
                                        #        sidebarLayout(
                                         #         mainPanel(
                                          #          tmapOutput("mapPlot", width = "100%", height = "700"),
                                           #       ),
                                            #      sidebarPanel(
                                             #       shinyjs::useShinyjs(),
                                              #      h4("Bin Location Points"),
                                               #     h5("Select the type of bin to be displayed on the map"),
                                                #    selectInput(inputId = "bin_type",
                                                 #               label = "Select Bin Type",
                                                  #              choices = unique(binlocation$`Type of Bin Placed`)),
                                                  #),
                                                  
                                                #),
                                                
                                       #),
                                       tabPanel("Kernel Density Estimation", 
                                                sidebarLayout(
                                                  mainPanel(
                                                    tmapOutput("kdePlot", width = "100%", height = "700"),
                                                    br(),
                                                    verbatimTextOutput("netKDESettings"),
                                                  ),
                                                  sidebarPanel(
                                                    shinyjs::useShinyjs(),
                                                    h4("Bandwidth Type:"),
                                                    h5("Select the bandwidth type:"),
                                                    selectInput(inputId = "bandwidth_type",
                                                                       label = "Bandwidth Type:",
                                                                       choices = c("Adaptive Bandwidth" = "Adaptive Bandwidth",
                                                                                   "Fixed Bandwidth" = "Fixed Bandwidth")),
                                                    h4("Kernel Density Estimation", id = "kernel_header"),
                                                    h5("Kernel Density Estimation Methods", id = "kernel_label"),
                                                    selectInput(inputId = "kernel_name",
                                                                label = "Choose the kernel to be used:",
                                                                choices = list("Quartic" = "quartic",
                                                                               "Disc" = "disc",
                                                                               "Epanechnikov" = "epanechnikov",
                                                                               "Gaussian" = "gaussian")),
                                                    h4("Bandwidth Selection", id = "bandwidth_header"),
                                                    h5("Select the bandwidth name:", id = "bandwidth_label"),
                                                    selectInput(inputId = "bandwidth_name",
                                                                label = "Bandwidth Name:",
                                                                choices = c("diggle", "ppl", "CvL", "scott"),
                                                                selected = "ppl")
                                                  ),
                                                  
                                                ),
                                                uiOutput("netKDEExpaliner"),
                                                br(),
                                       ),
                                       tabPanel("Network Constrained KDE (NetKDE) Analysis", 
                                                sidebarLayout(
                                                  mainPanel(
                                                    h3("Road Bin Plot"),
                                                    tmapOutput("roadBinPlot", width = "100%", height = "350"),
                                                    br(),
                                                    h3("NetKDE Plot"),
                                                    tmapOutput("networkPlot", width = "100%", height = "350") # Second map plot
                                                  ),
                                                  sidebarPanel(
                                                    shinyjs::useShinyjs(),
                                                    h4("Network Constrained KDE Analysis"),
                                                    h5("Select the area to perform the analysis"),
                                                    selectInput(inputId = "area",
                                                                label = "Select Area",
                                                                choices = list("MUSEUM",
                                                                               "MARINE PARADE",
                                                                               "DOWNTOWN CORE",
                                                                               "QUEENSTOWN",
                                                                               "OUTRAM",
                                                                               "KALLANG",
                                                                               "TANGLIN",
                                                                               "NEWTON",
                                                                               "CLEMENTI",
                                                                               "ORCHARD",
                                                                               "GEYLANG",
                                                                               "NOVENA",
                                                                               "BUKIT TIMAH",
                                                                               "TOA PAYOH",
                                                                               "JURONG WEST",
                                                                               "SERANGOON",
                                                                               "BISHAN",
                                                                               "TAMPINES",
                                                                               "BUKIT BATOK",
                                                                               "HOUGANG",
                                                                               "ANG MO KIO",
                                                                               "BUKIT PANJANG",
                                                                               "SUNGEI KADUT",
                                                                               "YISHUN",
                                                                               "PUNGGOL",
                                                                               "CHOA CHU KANG",
                                                                               "SENGKANG",
                                                                               "CENTRAL WATER CATCHMENT",
                                                                               "SEMBAWANG",
                                                                               "WOODLANDS"
                                                                ))
                                                  )
                                                ),
                                                uiOutput("netStatsExplainerp1"),
                                                br(),
                                       ),
                           ),
                           
                           
                  ),
                  # 4th Tab
                  tabPanel("Hotspot Analysis",
                           titlePanel("Hot Spot and Cold Spot Area Analysis (HCSA)"),
                           tabsetPanel(type = "tabs",
                                       tabPanel("Introduction",
                                                fluidRow(
                                                  column(8,
                                                         h2("Guide: Introduction to Hotspot and Coldspot Analysis for Recycling Bins in Singapore"),
                                                         hr(),
                                                         uiOutput("introductionhcsa")                         
                                                         
                                                  ),
                                                ),),
                                       tabPanel("Local Indicators of Spatial Association (LISA) Map",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("map_type", "Map:", 
                                                                choices = c("CartoDB", "Openstreetmap", "ESRI"), 
                                                                selected = "CartoDB"),
                                                    sliderInput("threshold", "Significant Level:", min = 0.05, max = 0.2, value = 0.05, step = 0.05),
                                                    selectInput("fill_var", "Bin Type:", choices = c("Blue Bins", "E-Waste Bins", "Incentive Bins"), selected = "Blue Bins"),
                                                    selectInput("category", "Category:", choices = c("Mean", "Median", "Pysal"), selected = "Mean")
                                                  ),
                                                  mainPanel(
                                                    tmapOutput("lisamap", width = "100%", height = "700")
                                                  )
                                                ),
                                                column(10,
                                                uiOutput("lisaExplainer")),
                                                br(),
                                       ),
                                       tabPanel("Visualising Hot Spot & Cold Spot Areas", 
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("map_var", "Map:", 
                                                                choices = c("CartoDB", "Openstreetmap", "ESRI"), 
                                                                selected = "CartoDB"),
                                                    sliderInput("sig_lvl", "Significant Level:", min = 0.05, max = 0.2, value = 0.05, step = 0.05),
                                                    selectInput("bin_var", "Bin Type:", choices = c("Blue Bins", "E-Waste Bins", "Incentive Bins"), selected = "Blue Bins")
                                                  ),
                                                  mainPanel(
                                                    tmapOutput("vhcsa", width = "100%", height = "700")
                                                  )
                                                  
                                                ),
                                                column(10,
                                                uiOutput("hcsaExplainer")),
                                                br(),
                                       )
                           )
                  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  id <- NULL
  tmap_options(check.and.fix = TRUE)
  
  #output$mapPlot <- renderTmap({
    # Filter binlocation data based on selected bin type
    #filtered_binlocation <- binlocation %>%
     # filter(`Type of Bin Placed` == input$bin_type)
    
    # Plot map with filtered binlocation data
   # tm_shape(mpsz) +
    #  tm_polygons() +
    #  tm_shape(filtered_binlocation) +
    #  tm_dots()
  #})
  
  output$edamap <- renderTmap({
    # Set pop data equals read csv
    pop <- read_csv("Data/singstat/PopulationSG_2023.csv")
    # Read rds
    ew_b <- readRDS("Data/alba/ewbins.rds")
    in_b <- readRDS("Data/nea/inbins.rds")
    # Read shape file that is not geojson
    blue_b <- st_read(dsn = "Data/gov", layer = "RECYCLINGBINS")
    
    
    popdata <- pop %>%
      group_by(PA, SZ, AG) %>%
      summarise(`POP` = sum(`Pop`)) %>%
      ungroup() %>%
      tidyr::pivot_wider(names_from = AG, 
                         values_from = POP) %>%
      mutate(`TOTAL` = rowSums(.[3:21])) %>%  
      dplyr::select(`PA`, `SZ`, `TOTAL`)
    
    popdata <- popdata %>%
      mutate_at(.vars = vars(PA, SZ), 
                .funs = toupper)
    
    pop <- left_join(mpsz, popdata,
                     by = c("SUBZONE_N" = "SZ"))
    
    
    # Check for invalid geometries
    invalid_geoms <- !st_is_valid(pop)
    # Fix invalid geometries
    pop[invalid_geoms, ] <- st_make_valid(pop[invalid_geoms, ])
    
    map_type = input$map_choice
    
    map <- switch(map_type,
                  "CartoDB" = "CartoDB.Positron",
                  "Openstreetmap" = "OpenStreetMap",
                  "ESRI" = "Esri.WorldTopoMap")
    
    pop_check <- input$switch_button
    bin_type <- input$bin_choice
    
    # Plot map
    map <- tm_basemap(map)
    
    if (pop_check){
      map <- map + 
        tm_shape(pop)+
        tm_fill("TOTAL", 
                style = "quantile", 
                palette = "Blues",
                alpha = 0.7) +
        tm_borders(alpha = 0.5) 
    }
    if (bin_type == "Blue Bins"){
      map <- map +
        tm_shape(blue_b)+
        tm_symbols(size = 0.005, col = "blue", shape = 2)
    }
    
    if (bin_type == "E-Waste Bins"){
      map <- map +
        tm_shape(ew_b)+
        tm_symbols(size = 0.3, col = "green", shape = 2)
    }
    
    if (bin_type == "Incentive Bins"){
      map <- map +
        tm_shape(in_b)+
        tm_symbols(size = 0.5, col = "red", shape = 2)
    }
    
    if (bin_type == "All Bins"){
      map <- map +
        tm_shape(blue_b)+
        tm_symbols(size = 0.005, col = "blue", shape = 2) +
        tm_shape(ew_b)+
        tm_symbols(size = 0.3, col = "green", shape = 2) +
        tm_shape(in_b)+
        tm_symbols(size = 0.5, col = "red", shape = 2)
    }
    
    map <- map +
      tm_view(set.view = 11, set.zoom.limits = c(11,15))

  })
  
  output$kdePlot <- renderTmap({
    # Convert binlocation to spatial objects
    bin_sf <- as_Spatial(binlocation)
    bin_sp <- as(bin_sf, "SpatialPoints")
    sg_sf <- st_transform(sg_sf, crs = 3414)
    sg <- as_Spatial(sg_sf)
    sg_sp <- as(sg, "SpatialPolygons")
    sg_owin <- as.owin(sg_sp)
    
    # Create ppp object
    bin_ppp <- as(bin_sp, "ppp")
    binSG_ppp <- bin_ppp[sg_owin] 
    bin_ppp.km <- rescale(binSG_ppp, 1, "km") # set to 1000 for working legend
    
    if(input$bandwidth_type == "Adaptive Bandwidth"){
      shinyjs::hide("bandwidth_name")
      shinyjs::hide("bandwidth_label")
      shinyjs::hide("bandwidth_header")
      shinyjs::hide("kernel_name")
      shinyjs::hide("kernel_label")
      shinyjs::hide("kernel_header")
      
      kde_origin_adaptive <- adaptive.density(bin_ppp.km, method="kernel")
      
      # Convert to raster
      gridded_kde_origin_adaptive <- as.SpatialGridDataFrame.im(kde_origin_adaptive)
      kde_raster <- raster(gridded_kde_origin_adaptive)
      projection(kde_raster) <- CRS("+init=EPSG:3414")
      
    }
    else{
      shinyjs::show("bandwidth_name")
      shinyjs::show("bandwidth_label")
      shinyjs::show("bandwidth_header")
      shinyjs::show("kernel_name")
      shinyjs::show("kernel_label")
      shinyjs::show("kernel_header")
      
      # Get selected kernel name from input
      kernel_name <- input$kernel_name
      
      # Define bandwidth using selected kernel
      bw_method <- switch(kernel_name,
                          "quartic" = "quartic",
                          "disc" = "disc",
                          "epanechnikov" = "epanechnikov",
                          "gaussian" = "gaussian")  # Default to gaussian if unknown kernel
      
      # Get selected bandwidth name from input
      bandwidth_name <- input$bandwidth_name
      
      # Define bandwidth sigma
      bw_sigma <- switch(bandwidth_name,
                         "diggle" = bw.diggle,
                         "ppl" = bw.ppl,
                         "CvL" = bw.CvL,
                         "scott" = bw.scott)
      
      # Compute kernel density estimation
      kde_bin_bw <- density(bin_ppp.km,
                            sigma = bw_sigma, 
                            edge = TRUE,
                            kernel = bw_method)
      
      # Convert to raster
      gridded_kde_bin_bw <- as.SpatialGridDataFrame.im(kde_bin_bw)
      kde_raster <- raster(gridded_kde_bin_bw)
      projection(kde_raster) <- CRS("+init=EPSG:3414")
    }
    
    # Plot KDE contours on top of polygons
    tmap_mode("plot")
    tm_shape(kde_raster) +
      tm_raster(style = "cont", palette = "plasma") +
      tm_layout(legend.outside = TRUE, legend.show = TRUE, legend.text.color = "white")+
      tm_view(set.view = 11, set.zoom.limits = c(11,15))
  })
  
  
  
  output$roadBinPlot <- renderTmap({
    # Debugging: Print selected area
    #print(input$area)
    
    # Filter mpsz based on selected area
    filtered_mpsz <- mpsz %>%
      filter(PLN_AREA_N == input$area) %>%
      st_union() %>%
      st_make_valid() %>%
      st_transform(crs = 3414)
    
    # Debugging: Print filtered mpsz
    #print(filtered_mpsz)
    
    # Perform intersection with roads_in_singapore based on selected area
    intersection_roads <- st_intersection(roads_in_singapore, filtered_mpsz)
    
    # Debugging: Print intersection_roads
    #print(intersection_roads)
    
    # Filter out POINT geometries
    filtered_roads <- intersection_roads[st_geometry_type(intersection_roads) != "POINT", ]
    
    # Debugging: Print filtered_roads
    #print(filtered_roads)
    
    # Cast non-POINT geometries to LINESTRINGs
    casted_roads <- st_cast(filtered_roads, "LINESTRING")
    
    # Debugging: Print casted_roads
    #print(casted_roads)
    
    # Lixelize roads
    lixels <- lixelize_lines(casted_roads, 700, mindist = 350)
    
    # Extract samples
    samples <- lines_center(lixels)
    
    # Intersect with bin locations
    origin <- st_intersection(binlocation, filtered_mpsz)
    
    # Plot the map
    tmap_mode('view')
    tm_basemap("OpenStreetMap") +
      tm_shape(casted_roads) +
      tm_lines(col = "red") +
      tm_shape(origin) + 
      tm_dots()
  })
  
  output$networkPlot <- renderTmap({
    # Debugging: Print selected area
    print(input$area)
    
    # Filter mpsz based on selected area
    filtered_mpsz <- mpsz %>%
      filter(PLN_AREA_N == input$area) %>%
      st_union() %>%
      st_make_valid() %>%
      st_transform(crs = 3414)
    
    # Debugging: Print filtered mpsz
    #print(filtered_mpsz)
    
    # Perform intersection with roads_in_singapore based on selected area
    intersection_roads <- st_intersection(roads_in_singapore, filtered_mpsz)
    
    # Debugging: Print intersection_roads
    #print(intersection_roads)
    
    # Filter out POINT geometries
    filtered_roads <- intersection_roads[st_geometry_type(intersection_roads) != "POINT", ]
    
    # Debugging: Print filtered_roads
    #print(filtered_roads)
    
    # Cast non-POINT geometries to LINESTRINGs
    casted_roads <- st_cast(filtered_roads, "LINESTRING")
    
    # Debugging: Print casted_roads
    #print(casted_roads)
    
    # Lixelize roads
    lixels <- lixelize_lines(casted_roads, 700, mindist = 350)
    #print(lixels)
    
    # Extract samples
    samples <- lines_center(lixels)
    #print(samples)
    
    # Intersect with bin locations
    origin <- st_intersection(binlocation, filtered_mpsz)
    #print(origin)
    
    # Compute network kernel
    densitiesMe <- nkde(casted_roads, 
                           events = origin,
                           w = rep(1,nrow(origin)),
                           samples = samples,
                           kernel_name = "quartic",
                           bw = 300, 
                           div= "bw", 
                           method = "simple", 
                           digits = 1, 
                           tol = 1,
                           grid_shape = c(1,1), 
                           max_depth = 8,
                           agg = 5, #we aggregate events within a 5m radius (faster calculation)
                           sparse = TRUE,
                           verbose = FALSE)
    
    # Create a dataframe for the densities
    samples$density <- densitiesMe
    lixels$density <- densitiesMe
    
    # rescaling to help the mapping
    samples$density <- samples$density*1000
    lixels$density <- lixels$density*1000
    
    # Plot the map
    tmap_mode('plot')
    tm_shape(lixels)+
      tm_lines(col="density")+
      tm_shape(origin)+
      tm_dots()
  })
  
  output$lisamap <- renderTmap({
    map_type = input$map_type
    
    map <- switch(map_type,
                  "CartoDB" = "CartoDB.Positron",
                  "Openstreetmap" = "OpenStreetMap",
                  "ESRI" = "Esri.WorldTopoMap")
    
    # Get selected kernel name from input
    bin_type <- input$fill_var
    
    
    # Define bandwidth using selected kernel
    lisa_data <- switch(bin_type,
                        "Blue Bins" = readRDS("Data/lisa/lisa_blue.rds"),
                        "E-Waste Bins" = readRDS("Data/lisa/lisa_ew.rds"),
                        "Incentive Bins" = readRDS("Data/lisa/lisa_in.rds") 
    )
    
    category <- input$category
    cat <- switch(category,
                  "Mean" = "mean",
                  "Median" = "median",
                  "Pysal" = "pysal")    
    
    thresh = input$threshold
    
    # Filter data based on threshold value
    lisa_sig <- reactive({
      req(lisa_data) 
      data <- lisa_data  
      data %>% 
        filter(p_ii < thresh)
    })
    
    # Render map
    tmap_mode("plot")+
      tm_basemap(map) +
      tm_shape(lisa_data) +
      tm_polygons(alpha = 0.1) +
      tm_borders(alpha = 0.2) +
      tm_shape(lisa_sig()) + 
      tm_fill(cat, alpha = 0.7) +  
      tm_borders(alpha = 0.4)+
      tm_view(set.view = 11, set.zoom.limits = c(11,15))
  })
  
  output$vhcsa <- renderTmap({
    map_type = input$map_var
    
    map <- switch(map_type,
                  "CartoDB" = "CartoDB.Positron",
                  "Openstreetmap" = "OpenStreetMap",
                  "ESRI" = "Esri.WorldTopoMap")
    
    # Get selected kernel name from input
    bin_type <- input$bin_var
    
    
    # Define bandwidth using selected kernel
    hcsa_data <- switch(bin_type,
                        "Blue Bins" = readRDS("Data/hsca/bluebins_hs.rds"),
                        "E-Waste Bins" = readRDS("Data/hsca/ewbins_hs.rds"),
                        "Incentive Bins" = readRDS("Data/hsca/inbins_hs.rds") 
    )
    
    sig = input$sig_lvl
    
    # Filter data based on significant level
    hcsa_sig <- reactive({
      req(hcsa_data) 
      data <- hcsa_data  
      data %>% 
        filter(p_sim < sig)
    })
    
    # Render map
    tmap_mode("plot")
    tm_basemap(map) +
      tm_shape(hcsa_data) +
      tm_polygons(alpha = 0.1) +
      tm_borders(alpha = 0.5) +
      tm_shape(hcsa_sig()) + 
      tm_fill("gi_star", alpha = 0.7) + 
      tm_borders(alpha = 0.4) +
      tm_view(set.view = 11, set.zoom.limits = c(11,15))
  })
  
  
  
  
  
  # Home page UI
  output$projectMotivation <- renderUI(HTML("<h4> In today’s technological advancing world, there are many useful and interesting spatial data sources that exist in the forms of Geospatial and Aspatial format. Geographical Geospatial data sets the foundation based on the geographical boundary locations and the Aspatial data are the records of observation that can be further prepared to be used to derive meaningful insights. 
                                              </h4> 
                                              <h4>Despite all the data sources out on the interweb, there are not many people who are knowledgeable and trained to perform such analysis. Without the fundamental knowledge and training involved, any results based on the analysis performed could result in inaccuracies.
                          Our group attempt is to mainly focus on performing analysis and develop a website based geographical spatial tool. R Shiny tool will be used with regards to developing the 1st/2nd Order & Network Constrained Point Pattern Analysis of Melbourne City, Australia.<h4>"))
  
  output$ppancppa <- renderUI(HTML("<h4> Point Pattern Analysis methods helps provide insights about where things occur, how the distribution of incidents or the arrangement of data aligns with other features in the landscape, and what the patterns may reveal about potential connections and correlations. <h4>
                                     <h4> Network constrained Spatial Point Patterns Analysis (NetSPAA) is a collection of spatial point patterns analysis methods special developed for analysing spatial point event occurs on or alongside network. The spatial point event can be locations of childcare centre for example. The network, on the other hand can be a road network or river network. <h4>"))
  
  output$aboutus <- renderUI(HTML("<h4> Spatial Bros is created to assist non technologically savvy users in performing geographical point pattern 
                             analysis. This application aims to assist users in 2 types of analysis, particularly in performing 1st/2nd Order & Network Constrained Spatial Point 
                             Pattern Analysis. For each of the analyses, the application will provide users with statistical functions, kernel density heat map estimation, various mappings and G&K function results. 
                             The application will cover an array of spatial points located in Melbourne City such as childcare centres, business establishments, famous landmarks including places of interest such as schools, theaters, health service, 
                             sports facilities, drinking fountains and public toilets. The spatial points will work in conjunction to cover areas of the city’s road, pedestrian and tram network. From this application, users would be able to perform types 
                             of hypothesis testing that allow them to generate insights towards their conclusion on the distribution along the spatial points along the network. </h4>"))
  
  output$netKDEExpaliner <- renderUI(HTML("
                                            <h3>Network Kernel Density Estimation Map</h3>
                                            <hr>
                                            <p>Network Constrained Spatial Point Pattern Analysis analyses point pattern events that happens alongside a network. We provide several point pattern (such as Business Establishment, Drinking Fountains) and Network (such as Pedestrian or Road) options to explore the effects of spatial point patterns and densities surrounding networks. These could be used to investigate the density of point patterns along networks, such as the amount of drinking fountains along pedestrian rotues to inform the planning and installing of more drinking fountains.</p>
                                            <h3> To begin your analysis, you can start by </h3>
                                            <ol>
                                              <li>Select your choice of locality</li>
                                              <li>Select your choice of network</li>
                                              <li>Select your location of interest and subcategories/theme if required</li>
                                              <li>Select your lixel length - we recommend you to start with 500 metres</li>
                                              <li>Select your minimum lixel length - we recommend you to start with 250 metres</li>
                                              <li>Select your kernel of choice</li>
                                              <li>Select your method of choice</li>
                                              <li>Click on 'Generate KDE Map' and you are ready to go!</li>
                                            </ol>
                                            <hr>
                                            <h3>Interpeting the Results</h3>
                                            <hr>
                                            <img src='nkde_example.png' height='400'>
                                            <br>
                                            <p> A legend will be shown at the top right side of the map. The colour shade intensity of the network will get darker if there is a higher relative density of spatial points specified (location of interest).</p>
                                            <p> On contrary, if the colour shade intensity of the network is lighter, it represents a lower relative density alongside the network</p>
                                            <p> The 'NetKDE Bandwidth Selection' tells us what bandwidth has been selected by the algorithm for bandwidth range between 100 and 900, in steps of 20. The goal is to find the highest possible Cross Validation (CV) score. The larger the bandwidth, the increased amount of smoothing, hence, the CV score has been capped at 900 to reduce the amount of detail lost.
                                            <h3>Key Function FAQ</h3>
                                            <hr>
                                            <h4>Lixels</h4>
                                            <img src='nKDElixels.png' height='300'>
                                            <p>Image credit to <a href='https://jeremygelb.github.io/spNetwork/articles/NKDE.html'>spNetwork</a></p>
                                            <p>Lixels are point samples along existing network lines to calculate the density of points near the region. 'Length of Lixel' defines the typical length between such point and 'Min. Lixel Length' defines the minimum if the typical length cannot be fufilled.</p>
                                            <h4>Kernel Density Estimation Methods</h4>
                                            <p>An inforgraphic has been prepared below:</p>
                                            <img src='KernelDensity.png' width='50%'>
                                                                                  "))
  output$netStatsExplainerp1 <- renderUI(HTML("
    <h3> Statistical Functions - G & K </h3>
    <hr>
    <img src='gkpicture.png' height='300'>
    <p>The K-function is a method used in spatial Point Pattern Analysis (PPA) to inspect the spatial distribution of a set of points. It allows the user to assess if the set of points is more or less clustered that what we could expect from a given distribution. </p>
    <p> Most of the time, the set of point is compared with a random distribution.
    The empirical K-function for a specified radius r is calculated with the following formula listed <a href ='https://cran.r-project.org/web/packages/spNetwork/vignettes/KNetworkFunctions.html'> here </a> </p>
    <p> Basically, the K-function calculates for a radius r the proportion of cells with a value below r in the distance matrix between all the points Dij. In other words, the K-function estimates the average number of neighbours of a typical random point </p>
    <p> A modified version of the K-function is the G-function (Pair Correlation Function). The regular K-function is calculated for subsequent disks with increasing radii and thus is cumulative in nature. The G-function uses rings instead of disks and permits the analysis of the points concentrations at different geographical scales. </p>"))
  
  
  output$netStatsExplainerp2 <- renderUI(HTML("<h3> To begin your analysis, you can start by:</h3>
    <ol>
      <li>Select your choice of network</li>
      <li>Select your location of interest</li>
      <li>Select your start value in (metres). We will recommend you to start with 0 to begin.</li>
      <li>Select your end value in (metres). We will recommend you to end with 500 metres to begin.</li>
      <li>Select your number of simulations. We will recommend you to start with 50 simulations to begin.</li>
      <li>Select your aggregate value. We will recommend you to start with 0 to begin.</li>
      <li>Click on 'Generate Statistical Results' and you are ready to go!</li>
    </ol>
    <h3>Interpreting the Results</h3>
    <hr>
    <img src='g&kexample.png' height='600'>
        <br>
    <p>Observed values - Black Line</p>

    <p>Upper plot - K function, lower plot - G function, the plot is interactive, you may mouse over at various points in the graph to inspect the exact values </p>
    <p>Hypothesis:</p>
    <p>H0: The distribution of spatial points are randomly distributed</p>
    <p>H1: The distribution of spatial points are not randomly distributed</p>
    <br>
    <p> The grey area represents the function ‘envelope’. The ‘blue’ line represents the empirical function value </p>
    <p> In the event if the observed value is above the envelope, we can reject the null hypothesis (H0) as the value is statistically significant. We can conclude that the spatial points resemble a <b>clustered distribution<b?</p>
    <p> In the event if the observed value is below the envelope, we can reject the null hypothesis (H0) as the value is statistically significant. We can conclude that the spatial points resemble a <b>dispersed distribution</b> </p>
    <p> On contrary, if the observed value is inside the envelope, we cannot reject the null hypothesis (H1) as the value is not statistically significant. We can conclude the spatial points resemble a <b>random distribution</b></p>
    <p> Note: the distances relates to the distance at which the spatial points exhibits a certain pattern</p>
    <h3>Key Function FAQ</h3>
    <hr>
    <h4>Start/End</h4>
    <p>Distances for statistical analysis to be run and plotted</p>
    <h4>Number of Simulations</h4>
    <p>How many simulations to run the statistical analysis. The more simulations, the more accurate the results will be.</p>
    <h4>Aggregate Value</h4>
    <p>Points within that radius will be aggregated (in metres)</p>
    <p>o - Null (no aggregation) | >0 - Aggregation of points</p>
                                                "))
  
  output$introductiondescription <- renderUI(HTML(
    "<h4> You will be able to perform network constrained spatial point patterns analysis methods special developed for analysing spatial point event occurs on or alongside network for City of Melbourne, Australia! </h4>
    <h4> There are 2 types of analysis that you can perform</h4>
      <ol> 
        <li> Network Kernel Density Estimation </li>
        <li> G & K Function Analysis </li>
      </ol>
    <h4> For each of the analysis, we offer you the options of selecting </h4>
      <ol> 
        <li> Road Network </li>
        <li> Pedestrian Network </li>
        <li> Tram Network </li>
      </ol>
    <h4> In addition you are allowed to pick your location of interest such as</h4>
      <ol> 
        <li> Childcare Centres </li>
        <li> Business Establishments </li>
        <li> Drinking Fountains </li>
        <li> Landmarks </li>
        <li> Public Toilets </li>
      </ol>
    <h3> Benefits of performing Network Constrained Point Pattern Analysis </h3>
    <hr>
        <ol> 
          <li> Accurate analysis: Network Constrained Point Pattern Analysis provides more accurate results compared to traditional point pattern analysis because it accounts for the underlying transportation network. This is particularly important in areas where the transportation network is dense and complex. </li> <br>
          <li> Better decision-making: Network Constrained Point Pattern Analysis can provide insights into how the network infrastructure affects the spatial distribution of points, which can be valuable for decision-making related to urban planning, transportation planning, and public policy </li> <br>
          <li> Improved resource allocation: Network Constrained Point Pattern Analysis can help optimize the allocation of resources, such as improving the accessibility to more drinking fountains/public toilets, by identifying areas with high concentrations of points and areas that are more accessible by the transportation network. </li>
      </ol>
"))
  
  output$IntroData <- renderUI(HTML(
    "<h3> Welcome to Data Exploration</h3>
      <hr>
      <p> Here, you can explore the network and spatial point datasets in the City of Melbourne included in the interaction application.</p>
      <h3>Included datasets</h3>
      <hr>
      <h4>Network</h4>
      <ul>
        <li>Road Network - City of Melbourne</li>
        <li>Pedestrian Network - City of Melbourne</li>
        <li>Tram Network - City of Melbourne</li>
      </ul>
      <h4>Spatial Points</h4>
      <ul>
        <li>Childcare Centres</li>
        <li>Business Establishments - Sub-categories by Industry Name</li>  
        <li>Drinking Fountains</li>
        <li>Landmarks - Themes</li>
        <li>Public Toilets</li>
      </ul>
      <h3> To start off:</h3>
      <hr>
    <ol>
      <li>Select the Network / Spatial Points tab to explore network or spatial points</li>
      <li>Select the localities</li>
      <li>Select the location of interest and any specific themes or sub-categories if necessary</li>
      <li>Click on 'Generate Map and Data Table' and you are ready to go!</li>
    </ol>
                                                "))    
  
  output$introkde <- renderUI(HTML(
    "<h4> You will be able to perform spatial point patterns analysis methods special developed for analysing spatial point event occurs on or alongside network for City of Melbourne, Australia! </h4>
    <p> There are 2 types of analysis that you can perform </p>
      <ol> 
        <li> Kernel Density Estimation </li>
        <li> G & K Function Analysis </li>
      </ol>
    <p> For each of the analysis, we offer you the options of selecting </p>
      <ol> 
        <li> Childcare Centres </li>
        <li> Business Establishments </li>
        <li> Drinking Fountains </li>
        <li> Landmarks </li>
        <li> Public Toilets </li>
      </ol>
    <h3> Benefits of performing Spatial Point Pattern Analysis </h3>
    <hr>
    <p> 
        <ol> 
          <li>Statistical benefits: Spatial Point Pattern Analysis helps to identify and statistically conclude the underlying spatial patterns in data and detect clustering or disperson of points over traditional point pattern analysis. </li> <br>
          <li> Better decision-making: Spatial Point Pattern Analysis can provide insights of the spatial distribution of points, which can be valuable for decision-making related to urban planning, and public policy </li> <br>
      </ol>
    </p>"))
  
  
  output$KDEstats <- renderUI(HTML("
    <h3>2nd Order Spatial Point Pattern Analysis and Statistical Functions - G & K </h3>
    <hr>
    <p>2nd Spatial Point Pattern Analysis analyses effects of interaction between point pattern events.</p>
    <img src='gkpicture.png' height='300'>
    <p>The G / K-function is a method used in spatial Point Pattern Analysis (PPA) to inspect the spatial distribution of a set of points. It allows the user to assess if the set of points is more or less clustered that what we could expect from a given distribution. </p>
    <p> Most of the time, the set of point is compared with a random distribution.
    The empirical K-function for a specified radius r is calculated with the following formula listed: <a href ='https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/Gest'> G Function </a> <a href ='https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/Kest'> K Function </a> </p>
    <p> Basically, the K-function calculates for a radius r the proportion of cells with a value below r in the distance matrix between all the points Dij. In other words, the K-function estimates the average number of neighbours of a typical random point </p>
    <p> A modified version of the K-function is the G-function (Pair Correlation Function). The regular K-function is calculated for subsequent disks with increasing radii and thus is cumulative in nature. The G-function uses rings instead of disks and permits the analysis of the points concentrations at different geographical scales. </p>
      <ol>
      <li>Select your location of interest</li>
      <li>Select your start value in (metres). We will recommend you to start with 0 to begin.</li>
      <li>Select your end value in (metres). We will recommend you to end with 500 metres to begin.</li>
      <li>Select the confidence level to perfom the statistical testing.</li>
      <li>Click on 'Generate Analysis' and you are ready to go!</li>
    </ol>
    <h3>Interpreting the Results</h3>
    <hr>
    <img src='gkKDEeg.png' height='600'>
    <br>
    <p>Observed values - Black Line</p>
    <p>Upper plot - K function, lower plot - G function, the plot is interactive, you may mouse over at various points in the graph to inspect the exact values </p>
    <p>Hypothesis:</p>
    <p>H0: The distribution of spatial points are randomly distributed</p>
    <p>H1: The distribution of spatial points are not randomly distributed</p>
    <br>
    <p> The grey area represents the function ‘envelope’. The ‘blue’ line represents the empirical function value </p>
    <p> In the event if the observed value is above the envelope, we can reject the null hypothesis (H0) as the value is statistically significant. We can conclude that the spatial points resemble a <b>clustered distribution<b?</p>
    <p> In the event if the observed value is below the envelope, we can reject the null hypothesis (H0) as the value is statistically significant. We can conclude that the spatial points resemble a <b>dispersed distribution</b> </p>
    <p> On contrary, if the observed value is inside the envelope, we cannot reject the null hypothesis (H1) as the value is not statistically significant. We can conclude the spatial points resemble a <b>random distribution</b></p>
    <p> Note: the distances relates to the distance at which the spatial points exhibits a certain pattern</p>
    <h3>Key Function FAQ</h3>
    <hr>
    <h4>Start/End</h4>
    <p>Distances for statistical analysis to be run and plotted</p>
    <h4>Confidence Level</h4>
    <p>How many simulations to run the statistical analysis. The number of simulations are mapped as follows: </p>
    <p>95% - 39 | 99% - 199 | 99.9% - 1999</p>
    <p>Given by the following formula: alpha = 2 * nrank / (1 + nsim) where nrank = 1</p>
    <p>95% and 99% are typical confidence levels used</p> 
                                     "))
  
  output$sppastuff <- renderUI(HTML("
                                            <h3>Kernel Density Estimation Map and 1st Order Spatial Point Pattern Analysis</h3>
                                            <hr>
                                            <p>1st Spatial Point Pattern Analysis analyses point pattern events and its effects with the environment. We provide several point pattern (such as Business Establishment, Drinking Fountains) options to explore the effects of spatial point patterns and densities. These could be used to investigate the density of point patterns, such as the amount of drinking fountains to inform the planning and installing of more drinking fountains.</p>
                                            <h3> To begin your analysis, you can start by </h3>
                                            <ol>
                                              <li>Select your choice of locality</li>
                                              <li>Select your location of interest and subcategories/theme if required</li>
                                              <li>Select your kernel of choice</li>
                                              <li>Select your method of choice</li>
                                              <li>Click on 'Generate KDE Map' and you are ready to go!</li>
                                            </ol>
                                            <h3>Interpeting the Results</h3>
                                            <hr>
                                            <img src='sppa.png' height='600'>
                                            <br>
                                            <p> A legend will be shown at the top right side of the map. The colour shade intensity of the network will get darker if there is a higher relative density of spatial points specified (location of interest).</p>
                                            <p> On contrary, if the colour shade intensity of the network is lighter, it represents a lower relative density alongside the network</p>
                                            <p> The 'Clark and Evans Test' is a nearest neighbour test to analyse and statistically conclude point pattern events and its effects with the environment.
                                            <br>
                                            <p>Hypothesis:</p>
                                            <p>H0: The distribution of spatial points are randomly distributed</p>
                                            <p>H1: The distribution of spatial points are not randomly distributed</p>
                                            <br>
                                            <p> If the p-value is less than the alpha of the confidence selected (ie. alpha will be 0.05 if confidence selected is 95%), we reject H0 (null hypothesis) that the spatial points are randomly distributed</p>
                                            <p> If the p-value is more than the alpha of the confidence selected (ie. alpha will be 0.05 if confidence selected is 95%), we cannot reject H0 (null hypothesis) that the spatial points are randomly distributed</p>
                                            <p> If H0 is rejected:</p>
                                            <p> In the event if the R < 1, we We can conclude that the spatial points resemble a <b>clustered distribution<b?</p>
                                            <p> In the event if the R > 1, we can conclude that the spatial points resemble a <b>dispersed distribution</b> </p>
                                            <h3>Key Function FAQ</h3>
                                            <hr>
                                            <h4>Confidence Level</h4>
                                            <p>How many simulations to run the statistical analysis. The number of simulations are mapped as follows: </p>
                                            <p>95% - 39 | 99% - 199 | 99.9% - 1999</p>
                                            <p>Given by the following formula: alpha = 2 * nrank / (1 + nsim) where nrank = 1</p>
                                            <p>95% and 99% are typical confidence levels used</p> 
                                            
                                            <h4>Kernel Density Estimation Methods</h4>
                                            <h5>Bandwidth Type</h5>
                                            <p>Fixed: Appropriate bandwidth will be selected by algorithm to generate Kernel Density Estimate</p>
                                            <p>Adaptive: Appropriate bandwidth will be selected by algorithm to generate Kernel Density Estimate. Adaptive kernel is suitable to provide a smoother estimate when dealing with varying spatial point distributions. An example could be urban vs rural typologies where urban may have more spatial points over rural.</p><br>
                                            <img src='sppakernel.png' width='50%'>
                                                                                  "))
  
  output$introductionhcsa <- renderUI(HTML("
                                           <p>Welcome to an exploration of hotspot and coldspot analysis in Singapore's efforts towards better waste management, specifically focusing on the placement of recycling bins. In this guide, we aim to shed light on how this analytical approach helps optimize the locations of recycling bins across our city-state, ensuring that they are where they're needed most.</p><br>
                                           <h4>What are Hotspot and Coldspot Analysis?</h4>
                                           <p>Hotspot and coldspot analysis might sound technical, but the concept is quite simple. It's about identifying areas where something is happening a lot (hotspots) or not happening much at all (coldspots). Think of it as shining a spotlight on where things are bustling with activity and where they're not.</p>
                                           <br><h4>Why Does it Matter for Recycling Bins?</h4>
                                           <p>When it comes to recycling, it's essential to have bins where they can make the most impact. Hotspot and coldspot analysis help us figure out exactly where these bins should go:</p>
                                           <ul>
                                             <li><b>Hotspots</b>: These are areas bustling with people and activities, like shopping malls, residential neighborhoods, or office districts. Placing recycling bins in hotspots ensures that they're easily accessible to many people, encouraging more recycling.</li>
                                             <li><b>Coldspots</b>: On the other hand, coldspots are areas where recycling might not be happening much, perhaps because there aren't enough bins or people aren't aware of recycling options. Identifying these areas helps us focus on improving recycling rates by adding bins or raising awareness.</li>
                                           </ul>
                                           <br><p>Hotspot and coldspot analysis might sound like complicated concepts, but they're powerful tools in our mission towards a greener, more sustainable Singapore. By understanding where recycling bins should go, we're taking steps to make recycling easier and more accessible for everyone. So the next time you toss a can or a bottle into a recycling bin, know that it's part of a larger effort guided by data to create a cleaner, healthier environment for us all.</p>"))
  
  output$lisaExplainer <- renderUI(HTML("
                                           <br><br><h1>Guide to Understanding and Interpreting LISA Map for R Programming Shiny App</h1>
                                          <br><p>The Local Indicators of Spatial Association (LISA) map is a powerful tool for analyzing spatial patterns and identifying outliers and clusters within geographical areas. When integrated into a Shiny app using R programming, it becomes a dynamic and interactive tool for visualizing spatial relationships. Here's a step-by-step guide to understanding and interpreting the LISA map:</p>
                                          
                                          <h2>Understanding LISA Map</h2>
                                          <br><p>The LISA map is a categorical map that displays spatial clusters and outliers based on statistical analysis. It identifies four types of spatial patterns:</p>
                                          <ul>
                                            <li><strong>High-High Clusters:</strong> Areas with high attribute values surrounded by neighboring areas with high values.</li>
                                            <li><strong>Low-Low Clusters:</strong> Areas with low attribute values surrounded by neighboring areas with low values.</li>
                                            <li><strong>High-Low Outliers:</strong> Areas with high attribute values surrounded by neighboring areas with low values.</li>
                                            <li><strong>Low-High Outliers:</strong> Areas with low attribute values surrounded by neighboring areas with high values.</li>
                                          </ul>
                                          
                                          <h2>Interpreting Outliers and Clusters</h2>
                                          <p>Each type of outlier or cluster indicates a different spatial pattern:</p>
                                          <ul>
                                            <li><strong>High-High Clusters:</strong> These areas represent significant clusters of high attribute values. They could indicate areas of concentrated development, economic activity, or other factors depending on the specific attributes being analyzed.</li>
                                            <li><strong>Low-Low Clusters:</strong> Conversely, low-low clusters indicate areas with consistently low attribute values. These could represent areas of low development, deprivation, or other factors.</li>
                                            <li><strong>High-Low Outliers:</strong> High-low outliers are areas with unexpectedly high attribute values compared to their neighbors. These could signify areas of rapid development or unique characteristics compared to their surroundings.</li>
                                            <li><strong>Low-High Outliers:</strong> Conversely, low-high outliers have low attribute values surrounded by areas with high values. These outliers could represent areas with potential for improvement or areas undergoing decline.</li>
                                          </ul>
                                          
                                          <h2>Significance Level</h2>
                                          <br><p>The significance level, often denoted as alpha (&alpha;), determines the threshold for identifying statistically significant spatial patterns. A commonly used significance level is 0.05, indicating a 5% chance of observing the detected spatial pattern under the null hypothesis of spatial randomness.In our case, we allow interaction of up to 20%, meaning that spatial patterns with a p-value less than or equal to 0.2 are considered statistically significant.</p>
                                          
                                          <br><h4>By following this guide, users can effectively interpret and utilize LISA maps within R programming Shiny apps to gain insights into spatial patterns, clusters, and outliers within their geographical datasets, while considering the significance level for identifying statistically meaningful results.</h4>
                                        "))
  
  output$hcsaExplainer <- renderUI(HTML("
                                           <br><br><h1>Guide to Understanding Hotspot and Coldspot Areas</h1>
                                            <br><p>Hotspot and coldspot areas refer to regions within a particular dataset or geographical area that exhibit unusually high (hotspot) or low (coldspot) values of a certain attribute or characteristic. These areas can provide valuable insights into various phenomena, ranging from socioeconomic trends to environmental conditions. One common method used to identify hotspot and coldspot areas is the Gi* statistic, which measures spatial clustering of values. Here's a guide to help you understand how to read and interpret hotspot and coldspot areas, focusing on the Gi* statistic:</p>
                                            <h2>What is the Gi* statistic?</h2>
                                            <br><p>
                                              The Gi* statistic, also known as the Getis-Ord Gi* statistic, is a measure of spatial autocorrelation used to identify clustering of high or low attribute values within a dataset. It calculates whether high or low values of a variable are clustered together in space or if they are randomly distributed.
                                            </p>
                                          
                                            <h2>Interpreting Gi* Values:</h2>
                                            <br><ul>
                                              <li>
                                                Positive values indicate clustering of high values (hotspots), while negative values indicate clustering of low values (coldspots).
                                              </li>
                                              <li>
                                                A value of 0 indicates random spatial distribution.
                                              </li>
                                              <li>
                                                The higher the absolute value of Gi*, the stronger the clustering.
                                              </li>
                                            </ul>
                                          
                                            <h2>Benefits of Understanding Hotspot and Coldspot Areas:</h2>
                                            <br><ul>
                                               <li>
                                                  <strong>Identifying Hotspot and Coldspot Areas for Recycling Bin Placement:</strong> Hotspot and coldspot analysis can help understand where there is a scarcity of recycling bins, especially in densely populated areas, guiding the placement of recycling bins for optimal usage.
                                                </li>
                                                <li>
                                                  <strong>Optimizing Waste Management Efforts:</strong> By analyzing hotspot and coldspot areas of recycling bin usage, authorities can optimize waste management efforts, ensuring efficient collection and recycling processes.
                                                </li>
                                                <li>
                                                  <strong>Promoting Environmental Sustainability:</strong> Understanding hotspot and coldspot areas of recycling bin usage facilitates targeted educational campaigns and initiatives to promote recycling and environmental sustainability practices in Singapore.
                                                </li>
                                                <li>
                                                  <strong>Evaluating Policy Effectiveness:</strong> Hotspot and coldspot analysis allows policymakers to evaluate the effectiveness of existing recycling policies and interventions, guiding future policy formulation and resource allocation.
                                                </li>
                                            </ul>
                                            
                                            <br><h4>Understanding hotspot and coldspot areas through the Gi* statistic empowers decision-makers to make informed choices, allocate resources effectively, and address spatial disparities in various domains. By following this guide, you can leverage spatial analysis techniques to uncover valuable insights within your dataset or geographical area of interest.</h4>
                                        "))
  
  output$edadescription <- renderUI(HTML("
                                         <br><br><h2>Exploring Recycling Bin Distribution in Singapore</h2>
                                          <br><h5>Welcome to the EDA (Exploratory Data Analysis) tab! This section offers an intuitive interface for delving into the distribution of recycling bins across Singapore. Here's a quick guide on how to make the most of this resource:</h5>
                                          <br><ul>
                                              <li>
                                                  <p><strong>Explore Different Bin Types:</strong></p>
                                                  <p>Utilize the interactive map or dropdown menus to visualize the distribution of various recycling bins, including blue bins for general waste, e-waste bins, and incentive bins.</p>
                                              </li>
                                              <li>
                                                  <p><strong>Understand Population Distribution:</strong></p>
                                                  <p>Gain insights into the population distribution across Singapore by examining the population density map combined with the distribution plot.</p>
                                              </li>
                                              <li>
                                                  <p><strong>Analyze Population-Density Relationship:</strong></p>
                                                  <p>Investigate how population density correlates with the distribution of recycling bins, pinpointing areas with high population density but limited recycling infrastructure.</p>
                                              </li>
                                              <li>
                                                  <p><strong>Informed Decision Making:</strong></p>
                                                  <p>Use the insights garnered from this EDA tool to make well-informed decisions regarding the placement and optimization of recycling infrastructure.</p>
                                              </li>
                                              <li>
                                                  <p><strong>Targeted Interventions:</strong></p>
                                                  <p>Identify areas characterized by low recycling bin density despite high population density, enabling targeted interventions to enhance recycling accessibility and foster sustainable waste management practices.</p>
                                              </li>
                                          </ul>
                                          <br><h5>Embark on your exploration of the EDA tab now to uncover valuable insights into recycling bin distribution and population density dynamics in Singapore. Your contributions will play a vital role in cultivating a more environmentally conscious and resilient city-state!</h5>
                                         "))
  
}
  
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)