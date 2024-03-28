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
library(tidyr)

mpsz <- st_read(dsn = "testdata", layer = "MPSZ-2019")
binlocation <- read_rds("Data/alba/ewbins.rds")
roads_in_singapore <- read_rds("testdata/sgRoad.rds")
inbins <- read_rds("Data/nea/inbins.rds")
sg_sf <- st_read(dsn = "testdata", layer = "CostalOutline")

# Define UI for application
ui <- fluidPage(theme = shinytheme("cosmo"),
                tags$head(
                  tags$style(
                    HTML(
                      "
                      .navbar-brand img {
                        margin-top: -5px; /* Adjust the top margin as needed */
                        margin-right: 5px; /* Adjust the right margin as needed */
                      }
                      "
                    )
                  )
                ),
                navbarPage(                                                
                  title = div(
                    a(
                      img(src = "recyclesglogo.png", height = "38px", width = "100%", style = "margin-left: 10px;"),  # Add your image here
                      href = "https://is415-gaa-jkw.netlify.app/"
                    )
                  ),
                  tags$head(tags$style(
                    type="text/css",
                    " 
                    
                    "
                  )),
                  tabPanel("Home Page",
                           fluidRow(
                             column(9,
                                    h2("Project Introduction & Motivation"),
                                    hr(),
                                    column(10,
                                           uiOutput("projectMotivation"),                         
                                    ),
                             ),
                             column(3,
                                    h1("RecycleSG"),
                                    uiOutput("myList"),
                             ),
                             
                           ),
                           fluidRow(
                             column(12,
                                    h2("What is First-order Spatial Point Patterns Analysis and Hot Spot and Cold Spot Area Analysis (HCSA)?"),
                                    hr(),
                                    uiOutput("ppancppa")),
                           ),
                           fluidRow(
                             column(12,
                                    h2("About RecycleSG"),
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
                  tabPanel("First-order Spatial Point Patterns Analysis",
                           titlePanel("First-order Spatial Point Patterns Analysis"),
                           hr(),
                           tabsetPanel(type = "tabs",
                                       tabPanel("Introduction",
                                                fluidRow(
                                                  column(6,
                                                         h2("Welcome to the First-order Spatial Point Patterns Analysis!"),
                                                         hr(),
                                                         uiOutput("introductiondescription")                         
                                                         
                                                  ),
                                                  column(6,
                                                         h2(),
                                                         imageOutput("introductionKernel")),
                                                ),
                                       ),  
                                   
                                       tabPanel("Kernel Density Estimation", 
                                                sidebarLayout(
                                                  mainPanel(
                                                    tmapOutput("kdePlot", width = "100%", height = "700"),
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
                                                    h3("NetKDE Plot"),
                                                    tmapOutput("roadBinPlot", width = "100%", height = "700")
                                                  ),
                                                  sidebarPanel(
                                                    shinyjs::useShinyjs(),
                                                    h4("Network Constrained KDE Analysis"),
                                                    h5("Select the type of bin to be displayed on the map"),
                                                    selectInput(inputId = "bin_type_ws",
                                                                label = "Select Type of Bin",
                                                                choices = list("Blue Bins" = "Blue Bins",
                                                                               "E-Waste Bins" = "E-Waste Bins")),
                                                    h5("Select the area to perform the analysis"),
                                                    selectInput(inputId = "area",
                                                                label = "Select Area",
                                                                choices = list(#"SINGAPORE RIVER",
                                                                               #"MUSEUM",
                                                                               "MARINE PARADE",
                                                                               "DOWNTOWN CORE",
                                                                               "QUEENSTOWN",
                                                                               "OUTRAM",
                                                                               "KALLANG",
                                                                               "TANGLIN",
                                                                               #"NEWTON",
                                                                               "CLEMENTI",
                                                                               #"ORCHARD",
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
                                                                               #"SUNGEI KADUT",
                                                                               "YISHUN",
                                                                               "PUNGGOL",
                                                                               "CHOA CHU KANG",
                                                                               "SENGKANG",
                                                                               #"CENTRAL WATER CATCHMENT",
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
                  tabPanel("Hot Spot and Cold Spot Area Analysis (HCSA)",
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
    bin_ppp.km <- rescale(binSG_ppp, 1000, "km") # set to 1000 for working legend
    bin_ppp.1 <- rescale(binSG_ppp, 1, "km") # set to 1000 for working legend
    
    if(input$bandwidth_type == "Adaptive Bandwidth"){
      shinyjs::hide("bandwidth_name")
      shinyjs::hide("bandwidth_label")
      shinyjs::hide("bandwidth_header")
      shinyjs::hide("kernel_name")
      shinyjs::hide("kernel_label")
      shinyjs::hide("kernel_header")
      
      kde_origin_adaptive <- adaptive.density(bin_ppp.km, method="kernel")
      kde_origin_adaptive1 <- adaptive.density(bin_ppp.1, method="kernel")
      
      # Convert to raster
      gridded_kde_origin_adaptive <- as.SpatialGridDataFrame.im(kde_origin_adaptive)
      gridded_kde_origin_adaptive1 <- as.SpatialGridDataFrame.im(kde_origin_adaptive1)
      kde_raster <- raster(gridded_kde_origin_adaptive)
      kde_raster1 <- raster(gridded_kde_origin_adaptive1)
      projection(kde_raster) <- CRS("+init=EPSG:3414")
      projection(kde_raster1) <- CRS("+init=EPSG:3414")
      
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
      
      kde_bin_bw1 <- density(bin_ppp.1,
                             sigma = bw_sigma, 
                             edge = TRUE,
                             kernel = bw_method)
      
      # Convert to raster
      gridded_kde_bin_bw <- as.SpatialGridDataFrame.im(kde_bin_bw)
      gridded_kde_bin_bw1 <- as.SpatialGridDataFrame.im(kde_bin_bw1)
      kde_raster <- raster(gridded_kde_bin_bw)
      kde_raster1 <- raster(gridded_kde_bin_bw1)
      projection(kde_raster) <- CRS("+init=EPSG:3414")
      projection(kde_raster1) <- CRS("+init=EPSG:3414")
    }
    
    # Plot KDE contours on top of polygons
    tmap_mode("plot")
    tm_shape(kde_raster) +
      tm_raster(style = "cont", palette = "plasma") +
      tm_shape(kde_raster1) +
      tm_raster(style = "cont", palette = "plasma",legend.show = FALSE) +
      tm_view(set.view = 11, set.zoom.limits = c(11,15))+
      tm_layout(legend.show = FALSE)
  })
  
  output$roadBinPlot <- renderTmap({
    if (input$bin_type_ws == "Blue Bins"){
      binlocation <- st_read(dsn = "Data/gov", layer = "RECYCLINGBINS") %>%
        st_transform(crs = 3414)
    }
    else{
      binlocation <- readRDS("Data/alba/ewbins.rds")
    }
    
    # Filter mpsz based on selected area
    filtered_mpsz <- mpsz %>%
      filter(PLN_AREA_N == input$area) %>%
      st_union() %>%
      st_make_valid() %>%
      st_transform(crs = 3414)
    
    # Perform intersection with roads_in_singapore based on selected area
    intersection_roads <- st_intersection(roads_in_singapore, filtered_mpsz)
    
    # Filter out POINT geometries
    filtered_roads <- intersection_roads[st_geometry_type(intersection_roads) != "POINT", ]
    
    # Cast non-POINT geometries to LINESTRINGs
    casted_roads <- st_cast(filtered_roads, "LINESTRING")
    
    # Lixelize roads
    lixels <- lixelize_lines(casted_roads, 700, mindist = 350)
    #lixels <- lixelize_lines(casted_roads, lixel_size, mindist = min_dist)
    
    # Extract samples
    samples <- lines_center(lixels)
    
    # Intersect with bin locations
    origin <- st_intersection(binlocation, filtered_mpsz)
    
    print(origin)
    
    # Plot the map
    tmap_mode('view')
    tm_basemap("OpenStreetMap") +
      tm_shape(casted_roads) +
      tm_lines(col = "red") +
      tm_shape(origin) + 
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
  output$myList <- renderUI(HTML(paste0("<h4>A Group Project done by<h4>
                                   <ul>
                                     <li><a href='https://www.linkedin.com/in/wan-shen-lim-8b9827163'>Lim Wan Shen</a></li>
                                     <li><a href='https://www.linkedin.com/in/jinyuan-low/'>Low Jin Yuan</a></li>
                                   </ul>
                                   <br>
                                   <p> Access our user guide <a href = 'https://is415-gaa-jkw.netlify.app/'>here</a>.</p>
                                   <br>
                                   <p> This project is for IS415 Geospatial Analytics & Application. </p>
                                   <img src = 'smulogo.png' width = 90%, height = 90%>")))
  
  output$projectMotivation <- renderUI(HTML("<h4>In our ever-evolving technological landscape, a wealth of spatial data sources, both geospatial and aspatial, offer a treasure trove of insights waiting to be unearthed. Geospatial data, anchored in geographical boundaries, serves as the bedrock upon which our analyses are built, while aspatial data records observations ripe for deeper examination and interpretation. Despite the abundance of these data streams available online, the expertise necessary to navigate and extract meaningful insights remains a rare commodity. Without the requisite knowledge and training, analyses run the risk of being marred by inaccuracies and misinterpretations.</h4>
                                              <h4>It is within this context that our group endeavors to carve a niche, focusing on conducting meticulous analyses and crafting a user-friendly website-based geographical spatial tool. Harnessing the power of the R Shiny framework, we aim to develop robust tools tailored specifically for our case study on Singapore’s recycling bin distribution, encompassing First-order Spatial Point Patterns Analysis and Hot Spot and Cold Spot Area Analysis, thereby facilitating a deeper understanding of spatial distribution dynamics in urban environments.</h4>
                                                <h4>As urbanization accelerates globally, effective waste management becomes paramount to maintain environmental sustainability and public health. Singapore, a densely populated city-state, faces significant waste management challenges. While initiatives like recycling have been implemented, optimizing the distribution of recycling bins remains crucial for maximizing participation and minimizing waste diversion. Geospatial analytics offer a powerful tool set to analyze spatial data, optimize resource allocation, and enhance waste management strategies.
                                                  Under Singapore’s Zero Waste Masterplan and the Singapore Green Plan, Singapore aims to increase its overall recycling rate to 70 per cent by 2030. Achieving this ambitious goal necessitates a comprehensive assessment of the existing recycling infrastructure and targeted interventions to address any gaps in distribution.</h4>"))
  
  output$ppancppa <- renderUI(HTML("<h3>First-order Spatial Point Patterns Analysis</h3>
<h4>First-order Spatial Point Patterns Analysis refers to the examination and characterization of the spatial distribution of point features within a study area without considering the influence of other point patterns. It involves analyzing the arrangement of individual point locations to identify patterns such as clustering, dispersion, or randomness. Common techniques used in first-order spatial point pattern analysis include Ripley's K-function, nearest neighbor analysis, and quadrat analysis. This analysis provides fundamental insights into the underlying spatial processes and can aid in understanding the drivers behind point pattern distributions.</h4>

<h3>Hot Spot and Cold Spot Area Analysis (HCSA)</h3>
<h4>Hot Spot and Cold Spot Area Analysis (HCSA), also known as hotspot analysis or spatial clustering analysis, is a method used to identify statistically significant clusters of high or low values within a spatial dataset. It involves identifying areas where the observed values are significantly different from what would be expected under a random spatial distribution. Hot spots represent areas with high values (e.g., high population density, high crime rates), while cold spots represent areas with low values. HCSA techniques often involve the application of statistical tests, such as the Getis-Ord Gi* statistic or the Moran's I statistic, to assess the significance of identified clusters. This analysis helps in identifying spatial patterns, trends, and areas of interest, which can be valuable for decision-making, resource allocation, and targeted interventions in various fields including public health, urban planning, and environmental management.</h4>"))
  
  output$aboutus <- renderUI(HTML("<h4> RecycleSG has been developed to support users, particularly those with limited technological expertise, in conducting geographical point pattern analysis within the context of Singapore's recycling bin distribution. This application is designed to aid users in two main types of analysis, focusing specifically on First Order Spatial Point Pattern Analysis as well as Hot Spot and Cold Spot Area Analysis (HCSA). Each analysis provided by the application includes statistical functions, kernel density estimation, and various mapping functionalities. </h4>
                                  <h4> Our project aims to utilize geospatial analytics to identify optimal locations for recycling bin placement and management, empowering relevant stakeholders to make impactful decisions that benefit the environment. Our project is a collaboration between Jin Yuan and Wan Shen, two students from the IS415 Geospatial Analytics course at Singapore Management University. We aim to develop a comprehensive application that leverages geospatial analytics to display the distribution of recycling bins across Singapore. By analyzing data on population density and existing bin locations, the application will identify spatial patterns, gaps in coverage, and opportunities for improvement in the placement of normal blue recycling bins, incentive bins, and e-waste bins. The application will then provide insights based on the analysis and allow users to interactively explore the data and results. </h4>"))
  
  output$netKDEExpaliner <- renderUI(HTML("<h3>Kernel Density Estimation Map</h3>
                                            <hr>
<p>Kernel density estimation (KDE) serves as a powerful spatial analysis technique, enabling the calculation of point feature densities across an area. By employing a mathematical kernel function, KDE transforms discrete point data into a continuous surface or 'heat map,' effectively visualizing spatial distribution patterns.</p>

<p>In the presented visualization, the shaded areas, ranging from hues of blue to red, depict varying density levels derived from KDE analysis. Darker shades of blue signify heightened concentrations of the underlying point data, while lighter hues indicate lower densities. Notably, areas characterized by deep red or purple hues denote dense clusters, potentially corresponding to urban centers, population hubs, or focal points of measured phenomena such as incidents, facilities, or resources. Specifically, in the depicted chart, darker red/purple areas highlight a pronounced density or concentration of recycling bins within the region.</p>

<p>The adaptive bandwidth KDE analysis represents an evolution of traditional KDE methods. By dynamically adjusting the bandwidth size based on local point density, this approach offers a more refined depiction of spatial patterns. Unlike fixed bandwidth methods, which may oversmooth areas of high density and undersmooth regions with low density, the adaptive bandwidth approach adapts to local variations. Consequently, it accurately captures nuances in the spatial distribution, enhancing the interpretability of results.</p>

<p>Overall, the output of KDE analysis furnishes an intuitive platform for exploring significant density hotspots, gradients, and spatial trends within the study area. Leveraging the input point data, this visualization facilitates the identification of key spatial patterns, thereby informing decision-making processes across a spectrum of domains, from urban planning to resource allocation and beyond.</p>"))
  
  output$netStatsExplainerp1 <- renderUI(HTML("
    <h3> NetKDE Analysis </h3>
    <hr>
    <p>NetKDE analysis is a spatial analysis technique used to study the distribution patterns of point features (like the recycling bin locations represented by black dots) in relation to an underlying constraining network (in this case, the street network).</p>

<p>The key aspects shown in the image relevant to NetKDE Analysis are:</p>

<ul>
  <li>Point data: The black dot symbols indicate the locations of recycling bins across the study area. These serve as the point features whose spatial pattern will be analyzed.</li>
  <li>Network dataset: The map displays the street network of the downtown core, providing the linear network constraints that the point pattern analysis must account for. Real-world accessibility and movement are limited by this transportation network.</li>
</ul>

<p>By performing NetKDE analysis on the recycling bin locations constrained by the street network geometry, analysts can assess if the point patterns exhibit clustering, dispersal, or random characteristics along the network paths. This contrasts traditional spatial point pattern analysis that considers Euclidean straight-line distances.</p>

<p>Based on the chart depicted above, there are two different types of bins: the blue bins and the e-waste bins. Based on the observation, we can infer that the blue bins are more clustered compared to the e-waste bins. Therefore, it is likely that the blue bins serve a more generalized waste collection purpose, while the e-waste bins may be strategically placed at specific locations catering to the disposal of electronic waste materials.</p>

<p>NetKDE analysis results can reveal insightful patterns, such as bins clustered around certain road segments or network nodes (intersections), which could inform optimized redistribution plans adhering to the network connectivity. Overall, it enables studying recycling bin accessibility while realistically incorporating the network constraints of the urban environment.</p>"))
  
  output$introductiondescription <- renderUI(HTML(
    "<h4> You will be able to perform first-order spatial point patterns analysis for analysing spatial point event occurs on or alongside network for Singapore. </h4>
    <h4> There are 2 types of analysis that you can perform</h4>
      <ol> 
        <li> Kernel Density Estimation </li>
        <li> Network Constrained KDE (NetKDE) Analysis </li>
      </ol>
    <h4> For each of the analysis, we offer you the options of selecting </h4>
      <ol> 
        <li> Bandwidth Type </li>
        <li> Bin Type </li>
        <li> Area of Singapore </li>
      </ol>
    <h3> Benefits of performing Network Constrained Point Pattern Analysis </h3>
    <hr>
        <h4><b>Identification of Spatial Distribution</b></h2>
        <p>First-order analysis helps identify the overall spatial distribution of point features within a study area. It provides insights into whether the points are clustered, dispersed, or randomly distributed, which is fundamental for understanding underlying spatial processes.</p>
        
        <h4><b>Quantification of Spatial Patterns</b></h2>
        <p>Through statistical measures such as Ripley's K-function or nearest neighbor analysis, first-order analysis quantifies the degree of clustering or dispersion in point patterns. This allows for objective comparisons between different datasets or spatial patterns.</p>
        
        <h4><b>Detection of Spatial Trends</b></h2>
        <p>First-order analysis can reveal spatial trends or gradients in point patterns across the study area. It helps identify areas with higher or lower densities of point features, providing valuable information for spatial planning, resource allocation, and decision-making.</p>
        
        <h4><b>Insights into Spatial Processes</b></h2>
        <p>Understanding the spatial distribution and arrangement of point features is crucial for gaining insights into underlying spatial processes. First-order analysis provides a foundation for exploring spatial relationships, interactions, and dependencies among point features.</p>
        
        <h4><b>Decision Support</b></h2>
        <p>Insights gained from first-order analysis support evidence-based decision-making in various domains. By understanding the spatial arrangement of point features, decision-makers can develop more effective strategies for resource allocation, land use planning, conservation efforts, and public health interventions.</p>"))
  
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