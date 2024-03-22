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
library(maptools)
library(raster)

mpsz <- st_read(dsn = "../testdata", layer = "MP14_SUBZONE_WEB_PL")
popagsex <- read_csv("../testdata/respopagesextod2011to2020.csv")
childcare <- st_read("../testdata/PreSchoolsLocation.geojson") %>% st_transform(crs = 3414)
binlocation <- read_rds("../../Data/alba/ewbins.rds")


# Define UI for application
ui <- fluidPage(theme = shinytheme("darkly"),
                navbarPage(                                                
                  title = div(
                    a(
                      h2("RecycleSG", style = "margin-top: -8px;padding-right:10px;padding-bottom:10px"),
                      href = "https://spatialbros.netlify.app/"
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
                  # 2nd Tab
                  tabPanel("Network Constrained Point Pattern Analysis",
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
                                       tabPanel("Network Kernel Density Estimation", 
                                                sidebarLayout(
                                                  mainPanel(
                                                    tmapOutput("mapPlot", width = "100%", height = "700"),
                                                    br(),
                                                    verbatimTextOutput("netKDESettings"),
                                                  ),
                                                  sidebarPanel(
                                                    shinyjs::useShinyjs(),
                                                    h4("Network Kernel Density Estimation Variable Inputs"),
                                                    h5("Network and Spatial Points"),
                                                    selectInput(inputId = "bin_type",
                                                                label = "Select Bin Type",
                                                                choices = unique(binlocation$`Type of Bin Placed`)
                                                    ),
                                                    selectInput(inputId = "network_type",
                                                                label = "Types of Network",
                                                                choices = list("Road Network" = "net_road",
                                                                               "Pedestrian Network" = "net_ped",
                                                                               "Tram Network" = "net_tram")),
                                                    selectInput(inputId = "locs",
                                                                label = "Location of Interest",
                                                                choices = list(
                                                                  "Business Establishments" = "sf_business",
                                                                  "Childcare Centres" = "sf_childcare",
                                                                  "Drinking Fountain" = "sf_drinking_fountain",
                                                                  "Landmarks" = "sf_landmarks",
                                                                  "Public Toilets" = "sf_pub_toilets")),
                                                    selectInput(inputId = "netsublocs",
                                                                label = "Specific Themes/Sub-Categories",
                                                                choices = list()),
                                                    h5("Lixels"),
                                                    sliderInput(inputId = "lx_length", "Length of Lixel",
                                                                min = 100, max = 1500, value = 700, step = 50),
                                                    sliderInput(inputId = "lx_length_min", "Min. Lixel Length",
                                                                min = 100, max = 1500, value = 350, step = 50),
                                                    h5("Kernel Density Estimation Methods"),
                                                    selectInput(inputId = "kernel_name",
                                                                label = "Choose the kernel to be used:",
                                                                choices = list("Quartic" = "quartic",
                                                                               "Triangle" = "triangle",
                                                                               "Tricube" = "tricube",
                                                                               "Cosine" = "cosine",
                                                                               "Triweight" = "triweight",
                                                                               "Epanechnikov" = "epanechnikov",
                                                                               "Uniform" = "uniform")),
                                                    selectInput(inputId = "method_name",
                                                                label = "Select the Method to be used",
                                                                choices = list("Simple" = "simple",
                                                                               "Discontinuous" = "discontinuous",
                                                                               "Continuous" = "Continuous")),
                                                    
                                                    actionButton("netKDEGenerate", "Generate KDE Map"),
                                                    
                                                    h5("Please note: The map will take a few minutes to generate after clicking the button."),
                                                    
                                                  ),
                                                  
                                                ),
                                                uiOutput("netKDEExpaliner"),
                                                br(),
                                       ),
                                       tabPanel("Statistical Functions", 
                                                sidebarLayout(
                                                  mainPanel(
                                                    plotlyOutput("kfun"),
                                                    plotlyOutput("gfun"),
                                                  ),
                                                  sidebarPanel(
                                                    shinyjs::useShinyjs(),
                                                    h4("Statistical Function Variable Inputs"),
                                                    h5("Network and Spatial Points"),
                                                    selectInput(inputId = "bin_type",
                                                                label = "Select Bin Type",
                                                                choices = unique(binlocation$`Type of Bin Placed`)
                                                                ),
                                                    selectInput(inputId = "netstatnetwork_type",
                                                                label = "Types of Network",
                                                                choices = list("Road Network" = "net_road",
                                                                               "Pedestrian Network" = "net_ped",
                                                                               "Tram Network" = "net_tram")),
                                                    selectInput(inputId = "netstatlocs",
                                                                label = "Location of Interest",
                                                                choices = list(
                                                                  "Business Establishments" = "sf_business",
                                                                  "Childcare Centres" = "sf_childcare",
                                                                  "Drinking Fountain" = "sf_drinking_fountain",
                                                                  "Landmarks" = "sf_landmarks",
                                                                  "Public Toilets" = "sf_pub_toilets")),
                                                    selectInput(inputId = "netstatsublocs",
                                                                label = "Specific Themes/Sub-Categories",
                                                                choices = list()),
                                                    h5("Other Variables"),
                                                    sliderInput(inputId = "netstatnet_start", "Start",
                                                                min = 0, max = 2000, value = 100, step = 50),
                                                    sliderInput(inputId = "netstatnet_end", "End",
                                                                min = 100, max = 5000, value = 500, step = 50),
                                                    sliderInput(inputId = "netstatn_sims", "Number of Simulations",
                                                                min = 10, max = 300, value = 50, step = 5),
                                                    sliderInput(inputId = "netstatagg", "Aggregate Value",
                                                                min = 0, max = 1000, value = 0, step = 50),
                                                    
                                                    actionButton("netKDEGenerateStats", "Generate Statistical Results"),
                                                    
                                                    h5("Please note: The graphs will take a few minutes to generate."),
                                                    
                                                  ),
                                                ),
                                                uiOutput("netStatsExplainerp1"),
                                                hr(),
                                                uiOutput("netStatsExplainerp2"),
                                       ),
                           ),
                           
                           
                  ),
                  # 3rd Tab
                  tabPanel("Test Page",
                           
                  ),
                )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  id <- NULL
  
  output$mapPlot <- renderTmap({
    # Filter binlocation data based on selected bin type
    filtered_binlocation <- binlocation %>%
      filter(`Type of Bin Placed` == input$bin_type)
    
    # Plot map with filtered binlocation data
    tm_shape(mpsz) +
      tm_polygons() +
      tm_shape(filtered_binlocation) +
      tm_dots()
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
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)