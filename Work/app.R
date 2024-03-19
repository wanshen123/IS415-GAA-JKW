library(shiny)
library(sf)
library(tmap)
library(tidyverse)
library(leaflet)

mpsz <- st_read(dsn = "testdata", layer = "MP14_SUBZONE_WEB_PL")
popagsex <- read_csv("testdata/respopagesextod2011to2020.csv")
childcare <- st_read("testdata/PreSchoolsLocation.geojson") %>% st_transform(crs = 3414)
binlocation <- read_rds("../Data/alba/ewbins.rds")

popagsex2018_male <- popagsex %>%
  filter(Sex == "Males") %>%
  filter(Time == 2018) %>%
  spread(AG, Pop) %>%
  mutate(YOUNG = `0_to_4` + `5_to_9` + `10_to_14` + `15_to_19` + `20_to_24`) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[9:13]) + rowSums(.[15:17])) %>%
  mutate(`AGED` = rowSums(.[18:22])) %>%
  mutate(`TOTAL` = rowSums(.[5:22])) %>%
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`) / `ECONOMY ACTIVE`) %>%
  mutate_at(.vars = vars(PA, SZ), .funs = funs(toupper)) %>%
  select(`PA`, `SZ`, `YOUNG`, `ECONOMY ACTIVE`, `AGED`, `TOTAL`, `DEPENDENCY`) %>%
  filter(`ECONOMY ACTIVE` > 0)

mpsz_agemale2018 <- left_join(mpsz, popagsex2018_male, by = c("SUBZONE_N" = "SZ"))


ui <- fluidPage(
  titlePanel("Choropleth Mapping"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "classification",
                  label = "Classification method:",
                  choices = list("sd" = "sd",
                                 "equal" = "equal",
                                 "pretty" = "pretty",
                                 "quantile" = "quantile",
                                 "kmeans" = "kmeans",
                                 "hclust" = "hclust",
                                 "bclust" = "bclust",
                                 "fisher" = "fisher",
                                 "jenks" = "jenks"),
                  selected = "pretty"),
      sliderInput(inputId = "classes",
                  label = "Number of classes",
                  min = 6,
                  max = 12,
                  value = c(6)),
      selectInput(inputId = "colour",
                  label = "Colour scheme:",
                  choices = list("blues" = "Blues",
                                 "reds" = "Reds",
                                 "greens" = "Greens",
                                 "Yellow-Orange-Red" = "YlOrRd",
                                 "Yellow-Orange-Brown" = "YlOrBr",
                                 "Yellow-Green" = "YlGn",
                                 "Orange-Red" = "OrRd"),
                  selected = "YlOrRd"),
      selectInput(inputId = "typeofbin",
                  label = "Type of Bin:",
                  choices = unique(binlocation$Type.of.Bin.Placed),
                  multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Age Distribution",
                 tmapOutput("mapPlot1"),
                 DT::dataTableOutput("aTable1")),
        tabPanel("Bin Locations",
                 tmapOutput("mapPlot2"),
                 DT::dataTableOutput("aTable2"))
      )
    )
  )
)

server <- function(input, output) {
  tmap_options(check.and.fix = TRUE)  # Add this line to enable automatic fixing
  
  output$mapPlot1 <- renderTmap({
    tm_shape(mpsz_agemale2018) +
      tm_fill("DEPENDENCY",
              n = input$classes,
              style = input$classification,
              palette = input$colour) +
      tm_borders(lwd = 0.1, alpha = 1)
  })
  
  output$aTable1 <- DT::renderDataTable({
    mpsz_agemale2018
  })
  
  output$mapPlot2 <- renderTmap({
    # Check if any value is selected in the input field
    if (length(input$typeofbin) == 0) {
      return(NULL)  # Return NULL if no value is selected
    } else {
      tm_shape(mpsz) +
        tm_polygons() +
        tm_shape(binlocation[binlocation$Type.of.Bin.Placed %in% input$typeofbin, ]) +
        tm_dots(col = "Type.of.Bin.Placed",
                palette = "Set1",
                style = "cat") +
        tm_layout(legend.outside = TRUE) +
        tm_legend(legend.show = TRUE)
    }
  })
  
  output$aTable2 <- DT::renderDataTable({
    # Check if any value is selected in the input field
    if (length(input$typeofbin) == 0) {
      return(NULL)  # Return NULL if no value is selected
    } else {
      binlocation[binlocation$Type.of.Bin.Placed %in% input$typeofbin, ]
    }
  })
}

shinyApp(ui = ui, server = server)