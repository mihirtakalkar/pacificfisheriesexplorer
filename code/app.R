# Load Packages #
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggthemes)


# Data Import #
# Directories #
dataDir <- "data"
plotDir <- "figures"
tableDir <- "tables"
sstdir <- "data/cobe"
meowdir <- "data/meows/processed"


source("E:/Important/RMP/Code/pacificfisheriesexplorer/code/monthly_fishery_scale.R")
source("E:/Important/RMP/Code/pacificfisheriesexplorer/code/sst_region_explorer.R")

mexicoLandings = readRDS(file.path("E:/Important/RMP/Code/pacificfisheriesexplorer/data", "2001_2020_mexico_landings_datamares.Rds"))
# Read MEOW shapefile
meows_orig <- readRDS(file.path("E:/Important/RMP/Code/pacificfisheriesexplorer/data/meows/processed/meows.Rds"))

# Read MEOW time series
sst_orig <- read.csv(file.path("E:/Important/RMP/Code/pacificfisheriesexplorer/data/cobe/COBE_1891_2020_sst_by_ecoregion.csv"))

# Panel 1 Data Build #
years <- 2001:2019
ecoregion_choices <- c("Oregon, Washington, Vancouver Coast and Shelf",
                       "Northern California",
                       "Southern California Bight",
                       "Magdalena Transition",
                       "Cortezian",
                       "Mexican Tropical Pacific",
                       "Chiapas-Nicaragua")

# User Interface #
ui <- fluidPage(theme = shinytheme("simplex"),
                navbarPage("Mexican-Pacific Fisheries Viewer",
                           # Page 1: Fishery Scale #
                           tabPanel("Fishery Scale",
                                    mainPanel(
                                      h1("Explore Artisanal and Industrial Fishery Scales"),
                                      h5("Mexican fisheries operate at two distinct scales."),
                                      selectInput(inputId = "year_landings", label = "Year",
                                                  choices = years, multiple = F),
                                      plotOutput(outputId = "plot_years", width=1000, height=375),

                                    )),
                           # Page 2: Sea Surface Temperature Viewer #
                           tabPanel("Sea Surface Temperature",
                                    mainPanel(
                                      h1("Explore SST"),
                                      h5("sst"),
                                      selectInput(inputId = "region_selects", label = "Ecoregion",
                                                  choices = ecoregion_choices, multiple = F),
                                      plotOutput(outputId = "plot_sst_regions", width=1000, height=375),
                                    )),
                           # Page 3: Map-Based Landings Viewer #
                           tabPanel("Location", "This panel is intentionally left blank"),
                           # Page 4: Potential Random Forest Predictor #
                           tabPanel("Predictor", "Possible trend predictor or user-input based prediction")

                )
)


# Server #
server <- function(input, output, session) {
  output$plot_years <- renderPlot({
    g <- plot_fishery_monthly(dataset=mexicoLandings, varyear = input$year_landings)
    g
  })
  output$plot_sst_regions <- renderPlot({
    g1 <- plot_ecoregion_sst(datasetmeow=meows_orig, datasetsst=sst_orig, region = input$region_selects)
    g1
  })
}



# Generate App #
shinyApp(ui = ui, server = server)

