# Load Packages #
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(rnaturalearth)


# Data Import #
# Directories #
dataDir <- "data"
plotDir <- "figures"
tableDir <- "tables"
sstdir <- "data/cobe"
meowdir <- "data/meows/processed"

# Source Files #
source("E:/Important/RMP/Code/pacificfisheriesexplorer/code/monthly_fishery_scale.R")
source("E:/Important/RMP/Code/pacificfisheriesexplorer/code/sst_region_explorer.R")

# Read Data #
mexicoLandings = readRDS(file.path("E:/Important/RMP/Code/pacificfisheriesexplorer/data", "2001_2020_mexico_landings_datamares.Rds"))
meows_orig <- readRDS(file.path("E:/Important/RMP/Code/pacificfisheriesexplorer/data/meows/processed/meows.Rds"))
sst_orig <- read.csv(file.path("E:/Important/RMP/Code/pacificfisheriesexplorer/data/cobe/COBE_1891_2020_sst_by_ecoregion.csv"))

# Data Build #
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
                                      plotOutput(outputId = "plot_years_value", width=1000, height=375),
                                      br(),
                                      plotOutput(outputId = "plot_years_landings", width=1000, height=375),
                                    )),
                           # Page 2: Sea Surface Temperature Viewer #
                           tabPanel("Sea Surface Temperature",
                                    mainPanel(
                                      h1("Explore SST"),
                                      h5("sst"),
                                      selectInput(inputId = "region_selects", label = "Ecoregion",
                                                  choices = ecoregion_choices, multiple = F),
                                      sliderInput(inputId = "year_sst", label = "Year",
                                                  min = 1890, max = 2020, c(1950, 2000), step = 5),
                                      plotOutput(outputId = "plot_sst_regions", width=1000, height=375),
                                      br(),
                                      plotOutput(outputId = "plot_regions_pacific", width=1000, height=375)
                                    )),
                           # Page 3: Species Based Viewer #
                           tabPanel("Species", "This panel is intentionally left blank"),
                           # Page 4: Map-Based Landings Viewer #
                           tabPanel("Location", "This panel is intentionally left blank"),
                           # Page 5: Potential Random Forest Predictor #
                           tabPanel("Predictor", "Possible trend predictor or user-input based prediction")

                )
)


# Server #
server <- function(input, output, session) {
  # Monthly Fishery Landings #
  output$plot_years_landings <- renderPlot({
    g <- plot_fishery_monthly_landings(dataset=mexicoLandings, varyear = input$year_landings)
    g
  })

  # Monthly Fishery Landings #
  output$plot_years_value <- renderPlot({
    g <- plot_fishery_monthly_values(dataset=mexicoLandings, varyear = input$year_landings)
    g
  })

  # SST Timeseries by Ecoregion #
  output$plot_sst_regions <- renderPlot({
    g <- plot_ecoregion_sst(datasetmeow=meows_orig, datasetsst=sst_orig, region = input$region_selects, range = input$year_sst)
    g
  })

  # Updated Ecoregion Map #
  output$plot_regions_pacific <- renderPlot({
    g <- plot_ecoregion(datasetmeow=meows_orig, region = input$region_selects)
    g
  })
}

# Generate App #
shinyApp(ui = ui, server = server)

