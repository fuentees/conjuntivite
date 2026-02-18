library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(leaflet)
library(lubridate)
library(tidyr)

source("R/data.R")
source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)
