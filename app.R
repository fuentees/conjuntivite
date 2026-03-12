library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(leaflet)
library(lubridate)
library(tidyr)
library(rsconnect)

source("R/data.R")
source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)
