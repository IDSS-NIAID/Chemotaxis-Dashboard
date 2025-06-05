
library(ChemotaxisDashboard)
# devtools::load_all()
library(shiny)
library(RSQLite)
library(DBI)


#######
# App #
#######

shinyApp(ui     = app_ui(),
         server = app_server)
