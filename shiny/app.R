
library(ChemotaxisDashboard)
# remotes::install_github('IDSS-NIAID/Chemotaxis-Dashboard')
# devtools::load_all()
library(shiny)
library(RSQLite)
library(DBI)


#######
# App #
#######

shinyApp(ui     = app_ui(),
         server = app_server)

