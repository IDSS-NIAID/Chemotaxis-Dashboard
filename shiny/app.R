#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ChemotaxisDashboard)
library(RSQLite)
library(DBI)


#################
# Configuration #
#################

config <- config::get(file = 'config.yaml')

# if db_path doesn't exist, try parsing it
if(!file.exists(config$db_path))
  config$db_path <- eval(parse(text = config$db_path))

# load database
con <- dbConnect(SQLite(), config$db_path)

# define some credentials
credentials <- dbGetQuery(con, 'SELECT * FROM users')


#######
# App #
#######

shinyApp(ui     = ChemotaxisDashboard::app_ui(),
         server = ChemotaxisDashboard::app_server(credentials, con))
