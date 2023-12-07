#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ChemotaxisDashboard)
library(shiny)
library(shinymanager)
library(RSQLite)
library(DBI)
library(bslib)

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(dplyr)


#################
# Configuration #
#################

config <- config::get(file = file.path(system('git rev-parse --show-toplevel', intern = TRUE),
                                       'shiny', 'config.yaml'))

# if db_path doesn't exist, try parsing it
if(!file.exists(config$db_path))
  config$db_path <- eval(parse(text = config$db_path))

# load database
con <- dbConnect(SQLite(), config$db_path)

# define some credentials
credentials <- dbGetQuery(con, 'SELECT * FROM users')


######
# UI #
######

ui <- page_sidebar(
  title = 'Chemotaxis Dashboard',
  sidebar = sidebar(
    title = 'QC parameters', qc_sidebarUI('qc')
  ),
  
  qc_cardsUI('qc')
) %>%
  secure_app(tags_top = tags$p(actionButton(inputId = "login_guest", 
                                            label = "Continue as guest", 
                                            icon = icon("user"))))


##########
# Server #
##########

server <- function(input, output, session) {

  # login as guest (see https://github.com/datastorm-open/shinymanager/issues/169)
  observeEvent(input$login_guest, {
    token <- shinymanager:::.tok$generate("shinyuser")
    shinymanager:::.tok$add(token, list(user = "shinyuser", role = "guest"))
    shinymanager:::addAuthToQuery(session, token, "en")
    session$reload()
  })

  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  qc_server("qc", con, reactiveValuesToList(res_auth)$user)
}


#######
# App #
#######

shinyApp(ui = ui, server = server)
