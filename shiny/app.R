#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymanager)
library(RSQLite)
library(DBI)
library(config)
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
    title = 'QC parameters',
    sliderInput('qc_min_track_len', 'Minimum Track Length', 3, 60, value = 6), # minimum track length in minutes
    numericInput('qc_n_cells', 'Number of cells', value = 100)
  ),
  card(plotOutput("qc_track_len")),
  card(plotOutput("qc_n_cells"))
) %>%
  secure_app()


##########
# Server #
##########

server <- function(input, output, session) {

    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )

    trackRaw <- reactive({
      get_dat(con,
              user = reactiveValuesToList(res_auth)$user,
              select = 'expID, trackID, frames',
              from = 'trackRaw',
              where = 'chanID=2')
    })
    
    output$qc_track_len <- renderPlot({
      group_by(trackRaw(), trackID) %>%
        summarize(min_time = min(frames) / 2,
                  max_time = max(frames) / 2) %>%
        ungroup() %>%
        
        arrange(min_time) %>%
        mutate(track_ordered = 1:length(min_time)) %>%
        
        ggplot(aes(x = min_time, xend = max_time, y = track_ordered, yend = track_ordered)) +
        geom_segment() +
        labs(x = 'Time (min)', y = 'Track number')
    })
    
    output$qc_n_cells <- renderPlot({
      group_by(trackRaw(), frames) %>%
        summarize(n_tracks = length(unique(trackID))) %>%
        ungroup() %>%
        
        mutate(time = frames / 2) %>%
        
        ggplot(aes(time, n_tracks)) +
        geom_line() +
        labs(x = 'Time (min)', y = '# Tracks')
    })
}


#######
# App #
#######

shinyApp(ui = ui, server = server)

dbDisconnect(con)
