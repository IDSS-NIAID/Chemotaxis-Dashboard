# app.R
# functions for setting up the app


#' app_ui
#' UI for Chemotaxis Dashboard
#' 
#' @return UI for Chemotaxis Dashboard
#' @export
#' @importFrom bslib page_navbar nav_panel sidebar
#' @importFrom shiny actionButton icon tags
app_ui <- function()
{
  ui <- page_navbar(title = 'Chemotaxis Dashboard',
                    nav_panel(title = 'Cross-Experiment Summary',
                              ces_cardsUI('ces')),
                    nav_panel(title = 'Within-Experiment Summary',
                              ses_cardsUI('ses')),
                    nav_panel(title = 'QC parameters',
                              qc_cardsUI('qc')))
}


#' app_server
#' Server for Chemotaxis Dashboard
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' 
#' @return Server function for Chemotaxis Dashboard
#' @export
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom shiny observeEvent reactiveVal reactiveValuesToList updateSliderInput
app_server <- function(input, output, session)
{
  #################
  # Configuration #
  #################
  
  config <- config::get(file = 'config.yaml')
  
  # if db_path doesn't exist, try parsing it
  if(!file.exists(config$db_path))
    config$db_path <- eval(parse(text = config$db_path))
  
  # un/load database
  con <- dbConnect(SQLite(), config$db_path)
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
  

  ###########
  # Filters #
  ###########
  # In general, we'll create reactiveVals to act as the central "source of truth" across tabs

  shared_time_filter <- reactiveVal(c(0, 60))
  shared_angle_filter <- reactiveVal(c(0, 90))
  shared_track_len <- reactiveVal(1)
  shared_track_n <- reactiveVal(3)

  ###############
  # Set up tabs #
  ###############
  
  # Cross-experiment summary tab
  ces_server("ces", con, shared_time_filter)
  
  # Single experiment summary tab
  ses_server("ses", con, shared_time_filter, shared_angle_filter,
             shared_track_len, shared_track_n)
  
  # QC tab  
  qc_server("qc", con, shared_time_filter, shared_angle_filter,
            shared_track_len, shared_track_n)
}