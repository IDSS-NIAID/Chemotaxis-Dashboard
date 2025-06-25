# app.R
# functions for setting up the app


#' app_ui
#' UI for Chemotaxis Dashboard
#' 
#' @return UI for Chemotaxis Dashboard
#' @export
#' @importFrom bslib page_navbar nav_panel sidebar
#' @importFrom shiny actionButton icon tags
#' @importFrom shinymanager secure_app
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
#' @importFrom shiny observeEvent reactiveValuesToList
#' @importFrom shinymanager check_credentials secure_server
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
  
  # get the username
  user <- Sys.getenv("USER") |>
    gsub(pattern = "@nih.gov", replacement = "", fixed = TRUE)
    
  if(user %in% c("", "rstudio-connect"))
    user <- strsplit(strsplit(session$request$HTTP_SHINY_SERVER_CREDENTIALS, '\\', fixed = TRUE)[[1]][3], '"')[[1]][1]

  
  ###############
  # Set up tabs #
  ###############
  
  # Cross-experiment summary tab
  ces_server("ces", con, user)
  
  # Single experiment summary tab
  ses_server("ses", con, user)
  
  # QC tab  
  qc_server("qc", con, user)
}