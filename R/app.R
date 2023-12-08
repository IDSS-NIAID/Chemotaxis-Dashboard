# app.R
# functions for setting up the app


#' app_ui
#' UI for Chemotaxis Dashboard
#' 
#' @return UI for Chemotaxis Dashboard
#' @export
#' @importFrom bslib page_sidebar sidebar
#' @importFrom shiny actionButton icon tags
#' @importFrom shinymanager secure_app
app_ui <- function()
{
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
}


#' app_server
#' Server for Chemotaxis Dashboard
#' 
#' @param credentials data frame with credentials. Should have columns `user` and `password`
#' @param con database connection returned from `DBI::dbConnect`
#' 
#' @return Server function for Chemotaxis Dashboard
#' @export
#' @importFrom shiny observeEvent reactiveValuesToList
#' @importFrom shinymanager check_credentials secure_server
app_server <- function(credentials, con)
{
  function(input, output, session) {
    
    # login as guest (see https://github.com/datastorm-open/shinymanager/issues/169)
    # this will result in a note from devtools::check(), since we are importing objects that aren't exported by shinymanager
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
}