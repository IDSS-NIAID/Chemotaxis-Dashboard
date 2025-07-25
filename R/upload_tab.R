#' upload_tab
#'
#' @name upload_tab
#' 
#' @param id Shiny namespace ID
#' 
#' @return A tabPanel
#'
#' @export
#' @importFrom shiny actionButton fileInput h3 mainPanel NS numericInput p sidebarLayout sidebarPanel tableOutput tabPanel
upload_ui <- function(id)
{
  ns <- NS(id)

  tabPanel(
    "Upload Data",
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("upload_files"), "Choose CSV File(s)",
                  multiple = TRUE,
                  accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
        tags$hr(),
        numericInput(ns("ledge_upper"), "Ledge Upper", value = 100),
        numericInput(ns("ledge_lower"), "Ledge Lower", value = 500),
        numericInput(ns("ledge_dist"), "Ledge Dist", value = 260),
        tags$hr(),
        p("Once you have selected your files, the data will be processed ",
          "and available in the other tabs."),
        actionButton(ns("import_data"), "Import Uploaded Data")
      ),
      mainPanel(
        h3("Instructions"),
        p("Please upload your CSV files using the controls on the left. ",
          "You can upload multiple files at once. The files should be have the ",
          "following columns: Track, Frame, X, and Y. ",
        "For more information see https://github.com/IDSS-NIAID/Chemotaxis-Dashboard."),
        h3("Uploaded Files"),
        tableOutput(ns("contents"))
      )
    )
  )
}

#' process_uploaded_data
#'
#' @name upload_tab
#' 
#' @description
#' \code{process_uploaded_data} server-side logic for processing uploaded data.
#'
#' @param id The shiny namespace ID.
#' @param con A database connection.
#'
#' @export
#' @importFrom shiny moduleServer observeEvent reactive renderTable req showNotification
upload_server <- function(id, con)
{
  moduleServer(
    id, 
    function(input, output, session)
    {
      processed_data <- reactive({
        # Require that files are uploaded before proceeding.
        req(input$upload_files)
        
        retval <- tryCatch({
          process_uploaded_data(
            uploaded_files = input$upload_files,
            ledge_upper = input$ledge_upper,
            ledge_lower = input$ledge_lower,
            ledge_dist = input$ledge_dist
          )
        }, error = function(e) {
          # If an error occurs, show a notification to the user.
          showNotification(
            paste("Error processing files:", e$message),
            type = "error",
            duration = 10 # Keep the message on screen longer
          )
          # Return NULL so that downstream reactive elements know processing failed.
          return(NULL)
        })

        return(retval)
      })
      
      output$contents <- renderTable({
        req(input$upload_files)
        # Displaying just the file names is a bit cleaner.
        input$upload_files[, "name", drop = FALSE]
      })
      
      # This observer handles the button click for importing data.
      observeEvent(input$import_data, {
        req(processed_data())

        dat_to_import <- processed_data()
        
        tryCatch({
          dbupdate(con,
                   table = 'chanSummary',
                   dat = dat_to_import$chanSummary,
                   key_fields = c('expID', 'chanID'))
          
          dbupdate(con,
                   table = 'trackRaw',
                   dat = dat_to_import$trackRaw,
                   key_fields = c('expID', 'chanID', 'trackID', 'frames'))
          
          showNotification("Data imported successfully.", type = "message")
          
        }, error = function(e) {
          showNotification(
            paste("Database import failed:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })
}
