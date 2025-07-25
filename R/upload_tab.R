#' @title upload_tab
#'
#' @description
#' \code{upload_tab} UI for the upload tab.
#'
#' @return A tabPanel
#'
#' @export
upload_tab <- function() {
  tabPanel(
    "Upload Data",
    sidebarLayout(
      sidebarPanel(
        fileInput("upload_files", "Choose CSV File(s)",
                  multiple = TRUE,
                  accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
        tags$hr(),
        checkboxInput("header", "Header", TRUE),
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        tags$hr(),
        numericInput("ledge_upper", "Ledge Upper", value = 100),
        numericInput("ledge_lower", "Ledge Lower", value = 500),
        numericInput("ledge_dist", "Ledge Dist", value = 260),
        tags$hr(),
        p("Once you have selected your files, the data will be processed ",
          "and available in the other tabs.")
      ),
      mainPanel(
        h3("Instructions"),
        p("Please upload your CSV files using the controls on the left. ",
          "You can upload multiple files at once. The files should be have the ",
          "following columns: Track, Frame, X, and Y. ",
        "For more information see https://github.com/IDSS-NIAID/Chemotaxis-Dashboard."),
        h3("Uploaded Files"),
        tableOutput("contents")
      )
    )
  )
}