# qc_tab.R
# UI and server elements for the QC tab

#' qc_sidebarUI
#' UI element for the QC sidebar
#' 
#' @name qc_tab
#' 
#' @param id Shiny namespace ID
#' 
#' @return A modularized tagList
#' @export
#' @importFrom shiny NS numericInput sliderInput tagList
#' @importFrom datamods select_group_ui
qc_sidebarUI <- function(id)
{
  ns <- NS(id)
  
  tagList(
    select_group_ui(id = ns("qc_channels"),
                    params = list(list(inputId = "expID",  label = "Experiment"),
                                  list(inputId = "sID",    label = "Sample"),
                                  list(inputId = "chanID", label = "Channel")),
                    inline = FALSE),
    #sliderInput(ns('qc_min_track_len'), 'Minimum Track Length', 3, 60, value = 6), # minimum track length in minutes
    #numericInput(ns('qc_n_cells'), 'Number of cells', value = 100)
  )
}


#' qc_cardsUI
#' UI element for the QC cards
#' 
#' @rdname qc_tab
#' 
#' @return A modularized tagList of cards
#' @export
#' 
#' @importFrom bslib card card_header card_body card_footer layout_sidebar
#' @importFrom shiny downloadButton NS plotOutput tagList
qc_cardsUI <- function(id)
{
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = sidebar(qc_sidebarUI('qc')),
      card(full_screen = TRUE, 
           card_header("Track length distribution"), 
           card_body(plotOutput(ns("qc_track_len"))),
           card_footer(downloadButton(ns('qc_track_len_download'), 'Download figure'))),
      card(full_screen = TRUE, 
           card_header("# Tracks (cells) over time"), 
           card_body(plotOutput(ns("qc_n_cells"))),
           card_footer(downloadButton(ns('qc_n_cells_download'), 'Download figure'))))
  )
}


#' qc_server
#' Server element for QC tab
#' 
#' @rdname qc_tab
#' 
#' @export
#' @importFrom datamods select_group_server
#'
#' @importFrom dplyr arrange group_by mutate summarize ungroup
#' @importFrom ggplot2 aes geom_line geom_segment ggplot labs
#' @importFrom shiny downloadHandler moduleServer reactive reactiveValues reactiveValuesToList renderPlot
qc_server <- function(id, con, user)
{
  # for all of those pesky "no visible binding" notes
  if(FALSE)
    chanID <- expID <- sID <- NULL
  
  moduleServer(
    id,
    function(input, output, session)
    {
      vals <- reactiveValues()
      
      # pull raw track information
      trackRaw <- reactive({
        if(length(chan_select()$expID) != 1)
          return(data.frame(expID = character(0),
                            trackID = integer(0),
                            frames = integer(0)))
                            
        get_dat(con,
                user = user,
                select = 'expID, trackID, frames',
                from = 'trackRaw',
                where = paste0( "expID='", chan_select()$expID,  "' AND ",
                               "chanID=", chan_select()$chanID))
        })
      
      # for channel/sample selection
      chan_select <- select_group_server(id = "qc_channels",
                                         data_r = reactive({
                                           get_dat(con,
                                                   user = user,
                                                   select = 'expID, chanID, sID, treatment',
                                                   from = 'chanSummary') %>%
                                             mutate(expID = factor(expID),
                                                    chanID = factor(chanID),
                                                    sID = factor(sID))
                                         }),
                                         vars_r = reactive(c('expID', 'sID', 'chanID'))
      )

      output$qc_track_len <- renderPlot((vals$track_len <- qc_track_len(trackRaw())))
      output$qc_track_len_download <- downloadHandler(
        filename = function() {
          if(length(chan_select()$expID) != 1)
            return('null.png')
          
          paste0("track_len_", chan_select()$expID, "_", chan_select()$sID, "_", chan_select()$chanID, ".png")
        },
        content = function(file) {
          ggsave(file, vals$track_len)
        }
      )
      
      output$qc_n_cells <- renderPlot((vals$n_cells <- qc_n_cells(trackRaw())))
      output$qc_n_cells_download <- downloadHandler(
        filename = function() {
          if(length(chan_select()$expID) != 1)
            return('null.png')
          
          paste0("n_cells_", chan_select()$expID, "_", chan_select()$sID, "_", chan_select()$chanID, ".png")
        },
        content = function(file) {
          ggsave(file, vals$n_cells)
        }
      )
    }
  )
}
