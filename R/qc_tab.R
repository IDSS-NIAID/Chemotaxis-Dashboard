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
#' @importFrom shiny NS
#' @importFrom shiny numericInput
#' @importFrom shiny sliderInput
#' @importFrom shiny tagList
qc_sidebarUI <- function(id)
{
  ns <- NS(id)
  
  tagList(
    sliderInput(ns('qc_min_track_len'), 'Minimum Track Length', 3, 60, value = 6), # minimum track length in minutes
    numericInput(ns('qc_n_cells'), 'Number of cells', value = 100)
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
#' @importFrom bslib card
#' @importFrom bslib card_header
#' 
#' @importFrom shiny NS
#' @importFrom shiny plotOutput
#' @importFrom shiny tagList
qc_cardsUI <- function(id)
{
  ns <- NS(id)
  
  tagList(
    card(full_screen = TRUE, card_header("Hello"), plotOutput(ns("qc_track_len"))),
    card(full_screen = TRUE, card_header("World"), plotOutput(ns("qc_n_cells")))
  )
}


#' qc_server
#' Server element for QC tab
#' 
#' @rdname qc_tab
#' 
#' @param con Active DBI database connection
#' @param user Username of the user
#' 
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' 
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' 
#' @importFrom shiny moduleServer
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValuesToList
#' @importFrom shiny renderPlot
qc_server <- function(id, con, user)
{
  # for all of those pesky "no visible binding" notes
  expID <- max_time <- min_time <- n_tracks <- time <- trackID <- track_ordered <- NULL
  
  moduleServer(
    id,
    function(input, output, session)
    {
      # pull raw track information
      trackRaw <- reactive({
        get_dat(con,
                user = user,
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
        tmp <- group_by(trackRaw(), frames) %>%
          summarize(n_tracks = length(unique(trackID))) %>%
          ungroup() %>%
          
          mutate(time = frames / 2)
        
        print(tmp)
          
        ggplot(tmp, aes(time, n_tracks)) +
          geom_line() +
          labs(x = 'Time (min)', y = '# Tracks')
      })
    }
  )
}
