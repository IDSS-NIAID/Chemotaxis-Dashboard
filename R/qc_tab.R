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
    sliderInput(
      inputId = ns("qc_time_filter"),
      label = "Time filter",
      min = 0,
      max = 60,
      value = c(0, 60)
    ),
    numericInput(
      inputId = ns("qc_angle_filter"),
      label = "min Angle of Migration",
      min = 0,
      max = 90,
      value = 0
    ),
    numericInput(ns('qc_track_len'), 'Minimum Track Length (μm)', value = 1),
    numericInput(ns('qc_track_n'), 'Minimum Track Length (n)', value = 3),
    numericInput(
      inputId = ns("qc_ce_filter"),
      label = "min Chemotactic Efficiency",
      min = -100,
      max = 100,
      value = 0
    )
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
#' @param con Active DBI database connection
#' @param shared_time_filter reactiveVal from the main server function for time filter definition
#' @param shared_angle_filter reactiveVal from the main server function for angle filter definition
#' @param shared_track_len reactiveVal from the main server function for physical track length definition
#' @param shared_track_n reactiveVal from the main server function for track length filter for total number of frames
#' @param shared_ce_filter reactiveVal from the main server function for filtering on minimum chemotactic efficiency
#'
#' @export
#' @importFrom datamods select_group_server
#'
#' @importFrom dplyr arrange group_by mutate summarize ungroup filter
#' @importFrom ggplot2 ggsave
#' @importFrom shiny downloadHandler moduleServer reactive reactiveValues reactiveValuesToList renderPlot
#' @importFrom tibble rownames_to_column
qc_server <- function(id, con, shared_time_filter, shared_angle_filter, shared_track_len,
                      shared_track_n, shared_ce_filter)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
    chanID <- count <- expID <- sID <- V1 <- time <- frames <- NULL
  
  moduleServer(
    id,
    function(input, output, session)
    {
      vals <- reactiveValues()

      # Filters
      time_filter <- reactive(input$qc_time_filter)
      angle_filter <- reactive(input$qc_angle_filter)
      track_len <- reactive(input$qc_track_len)
      track_n <- reactive(input$qc_track_n)
      ce_filter <- reactive(input$qc_ce_filter)

      # When filters change in THIS tab, update the shared value
      observeEvent(input$qc_time_filter, {
        shared_time_filter(time_filter())
      })
      
      observeEvent(input$qc_angle_filter, {
        shared_angle_filter(angle_filter())
      })
      
      observeEvent(input$qc_track_len, {
        shared_track_len(track_len())
      })

    observeEvent(input$qc_track_n, {
      shared_track_n(track_n())
    })
    
    observeEvent(input$qc_ce_filter, {
      shared_ce_filter(ce_filter())
    })

      # When shared values change, update filters in THIS tab
      observeEvent(shared_time_filter(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(time_filter(), shared_time_filter()))) {
          updateSliderInput(session, "qc_time_filter", value = shared_time_filter())
        }
      }, ignoreInit = TRUE)
      
      observeEvent(shared_angle_filter(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(angle_filter(), shared_angle_filter()))) {
          updateNumericInput(session, "qc_angle_filter", value = shared_angle_filter())
        }
      }, ignoreInit = TRUE)
      
      observeEvent(shared_track_len(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(track_len(), shared_track_len()))) {
          updateNumericInput(session, "qc_track_len", value = shared_track_len())
        }
      }, ignoreInit = TRUE)

    observeEvent(shared_track_n(), {
      # Check prevents an infinite loop
      if (!isTRUE(all.equal(track_n(), shared_track_n()))) {
        updateNumericInput(session, "qc_track_n", value = shared_track_n())
      }
    }, ignoreInit = TRUE)
    
    observeEvent(shared_ce_filter(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(ce_filter(), shared_ce_filter()))) {
          updateNumericInput(session, "qc_ce_filter", value = shared_ce_filter())
        }
      }, ignoreInit = TRUE)
      
      
      # for channel/sample selection
      chan_select <- select_group_server(id = "qc_channels",
                                         data_r = reactive({
                                           get_dat(con,
                                                   select = 'expID, chanID, sID, treatment',
                                                   from = 'chanSummary') |>
                                             mutate(expID = factor(expID),
                                                    chanID = factor(chanID),
                                                    sID = factor(sID))
                                         }),
                                         vars_r = reactive(c('expID', 'sID', 'chanID'))
      )


      # this has all raw track data plus drop
      track_raw_all <- reactive({
        if(length(chan_select()$expID) != 1)
          return(data.frame(expID = character(0),
                            trackID = integer(0),
                            frames = integer(0),
                            drop = logical(0)))
                            
        get_dat(con,
                select = 'expID, chanID, trackID, x, y, v_x, v_y, theta, frames',
                from = 'trackRaw',
                where = paste0( "expID='", chan_select()$expID,  "' AND ",
                               "chanID=", chan_select()$chanID)) |>
          
          mutate(time = frames / 2,
                 drop = time < time_filter()[1] | time > time_filter()[2]) |>
          
          left_join(get_dat(con,
                            select = "expID, sID, chanID, treatment",
                            from = "chanSummary",
                            where = paste0("expID='", chan_select()$expID, "' AND ",
                                           "chanID=", chan_select()$chanID)),
                    by = c("expID", "chanID"))          
        })
      
      
      # this has summary track information plus filtering metadata
      track_summ <- reactive({
        if(nrow(track_raw_all()) == 0)
          return(data.frame(expID = character(0),
                            trackID = integer(0),
                            frames = integer(0),
                            drop = logical(0)))
        
        track_raw_all() |>
          filter(!drop) |>
          summarize_tracks() |>
          mutate(drop = angle_migration < angle_filter() |
                        distance_traveled < track_len() |
                        n_frames < track_n() |
                        ce < ce_filter())
      })


      # this is the version for plotting, after filtering for tracks that should be dropped
      track_raw <- reactive({
        if(nrow(track_summ()) == 0)
          return(data.frame(expID = character(0),
                            trackID = integer(0),
                            frames = integer(0),
                            drop = logical(0)))

        track_raw_all() |>
            left_join(track_summ() |> 
                      select(chanID, trackID, drop) |> 
                      rename(drop_summ = drop)) |>
            filter(!drop & !drop_summ)
      })


      # Figures
      output$qc_track_len <- renderPlot((vals$track_len <- qc_track_len(track_raw())))
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
      
      output$qc_n_cells <- renderPlot((vals$n_cells <- qc_n_cells(track_raw())))
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
