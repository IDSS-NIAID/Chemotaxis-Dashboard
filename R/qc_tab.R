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
    sliderInput(
      inputId = ns("qc_angle_filter"),
      label = "Angle of migration filter",
      min = 0,
      max = 90,
      value = c(0, 90)
    ),
    numericInput(ns('qc_track_len'), 'Minimum Track Length (μm)', value = 1),
    numericInput(ns('qc_track_len_n'), 'Minimum Track Length (n)', value = 3)
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
           card_footer(downloadButton(ns('qc_n_cells_download'), 'Download figure'))),
      card(full_screen = TRUE, 
           card_header("Statistics"), 
           card_body(tableOutput(ns("qc_stats"))),
           card_footer(downloadButton(ns('qc_stats_download'), 'Download statistics'))))
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
#' @param shared_track_len reactiveVal from the main server function for track length definition
#' @param shared_track_len_n reactiveVal from the main server function for track length filter for total number of frames
#'
#' @export
#' @importFrom datamods select_group_server
#'
#' @importFrom dplyr arrange group_by mutate summarize ungroup filter
#' @importFrom ggplot2 ggsave
#' @importFrom shiny downloadHandler moduleServer reactive reactiveValues reactiveValuesToList renderPlot
#' @importFrom tibble rownames_to_column
qc_server <- function(id, con, shared_time_filter, shared_angle_filter, shared_track_len,
                      shared_track_len_n)
{
  # for all of those pesky "no visible binding" notes
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
      track_len_n <- reactive(input$qc_track_len_n)

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

    observeEvent(input$qc_track_len_n, {
      shared_track_len_n(track_len_n())
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
          updateSliderInput(session, "qc_angle_filter", value = shared_angle_filter())
        }
      }, ignoreInit = TRUE)
      
      observeEvent(shared_track_len(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(track_len(), shared_track_len()))) {
          updateNumericInput(session, "qc_track_len", value = shared_track_len())
        }
      }, ignoreInit = TRUE)

    observeEvent(shared_track_len_n(), {
      # Check prevents an infinite loop
      if (!isTRUE(all.equal(track_len_n(), shared_track_len_n()))) {
        updateNumericInput(session, "qc_track_len_n", value = shared_track_len_n())
      }
    }, ignoreInit = TRUE)
      
      # pull raw track information
      trackRaw <- reactive({
        if(length(chan_select()$expID) != 1)
          return(data.frame(expID = character(0),
                            trackID = integer(0),
                            frames = integer(0)))
                            
        get_dat(con,
                select = 'expID, trackID, frames',
                from = 'trackRaw',
                where = paste0( "expID='", chan_select()$expID,  "' AND ",
                               "chanID=", chan_select()$chanID)) |>
          mutate(time = frames / 2) |>
          filter(time >= time_filter()[1] & time <= time_filter()[2])
        })
      
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


      # Figures
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
      
      
      # statistics
      output$qc_stats <- renderTable({
        if(dim(chan_select())[1] != 1)
        {
          (vals$qc_stats <- data.frame())
        }else{
          (vals$qc_stats <- get_dat(con,
                                     select = "non_movers, little_movement, dns, few_frames, pre_start_frames, post_end_frames",
                                     from = "chanSummary",
                                     where = paste0("expID='", chan_select()$expID[1], "'", " AND ",
                                                    "chanID='", chan_select()$chanID[1], "'")
                                    )  |>
             select(-expID) |>
             t() |> 
             as.data.frame() |> 
             rename(count = V1) |>
             rownames_to_column("Filtered") |>
             mutate(count = as.integer(count),
                    Filtered = case_when(Filtered == "non_movers"       ~ "Track didn't change position",
                                         Filtered == "little_movement"  ~ "Track moved only a little",
                                         Filtered == "dns"              ~ "Track never crossed the upper ledge",
                                         Filtered == "few_frames"       ~ "Track had < 4 frames",
                                         Filtered == "pre_start_frames" ~ "# frames before crossing upper ledge",
                                         Filtered == "post_end_frames"  ~ "# frames after crossing lower ledge",
                                         TRUE ~ Filtered))
           )
        }
      })
      
      output$qc_stats_download <- downloadHandler(
        filename = function() {
          paste0("qc_stats_", paste(chan_select()[1,], collapse = '_'), ".csv")
        },
        content = function(file) {
          write.csv(vals$qc_stats, file, row.names = FALSE)
        }
      )
      
    }
  )
}
