# ses_tab.R
# UI and server elements for the Single Experiment Statistics tab

#' ses_sidebarUI
#' UI element for the SE sidebar
#' 
#' @name ses_tab
#' 
#' @param id Shiny namespace ID
#' 
#' @return A modularized tagList
#' @export
#' @importFrom shiny NS tagList
#' @importFrom datamods select_group_ui
ses_sidebarUI <- function(id)
{
  ns <- NS(id)
  
  tagList(
    select_group_ui(id = ns("ses_channels"),
                    params = list(list(inputId = "expID",  label = "Experiment")),
                    inline = FALSE),
    sliderInput(
      inputId = ns("ses_time_filter"),
      label = "Time filter",
      min = 0,
      max = 60,
      value = c(0, 60)
    ),
    sliderInput(
      inputId = ns("ses_angle_filter"),
      label = "Angle of migration filter",
      min = 0,
      max = 90,
      value = c(0, 90)
    ),
    numericInput(ns('ses_track_len'), 'Minimum Track Length (μm)', value = 1),
    numericInput(ns('ses_track_n'), 'Minimum Track Length (n)', value = 3)
  )
}


#' ses_cardsUI
#' UI element for the SE cards
#' 
#' @rdname ses_tab
#' 
#' @return A modularized tagList of cards
#' @export
#' 
#' @importFrom bslib card card_header card_body card_footer layout_sidebar
#' @importFrom shiny downloadButton NS plotOutput tagList
ses_cardsUI <- function(id)
{
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = ses_sidebarUI('ses'),
      card(full_screen = TRUE,
           card_header("Tracks over time"), 
           card_body(plotOutput(ns("ses_tracks_time"))),
           card_footer(downloadButton(ns('ses_tracks_time_download'), 'Download figure'))),
      card(full_screen = TRUE,
           card_header("Track velocity"), 
           card_body(plotOutput(ns("ses_tracks_v"))),
           card_footer(downloadButton(ns('ses_tracks_v_download'), 'Download figure'))),
      card(full_screen = TRUE,
           card_header("Angle of migration"), 
           card_body(plotOutput(ns("ses_angle_migration"))),
           card_footer(downloadButton(ns('ses_angle_migration_download'), 'Download figure'))),
      card(full_screen = TRUE,
           card_header('Instantaneous Angle of Migration'),
           card_body(plotOutput(ns('ses_instant_aom'))),
           card_footer(downloadButton(ns('ses_instant_aom_download'), 'Download figure'))),
       card(full_screen = TRUE,
           card_header("Chemotactic Efficiency"), 
           card_body(plotOutput(ns("ses_ce"))),
           card_footer(downloadButton(ns('ses_ce_download'), 'Download figure')))
    )
  )
}


#' ses_server
#' Server logic for the Single Experiment Statistics tab
#' 
#' @rdname ses_tab 
#' 
#' @param con Active DBI database connection
#' @param shared_time_filter reactiveVal from the main server function for time filter definition
#' @param shared_angle_filter reactiveVal from the main server function for angle filter definition
#' @param shared_track_len reactiveVal from the main server function for physical track length filter in μm
#' @param shared_track_n reactiveVal from the main server function for track length filter for total number of frames
#'
#' @export
#' @importFrom shiny downloadHandler moduleServer reactive reactiveValues renderPlot renderTable
#' @importFrom dplyr left_join filter
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.csv
ses_server <- function(id, con, shared_time_filter, shared_angle_filter, shared_track_len,
                       shared_track_n)
{
  moduleServer(id, function(input, output, session)
  {
    if(FALSE)
      time <- trackID <- frames <- NULL

    vals <- reactiveValues()

    # Filters
    time_filter <- reactive(input$ses_time_filter)
    angle_filter <- reactive(input$ses_angle_filter)
    track_len <- reactive(input$ses_track_len)
    track_n <- reactive(input$ses_track_n)


    # When filters change in THIS tab, update the shared value
    observeEvent(input$ses_time_filter, {
      shared_time_filter(time_filter())
    })
    
    observeEvent(input$ses_angle_filter, {
      shared_angle_filter(angle_filter())
    })
    
    observeEvent(input$ses_track_len, {
      shared_track_len(track_len())
    })

    observeEvent(input$ses_track_n, {
      shared_track_n(track_n())
    })


    # When shared values change, update filters in THIS tab
    observeEvent(shared_time_filter(), {
      # Check prevents an infinite loop
      if (!isTRUE(all.equal(time_filter(), shared_time_filter()))) {
        updateSliderInput(session, "ses_time_filter", value = shared_time_filter())
      }
    }, ignoreInit = TRUE)
    
    observeEvent(shared_angle_filter(), {
      # Check prevents an infinite loop
      if (!isTRUE(all.equal(angle_filter(), shared_angle_filter()))) {
        updateSliderInput(session, "ses_angle_filter", value = shared_angle_filter())
      }
    }, ignoreInit = TRUE)
    
    observeEvent(shared_track_len(), {
      # Check prevents an infinite loop
      if (!isTRUE(all.equal(track_len(), shared_track_len()))) {
        updateNumericInput(session, "ses_track_len", value = shared_track_len())
      }
    }, ignoreInit = TRUE)

    observeEvent(shared_track_n(), {
      # Check prevents an infinite loop
      if (!isTRUE(all.equal(track_n(), shared_track_n()))) {
        updateNumericInput(session, "ses_track_n", value = shared_track_n())
      }
    }, ignoreInit = TRUE)


    # Experiment selection
    exp_select <- select_group_server(id = "ses_channels",
                                      data_r = reactive({
                                        get_dat(con,
                                                select = "DISTINCT expID",
                                                from = "chanRaw")
                                      }),
                                      vars_r = 'expID')

    
    track_raw <- reactive({
      
      get_dat(con,
              select = "expID, chanID, trackID, x, y, v_x, v_y, theta, frames",
              from = "trackRaw",
              where = paste0("expID='", exp_select()[1], "'")) |>
        mutate(time = frames / 2) |>
        filter(time >= time_filter()[1] & time <= time_filter()[2]) |>
        
        left_join(get_dat(con,
                          select = "expID, sID, chanID, treatment",
                          from = "chanSummary",
                          where = paste0("expID='", exp_select()[1], "'")),
                  by = c("expID", "chanID"))
    })
    
    
    track_summ <- reactive({
      
      get_dat(con,
              select = "expID, chanID, trackID, ce, angle_migration",
              from = "trackSummary",
              where = paste0("expID='", exp_select()[1], "'")) |>
        filter(trackID %in% unique(track_raw()$trackID)) |>
        filter(angle_migration >= angle_filter()[1] & angle_migration <= angle_filter()[2]) |>
        
        left_join(get_dat(con,
                          select = "expID, sID, chanID, treatment",
                          from = "chanSummary",
                          where = paste0("expID='", exp_select()[1], "'")),
                  by = c("expID", "chanID"))
    })

    
    # Track length distribution
    output$ses_tracks_time <- renderPlot({
      if(length(exp_select()) != 1)
      {
        (vals$ses_tracks_time <- plot_nothing())
      }else{
        (vals$ses_tracks_time <- track_raw() |>
          ses_tracks_time())
      }
    })
    
    output$ses_tracks_time_download <- downloadHandler(
      filename = function() {
        paste0("tracks_time_", exp_select()[1], ".png")
      },
      content = function(file) {
        ggsave(file, vals$ses_tracks_time)
      }
    )
    
    
    # Track velocity
    output$ses_tracks_v <- renderPlot({
      if(length(exp_select()) != 1)
      {
        (vals$ses_tracks_v <- plot_nothing())
      }else{
        (vals$ses_tracks_v <- track_raw() |>
          ses_tracks_v())
      }
    })
    
    output$ses_tracks_v_download <- downloadHandler(
      filename = function() {
        paste0("tracks_v_", exp_select()[1], ".png")
      },
      content = function(file) {
        ggsave(file, vals$ses_tracks_v)
      }
    )
    
    
    # Angle of migration
    output$ses_angle_migration <- renderPlot({
      if(length(exp_select()) != 1)
      {
        (vals$ses_angle_migration <- plot_nothing())
      }else{
        (vals$ses_angle_migration <- track_summ() |>
          ses_angle_migration())
      }
    })
    
    output$ses_angle_migration_download <- downloadHandler(
      filename = function() {
        paste0("angle_migration_", exp_select()[1], ".png")
      },
      content = function(file) {
        ggsave(file, vals$ses_angle_migration)
      }
    )


    # Instantaneous Angle of Migration
    output$ses_instant_aom <- renderPlot({
      if(length(exp_select()) != 1)
      {
        (vals$ses_instant_aom <- plot_nothing())
      }else{
        (vals$ses_instant_aom <- track_raw() |>
          ses_angle_migration_time())
      }
    })

    output$ses_instant_aom_download <- downloadHandler(
      filename = function(){
        paste0("instant_aom_", exp_select()[1], ".png")
      },
      content = function(file) {
        ggsave(file, vals$ses_instant_aom)
      }
    )
    
    
    # Chemotactic Efficiency
    output$ses_ce <- renderPlot({
      if(length(exp_select()) != 1)
      {
        (vals$ses_ce <- plot_nothing())
      }else{
        (vals$ses_ce <- track_summ() |>
          ses_chemotactic_efficiency())
      }
    })
    
    output$ses_ce_download <- downloadHandler(
      filename = function() {
        paste0("ce_", exp_select()[1], ".png")
      },
      content = function(file) {
        ggsave(file, vals$ses_ce)
      }
    )
  })
}
