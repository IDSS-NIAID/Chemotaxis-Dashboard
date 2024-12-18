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
                    inline = FALSE)
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
           card_footer(downloadButton(ns('ses_ce_download'), 'Download figure'))),
      card(full_screen = TRUE,
           card_header("Statistics"), 
           card_body(tableOutput(ns("ses_stats"))),
           card_footer(downloadButton(ns('ses_stats_download'), 'Download statistics'),
                       downloadButton(ns('ses_raw_download'), 'Download raw data')))
    )
  )
}


#' ses_server
#' Server logic for the Single Experiment Statistics tab
#' 
#' @rdname ses_tab 
#' 
#' @param con Active DBI database connection
#' @param user Username of the user
#'
#' @export
#' @importFrom shiny downloadHandler moduleServer reactive reactiveValues renderPlot renderTable
#' @importFrom dplyr left_join
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.csv
ses_server <- function(id, con, user)
{
  moduleServer(id, function(input, output, session)
  {
    vals <- reactiveValues()
    
    # Experiment selection
    exp_select <- select_group_server(id = "ses_channels",
                                      data_r = reactive({
                                        get_dat(con,
                                                user = user,
                                                select = "expID",
                                                from = "expSummary")
                                      }),
                                      vars_r = 'expID')

    
    track_raw <- reactive({
      
      get_dat(con,
              user = user,
              select = "expID, chanID, trackID, x, y, v_x, v_y, theta, frames",
              from = "trackRaw",
              where = paste0("expID='", exp_select()[1], "'")) |>
        
        left_join(get_dat(con,
                          user = user,
                          select = "expID, sID, chanID, treatment",
                          from = "chanSummary",
                          where = paste0("expID='", exp_select()[1], "'")),
                  by = c("expID", "chanID"))
    })
    
    
    track_summ <- reactive({
      
      get_dat(con,
              user = user,
              select = "expID, chanID, trackID, ce, angle_migration",
              from = "trackSummary",
              where = paste0("expID='", exp_select()[1], "'")) |>
        
        left_join(get_dat(con,
                          user = user,
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
    
    
    # Statistics
    output$ses_stats <- renderTable({
      if(length(exp_select()) != 1)
      {
        (vals$ses_stats <- data.frame())
      }else{
        (vals$ses_stats <- get_dat(con,
                                   user = user,
                                   select = "*",
                                   from = "expStats",
                                   where = paste0("expID='", exp_select()[1], "'")))
      }
    })
    
    output$ses_stats_download <- downloadHandler(
      filename = function() {
        paste0("stats_", exp_select()[1], ".csv")
      },
      content = function(file) {
        write.csv(vals$ses_stats, file, row.names = FALSE)
      }
    )
    
    output$ses_raw_download <- downloadHandler(
      filename = function() {
        paste0("raw_tracks", exp_select()[1], ".csv")
      },
      content = function(file) {
        write.csv(track_raw(), file, row.names = FALSE)
      }
    )
  })
}
