# ces_tab.R
# UI and server elements for the Cross-Experiment Summary tab

#' ces_sidebarUI
#' UI element for the CE sidebar
#' 
#' @name ces_tab
#' 
#' @param id Shiny namespace ID
#' 
#' @return A modularized tagList
#' @export
#' @importFrom shiny NS tagList
#' @importFrom datamods select_group_ui
ces_sidebarUI <- function(id)
{
  ns <- NS(id)
  
  tagList(
    select_group_ui(id = ns("ces_channels"),
                    params = list(list(inputId =     "expID", label = "Experiment"),
                                  list(inputId =       "sID", label = "Sample"),
                                  list(inputId =    "chanID", label = "Channel"),
                                  list(inputId = "treatment", label = "Treatment")),
                    inline = FALSE),
    
    # Add the new user input for filtering
    sliderInput(
      inputId = ns("ces_time_filter"),
      label = "Time filter",
      min = 0,
      max = 60,
      value = c(0, 60)
    ),
    sliderInput(
      inputId = ns("ces_angle_filter"),
      label = "Angle of migration filter",
      min = 0,
      max = 90,
      value = c(0, 90)
    ),
    numericInput(ns('ces_track_len'), 'Minimum Track Length (μm)', value = 1),
    numericInput(ns('ces_track_n'), 'Minimum Track Length (n)', value = 3),
    numericInput(
      inputId = ns("ces_ce_filter"),
      label = "min Chemotactic Efficiency",
      min = -100,
      max = 100,
      value = 0
    )

  )
}


#' ces_cardsUI
#' UI element for the CE cards
#' 
#' @rdname ces_tab
#' 
#' @return A modularized tagList of cards
#' @export
#' 
#' @importFrom bslib card card_header card_body card_footer layout_columns layout_sidebar
#' @importFrom DT dataTableOutput
#' @importFrom shiny downloadButton NS plotOutput tableOutput tagList
ces_cardsUI <- function(id)
{
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = ces_sidebarUI('ces'),
      card(full_screen = TRUE, 
           card_header("Selected Samples"), 
           card_body(DT::dataTableOutput(ns("ces_sample_table"))),
      card_footer(downloadButton(ns('ces_sample_table_download'), 'Download table'))),
      layout_columns(
        card(full_screen = TRUE, 
             card_header("Directed cell velocity over time (y)"), 
             card_body(plotOutput(ns("ces_vy"))),
        card_footer(downloadButton(ns('ces_vy_download'), 'Download figure'))),
        card(full_screen = TRUE, 
             card_header("Undirected cell velocity over time (x)"), 
             card_body(plotOutput(ns("ces_vx"))),
        card_footer(downloadButton(ns('ces_vx_download'), 'Download figure')))
      )
    )
  )
}


#' ces_server
#' Server logic for the Cross-Experiment Summary tab
#' 
#' @rdname ces_tab
#' 
#' @param con Active DBI database connection
#' @param shared_time_filter reactiveVal from the main server function for time filter definition
#' @param shared_angle_filter reactiveVal from the main server function for angle filter definition
#' @param shared_track_len reactiveVal from the main server function for physical track length filter in μm
#' @param shared_track_n reactiveVal from the main server function for track length filter for total number of frames
#' @param shared_ce_filter reactiveVal from the main server function for filtering on minimum chemotactic efficiency
#' 
#' @export
#' @importFrom datamods select_group_server
#' @importFrom dplyr mutate left_join join_by rename
#' @importFrom DT renderDataTable
#' @importFrom ggplot2 ggsave
#' @importFrom shiny downloadHandler moduleServer reactive reactiveValues renderPlot
#' @importFrom utils write.csv
ces_server <- function(id, con, shared_time_filter, shared_angle_filter, shared_track_len, 
                       shared_track_n, shared_ce_filter)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
    chanID <- expID <- sID <- frames <- v_x <- v_y <- NULL
  
  moduleServer(
    id,
    function(input, output, session)
    {
      # reactive values
      vals <- reactiveValues()
      
      
      # channel/sample/treatment selection
      chan_select <- select_group_server(id = 'ces_channels', 
                                         data_r = reactive({
                                           get_dat(con, 
                                                   select = 'expID, sID, chanID, treatment', 
                                                   from = 'chanSummary')
                                         }),
                                         vars_r = c('expID', 'sID', 'chanID', 'treatment'))
      
      
      # pull raw channel information
      chan_raw <- reactive({
        
        if(length(chan_select()$expID) > 0)
        {
          where <- paste(paste0( "expID IN (", paste0('"', chan_select()$ expID, '"', collapse = ", "), ")"),
                         "AND",
                         paste0("chanID IN (", paste(      chan_select()$chanID,      collapse = ", "), ")"))
        }else{
          where <- NULL
        }
        
        get_dat(con,
                select = 'expID, chanID, frames, v_x, v_y',
                from = 'chanRaw',
                where = where) |>
          
          mutate(time = frames / 2) |>
          
          left_join(chan_select(), by = join_by(expID, chanID))
      })
      

      # Filters
      time_filter <- reactive(input$ces_time_filter)
      angle_filter <- reactive(input$ces_angle_filter)
      track_len <- reactive(input$ces_track_len)
      track_n <- reactive(input$ces_track_n)
      ce_filter <- reactive(input$ces_ce_filter)
      

      # When filters change in THIS tab, update the shared value
      observeEvent(input$ces_time_filter, {
        shared_time_filter(time_filter())
      })
      
      observeEvent(input$ces_angle_filter, {
        shared_angle_filter(angle_filter())
      })
      
      observeEvent(input$ces_track_len, {
        shared_track_len(track_len())
      })
      
      observeEvent(input$ces_track_n, {
        shared_track_n(track_n())
      })

      observeEvent(input$ces_ce_filter, {
        shared_ce_filter(ce_filter())
      })


      # When shared values change, update filters in THIS tab
      observeEvent(shared_time_filter(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(time_filter(), shared_time_filter()))) {
          updateSliderInput(session, "ces_time_filter", value = shared_time_filter())
        }
      }, ignoreInit = TRUE)
      
      observeEvent(shared_angle_filter(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(angle_filter(), shared_angle_filter()))) {
          updateSliderInput(session, "ces_angle_filter", value = shared_angle_filter())
        }
      }, ignoreInit = TRUE)
      
      observeEvent(shared_track_len(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(track_len(), shared_track_len()))) {
          updateNumericInput(session, "ces_track_len", value = shared_track_len())
        }
      }, ignoreInit = TRUE)
      
      observeEvent(shared_track_n(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(track_n(), shared_track_n()))) {
          updateNumericInput(session, "ces_track_n", value = shared_track_n())
        }
      }, ignoreInit = TRUE)
      
      observeEvent(shared_ce_filter(), {
        # Check prevents an infinite loop
        if (!isTRUE(all.equal(ce_filter(), shared_ce_filter()))) {
          updateNumericInput(session, "ces_ce_filter", value = shared_ce_filter())
        }
      }, ignoreInit = TRUE)


      # Summary table
      output$ces_sample_table <- DT::renderDataTable({
        chan_select()
      })
      
      output$ces_sample_table_download <- downloadHandler(
        filename = function() {
          paste0('ces_sample_table_', Sys.Date(), '.csv')},
        content = function(file) {
          write.csv(chan_select(), file, row.names = FALSE)}
      )
      
      
      # directed velocity plot
      output$ces_vy <- renderPlot({
        (vals$ces_vy <- chan_raw() |> 
           mutate(v = v_y) |>
           ces_v(ylab = 'Relative velocity (y - directed)',
                 xlim = time_filter()))
      })
      
      output$ces_vy_download <- downloadHandler(
        filename = function() {
          paste0('ces_vy_', Sys.Date(), '.png')},
        content = function(file) {
          ggsave(file, vals$ces_vy)}
      )
      
      
      # undirected velocity plot
      output$ces_vx <- renderPlot({
        (vals$ces_vx <- chan_raw() |> 
           mutate(v = v_x) |>
           ces_v('Relative velocity (x - undirected)',
                 xlim = time_filter()))
      })
      
      output$ces_vx_download <- downloadHandler(
        filename = function() {
          paste0('ces_vx_', Sys.Date(), '.png')},
        content = function(file) {
          ggsave(file, vals$ces_vx)}
      )
    }
  )
}
