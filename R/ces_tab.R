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
                    inline = FALSE)
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
           card_body(DT::dataTableOutput(ns("ces_sample_table")))),
      #card_footer(downloadButton(ns('ces_sample_table_download'), 'Download table')),
      layout_columns(
        card(full_screen = TRUE, 
             card_header("Directed cell velocity over time (y)"), 
             card_body(plotOutput(ns("ces_vy")))),
        #card_footer(downloadButton(ns('ces_n_cells_download'), 'Download figure')))
        card(full_screen = TRUE, 
             card_header("Undirected cell velocity over time (x)"), 
             card_body(plotOutput(ns("ces_vx"))))
        #card_footer(downloadButton(ns('ces_n_cells_download'), 'Download figure')))
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
#' @param user Username of the user
#' 
#' @export
#' @importFrom datamods select_group_server
#' @importFrom dplyr mutate left_join join_by rename
#' @importFrom DT renderDataTable
#' @importFrom shiny moduleServer reactive reactiveValues renderPlot
ces_server <- function(id, con, user)
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
                                                   user = user, 
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
                user = user,
                select = 'expID, chanID, frames, v_x, v_y',
                from = 'chanRaw',
                where = where) |>
          
          mutate(time = frames / 2) |>
          
          left_join(chan_select(), by = join_by(expID, chanID))
      })
      
      
      # Summary table
      output$ces_sample_table <- DT::renderDataTable({
        chan_select()
      })
      
      
      # directed velocity plot
      output$ces_vy <- renderPlot({
        (vals$ces_vy <- chan_raw() |> 
           mutate(v = v_y) |>
           ces_v('Relative velocity (y - directed)'))
      })
      
      
      # undirected velocity plot
      output$ces_vx <- renderPlot({
        (vals$ces_vx <- chan_raw() |> 
           mutate(v = v_x) |>
           ces_v('Relative velocity (x - undirected)'))
      })
    }
  )
}
