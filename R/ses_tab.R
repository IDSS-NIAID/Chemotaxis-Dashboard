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
#' @importFrom shiny NS tagList selectizeInput tags HTML
ses_sidebarUI <- function(id)
{
  ns <- NS(id)
  
  tagList(
    tags$style(HTML(paste0(
      "#", ns("expID"), " + .selectize-control .selectize-input {",
      "  max-width: 100%;",
      "  white-space: normal;",
      "  overflow-wrap: anywhere;",
      "  word-break: break-word;",
      "  height: auto;",
      "  min-height: 38px;",
      "}",
      
      "#", ns("expID"), " + .selectize-control.single .selectize-input > div {",
      "  white-space: normal;",
      "  overflow-wrap: anywhere;",
      "  word-break: break-word;",
      "  max-width: calc(100% - 20px);",
      "}",
      
      "#", ns("expID"), " + .selectize-control .selectize-dropdown-content .option {",
      "  white-space: nowrap;",
      "  overflow: hidden;",
      "  text-overflow: clip;",
      "  max-width: 100%;",
      "}"
    ))),
    
    selectizeInput(
      inputId = ns("expID"),
      label = "Experiment",
      choices = NULL,
      selected = NULL,
      multiple = FALSE,
      width = "100%",
      options = list(
        placeholder = "Select one experiment",
        allowEmptyOption = TRUE,
        render = I("
        {
          option: function(item, escape) {
            var label = item.label || item.value;
            return '<div title=\"' + escape(label) + '\">' + escape(label) + '</div>';
          },
          item: function(item, escape) {
            var label = item.label || item.value;
            return '<div title=\"' + escape(label) + '\">' + escape(label) + '</div>';
          }
        }
        ")
      )
    ),
    
    sliderInput(
      inputId = ns("ses_time_filter"),
      label = "Time filter",
      min = 0,
      max = 60,
      value = c(0, 60)
    ),
    
    numericInput(
      inputId = ns("ses_angle_filter"),
      label = "min Angle of Migration (%)",
      min = 0,
      max = 90,
      value = 0
    ),
    
    numericInput(ns("ses_track_len"), "Minimum Track Length (μm)", value = 1),
    numericInput(ns("ses_track_n"), "Minimum Track Length (n)", value = 3),
    
    numericInput(
      inputId = ns("ses_ce_filter"),
      label = "min Chemotactic Efficiency (%)",
      min = -100,
      max = 100,
      value = 0
    )
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
#' @importFrom shiny downloadButton NS plotOutput tagList selectizeInput
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
#' @param shared_ce_filter reactiveVal from the main server function for filtering on minimum chemotactic efficiency
#'
#' @export
#' @importFrom shiny downloadHandler moduleServer observe observeEvent reactive reactiveValues renderPlot req updateNumericInput updateSelectizeInput
#' @importFrom dplyr left_join filter
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.csv
ses_server <- function(id, con, shared_time_filter, shared_angle_filter, shared_track_len,
                       shared_track_n, shared_ce_filter)
{
  # for all those pesky no visible binding notes
  if(FALSE)
    angle_migration <- distance_traveled <- n_frames <- ce <- chanID <- drop_summ <- NULL
  
  moduleServer(id, function(input, output, session)
  {
    ses_debug <- function(..., .level = "INFO") {
      message(sprintf(
        "[SES DEBUG %s %s] %s",
        format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
        .level,
        paste(..., collapse = "")
      ))
    }
    
    ses_size <- function(x) {
      format(utils::object.size(x), units = "auto")
    }
    
    `%||%` <- function(x, y) {
      if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) y else x
    }
    
    ses_debug("module initialized for session ", session$token)
    
    if(FALSE)
      time <- trackID <- frames <- NULL
    
    vals <- reactiveValues()
    
    # Filters
    time_filter <- reactive(input$ses_time_filter)
    angle_filter <- reactive(input$ses_angle_filter)
    track_len <- reactive(input$ses_track_len)
    track_n <- reactive(input$ses_track_n)
    ce_filter <- reactive(input$ses_ce_filter)
    
    
    # When filters change in THIS tab, update the shared value
    observeEvent(input$ses_time_filter, {
      ses_debug("input$ses_time_filter changed to ", paste(time_filter(), collapse = ", "))
      shared_time_filter(time_filter())
    })
    
    observeEvent(input$ses_angle_filter, {
      ses_debug("input$ses_angle_filter changed to ", angle_filter())
      shared_angle_filter(angle_filter())
    })
    
    observeEvent(input$ses_track_len, {
      ses_debug("input$ses_track_len changed to ", track_len())
      shared_track_len(track_len())
    })
    
    observeEvent(input$ses_track_n, {
      ses_debug("input$ses_track_n changed to ", track_n())
      shared_track_n(track_n())
    })
    
    observeEvent(input$ses_ce_filter, {
      ses_debug("input$ses_ce_filter changed to ", ce_filter())
      shared_ce_filter(ce_filter())
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
        updateNumericInput(session, "ses_angle_filter", value = shared_angle_filter())
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
    
    observeEvent(shared_ce_filter(), {
      # Check prevents an infinite loop
      if (!isTRUE(all.equal(ce_filter(), shared_ce_filter()))) {
        updateNumericInput(session, "ses_ce_filter", value = shared_ce_filter())
      }
    }, ignoreInit = TRUE)
    
    
    # Experiment selection
    # Use a plain Shiny selectizeInput instead of datamods::select_group_server.
    # The datamods widget was throwing "subscript out of bounds" internally
    # when the user cleared the selection. The SES tab only needs a single
    # experiment ID, so a single selectize input is simpler and more robust.
    observe({
      t0 <- proc.time()[["elapsed"]]
      ses_debug("experiment selector query started")
      
      exp_choices <- get_dat(con,
                             select = "DISTINCT expID",
                             from = "chanSummary") |>
        dplyr::arrange(expID) |>
        dplyr::pull(expID)
      
      ses_debug("experiment selector query finished: choices=", length(exp_choices),
                ", elapsed_s=", round(proc.time()[["elapsed"]] - t0, 3))
      
      updateSelectizeInput(session = session,
                           inputId = "expID",
                           choices = exp_choices,
                           selected = character(0),
                           server = TRUE)
    })
    
    # Safely extract exactly one selected experiment.
    # Returns NULL when the selector is empty or temporarily updating.
    selected_exp <- reactive({
      exp_id <- input$expID
      
      ses_debug("selected_exp reactive evaluated: raw expID=", paste(exp_id, collapse = ", "))
      
      if (is.null(exp_id) || length(exp_id) != 1 || is.na(exp_id) || !nzchar(exp_id)) {
        ses_debug("selected_exp rejected: no valid single experiment selected")
        return(NULL)
      }
      
      ses_debug("selected_exp accepted: ", exp_id)
      exp_id
    })
    
    # this has all raw track data plus drop
    track_raw_all <- reactive({
      t0 <- proc.time()[["elapsed"]]
      exp_id <- selected_exp()
      req(exp_id)
      
      # Convert minutes from the UI time filter to frame numbers.
      # Existing app logic uses: time = frames / 2
      frame_min <- time_filter()[1] * 2
      frame_max <- time_filter()[2] * 2
      
      where_track_raw <- paste0(
        "expID = '", exp_id, "' ",
        "AND frames BETWEEN ", frame_min, " AND ", frame_max
      )
      
      ses_debug("track_raw_all started: exp_id=", exp_id,
                ", time_filter=", paste(time_filter(), collapse = "-"),
                ", frame_min=", frame_min,
                ", frame_max=", frame_max)
      
      t_query <- proc.time()[["elapsed"]]
      raw_dat <- get_dat(con,
                         select = "expID, chanID, trackID, x, y, v_x, v_y, v, theta, frames",
                         from = "trackRaw",
                         where = where_track_raw)
      ses_debug("track_raw_all trackRaw query finished: rows=", nrow(raw_dat),
                ", cols=", ncol(raw_dat),
                ", size=", ses_size(raw_dat),
                ", elapsed_s=", round(proc.time()[["elapsed"]] - t_query, 3))
      
      t_meta <- proc.time()[["elapsed"]]
      chan_dat_raw <- get_dat(con,
                              select = "expID, sID, chanID, treatment",
                              from = "chanSummary",
                              where = paste0("expID = '", exp_id, "'"))
      ses_debug("track_raw_all chanSummary raw query finished: rows=", nrow(chan_dat_raw),
                ", cols=", ncol(chan_dat_raw),
                ", size=", ses_size(chan_dat_raw),
                ", elapsed_s=", round(proc.time()[["elapsed"]] - t_meta, 3))
      
      # Defensive metadata cleanup before joining.
      # The plotting data only needs one metadata row per expID + chanID. If chanSummary
      # has multiple rows for the same expID + chanID, joining directly causes a
      # many-to-many join that can inflate trackRaw by 100x or more.
      t_meta_clean <- proc.time()[["elapsed"]]
      key_counts <- chan_dat_raw |>
        dplyr::count(expID, chanID, name = "n")
      
      duplicate_keys <- key_counts |>
        dplyr::filter(n > 1)
      
      if (nrow(duplicate_keys) > 0) {
        ses_debug("track_raw_all chanSummary duplicate join keys detected: duplicate_key_rows=",
                  nrow(duplicate_keys),
                  ", max_matches_per_key=", max(duplicate_keys$n, na.rm = TRUE),
                  ", total_extra_matches=", sum(duplicate_keys$n - 1, na.rm = TRUE),
                  .level = "WARN")
      } else {
        ses_debug("track_raw_all chanSummary join keys are unique")
      }
      
      chan_dat <- chan_dat_raw |>
        dplyr::distinct(expID, chanID, sID, treatment) |>
        dplyr::group_by(expID, chanID) |>
        dplyr::summarize(
          sID = dplyr::first(sID),
          treatment = dplyr::first(treatment),
          .groups = "drop"
        )
      
      ses_debug("track_raw_all chanSummary cleanup finished: raw_rows=", nrow(chan_dat_raw),
                ", unique_join_keys=", nrow(key_counts),
                ", collapsed_rows=", nrow(chan_dat),
                ", size=", ses_size(chan_dat),
                ", elapsed_s=", round(proc.time()[["elapsed"]] - t_meta_clean, 3))
      
      t_join <- proc.time()[["elapsed"]]
      out <- raw_dat |>
        mutate(time = frames / 2) |>
        left_join(chan_dat, by = c("expID", "chanID")) |>
        
        # Since the time filter is now applied in SQL, these rows should all be FALSE.
        # Keeping this column avoids changing downstream logic.
        mutate(drop = FALSE)
      
      if (nrow(out) != nrow(raw_dat)) {
        ses_debug("track_raw_all row count changed during metadata join: raw_rows=", nrow(raw_dat),
                  ", joined_rows=", nrow(out),
                  ", expansion_factor=", round(nrow(out) / max(nrow(raw_dat), 1), 3),
                  .level = "WARN")
      } else {
        ses_debug("track_raw_all metadata join preserved row count: rows=", nrow(out))
      }
      
      ses_debug("track_raw_all transform/join finished: rows=", nrow(out),
                ", cols=", ncol(out),
                ", size=", ses_size(out),
                ", join_elapsed_s=", round(proc.time()[["elapsed"]] - t_join, 3),
                ", total_elapsed_s=", round(proc.time()[["elapsed"]] - t0, 3))
      
      out
    })
    
    
    # this has summary track information plus filtering metadata
    track_summ <- reactive({
      t0 <- proc.time()[["elapsed"]]
      ses_debug("track_summ started")
      
      raw <- track_raw_all()
      ses_debug("track_summ input ready: rows=", nrow(raw),
                ", cols=", ncol(raw),
                ", size=", ses_size(raw))
      
      t_summary <- proc.time()[["elapsed"]]
      summ <- raw |>
        filter(!drop) |>                                    # drop frames not passing time filter
        summarize_tracks()
      ses_debug("track_summ summarize_tracks finished: rows=", nrow(summ),
                ", cols=", ncol(summ),
                ", size=", ses_size(summ),
                ", elapsed_s=", round(proc.time()[["elapsed"]] - t_summary, 3))
      
      t_filter <- proc.time()[["elapsed"]]
      out <- summ |>
        mutate(drop = angle_migration < angle_filter() |    # drop tracks not passing these filters 
                 distance_traveled < track_len() |
                 n_frames < track_n() |
                 ce < ce_filter())
      
      ses_debug("track_summ finished: rows=", nrow(out),
                ", kept_rows=", sum(!out$drop, na.rm = TRUE),
                ", dropped_rows=", sum(out$drop, na.rm = TRUE),
                ", size=", ses_size(out),
                ", filter_elapsed_s=", round(proc.time()[["elapsed"]] - t_filter, 3),
                ", total_elapsed_s=", round(proc.time()[["elapsed"]] - t0, 3))
      
      out
    })
    
    
    # this is the version for plotting, after filtering for tracks that should be dropped
    track_raw <- reactive({
      t0 <- proc.time()[["elapsed"]]
      ses_debug("track_raw started")
      
      raw <- track_raw_all()
      summ <- track_summ()
      
      t_join <- proc.time()[["elapsed"]]
      out <- raw |>
        left_join(summ |> 
                    select(chanID, trackID, drop) |> 
                    rename(drop_summ = drop),
                  by = join_by(chanID, trackID)) |>
        filter(!drop & !drop_summ)
      
      ses_debug("track_raw finished: input_rows=", nrow(raw),
                ", summary_rows=", nrow(summ),
                ", output_rows=", nrow(out),
                ", output_size=", ses_size(out),
                ", join_filter_elapsed_s=", round(proc.time()[["elapsed"]] - t_join, 3),
                ", total_elapsed_s=", round(proc.time()[["elapsed"]] - t0, 3))
      
      out
    })
    
    
    # Track length distribution
    output$ses_tracks_time <- renderPlot({
      t0 <- proc.time()[["elapsed"]]
      exp_id <- selected_exp()
      
      if (is.null(exp_id)) {
        ses_debug("renderPlot ses_tracks_time skipped: no experiment selected")
        vals$ses_tracks_time <- plot_nothing()
        return(vals$ses_tracks_time)
      }
      
      ses_debug("renderPlot ses_tracks_time started: exp_id=", exp_id)
      
      dat <- track_raw()
      ses_debug("renderPlot ses_tracks_time data ready: rows=", nrow(dat),
                ", size=", ses_size(dat))
      
      vals$ses_tracks_time <- dat |>
        ses_tracks_time()
      
      ses_debug("renderPlot ses_tracks_time finished: elapsed_s=",
                round(proc.time()[["elapsed"]] - t0, 3))
      vals$ses_tracks_time
    })
    
    output$ses_tracks_time_download <- downloadHandler(
      filename = function() {
        paste0("tracks_time_", selected_exp() %||% "no_experiment", ".png")
      },
      content = function(file) {
        t0 <- proc.time()[["elapsed"]]
        ses_debug("download ses_tracks_time started: file=", file)
        ggsave(file, vals$ses_tracks_time)
        ses_debug("download ses_tracks_time finished: elapsed_s=", round(proc.time()[["elapsed"]] - t0, 3))
      }
    )
    
    
    # Track velocity
    output$ses_tracks_v <- renderPlot({
      t0 <- proc.time()[["elapsed"]]
      exp_id <- selected_exp()
      
      if (is.null(exp_id)) {
        ses_debug("renderPlot ses_tracks_v skipped: no experiment selected")
        vals$ses_tracks_v <- plot_nothing()
        return(vals$ses_tracks_v)
      }
      
      ses_debug("renderPlot ses_tracks_v started: exp_id=", exp_id)
      
      dat <- track_raw()
      ses_debug("renderPlot ses_tracks_v data ready: rows=", nrow(dat),
                ", size=", ses_size(dat))
      
      vals$ses_tracks_v <- dat |>
        ses_tracks_v()
      
      ses_debug("renderPlot ses_tracks_v finished: elapsed_s=",
                round(proc.time()[["elapsed"]] - t0, 3))
      vals$ses_tracks_v
    })
    
    output$ses_tracks_v_download <- downloadHandler(
      filename = function() {
        paste0("tracks_v_", selected_exp() %||% "no_experiment", ".png")
      },
      content = function(file) {
        t0 <- proc.time()[["elapsed"]]
        ses_debug("download ses_tracks_v started: file=", file)
        ggsave(file, vals$ses_tracks_v)
        ses_debug("download ses_tracks_v finished: elapsed_s=", round(proc.time()[["elapsed"]] - t0, 3))
      }
    )
    
    
    # Angle of migration
    output$ses_angle_migration <- renderPlot({
      t0 <- proc.time()[["elapsed"]]
      exp_id <- selected_exp()
      
      if (is.null(exp_id)) {
        ses_debug("renderPlot ses_angle_migration skipped: no experiment selected")
        vals$ses_angle_migration <- plot_nothing()
        return(vals$ses_angle_migration)
      }
      
      ses_debug("renderPlot ses_angle_migration started: exp_id=", exp_id)
      
      dat <- track_summ() |>
        filter(!drop)
      ses_debug("renderPlot ses_angle_migration data ready: rows=", nrow(dat),
                ", size=", ses_size(dat))
      
      vals$ses_angle_migration <- dat |>
        ses_angle_migration()
      
      ses_debug("renderPlot ses_angle_migration finished: elapsed_s=",
                round(proc.time()[["elapsed"]] - t0, 3))
      vals$ses_angle_migration
    })
    
    output$ses_angle_migration_download <- downloadHandler(
      filename = function() {
        paste0("angle_migration_", selected_exp() %||% "no_experiment", ".png")
      },
      content = function(file) {
        t0 <- proc.time()[["elapsed"]]
        ses_debug("download ses_angle_migration started: file=", file)
        ggsave(file, vals$ses_angle_migration)
        ses_debug("download ses_angle_migration finished: elapsed_s=", round(proc.time()[["elapsed"]] - t0, 3))
      }
    )
    
    
    # Instantaneous Angle of Migration
    output$ses_instant_aom <- renderPlot({
      t0 <- proc.time()[["elapsed"]]
      exp_id <- selected_exp()
      
      if (is.null(exp_id)) {
        ses_debug("renderPlot ses_instant_aom skipped: no experiment selected")
        vals$ses_instant_aom <- plot_nothing()
        return(vals$ses_instant_aom)
      }
      
      ses_debug("renderPlot ses_instant_aom started: exp_id=", exp_id)
      
      dat <- track_raw()
      ses_debug("renderPlot ses_instant_aom data ready: rows=", nrow(dat),
                ", size=", ses_size(dat))
      
      vals$ses_instant_aom <- dat |>
        ses_angle_migration_time()
      
      ses_debug("renderPlot ses_instant_aom finished: elapsed_s=",
                round(proc.time()[["elapsed"]] - t0, 3))
      vals$ses_instant_aom
    })
    
    output$ses_instant_aom_download <- downloadHandler(
      filename = function(){
        paste0("instant_aom_", selected_exp() %||% "no_experiment", ".png")
      },
      content = function(file) {
        t0 <- proc.time()[["elapsed"]]
        ses_debug("download ses_instant_aom started: file=", file)
        ggsave(file, vals$ses_instant_aom)
        ses_debug("download ses_instant_aom finished: elapsed_s=", round(proc.time()[["elapsed"]] - t0, 3))
      }
    )
    
    
    # Chemotactic Efficiency
    output$ses_ce <- renderPlot({
      t0 <- proc.time()[["elapsed"]]
      exp_id <- selected_exp()
      
      if (is.null(exp_id)) {
        ses_debug("renderPlot ses_ce skipped: no experiment selected")
        vals$ses_ce <- plot_nothing()
        return(vals$ses_ce)
      }
      
      ses_debug("renderPlot ses_ce started: exp_id=", exp_id)
      
      dat <- track_summ() |>
        filter(!drop)
      ses_debug("renderPlot ses_ce data ready: rows=", nrow(dat),
                ", size=", ses_size(dat))
      
      vals$ses_ce <- dat |>
        ses_chemotactic_efficiency()
      
      ses_debug("renderPlot ses_ce finished: elapsed_s=",
                round(proc.time()[["elapsed"]] - t0, 3))
      vals$ses_ce
    })
    
    output$ses_ce_download <- downloadHandler(
      filename = function() {
        paste0("ce_", selected_exp() %||% "no_experiment", ".png")
      },
      content = function(file) {
        t0 <- proc.time()[["elapsed"]]
        ses_debug("download ses_ce started: file=", file)
        ggsave(file, vals$ses_ce)
        ses_debug("download ses_ce finished: elapsed_s=", round(proc.time()[["elapsed"]] - t0, 3))
      }
    )
  })
}
